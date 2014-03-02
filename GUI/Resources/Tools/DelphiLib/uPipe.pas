unit uPipe;  // (C) Alphasoft Pty Ltd

interface

	uses
		Forms, Classes, Windows, Messages, Dialogs, Contnrs, SysUtils,
		uUtils, uGT, uPacket;

	{  $define Verbose }    // for ShowMessage(... errors
type

	cPipe = class;   // foreward

	cReaderThread = class( TThread )
			constructor Create( pipe : cPipe; h : int );
            destructor   Destroy;    override;
			procedure Execute; override;
		public
			mConnected : boolean;
			mShutdown : boolean;
			procedure SendMesg( const pm : apPacket );
		private
			mPipe : cPipe;  // use lock to access
			mhRxPipe, mhTxPipe : int;
			mTxName : string;
			mID : byte;
            mThreadTerminated : boolean;
			procedure AddDest();
			end;

	cPipe = class( TObject )
		private
			mEr : int;
			mMaster : boolean;
			oReaderThreads : TList;  // of cReaderThread
			mName : string;
    mMasterName: string;
//			mhWindow : HWND;
//			mWinMesgNo : int;
			function  NewRxPipe() : cReaderThread;
			function  ConnectTxPipe( const name : string ) : int;
			procedure AnnouncePipe( slave : string );
            function  GetConnected() : boolean;
		public
            oInQ : cLockedQueue;
            mOnConnection : aEventHandler;
			mShutdown : boolean;
			constructor CreatePipe( const MyName, MasterName : string; wh : HWND; winMesgNo : int );
            destructor  Destroy;    override;
			procedure	Connect( con : boolean );
            procedure	Clear;
			function    ClientCount : int;
			procedure   FreeMesg( const pm : apPacket );
			//class function  MesgStr( const pm : apPacket ) : string;
			procedure Broadcast( const mesg : string; excludeID : int = -1; mType : aPktType = pkPassThrough ); // to all pipes
		property Error : integer  read mEr;
		property Master : boolean  read mMaster;
		property Connected : boolean  read GetConnected;
			end;

implementation

const
	BasePipeName = '\\.\pipe\';   // DO NOT CHANGE - WINDOWS CONST
	PipeBufferSize = $2000;


constructor cReaderThread.Create( pipe : cPipe; h : int );

	begin
	mPipe := pipe;
	mhRxPipe := h;
	// mhTxPipe := 0;
	inherited Create( false );
	end;


destructor   cReaderThread.Destroy;

	begin
    Terminate;
    if mhTxPipe > 0 then  begin
        CloseHandle( mhTxPipe );
        mhTxPipe := 0;
        end;
    if mConnected then  begin     // waiting to connect cannot be rescued
        while not mThreadTerminated do
          Sleep( 10 );
        if mhRxPipe > 0 then  begin
            CloseHandle( mhRxPipe );  // thread stays locked in ConnectNamedPipe and cannot close - MS bug
            mhRxPipe := 0;
            end;
    	inherited  Destroy;           // thread still locked see above
	    end;
	end;


procedure cReaderThread.SendMesg( const pm : apPacket );   // main thread except echo disconnect

	var
		n : cardinal;
	begin
	if mhTxPipe > 0 then  begin
		n := 0;
		if not WriteFile( mhTxPipe, pm^, pm.pkSize, n, nil ) then  begin
			{$ifdef Verbose }  ShowMessage( 'PIPE write failed.' + WinError( mPipe.mEr ) );
			{$else }   mPipe.mEr := GetLastError();
			{$endif }
			end;
		end;
	end;


procedure cReaderThread.AddDest();

	var
		h : int;
	begin
	h := mPipe.ConnectTxPipe( mTxName );
	if h > 0 then  begin
		mhTxPipe := h;
		end;
	end;


procedure cReaderThread.Execute;

	var
		n, s : cardinal;
		pm : apPacket;
		rt : cReaderThread;
	begin
	repeat              // wait for someone to connect
		mConnected := ConnectNamedPipe( mhRxPipe, nil );
		if mConnected then  break;
        Sleep( 1000 );
		until mPipe.mShutdown;
    Sleep( 30 );    // need pause to allow master to establish back link - windows bug ?

	if not Terminated and not mPipe.mShutdown and mConnected and mPipe.mMaster then  begin
		rt := mPipe.NewRxPipe();   // make a new reader to handle any more connectors

		if rt <> nil then  begin    // put in empty slot or add to list - keeps mID valid index
			for n := 0 to mPipe.oReaderThreads.Count - 1 do  begin
				if mPipe.oReaderThreads[ n ] = nil then  begin
					mPipe.oReaderThreads[ n ] := rt;
					rt.mID := n;
					rt := nil;
					break;
					end;
				end;
			end;
		if rt <> nil then  rt.mID := mPipe.oReaderThreads.Add( rt );
		end;

	if not Terminated and mConnected then  begin
		repeat             // read and post incomming packets
			n := 0;
			if ReadFile( mhRxPipe, s, SizeOf( s ), n, nil ) then  begin
				if ( n = SizeOf( s ) ) then  begin
					if not Terminated and ( s >= SizeOfPktHdr ) then  begin  // at least 8 bytes
						GetMem( pm, s );
						pm.pkSize := s;
						if ReadFile( mhRxPipe, pm.pkType, s - SizeOf( s ), n, nil ) then  begin
							if pm.pkFlags = MagicFlags then  begin
								pm.pkSrcID := mID;
								if n = s - SizeOf( s ) then  begin  // all here
									if pm.pkType = pkPassThrough then  begin
										mPipe.oInQ.Post( pm, alPipe );
										end
									else if pm.pkType = pkConnectTo then  begin
										mTxName := pm.ToString;
										AddDest();
										mPipe.oInQ.Post( pm, alPipe );
										end
									else if pm.pkType = pkDisconnect then  begin
										SendMesg( pm );   // echo disconnct
										mConnected := false;   // I've been dumped
										pm.pkType := pkPassThrough;  // make message innocuous
										mPipe.oInQ.Post( pm, alPipe );
										end
									else  begin
										FreeMem( pm );
										mPipe.mEr := 102;  // unrecognised type
										end;
									end
								else  begin
									FreeMem( pm );
									mPipe.mEr := 103;  // incomplete
									end;
								end
							else  begin
								FreeMem( pm );
								mPipe.mEr := 101;  // incorrect magic flags
								end;
							end
						else  mConnected := false;
						end;
					end;
				end

			else  begin
				{$ifdef Verbose } Synchronize( procedure() begin ShowMessage( 'Read fail ' + WinError );  end );
				{$else } mPipe.mEr := GetLastError();
				{$endif }
				mConnected := false;
				end;
			until mPipe.mShutdown or not mConnected or Terminated;
		end;
    mThreadTerminated := true;
	CloseHandle( mhRxPipe );  mhRxPipe := 0;
	mConnected := false;
	end;

//____________________________ MAIN THREAD _____________________________________

constructor	cPipe.CreatePipe( const MyName, MasterName : string; wh : HWND; winMesgNo : int );

//	var
//		rt : cReaderThread;
//		h : int;
	begin
	mName := BasePipeName + MyName;
    mMasterName := MasterName;
//	mhWindow := wh;
//	mWinMesgNo := winMesgNo;
	oReaderThreads := TList.Create;
	oInQ := cLockedQueue.Create( wh, winMesgNo );
	mMaster := MasterName = '';
	end;


destructor   cPipe.Destroy;

//	var
//		p : pointer;
//		rt : cReaderThread;
	begin
//    Broadcast( '', -1, pkDisconnect );
//    mShutdown := true;
//    Sleep( 20 );
//    Clear;
//    for p in mReaderThreads do  begin
//        if p <> nil then  begin
//            rt := p;
//            //rt.Terminate;     rt.Resume;
//            rt.Free;
//            end;
//        end;
	Connect( false );
    oInQ.Free;
    oReaderThreads.Free;
    inherited Destroy;
	end;


procedure	cPipe.Clear;

	var
		p : pointer;
		rt : cReaderThread;
	begin
    for p in oReaderThreads do  begin
        if p <> nil then  begin
            rt := p;
            if not mMaster then  CloseHandle( rt.mhRxPipe );   // rx thread still waiting in there so blocks
            CloseHandle( rt.mhTxPipe );
            //rt.Terminate;     rt.Resume;
            rt.Free;
            end;
        end;
    end;


procedure	cPipe.Connect( con : boolean );

	var
		rt : cReaderThread;
		h : int;
	begin
    if con then  begin
        if not mMaster then  begin
            h := ConnectTxPipe( BasePipeName + mMasterName );  // try connect to master pipe
            if h > 0 then  begin
                rt := NewRxPipe();
                if rt <> nil then    begin
                    rt.mID := oReaderThreads.Add( rt );
                    rt.mTxName := BasePipeName + mMasterName;
                    rt.mhTxPipe := h;
                    AnnouncePipe( mName );                 // tell master to send to me
                    end;
                end;
            end
        else  begin
            rt := NewRxPipe();
            if rt <> nil then    begin
                rt.mID := oReaderThreads.Add( rt );
                end;
            end;
    	end
    else  begin
    	Broadcast( '', -1, pkDisconnect );
        mShutdown := true;
        Sleep( 20 );
        Clear;

    	end;
    end;


function  cPipe.NewRxPipe() : cReaderThread;

	var
		h : int;
	begin
	result := nil;
	h := CreateNamedPipe(
		PChar( mName ), PIPE_ACCESS_INBOUND,
		PIPE_TYPE_MESSAGE or PIPE_WAIT,
		PIPE_UNLIMITED_INSTANCES, PipeBufferSize, PipeBufferSize,
		{100,} NMPWAIT_USE_DEFAULT_WAIT, nil );
	if h < 0 then
		{$ifdef Verbose } ShowMessage( 'Create Pipe failed ' + mName + ' ' + WinError( mEr ) )
		{$else } mEr := GetLastError()
		{$endif }
	else  result := cReaderThread.Create( self, h );  // start reader thread
	end;


function    cPipe.ClientCount : int;

	var
		p : int;
		rt : cReaderThread;
	begin
	result := 0;
    if ( oReaderThreads <> nil ) then  begin
        for p := 0 to oReaderThreads.Count - 1 do  begin
            if oReaderThreads[ p ] <> nil then  begin
                rt := oReaderThreads[ p ];
                if rt.mhTxPipe > 0 then  Inc( result );
                end;
            end;
    	end;
	end;


function  cPipe.GetConnected() : boolean;

	var
    	r : int;
        rt : cReaderThread;
	begin
    result := false;
    if oReaderThreads <> nil then  begin
        for r := 0 to oReaderThreads.Count - 1 do  begin
            rt := oReaderThreads[ r ];
            if ( rt <> nil ) and rt.mConnected then  result := true;
        	end;
    	end;
    end;


procedure cPipe.FreeMesg( const pm : apPacket );

	var
		rt : cReaderThread;
	begin
	if pm <> nil then  begin
		if pm.pkType = pkDisconnect then  begin
			if mMaster and ( oReaderThreads <> nil ) and ( pm.pkSrcID < oReaderThreads.Count ) then  begin
				rt := oReaderThreads[ pm.pkSrcID ];
				// mReaderThreads.Delete( pm.pkPipeID );
				rt.Free;  oReaderThreads[ pm.pkSrcID ] := nil;
				end;
			end;
		pm.FreePkt();
	    end;
	end;


function cPipe.ConnectTxPipe( const name : string ) : int;

	var
		n : string;
	begin
	n := name;
	result := CreateFile( PChar( n ), GENERIC_WRITE,
					FILE_SHARE_WRITE, Nil, OPEN_EXISTING,
					FILE_ATTRIBUTE_NORMAL, 0 );
	if result < 0 then
		{$ifdef Verbose }  ShowMessage( 'Create client handle failed ' + n + ' ' + WinError( mEr ) );
		{$else } mEr := GetLastError();
		{$endif }
	end;


procedure cPipe.Broadcast( const mesg : string; excludeID : int = -1; mType : aPktType = pkPassThrough );

	var
		pm : apPacket;
		p : pointer;
		rt : cReaderThread;
		x : int;
	begin
	pm := NewPkt( mesg );
	pm.pkType := mType;
	x := 0;
	for p in oReaderThreads do  begin
		if p <> nil then  begin
			rt := p;
			if rt.mhRxPipe = 0 then  begin   // garbage collect
				rt.Free;
				oReaderThreads[ x ] := nil;
				end
			else if rt.mID <> excludeID then begin
				rt.SendMesg( pm );  // to avoid mesages to self
				end;
			end;
		Inc( x );
		end;
	FreeMem( pm );
	end;


procedure cPipe.AnnouncePipe( slave : string );

	begin
	Broadcast( slave, -1, pkConnectTo );
    if Assigned( mOnConnection ) then  mOnConnection();
	end;


{// ______________________________ USEAGE ______________________________________

see uPipeTest.pas
procedure  TForm1.WMPipe( var wm : TMessage ); // message WM_Pipe;   // private in TfMain

	var
		mp : apPipeMesg;
	begin
	if mPipe.Count > 0 then  begin
		mp := mPipe.Read();
		if mp.pkType = mtPassThrough then 	Memo1.Lines.Add( cPipe.MesgStr( mp ) )
		else if mp.pkType = mtConnentTo then   lbConnect.Caption := cPipe.MesgStr( mp )
		else if mp.pkType = mtDisconnect then   Close;
		FreeMem( mp );
		end;
	end;


procedure TForm1.btCreateClick(Sender: TObject);

	begin
	if mPipe = nil then  begin
		if cbMaster.Checked then  begin
			mPipe := cPipe.CreatePipe( ebCreate.Text, '' );
			end
		else  mPipe := cPipe.CreatePipe( ebCreate.Text, ebConnect.Text );
		end;
	end;


procedure TForm1.btDisconnectClick(Sender: TObject);

	begin
	mPipe.Broadcast( '', mtDisconnect );
	Close;
	end;


procedure TForm1.btSendClick(Sender: TObject);

	begin
	mPipe.Broadcast( ebSend.Text )
	end;

	  }

end.
