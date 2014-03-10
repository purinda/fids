unit uUDP;

interface

uses
    Windows, SysUtils,
    IdUDPServer, IdBaseComponent, IdComponent, IdUDPBase, IdGlobal, IdSocketHandle,
    uPacket, uGT, uGlobalDefs;

type
    cUDP = class( TIdUDPServer )   // based on indy UDP transciever
        private
            mPortNo : word;
        	mServerName, mIamServerName, mSeekServerName : string;
			mServerIP : string;
            mLog : aLogProc;
            //mSrcIP : string;
            //mEr : int;
            mBroadcast : boolean;
			//procedure  Log( er : int; const s : string );
            procedure  UDPReadEvent( AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
            procedure  SetIamServerName( const name : string );
        public
            mInQ : cLockedQueue;
			constructor  Create( port : int; wh : HWND; winMesgNo : int; log : aLogProc = nil );
            destructor   Destroy;   override;

            procedure    RequestServer( const ServerName : string );
			procedure    AnnounceServer;
			procedure 	 Send( const pkt : aPacket );   overload;
			procedure 	 Send( const str : string; PktType : aPktType = pkPassThrough );   overload;
			procedure    FreeMesg( const pm : apPacket );

            property     PortNo : word  read mPortNo  write mPortNo;
            property	 ServerIP : string  read mServerIP;
            property     ServerName : string  read  mServerName;
            property     IamServerName : string  read  mIamServerName write SetIamServerName;
    	end;

implementation

uses	IdStack;

//________________________________ READER THREAD _______________________________    




procedure  cUDP.UDPReadEvent( AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);

    var
        pp : apPacket;
        s : string;
    begin
    if ( Length( AData ) > SizeOfPktHdr ) and ( GStack.LocalAddresses.IndexOf( ABinding.PeerIP ) < 0 ) then  begin
    	//  long enough                                            not my echo
        pp := NewPkt( AData );
        if pp.pkFlags = MagicFlags then  begin
            if pp.pkType = pkRequestServerIP then  begin
            	s := pp.ToString();
                //Log( stUDPRequest, 'UDP Request ' + s );
            	if s = mIamServerName then   AnnounceServer;
                end
            else if pp.pkType = pkAnnounceServer then  begin
            	s := pp.ToString();
                if mSeekServerName = s then  begin
                    mServerName := s;
                    mServerIP := ABinding.PeerIP;
                	//Log( stUDPReply, 'UDP Relevant Announcement ' + s );
                    end
                // else  Log( stUDPReply, 'UDP Announcement ' + s );
	            end;
            mInQ.Post( pp, alUDP );
	        end;
	    end;
    end;

//______________________________ MAIN THREAD ___________________________________
    
constructor  cUDP.Create( port : int; wh : HWND; winMesgNo : int; log : aLogProc = nil );

	begin
    inherited Create;
    mInQ := cLockedQueue.Create( wh, winMesgNo );

    mPortNo := port;
    DefaultPort := mPortNo;
    mBroadcast := true;
    mLog := log;

    OnUDPRead := UDPReadEvent;
    BroadcastEnabled := mBroadcast;
    Active := true;
	end;


destructor   cUDP.Destroy;

	begin
    Active := false;
    FreeAndNil( mInQ );
    inherited Destroy;
    end;


{procedure  cUDP.Log( er : int; const s : string );

	begin
	if er > mEr then  mEr := er;
	if Assigned( mLog ) then  mLog( er, s );
	end;}


procedure  cUDP.Send( const pkt : aPacket );

	begin
    if mBroadcast then  Broadcast( pkt.ToBytes(), mPortNo );
    end;


procedure 	 cUDP.Send( const str : string; PktType : aPktType = pkPassThrough );

    var
      pp : apPacket; 
	begin
    pp := NewPkt( str );
    pp.pkType := PktType;
    Send( pp^ );
    pp.FreePkt;
    end;


procedure  cUDP.RequestServer( const ServerName : string );

	begin
    Send( ServerName, pkRequestServerIP );
    mSeekServerName := ServerName;
    end;


procedure  cUDP.AnnounceServer;

	begin
    Broadcast( PktBytes( mIamServerName, pkAnnounceServer ), PortNo );
	end;


procedure  cUDP.SetIamServerName( const name : string );

	begin
    mIamServerName := name;
    // Log( stUDPServer, 'UDP server ' + name );
    AnnounceServer;
    end;


procedure cUDP.FreeMesg( const pm : apPacket );

	begin
	if pm <> nil then  begin
		pm.FreePkt;
		end;
	end;
    
end.
