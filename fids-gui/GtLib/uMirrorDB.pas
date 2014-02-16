unit uMirrorDB;
// (C) ALphasoft Pty Ltd

{ http://blogs.neotechnology.com/emil/2009/11/nosql-scaling-to-size-and-scaling-to-complexity.html
  "... there's a lot more to NoSQL than just performance and scaling.
  Most importantly (for me, at least) is that NoSQL DBs often provide better substrates for modeling
  business domains.
  I've spent more than two years struggling to map just part of the comic book business onto MySQL,
  for instance, where something like a graph database would be a vastly better fit."
}

interface

uses
	uGlobalDefs, uMessageHub, uXmlParser, uDbTree, uGT, Classes, SyncObjs,
	Generics.Collections, SysUtils;

const
	msInit = 0; // mirror state constants  0..path count - 1
	msDone = MaxInt;

type

	aDeltaNotify = procedure(const xml: string) of object;
	apDeltaNotify = ^aDeltaNotify;

	aOnEditNotify = procedure(const req: cXmlParser) of object;

	// aMirrirAttribute = ( maNone, maKey, maCanMaster, maLocal );
	aDBReadop = (opNone, opRead, opScan);
	aDBEditop = (opEdNone, opNew, opRename, opDelete, opEdit);

	cMirrorDB = class(cXmlParser)
		constructor Create(hub: cMessageHub; id: string;
		  pCanMaster: boolean = false; isKey: boolean = true; treeID: word = 0;
		  log: aLogProc = nil); overload;
		constructor Create; overload;
		destructor Destroy; override;
	private
		mHub: cMessageHub;
		oMirrorList: TStringList;
		mMirrorState: int;
		oDeltaNotify: TList; // of aDeltaNotify
		mOnMasterEdit: aOnEditNotify;
		mRequesterID: string;
		// mOnEvent	: apConnectionEventReader;

		oLoggedIn: TStringList;
		mHubID: int;
		mJournalCount: Integer;
		mJournalFile: TextFile;
		mJournalName: string;
		mJournalDir: string;
		mID, mLastID: string;
		mLastResult: int;
		mLoadingFromJournal: boolean;
		mBlockRequests: boolean;
		mWaitingUpdate: boolean;
		mEntered: boolean;

		procedure NextMirrorRequest;
		procedure AddABranch(const rep: cXmlParser);
		// function   IncludeNode( pt : apNode; req : cDbTree ) : boolean;
		// function   DoSearchOp( pt, apt : apNode; req, ans : cDbTree ) : boolean;
		function ReadOp(req: cDbTree; pBase: apNode): string;
		function ScanOp(req: cDbTree; pBase: apNode): string;
		// function   SearchOp( req : cDbTree ) : string;
		// procedure  Replicate( sNode, dNode : apNode );
		procedure NewOp(req: cDbTree; pBase: apNode; lPath: TStringList;
		  deleteFirst: boolean = false);
		procedure RenameOp(req: cDbTree; pBase: apNode; lPath: TStringList);
		procedure DeleteOp(req: cDbTree; pBase: apNode; lPath: TStringList);
		procedure EditOp(req: cDbTree; pBase: apNode; lPath: TStringList);
		procedure ProcessLogin(const fromNet: string); // : string;
		function ValidateUser(req: cXmlParser): boolean; // todo
		procedure HandleAEditRequest(req: cDbTree);
		function HandleADataRequest(req: cDbTree): string;
		procedure SlaveHandler(const reply: string);
		procedure ProcessJournal;
		procedure AddToJournal(request: string);
		procedure SetHub(hub: cMessageHub);
		function GetReady: boolean;
		procedure SetReady(rdy: boolean);
	public
		ErrorHandler: procedure(reply: string; rep: cXmlParser; id: string;
		  er: int) of object;
		WaitForCompletion: boolean;
		CanMaster: boolean;
		OnEvent: apConnectionEventReader;

		property OnMasterEdit: aOnEditNotify read mOnMasterEdit
		  write mOnMasterEdit;
		property id: string read mID write mID;
		property hub: cMessageHub read mHub write SetHub;
		property Ready: boolean read GetReady write SetReady;

		procedure InitMirror(paths: TStringList);
		procedure CancelMirror;
		procedure RegisterReader(const rdr: aDeltaNotify);
		function DeRegisterReader(const rdr: aDeltaNotify): Integer;
		procedure ClearAllReaders;
		function LocalRequestHandler(const request: string;
		  link: apLinkID): string;
		procedure RequestHandler(const request: string; link: apLinkID);
		// aReader - all net DBreq comes via here
		// function   SendRequest( const mesg : string;  wait : boolean = false;  lid : apLinkID = nil ) : integer;
		function Master: boolean;
		procedure BroadcastRequest(r: string; id: string = ''); virtual;
		// now can block
		procedure GlobalEdit(pBase: apNode; NodeName, val: string;
		  id: string = ''); overload;
		procedure GlobalEdit(basePath: string; NodeName, val: string;
		  id: string = ''); overload;

		// procedure  GlobalEdit( pBase : apNode;  NodeName, val, old : string; id : string = '' );   overload;
		procedure GlobalRename(pBase: apNode; newName: string; id: string = '');
		procedure GlobalDelete(pBase: apNode; id: string = '');
		procedure GlobalFlag(basePath: string; name: string; state: boolean;
		  id: string = ''); overload;
		procedure GlobalFlag(pBase: apNode; name: string; state: boolean;
		  id: string = ''); overload;
		procedure InitJournal;
		procedure FlushJournal;
		function Broadcast(const xml: string): Integer;
		// local result distribution
		procedure LoadObject(obj: TObject; baseNode: apNode); overload;
		procedure LoadObject(obj: TObject; basePath: string); overload;
		procedure PersistObject(obj: TObject; baseNode: apNode); overload;
		procedure PersistObject(obj: TObject; basePath: string); overload;

		procedure SaveFiles;
	end;

	ifDB = record // light weight interface generic connect to tree DB stuff
		DB: cMirrorDB;
		id: string; // location ID
	end;

	// iDB = interface           // generic connect to tree DB stuff for forms
	// function	GetDB() : cMirrorDB;
	// function	GetID() : string;
	// end;

	cMultiThreadMirrorDB = class;
	apWorkTrigger = procedure(sender: cMultiThreadMirrorDB) of object;

	aUpdate = record
		mID: string;
		mRequest: string;
	end;

	apUpdate = ^aUpdate;

	cMultiThreadMirrorDB = class(cMirrorDB)
	// DB cach - for multi thread readers like http server and formatter
		constructor Create;
		destructor Destroy; override;
	private
		oUpdateQ: TQueue<string>;
		oOutgoingUpdates: TQueue<aUpdate>;
		// StringList;   // reader thread deferred updates
		oOutgoingLock: TCriticalSection;
		mDB: cMirrorDB;
		procedure UpdateRequest(const xml: string); // aDeltaNotify
		function GetOutgoingUpdates: int;
	public
		Mode: (modLocked, modSync);
		state: (staIdle, staBusy);
		oLock: TMultiReadExclusiveWriteSynchronizer;
		OnStartWork: apWorkTrigger;
		procedure BroadcastRequest(req, id: string); override;
		// worker threads directs updates into queue
		procedure FlushOutgoingUpdates; // main thread
		procedure FlushUpdateQ;
		procedure CopyDB(DB: cMirrorDB);
		property OutgoingUpdates: int read GetOutgoingUpdates;
	end;

var
	OpReadTagName: array [aDBReadop] of string = (
		'????',
		TagRead,
		TagScan
	);
	OpEditTagName: array [aDBEditop] of string = (
		'????',
		TagNewTag,
		TagRenameTag,
		TagDelete,
		TagEdit
	);

function CollectList(pt: apNode): TStringList;
function DbDateToStr(dt: string): string;

implementation

uses
	Forms, Windows, Dialogs, TypInfo, uUtils, uPoller, uPacket, ASCII;

const
	SysID = '_Alpha_'; // backdoor password sshhhhhh !
	JournalExt = '.jnl';

	// if mp.mesgPipeID > High( mPipeLogins ) then  SetLength( mPipeLogins, mp.mesgPipeID * 2 );
	// if mPipeLogins[ mp.mesgPipeID ] <> '' then  begin

function DbDateToStr(dt: string): string;

begin

	if Length(dt) >= 8 then
	begin
		if dt[7] = '0' then
			result := Copy(dt, 7, 2)
		else
			result := Copy(dt, 7, 2);
		result := result + '/';
		if dt[5] = '0' then
			result := result + Copy(dt, 5, 2)
		else
			result := result + Copy(dt, 5, 2); // can be 'TBA' etc
		result := result + '/' + Copy(dt, 1, 4);
	end
	else
		result := dt;
end;

constructor cMultiThreadMirrorDB.Create;
// for multi thread readers like http server and formatter

begin
	inherited;
	oLock := TMultiReadExclusiveWriteSynchronizer.Create;
	oUpdateQ := TQueue<string>.Create;
	oOutgoingUpdates := TQueue<aUpdate>.Create;
	// reader thread deferred updates
	oOutgoingLock := TCriticalSection.Create;
	CanMaster := false;
end;

destructor cMultiThreadMirrorDB.Destroy;

begin
	oLock.Free;
	oUpdateQ.Free;
	oOutgoingUpdates.Free;
	oOutgoingLock.Free;
	inherited;
end;

procedure cMultiThreadMirrorDB.CopyDB(DB: cMirrorDB);

begin
	CopySubs(DB.GetRoot);
	mDB := DB;
	mDB.RegisterReader(UpdateRequest); // call update request on deltas
end;

procedure cMultiThreadMirrorDB.FlushUpdateQ; // main thread updates MT data here

var
	req: string;
begin
	oLock.BeginWrite;
	try
		begin
			while oUpdateQ.Count > 0 do
			begin
				req := oUpdateQ.Dequeue;
				LocalRequestHandler(req, nil);
			end;
		end;
	finally
		oLock.EndWrite;
	end;
end;

procedure cMultiThreadMirrorDB.BroadcastRequest(req, id: string); // override

var
	request: aUpdate; // so global edit etc will work
begin
	request.mID := id;
	request.mRequest := req;
	oOutgoingLock.Acquire;
	try
		oOutgoingUpdates.Enqueue(request);
	finally
		oOutgoingLock.Release;
	end;
end;

procedure cMultiThreadMirrorDB.FlushOutgoingUpdates;
// typicaly called by main thread to batch update

var
	request: aUpdate; // so global edit etc will work
	wfc: boolean;
begin
	wfc := mDB.WaitForCompletion;
	mDB.WaitForCompletion := true;
	try
		begin
			// updates will trickle through to MT copy of DB via  cMultiThreadMirrorDB.UpdateRequest
			while oOutgoingUpdates.Count > 0 do
			begin
				oOutgoingLock.Acquire;
				try
					request := oOutgoingUpdates.Dequeue;
				finally
					oOutgoingLock.Release;
				end;
				mDB.BroadcastRequest(request.mRequest, request.mID);
			end;
		end;
	finally
		mDB.WaitForCompletion := wfc;
	end;
end;

procedure cMultiThreadMirrorDB.UpdateRequest(const xml: string);
// aDeltaNotify called in response to global db updates to copy updates into copy MT db

var
	prevReq: string;
begin
	if Mode = modSync then
	begin
		if state = staIdle then
		begin
			while oUpdateQ.Count > 0 do
			begin
				prevReq := oUpdateQ.Dequeue;
				LocalRequestHandler(prevReq, nil);
			end;
			LocalRequestHandler(xml, nil);
			state := staBusy;
			if Assigned(OnStartWork) then
				OnStartWork(self);
		end
		else
			oUpdateQ.Enqueue(xml);
	end

	else
	begin // modLock    locking mode so lock out reader threads during updates
		oLock.BeginWrite;
		try
			SlaveHandler(xml);
		finally
			oLock.EndWrite;
		end;
	end;
end;

function cMultiThreadMirrorDB.GetOutgoingUpdates: int;

begin
	result := oOutgoingUpdates.Count;
end;

// ______________________________________ SINGLE THREAD __________________________________________

constructor cMirrorDB.Create(hub: cMessageHub; id: string;
  pCanMaster: boolean = false; isKey: boolean = true; treeID: word = 0;
  log: aLogProc = nil);

begin
	inherited Create(treeID, isKey, log);
	mHub := hub;
	if hub <> nil then
		mHubID := hub.RegisterReader(RequestHandler)
	else
		mMirrorState := msDone; // stand alone loaded
	CanMaster := pCanMaster;
	mRequesterID := id;
	oMirrorList := TStringList.Create;
	// mMirrorState := msDone;
	oLoggedIn := TStringList.Create;
	oLoggedIn.Sorted := true;
	oDeltaNotify := TList.Create;
	// MessageHub.mMirrorDB := self;
end;

constructor cMirrorDB.Create; // overload;  copy/slave version

begin
	inherited Create(69, true);
	oDeltaNotify := TList.Create;
	CanMaster := true;
end;

procedure cMirrorDB.SetHub(hub: cMessageHub);

begin
	mHub := hub;
	if hub <> nil then
		mHubID := hub.RegisterReader(RequestHandler);
end;

destructor cMirrorDB.Destroy;

var
	rp: apDeltaNotify;
	x: int;
begin
	if mJournalCount > 0 then
		try
			CloseFile(mJournalFile);
		except
		end;
	FreeAndNil(oLoggedIn);
	FreeAndNil(oMirrorList);
	for x := 0 to oDeltaNotify.Count - 1 do
	begin
		rp := oDeltaNotify[x];
		Dispose(rp);
	end;
	FreeAndNil(oDeltaNotify);
	inherited Destroy;
end;

procedure cMirrorDB.ClearAllReaders;

var
	rp: apDeltaNotify;
	x: int;
begin
	if oDeltaNotify <> nil then
	begin
		for x := 0 to oDeltaNotify.Count - 1 do
		begin
			rp := oDeltaNotify[x];
			Dispose(rp);
		end;
		oDeltaNotify.Clear;
	end;
end;

procedure cMirrorDB.RegisterReader(const rdr: aDeltaNotify);
// to catch 'local' DB deltas   : integer

var
	rp: apDeltaNotify;
	// p : pointer;
begin
	New(rp); // := AllocMem( SizeOf( aDeltaNotify ) );
	rp^ := rdr;
	// was result := oDeltaNotify.Add( rp );
	oDeltaNotify.Insert(0, rp);
	// build reverse list to allow for reverse reading order
end;

function cMirrorDB.DeRegisterReader(const rdr: aDeltaNotify): Integer;

var // aDeltaNotify = procedure( const xml : string ) of object;
	// apDeltaNotify = ^ aDeltaNotify;
	rp: apDeltaNotify;
	x: int;
begin
	result := -1;
	if oDeltaNotify <> nil then
	begin
		for x := 0 to oDeltaNotify.Count - 1 do
		begin
			rp := oDeltaNotify[x];
			// if rp = @rdr then  begin     NFG
			if (TMethod(rp^).Code = TMethod(rdr).Code) and
			  (TMethod(rp^).Data = TMethod(rdr).Data) then
			begin
				Dispose(rp);
				oDeltaNotify.Delete(x);
				result := x;
				break;
			end;
		end;
	end;
end;

function cMirrorDB.Broadcast(const xml: string): Integer;

var
	i: Integer;
	p: apDeltaNotify;
begin
	result := -1;
	if not mEntered then
	begin
		try
			begin
				mEntered := true;
				i := 0;
				if oDeltaNotify <> nil then
				begin
					i := oDeltaNotify.Count - 1;
					while (i >= 0) and (i < oDeltaNotify.Count) do
					begin // tolerant to list changing
						p := oDeltaNotify[i];
						try
							p^(xml);
						finally
							Dec(i);
						end;
					end;
				end;
				result := i;
			end;
		finally
			mEntered := false;
		end;
	end;
end;

procedure cMirrorDB.CancelMirror;

begin
	Clear;
	oMirrorList.Clear;
	ClearAllReaders;
end;

procedure cMirrorDB.InitMirror(paths: TStringList);

var
	path: string;
begin
	Clear;
	if oMirrorList = nil then
		oMirrorList := TStringList.Create
	else
		oMirrorList.Clear;
	// oMirrorList.Sort;
	if Assigned(paths) then
	begin
		for path in paths do
		begin
			oMirrorList.Add(path);
		end;
	end
	else
		oMirrorList.Add(LSep); // default is root. ie all

	mMirrorState := msInit; // 0 ie first in list
	oMirrorList.Sort; // so Find can work
	NextMirrorRequest;
end;

procedure cMirrorDB.NextMirrorRequest;

var // requests a scan of the next required tree branch for mirroring
	path: string;
	xml: string;
begin // do scans to initialize data tree
	while mMirrorState < oMirrorList.Count do
	begin // more branches to load
		path := oMirrorList[mMirrorState];
		if path <> '' then
		begin // skip any blanks
			xml := FormatADataRequest(false, TagScan, mRequesterID, '', path);
			mHub.Broadcast(xml);
			break;
		end;
		Inc(mMirrorState);
	end;

	if mMirrorState >= oMirrorList.Count then
	begin
		mMirrorState := msDone;
		InitJournal; // slaves have db now
	end;
end;

procedure cMirrorDB.AddABranch(const rep: cXmlParser);

var
	pt, pResult, pPath: apNode;
begin
	// if this is expected scan response      todo handle sub branches
	if mMirrorState < msDone then
	begin
		pt := FollowPath(TagDataReply + LSep + TagResult + LSep + TagScan,
		  rep.GetRoot); // rep.FindNodeMulti( rep.GetRoot, TagScan );
		if pt <> nil then
		begin
			pResult := pt.Back;
			if { ( pResult <> nil ) and ( pResult.NodeName = 'Result' ) and ( pResult.Content = 'OK' ) and }
			  (pResult.SubNodes <> nil) and (pResult.SubNodes.Count >= 2) then
			begin
				pPath := FindName(pResult, TagPath); // .SubNodes[ 1 ];
				if (pPath <> nil) and
				  (pPath.Content = oMirrorList[mMirrorState]) then
				begin
					pt := pResult.Back;
					if (pt <> nil) and (pt.SubNodes <> nil) and
					  (pt.SubNodes.Count >= 2) then
					begin
						while pt.SubNodes.Count > 1 do
							MoveBranch(pt.SubNodes[1]);
					end;
					if mMirrorState < oMirrorList.Count - 1 then
					begin
						Inc(mMirrorState);
						NextMirrorRequest;
					end
					else
					begin
						mMirrorState := msDone;
						if Assigned(OnEvent) then
							OnEvent(ceDbReady, '');
						Broadcast('');
					end;
				end;
			end;
		end;
	end;
end;

// journal relies on |SystemConfig|Journal| and having  InitJournal after load, and FlushJournal after files save

const
	JournalMask = '*' + JournalExt;

procedure cMirrorDB.ProcessJournal;

var // Accs.jnl exists then FlushJournal didn't run ( ie crash ) so apply previous changes
	f: TextFile;
	xml, ln: string;
begin
	AssignFile(f, mJournalDir + mJournalName + JournalExt);
	Reset(f);
	try
		begin
			mLoadingFromJournal := true; // prevent broadcast of journal changes
			while not EOF(f) do
			begin
				ReadLn(f, ln);
				if Pos('<EditReply>', ln) = 1 then
				begin
					xml := ln;
					while not EOF(f) do
					begin
						ReadLn(f, ln);
						xml := xml + ln;
						if Pos('</EditReply>', ln) = 1 then
						begin
							SlaveHandler(xml);
							break;
						end
						else if ln[1] <> tab then
							break; // dud request
					end;
				end;
			end;
		end;
	finally
		mLoadingFromJournal := false;
		CloseFile(f);
	end;
end;

procedure cMirrorDB.InitJournal;

var
	val: string;
begin
	if HasContent('|SystemConfig|Journal|Number|', val) then
	begin
		if TryStrToInt(val, mJournalCount) then
		begin
			if not HasContent('|SystemConfig|Journal|Directory|',
			  mJournalDir) then
				mJournalDir := Directory;
			if not HasContent('|SystemConfig|Journal|Name|', mJournalName) then
				mJournalName := 'Journal';
			if FileExists(mJournalDir + mJournalName + JournalExt) then
			begin
				ProcessJournal;
				FlushJournal;
			end;
			AssignFile(mJournalFile, mJournalDir + mJournalName + JournalExt);
			Rewrite(mJournalFile);
		end;
	end;
end;

procedure cMirrorDB.AddToJournal(request: string);

var
	ts: string;
begin
	if mJournalCount <> 0 then
	begin
		ts := TimeStamp;
		Write(mJournalFile, ts);
		WriteLn(mJournalFile, request);
	end;
end;

procedure cMirrorDB.FlushJournal;

var // manage numbered set of archived journal files   eg FIDS-3.jnl
	fn: string;
	n: int;
begin
	if mJournalCount <> 0 then
	begin
		try
			CloseFile(mJournalFile)
		except
		end; // may not be open
		if CountFiles(mJournalDir, mJournalName + JournalMask) >
		  mJournalCount then
		begin // too many so replace oldest
			fn := OldestFile(mJournalDir, JournalMask, true);
			// true => ignore current journal
			if fn = '' then
				fn := mJournalName + JournalExt;
			fn := mJournalDir + fn;
			DeleteFile(PChar(fn));
		end
		else
		begin // one after newest count
			fn := NewestFile(mJournalDir, mJournalName + JournalMask, true);
			if fn = '' then
				fn := mJournalName + '-' + JournalExt;
			n := AnyDecimal(fn) + 1;
			fn := ExtractOnlyFileName(fn); // core name only eg 'Journal-'
			while FileExists(mJournalDir + fn + IntToStr(n) + JournalExt) do
				Inc(n);
			fn := mJournalDir + fn + IntToStr(n) + JournalExt;
		end;
		RenameFile(mJournalDir + mJournalName + JournalExt, fn);
	end;
end;


// _____________________________ Req handler ___________________________________

{ procedure  CopyNode( const src : aNode; var dest : aNode );

  begin
  // dest.NodeName := src.NodeName;  done by build
  dest.Content := src.Content;
  // dest.Attribs := src.Attribs;
  // dest.fPtr    := src.fPtr;
  // dest.KeyNode  := src.KeyNode;    todo Table <> nil
  end; }

function CollectList(pt: apNode): TStringList;

begin
	if pt <> nil then
		result := StrToPath(pt.Content)
	else
		result := TStringList.Create;
end;

{
  function   cMirrorDB.IncludeNode( pt : apNode; req : cDbTree ) : boolean;

  var
  x : integer;
  ok : boolean;
  begin
  ok := true;
  if req.SubNodes.Count <> 0 then  begin // must be in the required tags list
  ok := req.SubNodes.Find( pt.NodeName, x );
  end;
  if ok then  begin
  if req.mContentMatchL.Count <> 0 then  begin // matchl must be in tag content
  ok := false;
  for x := 0 to req.mContentMatchL.Count - 1 do  begin
  if Pos( req.mContentMatchL[ x ], pt.Content ) > 0 then  begin
  ok := true;
  break;
  end;
  end;
  end;
  if ok then  begin
  if req.mNodeMatchL.Count <> 0 then  begin // matchl must be in tag name
  ok := false;
  for x := 0 to req.mNodeMatchL.Count - 1 do  begin
  if Pos( req.mNodeMatchL[ x ], pt.NodeName ) > 0 then  begin
  ok := true;
  break;
  end;
  end;
  end;
  end;
  end;
  Result := ok;
  end; }

function cMirrorDB.ReadOp(req: cDbTree; pBase: apNode): string;

var
	pDest: apNode;
	x, d: Integer;
	pt: apNode;
	r: string;
begin
	result := '';
	if pBase <> nil then
	begin
		pDest := GetNode(ReadContent(pBase, TagPath));
		if pDest <> nil then
		begin
			d := 1;
			if pDest.Back <> nil then
			begin
				r := StartPath(d, pDest.Back);
				r := r + FormatATag(pDest, d, true);
			end;
			if pDest.SubNodes <> nil then
			begin
				for x := 0 to pDest.SubNodes.Count - 1 do
				begin
					pt := pDest.SubNodes[x];
					r := r + BracketATag(pt.NodeName, ttEmpty, d + 1) + EOL;
					{ r := r + FormatATag( pt, d + 1, true );
					  r := r + FormatATag( pt, d + 1, false ); }
				end;
			end;
			if pDest.Back <> nil then
			begin
				r := r + FormatATag(pDest, d, false);
				r := r + uXmlParser.EndPath(d, pDest.Back);
			end;
			result := r;
		end;
	end;
end;

function cMirrorDB.ScanOp(req: cDbTree; pBase: apNode): string;

var
	d: Integer;
	pDest: apNode;
begin
	result := '';
	if pBase <> nil then
	begin
		pDest := GetNode(ReadContent(pBase, TagPath));
		if pDest <> nil then
		begin
			d := 1;
			result := '';
			if pDest.Back <> nil then
				result := StartPath(d, pDest.Back);
			result := result + FormatAllNodes(pDest, d);
			d := 1;
			if pDest.Back <> nil then
				result := result + uXmlParser.EndPath(d, pDest.Back);
		end;
	end;
end;

{ function   cMirrorDB.DoSearchOp( pt, apt : apNode; req, ans : cDbTree ) : boolean;
  pt=this tag in data, apt=ans parent
  if terminal
  if  incl
  make new answer and copy
  retn true        to keep
  else
  pass back false   to maybe demolish temp answer chain
  else
  make temp answer tag
  if not ( keep deeper  or incl(this) )
  dispose
  pass back false
}
{ var
  pa : apNode;  // p to current answer tag
  pn : apNode;  // p to next downstream data
  x : integer;
  keep, k : boolean;
  begin
  Result := false;
  if pt.SubNodes = nil then  begin        // if terminal
  if IncludeNode( pt, req ) then  begin   // if keep me
  // keep := true;
  pa := ans.NewNode( pt.NodeName, pt.Attribs, apt );
  if pa <> nil then  begin
  CopyNode( pt^, pa^ );
  Result := true;                      // and pass back keep
  end;
  end;
  end
  else  begin                            // not terminal
  pa := ans.NewNode( pt.NodeName, pt.Attribs, apt );   // make temp answer tag to chain to possible deeper answers
  // CopyNode( pt^, pa^ );  defer
  keep := IncludeNode( pt, req );
  for x := 0 to pt.SubNodes.Count - 1 do  begin
  pn := pt.SubNodes[ x ];
  k := DoSearchOp( pn, pa, req, ans );
  keep := keep or k;
  end;
  if keep then  begin
  Result := true;
  CopyNode( pt^, pa^ );  // defered keepind ans tag so fill it in
  end
  else  begin
  ans.DisposeNode( pa );
  end;
  end;
  end;


  function  cMirrorDB.SearchOp( req : cDbTree ) : string;

  var
  x : integer;
  pt, pBase, pAnsBase : apNode;
  ans : cDbTree;
  begin
  pBase := mBasePath;
  ans := cDbTree.Create( 6 );
  ans.mBasePath := ans.ForcePath( mPathL );  // force a corresponding path in the ans set
  pAnsBase := ans.mBasePath;
  if ( pBase <> nil ) and ( pAnsBase <> nil ) then  begin
  req.mNodeL.Sort;  //  do faster tag name matching
  if pBase.SubNodes <> nil then  begin
  for x := 0 to pBase.SubNodes.Count - 1 do begin
  pt := pBase.SubNodes[ x ];
  DoSearchOp( pt, pAnsBase, req, ans );
  end;
  end;
  end;
  result := ans.FormatAllNodes( nil, 1 );
  ans.Free;
  end; }

{ procedure  cMirrorDB.Replicate( sNode, dNode : apNode );   // $$$ todo use copybranch

  var
  x : integer;
  ps, pt : apNode;
  begin
  if ( sNode <> nil ) and ( dNode <> nil ) then  begin
  if sNode.SubNodes <> nil then  begin
  for x := 0 to sNode.SubNodes.Count - 1 do  begin
  ps := sNode.SubNodes[ x ];
  pt := CopyNode( ps, dNode ); //NewNode( ps.NodeName, ps.Table <> nil, dNode );
  //if pt <> nil then  begin
  //CopyNode( ps^, pt^ );
  Replicate( ps, pt );
  end;
  end;
  end;
  end;
  end; }

procedure cMirrorDB.NewOp(req: cDbTree; pBase: apNode; lPath: TStringList;
  deleteFirst: boolean = false);

var
	atrib, nam, cont: string;
	pNew, pDest, pName: apNode;
begin
	if pBase <> nil then
	begin
		pName := FindName(pBase, TagNewTag);
		if NodeContent(pName) <> '' then
		begin // ( pPath <> nil ) and (   )
			pDest := ForcePath(lPath);
			nam := MakeTagNameLegal(ReadContent(pBase, TagNewTag), true);
			if (pDest <> nil) and (nam <> '') then
			begin
				atrib := ReadContent(pBase, 'Attributes');
				cont := ReadContent(pBase, 'Content');
				if deleteFirst then
				begin
					DeleteAllFast(FindName(pDest, nam));
				end;
				pNew := NewNode(nam, atrib, pDest);
				if pNew <> nil then
				begin
					if (pNew.NodeName <> nam) and Master then
					begin // auto renamed so fix reply so slaves see same key
						pName.Content := pNew.NodeName;
					end;
					pNew.Content := cont;
					Inc(mNewOpNodeCount);
					CopySubs(pName, pNew); // Replicate( request tree to new );
				end;
			end;
		end
		else
			LogEr(erBadReqNew, 'Invalid ReqNewTag request');
	end;
end;
{ <EditRequest>
  <ReqNewTag> QF127-20090810
  <STD> 20090810 120000 </STD>
  <ETD> 20090810 120700 </ETD>
  <Ports> SYD,MEL </Ports>
  <Gates> 13 </Gates>
  <DStatus> Boarding </DStatus>

  <Flights>
  <QF127/>
  </Flights>
  </ReqNewTag>
  <Attributes> KeyTag="true" </Attributes>
  <Content> some content </Content>
  <Path> |Departures| </Path>
  <ReqID> GUI </ReqID>
  </EditRequest> }

procedure cMirrorDB.RenameOp(req: cDbTree; pBase: apNode; lPath: TStringList);

var
	nam: string;
	pName, pDest: apNode; // , pPath
begin
	if pBase <> nil then
	begin
		// pPath := FindName( pBase, TagPath );
		pName := FindName(pBase, TagRenameTag);
		if NodeContent(pName) <> '' then
		begin // ( pPath <> nil ) and (  )
			pDest := FollowPath(lPath, GetRoot);
			// GetNode( NodeContent( pPath ) );
			nam := MakeTagNameLegal(NodeContent(pName), true);
			if (nam <> '') and (pDest <> nil) and (pDest.Back <> nil) then
			begin
				if pDest.Back.Table <> nil then
				begin // does hash table need changing
					if CanAdd(nam, pDest.Back) then
					begin
						pDest.Back.Table.Delete(pDest);
						pDest.NodeName := nam;
						pDest.Back.Table.Add(pDest); // was   pPath
					end
					else
						LogEr(erRenToNonuniqueKeyNode,
						  'RenameOp ' + lPath[lPath.Count - 1] + ' to ' + nam);
				end
				else
					pDest.NodeName := nam;
				Inc(mEditCount);
				pDest.Modified := Seconds;
			end;
		end
		else
			LogEr(erRenToEmpty, 'RenameOp To ""');
	end
	else
		LogEr(erRenNonExistent, 'RenameOp badly formed or Non Existent Path');
end;

procedure cMirrorDB.DeleteOp(req: cDbTree; pBase: apNode; lPath: TStringList);

var
	pDest: apNode;
begin
	if pBase <> nil then
	begin
		pDest := FollowPath(lPath, GetRoot);
		// GetNode( ReadContent( pBase, TagPath ) );
		if (pDest <> nil) and (pDest.Back <> nil) then
		begin // don't delete root
			DeleteAllFast(pDest);
		end;
	end
	else
		LogEr(erDelNonExistent, 'DeleteOp Non Existent Node');
end;

procedure cMirrorDB.EditOp(req: cDbTree; pBase: apNode; lPath: TStringList);
// if <Prev> provided it must match old content

var
	pPrev, pDest: apNode;
	New: string;
	go: boolean;
begin
	if pBase <> nil then
	begin
		pDest := FollowPath(lPath, GetRoot);
		// GetNode( ReadContent( pBase, TagPath ) );
		if pDest <> nil then
		begin
			pPrev := FindName(pBase, 'Prev');
			if pPrev <> nil then
			begin // must have correct old value - external consistency check
				go := NodeContent(pDest) = NodeContent(pPrev);
			end
			else
				go := true;
			New := ReadContent(pBase, TagEdit);
			if go then
			begin
				pDest.Content := New;
				pDest.Modified := Seconds;
				Inc(mEditCount);
			end;
		end
		else
			LogEr(erEditNonExistent, 'EditOp NonExistent');
	end;
end;
{ <EditReply>
  <Result> OK
  //<ReqEdit> |Hello again|Hello| </ReqEdit>     old list style - obs
  <ReqEdit> Hello again </ReqEdit>
  <Prev> Hello </Prev>
  <Path> |Users|Alphasoft|PassWord| </Path>
  <ReqID> 2 </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqEdit>  |Hello again|Hello| </ReqEdit>
  <Path>  |Users|Alphasoft|PassWord| </Path>
  <ReqID> 2 </ReqID>
  </EditRequest>
}

{ function   cMirrorDB.SetReadOperationAndOptions( req : cDbTree ) : aDBReadop;

  var
  o : aDBReadop;
  pt, pr : apNode;
  begin
  pr := req.GetRoot;                      // get operation type
  Result := opNone;
  for o := Low( aDBReadop ) to High( aDBReadop ) do  begin
  if o <> opNone then  begin
  pt := req.FindNodeMulti( pr, OpReadTagName[ o ] );
  if pt <> nil then  begin
  mOptionsL := CollectList( pt );
  Result := o;
  req.mCur := pt;        // $$$ redundant ?
  //req.mRequestNode := pt.NodeName;
  break;
  end;
  end;
  end;
  pt := req.FindNodeMulti( pr, TagPath );        // get parameter lists
  mPathL := CollectList( pt );
  mBasePath := FollowPath( mPathL, GetRoot );       // find base path in data - adjusted for new and edit
  end; }

function cMirrorDB.HandleADataRequest(req: cDbTree): string;

var
	pr, pBase: apNode;
begin
	result := '';
	pr := req.GetRoot; // handle either a request or result mesg
	pBase := FindName(pr, TagDataRequest);
	if pBase = nil then
	begin
		pBase := FollowPath('|DataReply|Result|', pr);
	end;

	if pBase <> nil then
	begin // despatch to appropriate handler
		if FindName(pBase, TagRead) <> nil then
			result := ReadOp(req, pBase)
		else if FindName(pBase, TagScan) <> nil then
			result := ScanOp(req, pBase)
		else
			LogEr(erUndefinedDataOp, 'Undefined Data Op');
	end;
end;

procedure cMirrorDB.HandleAEditRequest(req: cDbTree);

var
	pr, pBase, pPath: apNode;
	lPath: TStringList;
	x: int;
begin
	pr := req.GetRoot; // handle either a request or result mesg
	pBase := FindName(pr, TagEditRequest);
	if pBase = nil then
	begin
		pBase := FollowPath('|EditReply|Result|', pr); // adapt to result format
	end;

	if pBase <> nil then
	begin // despatch to appropriate handler
		mRequesterID := ReadContent(pBase, 'ReqID');
		mLastID := mRequesterID;

		pPath := FindName(pBase, TagPath);
		// check path is mirrored here to exclude extraneous errors
		if pPath <> nil then
		begin
			lPath := StrToPath(pPath.Content);
			// if ( lPath <> nil ) and ( lPath.Count > 0 ) then  begin
			if Master or (oMirrorList.Count > 0) and
			  ((oMirrorList[0] = LSep) or (oMirrorList.Find(lPath[0], x))) then
			begin // edit path is mirrored

				if FindName(pBase, TagEdit) <> nil then
					EditOp(req, pBase, lPath)
				else if FindName(pBase, TagNewTag) <> nil then
					NewOp(req, pBase, lPath)
				else if FindName(pBase, TagReplaceTag) <> nil then
					NewOp(req, pBase, lPath, true)
				else if FindName(pBase, TagDelete) <> nil then
					DeleteOp(req, pBase, lPath)
				else if FindName(pBase, TagRenameTag) <> nil then
					RenameOp(req, pBase, lPath)
				else
					LogEr(erUndefinedEditOp, 'Undefined Edit Op');
			end;
			// end;
			lPath.Free;
		end;
	end;
	mWaitingUpdate := false;
end;

function cMirrorDB.ValidateUser(req: cXmlParser): boolean; // todo

var
	pNode: apNode;
	x: int;
begin
	result := false;
	pNode := FindNodeMulti(req.GetRoot, TagReqID);
	if pNode <> nil then
	begin
		if pNode.Content = SysID then
			result := true
		else if oLoggedIn <> nil then
		begin
			result := oLoggedIn.Find(pNode.Content, x);
		end
		else
			result := true; // $$$ todo debug bypass login
	end;
	if not result then
		mEr := erUserNotLoggedIn;
end;

procedure cMirrorDB.BroadcastRequest(r: string; id: string = '');

var
	tick: int;
begin
	if mHub <> nil then
	begin
		if not mBlockRequests then
		begin
			mLastID := '';
			mHub.Broadcast(r, nil); // broadcast response to all
			if not self.Master and WaitForCompletion then
			begin
				tick := 0;
				if id = '' then
					id := mID;
				mBlockRequests := true;
				while mLastID <> id do
				begin
					Sleep(10);
					Application.ProcessMessages;
					Inc(tick);
					if tick > 500 then
					begin
						mLastResult := erConnectionTimeOut;
						LogEr(erConnectionTimeOut, 'Connection TimeOut');
						break;
					end;
				end;
				mBlockRequests := false;
			end;
		end
		else
		begin
			ShowMessage('DB still busy !');
		end;
	end
	else
		LocalRequestHandler(r, nil); // stand alone mode
end;

procedure cMirrorDB.GlobalEdit(basePath: string; NodeName, val: string;
  id: string = '');

var
	r: string;
	pn, pBase: apNode;
begin
	pBase := FollowPath_(basePath);
	pn := FindName(pBase, NodeName);
	if id = '' then
		id := mID;
	if pn <> nil then
	begin // already exists
		if (pn.Content <> val) then
		begin // ignore nop changes
			r := FormatShortEditRequest(basePath + NodeName + '|', val, id);
			// SysID
		end;
	end
	else
	begin
		r := StartRequestNew(NodeName);
		r := EndRequestNew(r, basePath, '', val, id); // SysID
	end;
	if r <> '' then
		BroadcastRequest(r, id);
	// mHub.Broadcast( r, nil );  // broadcast response to all
end;

procedure cMirrorDB.GlobalEdit(pBase: apNode; NodeName, val: string;
  id: string = '');

var
	r: string;
	pn: apNode;
begin
	pn := FindName(pBase, NodeName);
	if id = '' then
		id := mID;
	if pn <> nil then
	begin // already exists
		if (pn.Content <> val) then
		begin // ignore nop changes
			r := FormatShortEditRequest(ResolvePathStr(pBase) + NodeName, val,
			  id); // SysID
		end;
	end
	else
	begin
		r := StartRequestNew(NodeName);
		r := EndRequestNew(r, ResolvePathStr(pBase), '', val, id); // SysID
	end;
	if r <> '' then
		BroadcastRequest(r, id);
	// mHub.Broadcast( r, nil );  // broadcast response to all
end;


// procedure  cMirrorDB.GlobalEdit( pBase : apNode;  NodeName, val, old : string; id : string );
//
// var
// r : string;
// pn : apNode;
// begin
// pn := FindName( pBase, NodeName );
// if pn <> nil then  begin   // already exists
// if ( pn.Content <> val )  then  begin  // ignore nop changes
// r := FormatEditRequest( ResolvePathStr( pBase ) + NodeName, val, old, id ); // SysID
// end;
// end
// else  begin
// StartRequestNew( NodeName );
// r := EndRequestNew( ResolvePathStr( pBase ), '', val, id ); // SysID
// end;
// if id = '' then  id := mID;
// if r <> ''  then  BroadcastRequest( r, id );  // mHub.Broadcast( r, nil );
// end;

procedure cMirrorDB.GlobalRename(pBase: apNode; newName: string;
  id: string = '');

var
	r: string;
begin
	if (pBase <> nil) and (NodeName(pBase) <> newName) then
	begin // already exists
		if id = '' then
			id := mID;
		r := FormatRenameRequest(ResolvePathStr(pBase), newName, id);
		// was SysID
		if r <> '' then
			BroadcastRequest(r, id); // mHub.Broadcast( r, nil );
	end;
end;

procedure cMirrorDB.GlobalDelete(pBase: apNode; id: string = '');

var
	r: string;
begin
	if (pBase <> nil) and (pBase <> GetRoot) then
	begin
		if id = '' then
			id := mID;
		r := FormatDelete(ResolvePathStr(pBase), id);
		BroadcastRequest(r, id);
	end;
end;

procedure cMirrorDB.GlobalFlag(basePath: string; name: string; state: boolean;
  id: string = '');

var
	r, path: string;
begin
	if (basePath <> '') then
	begin
		if id = '' then
			id := mID;
		path := basePath + name + '|';
		if state then
		begin
			if FollowPath_(path) = nil then
			begin // one flag is enough
				r := StartRequestNew(Name);
				r := EndRequestNew(r, basePath, '', '', id);
			end;
		end
		else
		begin
			if FollowPath_(path) <> nil then
			begin // one flag is enough
				r := FormatDelete(path, id);
			end;
		end;
		if r <> '' then
			BroadcastRequest(r, id);
	end;
end;

procedure cMirrorDB.GlobalFlag(pBase: apNode; name: string; state: boolean;
  id: string = '');

var
	r: string;
begin
	if (pBase <> nil) and (pBase <> GetRoot) then
	begin
		if id = '' then
			id := mID;
		if state then
		begin
			if FindName(pBase, name) = nil then
			begin // one flag is enough
				r := StartRequestNew(Name);
				r := EndRequestNew(r, ResolvePathStr(pBase), '', '', id);
			end;
		end
		else
		begin
			if FindName(pBase, name) <> nil then
			begin // one flag is enough
				r := FormatDelete(ResolvePathStr(pBase) + name + '|', id);
			end;
		end;
		if r <> '' then
			BroadcastRequest(r, id);
	end;
end;

procedure cMirrorDB.LoadObject(obj: TObject; baseNode: apNode);

var // http://delphi.about.com/od/windowsshellapi/a/reader-writer.htm
	lPropInfo: PPropInfo;
	Count, p, v: Integer;
	lPropList: PPropList;
	lPropType: PPTypeInfo;
	val, nam: string;
begin // persists published properties if {$TYPEINFO ON}
	if baseNode <> nil then
	begin
		Count := GetPropList(PTypeInfo(obj.ClassInfo), lPropList);
		for p := 0 to Count - 1 do
		begin
			lPropInfo := lPropList^[p];
			nam := string(lPropInfo.Name);
			val := ReadContent(baseNode, nam);
			lPropType := lPropInfo^.PropType;
			if lPropType^.Kind = tkUString then
				SetPropValue(obj, nam, val)
			else if lPropType^.Kind = tkInteger then
			begin
				if TryStrToInt(val, v) then
					SetPropValue(obj, nam, v);
			end
			else if lPropType^.Kind = tkEnumeration then
			begin // assume boolean
				SetPropValue(obj, nam, Ord(FindName(baseNode, nam) <> nil));
			end;
		end;
	end;
end;

procedure cMirrorDB.LoadObject(obj: TObject; basePath: string);

begin
	LoadObject(obj, FollowPath_(basePath));
end;

procedure cMirrorDB.PersistObject(obj: TObject; baseNode: apNode);

var // http://delphi.about.com/od/windowsshellapi/a/reader-writer.htm
	lPropInfo: PPropInfo;
	Count, p: Integer;
	lPropList: PPropList;
	lPropType: PPTypeInfo;
	nam: string;

begin
	if baseNode <> nil then
	begin
		Count := GetPropList(PTypeInfo(obj.ClassInfo), lPropList);
		for p := 0 to Count - 1 do
		begin
			lPropInfo := lPropList^[p];
			nam := string(lPropInfo.Name);
			lPropType := lPropInfo^.PropType;
			if lPropType^.Kind = tkUString then
				GlobalEdit(baseNode, nam, GetPropValue(obj, lPropInfo))
			else if lPropType^.Kind = tkInteger then
			begin
				GlobalEdit(baseNode, nam, GetPropValue(obj, lPropInfo));
			end
			else if lPropType^.Kind = tkEnumeration then
			begin // assume boolean
				GlobalFlag(baseNode, nam, GetPropValue(obj, lPropInfo));
			end;
		end;
	end;
end;

procedure cMirrorDB.PersistObject(obj: TObject; basePath: string);

var // http://delphi.about.com/od/windowsshellapi/a/reader-writer.htm
	lPropInfo: PPropInfo;
	Count, p: Integer;
	lPropList: PPropList;
	lPropType: PPTypeInfo;
	nam: string;
begin
	Count := GetPropList(PTypeInfo(obj.ClassInfo), lPropList);
	for p := 0 to Count - 1 do
	begin
		lPropInfo := lPropList^[p];
		nam := string(lPropInfo.Name);
		lPropType := lPropInfo^.PropType;
		if lPropType^.Kind = tkUString then
			GlobalEdit(basePath, nam, GetPropValue(obj, lPropInfo))
		else if lPropType^.Kind = tkInteger then
		begin
			GlobalEdit(basePath, nam, GetPropValue(obj, lPropInfo));
		end
		else if lPropType^.Kind = tkEnumeration then
		begin // assume boolean
			GlobalFlag(basePath, nam, GetPropValue(obj, lPropInfo));
		end;
	end;
end;

procedure cMirrorDB.ProcessLogin(const fromNet: string);

var
	access: string;
	nam, pw, id: string;
	req: cXmlParser;
	pt, pb, pUsrName: apNode;
	accept: boolean;
	x, er: int;
	me: aLinkID;
begin // process <LoginRequest> .....
	if Master and (fromNet <> '') then
	begin // mHub.
		// hd := '';
		accept := false;
		req := cXmlParser.Create;
		req.LoadFromString(fromNet);
		pb := req.FindNodeMulti(nil, TagLoginRequest);
		if pb <> nil then
		begin
			pt := req.FindNodeMulti(pb, TagReqID);
			if pt <> nil then
			begin
				id := pt.Content; // location of requesting id

				pt := req.FindNodeMulti(pb, TagUserName);
				if pt <> nil then
				begin
					nam := pt.Content;

					pUsrName := FollowPath('|Users|' + nam + LSep, GetRoot);
					// NOTE name cannot have a space
					if pUsrName <> nil then
					begin // user name found in DB

						pt := req.FindNodeMulti(pb, TagLogout); // log out
						if pt <> nil then
						begin
							if oLoggedIn.Find(id, x) then
							begin
								oLoggedIn.Delete(x);
								GlobalEdit(pUsrName, 'LoggedOut',
								  ShortDateTime(Now) + '  from ' + id, SysID);
								LogEr(stLogOut, 'Log Out  ID ' + id);
							end;
						end

						else
						begin // log in

							pt := req.FindNodeMulti(pb, TagPassWord);
							// todo NOTE pw should be pre hashed before here
							if pt <> nil then
							begin
								pw := pt.Content;
								pt := FindName(pUsrName, TagPassWord);
								if pt <> nil then
									if pt.Content = pw then
									begin // valid name & password
										pw := '';
										pt := FindName(pUsrName, 'Access');
										if pt <> nil then
										begin
										access := pt.Content; // access rights
										if access <> '' then
										begin
										LogEr(stLogin, 'Login ' + nam + ' ID ' +
										id); // if mLog <> nil then  mLog
										GlobalEdit(pUsrName, 'LoggedIn',
										ShortDateTime(Now) + '  from ' +
										id, SysID);
										GlobalEdit(pUsrName, TagReqID,
										id, SysID);
										// login := '<LogIn>' + id + ':' + access + '</LogIn>';
										// mHub.Broadcast( login );
										accept := true;
										end;

										end;
									end;

							end
							else if (pw = SysID) or (pw = '_Alphasoft_') then
							begin
								accept := true;
								access := '9,*'; // result := nam;
							end;
						end;
					end;
					if not accept then
					begin // failed attempt
						LogEr(wnLogin, 'FAILED Login attempt ' + nam +
						  ' ID ' + id);
						er := wnLogin;
					end
					else
					begin
						er := erNone;
						if oLoggedIn = nil then
						begin
							oLoggedIn := TStringList.Create;
							oLoggedIn.Sorted := true;
						end;
						if not oLoggedIn.Find(id, x) then
							oLoggedIn.Add(id);
					end;
					if mHub <> nil then
					begin
						me.fLink := alLocal;
						me.fID := mHubID;
						mHub.Broadcast(FormatLogInReply(id, access, er), @me);
						// broadcast response to all but me
					end;
				end;
			end;
		end;
	end;
end;


// function   cMirrorDB.SendRequest( const mesg : string;  wait : boolean = false;  lid : apLinkID = nil ) : integer;
//
// var
// count : int;
// begin
// result := -1;
// if mesg <> '' then  begin
// result := mHub.Broadcast( mesg, lid );
// if wait then  begin    // wait for response
// mWaitingUpdate := true;   count := 100;
// while mWaitingUpdate and ( count > 0 ) do  begin
// Sleep( 2 );
// Application.ProcessMessages;
// Dec( count );
// end;
// end;
// end;
// end;

function cMirrorDB.Master: boolean;

begin
	if Assigned(mHub) then
		result := mHub.Master and CanMaster
	else
		result := true; // mCanMaster;
end;

function cMirrorDB.GetReady: boolean;

begin
	// result := mHub.Master or mHub.Connected and ( oMirrorList.Count > 0 ) and ( mMirrorState = msDone );   // finished initial scans
	result := mMirrorState = msDone;
end;

procedure cMirrorDB.SetReady(rdy: boolean);

begin
	if rdy then
		mMirrorState := msDone;
	// typically loaded from files - master or standalone
end;

procedure cMirrorDB.RequestHandler(const request: string; link: apLinkID);
// all DBreq comes via here

var
	reply: string;
	me: aLinkID;
begin
	if Master then
	begin
		reply := LocalRequestHandler(request, link);
		if (mHub <> nil) and (reply <> '') then
		begin
			me.fLink := alLocal;
			me.fID := mHubID;
			mHub.Broadcast(reply, @me); // broadcast response to all but me
		end;
	end
	else
		SlaveHandler(request);
end;

procedure cMirrorDB.SlaveHandler(const reply: string);

var
	rep: cXmlParser;
	edReply, edResult: apNode;
	// intro : string;
	er: int;
begin
	mEr := 0;
	if Length(reply) > Length(TagEditReply) * 4 then
	begin
		// intro := uUtils.Slice( reply, 1, Length( TagEditReply ) * 4 );
		if PosN(TagEditReply, reply, 60) > 0 then
		begin // intro
			rep := cXmlParser.Create(20);
			rep.LoadFromString(reply);
			// if ResultOK( rep.GetRoot ) then  begin
			edReply := FindName(rep.GetRoot, 'EditReply');
			if edReply <> nil then
			begin
				edResult := FindName(edReply, 'Result');
				if edResult <> nil then
				begin
					er := AnyDecimal(NodeContent(edResult));
					mLastID := ReadContent(edResult, 'ReqID');
					if er = 0 then
					begin // good result pkt so do edit here to achieve mirror
						HandleAEditRequest(rep);
						if not mLoadingFromJournal then
						begin
							Broadcast(reply);
							// distribute change notification locally
							if mJournalCount <> 0 then
								AddToJournal(reply);
						end;
					end
					else if not mLoadingFromJournal and
					  Assigned(ErrorHandler) then
					begin // respond to incoming error mesg
						ErrorHandler(reply, rep, mLastID, er);
					end;
				end
			end;
			rep.Free;
		end
		else if mMirrorState < msDone then
		begin // load up initial scans ie <DataReply> messages
			if PosN(TagDataReply, reply, 60) > 0 then
			begin
				rep := cXmlParser.Create(GetRoot.id, false, log);
				if rep.LoadFromString(reply) then
				begin
					if ResultOK(rep.GetRoot) then
					begin
						AddABranch(rep);
						// Broadcast( reply );   // wake up initial data display
					end;
				end;
				rep.Free;
			end;
		end;
	end;
end;

function cMirrorDB.LocalRequestHandler(const request: string; link: apLinkID)
  : string; // all DBreq comes via here

var
	req: cXmlParser;
	pr, pNode: apNode;
	response: string;
	intro, id: string;
	ed: boolean;
begin
	result := '';
	req := nil;
	ed := false;
	mEr := 0;
	if Length(request) > Length(TagDataRequest) * 4 then
	begin
		intro := Slice(request, 1, Length(TagDataRequest) * 2);

		if Pos(TagDataRequest, intro) > 0 then
		begin
			req := cXmlParser.Create(10, false, log);
			req.LoadFromString(request);
			ed := false;
			mEr := req.mEr;
			pr := req.GetRoot;
			if (mEr = 0) and (pr <> nil) and (pr.SubNodes <> nil) and
			  (pr.SubNodes.Count > 0) then
			begin
				pNode := pr.SubNodes[0];
				// todo handle multiple requests in one xml ie ACID
				if pNode.NodeName = TagDataRequest then
				begin
					response := HandleADataRequest(req);
				end;
			end;
		end
		else if Pos(TagEditRequest, intro) > 0 then
		begin
			req := cXmlParser.Create(11, false, log);
			req.LoadFromString(request);
			mEr := req.mEr;
			ed := true;
			if (mEr = 0) and ValidateUser(req) then
			begin
				// pr := req.GetRoot;
				// if ( pr <> nil ) and ( pr.SubNodes <> nil ) and ( pr.SubNodes.Count > 0 ) then  begin
				// pNode := pr.SubNodes[ 0 ];
				pNode := FindName(req.GetRoot, TagEditRequest);
				if pNode <> nil then
				begin
					HandleAEditRequest(req);
					if Assigned(mOnMasterEdit) then
						mOnMasterEdit(req);
				end;
			end;
			if mEr <> 0 then
				log(mEr, 'EditRequest : ' +
				  NodeContent(req.FollowPath_('|EditRequest|ReqID|')));
		end
		else if Pos(TagLoginRequest, intro) > 0 then
		begin
			ProcessLogin(request);
		end;

		if req <> nil then
		begin
			result := req.FormatADataReply(ed, mEr) + response +
			  req.FormatEndDataReply(ed);
			if ed then
			begin
				Broadcast(result);
				if mJournalCount <> 0 then
					AddToJournal(result);
			end;
			if (mEr <> 0) and
			  (Assigned(ErrorHandler) and not mLoadingFromJournal) then
			begin
				req.HasContent('|EditRequest|ReqID|', id);
				ErrorHandler(result, req, id, mEr);
			end;

			// mOptionsL.Free;   	mOptionsL := nil;
			// mPathL.Free;        mPathL := nil;
			req.Free;
		end;
	end;
end;

procedure cMirrorDB.SaveFiles;

var
	saves: TStringList;
	s, name: string;
	x: int;
begin
	if HasContent('|SystemConfig|Save|', s) then
	begin
		x := 1;
		saves := BuildParamsL(s, x);
		for name in saves do
		begin
			SaveDBToFile(FindName(GetRoot, name));
		end;
	end
	else
		SaveDBToFiles('*');
end;

{ sample traffic

  <TimeStamp> 20110428 114141 </TimeStamp>    // journal
  <EditReply>
  <Result> OK
  <ReqEdit> |Parts|0603|PCB|Description| </ReqEdit>
  <Prev> Parts|ST0.2|R_4 </Prev>
  <Path> |PCBs|AS710|PartsList|CL|PartPath| </Path>
  <ReqID> GT9 </ReqID>
  </Result>
  </EditReply>

  <DataReply>
  <Result> OK
  <ReqRead> | </ReqRead>
  <Path> |Users|Alphasoft| </Path>
  <ReqID> 2 </ReqID>
  </Result>
  <Users>
  <Alphasoft>
  <PassWord/>
  <Access/>
  <LoggedIn/>
  </Alphasoft>
  </Users>
  </DataReply>
  <DataRequest>
  <ReqRead>  | </ReqRead>
  <Path>  |Users|Alphasoft| </Path>
  <ReqID> 2 </ReqID>
  </DataRequest>


  <EditReply>
  <Result> OK
  <ReqEdit> SIN,ADL </ReqEdit>
  <Prev> SIN </Prev>
  <Path> |Departures|SIA238-1923|Flights|SIA238|Ports| </Path>
  <ReqID> EgGUI </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqEdit> SIN,ADL </ReqEdit>
  <Prev> SIN </Prev>
  <Path> |Departures|SIA238-1923|Flights|SIA238|Ports| </Path>
  <ReqID> EgGUI </ReqID>
  </EditRequest>

  <EditReply>
  <Result> ERROR 105
  <ReqEdit> |5|| </ReqEdit>
  <Path> |Departures|ST1234-20090114|Bay| </Path>
  <ReqID> 2 </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqEdit>  |5|| </ReqEdit>
  <Path>  |Departures|ST1234-20090114|Bay| </Path>
  <ReqID> 2 </ReqID>
  </EditRequest>


  <EditReply>
  <Result> OK
  <ReqDelete/>
  <Path> |DisplayConfig|Groups|Dep-Pic| </Path>
  <ReqID> 13 </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqDelete/>
  <Path> |DisplayConfig|Groups|Dep-Pic| </Path>
  <ReqID> 13 </ReqID>
  </EditRequest>

  <EditReply>
  <Result> OK
  <ReqNewTag>
  |Dep-Pic|
  <Format> Departures,1,5 </Format>
  <Format> Departures,2,5 </Format>
  <Format> Departures,3,5 </Format>
  <Image> |Graphics|GIFs|Misc|ttopcl.gif,5 </Image>
  </ReqNewTag>
  <Path> |DisplayConfig|Groups| </Path>
  <ReqID> 13 </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqNewTag> |Dep-Pic|
  <Format> Departures,1,5</Format>
  <Format> Departures,2,5</Format>
  <Format> Departures,3,5</Format>
  <Image> |Graphics|GIFs|Misc|ttopcl.gif,5</Image>
  </ReqNewTag>
  <Path>|DisplayConfig|Groups|</Path>
  <ReqID> 13 </ReqID>
  </EditRequest>

  out <<  <LoginRequest>
  <ReqID> 9901 </ReqID>
  <UserName> Feed </UserName>
  <PassWord> _DI_system_ </PassWord>
  </LoginRequest>

  out <<  <EditRequest>
  <ReqDelete/>
  <Path> |Departures| </Path>
  <ReqID> 9901 </ReqID>
  </EditRequest>

  in >>  <LoginReply> Success   <Access> 9,* </Access> </LoginReply>


  in >>  <EditReply>
  <Result> OK
  <ReqDelete/>
  <Path> |Departures| </Path>
  <ReqID> 9901 </ReqID>
  </Result>
  </EditReply>


  <LoginReply>
  <Result> OK </Result>
  <Access> 9,* </Access>
  <ReqID> HTTP_GT </ReqID>
  </LoginReply>
  <LoginRequest>
  <ReqID> HTTP_GT </ReqID>
  <UserName> GT </UserName>
  <PassWord> itsme </PassWord>
  </LoginRequest>

  <EditReply>
  <Result> OK
  <ReqEdit> |20081223 083100|20081223 083000| </ReqEdit>
  <Path> |Departures|JQ666-20081223|ETD| </Path>
  <ReqID> GUI </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqEdit> |20081223 083100|20081223 083000| </ReqEdit>
  <Path> |Departures|JQ666-20081223|ETD| </Path>
  <ReqID> GUI </ReqID>
  </EditRequest>

  <EditReply>
  <Result> OK
  <ReqNewTag>
  |QF127-20090810|
  <STD> 20090810 120000 </STD>
  <ETD> 20090810 120700 </ETD>
  <Ports> SYD,MEL </Ports>
  <Gates> 13 </Gates>
  <DStatus> Boarding </DStatus>
  <Flights>
  <QF127/>
  </Flights>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> GUI </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqNewTag> |QF127-20090810|
  <STD> 20090810 120000 </STD>
  <ETD> 20090810 120700 </ETD>
  <Ports> SYD,MEL </Ports>
  <Gates> 13 </Gates>
  <DStatus> Boarding </DStatus>

  <Flights>
  <QF127/>
  </Flights>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> GUI </ReqID>
  </EditRequest>

  <EditReply>
  <Result> OK
  <ReqNewTag> |GT15||yet another| </ReqNewTag>       // note auto numbered flight key GT/GT15
  <Path> |Departures| </Path>
  <ReqID> DBView </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqNewTag> |GT||yet another| </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> DBView </ReqID>
  </EditRequest>

  <EditReply>
  <Result> OK
  <ReqNewTag>
  |GT314-44|||
  <Flights>
  <GT314>
  <Ports> PER </Ports>
  <CheckIns> 4 </CheckIns>
  </GT314>
  </Flights>
  <DStatus> Closed </DStatus>
  <ST> 20100406 220000 </ST>
  <ETdata> 20100220 </ETdata>
  <Gates> 3 </Gates>
  <Comment/>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> HTTP_Server </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqNewTag> |GT314-|||
  <Flights>
  <GT314>
  <Ports> PER </Ports>
  <CheckIns> 4 </CheckIns>
  </GT314>
  </Flights>
  <DStatus> Closed </DStatus>
  <ST> 20100406 220000 </ST>
  <ETdata> 20100220 </ETdata>
  <Gates> 3 </Gates>
  <Comment/>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> HTTP_Server </ReqID>
  </EditRequest>

  <EditReply>          // short form - and uses SysID instant in
  <Result> OK
  <ReqEdit> |HTTP_ST| </ReqEdit>
  <Path> |Users|ST|ReqID| </Path>
  <ReqID> _DI_system_ </ReqID>
  </Result>
  </EditReply>

  <EditReply>
  <Result> OK
  <ReqNewTag>
  |JST6681-285|||
  <Flights>
  <JST6681>
  <Ports> ADL </Ports>
  </JST6681>
  </Flights>
  <ST> 20100802 111000 </ST>
  <ET> 20100802 110900 </ET>
  <AT> 20100802 1123 </AT>
  <Rego> VHTJI </Rego>
  <Carrier> Domestic </Carrier>
  <AirCraft> 734 </AirCraft>
  <StaffComment> 144 </StaffComment>
  <Public/>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> Feed </ReqID>
  </Result>
  </EditReply>
  <EditRequest>
  <ReqNewTag> |JST6681-|||
  <Flights>
  <JST6681>
  <Ports> ADL </Ports>
  </JST6681>
  </Flights>
  <ST> 20100802 111000 </ST>
  <ET> 20100802 110900 </ET>
  <AT> 20100802 1123 </AT>
  <Rego> VHTJI </Rego>
  <Carrier> Domestic </Carrier>
  <AirCraft> 734 </AirCraft>
  <StaffComment> 144 </StaffComment>
  <Public/>
  </ReqNewTag>
  <Path> |Departures| </Path>
  <ReqID> Feed </ReqID>
  </EditRequest>
}

end.
