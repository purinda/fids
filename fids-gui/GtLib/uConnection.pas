unit uConnection;

// uConnection waits till the application is finished initialization, reads LocalConfig.xml, connects then logs in automatically.
// The global Xml_Connection maintains the mirrored DB and interfaces to the message hub.


interface

uses
	Forms, Generics.Collections, uMessageHub, uMirrorDB, uXmlParser, uGT, uGlobalDefs; //  uGlobalDefs

type

	aConnectionType = ( ctNone, ctStandAlone, ctLocalClient, ctLocalServer, ctRemoteClient, ctServer, ctMultiServer );

	cConnection = class
    	constructor	Create;
        destructor	Destroy;  override;
        private
            oHub : cMessageHub;
            // mLog : aLogProc;
            mPrevIdler	: TIdleEvent;
            mOriginalDirectory : string;     // base directory - holds sysconfig and users
            mServerHasShutDown : boolean;
            oReaders	: TList< apConnectionEventReader >;
            mRegistryBase : string;
			procedure	OnConnectionEvent( event : aConnectionEvent; param : string );  // called by message hub when connection established
            function	GetConnected : boolean;
            function	GetMaster : boolean;
			procedure	NotifyEvent( event : aConnectionEvent; param : string );
			procedure	IdleEventHandler(Sender: TObject; var Done: Boolean);
			procedure	Incoming( const mesg : string; link : apLinkID = nil );
			procedure	NewConnection;
        public
            ConnectionType : aConnectionType;          // property
            ServerName	: string;
            UserName	: string;
    		AppName		: string;
            Password	: string;
            MyIpAddr	: string;
            PortNo		: int;
            iDB         : ifDB;
            OnConnect	: aDeltaNotify;  // procedure of object;
            OnServerShutDown : procedure of object;
            oLocalDB	: cXmlParser;
            Log 		: aLogProc;
            ShutDown	: boolean;
			procedure	ClientLogIn;
            procedure	LogIn;
			procedure   LogOut;
			procedure	InitConnection;
			procedure	Close;
            procedure	RegisterEventReader( reader : apConnectionEventReader );
        	property	DB : cMirrorDB  read iDB.DB;
            property	Hub : cMessageHub  read oHub;
            property	LocationID : string  read iDB.ID  write iDB.ID;
            property	Connected : boolean  read GetConnected; // ;   oHub.Connected
            property	Master : boolean  read GetMaster;

    	end;


function	Connect( reader : apConnectionEventReader = nil ) : cMirrorDb;
function	DB() : cMirrorDB;  inline;


var
	Xml_Connection : cConnection = nil;



implementation  // ______________________________________________________________

{$WARNINGS OFF }
uses
	SysUtils, Classes, Dialogs, FileCtrl, Windows, uPoller, uPacket,
    uUtils, uDbTree;

{$WARNINGS ON }
{
create
set mode
Connect hub
init db
< new connection
shutdown
destroy
}



const
	DefaultPort = 1666;

//var
//    conPrevAppIdle : TIdleEvent;


function	DB() : cMirrorDB;  inline;

	begin
    result := Xml_Connection.iDB.DB;
    end;


function	Connect( reader : apConnectionEventReader = nil ) : cMirrorDb;

	begin
    if Xml_Connection = nil then  begin
    	Xml_Connection:= cConnection.Create();
        if @reader <> nil then  Xml_Connection.RegisterEventReader( reader );
    	end;
    result := Xml_Connection.DB;
    end;


// _____________________________________________________________________________


constructor	cConnection.Create; // ( log : aLogProc );

    var                         // find machine specific and connection info from LocalConfig.xml
    	value, path : string;
        host, ip, er : string;
        machine, id, pt : apNode;
        x : int;
	begin
    path := ChangeFileExt( Application.ExeName, '' );
    AppName := ExtractFileName( path );   // strip path and .exe
    oReaders := TList< apConnectionEventReader >.Create;
    GetIPFromHost( host, ip, er );   // get machine name and IP addr
    if er <> '' then  ShowMessage( er );
    MyIpAddr := ip;
    iDB.DB := cMirrorDB.Create;
    oLocalDB := cXmlParser.Create;

    if FileExists( path + 'Config.XML' ) then  begin
    	if oLocalDB.LoadFromFile( path + 'Config.XML' ) = nil then  ShowMessage( AppName + 'Config.XML could not be loaded' );
    	end
    else  if oLocalDB.LoadFromFile( 'LocalConfig.xml' ) = nil then ShowMessage( 'LocalConfig not found' );		// get local application info
    oLocalDB.HasContent( '|LocalConfig|XmlDirectory|', iDB.DB.Directory );// get default directory
    if not oLocalDB.HasContent( '|LocalConfig|RegistryBase|', mRegistryBase ) then  mRegistryBase := 'Alphasoft\Default';

    machine := oLocalDB.GetNode( '|LocalConfig|Machine|' );     // find initialization by machine name or IP addr
    x := -1;    id := nil;
    while EachSubNode( machine, x, pt ) do  begin
        if NodeName( pt ) = 'IP' then  begin
            if NodeContent( pt ) = ip then  begin   // ip address match so use these id values
                id := pt;
                break;
                end;
            end;
        end;
    if id = nil then  begin   // try host name match
        x := -1;
        while EachSubNode( machine, x, pt ) do  begin
            if LowerCase( NodeName( pt ) ) = Lowercase( host ) then  begin
                id := pt;
                break;
                end;
	        end;

        if id = nil then  id := oLocalDB.GetNode( '|LocalConfig|Machine|Else|' ); // else use defaults
        if id = nil then  id := oLocalDB.GetNode( '|LocalConfig|Machine|Default|' ); // else use defaults
        end;

    ConnectionType := ctStandAlone;
    if id <> nil then  begin
        if FindName( id, 'MultiServer' ) <> nil then  ConnectionType := ctMultiServer
        else if FindName( id, 'SingleServer' ) <> nil then  ConnectionType := ctServer
        else if FindName( id, 'LocalServer' ) <> nil then  ConnectionType := ctLocalServer
        else if FindName( id, 'Client' ) <> nil then  begin
        	ConnectionType := ctRemoteClient;
            iDB.DB.CanMaster := false;
        	end
        else if FindName( id, 'StandAlone' ) <> nil then  ConnectionType := ctStandAlone;
        UserName := ReadContent( id,  'UserName' );  // not for 'secure' applications
        Password := ReadContent( id,  'Password' );
        iDB.ID := ReadContent( id,  'LocationID' );  // not for 'secure' applications
        end;

    if oLocalDB.HasContent( '|LocalConfig|Server|Port|', value ) then  TryStrToInt( value, PortNo );
    if PortNo = 0 then  PortNo := DefaultPort;
    if UserName = '' then  oLocalDB.HasContent( '|LocalConfig|UserName|', UserName );
    if Password = '' then  oLocalDB.HasContent( '|LocalConfig|Password', Password );
    if iDB.ID = '' then  oLocalDB.HasContent( '|LocalConfig|LocationID', iDB.ID );
    if iDB.ID = '' then  iDB.ID := host;   // no other name value so settle for PC name
    iDB.DB.ID := iDB.ID;  // default id for global edits
    oLocalDB.HasContent( '|LocalConfig|Server|Name|', ServerName );

    if ConnectionType <> ctStandAlone then  begin
        oHub := cMessageHub.Create( log );
        iDB.DB.Hub := oHub;
        oHub.OnConnection := OnConnectionEvent;
        oHub.RegisterReader( Incoming );
    	end;
    InitConnection;
    end;


destructor	cConnection.Destroy;  // override;

	begin
    Close;
    FreeAndNil( oHub );
    FreeAndNil( oLocalDB );
    FreeAndNil( iDB.DB );
    inherited Destroy;
    end;


procedure  cConnection.OnConnectionEvent( event : aConnectionEvent; param : string );  // called by message hub when connection established

	begin
    if event = ceConnected then  NewConnection;
    NotifyEvent( event, param );
    end;


procedure  cConnection.InitConnection;

//    var
//        timeout : boolean;
	begin
    if not Assigned( mPrevIdler ) then  begin
        mPrevIdler := Application.OnIdle;
        Application.OnIdle := IdleEventHandler;    // wait for a quiet moment to announce all clear (all forms should be created)
        end;
    end;


procedure	cConnection.IdleEventHandler(Sender: TObject; var Done: Boolean );

    var
        timeout : boolean;
	begin            // all forms should be created by now - safe to start talking
    Application.OnIdle := mPrevIdler;
    timeout := false;

    case ConnectionType of
        ctRemoteClient	: begin
			oHub.InitConnection( [ alTCP ], '', ServerName, ServerName,
            	tcpClient, TStringList( nil ), PortNo );
            timeout := true;
            end;
        ctServer	: begin
        	oHub.InitConnection( [ alLocal, alTCP ], ServerName, '', ServerName,
            tcpMaster, TStringList( nil ), PortNo );
            end;
        ctMultiServer	: begin
			oHub.InitConnection( [ alLocal, alTCP ], ServerName, '', ServerName,
            tcpMutiMaster, TStringList( nil ), PortNo );
            end;
        ctStandAlone	: begin
        	NewConnection;
        	end;
    	end;
    if timeout then  begin
        Poller.OnTimeOut( 50,              // in 5 seconds time check the connection
            procedure() begin  // aOnTimeOutProc
                if ( Xml_Connection <> nil ) and not mServerHasShutDown
                	and not oHub.Connected then  begin
                    if not oHub.Master then  begin
                        ShowMessage( 'Could not find server ' + ServerName );
                        end;
                    end;
                end
            );
    	end;
    end;


procedure	cConnection.Incoming( const mesg : string; link : apLinkID = nil );

	begin
    if PosN( MesgShutDown, mesg, 1 ) = 1 then  begin   //  <ShutDown/>
        NotifyEvent( ceShutdown, '' );
        end
    else if PosN( MesgEditReply, mesg, 1 ) = 1 then  begin
        NotifyEvent( ceEdit, mesg );
        end
    else if PosN( MesgLogin, mesg, 1 ) = 1 then  begin   //  Login
        NotifyEvent( ceLogin, mesg );
        end;
    end;


procedure	cConnection.ClientLogIn;


    var
		sl : TStringList;
		x : int;
		fn : string;
    	value : string;
        found : boolean;
	begin
    sl := nil;    x := 1;     // load all
    if oLocalDB.HasContent( '|LocalConfig|Mirror|', value ) then  begin
        sl := BuildParamsL( value, x );
        found := false;
        for fn in sl do  begin     // make sure SystemConfig is in the list
            if fn = 'SystemConfig' then  begin
                found := true;
                break;
                end;
            end;
        if not found then  sl.Insert( 0, 'SystemConfig' );
        end;
    DB.InitMirror( sl );  // maintain a local copy of the data base
    sl.Free;
    if Password <> '' then  begin
        LogIn;                    // clients load from master
        end;
    end;


procedure	cConnection.NewConnection;

    var
		sl : TStringList;
		x : int;
		fn : string;
    	value, dir, name : string;
    begin
    if not DB.HasContent( 'SystemConfig', value ) then  begin   // no sys config yet
        if Master then  begin  // master only - system initial load from files
        	DB.LoadFromFile( 'SystemConfig.xml' );
            if DB.Error <> 0 then  ShowMessage( 'Problem loading file SystemConfig.xml' );
            if DB.HasContent( '|SystemConfig|XmlDirectory|', value ) then  DB.Directory := value;   // static work directory
            mOriginalDirectory := DB.Directory;
        	DB.LoadFromFile( 'Users.xml' );
            if DB.HasContent( '|SystemConfig|PromptDirectory|', value ) then  begin     // user set working directory
                dir := ReadRegistry( mRegistryBase, 'WorkDirectory' );
                //if dir <> '' then  DB.Directory := name;
                name := dir;
                if SelectDirectory( name, [sdAllowCreate, sdPerformCreate, sdPrompt], 0 ) then  begin
                    name := name + '\';
                    if dir <> name then  begin    // new directory selected so save it back to registry
                        // SetContent( DB.FollowPath_( 'SystemConfig|PromptDirectory|' ), name );   // remember directory
                        // DB.SaveDBToFile( FindName( DB.GetRoot, 'SystemConfig' ) );
                        WriteRegistry( mRegistryBase, 'WorkDirectory', name );
                        dir := name;
                        end;
                    end;
                DB.Directory := dir;
            	end;
			if DB.HasContent( '|SystemConfig|Load|', value ) then  begin  // list of xml files to load
                x := 1;
                sl := BuildParamsL( value, x );
                for fn in sl do  begin
                	DB.LoadFromFile( fn );         // LOAD FILE LIST
                    if DB.HasContent( '|LocalConfig|Server|Attended|', value ) then  begin
                    	if DB.Error <> 0 then  ShowMessage( 'Problem loading file ' + fn );
                    	end;
                	end;
                sl.Free;
                end;
            DB.Ready := true;
            DB.InitJournal;
            NotifyEvent( ceDbReady, '' );
            DB.Broadcast( '' );  // signal anyone waiting for db ready
        	end
        else if Connected then  begin  // CLIENT
        	ClientLogIn;
//        	if Password <> '' then  begin
//                LogIn;                    // clients load from master
//                sl := nil;    x := 1;     // load all
//                if oLocalDB.HasContent( '|LocalConfig|Mirror|', value ) then  begin
//                    sl := BuildParamsL( value, x );
//                    found := false;
//                    for fn in sl do  begin     // make sure SystemConfig is in the list
//                        if fn = 'SystemConfig' then  begin
//                            found := true;
//                            break;
//                            end;
//                        end;
//                    if not found then  sl.Insert( 0, 'SystemConfig' );
//                    end;
//                DB.InitMirror( sl );  // maintain a local copy of the data base
//                sl.Free;
//                end;
        	end;
        end;
    end;


procedure cConnection.Close;

    var
		sl : TStringList;
		x : int;
		fn : string;
    	value : string;
    begin
    if Master then  begin
    	if oHub <> nil then  oHub.Disconnect( true );
        if DB.HasContent( '|SystemConfig|Save|', value ) then  begin  // list of xml files to save
            x := 1;
            sl := BuildParamsL( value, x );
            for fn in sl do  DB.SaveDBToFiles( fn );
            sl.Free;
            end;
        if mOriginalDirectory <> '' then  begin
            DB.Directory := mOriginalDirectory;
            DB.SaveDBToFiles( 'Users.xml' );
        	end;
    	end;
    DB.FlushJournal;
    end;


procedure	cConnection.RegisterEventReader( reader : apConnectionEventReader );

	begin
    oReaders.Add( reader );
    end;


procedure	cConnection.NotifyEvent( event : aConnectionEvent; param : string );

	var
    	x : int;
	begin
    for x := 0 to oReaders.Count - 1 do  begin
    	oReaders[ x ]( event, param );
    	end;
    end;


procedure   cConnection.LogIn;

	begin
    //oHub.Broadcast( FormatLogIn( UserName, Password, LocationID ) );
    DB.BroadcastRequest( FormatLogIn( UserName, Password, LocationID ), LocationID );
    end;


procedure   cConnection.LogOut;

	begin
    //if ( oHub <> nil ) and not Master and oHub.Connected then  begin
    	//oHub.Broadcast( FormatLogOut( UserName, LocationID ) );
    	//end;
    DB.BroadcastRequest( FormatLogOut( UserName, LocationID ), LocationID );
    end;


function	cConnection.GetConnected : boolean;

	begin
    result := false;
    if oHub <> nil then  result := oHub.Connected;
    end;


function	cConnection.GetMaster : boolean;

	begin
    result := false;
    if oHub <> nil then  result := oHub.Master
    else  if ConnectionType = ctStandAlone then  result := true;
    end;

(*
USEAGE
   DB := Connect;
   Xml_Connection.RegisterEventReader( ConnectionEvent );  // to be notified of connection events if required

procedure	TfMain.ConnectionEvent( event : aConnectionEvent; param : string );

	begin
    case event of
        ceNone: ;
        ceConnected	: ;
        ceDbReady	: InitMainForm;
        ceLogin		: HandleLogIn( param );
        ceEdit: ;
        ceDisconnected: ;
        ceShutdown	: Close;
    	end;
    end;



OP SEQUENCE

cConnection.Create
	oHub := cMessageHub.Create
    cMirrorDB.Create( oHub,..
    	if hub <> nil then  mHubID := hub.RegisterReader( RequestHandler );
    oLocalDB.LoadFromFile( 'LocalConfig.xml'

cConnection.Connect
    oHub.InitConnection

    cConnection.IdleEventHandler
        NewConnection
        Master - DB.LoadFromFile( 'SystemConfig.xml' );  etc
        Slave - InitMirror
        DB.Broadcast( '' );  // local signal anyone waiting for db ready - application start

        TfMain.InitMainForm( const xml : string );     // called when DB ready
            Xml_Connection.LogIn;
                oHub.Broadcast login req




*)

end.
