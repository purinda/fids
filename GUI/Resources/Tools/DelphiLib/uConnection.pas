unit uConnection;

// hack this unit suit your application

interface

uses
	Forms, uMessageHub, uMirrorDB, uXmlParser, uGlobalDefs, uGT;

type

	aConnectionType = ( ctNone, ctLocalClient, ctLocalServer, ctRemoteClient, ctServer, ctMultiServer );

	cConnection = class
    	constructor	Create( log : aLogProc );
        destructor	Destroy;  override;
        private
            //mShutDown, mInit : boolean;
            oHub : cMessageHub;
            // oDB : cMirrorDB;
            mLog : aLogProc;
            mPrevIdler	: TIdleEvent;
            mServerHasShutDown : boolean;
            function	GetConnected : boolean;
            function	GetMaster : boolean;
			procedure	IdleEventHandler(Sender: TObject; var Done: Boolean);
            //procedure	SelfDestruct(Sender: TObject; var Done: Boolean);
			procedure	Incoming( const mesg : string; link : apLinkID = nil );
			procedure	NewConnection;
        public
            ConnectionType : aConnectionType;          // property
            //MyName		: string;
            ServerName	: string;
            PipeName	: string;
            UserName	: string;
            Password	: string;
            MyIpAddr	: string;
            PortNo		: int;
            iDB         : ifDB;
            OnConnect	: aDeltaNotify;  // procedure of object;
            OnServerShutDown : procedure of object;
            oLocalDB	: cXmlParser;
            procedure	LogIn;
			procedure   LogOut;
			procedure	Connect;
			procedure	Close;
        	property	DB : cMirrorDB  read iDB.DB;
            property	LocationID : string  read iDB.ID;
            property	Connected : boolean  read GetConnected; // ;   oHub.Connected
            property	Master : boolean  read GetMaster;
    	end;

var
	Xml_Connection : cConnection = nil;


implementation  // ______________________________________________________________


uses
	SysUtils, Classes, Dialogs, uPoller, uPacket, uUtils, uDbTree;
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


// ______________________________________________________________________________


constructor	cConnection.Create( log : aLogProc );

    var                         // find machine specific and connection info from LocalConfig.xml
    	value : string;
        host, ip, er : string;
        machine, id, pt : apNode;
        x : int;
	begin
    mLog := log;
    oHub := cMessageHub.Create( log );
    GetIPFromHost( host, ip, er );
    if er <> '' then  ShowMessage( er );
    MyIpAddr := ip;
    //iDB.ID := locationID;
    iDB.DB := cMirrorDB.Create( oHub, locationID, true, true, 42, log );  // might not want can master in pure slaves/GUI
    oLocalDB := cXmlParser.Create;
    oLocalDB.LoadFromFile( 'LocalConfig.xml' );		// get local application info
    oLocalDB.HasContent( '|LocalConfig|XmlDirectory|', iDB.DB.Directory );// get default directory
    // if locationID = '' then  begin  // not explicitly set by application so get location id from config file

    machine := oLocalDB.GetNode( '|LocalConfig|Machine|' );
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
        id := oLocalDB.GetNode( '|LocalConfig|Machine|' + host );   // use computer name values from config
        if id = nil then  id := iDB.DB.GetNode( '|LocalConfig|Machine|' + host ); // else use defaults
        end;

    ConnectionType := ctLocalClient;
    if id <> nil then  begin
        if FindName( id, 'MultiServer' ) <> nil then  ConnectionType := ctMultiServer
        else if FindName( id, 'SingleServer' ) <> nil then  ConnectionType := ctServer
        else if FindName( id, 'LocalServer' ) <> nil then  ConnectionType := ctLocalServer
        else if FindName( id, 'Client' ) <> nil then  ConnectionType := ctRemoteClient;
        PipeName := ReadContent( id,  'PipeName' );
        UserName := ReadContent( id,  'UserName' );  // not for 'secure' applications
        Password := ReadContent( id,  'Password' );
        iDB.ID := ReadContent( id,  'LocationID' );  // not for 'secure' applications
        end;

    if oLocalDB.HasContent( '|LocalConfig|Server|Port|', value ) then  TryStrToInt( value, PortNo );
    if PortNo = 0 then  PortNo := DefaultPort;
    if PipeName = '' then  oLocalDB.HasContent( '|LocalConfig|PipeName|', PipeName );    // set default values if any still blank
    if UserName = '' then  oLocalDB.HasContent( '|LocalConfig|UserName|', UserName );
    if Password = '' then  oLocalDB.HasContent( '|LocalConfig|Password', Password );
    if iDB.ID = '' then  oLocalDB.HasContent( '|LocalConfig|LocationID', iDB.ID );
    if iDB.ID = '' then  iDB.ID := host;   // no other name value so settle for PC name
    oLocalDB.HasContent( '|LocalConfig|Server|Name|', ServerName );
    end;


destructor	cConnection.Destroy;  // override;

	begin
    Close;
    FreeAndNil( oHub );
    FreeAndNil( oLocalDB );
    FreeAndNil( iDB.DB );
    end;


procedure  cConnection.Connect;

    var
        timeout : boolean;
	begin
    mServerHasShutDown := false;
    oHub.OnConnection := NewConnection;
    timeout := false;
    case ConnectionType of
        ctLocalClient	: begin
			oHub.InitConnection( [ alLocal, alPipe ], PipeName, ServerName, ServerName,
            tcpNone, TStringList( nil ), PortNo );
            timeout := true;
            end;
        ctLocalServer	: begin
			oHub.InitConnection( [ alLocal, alPipe ], ServerName, '', '',
            tcpNone, TStringList( nil ), PortNo );
			Application.OnIdle := IdleEventHandler;    // wait for a quiet moment to announce all clear (all forms should be created)
            end;
        ctRemoteClient	: begin
			oHub.InitConnection( [ alPipe, alTCP ], PipeName, ServerName, ServerName,
            tcpClient, TStringList( nil ), PortNo );
            timeout := true;
            end;
        ctServer	: begin
			oHub.InitConnection( [ alLocal, alPipe, alTCP ], ServerName, '', ServerName,
            tcpMaster, TStringList( nil ), PortNo );
			mPrevIdler := Application.OnIdle;
			Application.OnIdle := IdleEventHandler;    // wait for a quiet moment to announce all clear (all forms should be created)
            end;
        ctMultiServer	: begin
			oHub.InitConnection( [ alLocal, alPipe, alTCP ], ServerName, '', ServerName,
            tcpMutiMaster, TStringList( nil ), PortNo );
            end;
    	end;
    if timeout then  begin
        Poller.OnTimeOut( 100,              // in 10 seconds time check the connection
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


procedure	cConnection.IdleEventHandler(Sender: TObject; var Done: Boolean );

	begin
    Application.OnIdle := mPrevIdler;
    NewConnection;  // internal db initialize
    if Assigned( OnConnect ) then  OnConnect( '' );  // signal server to start up
    done := true;
    end;


//procedure	cConnection.SelfDestruct(Sender: TObject; var Done: Boolean);
//
//	begin
//    Application.OnIdle := mPrevIdler;
//    done := true;
//    if mServerHasShutDown then  FreeAndNil( Xml_Connection );  // SELF DESTRUCT !!!
//    end;


procedure	cConnection.Incoming( const mesg : string; link : apLinkID = nil );

	begin
	if Length( mesg ) < 50 then  begin
        if Pos( MesgShutDown, mesg ) = 1 then  begin   //  <ShutDown/>
            if Assigned( OnServerShutDown ) then  OnServerShutDown;
        	if not Master then  begin
                mServerHasShutDown := true;
                iDB.DB.CancelMirror;
                //mPrevIdler := Application.OnIdle;
                //Application.OnIdle := SelfDestruct;
            	end;
        	end;
    	end;
    end;


procedure	cConnection.NewConnection;

    var
		sl : TStringList;
		x : int;
		fn : string;
    	value : string;
        found : boolean;
    begin
    oHub.RegisterReader( Incoming );
    if not DB.HasContent( 'SystemConfig', value ) then  begin
        if Master then  begin  // master only - system initial load from files
        	DB.LoadFromFile( 'SystemConfig.xml' );
            if DB.Error <> 0 then  ShowMessage( 'Problem loading file SystemConfig.xml' );
            if DB.HasContent( '|SystemConfig|XmlDirectory|', value ) then  DB.Directory := value;
			if DB.HasContent( '|SystemConfig|Load|', value ) then  begin  // list of xml files to load
                x := 1;
                sl := BuildParamsL( value, x );
                for fn in sl do  begin
                	DB.LoadFromFile( fn );
                    if DB.HasContent( '|LocalConfig|Server|Attended|', value ) then  begin
                    	if DB.Error <> 0 then  ShowMessage( 'Problem loading file ' + fn );
                    	end;
                	end;
                sl.Free;
                end;
            DB.InitJournal;
            DB.Broadcast( '' );  // signal anyone waiting for db ready
        	end
        else if Connected then  begin
    		LogIn;                    // clients load from master
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
    	oHub.Disconnect( true );
        if DB.HasContent( '|SystemConfig|Save|', value ) then  begin  // list of xml files to save
            x := 1;
            sl := BuildParamsL( value, x );
            for fn in sl do  DB.SaveDBToFiles( fn );
            sl.Free;
            end;
    	end;
    DB.FlushJournal;
    end;


procedure   cConnection.LogIn;

	begin
    oHub.Broadcast( FormatLogIn( UserName, Password, LocationID ) );
    end;


procedure   cConnection.LogOut;

	begin
    if ( oHub <> nil ) and not Master and oHub.Connected then  begin
    	oHub.Broadcast( FormatLogOut( UserName, LocationID ) );
    	end;
    end;


function	cConnection.GetConnected : boolean;

	begin
    result := false;
    if oHub <> nil then  result := oHub.Connected;
    end;


function	cConnection.GetMaster : boolean;

	begin
    result := false;
    if oHub <> nil then  result := oHub.Master;
    end;


end.
