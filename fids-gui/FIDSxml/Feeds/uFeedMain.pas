unit uFeedMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids, StdCtrls, ExtCtrls,
  uGlobalDefs, uGT, uMessageHub, uDbTree, uMirrorDB, uTimeTable, uHttpServer,
  uUDCManagement, uKeyPadServer, uQantasFeedHandler;


const
	Server = 'FIDSxml';
	FeedID = 'Feed';
    SysPW  = 'DI_system';

type

	//aCol = ( cFlight, cSTD, cETD, cPorts, cGates, cDStatus );

	//aXML = string;

	TfFeedMain = class( TForm )
    	memoLog: TMemo;
    	MainMenu1: TMainMenu;
    	imetable1: TMenuItem;
    	Reset1: TMenuItem;
    	UDCs1: TMenuItem;
		procedure FormCreate(Sender: TObject);
    	procedure Reset1Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure UDCs1Click(Sender: TObject);
	public
    	procedure   Display( const src, op, name, std : string );
        procedure   LogIn( const usr, pw, id : string );
	private
        mUserID, mPassword : string;
		mShutDown, mInit : boolean;
        oDataTree : cMirrorDB;
        oHub : cMessageHub;
        oTimeTable : cTimeTable;
        oHttpServer : cHttpServer;
        oUDCManagement : cUDCManagement;
        oKeyPadServer : cKeyPadServer;
        oQantasFeedHandler : cQantasFeedHandler;
		procedure   fOnIdle(Sender: TObject; var Done: Boolean);
		procedure   Log( ErrorNo : integer; const s : string );
        procedure	WMKeyPad( var wm : TMessage );  message WM_KeyPad;
		procedure   MessageReader( const mesg : string; link : apLinkID = nil );  // aReader
		procedure   InitKeyPads;
		procedure   NewData( const xml : string );  // aDeltaNotify
		procedure   Connect();
		//procedure   LogOut( userID : string );
		procedure   NewConnection;
		end;

var
	fFeedMain: TfFeedMain;

implementation

{$R *.dfm}


uses  ASCII, uXmlParser, uUtils, uPoller, uPacket, uUDCs;



// ________________________ Form1 __________________________________


procedure TfFeedMain.FormClose(Sender: TObject; var Action: TCloseAction);

	begin
    if oHttpServer <> nil then  oHttpServer.ServerActive := false;  // todo free
    FreeAndNil( oKeyPadServer );
	end;


procedure TfFeedMain.FormCreate(Sender: TObject);

	begin                     // create 2 login IDs for journaling clarity
    mUserID := FeedID;
    mPassword := SysPW;
    oTimeTable := cTimeTable.Create;
    oTimeTable.Log := Log;
	Poller.OnTimeOut( 3,               // in 3/10 sec Connect   ie wait for create to finish
    	procedure() begin  //aOnTimeOutProc
            Connect;
            end
        );
    {mUserID := TimeTableID;
    mPassword := SysPW;
	LogIn;}
    Application.OnIdle := fOnIdle;
	end;


procedure   TfFeedMain.fOnIdle(Sender: TObject; var Done: Boolean);

	begin
    if mShutDown then  Close;
    end;


procedure	TfFeedMain.WMKeyPad( var wm : TMessage );  // message WM_KeyPad;

	begin
    if oKeyPadServer <> nil then  oKeyPadServer.WMKeyPad( wm );  // just pass it through
    end;


procedure   TfFeedMain.Display( const src, op, name, std : string );

	begin
    memoLog.Lines.Add( TimeDateToStr( Now ) + ' ' + TAB + src + TAB + op + TAB + name + TAB + std );
    end;


procedure   TfFeedMain.Log( ErrorNo : integer; const s : string );

	begin
    // memoLog.Lines.Add( InttoStr( ErrorNo ) + '  ' + s );
    if ErrorNo <> 0 then  Display( 'ER ' + IntToStr( ErrorNo ), s, '', '' )
    else  Display( s, '', '', '' );
	end;


procedure   TfFeedMain.MessageReader( const mesg : string; link : apLinkID = nil );  // aReader

    var
        lir : cXmlParser;
        pt : apNode;
	begin
	if mesg = '<' + TagShutDown + '/>' then  begin
		mShutDown := true;
		end;
    if ( Length( mesg ) < 1000 ) and ( Pos( TagLoginReply, mesg ) = 2 ) then  begin  // check log in response
        lir := cXmlParser.Create();
        lir.LoadFromString( mesg );
        pt := FindName( lir.GetRoot, 'LoginReply' );
        if ( ReadContent( pt, 'Result' ) = 'OK' ) then  begin
            if oHttpServer <> nil then oHttpServer.LogInOK( ReadContent( pt, 'ReqID' ), ReadContent( pt, 'Access' ) );
        	end;
	    end;
	end;

procedure   TfFeedMain.InitKeyPads;

	begin         // wait for UDC Mgt to set host IPs first
    // Sleep( 1000 );
    if oKeyPadServer = nil then  begin
        oKeyPadServer := cKeyPadServer.Create( Log, oDataTree );
        end;
    // Sleep( 1000 );
    oKeyPadServer.Initialize;
    end;


procedure   TfFeedMain.NewData( const xml : string );

	begin
    if not mInit and oDataTree.Ready then  begin  // mirrored data is complete
        mInit := true;
        oTimeTable.DataTree := oDataTree;  // todo reorganise TT init to match others here
        oTimeTable.Hub := oHub;
        oTimeTable.Initialize;

        if oHttpServer = nil then  begin
            oHttpServer := cHttpServer.Create( Log, oDataTree );
        	oHttpServer.Initialize;
	        end;

        if oUDCManagement = nil then  begin
            oUDCManagement := cUDCManagement.Create( Log, oDataTree, FeedID );
            oUDCManagement.OnUDCHostsReady := InitKeyPads;    // wait till UDC keypads initialised
            oUDCManagement.Initialize;
        	end;

        if oQantasFeedHandler = nil then  begin
        	oQantasFeedHandler := cQantasFeedHandler.Create( oDataTree, FeedID, Log );
            oQantasFeedHandler.Initialize;
	        end;
    	end;
    if oKeyPadServer <> nil then  oKeyPadServer.Redraw;
    end;


procedure   TfFeedMain.Reset1Click(Sender: TObject);

    begin
    oTimeTable.Initialize( true );
    end;


procedure TfFeedMain.UDCs1Click(Sender: TObject);

    begin
    oUDCManagement.ShowGrid( fUDCs.oGrid );
    if fUDCs.Showing then fUDCs.Invalidate
    else  begin
    	fUDCs.Show;
        fUDCs.OnRequest := UDCs1Click;
	    end;
    end;


procedure   TfFeedMain.NewConnection;

	var
        sl : TStringList;
	begin
    if ( oDataTree = nil ) and ( oHub.Connected ) then  begin
    	LogIn( FeedID, SysPW, FeedID );
        oDataTree := cMirrorDB.Create( oHub, FeedID, false, true, 13 );
        oDataTree.RegisterReader( NewData );   // I want to see incoming messages
        sl := TStringList.Create;              // tell mirror what I'm interested in
        sl.Add( 'SystemConfig' );
        sl.Add( 'SystemSettings' );
        sl.Add( 'Arrivals' );
        sl.Add( 'Departures' );
        sl.Add( 'Timetable' );
        sl.Add( 'CheckIns' );
        sl.Add( 'DisplayConfig' );   // for list of UDC IPs - see uKeyPadServer
        // don't need to load big IATA stuff ...
        oDataTree.InitMirror( sl );  // maintain a local copy of the data base
        sl.Free;
        end;
    end;


procedure  TfFeedMain.Connect;

	begin  // 192.168.0.163 : 1666    todo load systemconfig.xml   todo encrypt password?
    if oHub = nil then  begin
        oHub := cMessageHub.Create( Log );
        oHub.OnConnection := NewConnection;
        oHub.RegisterReader( MessageReader );
        oHub.InitConnection( [ alLocal, alPipe ], FeedID, Server, '',
            	tcpNone, TStringList( nil ), 0 );
        Poller.OnTimeOut( 100,              // in 10 seconds time check the connection
        	procedure() begin  //aOnTimeOutProc
            	if not oHub.Connected then  begin
                    if not oHub.Master then  begin
                        FreeAndNil( oHub );
            			ShowMessage( 'Could not find server ' + Server );
                        Close;
                        end;
                    end
                else NewConnection;        // should be redundant
                end
            );
        end;
    end;


procedure   TfFeedMain.LogIn( const usr, pw, id : string );

	begin
    oHub.Broadcast( FormatLogIn( usr, pw, id ) );
    end;

{procedure TfFeedMain.LogOut( userID : string );

	var
		r : aXML;
	begin
	if Hub <> nil then  begin
		r := FormatLogOut( userID, FeedID );
		Hub.Broadcast( r );
        Sleep( 20 );   // wait for message to get out
        FreeAndNil( Hub );
        FreeAndNil( DataTree );
    	end;
	end;  }

end.
