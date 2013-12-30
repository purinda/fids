unit uGUIMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids, StdCtrls, ExtCtrls,
  uGlobalDefs, uGT, uMessageHub, uDbTree, uMirrorDB;    // in ../../Tools/DelphiLib/


const
	Server = 'FIDSxml';
	GUIID = 'EgGUI';
    SysPW  = 'DI_system';

type

	TfGUIMain = class( TForm )
    	memoLog: TMemo;
    MainMenu1: TMainMenu;
    Departures1: TMenuItem;
    imeTable1: TMenuItem;
    DataTree1: TMenuItem;
		procedure FormCreate(Sender: TObject);
    procedure btDeparturesClick(Sender: TObject);
    procedure Departures1Click(Sender: TObject);
    procedure DataTree1Click(Sender: TObject);
    procedure imeTable1Click(Sender: TObject);
    procedure memoLogChange(Sender: TObject);
	public
    	procedure   Display( const src, op, name, std : string );
        procedure   LogIn( const usr, pw, id : string );
        function	GetDB() : cMirrorDB;
        function	GetID() : string;
	private
        mUserID, mPassword : string;
		mShutDown, mInit : boolean;
        oDataTree : cMirrorDB;
        oHub : cMessageHub;
		procedure   fOnIdle(Sender: TObject; var Done: Boolean);
		procedure   Log( ErrorNo : integer; const s : string );
		procedure   MessageReader( const mesg : string; link : apLinkID = nil );  // aReader
		procedure   NewData( const xml : string );  // aDeltaNotify
		procedure   Connect();
		//procedure   LogOut( userID : string );
		procedure   NewConnection;
		end;

var
	fGUIMain: TfGUIMain;

implementation

{$R *.dfm}


uses  ASCII, uXmlParser, uUtils, uPoller, uPacket, uDepartures, uDataTreeView, uTimeTableView;



// ________________________ Form1 __________________________________



procedure TfGUIMain.FormCreate(Sender: TObject);

	begin                     // create 2 login IDs for journaling clarity
    mUserID := GUIID;
    mPassword := SysPW;
	Poller.OnTimeOut( 3,               // in 3/10 sec Connect   ie wait for create to finish
    	procedure() begin  //aOnTimeOutProc
            Connect;
            end
        );
    Application.OnIdle := fOnIdle;
	end;


procedure   TfGUIMain.fOnIdle(Sender: TObject; var Done: Boolean);

	begin
    if mShutDown then  Close;
    end;


function	TfGUIMain.GetDB() : cMirrorDB;

	begin
    result := nil;
    if ( oDataTree <> nil ) and oDataTree.Ready  then  result := oDataTree;
    end;


function    TfGUIMain.GetID() : string;

	begin
    result := GUIID;
    end;


procedure TfGUIMain.imeTable1Click(Sender: TObject);

    begin
    fTimeTable.Show;
    end;


procedure TfGUIMain.DataTree1Click(Sender: TObject);

	begin
    fDataTree.Show;
	end;


procedure TfGUIMain.Departures1Click(Sender: TObject);

	begin
    fDepartures.Show;
	end;


procedure   TfGUIMain.Display( const src, op, name, std : string );

	begin
    memoLog.Lines.Add( TimeDateToStr( Now ) + ' ' + TAB + src + TAB + op + TAB + name + TAB + std );
    end;


procedure   TfGUIMain.Log( ErrorNo : integer; const s : string );

	begin
    // memoLog.Lines.Add( InttoStr( ErrorNo ) + '  ' + s );
    if ErrorNo <> 0 then  Display( 'ER ' + IntToStr( ErrorNo ), s, '', '' )
    else  Display( s, '', '', '' );
	end;


procedure   TfGUIMain.MessageReader( const mesg : string; link : apLinkID = nil );  // aReader - catch raw xml messages here

	begin
	if mesg = '<' + TagShutDown + '/>' then  begin       // eg catch shutdown message
		mShutDown := true;
		end;
	end;


procedure   TfGUIMain.NewData( const xml : string ); // catch DB tree changes here

	begin
    if not mInit and oDataTree.Ready then  begin  // mirrored data is complete
        mInit := true;
        //   a good time to initialize stuff needing the tree
    	end;
    end;


procedure   TfGUIMain.NewConnection;

	var
        sl : TStringList;
	begin
    if ( oDataTree = nil ) and ( oHub.Connected ) then  begin
    	LogIn( GUIID, SysPW, GUIID );
        oDataTree := cMirrorDB.Create( oHub, GUIID, false, true, 13 );
        oDataTree.RegisterReader( NewData );   // I want to see incoming messages
        sl := TStringList.Create;              // tell mirror what I'm interested in
        sl.Add( 'SystemConfig' );
        sl.Add( 'SystemSettings' );
        sl.Add( 'Arrivals' );
        sl.Add( 'Departures' );
        sl.Add( 'Timetable' );
        sl.Add( 'CheckIns' );
        //sl.Add( 'DisplayConfig' );   // for list of UDC IPs - see uKeyPadServer
        // don't need to load big IATA stuff ...
        oDataTree.InitMirror( sl );  // maintain a local copy of the data base
        sl.Free;
        end;
    end;


procedure TfGUIMain.btDeparturesClick(Sender: TObject);

	begin
    fDepartures.Show;
	end;


procedure  TfGUIMain.Connect;

	begin
    if oHub = nil then  begin
        oHub := cMessageHub.Create( Log );
        oHub.OnConnection := NewConnection;
        oHub.RegisterReader( MessageReader );
        oHub.InitConnection( [ alLocal, alPipe ], GUIID, Server, '',
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


procedure   TfGUIMain.LogIn( const usr, pw, id : string );

	begin
    oHub.Broadcast( FormatLogIn( usr, pw, id ) );
    end;

procedure TfGUIMain.memoLogChange(Sender: TObject);
begin

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
