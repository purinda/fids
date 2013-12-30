unit uFIDSxml;

interface

uses
    ASCII, uXmlParser, uUtils, uPoller, uPacket, uFlight, uGlobalDefs,
    uGT, uMessageHub, udbTree, uMirrorDB, uFidsTags, Classes, SysUtils,
    uCommon, Dialogs;

const
    { Following credentials are only
      used if no user, pass submitted to
      Login(), disable in End-Product }
    mUserName = 'Feed';
    mPassword = 'DI_system';

type
    TFIDSxml = class
    private
        MessageLog: TStringList;
        { variables require for singleton interface }
        class var FInstance: TFIDSxml;
        class function GetInstance: TFIDSxml; static;
        { Basic object creation procedures }
        constructor Create;
        destructor Destroy; override;

        { set of procedures for connectivity }
        procedure Log(ErrorNo: integer; const s: string);
        procedure MessageReader
        (const mesg: string; link: apLinkID = nil);
        // aReader
        //procedure Notify(const xml: string); // aDeltaNotify

        procedure LogIn(const usr, pw, id: string);
        // procedure   LogOut( userID : string );
        //procedure NewConnection;
        // procedure   SetDataTree( dt : cMirrorDB );
    public
        mInit, mShutDown: boolean;
        //oDataTree: cMirrorDB;
        //oHub: cMessageHub;
        //procedure Connect();
        { procedure require to get singleton interface }

        class property Instance: TFIDSxml read GetInstance;
        function GetUserName: String;
        procedure SetLogin(usr, pw: string);
        // Properties used to access private data fields
        // They use private methods to do this
        function GetDataTree: cMirrorDB;
        //function GetMessageHub: cMessageHub;

    end;

implementation

uses
  uConnection;

constructor TFIDSxml.Create();
begin
    mShutDown := false;
    mInit := FALSE;
    // Following is a string list to log all messages
    MessageLog := TStringList.Create;

    Poller.OnTimeOut(3, procedure()begin // aOnTimeOutProc
      Connect; end);
end;

class function TFIDSxml.GetInstance: TFIDSxml;
begin
    if not Assigned(FInstance) then
    begin
        FInstance := TFIDSxml.Create;
    end;

    Result := FInstance;
end;

destructor TFIDSxml.Destroy();
begin
    if Assigned(FInstance) then
    begin
        MessageLog.Free;
        //oHub.Free;
        //oDataTree.Free;
        FInstance.Free;
    end;

end;

function TFIDSxml.GetUserName(): String;
begin
    Result := mUserName;
end;

function TFIDSxml.GetDataTree(): cMirrorDB;
begin
    Result := DB;//Self.oDataTree;
end;

//function TFIDSxml.GetMessageHub(): cMessageHub;
//begin
//    Result := Self.oHub;
//end;

//procedure TFIDSxml.NewConnection;
//var
//    sl: TStringList;
//begin
//
//    if (oDataTree = nil) and (oHub.Connected) then
//    begin
//        LogIn(mUserName, mPassword, mUserName);
//
//        oDataTree := cMirrorDB.Create(oHub, mUserName, FALSE, TRUE, 13);
//        oDataTree.RegisterReader(Notify); // I want to see incoming messages
//
//        // oFlight := cFlight.Create( oDataTree, mUserName );
//        sl := TStringList.Create; // tell mirror what I'm interested in
//        sl.Add('Sensors');
//        sl.Add('Comments');
//        sl.Add('SystemConfig');
//        sl.Add('SystemSettings');
//        sl.Add('Arrivals');
//        sl.Add('Departures');
//        sl.Add('Timetable');
//        sl.Add('CheckIns');
//        sl.Add('DisplayConfig');   // for list of UDC IPs - see uKeyPadServer
//        // don't need to load big IATA stuff ...
//        oDataTree.InitMirror(sl); // maintain a local copy of the data base
//        oDataTree.RegisterReader(Notify);
//        // oDataTree.RegisterReader(  );
//        sl.Free;
//    end;
//
//end;
//
//procedure TFIDSxml.Connect;
//begin // 192.168.0.163 : 1666    todo load systemconfig.xml   todo encrypt password?
//    if oHub = nil then
//    begin
//        oHub := cMessageHub.Create(Log);
//        oHub.OnConnection := NewConnection;
//        oHub.RegisterReader(MessageReader);
//        oHub.InitConnection([alLocal, alPipe], mUserName, uCommon.Server, '',
//          tcpNone, TStringList(nil), 0);
//        Poller.OnTimeOut(100, // in 10 seconds time check the connection
//          procedure()begin // aOnTimeOutProc
//          if not oHub.Connected then begin if not oHub.Master then begin
//          FreeAndNil(oHub);
//          { Return error }
//          //ShowMessage('Could not find server ' + Server);
//          { Close; }
//        end; end else NewConnection; // should be redundant
//        end);
//    end;
//end;

//procedure TFIDSxml.Notify(const xml: string);
//begin
//    if not mInit and oDataTree.Ready then
//    begin // mirrored data is complete
//        mInit := TRUE;
//        // ShowMessage('Notify Event');
//        // a good time to initialize stuff needing the tree
//    end;
//end;

procedure TFIDSxml.SetLogin(usr, pw: string);
begin

end;

procedure TFIDSxml.LogIn(const usr, pw, id: string);
var
    strUser, strPass: String;
begin // 192.168.0.163 : 1666    todo load systemconfig.xml   todo encrypt password?
    if ((Length(usr) <= 0) OR (Length(pw) <= 0)) then   begin
      Xml_Connection.UserName := mUserName;
      Xml_Connection.Password := mPassword;
      end
    else  begin
      Xml_Connection.UserName := usr;
      Xml_Connection.Password := pw;

      end;
    Xml_Connection.LogIn;
//    BEGIN
//        strUser := mUserName;
//        strPass := mPassword;
//    END
//    ELSE
//    BEGIN
//        strUser := usr;
//        strPass := pw;
//    END;
//
//    oHub.Broadcast(FormatLogIn(strUser, strPass, id));

end;

procedure TFIDSxml.Log(ErrorNo: integer; const s: string);
begin
    // memoLog.Lines.Add( InttoStr( ErrorNo ) + '  ' + s );
end;

procedure TFIDSxml.MessageReader(const mesg: string; link: apLinkID = nil);
// aReader
begin
    if mesg = '<' + TagShutDown + '/>' then
    begin // eg catch shutdown message
        mShutDown := TRUE;
    end;
end;

end.
