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
		procedure MessageReader(const mesg: string; link: apLinkID = nil);
		// aReader
		// procedure Notify(const xml: string); // aDeltaNotify

		procedure LogIn(const usr, pw, id: string);
		// procedure   LogOut( userID : string );
		// procedure NewConnection;
		// procedure   SetDataTree( dt : cMirrorDB );
	public
		mInit, mShutDown: boolean;
		// oDataTree: cMirrorDB;
		// oHub: cMessageHub;
		// procedure Connect();
		{ procedure require to get singleton interface }

		class property Instance: TFIDSxml read GetInstance;
		function GetUserName: String;
		procedure SetLogin(usr, pw: string);
		// Properties used to access private data fields
		// They use private methods to do this
		function GetDataTree: cMirrorDB;
		// function GetMessageHub: cMessageHub;

	end;

implementation

uses
	uConnection;

constructor TFIDSxml.Create();
begin
	mShutDown := false;
	mInit := false;
	// Following is a string list to log all messages
	MessageLog := TStringList.Create;
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
		FInstance.Free;
	end;

end;

function TFIDSxml.GetUserName(): String;
begin
	Result := mUserName;
end;

function TFIDSxml.GetDataTree(): cMirrorDB;
begin
	Result := DB; // Self.oDataTree;
end;

procedure TFIDSxml.SetLogin(usr, pw: string);
begin

end;

procedure TFIDSxml.LogIn(const usr, pw, id: string);
var
	strUser, strPass: String;
begin // 192.168.0.163 : 1666    todo load systemconfig.xml   todo encrypt password?
	if ((Length(usr) <= 0) OR (Length(pw) <= 0)) then
	begin
		Xml_Connection.UserName := mUserName;
		Xml_Connection.Password := mPassword;
	end
	else
	begin
		Xml_Connection.UserName := usr;
		Xml_Connection.Password := pw;

	end;
	Xml_Connection.LogIn;
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
