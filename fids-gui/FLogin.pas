unit FLogin;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, uLogin, Menus, ExtCtrls, ImgList, VrControls, VrLcd, uGlobalDefs;

type
	TfrmLogin = class(TForm)
    pnlContent: TPanel;
    EUsername: TLabeledEdit;
    EPassword: TLabeledEdit;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    lblJobName: TLabel;
    tmrConnectionChecker: TTimer;
		procedure Button2Click(Sender: TObject);
		procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConnectionEvent( event : aConnectionEvent; param : string );
    procedure tmrConnectionCheckerTimer(Sender: TObject);
	private
		{ Private declarations }
		LoginManager: CLogin;
	public
		{ Public declarations }
	end;

var
	frmLogin: TfrmLogin;
	Users: TStringList;

implementation

uses FMain;

{$R *.dfm}

procedure TfrmLogin.Button1Click(Sender: TObject);
var
	User, Pass: String;
begin

	User := EUsername.Text;
	Pass := EPassword.Text;

    LoginManager.Check(User, Pass);

	if ((Length(User) <= 0) OR ((Length(Pass) <= 0))) then
	begin
		ShowMessage('Username or Password not supplied');
		Exit;
	end;

	if (User = 'test') AND (Pass = 'test') then
	begin
		frmMain.show;
		Hide;
	end
	else
	begin
		ShowMessage('Incorrect Username or Password, Please try again');
	end;

end;

procedure TfrmLogin.Button2Click(Sender: TObject);
begin
	Application.Terminate;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
	LoginManager.Connect(ConnectionEvent);
end;

procedure TfrmLogin.tmrConnectionCheckerTimer(Sender: TObject);
begin

	lblJobName.Caption := LoginManager.GetJobName();
end;

procedure TfrmLogin.ConnectionEvent( event : aConnectionEvent; param : string );
begin
    case event of
        ceNone:
        	;
        ceConnected:
			tmrConnectionChecker.Enabled := true;
        ceDbReady:
			;
        ceLogin:
			;
        ceEdit:
			;
        ceDisconnected:
			;
        ceShutdown:
        	Close;
    end;
end;

end.
