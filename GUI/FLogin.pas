unit FLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uLogin, Menus, ExtCtrls, ImgList, VrControls, VrLcd,
  uGlobalDefs, uCommon, uSocket;

type
  TfrmLogin = class(TForm)
    pnlContent: TPanel;
    EUsername: TLabeledEdit;
    EPassword: TLabeledEdit;
    btnLogin: TButton;
    btnCancel: TButton;
    CheckBox1: TCheckBox;
    lblJobName: TLabel;
    tmrConnectionChecker: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConnectionEvent(event: aConnectionEvent; param: string);
    procedure tmrConnectionCheckerTimer(Sender: TObject);
  private
    { Private declarations }
    LoginManager: CLogin;

    { Function to manage form behaviour when Engine is not running }
    procedure EngineAway(Away: Boolean);
  public
    { Public declarations }
  end;

var
  frmLogin: TfrmLogin;
  Users: TStringList;

implementation

uses FMain;

{$R *.dfm}

procedure TfrmLogin.btnLoginClick(Sender: TObject);
var
  LoginStatus: Boolean;
begin

  // Debug mode
  if DEBUG_MODE = 1 then
  begin
    frmMain.show;
    Hide;
    Exit;
  end;


  if ((Length(EUsername.Text) <= 0) OR ((Length(EPassword.Text) <= 0))) then
  begin
    MessageBox(Application.Handle, 'Username or password not supplied.',
      'Login Failure', MB_ICONINFORMATION + MB_OK);
    Exit;
  end;

  LoginStatus := LoginManager.Check(EUsername.Text, EPassword.Text);

  if (LoginStatus) then
  begin
    frmMain.show;
    Hide;
  end
  else
  begin
    MessageBox(Application.Handle,
      'Incorrect username or password, Please try again', 'Login Failure',
      MB_ICONSTOP + MB_OK);
  end;

end;

procedure TfrmLogin.btnCancelClick(Sender: TObject);
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

procedure TfrmLogin.ConnectionEvent(event: aConnectionEvent; param: string);
begin
  case event of
    ceNone:
      ;
    ceConnected:
      EngineAway(false);
    ceDbReady:
      ;
    ceLogin:
      ;
    ceEdit:
      ;
    ceDisconnected:
      ;
    ceShutdown:
      EngineAway(True);
  end;
end;

procedure TfrmLogin.EngineAway(Away: Boolean);
begin
  if (Away) then
  begin
    tmrConnectionChecker.Enabled := false;
    btnLogin.Enabled := false;

    lblJobName.Color := clRed;
    lblJobName.Caption := 'FIDS engine is unavailable.';
  end
  else
  begin
    tmrConnectionChecker.Enabled := true;
    btnLogin.Enabled := true;
    lblJobName.Color := clBlack;
    lblJobName.Caption := 'Connecting...';
  end;
end;

end.
