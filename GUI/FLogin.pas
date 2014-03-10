unit FLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uLogin, Menus, ExtCtrls, ImgList, VrControls, VrLcd,
  uGlobalDefs, uCommon;

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
    procedure ConnectionEvent(event: aConnectionEvent; param: string);
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

procedure TfrmLogin.ConnectionEvent(event: aConnectionEvent; param: string);
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
