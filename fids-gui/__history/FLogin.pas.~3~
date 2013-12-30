unit FLogin;

interface

uses
  Windows,  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, uFlight, uFidsTags, uCommon, uController, FWindow,
  FSearch, Menus, ExtCtrls, ImgList, VrControls, VrLcd;

type
  TfrmLogin = class(TForm)
    pnlContent: TPanel;
    EUsername: TLabeledEdit;
    EPassword: TLabeledEdit;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    lblFidsStatus: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fcWindow : CFlightController;
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
	User, Pass : String;
begin
	User := EUsername.Text;
	Pass := EPassword.Text;

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
    fcWindow := CFlightController.Create(fkNone, ArrivalFields);
end;

procedure TfrmLogin.Timer1Timer(Sender: TObject);
begin

    if not (fcWindow.isHostRunning) then
    begin
        lblFidsStatus.Visible := true;
        height := 245 + lblFidsStatus.Height+ 10;
        pnlContent.Top := lblFidsStatus.Top + lblFidsStatus.Height + 10;
    end;

    //timer1.Enabled := false;

    //fcWindow.NewConnection(FIDSDepartures);
		//Users := fcWindow.GetUsers;
end;

end.
