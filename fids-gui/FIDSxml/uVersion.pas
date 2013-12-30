unit uVersion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfVersion = class(TForm)
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lbVersion: TLabel;
    lbLicence: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fVersion: TfVersion;

implementation

{$R *.dfm}
uses  uFIDSmain;

procedure TfVersion.FormCreate(Sender: TObject);
begin
lbVersion.Caption := Version + Build;
lbLicence.Caption := 'NO LICENCE';
end;

end.
