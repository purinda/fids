unit ufStringEntry;

// trivial string entry form

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfStrEntry = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    function GetStrVal(): string;
  public
    function GetStr(const lab, initial: string): integer;
    property Str: string read GetStrVal;
  end;

var
  fStrEntry: TfStrEntry;

implementation

{$R *.dfm}
{ USAGE
  if fStrEntry.GetStr( 'Enter Language tag for ', 'English' ) = mrOK then  begin
  lan := fStrEntry.Name;
  end;
}

function TfStrEntry.GetStr(const lab, initial: string): integer;

begin
  Label1.Caption := lab;
  Edit1.Text := initial;
  result := ShowModal;
end;

function TfStrEntry.GetStrVal(): string;

begin
  result := Edit1.Text;
end;

procedure TfStrEntry.Button1Click(Sender: TObject);

begin
  ModalResult := mrOK;
end;

end.
