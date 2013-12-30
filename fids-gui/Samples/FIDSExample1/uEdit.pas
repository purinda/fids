unit uEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfEdit = class(TForm)
    Edit1: TEdit;
    btCancel: TButton;
    btOK: TButton;
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    mResult : integer;
  end;

var
  fEdit: TfEdit;

implementation

{$R *.dfm}

procedure TfEdit.btCancelClick(Sender: TObject);
begin
mResult := mrCancel;
ModalResult := mrCancel;
//Close;
end;

procedure TfEdit.btOKClick(Sender: TObject);
begin
mResult := mrOk;
ModalResult := mrOk;
//Close;
end;

end.
