unit FSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmSearch = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    cmbFieldList: TComboBox;
    GroupBox1: TGroupBox;
    chkCase: TCheckBox;
    chkWholeWords: TCheckBox;
    Label2: TLabel;
    cmbSearch: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Passing vars }
    Fields: TStringList;

    { Public declarations }

  end;

var
  frmSearch: TfrmSearch;

implementation

{$R *.dfm}

procedure TfrmSearch.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmSearch.Button2Click(Sender: TObject);
begin
  ModalResult := mrClose;
  self.Close();
end;

procedure TfrmSearch.FormShow(Sender: TObject);
begin
  cmbFieldList.Items := Fields;
  { change the autoselect item later [use registry to store val] }
  cmbFieldList.ItemIndex := 0;
end;

end.
