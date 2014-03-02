unit CrawlingEdit;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls;

type
	TFCrawlingLineEdit = class(TForm)
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		Label10: TLabel;
		txtIP: TEdit;
		cmbLocation: TComboBox;
		cmbGroup: TComboBox;
		cmbStatus: TComboBox;
		cmbBlinking: TComboBox;
		cmbCrawling: TComboBox;
		cmbMsgType: TComboBox;
		cmbBg: TComboBox;
		cmbFg: TComboBox;
		txtMsg: TEdit;
		Button1: TButton;
		Button2: TButton;
		procedure Button2Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	FCrawlingLineEdit: TFCrawlingLineEdit;

implementation

{$R *.dfm}

procedure TFCrawlingLineEdit.Button2Click(Sender: TObject);
begin
	Close;
end;

end.
