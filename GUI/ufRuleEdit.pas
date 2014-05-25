unit ufRuleEdit;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, uTTRules, uGT, ComCtrls, uCommon, VarUtils,
	StrUtils, uConnection, uLogin;

type
	TfRuleEdit = class(TForm)
		ebRuleName: TEdit;
		Label1: TLabel;
		ComboBox1: TComboBox;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		ebExcept: TEdit;
		Label6: TLabel;
		gbDays: TGroupBox;
		RadioButton1: TCheckBox;
		RadioButton2: TCheckBox;
		RadioButton3: TCheckBox;
		RadioButton4: TCheckBox;
		RadioButton5: TCheckBox;
		RadioButton6: TCheckBox;
		RadioButton7: TCheckBox;
		ebPorts: TEdit;
		Label7: TLabel;
		ebGates: TEdit;
		Label8: TLabel;
		ebBays: TEdit;
		Label9: TLabel;
		ebCheckIns: TEdit;
		Label10: TLabel;
		gbDaysExcept: TGroupBox;
		CheckBox2: TCheckBox;
		CheckBox3: TCheckBox;
		CheckBox4: TCheckBox;
		CheckBox5: TCheckBox;
		CheckBox6: TCheckBox;
		CheckBox7: TCheckBox;
		CheckBox8: TCheckBox;
		btDelete: TButton;
		lbFlightNumber: TLabel;
		lbFlightNum: TLabel;
		pnlButtons: TPanel;
		btnClose: TButton;
		Button3: TButton;
		ebStart: TDateTimePicker;
		ebEnd: TDateTimePicker;
		GroupBox1: TGroupBox;
		ebTime: TDateTimePicker;
		lstExclusions: TListBox;
		btnAdd: TButton;
		btnRem: TButton;
		dtExFrom: TDateTimePicker;
		Label11: TLabel;
		dtUntill: TDateTimePicker;
		Label12: TLabel;
		Label13: TLabel;
		procedure FormShow(Sender: TObject);
		procedure btOKClick(Sender: TObject);
		procedure btDeleteClick(Sender: TObject);
		procedure btNewClick(Sender: TObject);
		procedure ebRuleNameChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure btnAddClick(Sender: TObject);
		procedure btnRemClick(Sender: TObject);
	private
		oTTRule: cTTRule;
		function CheckError: boolean;
		procedure Clear;
		procedure SetCheckBoxes(group: TGroupBox; vals: int);
		function GetCheckBoxes(group: TGroupBox): int;
		procedure CollectFieldValues;
		procedure SetTTRule(rule: cTTRule);
	public
		property TTRule: cTTRule read oTTRule write SetTTRule;
		// how form knows which rule to edit
	end;

var
	fRuleEdit: TfRuleEdit;

implementation

{$R *.dfm}

uses
	uFlight, uUtils, uFidsTags;

procedure TfRuleEdit.SetCheckBoxes(group: TGroupBox; vals: int);

var // relies on check box tags set to 2^n values
	x: int;
	c: TControl;
	cb: TCheckBox;
begin // sets the 7 day check values from a single value
	for x := 0 to group.ControlCount - 1 do
	begin
		c := group.Controls[x];
		if c is TCheckBox then
		begin
			cb := TCheckBox(c);
			cb.Checked := vals and cb.Tag <> 0;
		end;
	end;
end;

function TfRuleEdit.GetCheckBoxes(group: TGroupBox): int;

var // relies on check box tags set to 2^n values
	x: int;
	c: TControl;
	cb: TCheckBox;
begin // read the 7 day check values into a single value
	result := 0;
	for x := 0 to group.ControlCount - 1 do
	begin
		c := group.Controls[x];
		if c is TCheckBox then
		begin
			cb := TCheckBox(c);
			if cb.Checked then
				result := result or cb.Tag;
		end;
	end;
end;

function TfRuleEdit.CheckError: boolean;

begin // produces crude error messages from error number - true is NO errors
	result := true;
	if oTTRule.Error <> teNone then
	begin
		ShowMessage('ERROR ' + EnumToStr(Ord(oTTRule.Error),
		  TypeInfo(aTTValidationError)));
		result := false;
	end;
end;

procedure TfRuleEdit.CollectFieldValues;

begin // feed all fields into cTTRules for validation and global update of changes
	// should be automated with data dictionary ?
	oTTRule.Error := teNone;
	oTTRule.Presentation[tfPath] := ComboBox1.Items[ComboBox1.ItemIndex];
	oTTRule.Presentation[tfRuleName] := ebRuleName.Text;
	oTTRule.Presentation[tfTime] := uCommon.FIDS_TimeTOStr(ebTime.Time);
	oTTRule.Presentation[tfDateStart] := uCommon.FIDS_DtTOStr(ebStart.Date);
	oTTRule.Presentation[tfDateEnd] := uCommon.FIDS_DtTOStr(ebEnd.Date);
	oTTRule.Presentation[tfDateException] := ebExcept.Text;
	// mTTRule.oTemplate.Presentation[ ffFlight ] := ebFlightNumber.Text;     derrived from rule name
	oTTRule.oTemplate.Presentation[ffPorts] := ebPorts.Text;
	oTTRule.oTemplate.Presentation[ffGates] := ebGates.Text;
	oTTRule.oTemplate.Presentation[ffBays] := ebBays.Text;
	oTTRule.oTemplate.Presentation[ffCheckIns] := ebCheckIns.Text;

	oTTRule.Presentation[tfDays] := IntToHex(GetCheckBoxes(gbDays), 2);
	oTTRule.Presentation[tfDaysExcept] :=
	  IntToHex(GetCheckBoxes(gbDaysExcept), 2);
end;

procedure TfRuleEdit.ebRuleNameChange(Sender: TObject);

begin // new flight name is derrived from rule name ?
	lbFlightNum.Caption := StripTail(ebRuleName.Text);
end;

procedure TfRuleEdit.btDeleteClick(Sender: TObject);

begin // delete a rule
	oTTRule.Delete;
	if CheckError() then
		ModalResult := mrOK;
end;

procedure TfRuleEdit.btnAddClick(Sender: TObject);
var
	strExclusion: string;
begin
	lstExclusions.Items.Add(uCommon.FIDS_DtTOStr(dtExFrom.Date) + '-' +
	  uCommon.FIDS_DtTOStr(dtUntill.Date));
	ebExcept.Clear;

	for strExclusion in lstExclusions.Items do
	begin
		ebExcept.Text := ebExcept.Text + ',' + strExclusion;
	end;

end;

procedure TfRuleEdit.btNewClick(Sender: TObject);

begin // ceate a new rule in timetable
	oTTRule.DbNode := nil;
	oTTRule.oTemplate.DbNode := nil;
	CollectFieldValues;
	if CheckError() then
	begin
		oTTRule.New;
		if CheckError() then
			ModalResult := mrOK;
	end;
end;

procedure TfRuleEdit.btnRemClick(Sender: TObject);
var
	strExclusion: string;
begin
	lstExclusions.Items.Delete(lstExclusions.ItemIndex);
	ebExcept.Clear;

	for strExclusion in lstExclusions.Items do
	begin
		ebExcept.Text := ebExcept.Text + ',' + strExclusion;
	end;

	if lstExclusions.Count = 0 then
	begin
		ebExcept.Clear;
	end;

end;

procedure TfRuleEdit.btOKClick(Sender: TObject);

begin // ok finished edits so update any changed fields

end;

procedure TfRuleEdit.Button3Click(Sender: TObject);
begin
	CollectFieldValues;
	// if CheckError() then  ModalResult := mrOK;
	ModalResult := mrOK;
end;

procedure TfRuleEdit.Clear;

begin
	ebRuleName.Text := '';
	ebExcept.Text := '';
	lbFlightNum.Caption := '';
	ebPorts.Text := '';
	ebGates.Text := '';
	ebBays.Text := '';
	ebCheckIns.Text := '';
	SetCheckBoxes(gbDays, 0);
	SetCheckBoxes(gbDaysExcept, 0);
end;

procedure TfRuleEdit.FormCreate(Sender: TObject);

var
	kind: aFlightKind;
begin // init flight kind drop down
	for kind := Succ( Low(aFlightKind)) to High(aFlightKind) do
	begin
		ComboBox1.Items.Add(EnumToStr(Ord(kind), TypeInfo(aFlightKind)));
		// Arrivals etc
	end;
end;

procedure TfRuleEdit.FormShow(Sender: TObject);

var
	kind, s: string;
	x, days: int;

	strExceptions: TStringList;
	strException: String;

begin // initialize various edit controls

	if oTTRule <> nil then
	begin
		kind := oTTRule.Presentation[tfPath]; // set kind selector
		for x := 0 to ComboBox1.Items.Count - 1 do
		begin
			if kind = ComboBox1.Items[x] then
			begin
				ComboBox1.ItemIndex := x;
				break;
			end;
		end;

		ebRuleName.Text := oTTRule.Presentation[tfRuleName];
		ebTime.Time := uCommon.FIDS_StrTOTime(oTTRule.Presentation[tfTime]);
		ebStart.DateTime := uCommon.FIDS_StrToDT
		  (oTTRule.Presentation[tfDateStart]);
		ebEnd.DateTime := uCommon.FIDS_StrToDT(oTTRule.Presentation[tfDateEnd]);
		ebExcept.Text := oTTRule.Presentation[tfDateException];
		lbFlightNum.Caption := StripTail(oTTRule.Presentation[tfRuleName]);
		ebPorts.Text := oTTRule.oTemplate.Presentation[ffPorts];
		ebGates.Text := oTTRule.oTemplate.Presentation[ffGates];
		ebBays.Text := oTTRule.oTemplate.Presentation[ffBays];
		ebCheckIns.Text := oTTRule.oTemplate.Presentation[ffCheckIns];

		s := oTTRule.Presentation[tfDays];
		x := 1;
		days := GetHex(s, x);
		SetCheckBoxes(gbDays, days);

		s := oTTRule.Presentation[tfDaysExcept];
		x := 1;
		days := GetHex(s, x);
		SetCheckBoxes(gbDaysExcept, days);
	end
	else
		Clear;

	lstExclusions.Clear;
	strExceptions := TStringList.Create;
	uCommon.Split(',', ebExcept.Text, strExceptions);

	for strException in strExceptions do
	begin
		if length(strException) >= 1 then
			lstExclusions.Items.Add(strException);
	end;

	strExceptions.Free;
end;

procedure TfRuleEdit.SetTTRule(rule: cTTRule);

begin // tells form which rule to adjust
	if oTTRule = nil then
		oTTRule := cTTRule.Create(DB, DB.id);

  oTTRule := cTTRule.Create(DB(), 'Feed');
	oTTRule.DbNode := rule.DbNode;
  oTTRule.DbPath := rule.DbPath;
	// need own object since vst calls GetText a lot
	oTTRule.oTemplate.DbNode := rule.oTemplate.DbNode;
end;

end.
