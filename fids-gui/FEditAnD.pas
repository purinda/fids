unit FEditAnD;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, uCommon, uGlobalDefs, uFlight, uFidsTags,
	ComCtrls, VrControls, VrBlinkLed;

type
	TfrmEditAnD = class(TForm)
		pnlButtons: TPanel;
		btnClose: TButton;
		Button3: TButton;
		tmrBackground: TTimer;
		procedure EditKeyPress(Sender: TObject; var Key: Char);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure tmrBackgroundTimer(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure Button3Click(Sender: TObject);
	private
	{ Private declarations }
	  const
		Row1 = 10;

	const
		Row2 = 40;

	const
		Row3 = 70;

	const
		Row4 = 100;

	const
		Row5 = 130;

	const
		Row6 = 160;

	const
		Row7 = 190;

	const
		Row8 = 220;

	const
		Row9 = 270;

	const
		Row10 = 300;

	const
		Col1 = 10;

	const
		Col2 = 210;

	const
		Col3 = 410;

	public
		{ Lists }
		lstStatuses: TStringList;
		lstGates: TStringList;
		lstBays: TStringList;
		lstBelts: TStringList;
		lstCheckin: TStringList;
		lstCarrierTypes: TStringList;
		lstAircrafts: TStringList;
		lstTerminals: TStringList;

		{ Public declarations }
		procedure SetFields(IffFields: array of aFlightField;
		  strFields: array of string);
		procedure SetData(InData: PTreeData);
	end;

var
	frmEditAnD: TfrmEditAnD;
	FieldNames: array of String;
	ffFields: array of aFlightField;
	FlightData: PTreeData;

	lblField: array of TLabel;
	inpField: array of TControl;

	// Related to gates hack
	Gate1, Gate2: TEdit;
	Gates: TStringList;

	Belt1, Belt2: TEdit;
	Belts: TStringList;

implementation

{$R *.dfm}

procedure TfrmEditAnD.FormDestroy(Sender: TObject);
begin
	lstGates.Free;
	lstStatuses.Free;
	lstBays.Free;
	lstBelts.Free;
	lstCheckin.Free;
	lstCarrierTypes.Free;
	lstAircrafts.Free;
	lstTerminals.Free;
end;

procedure TfrmEditAnD.FormShow(Sender: TObject);
begin
	// Choose appopriate title for edit windows
	Caption := Caption + ' ' + FlightData.Flight;

end;

procedure TfrmEditAnD.SetData(InData: PTreeData);
begin
	FlightData := InData;
end;

procedure TfrmEditAnD.SetFields(IffFields: array of aFlightField;
  strFields: array of string);
Var
	I: Integer;
	FieldName: String;
	ffField: aFlightField;
begin

	SetLength(FieldNames, length(strFields));
	SetLength(ffFields, length(IffFields));

	I := 0;
	for ffField in IffFields do
	begin
		ffFields[I] := ffField;
		Inc(I);
	end;

	I := 0;
	for FieldName in strFields do
	begin
		FieldNames[I] := FieldName;
		Inc(I);
	end;

end;

procedure TfrmEditAnD.tmrBackgroundTimer(Sender: TObject);
var
	I: smallint;
begin

	for I := 0 to self.ComponentCount - 1 do
	begin
		if (Components[I].Name = 'cmbStatus') then
			TComboBox(Components[I]).Items.AddStrings(lstStatuses);
		if (Components[I].Name = 'cmbGates') then
			TComboBox(Components[I]).Items.AddStrings(lstGates);
		if (Components[I].Name = 'cmbBays') then
			TComboBox(Components[I]).Items.AddStrings(lstBays);
		if (Components[I].Name = 'cmbCheckins') then
			TComboBox(Components[I]).Items.AddStrings(lstCheckin);
		if (Components[I].Name = 'cmbBelts') then
			TComboBox(Components[I]).Items.AddStrings(lstBelts);
		if (Components[I].Name = 'cmbAirCrafts') then
			TComboBox(Components[I]).Items.AddStrings(lstAircrafts);
		if (Components[I].Name = 'cmbCarrierTypes') then
			TComboBox(Components[I]).Items.AddStrings(lstCarrierTypes);
		if (Components[I].Name = 'cmbTerminals') then
			TComboBox(Components[I]).Items.AddStrings(lstTerminals);
	end;

	tmrBackground.Enabled := false;
end;

procedure TfrmEditAnD.Button3Click(Sender: TObject);
var
	AComponent: TComponent;
	Gate: String;
	Belt: String;
begin
	// Gates hack
	AComponent := self.FindComponent('cmbGates');

	if assigned(AComponent) then
		if AComponent is TComponent then
		begin
			// set flight number
			if (length(Gate1.Text) > 0) then
			begin
				Gate := Gate + Gate1.Text;
			end;

			if (length(Gate2.Text) > 0) then
			begin
				if (length(Gate) > 0) then
					Gate := Gate + ',' + Gate2.Text
				else
					Gate := Gate + Gate2.Text;
			end;

			TComboBox(AComponent).Text := Gate;
		end
		else
		begin

		end;

	// Gates hack
	AComponent := self.FindComponent('cmbBelts');

	if assigned(AComponent) then
		if AComponent is TComponent then
		begin
			// set flight number
			if (length(Belt1.Text) > 0) then
			begin
				Belt := Belt + Belt1.Text;
			end;

			if (length(Belt2.Text) > 0) then
			begin
				if (length(Belt) > 0) then
					Belt := Belt + ',' + Belt2.Text
				else
					Belt := Belt + Belt2.Text;
			end;

			TComboBox(AComponent).Text := Belt;
		end
		else
		begin

		end;


	// Validate time fields

end;

procedure TfrmEditAnD.EditKeyPress(Sender: TObject; var Key: Char);
begin
	// Number only
	if ((TComponent(Sender).Name) = 'txtOnBlock') OR
	  ((TComponent(Sender).Name) = 'txtOffBlock') OR
	  ((TComponent(Sender).Name) = 'txtSlotTime') OR
	  ((TComponent(Sender).Name) = 'txtCheckinOpeningTime') OR
	  ((TComponent(Sender).Name) = 'txtCheckinClosingTime') OR
	  ((TComponent(Sender).Name) = 'dtSTTime') OR
	  ((TComponent(Sender).Name) = 'dtETTime') OR
	  ((TComponent(Sender).Name) = 'dtATTime') then
	begin
		if not(Key in [#8, '0' .. '9']) then
		begin
			// Discard the key
			Key := #0;
		end;
	end;

	// Alpha only
	if ((TComponent(Sender).Name) = 'txtPorts') OR
	  ((TComponent(Sender).Name) = 'cmbStatus') then
	begin
		if not(Key in [#8, #32, 'a' .. 'z', 'A' .. 'Z']) then
		begin
			// Discard the key
			Key := #0;
		end;
	end;

	// Alphanumeric only
	if ((TComponent(Sender).Name) = 'txtRego') OR
	  ((TComponent(Sender).Name) = 'txtFlight') OR
	  ((TComponent(Sender).Name) = 'cmbRelatedFlight') OR
	  ((TComponent(Sender).Name) = 'txtGate1') OR
	  ((TComponent(Sender).Name) = 'txtGate2') OR
	  ((TComponent(Sender).Name) = 'cmbBays') OR
	  ((TComponent(Sender).Name) = 'txtBelt1') OR
	  ((TComponent(Sender).Name) = 'txtBelt2') OR
	  ((TComponent(Sender).Name) = 'cmbRaceway') OR
	  ((TComponent(Sender).Name) = 'cmbCheckins') OR
	  ((TComponent(Sender).Name) = 'cmbAircrafts') OR
	  ((TComponent(Sender).Name) = 'cmbTerminals') OR
	  ((TComponent(Sender).Name) = 'cmbCarrierTypes') then
	begin
		if not(Key in [#8, 'a' .. 'z', 'A' .. 'Z', '0' .. '9']) then
		begin
			// Discard the key
			Key := #0;
		end;
	end;

end;

procedure TfrmEditAnD.FormCreate(Sender: TObject);
var
	FieldName: string;
	FieldVal: string;
	ffField: aFlightField;

	I, PTop, PLeft, PWidth: smallint;
	MaxLabelWidth, FormHeight: smallint;

	SameLevel: Boolean;
begin

	// Set form height to 0, it will be changed later in the fucntion
	self.Height := 0;

	lstGates := TStringList.Create;
	lstStatuses := TStringList.Create;
	lstBays := TStringList.Create;
	lstBelts := TStringList.Create;
	lstCarrierTypes := TStringList.Create;
	lstCheckin := TStringList.Create;
	lstAircrafts := TStringList.Create;
	lstTerminals := TStringList.Create;

	SetLength(lblField, length(FieldNames));
	SetLength(inpField, length(ffFields));

	I := 0;
	MaxLabelWidth := 0;
	FormHeight := 0;

	for FieldName in FieldNames do
	begin

		// This ignores date(ETA, ETA[date]) fields displaying as labels in the edit window.
		if (Pos('date', lowercase(FieldName)) = 0) then
		begin
			{ Positioning Labels }
			lblField[I] := TLabel.Create(self);
			lblField[I].SetParentComponent(self);
			lblField[I].Caption := FieldName + ':';
			lblField[I].AutoSize := true;
			lblField[I].Transparent := true;

			// if (MaxLabelWidth <= ucommon.MeasureTextLengthInPixels(lblField[i].caption, Self.Font) ) then
			if (MaxLabelWidth <= lblField[I].Width) then
				MaxLabelWidth := lblField[I].Width;

			{ position labels }
			if (lowercase(FieldName) = 'flight') then // 1 row 1 column
			begin
				lblField[I].Top := Row1;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'rego') then // 1 row 2 column
			begin
				lblField[I].Top := Row1;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'aircraft type') then // 1 row 3 column
			begin
				lblField[I].Top := Row1;
				lblField[I].Left := Col3;
			end;
			if (lowercase(FieldName) = 'ports') then // 2 row 1 column
			begin
				lblField[I].Top := Row2;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'status') then // 2 row 2 column
			begin
				lblField[I].Top := Row2;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'std') OR (lowercase(FieldName) = 'sta')
			then // 3 row 1 column
			begin
				lblField[I].Top := Row3;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'etd') OR (lowercase(FieldName) = 'eta')
			then // 3 row 2.5 column
			begin
				lblField[I].Top := Row3;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'off blocks') OR
			  (lowercase(FieldName) = 'on blocks') then // 4 row 1 column
			begin
				lblField[I].Top := Row4;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'atd') OR (lowercase(FieldName) = 'ata')
			then // 4 row 2 column
			begin
				lblField[I].Top := Row4;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'slot time') then // 4 row 3 column
			begin
				lblField[I].Top := Row4;
				lblField[I].Left := Col3;
			end;
			if (lowercase(FieldName) = 'gate') then // 5 row 1 column
			begin
				lblField[I].Top := Row5;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'bay') then // 5 row 2 column
			begin
				lblField[I].Top := Row5;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'terminal') then // 5 row 3 column
			begin
				lblField[I].Top := Row5;
				lblField[I].Left := Col3;
			end;
			if (lowercase(FieldName) = 'related flight') then // 6 row 1 column
			begin
				lblField[I].Top := Row2;
				lblField[I].Left := Col3;
			end;
			if (lowercase(FieldName) = 'check-in') then // 7 row 1 column
			begin
				lblField[I].Top := Row7;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'belts') then // 7 row 1 column
			begin
				lblField[I].Top := Row7;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'carrier') then // 7 row 2 column
			begin
				lblField[I].Top := Row7;
				lblField[I].Left := Col2;
			end;
			if (lowercase(FieldName) = 'public') then // 7 row 3 column
			begin
				lblField[I].Top := Row7;
				lblField[I].Left := Col3;
			end;
			if (lowercase(FieldName) = 'scheduled checkin counters') then
			// 8 row 1 column
			begin
				lblField[I].Top := Row8;
				lblField[I].Left := Col1;
				lblField[I].AutoSize := false;
				lblField[I].Width := 60;
				lblField[I].Height := 40;
				lblField[I].WordWrap := true;
			end;
			if (lowercase(FieldName) = 'checkin opening time') then
			// 8 row 2 column
			begin
				lblField[I].Top := Row8;
				lblField[I].Left := Col2;
				lblField[I].AutoSize := false;
				lblField[I].Width := 60;
				lblField[I].Height := 40;
				lblField[I].WordWrap := true;
			end;
			if (lowercase(FieldName) = 'checkin closing time') then
			// 8 row 3 column
			begin
				lblField[I].Top := Row8;
				lblField[I].Left := Col3;
				lblField[I].AutoSize := false;
				lblField[I].Width := 60;
				lblField[I].Height := 40;
				lblField[I].WordWrap := true;
			end;
			if (lowercase(FieldName) = 'lateral') then
			begin
				lblField[I].Top := Row8;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'message') then
			begin
				lblField[I].Top := Row9;
				lblField[I].Left := Col1;
			end;
			if (lowercase(FieldName) = 'crawling') then
			begin
				lblField[I].Top := Row9;
				lblField[I].Left := Col3;
			end;

			{ Positioning Bottom Panel }
			if (self.Height <= lblField[I].Top) then
				self.Height := lblField[I].Top;

			Inc(I);
		end;
	end;
	// Add little more space in the bottom of the window
	self.Height := self.Height + 110;

	I := 0;
	PTop := 0;
	PLeft := 0;

	for ffField in ffFields do
	begin
		SameLevel := false;

		// ShowMessage(inttostr(I));
		case ffField of
			{ TextFields }
			ffStaffNote:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtNote';
					inpField[I].SetParentComponent(self);
				end;
			ffStaffComment:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtStaffComment';
					inpField[I].SetParentComponent(self);
				end;
			ffOnBlock:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtOnBlock';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.OnBlock;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row4;
					inpField[I].Left := Col1 + 50;
					inpField[I].Width := 60;
				end;
			ffOffBlock:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtOffBlock';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.OffBlock;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row4;
					inpField[I].Left := Col1 + 50;
					inpField[I].Width := 60;
				end;
			ffPorts:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtPorts';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.Ports;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 40;
					inpField[I].Top := Row2;
					inpField[I].Left := Col1 + 40;
					inpField[I].Width := 140;
				end;
			ffRego:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtRego';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.Rego;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 10;
					inpField[I].Top := Row1;
					inpField[I].Left := Col2 + 40;
					inpField[I].Width := 60;
				end;
			ffSlotTime:
				begin
					inpField[I] := TEdit.Create(self);
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Name := 'txtSlotTime';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.Slottime;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row4;
					inpField[I].Left := Col3 + 60;
					inpField[I].Width := 50;
				end;
			ffScheduledCheckinCounters:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtScheduledCheckinCounters';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text :=
					  FlightData.ScheduledCheckinCounters;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row8;
					inpField[I].Left := Col1 + 60;
					inpField[I].Width := 80;
				end;
			ffCheckinOpeningTime:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtCheckinOpeningTime';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.CheckinOpeningTime;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row8;
					inpField[I].Left := Col2 + 60;
					inpField[I].Width := 80;
				end;
			ffCheckinClosingTime:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtCheckinClosingTime';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.CheckinClosingTime;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row8;
					inpField[I].Left := Col3 + 60;
					inpField[I].Width := 80;
				end;
			{ Date&Time Components }
			ffSTime:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'dtSTTime';
					inpField[I].Tag := 1;
					inpField[I].SetParentComponent(self);
					{ TDateTimePicker(inpField[I]).Kind := dtkTime;
					  TDateTimePicker(inpField[I]).Format := fidsGUITimeFormat; }
					TEdit(inpField[I]).Width := 60;
					TEdit(inpField[I]).Text :=
					  FormatDateTime('hhmm', FlightData.STime);
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row3;
					inpField[I].Left := Col1 + 40;
				end;

			ffSTdate:
				begin
					SameLevel := true;
					inpField[I] := TDateTimePicker.Create(self);
					inpField[I].Name := 'dtSTDate';
					TDateTimePicker(inpField[I]).Kind := dtkDate;
					inpField[I].SetParentComponent(self);
					TDateTimePicker(inpField[I]).Date := FlightData.STDate;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Top := Row3;
					inpField[I].Left := Col1 + 60 + 40;
					inpField[I].Width := 80;
				end;

			ffETime:
				begin

					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'dtETTime';
					inpField[I].Tag := 1;
					inpField[I].SetParentComponent(self);
					{ TDateTimePicker(inpField[I]).Kind := dtkTime;
					  TDateTimePicker(inpField[I]).Format := fidsGUITimeFormat; }
					TEdit(inpField[I]).Width := 60;
					TEdit(inpField[I]).Text :=
					  FormatDateTime('hhmm', FlightData.ETime);
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row3;
					inpField[I].Left := Col2 + 40;
				end;

			ffETdate:
				begin
					SameLevel := true;
					inpField[I] := TDateTimePicker.Create(self);
					inpField[I].Name := 'dtETDate';
					TDateTimePicker(inpField[I]).Kind := dtkDate;
					inpField[I].SetParentComponent(self);
					TDateTimePicker(inpField[I]).Width := 80;
					TDateTimePicker(inpField[I]).Date := FlightData.ETDate;

					inpField[I].Top := Row3;
					inpField[I].Left := Col2 + 40 + 60;
				end;
			ffATime:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'dtATTime';
					inpField[I].Tag := 1;
					inpField[I].SetParentComponent(self);
					{ TEdit(inpField[I]).Kind := dtkTime;
					  TEdit(inpField[I]).Format := fidsGUITimeFormat; }
					TEdit(inpField[I]).Width := 60;
					TEdit(inpField[I]).MaxLength := 4;
					TEdit(inpField[I]).Text :=
					  FormatDateTime('hhmm', FlightData.ATime);
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Top := Row4;
					inpField[I].Left := Col2 + 40;
				end;

			ffATdate:
				begin
					SameLevel := true;
					inpField[I] := TDateTimePicker.Create(self);
					inpField[I].Name := 'dtATDate';
					TDateTimePicker(inpField[I]).Kind := dtkDate;
					inpField[I].SetParentComponent(self);
					TDateTimePicker(inpField[I]).Date := FlightData.ATDate;

					inpField[I].Top := Row4;
					inpField[I].Left := Col2 + 40 + 60;
					inpField[I].Width := 80;

				end;
			{ Static Field, Not-editable }
			ffFlight:
				begin
					inpField[I] := TEdit.Create(self);
					inpField[I].Name := 'txtFlight';
					inpField[I].SetParentComponent(self);
					TEdit(inpField[I]).Text := FlightData.Flight;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 10;
					inpField[I].Top := Row1;
					inpField[I].Left := Col1 + 40;
					inpField[I].Width := 60;
				end;

			{ Dropdowns }
			ffRelatedFlight:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbRelatedFlight';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.RelatedFlight;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					TEdit(inpField[I]).MaxLength := 10;
					inpField[I].Top := Row2;
					inpField[I].Left := Col3 + 70;
					inpField[I].Width := 80;
				end;
			ffAStatus:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbStatus';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Status;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Top := Row2;
					inpField[I].Left := 40 + Col2;
					inpField[I].Width := 100;
				end;
			ffDStatus:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbStatus';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Status;
					TEdit(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Top := Row2;
					inpField[I].Left := 40 + Col2;
					inpField[I].Width := 100;
				end;
			ffGates:
				begin
					// This is a hack, as bernard wanted two inputboxes for gates

					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbGates';
					inpField[I].SetParentComponent(self);
					inpField[I].Visible := false; // Hide the gates inputbox
					// TComboBox(inpField[I]).Text := FlightData.Gates;

					Gates := TStringList.Create;
					Split(fidsSplitField, FlightData.Gates, Gates);

					// Two textboxes
					Gate1 := TEdit.Create(self);
					Gate1.Name := 'txtGate1';
					Gate1.OnKeyPress := EditKeyPress;

					Gate1.SetParentComponent(self);
					if (Gates.Count >= 1) then
						Gate1.Text := Gates[0];

					Gate1.MaxLength := 4;
					Gate1.Visible := true;

					Gate2 := TEdit.Create(self);
					Gate1.Name := 'txtGate2';
					Gate2.OnKeyPress := EditKeyPress;

					Gate2.SetParentComponent(self);
					if (Gates.Count > 1) then
						Gate2.Text := Gates[1];

					Gate2.MaxLength := 4;
					Gate2.Visible := true;

					// Positioning
					Gate1.Top := Row5;
					Gate1.Left := Col1 + 50;
					Gate1.Width := 30;

					Gate2.Top := Row5;
					Gate2.Left := Col1 + 50 + 30;
					Gate2.Width := 30;

					Gates.Free;
					{
					  inpField[I].Top := Row5;
					  inpField[I].Left := Col1 + 50;
					  inpField[I].Width := 60;
					}
				end;
			ffBays:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbBays';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Bays;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					TComboBox(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row5;
					inpField[I].Left := Col2 + 40;
					inpField[I].Width := 60;
				end;
			ffBelts:
				begin
					// This is a hack, as bernard wanted two inputboxes for gates

					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbBelts';
					inpField[I].SetParentComponent(self);
					inpField[I].Visible := false; // Hide the gates inputbox
					// TComboBox(inpField[I]).Text := FlightData.Gates;

					Belts := TStringList.Create;
					Split(fidsSplitField, FlightData.Belts, Belts);

					// Two textboxes
					Belt1 := TEdit.Create(self);
					Belt1.Name := 'txtBelt1';
					Belt1.OnKeyPress := EditKeyPress;
					Belt1.SetParentComponent(self);
					if (Belts.Count >= 1) then
						Belt1.Text := Belts[0];

					Belt1.MaxLength := 4;
					Belt1.Visible := true;

					Belt2 := TEdit.Create(self);
					Belt2.Name := 'txtBelt2';
					Belt2.OnKeyPress := EditKeyPress;
					Belt2.SetParentComponent(self);
					if (Belts.Count > 1) then
						Belt2.Text := Belts[1];

					Belt2.MaxLength := 4;
					Belt2.Visible := true;

					// Positioning
					Belt1.Top := Row7;
					Belt1.Left := Col1 + 50;
					Belt1.Width := 30;

					Belt2.Top := Row7;
					Belt2.Left := Col1 + 50 + 30;
					Belt2.Width := 30;

					Belts.Free;
					{
					  inpField[I].Top := Row5;
					  inpField[I].Left := Col1 + 50;
					  inpField[I].Width := 60;
					}

				end;
			ffRaceway:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbRaceway';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Raceway;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					TComboBox(inpField[I]).MaxLength := 5;
					inpField[I].Top := Row8;
					inpField[I].Left := Col1 + 50;
					inpField[I].Width := 60;
				end;

			ffCheckIns:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbCheckins';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Checkins;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					TComboBox(inpField[I]).MaxLength := 8;
					inpField[I].Top := Row7;
					inpField[I].Left := Col1 + 60;
					inpField[I].Width := 80;
				end;

			ffAirCraft:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbAircrafts';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Aircraft;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					TComboBox(inpField[I]).MaxLength := 20;
					inpField[I].Top := Row1;
					inpField[I].Left := 70 + Col3;
					inpField[I].Width := 80;
				end;
			ffTerminal:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbTerminals';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Terminal;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					TComboBox(inpField[I]).MaxLength := 4;
					inpField[I].Top := Row5;
					inpField[I].Left := 60 + Col3;
					inpField[I].Width := 60;
				end;

			ffCarrier:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbCarrierTypes';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Carrier;
					TComboBox(inpField[I]).OnKeyPress := EditKeyPress;

					inpField[I].Top := Row7;
					inpField[I].Left := Col2 + 40;
					inpField[I].Width := 100;

				end;
			ffComment:
				begin
					inpField[I] := TComboBox.Create(self);
					inpField[I].Name := 'cmbComment';
					inpField[I].SetParentComponent(self);
					TComboBox(inpField[I]).Text := FlightData.Comment;
					TComboBox(inpField[I]).Width := 300;

					inpField[I].Top := Row9;
					inpField[I].Left := Col1 + 80;
				end;
			{ Checkboxes }
			ffNonPublic:
				begin
					inpField[I] := TCheckBox.Create(self);
					inpField[I].Name := 'chkNonPublic';
					inpField[I].SetParentComponent(self);
					TCheckBox(inpField[I]).Caption := '';
					// ShowMessage(flightdata.NonPublic);
					if (Trim(FlightData.NonPublic) = '1') then
						TCheckBox(inpField[I]).Checked := true;

					if (Trim(FlightData.NonPublic) = '0') then
						TCheckBox(inpField[I]).Checked := false;

					inpField[I].Top := Row7;
					inpField[I].Left := Col3 + 40;
					inpField[I].Width := 20;
				end;
			ffCrawling:
				begin
					inpField[I] := TCheckBox.Create(self);
					inpField[I].Name := 'chkCrawling';
					inpField[I].SetParentComponent(self);
					TCheckBox(inpField[I]).Caption := '';
					// ShowMessage(flightdata.NonPublic);
					if (Trim(FlightData.Crawling) = '1') then
						TCheckBox(inpField[I]).Checked := true;

					if (Trim(FlightData.Crawling) = '0') then
						TCheckBox(inpField[I]).Checked := false;

					inpField[I].Top := Row9;
					inpField[I].Left := Col3 + 50;
					inpField[I].Width := 20;
				end;
		end;

		// Dont show
		// inpField[I].Visible := true;

		Inc(I);
	end;

end;

end.
