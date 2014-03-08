unit FEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, uCommon, uGlobalDefs, uFlight, uFidsTags,
  ComCtrls, VrControls, VrBlinkLed;

type
  TfrmEdit = class(TForm)
    pnlButtons: TPanel;
    btnClose: TButton;
    Button3: TButton;
    tmrBackground: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrBackgroundTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
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
  frmEdit: TfrmEdit;
  FieldNames: array of String;
  ffFields: array of aFlightField;
  FlightData: PTreeData;

  lblField: array of TLabel;
  inpField: array of TControl;

implementation

{$R *.dfm}

procedure TfrmEdit.FormDestroy(Sender: TObject);
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

procedure TfrmEdit.FormShow(Sender: TObject);
begin
  // Choose appopriate title for edit windows
  Caption := Caption + ' ' + FlightData.Flight;
end;

procedure TfrmEdit.SetData(InData: PTreeData);
begin
  FlightData := InData;
end;

procedure TfrmEdit.SetFields(IffFields: array of aFlightField;
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

procedure TfrmEdit.tmrBackgroundTimer(Sender: TObject);
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

procedure TfrmEdit.FormCreate(Sender: TObject);
var
  FieldName: string;
  FieldVal: string;
  ffField: aFlightField;

  I, PTop, PLeft, PWidth: smallint;
  MaxLabelWidth, FormHeight: smallint;

  SameLevel: Boolean;
begin

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

      // if (MaxLabelWidth <= ucommon.MeasureTextLengthInPixels(lblField[i].caption, Self.Font) ) then
      if (MaxLabelWidth <= lblField[I].Width) then
        MaxLabelWidth := lblField[I].Width;

      { Only 10 vertically }
      if ((I mod length(FieldNames) - 1) = 0) AND NOT(I = 0) then
      begin
        PTop := uCommon.ComponentTop;
        PLeft := 280;
      end
      else
      begin
        PLeft := uCommon.ComponentLeft;
        { Calculate TOP Pixel }
        if (I = 0) then
          PTop := uCommon.ComponentTop
        else
          PTop := PTop + (lblField[I].Height * 2);
      end;

      { position labels }
      lblField[I].Top := PTop;
      lblField[I].Left := PLeft;

      { Set form height }
      if (FormHeight <= PTop) then
        FormHeight := PTop;

      Inc(I);
    end;

  end;

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
      ffOffBlock:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtOffBlock';
          inpField[I].SetParentComponent(self);
        end;

      ffPorts:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtPorts';
          inpField[I].SetParentComponent(self);
          TEdit(inpField[I]).Text := FlightData.Ports;
        end;

      ffRego:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtRego';
          inpField[I].SetParentComponent(self);
          TEdit(inpField[I]).Text := FlightData.Rego;
        end;
      ffSlotTime:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtSlotTime';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Text := FlightData.Rego;
        end;
      ffRelatedFlight:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtRelatedFlight';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Text := FlightData.Rego;
        end;
      ffScheduledCheckinCounters:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtScheduledCheckinCounters';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Text := FlightData.Rego;
        end;
      ffCheckinOpeningTime:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtCheckinOpeningTime';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Text := FlightData.Rego;
        end;
      ffCheckinClosingTime:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtCheckinClosingTime';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Text := FlightData.Rego;
        end;

      { Date&Time Components }
      ffSTime:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'dtSTTime';

          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Kind := dtkTime;
          // TEdit(inpField[I]).Format := fidsGUITimeFormat;
          TEdit(inpField[I]).Width := 60;
          TEdit(inpField[I]).Text := FormatDateTime('hhmm', FlightData.STime);
        end;

      ffSTdate:
        begin
          SameLevel := true;
          inpField[I] := TDateTimePicker.Create(self);
          inpField[I].Name := 'dtSTDate';
          inpField[I].Tag := 1;
          TDateTimePicker(inpField[I]).Kind := dtkDate;
          inpField[I].SetParentComponent(self);
          TDateTimePicker(inpField[I]).Width := 100;
          TDateTimePicker(inpField[I]).Date := FlightData.STDate;
        end;

      ffETime:
        begin

          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'dtETTime';

          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Kind := dtkTime;
          // TEdit(inpField[I]).Format := fidsGUITimeFormat;
          TEdit(inpField[I]).Width := 60;
          TEdit(inpField[I]).Text := FormatDateTime('hhmm', FlightData.ETime);
        end;

      ffETdate:
        begin
          SameLevel := true;
          inpField[I] := TDateTimePicker.Create(self);
          inpField[I].Name := 'dtETDate';
          inpField[I].Tag := 1;
          TDateTimePicker(inpField[I]).Kind := dtkDate;
          inpField[I].SetParentComponent(self);
          TDateTimePicker(inpField[I]).Width := 100;
          TDateTimePicker(inpField[I]).Date := FlightData.ETDate;
        end;
      ffATime:
        begin

          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'dtATTime';
          inpField[I].SetParentComponent(self);
          // TEdit(inpField[I]).Kind := dtkTime;
          // TEdit(inpField[I]).Format := fidsGUITimeFormat;
          TEdit(inpField[I]).Width := 60;
          TEdit(inpField[I]).Text := FormatDateTime('hhmm', FlightData.ATime);

        end;

      ffATdate:
        begin
          SameLevel := true;
          inpField[I] := TDateTimePicker.Create(self);
          inpField[I].Name := 'dtATDate';
          inpField[I].Tag := 1;
          TDateTimePicker(inpField[I]).Kind := dtkDate;
          inpField[I].SetParentComponent(self);
          TDateTimePicker(inpField[I]).Width := 100;
          TDateTimePicker(inpField[I]).Date := FlightData.ATDate;
        end;
      { Static Field, Not-editable }
      ffFlight:
        begin
          inpField[I] := TEdit.Create(self);
          inpField[I].Name := 'txtFlight';
          inpField[I].SetParentComponent(self);
          TEdit(inpField[I]).Text := FlightData.Flight;
        end;

      { Dropdowns }
      ffAStatus:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbStatus';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Status;
        end;

      ffDStatus:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbStatus';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Status;
        end;

      ffGates:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbGates';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Gates;
        end;

      ffBays:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbBays';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Bays;
        end;

      ffBelts:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbBelts';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Belts;
        end;

      ffCheckIns:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbCheckins';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Checkins;
        end;

      ffAirCraft:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbAircrafts';
          inpField[I].SetParentComponent(self);
        end;

      ffTerminal:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbTerminals';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Terminal;
        end;

      ffCarrier:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbCarrierTypes';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Carrier;
        end;
      ffComment:
        begin
          inpField[I] := TComboBox.Create(self);
          inpField[I].Name := 'cmbComment';
          inpField[I].SetParentComponent(self);
          TComboBox(inpField[I]).Text := FlightData.Comment;
          TComboBox(inpField[I]).Width := 380;
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

        end;
    end;

    { Only 10 vertically }
    if ((I mod length(ffFields) - 1) = 0) AND NOT(I = 0) then
    begin
      PTop := uCommon.ComponentTop;
    end
    else
    begin
      { Calculate TOP Pixel }
      if not SameLevel
      then { ST/ET/AT should be in the same level and never split }
        if (I = 0) then
          PTop := uCommon.ComponentTop
        else
          PTop := PTop + (lblField[0].Height * 2);
    end;

    { position labels }
    inpField[I].Top := PTop;

    Inc(I);
  end;

  { assigning left value for components }
  I := 0;
  PLeft := 0;
  for ffField in ffFields do
  begin
    if ((I mod length(ffFields) - 1) = 0) AND NOT(I = 0) then
      PLeft := 320
    Else if inpField[I].Tag = 1 then
      PLeft := uCommon.ComponentLeft + uCommon.ComponentLeft + MaxLabelWidth +
        inpField[I].Width +
        -40 { Gap | Associated with Splitfield onlt, ET, ETA, etc }
    else
      PLeft := uCommon.ComponentLeft + uCommon.ComponentLeft + MaxLabelWidth;

    // ShowMessage(inttostr(PLeft));
    inpField[I].Left := PLeft;
    Inc(I);
  end;

  { Positioning Bottom Panel }
  self.Height := PTop + uCommon.ComponentAndButtonsGap + pnlButtons.Height;

end;

end.
