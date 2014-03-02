unit FIndicators;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, VirtualTrees, uCommon, uController, uFlight, uFidsTags;

type
	PSensor = ^TSensor;

	TSensor = record
		IP: String;
		Status: String;
		Kind: String;
	end;

type
	TfrmManageIndicators = class(TForm)
		VST: TVirtualStringTree;
		mnuMain: TMainMenu;
		Edit1: TMenuItem;
		Cut1: TMenuItem;
		Copy1: TMenuItem;
		Past1: TMenuItem;
		Delete1: TMenuItem;
		N1: TMenuItem;
		Filter1: TMenuItem;
		mnuContext: TPopupMenu;
		View1: TMenuItem;
		Refresh1: TMenuItem;
		procedure FormShow(Sender: TObject);
		procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
		procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
		  var NodeDataSize: Integer);
		procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
		  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
		procedure VSTPaintText(Sender: TBaseVirtualTree;
		  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
		  TextType: TVSTTextType);
	public
		{ Public declarations }
	end;

var
	frmManageIndicators: TfrmManageIndicators;
	FlightFields: array of aFlightField;
	fcWindow: CFlightController;
	Indicators: array of TSensor;

implementation

{$R *.dfm}

procedure TfrmManageIndicators.FormShow(Sender: TObject);
var
	SensorType, PrevSensorType: String;
	SensorCount, I, IndicatorIndex: Integer;

	// VST Related
	TableData: PSensor;
	ChildNode, ParentNode: PVirtualNode;
begin
	VST.Clear;
	VST.Header.Columns.Clear;

	fcWindow := CFlightController.Create(fkDepartures, FlightFields);
	Setlength(Indicators, 100);
	IndicatorIndex := 0;

	// Add three columns as Datastructure has only three
	VST.Header.Columns.Add.Text := 'IP';
	VST.Header.Columns.Add.Text := 'Status';
	VST.Header.Columns.Add.Text := 'Kind';
	VST.Header.Columns.Items[0].Width := 120;
	VST.Header.Columns.Items[1].Width := 100;
	VST.Header.Columns.Items[2].Width := 80;

	for SensorType in fcWindow.GetSensorTypes do
	begin
		SensorCount :=
		  (fcWindow.GetSensorCount(fcWindow.StringToSensorType(SensorType)));

		// Add Groups of sensor types
		Indicators[IndicatorIndex].IP := SensorType;
		Indicators[IndicatorIndex].Status := '';
		Indicators[IndicatorIndex].Kind := '';

		ParentNode := VST.AddChild(nil);
		TableData := VST.GetNodeData(ParentNode);
		TableData^ := Indicators[IndicatorIndex];
		Inc(IndicatorIndex);

		for I := 0 to (SensorCount - 1) do
		begin
			// ShowMessage(inttostr(IndicatorIndex));
			Indicators[IndicatorIndex].IP :=
			  fcWindow.GetSensorData
			  ((fcWindow.StringToSensorType(SensorType)), IP, I);
			Indicators[IndicatorIndex].Status :=
			  fcWindow.GetSensorData((fcWindow.StringToSensorType(SensorType)),
			  Status, I);
			Indicators[IndicatorIndex].Kind := SensorType;

			// Add Groups of sensor types
			ChildNode := VST.AddChild(ParentNode);
			TableData := VST.GetNodeData(ChildNode);
			TableData^ := Indicators[IndicatorIndex];

			Inc(IndicatorIndex);
		end; // end FOR

	end; // end FOR IN

end;

procedure TfrmManageIndicators.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
	VST.Refresh;
end;

procedure TfrmManageIndicators.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
	NodeDataSize := SizeOf(TSensor);
end;

procedure TfrmManageIndicators.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
	Data: PSensor;
begin
	Data := VST.GetNodeData(Node);
	case Column of
		0:
			CellText := Data^.IP;
		1:
			CellText := Data^.Status;
		2:
			CellText := Data^.Kind;
	end;
end;

procedure TfrmManageIndicators.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
Var
	Data: PSensor;
begin
	Data := VST.GetNodeData(Node);

	try
		if LowerCase(VST.Header.Columns.Items[Column].Text) = 'status' then
		begin

			if LowerCase(Data^.Status) = 'down' then
			begin
				TargetCanvas.Font.Color := clRed;
				TargetCanvas.Font.Style := [fsBold];
			end;

			if LowerCase(Data^.Status) = 'up' then
			begin
				TargetCanvas.Font.Color := clBlue;
				TargetCanvas.Font.Style := [fsBold];
			end;

		end;
	except

	end;

end;

end.
