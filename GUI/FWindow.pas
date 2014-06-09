unit FWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uController, Grids, StdCtrls, uGlobalDefs, uFlight, uFidsTags,
  Registry,
  ImgList, Menus, ExtCtrls, ComCtrls, ToolWin, VirtualTrees, usettingsmanager,
  uCommon, Buttons, uTTRules, VrControls, VrLcd, ActnPopup, StdActns,
  ActnList, PlatformDefaultStyleActnCtrls, ActnMan, ActnCtrls, ActnMenus,
  ActnColorMaps, RibbonLunaStyleActnCtrls, ufStringEntry, uDbTree, uGT,
  uUtils, uPoller, ulogin;

type
  TfrmWindow = class(TForm)
    stMain: TStatusBar;
    panelSensors: TFlowPanel;
    imlSensors: TImageList;
    bhManager: TBalloonHint;
    pnlSearchPanel: TPanel;
    tbClearSearch: TSpeedButton;
    VST: TVirtualStringTree;
    pnlClock: TPanel;
    VrClock1: TVrClock;
    lblSeparator: TLabel;
    imlMain: TImageList;
    mnuQuickSelect: TPopupMenu;
    Arrivals3: TMenuItem;
    Departures3: TMenuItem;
    Checkins2: TMenuItem;
    Gates2: TMenuItem;
    Bays2: TMenuItem;
    Belts1: TMenuItem;
    ammbMainMenu: TActionMainMenuBar;
    amMainMenu: TActionManager;
    HelpContents1: THelpContents;
    colorMapMenu: TStandardColorMap;
    FileExit1: TFileExit;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditDelete1: TEditDelete;
    amnuFind: TSearchFind;
    amnuIndicatorManager: TAction;
    ControlBar1: TControlBar;
    tblDetails: TToolBar;
    tbbModify: TToolButton;
    tbbGantt: TToolButton;
    tbbHome: TToolButton;
    tbExtended: TToolBar;
    tbDummyButton: TToolButton;
    tbBasic: TToolBar;
    tbbAdd: TToolButton;
    tbbDelete: TToolButton;
    ToolButton1: TToolButton;
    amnuCrawlingLines: TAction;
    lblHostUnavailable: TLabel;
    tbbCodeshare: TToolButton;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbDummyButtonClick(Sender: TObject);
    procedure Find2Click(Sender: TObject);
    procedure tbClearSearchClick(Sender: TObject);
    procedure tbbModifyClick(Sender: TObject);
    procedure tbbAddClick(Sender: TObject);
    procedure tbbDeleteClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ControlBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Arrivals3Click(Sender: TObject);
    procedure Checkins2Click(Sender: TObject);
    procedure Departures3Click(Sender: TObject);
    procedure Gates2Click(Sender: TObject);
    procedure Bays2Click(Sender: TObject);
    procedure Belts1Click(Sender: TObject);
    procedure tbbHomeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure amnuCrawlingLinesExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; ItemRect: TRect);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure tbbGanttClick(Sender: TObject);
    procedure tbbCodeshareClick(Sender: TObject);
    procedure VSTCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure VSTAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; ItemRect: TRect);
  private
    { Main db backend handling Object }
    fcWindow: CFlightController;
    oRule: cTTRule;
    { Private declarations }
    procedure SetDetail(Field: aFlightField; Detail: string);
    procedure PopulateGrid();
    procedure PopulateVertically(SearchField: SmallInt; SearchInput: Variant);
    procedure PopulateHorizontally(SearchField: SmallInt; SearchInput: Variant);
    procedure UpdateFlightData(ComponentName: String; ObjectType: string;
      ffField: aFlightField);
    procedure ConnectionEvent(event: aConnectionEvent; param: string);
    procedure AddNewFlight(CodeShare: Boolean; ExistingFlight: apNode);
    function NodesCount(vtTree: TVirtualStringTree): int64;
    function FindInTable(row: Integer): String;

    procedure DrawItemText(X: Integer; ACanvas: TCanvas; ARect: TRect;
      Text: string);
    procedure CustomDrawMenu(Menu: TMenuItem; Selected: Boolean;
      ACanvas: TCanvas; ARect: TRect);
    function ExpandNodes(): Integer;
    procedure InitialiseWindow();
  public
    { Public declarations }
    { Window specific variables }
    ControllerName: String;
    ControllerMode: TFIDSWindowMode;
    ControllerType: TFIDSWindowType;
    ControllerID: TFIDSWindowID;

    // Login
    Login: cLogin;

    afkWindowKind: aFlightKind;
    FlightFields: array of aFlightField;
    ColumnNames: array of string;

    procedure Initialize();
    procedure SetController(WindowName: string; fKind: aFlightKind;
      fFields: array of aFlightField; InpColumnNames: array of string);

  protected
    procedure CreateParams(var Params: TCreateParams); override;

  end;

var
  frmWindow: TfrmWindow;
  frmWindows: array [0 .. 7] of TfrmWindow;
  { Following vars to keep track of selected nodes, scroll position, sorting when redraw grid every second }
  SelectedFlightPath: String;

  { Zebra strip }
  ParentNodes: array [0 .. 10000] of Cardinal;
  
  { Remember Filters/Searched strings }
  SearchString: string;
  SearchField: SmallInt;
  SearchCaseSensitive: Boolean;

  { Dodgy flags }
  ErrorOccuredVSTMouseUP: Boolean;
  mShutDown: Boolean;

  { Flags to detect dyanmic components }
  SensorsPanelInitialised: Boolean;
  StatusButtonsInitialised: Boolean;

  { Remember status of expanding nodes and we need to disable expanding/collapsing of
    nodes for users (as of 01/06/2014) }
  DisableNodeCollapsing: Boolean;

implementation

uses FSearch, FEditAnD, ufRuleEdit, FIndicators, FMain, FEdit, FCrawlineLines,
  uConnection,
  CrawlingEdit;

{$R *.dfm}

procedure TfrmWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmWindow.SetController(WindowName: string; fKind: aFlightKind;
  fFields: array of aFlightField; InpColumnNames: array of string);
Var
  I: Integer;
  ColumnName: String;
  ffField: aFlightField;
begin
  SetLength(FlightFields, length(fFields));
  SetLength(ColumnNames, length(InpColumnNames));

  I := 0;
  for ffField in fFields do
  begin
    FlightFields[I] := ffField;
    Inc(I);
  end;

  I := 0;
  for ColumnName in InpColumnNames do
  begin
    ColumnNames[I] := ColumnName;
    Inc(I);
  end;

  self.ControllerName := WindowName;
  Caption := WindowName;
  afkWindowKind := fKind;

  { Window specific object creation }
  fcWindow := CFlightController.Create(afkWindowKind, FlightFields);

  SearchField := -1;
  SearchString := '';

  // Add columns to the VST
  if ControllerType = FIDSVerticallyPopulated then
  begin
    { For Arrivals/Dep/T-Arrivals/T-Departures }
    { Populate colums of VST }
    for I := 0 to HIGH(InpColumnNames) do
    begin
      if (LowerCase(InpColumnNames[I]) = 'crawling line') OR
        (LowerCase(InpColumnNames[I]) = 'nonpublic') then
      else
      begin
        VST.Header.Columns.Add();
        VST.Header.Columns.Items[I].Width := 100;
      end;
    end;
  end
  else
  begin
    { Checkins/Bays/Belts/Gates }
    { Populate colums of VST }
    for I := 0 to 100 do
    begin
      VST.Header.Columns.Add();
      VST.Header.Columns.Items[I].Width := 80;
    end;
  end;

end;

procedure TfrmWindow.PopulateGrid();
var
  ForceRefresh : Boolean;
begin
  ForceRefresh := True;

  if (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) then
  begin
    fcWindow.ControllerID := ControllerID;
    fcWindow.PopulateTTGrid();
  end
  else
  begin
    fcWindow.PopulateGrid(ForceRefresh);
  end;

  if (ControllerType = FIDSHorizontallyPopulated) then
    self.PopulateHorizontally(SearchField, SearchString)
  else
    self.PopulateVertically(SearchField, SearchString);
end;

procedure TfrmWindow.Departures3Click(Sender: TObject);
begin
  // if (ucommon.DeparturesRunning) then
  // begin
  // ShowWindow(ucommon.DeparturesHandle, SW_RESTORE);
  // Exit;
  // end;

  { Set departures window }
  if (assigned(frmWindows[1])) then
  begin
    frmWindows[1].Show;
    frmWindows[1].WindowState := wsNormal;
  end
  else
  begin
    frmWindows[1] := TfrmWindow.Create(nil);
    frmWindows[1].ControllerType := FIDSVerticallyPopulated;
    frmWindows[1].ControllerID := FIDSDepartures;
    frmWindows[1].SetController('Departures - ' + fcWindow.GetJobName(),
      fkDepartures, uCommon.DeparturesSortedFields,
      uCommon.DeparturesSortedColumns);
    frmWindows[1].Show;
  end;
end;

procedure TfrmWindow.DrawItemText(X: Integer; ACanvas: TCanvas; ARect: TRect;
  Text: string);
begin
  ARect.Left := X;
  DrawText(ACanvas.Handle, PChar(Text), -1, ARect, DT_LEFT or DT_VCENTER or
    DT_SINGLELINE or DT_NOCLIP);
end;

procedure TfrmWindow.amnuCrawlingLinesExecute(Sender: TObject);
begin
  FCrawlineLinesAllocator := TFCrawlineLinesAllocator.Create(nil);
  FCrawlineLinesAllocator.Show;
end;

procedure TfrmWindow.Arrivals3Click(Sender: TObject);
begin
  // if (ucommon.ArrivalsRunning) then
  // begin
  // ShowWindow(ucommon.ArrivalsHandle, SW_RESTORE);
  // Exit;
  // end;

  if (assigned(frmWindows[0])) then
  begin
    frmWindows[0].Show;
    frmWindows[0].WindowState := wsNormal;
  end
  else
  begin
    frmWindows[0] := TfrmWindow.Create(nil);
    frmWindows[0].ControllerType := FIDSVerticallyPopulated;
    frmWindows[0].ControllerID := FIDSArrivals;
    frmWindows[0].SetController('Arrivals - ' + fcWindow.GetJobName(),
      fkArrivals, uCommon.ArrivalSortedFields, uCommon.ArrivalSortedColumns);
    frmWindows[0].Show;
  end;
end;

procedure TfrmWindow.Bays2Click(Sender: TObject);
begin
  ShowMessage('Still improving bays as a summary window, Not finished');
end;

procedure TfrmWindow.Belts1Click(Sender: TObject);
begin
  // if (ucommon.BeltsRunning) then
  // begin
  // ShowWindow(ucommon.BeltsHandle, SW_RESTORE);
  // Exit;
  // end;

  { Set checkkins window }
  if (assigned(frmWindows[5])) then
  begin
    frmWindows[5].Show;
    frmWindows[5].WindowState := wsNormal;
  end
  else
  begin
    frmWindows[5] := TfrmWindow.Create(nil);
    frmWindows[5].ControllerType := FIDSHorizontallyPopulated;
    frmWindows[5].ControllerID := FIDSBelts;
    frmWindows[5].SetController('Belts - ' + fcWindow.GetJobName(), fkArrivals,
      uCommon.BeltsFields, uCommon.BeltsColumns);
    frmWindows[5].Show;
  end;
end;

procedure TfrmWindow.Checkins2Click(Sender: TObject);
begin
  // if (ucommon.CheckinsRunning) then
  // begin
  // ShowWindow(ucommon.CheckinsHandle, SW_RESTORE);
  // Exit;
  // end;

  { Set checkkins window }
  if (assigned(frmWindows[2])) then
  begin
    frmWindows[2].Show;
    frmWindows[2].WindowState := wsNormal;
  end
  else
  begin
    frmWindows[2] := TfrmWindow.Create(nil);
    frmWindows[2].ControllerType := FIDSHorizontallyPopulated;
    frmWindows[2].ControllerID := FIDSCheckins;
    frmWindows[2].SetController('Check-in - ' + fcWindow.GetJobName(),
      fkDepartures, uCommon.CheckinsFields, uCommon.CheckinsColumns);
    frmWindows[2].Show;
  end;
end;

procedure TfrmWindow.ControlBar1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  tbBasic.Left := ControlBar1.Width - 90;
end;

procedure TfrmWindow.CustomDrawMenu(Menu: TMenuItem; Selected: Boolean;
  ACanvas: TCanvas; ARect: TRect);
begin
  if Selected then
    ACanvas.Brush.Color := clHighlight
  else
    ACanvas.Brush.Color := clGreen;

  { move the rect to make place for the side bar }
  // ARect.Left := 10;

  ACanvas.FillRect(ARect);
  ACanvas.Font.Style := [];

  { user defined text drawing function: }
  DrawItemText(ARect.Left, ACanvas, ARect, Menu.Caption);
end;

procedure TfrmWindow.Find2Click(Sender: TObject);
var
  I: Int16;
  strCol: String;
begin

  if (ControllerType = FIDSVerticallyPopulated) then
  begin

    Application.CreateForm(TfrmSearch, frmSearch);

    // Add columns to search types
    frmSearch.Fields := TStringList.Create;
    I := 0;

    for strCol in ColumnNames do
    begin
      frmSearch.Fields.Add(strCol);
      Inc(I);
    end;

    if (frmSearch.ShowModal = SHOW_OPENWINDOW) then
      if (frmSearch.ModalResult = mrOk) then
      begin
        { when search returns string and field }
        SearchString := frmSearch.cmbsearch.Text;
        SearchField := frmSearch.cmbFieldList.ItemIndex;
        SearchCaseSensitive := frmSearch.chkCase.Checked;

        self.pnlSearchPanel.Visible := True;
        self.tbClearSearch.Visible := True;

        // Set window mode to SEARCH
        ControllerMode := FIDSSearchMode;
        VST.Clear;

        PopulateGrid();
      end;
  end
  else
  begin
    ShowMessage('Search is not implemented for this Window');
  end;

end;

procedure TfrmWindow.FormActivate(Sender: TObject);
begin
  panelSensors.Refresh;
  panelSensors.Repaint;
end;

procedure TfrmWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  case (ControllerID) of
    FIDSArrivals:
      uCommon.ArrivalsRunning := false;
    FIDSDepartures:
      uCommon.DeparturesRunning := false;
    FIDSBays:
      uCommon.BaysRunning := false;
    FIDSGates:
      uCommon.GatesRunning := false;
    FIDSBelts:
      uCommon.BeltsRunning := false;
    FIDSCheckins:
      uCommon.CheckinsRunning := false;
  end;

end;

procedure TfrmWindow.Initialize();
begin
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfRuleEdit, fRuleEdit);
  Application.CreateForm(TfrmManageIndicators, frmManageIndicators);
  Application.CreateForm(TfrmEdit, frmEdit);
  Application.CreateForm(TFCrawlingLineEdit, FCrawlingLineEdit);
end;

procedure TfrmWindow.FormCreate(Sender: TObject);
begin
  SensorsPanelInitialised := false;
  StatusButtonsInitialised := false;
  DisableNodeCollapsing := true;
end;

procedure TfrmWindow.FormResize(Sender: TObject);
begin
  tbBasic.Width := 90;
  tbBasic.Left := ControlBar1.Width - 90;
end;

procedure TfrmWindow.FormShow(Sender: TObject);
begin
  // Bind connection event reader for UI purposes
  uConnection.Xml_Connection.RegisterEventReader(ConnectionEvent);

  // Assume we have an active connection as Login and other steps must have
  // been successful to get here. Unless the server is stopped after running
  PopulateGrid;
  VST.Repaint;
  InitialiseWindow;

  // Focus on the first node on FormLoad.
  VST.ScrollIntoView(VST.GetFirst(), true);
end;

procedure TfrmWindow.InitialiseWindow();
var
  I: SmallInt;
  { Statuses related }
  Status: String;
  Button, Separator: TToolButton;
  Statuses: TStringList;

  icon: TIcon;
begin
  ControllerMode := FIDSListingMode;

  VST.TreeOptions.PaintOptions := [
    toShowVertGridLines,
    toShowHorzGridLines,
    toFullVertGridLines,
    toHotTrack
  ];

  if (ControllerType = FIDSHorizontallyPopulated) then
  begin
    VST.TreeOptions.SelectionOptions := [toExtendedFocus];
  end
  else
  begin
    // vst.Header.Options := [hoVisible, hoHotTrack];
    VST.TreeOptions.SelectionOptions := [toFullRowSelect];
  end;

  if (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) then
  begin
    tbExtended.Visible := false;
  end;

  if (ControllerID = FIDSCheckins) OR (ControllerID = FIDSBays) OR
    (ControllerID = FIDSBelts) then
    tbbGantt.Visible := True
  else
    tbbGantt.Visible := True;

  // TODO: Fix this window handlers. Used for focussing a perticular window from the main screen and sub screens.
  // case (ControllerID) of
  // FIDSArrivals:  begin uCommon.ArrivalsHandle := Handle; uCommon.ArrivalsRunning := true; end;
  // FIDSDepartures:  begin ucommon.DeparturesHandle := Handle;  uCommon.DeparturesRunning := true;  end;
  // FIDSBays:  begin uCommon.BaysHandle := Handle; uCommon.BaysRunning := true; end;
  // FIDSGates: begin uCommon.GatesHandle := Handle;  uCommon.GatesRunning := true; end;
  // FIDSBelts: begin uCommon.BeltsHandle := Handle; uCommon.BeltsRunning := true; end;
  // FIDSCheckins: begin  uCommon.CheckinsHandle := Handle; uCommon.CheckinsRunning := true; end;
  // end;

  VrClock1.Color := rgb(0, 5, 5);
  VrClock1.Palette.Low := rgb(0, 20, 20);
  VrClock1.Palette.High := rgb(0, 255, 255);
  pnlClock.Color := rgb(0, 0, 0);

  // -------------------

  // Set Icon Dynamically
  icon := TIcon.Create;
  if (ControllerID = FIDSArrivals) then
  begin
    colorMapMenu.Color := rgb(0, 180, 235);
    panelSensors.Color := colorMapMenu.Color;
    // pnlClock.Color := colorMapMenu.Color;
    imlMain.GetIcon(2, icon);
  end;

  if (ControllerID = FIDSDepartures) then
  begin
    colorMapMenu.Color := rgb(140, 200, 120);
    panelSensors.Color := colorMapMenu.Color;
    // pnlClock.Color := colorMapMenu.Color;
    imlMain.GetIcon(12, icon);
  end;

  if (ControllerID = FIDSCheckins) then
  begin
    colorMapMenu.Color := rgb(255, 80, 102);
    panelSensors.Color := colorMapMenu.Color;

    imlMain.GetIcon(7, icon);
  end;

  if (ControllerID = FIDSBays) then
  begin
    colorMapMenu.Color := rgb(0, 160, 130);
    panelSensors.Color := colorMapMenu.Color;

    imlMain.GetIcon(3, icon);
  end;

  if (ControllerID = FIDSBelts) then
  begin
    colorMapMenu.Color := rgb(255, 255, 180);
    panelSensors.Color := colorMapMenu.Color;

    imlMain.GetIcon(4, icon);
  end;

  if (ControllerID = FIDSGates) then
  begin
    colorMapMenu.Color := rgb(230, 230, 180);
    panelSensors.Color := colorMapMenu.Color;

    imlMain.GetIcon(19, icon);
  end;

  self.icon := icon;
  // END - Set Icon Dynamically

  I := 0;
  oRule := cTTRule.Create(DB, Login.GetUserName);

  Statuses := fcWindow.GetStatuses;
  if (ControllerType = FIDSVerticallyPopulated) AND
    (StatusButtonsInitialised = false) then
  begin
    { Flag this process so next time the window gets created we wouldnt have duplicates }
    StatusButtonsInitialised := True;

    { Build status change buttons }
    for Status in Statuses do
    begin
      Button := TToolButton.Create(tbExtended);

      if LowerCase(Status) = 'cancelled' then
        if (afkWindowKind = fkArrivals) then
        begin
          Button.ImageIndex := 1
        end
        else
        begin
          Button.ImageIndex := 14;
        end;

      if LowerCase(Status) = 'landed' then
        Button.ImageIndex := 20;
      if LowerCase(Status) = 'diverted' then
        Button.ImageIndex := 16;
      if LowerCase(Status) = 'on blocks' then
        Button.ImageIndex := 22;
      if LowerCase(Status) = 'boarding' then
        Button.ImageIndex := 5;
      if LowerCase(Status) = 'final call' then
        Button.ImageIndex := 17;
      if LowerCase(Status) = 'closed' then
        Button.ImageIndex := 9;
      if LowerCase(Status) = 'departed' then
        Button.ImageIndex := 13;
      if LowerCase(Status) = 'off blocks' then
        Button.ImageIndex := 21;
      if LowerCase(Status) = 'take off' then
        Button.ImageIndex := 23;
      if LowerCase(Status) = 'pre landed' then
        Button.ImageIndex := 2;
      if LowerCase(Status) = 'delayed' then
        Button.ImageIndex := 10;

      Button.SetParentComponent(tbExtended);
      Button.AutoSize := True;
      Button.Caption := Status;
      Button.OnClick := tbDummyButton.OnClick;
      tbExtended.Width := tbExtended.Width + Button.Width;
      Inc(I);
    end;

    // Adding the clear Button on both Arrivals and Departures
    if (ControllerID = FIDSArrivals) OR (ControllerID = FIDSDepartures) then
    begin
      Button := TToolButton.Create(tbExtended);
      Button.ImageIndex := 8;
      Button.SetParentComponent(tbExtended);
      Button.AutoSize := True;
      Button.Caption := 'Clear';
      Button.OnClick := tbDummyButton.OnClick;

      // Show codeshare flights toggle button
      tbbCodeshare.Visible := true;
    end
    else
    begin
      { Hide codeshare flights toggle button on everywindow except ARR/DEP}
      tbbCodeshare.Visible := false;
    end;

  end
  else if (ControllerType = FIDSHorizontallyPopulated) then
  begin
    { Static set of buttons for }
    tbExtended.Visible := false;
  end;

  if (SensorsPanelInitialised = false) then
  begin
    { Init sensors }
    fcWindow.InitSensors(panelSensors);
    { Redraw Sensors }
    fcWindow.ImplementSensors(self, imlSensors);
    SensorsPanelInitialised := True;
  end;

  tbClearSearch.Caption := 'Back to "' + ControllerName + '" view';
end;

procedure TfrmWindow.Gates2Click(Sender: TObject);
begin

  if (uCommon.GatesRunning) then
  begin
    ShowWindow(uCommon.GatesHandle, SW_RESTORE);
    Exit;
  end;

  { Set gates window }
  if (assigned(frmWindows[3])) then
  begin
    frmWindows[3].Show;
    frmWindows[3].WindowState := wsNormal;
  end
  else
  begin
    frmWindows[3] := TfrmWindow.Create(nil);
    frmWindows[3].ControllerType := FIDSHorizontallyPopulated;
    frmWindows[3].ControllerID := FIDSGates;
    frmWindows[3].SetController('Gates - ' + fcWindow.GetJobName(),
      fkDepartures, uCommon.GatesFields, uCommon.GatesColumns);
    frmWindows[3].Show;
  end;
end;

function TfrmWindow.NodesCount(vtTree: TVirtualStringTree): int64;
var
  NodeCount: int64;
  tmpLoopNode: PVirtualNode;
begin

  NodeCount := 0;

  try
    tmpLoopNode := vtTree.GetFirst();
    while assigned(tmpLoopNode) do
    begin
      NodeCount := NodeCount + tmpLoopNode.ChildCount;
      tmpLoopNode := vtTree.GetNext(tmpLoopNode);
    end;
  except
    // DO NOTHING; EXPECTED ISSUE
    // beep;
  end;

  Result := NodeCount + VST.RootNodeCount;

end;

procedure TfrmWindow.PopulateHorizontally(SearchField: SmallInt;
  SearchInput: Variant);
var
  CGBBUsed: TStringList; { Checkins Gates Bays Belts Used }
  FixedCol, FlightCGBBValue: String;
  Rows: array of TCGBB;
  row: TCGBB; { Actual Var to CGBB }
  PRow: PCGBB; { Pointer to CGBB }
  Flight: TTreeData;

  I, J: Integer; { Counters for loops used }
  XNode: PVirtualNode;
begin
  VST.Clear;

  I := 0;

  for I := 1 to 99 do
  begin
    VST.Header.Columns.Items[I].Text := 'Flight ' + Inttostr(I);
  end;

  I := 0;
  CGBBUsed := fcWindow.GetListOfCGBBUsed(ControllerID);
  try

    { Set how many rows to Rows array depending on Checkins/Gates/Etc used }
    SetLength(Rows, CGBBUsed.Count);

    { Loop through checkins in use }
    // for I := 0 to CGBBUsed.Count - 1 do
    for FixedCol in CGBBUsed do
    begin

      Rows[I].CGBB := FixedCol;
      { The list within Rows to keep track of Flights | Ex: Rows[x] = (CGBBNo, Array(Flight1, Flight2, Flight3) }
      Rows[I].FlightList := TStringList.Create;
      Rows[I].FlightPathList := TStringList.Create;

      try
        { Loop through Flights in Database }
        for Flight in fcWindow.Table do
        begin
          { Differentiate what data extract from flight info }
          case (ControllerID) of
            FIDSGates:
              FlightCGBBValue := trim(Flight.Gates);
            FIDSBays:
              FlightCGBBValue := trim(Flight.Bays);
            FIDSCheckins:
              FlightCGBBValue := trim(Flight.CheckIns);
            FIDSBelts:
              FlightCGBBValue := trim(Flight.Belts);
          end;

          if not(Pos(LowerCase(FixedCol), LowerCase(FlightCGBBValue)) = 0) then
          // Match Found!
          begin
            // ListBox1.Items.Add(Flight.Flight);
            Rows[I].FlightList.Add(Flight.Flight);
            // Copy the flight number into Rows.FlightList
            Rows[I].FlightPathList.Add(Flight.DBPath);
          end;

        end;

        { Start pushing data to the CELLS on VST }
        { New node }
        XNode := VST.AddChild(nil);

        if VST.AbsoluteIndex(XNode) > -1 then
        begin
          PRow := VST.GetNodeData(XNode);
          PRow^ := Rows[I];
        End;

      except
        on E: Exception do
        begin

        end;
        { Never free here coz we use it later }
        { Rows[I].FlightList.Free; }
      end;

      // Inc(I);
    end;
    { ===========END = THIS CAN BE A DIFFERENT FUNCTION================ }

  finally
    // CGBBUsed.Free;
    { TODO: FREE UP MEMORY }
    { Free up memory used by Rows, No Mem Leak! }
    // for I := 0 to CGBBUsed.Count do
    // Rows[I].FlightList.Free;     	{Found a memory leak somewhere, but here it affects the VST grid}

  end;

  // Commented out as I wanted to see FIDS unavailable message in Gates bays and belts
  // tmrContinuous.Enabled := false;
end;

function TfrmWindow.FindInTable(row: Integer): String;
begin

  if (SearchField = 0) then
    Result := fcWindow.Table[row].Flight;

  if (SearchField = 1) then
    Result := fcWindow.Table[row].ports;
  {
    if (SearchField = 2) then
    Result := fcWindow.Table[row].STDate;

    if (SearchField = 3) then
    Result := fcWindow.Table[row].ETDate;
  }
  if (SearchField = 4) then
    Result := fcWindow.Table[row].Status;

  if (afkWindowKind = fkDepartures) then
    if (SearchField = 5) then
      Result := fcWindow.Table[row].CheckIns
    else if (afkWindowKind = fkArrivals) then
      Result := fcWindow.Table[row].Gates;

  if (afkWindowKind = fkDepartures) then
    if (SearchField = 6) then
      Result := fcWindow.Table[row].Gates
    else if (afkWindowKind = fkArrivals) then
      Result := fcWindow.Table[row].Bays;

  if (afkWindowKind = fkDepartures) then
    if (SearchField = 7) then
      Result := fcWindow.Table[row].Bays
    else if (afkWindowKind = fkArrivals) then
      Result := fcWindow.Table[row].Belts;

  if (SearchField = 8) then
    Result := fcWindow.Table[row].Carrier;

  if (SearchField = 9) then
    Result := fcWindow.Table[row].Terminal;

  if (SearchField = 10) then
    Result := fcWindow.Table[row].Rego;

end;

procedure TfrmWindow.PopulateVertically(SearchField: SmallInt;
  SearchInput: Variant);
var
  TableData, tmpLoopData, PDataRecord: PTreeData;
  XNode, tmpLoopNode: PVirtualNode;
  CSPrimary: PVirtualNode;

  strCol: String;
  I, J, K, SearchResultCount: Integer;

  { search related }
  TranslatedSearch: String;

begin
  {
    dont clear as it tries to redraw the whole table everytime and uselose
    window state, what node focused on, etc. try to match and change content of VST
    Call populategrid in uController so Table will be updated
  }

  // Draw Columns
  I := 0;
  for strCol in ColumnNames do
  begin
    if (LowerCase(ColumnNames[I]) = 'crawling line') OR
      (LowerCase(ColumnNames[I]) = 'nonpublic') then
    else
    begin
      VST.Header.Columns.Items[I].Text := strCol;
      Inc(I);
    end;
  end;

  J := 0;
  for I := 0 to Length(fcWindow.Table) - 1 do
  begin
    { Cleanup LATER! }
    if (SearchField <= -1) then
    begin
      // Assign values to Pointer
      PDataRecord := @fcWindow.Table[I];

      if NodesCount(VST) = Length(fcWindow.Table) - 1 then
      begin
        try
          tmpLoopNode := VST.GetFirst();
          while Assigned(tmpLoopNode) do
          begin
            tmpLoopData := VST.GetNodeData(tmpLoopNode);

            if (uCommon.CompareFlightPath(tmpLoopData, PDataRecord)) then
            begin
              { Update flight data on VST }
              TableData := VST.GetNodeData(tmpLoopNode);
              TableData^ := fcWindow.Table[I];
            end; { END CompareFlightPath }

            tmpLoopNode := VST.GetNext(tmpLoopNode);
          end;
        except
          // DO NOTHING; EXPECTED ISSUE
          // beep;
        end;

      end { RootNodeCount IF Statement }
      else
      begin
        { search function }
        if not(fcWindow.Table[I].CodeShare = 0) then
        begin
          XNode := VST.AddChild(CSPrimary);
          VST.Expanded[CSPrimary] := True;
        end
        else
        begin
          XNode := VST.AddChild(nil);
          CSPrimary := XNode;
        end;

        if VST.AbsoluteIndex(XNode) > -1 then
        begin
          TableData := VST.GetNodeData(XNode);
          TableData^ := fcWindow.Table[I];
        End;

      end; { END OF ELSE ON RootNodeCount IF Statement }

    end;
  end;

  { search function }

  if (ControllerMode = FIDSSearchMode) then
  begin
    // VST.Clear;

    // Presearch to calculate how many results would be returned
    for K := 0 to length(fcWindow.Table) - 1 do
    begin
      TranslatedSearch := FindInTable(K);

      if (Pos(String(SearchInput), TranslatedSearch) > 0) then
      begin
        Inc(SearchResultCount);
      end;

    end; { END OF Forloop }

    K := 0;
    for K := 0 to length(fcWindow.Table) - 1 do
    begin
      TranslatedSearch := FindInTable(K);
      // HACK! CHECK SEARCH WITH FOLLOWING LINE AND REMOVING THIS LINE
      self.Hint := Inttostr(SearchResultCount) + ' | ' + Inttostr(SearchField);
      { apply search criteria }
      { case sensitive }
      if not(SearchCaseSensitive) then
      begin
        SearchInput := LowerCase(SearchInput);
        TranslatedSearch := LowerCase(TranslatedSearch);
      end;

      PDataRecord := @fcWindow.Table[K];
      if (Pos(String(SearchInput), TranslatedSearch) > 0) then
      begin

        if NodesCount(VST) = SearchResultCount then
        begin

          try
            tmpLoopNode := VST.GetFirst();
            while assigned(tmpLoopNode) do
            begin
              tmpLoopData := VST.GetNodeData(tmpLoopNode);

              if (uCommon.CompareFlightPath(tmpLoopData, PDataRecord)) then
              begin
                { Update flight data on VST }
                TableData := VST.GetNodeData(tmpLoopNode);
                TableData^ := fcWindow.Table[K];
              end; { END CompareFlightPath }

              tmpLoopNode := VST.GetNext(tmpLoopNode);
            end;
          except
            // DO NOTHING; EXPECTED ISSUE
            // beep;
          end;

        end { RootNodeCount IF Statement }
        else
        begin
          { search function }
          if not(fcWindow.Table[K].CodeShare = 0) then
          begin
            XNode := VST.AddChild(CSPrimary);
            VST.Expanded[CSPrimary] := True;
          end
          else
          begin
            XNode := VST.AddChild(nil);
            CSPrimary := XNode;
          end;

          if VST.AbsoluteIndex(XNode) > -1 then
          begin
            TableData := VST.GetNodeData(XNode);
            TableData^ := fcWindow.Table[K];
          End;

        end; { END OF ELSE ON RootNodeCount IF Statement }

      end; { end of search if }

      { if no occurences matched in search }
      if (SearchField >= 0) AND (SearchResultCount <= 0) then
      begin
        VST.Clear;
        ControllerMode := FIDSListingMode;
        { disable otherwise you will get messageboxes }
        self.pnlSearchPanel.Visible := false;
        MessageBox(Handle, 'No results returned',
          PWideChar(ControllerName + ' | FIDS'), MB_ICONINFORMATION);

        tbClearSearchClick(tbClearSearch);
        { Enable so normal routine will go on }
        Exit;
      end;

    end; { END OF SEARCH Forloop }
  end; { END OF FIDSSEARCHLISTING IF }

end;

procedure TfrmWindow.SetDetail(Field: aFlightField; Detail: string);
var
  tmpLoopData: PTreeData;
  tmpLoopNode: PVirtualNode;
begin
  // Select previously selected item from GRID
  tmpLoopNode := VST.GetFirst();
  tmpLoopData := VST.GetNodeData(tmpLoopNode);

  if (VST.Selected[tmpLoopNode] = True) then
  begin
    fcWindow.SetDetail(tmpLoopData^.DBPath, Field, Detail);
  end;
  try
    while assigned(tmpLoopNode) do
    begin
      tmpLoopNode := VST.GetNext(tmpLoopNode);
      tmpLoopData := VST.GetNodeData(tmpLoopNode);
      if (VST.Selected[tmpLoopNode] = True) then
      begin
        fcWindow.SetDetail(tmpLoopData^.DBPath, Field, Detail);
      end;
    end;
  except
    // DO NOTHING; EXPECTED ISSUE
    // beep;
  end;
end;

procedure TfrmWindow.AddNewFlight(CodeShare: Boolean; ExistingFlight: apNode);
var
  { Required for Flight Information Update Reoutine }
  AComponent: TComponent;
  EditWndVal, ETDate, ETTime, STDate, STTime, ATDate, ATTime: String;
  NewFlight: cFlight;
begin

  if (CodeShare = false) then
  begin
    { Create/Add the BLANK Flight }
    NewFlight := cFlight.Create(DB, Login.GetUserName());
    // fcWindow.FXml.oDataTree
    NewFlight.Kind := afkWindowKind;
    NewFlight.Clear;
    NewFlight.DbNode := nil;
  end
  else
  begin
    NewFlight.DbNode := nil;
    NewFlight.Clear;
  end;

  { Get Flight Name }
  AComponent := frmEdit.FindComponent('txtFlight');
  if assigned(AComponent) then
    if AComponent is TComponent then
    begin
      // set flight number
      NewFlight.Presentation[ffFlight] := (TEdit(AComponent).Text);
    end
    else
    begin
      ShowMessage('Something went wrong with Flight number');
      Exit;
    end;

  AComponent := frmEdit.FindComponent('dtETDate');
  if (TDateTimePicker(AComponent).Kind = dtkDate) then
  Begin
    ETDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
  End;

  AComponent := frmEdit.FindComponent('dtETTime');
  // if (TEdit(AComponent).Kind = dtkTime) then
  Begin
    // STTime := FIDS_TimeTOStr(TDateTimePicker(AComponent).Time) + '00';
    ETTime := TEdit(AComponent).Text + '00';
    NewFlight.Presentation[ffET] := ETDate + ' ' + ETTime;
  End;
  {
    AComponent := frmEdit.FindComponent('dtETTime');
    if (TDateTimePicker(AComponent).Kind = dtkTime) then
    Begin
    ETTime := FIDS_TimeTOStr(TDateTimePicker(AComponent).Time) + '00';
    NewFlight.Presentation[ffET] := ETDate + ' ' + ETTime;
    End;
  }

  AComponent := frmEdit.FindComponent('dtSTDate');
  if (TDateTimePicker(AComponent).Kind = dtkDate) then
  Begin
    STDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
  End;

  AComponent := frmEdit.FindComponent('dtSTTime');
  // if (TEdit(AComponent).Kind = dtkTime) then
  Begin
    // STTime := FIDS_TimeTOStr(TDateTimePicker(AComponent).Time) + '00';
    STTime := TEdit(AComponent).Text + '00';
    NewFlight.Presentation[ffST] := STDate + ' ' + STTime;
  End;

  AComponent := frmEdit.FindComponent('dtATDate');
  if (TDateTimePicker(AComponent).Kind = dtkDate) then
  Begin
    ATDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
  End;

  AComponent := frmEdit.FindComponent('dtATTime');
  // if (TEdit(AComponent).Kind = dtkTime) then
  Begin
    // STTime := FIDS_TimeTOStr(TDateTimePicker(AComponent).Time) + '00';
    ATTime := TEdit(AComponent).Text + '00';
    NewFlight.Presentation[ffAT] := ATDate + ' ' + ATTime;
  End;

  { Setting Status Field }
  AComponent := frmEdit.FindComponent('cmbStatus');
  EditWndVal := (TComboBox(AComponent).Text);
  if (afkWindowKind = fkArrivals) then
    NewFlight.Presentation[ffAStatus] := EditWndVal;
  if (afkWindowKind = fkDepartures) then
    NewFlight.Presentation[ffDStatus] := EditWndVal;

  { Update flight details here }
  if afkWindowKind = fkArrivals then
  begin
    AComponent := frmEdit.FindComponent('cmbRaceway');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffRaceway] := EditWndVal;

    AComponent := frmEdit.FindComponent('txtOnBlock');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffOnBlock] := EditWndVal;

    AComponent := frmEdit.FindComponent('txtSlotTime');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffSlotTime] := EditWndVal;
  end;

  if afkWindowKind = fkDepartures then
  begin

    AComponent := frmEdit.FindComponent('txtScheduledCheckinCounters');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffScheduledCheckinCounters] := EditWndVal;

    AComponent := frmEdit.FindComponent('txtCheckinOpeningTime');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffCheckinOpeningTime] := EditWndVal;

    AComponent := frmEdit.FindComponent('txtCheckinClosingTime');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffCheckinClosingTime] := EditWndVal;

    AComponent := frmEdit.FindComponent('txtOffBlock');
    EditWndVal := (TComboBox(AComponent).Text);
    NewFlight.Presentation[ffOffBlock] := EditWndVal;

  end;

  { Setting Terminal Field }
  AComponent := frmEdit.FindComponent('cmbTerminals');
  EditWndVal := (TComboBox(AComponent).Text);
  NewFlight.Presentation[ffTerminal] := EditWndVal;

  { Setting Belt Field }
  AComponent := frmEdit.FindComponent('cmbBelts');
  EditWndVal := (TComboBox(AComponent).Text);
  NewFlight.Presentation[ffBelts] := EditWndVal;

  { Setting Checkins Field }
  AComponent := frmEdit.FindComponent('cmbCheckins');
  EditWndVal := (TComboBox(AComponent).Text);
  NewFlight.Presentation[ffCheckins] := EditWndVal;

  { Setting Bays Field }
  AComponent := frmEdit.FindComponent('cmbBays');
  EditWndVal := (TComboBox(AComponent).Text);
  NewFlight.Presentation[ffBays] := EditWndVal;

  { Setting Gates Field }
  AComponent := frmEdit.FindComponent('cmbGates');
  EditWndVal := (TComboBox(AComponent).Text);
  NewFlight.Presentation[ffGates] := EditWndVal;

  { Setting Rego Field }
  AComponent := frmEdit.FindComponent('txtRego');
  EditWndVal := (TEdit(AComponent).Text);
  NewFlight.Presentation[ffRego] := EditWndVal;

  { Setting Ports Field }
  AComponent := frmEdit.FindComponent('txtPorts');
  EditWndVal := (TEdit(AComponent).Text);
  NewFlight.Presentation[ffPorts] := EditWndVal;

  { Following is for checkboxes, they will return a string based value for checked and unchecked items }
  AComponent := frmEdit.FindComponent('chkNonPublic');
  if (TCheckBox(AComponent).Checked) then
  begin
    EditWndVal := '1';
    NewFlight.Presentation[ffNonPublic] := EditWndVal;
  end;

  if Not(TCheckBox(AComponent).Checked) then
  begin
    EditWndVal := '0';
    NewFlight.Presentation[ffNonPublic] := EditWndVal;
  end;

  AComponent := frmEdit.FindComponent('chkCrawling');
  if (TCheckBox(AComponent).Checked) then
  begin
    EditWndVal := '1';
    NewFlight.Presentation[ffCrawling] := EditWndVal;
  end;

  if Not(TCheckBox(AComponent).Checked) then
  begin
    EditWndVal := '0';
    NewFlight.Presentation[ffCrawling] := EditWndVal;
  end;
  /// ///////////////////////
  {
    oFlight.DbPath := oRowList[ row - 1 ];   // connect flight to a particular flight node in DB ready to add code share
    existingFlight := oFlight.DbNode;
    if existingFlight <> nil then  begin
    fStrEntry.Caption := 'Enter Code Share Name eg PJ123 to ' + oFlight.Presentation[ ffFlight ];
    oFlight.DbNode := nil;   // disassociate oflight from any particular flight  to stop field updates
    if fStrEntry.GetStr( 'New code share', '' ) = mrOK then  begin   // get new value from user
    oFlight.Clear;
    oFlight.Presentation[ ffFlight ] := fStrEntry.Str;       // set flight name
    // .....set any other fields of interest....  destinations and checkins are unique to each code share
    oFlight.NewCodeShare( existingFlight );  // and post it
    end;
    end;
    end;
  }
  /// ////////////////////////

  { Last step on creating a new Flight }
  NewFlight.Error := feNone;
  if (CodeShare = false) then
    NewFlight.New // and post it
  else
  begin
    NewFlight.NewCodeShare(ExistingFlight); // new codeshare
  end;

end;

procedure TfrmWindow.UpdateFlightData(ComponentName: String; ObjectType: string;
  ffField: aFlightField);
var
  { Required for Flight Information Update Reoutine }
  AComponent: TComponent;
  EditWndVal, ETDate, ETTime, STDate, STTime, ATDate, ATTime: String;

begin
  {
    Sample datetime update calls
    UpdateFlightData('dtETDate', TDateTimePicker.ClassName, ffETdate);
    UpdateFlightData('dtETTime', TDateTimePicker.ClassName, ffETime);
  }

  if (ffField = ffET) then
  begin
    AComponent := frmEdit.FindComponent('dtETDate');
    if (TDateTimePicker(AComponent).Kind = dtkDate) then
    Begin
      ETDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
    End;

    AComponent := frmEdit.FindComponent('dtETTime');
    // ETTime := FIDS_TimeTOStr(TEdit(AComponent).Text) + '00';
    ETTime := TEdit(AComponent).Text + '00';
    fcWindow.SetDetail(SelectedFlightPath, ffET, ETDate + ' ' + ETTime);
    Exit;
  end;

  if (ffField = ffST) then
  begin
    AComponent := frmEdit.FindComponent('dtSTDate');
    if (TDateTimePicker(AComponent).Kind = dtkDate) then
    Begin
      STDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
    End;

    AComponent := frmEdit.FindComponent('dtSTTime');
    // ETTime := FIDS_TimeTOStr(TEdit(AComponent).Text) + '00';
    STTime := TEdit(AComponent).Text + '00';
    fcWindow.SetDetail(SelectedFlightPath, ffST, STDate + ' ' + STTime);

    Exit;
  end;

  if (ffField = ffAT) then
  begin

    AComponent := frmEdit.FindComponent('dtATDate');
    if (TDateTimePicker(AComponent).Kind = dtkDate) then
    Begin
      ATDate := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
    End;

    AComponent := frmEdit.FindComponent('dtATTime');
    ATTime := TEdit(AComponent).Text + '00';
    fcWindow.SetDetail(SelectedFlightPath, ffAT, ATDate + ' ' + ATTime);

    Exit;
  end;

  AComponent := frmEdit.FindComponent(ComponentName);
  if assigned(AComponent) then
    if AComponent is TComponent then
    begin
      { Continue with component types I use in Edit window }

      { Text fields }
      if ObjectType = TEdit.ClassName then
        EditWndVal := (TEdit(AComponent).Text);

      { Combobox fields }
      if ObjectType = TComboBox.ClassName then
        EditWndVal := (TComboBox(AComponent).Text);

      { Same Object type will be used for date and time fields so you have to consider it's Kind }
      if ObjectType = TDateTimePicker.ClassName then
        if (TDateTimePicker(AComponent).Kind = dtkDate) then
        begin
          EditWndVal := FIDS_DtTOStr(TDateTimePicker(AComponent).Date);
        end;

      { Same Object type will be used for date and time fields so you have to consider it's Kind }
      if ObjectType = TDateTimePicker.ClassName then
        if (TDateTimePicker(AComponent).Kind = dtkTime) then
        Begin
          EditWndVal := FIDS_TimeTOStr(TDateTimePicker(AComponent).Time) + '00';
        End;

      { Following is for checkboxes, they will return a string based value for checked and unchecked items }
      if ObjectType = TCheckBox.ClassName then
        if (TCheckBox(AComponent).Checked) then
        begin
          EditWndVal := '1';
        end;

      if ObjectType = TCheckBox.ClassName then
        if Not(TCheckBox(AComponent).Checked) then
        begin
          EditWndVal := '0';
        end;

    end;

  if not(length(trim(EditWndVal)) <= 0) then
  begin
    { Checks for non-empty fields }
    { Also check for updated values against exisitng flight information }     { To be implemented }
    fcWindow.SetDetail(SelectedFlightPath, ffField, EditWndVal);
  end;

end;

procedure TfrmWindow.tbbAddClick(Sender: TObject);
var
  { ============================== }
  VSTDataNode: PVirtualNode;
  TDataItem: TTreeData;
  PDataItem: PTreeData;

  PCGBBItem: PCGBB;
  { Required for Flight Information Update Reoutine }
  UpdateValue: String;

  I: Int16;
  // TT Specific
  NodeIndex: Integer;

  { Required for Flight Information Update Reoutine }
  AComponent: TComponent;
  ETDate, ETTime: String;

  Flight: cFlight;
begin

  if (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) then
  begin
    { Timetable specific edit }
    {
      if VST.GetFirstSelected().Index < Cardinal(fcWindow.oTTRulesList.Count) then

      begin
      NodeIndex := VST.GetFirstSelected().Index;
      oRule.DbNode := fcWindow.oRules[NodeIndex];
      // oRule.DbPath := fcWindow.Table[NodeIndex].DBPath;
      oRule.oTemplate.DbNode := oRule.oTemplate.DbNode;

      // ShowMessage(oRule.DbNode.Content);
      // oRule.DbNode := fcWindow.oTTRulesList[ NodeIndex ];
      fRuleEdit.TTRule := oRule; // link oRule to vst row;
      fRuleEdit.ShowModal;

      // retrieve data and redraw grid
      fcWindow.PopulateTTGrid;
      PopulateGrid;

      end;
    }
    Exit();
  end;

  // ShowMessage(inttostr(VST.GetFirstSelected().Index));
  {
    if (self.ControllerType = FIDSHorizontallyPopulated) then
    begin
    if (ErrorOccuredVSTMouseUP) then
    exit;

    if not Assigned(VST.FocusedNode) then
    exit;
    if (VST.FocusedColumn <= 0) then
    exit;

    for I := 0 to length(fcWindow.Table) - 1 do
    begin

    if fcWindow.Table[I].DBPath = SelectedFlightPath then
    begin
    PDataItem := @fcWindow.Table[I];
    SelectedFlightPath := PDataItem.DBPath;
    end;

    // if (I >= length(fcWindow.Table)-1) then
    // exit;

    end;

    frmEdit.SetData(PDataItem);
    FlightFound := True;
    end;

  }

  if (self.ControllerType = FIDSVerticallyPopulated) then
  begin
    TDataItem.Flight := '';
    TDataItem.Ports := '';
    TDataItem.Status := '';
    TDataItem.CheckIns := '';
    TDataItem.STDate := Now;
    TDataItem.STime := Now;
    TDataItem.ETDate := Now;
    TDataItem.ETime := Now;
    TDataItem.ATDate := Now;
    TDataItem.ATime := Now;

    TDataItem.Gates := '';
    TDataItem.Bays := '';
    TDataItem.Belts := '';
    TDataItem.Carrier := '';
    TDataItem.Terminal := '';
    TDataItem.Rego := '';
    TDataItem.DBPath := '';

    TDataItem.Raceway := '';
    TDataItem.Slottime := '';
    TDataItem.OnBlock := '';
    TDataItem.OffBlock := '';
    TDataItem.ScheduledCheckinCounters := '';
    TDataItem.CheckinOpeningTime := '';
    TDataItem.CheckinClosingTime := '';
    TDataItem.RelatedFlight := '';

    PDataItem := @TDataItem;
    // frmEdit.SetData(PDataItem);

    if (self.ControllerID = FIDSArrivals) OR (self.ControllerID = FIDSDepartures)
    then
      frmEditAnD.SetData(PDataItem)
    else
      frmEdit.SetData(PDataItem);
  end;

  { Common Function for both Vertically and Horizontally populated data }

  if (ControllerID = FIDSArrivals) then
  begin
    frmEditAnD.SetFields(uCommon.ArrivalFields, uCommon.ArrivalColumns);
  end;

  if (ControllerID = FIDSDepartures) then
  begin
    frmEditAnD.SetFields(uCommon.DeparturesFields, uCommon.DeparturesColumns);
  end;

  if (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) OR
    (ControllerID = FIDSCheckins) OR (ControllerID = FIDSGates) OR
    (ControllerID = FIDSBays) OR (ControllerID = FIDSBelts) then
    frmEdit.SetFields(FlightFields, ColumnNames);

  if (self.ControllerID = FIDSArrivals) OR (self.ControllerID = FIDSDepartures)
  then
    Application.CreateForm(TfrmEditAnD, frmEdit)
  else
    Application.CreateForm(TfrmEdit, frmEdit);

  { Load dropdown box values }
  frmEdit.lstStatuses.Add('');
  frmEdit.lstStatuses.AddStrings(fcWindow.GetStatuses);
  frmEdit.lstGates.AddStrings(fcWindow.GetGates);
  frmEdit.lstBays.Add('');
  frmEdit.lstBays.AddStrings(fcWindow.GetBays);
  frmEdit.lstBelts.AddStrings(fcWindow.GetBelts);
  frmEdit.lstCarrierTypes.Add('');
  frmEdit.lstCarrierTypes.AddStrings(fcWindow.GetCarrierTypes);
  frmEdit.lstCheckin.AddStrings(fcWindow.GetCheckinCounters);
  frmEdit.lstAircrafts.AddStrings(fcWindow.GetAircrafts);
  frmEdit.lstTerminals.Add('');
  frmEdit.lstTerminals.AddStrings(fcWindow.GetTerminals);

  Flight := cFlight.Create(DB, Login.GetUserName);
  Flight.Kind := afkWindowKind;
  Flight.DBPath := SelectedFlightPath;

  // Set the icon
  frmEdit.icon := icon;
  if frmEdit.ShowModal = mrOk then
  begin
    // ShowMessage(Inttostr(Flight.CodeShare));
    if (Flight.CodeShare >= 1) then
      AddNewFlight(True, Flight.DbNode)
    else
      AddNewFlight(false, Flight.DbNode);

  end;

end;

procedure TfrmWindow.tbbCodeshareClick(Sender: TObject);
var 
  I, MidLeft, MidTop : Int;
  NextNode, LastViewNode : PVirtualNode;
 
begin
  DisableNodeCollapsing := false;
  NextNode := VST.GetFirst();
  
  MidLeft := Left + (Width div 2);
  MidTop := Top - VST.Top + (Height div 2);
 
  LastViewNode := VST.GetNodeAt(Point(MidLeft, MidTop));

  // If focused on a child get the parent node so when collapsed 
  // it will focus on the parent leaving the node in collapsed state.
  if not (LastViewNode.Parent = VST.GetFirst().Parent) then
  begin
    LastViewNode := LastViewNode.Parent;
  end;

  VST.Hide;
  for I := 0 to NodesCount(VST) -1 do
  begin
    VST.Expanded[NextNode] := tbbCodeshare.Down;
    NextNode := VST.GetNextSibling(NextNode);
  end;
    
  VST.Show;
  VST.ScrollIntoView(LastViewNode, True);
  DisableNodeCollapsing := true;

end;

procedure TfrmWindow.tbbDeleteClick(Sender: TObject);
var
  ChosenFlight: cFlight;
  PFlightV: PTreeData;
  PFlightH: PCGBB;
  Flight: string;
begin

  if Not Assigned(VST.FocusedNode) then
  begin
    Exit;
  end;

  if (ControllerType = FIDSVerticallyPopulated) then
  begin
    PFlightV := VST.GetNodeData(VST.FocusedNode);
    Flight := PFlightV.Flight;
  end;

  if (ControllerType = FIDSHorizontallyPopulated) then
  begin
    PFlightH := VST.GetNodeData(VST.FocusedNode);
    Flight := PFlightH.FlightList[VST.FocusedColumn - 1];
  end;

  if MessageDlg('Please confirm deletion of flight "' + Flight + '" ?',
    mtconfirmation, [mbNo, mbYes], 0, mbNo) = mrYes then
  begin
    ChosenFlight := cFlight.Create(DB, Login.GetUserName);
    ChosenFlight.DBPath := SelectedFlightPath;
    // connect flight to a particular flight node in DB
    if ChosenFlight.DbNode <> nil then
    begin
      if (PFlightV.CodeShare > 0) then
        ChosenFlight.DeleteSub
      else
        ChosenFlight.Delete;
    end;

    VST.Clear;
    PopulateGrid;
  end;

end;

procedure TfrmWindow.tbbGanttClick(Sender: TObject);
begin
    VST.Clear;
    // retrieve data and redraw grid
    PopulateGrid;
    VST.Repaint;
end;

procedure TfrmWindow.tbbHomeClick(Sender: TObject);
begin
  frmMain.Show();

  if (frmMain.WindowState = wsMinimized) then
  begin
    frmMain.WindowState := wsNormal;
  end;

end;

procedure TfrmWindow.tbbModifyClick(Sender: TObject);
var

  VSTDataNode: PVirtualNode;
  PDataItem: PTreeData;
  FlightFound: Boolean;

  PCGBBItem: PCGBB;
  { Required for Flight Information Update Reoutine }
  UpdateValue: String;

  I: Int16;
  // TT Specific
  NodeIndex: Integer;

  { Required for Flight Information Update Reoutine }
  AComponent: TComponent;
  ETDate, ETTime: String;

  thread1, id1: Integer;
begin
  FlightFound := false;

  if (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) then
  begin
    { Timetable specific edit }
    if VST.GetFirstSelected().Index <= Cardinal(Length(fcWindow.Table)) then
    begin

      // Initialise edit window
      if not (Assigned(fRuleEdit)) then
      begin
        Application.CreateForm(TfRuleEdit, fRuleEdit);
      end;

      NodeIndex := VST.GetFirstSelected().Index;
      oRule.DbNode := fcWindow.oRules[NodeIndex];
      oRule.oTemplate.DbNode := oRule.oTemplate.DbNode;

      // oRule.DbNode := fcWindow.oTTRulesList[ NodeIndex ];
      fRuleEdit.TTRule := oRule; // link oRule to vst row;
      fRuleEdit.ShowModal;
    end;

    Exit();
  end;

  if (self.ControllerType = FIDSHorizontallyPopulated) then
  begin
    if (ErrorOccuredVSTMouseUP) then
      Exit;

    if not assigned(VST.FocusedNode) then
      Exit;

    if (VST.FocusedColumn <= 0) then
      Exit;

    for I := 0 to length(fcWindow.Table) - 1 do
    begin
      if fcWindow.Table[I].DBPath = SelectedFlightPath then
      begin
        PDataItem := @fcWindow.Table[I];
        SelectedFlightPath := PDataItem.DBPath;
      end;
    end;

    frmEditAnD.SetData(PDataItem);
    FlightFound := True;
  end;

  if (self.ControllerType = FIDSVerticallyPopulated) then
  begin
    // Select previously selected item from GRID
    VSTDataNode := VST.GetFirst();
    PDataItem := VST.GetNodeData(VSTDataNode);
    if (PDataItem^.DBPath = SelectedFlightPath) then
    begin

      if (self.ControllerID = FIDSArrivals) OR
        (self.ControllerID = FIDSDepartures) then
        frmEditAnD.SetData(VST.GetNodeData(VSTDataNode))
      else
        frmEdit.SetData(VST.GetNodeData(VSTDataNode));

      FlightFound := True;
    end;

    try
      while assigned(VSTDataNode) do
      begin
        VSTDataNode := VST.GetNext(VSTDataNode);
        PDataItem := VST.GetNodeData(VSTDataNode);
        if (PDataItem^.DBPath = SelectedFlightPath) then
        begin

          if (self.ControllerID = FIDSArrivals) OR
            (self.ControllerID = FIDSDepartures) then
            frmEditAnD.SetData(VST.GetNodeData(VSTDataNode))
          else
            frmEdit.SetData(VST.GetNodeData(VSTDataNode));

          FlightFound := True;
        end;
      end;
    except
      // DO NOTHING
      // beep;
    end;
  end;

  { Common Function for both Vertically and Horizontally populated data }
  if FlightFound then
  begin

    if (ControllerID = FIDSArrivals) then
    begin
      frmEditAnD.SetFields(uCommon.ArrivalFields, uCommon.ArrivalColumns);
    end;

    if (ControllerID = FIDSDepartures) then
    begin
      frmEditAnD.SetFields(uCommon.DeparturesFields, uCommon.DeparturesColumns);
    end;

    if (ControllerID = FIDSCheckins) OR (ControllerID = FIDSGates) OR
      (ControllerID = FIDSBays) OR (ControllerID = FIDSBelts) then
    begin
      frmEditAnD.SetFields(FlightFields, ColumnNames);
    end;

    if (self.ControllerID = FIDSArrivals) OR (self.ControllerID = FIDSDepartures)
    then
    begin
      Application.CreateForm(TfrmEditAnD, frmEdit)
    end
    else
    begin
      Application.CreateForm(TfrmEditAnD, frmEdit);
    end;

    { Load dropdown box values }
    frmEdit.lstStatuses.Add('');
    frmEdit.lstStatuses.AddStrings(fcWindow.GetStatuses);
    frmEdit.lstGates.AddStrings(fcWindow.GetGates);
    frmEdit.lstBays.Add('');
    frmEdit.lstBays.AddStrings(fcWindow.GetBays);
    frmEdit.lstBelts.AddStrings(fcWindow.GetBelts);
    frmEdit.lstCarrierTypes.Add('');
    frmEdit.lstCarrierTypes.AddStrings(fcWindow.GetCarrierTypes);
    frmEdit.lstCheckin.AddStrings(fcWindow.GetCheckinCounters);
    frmEdit.lstAircrafts.AddStrings(fcWindow.GetAircrafts);
    frmEdit.lstTerminals.Add('');
    frmEdit.lstTerminals.AddStrings(fcWindow.GetTerminals);

    // Set the icon
    frmEdit.icon := icon;

    case ControllerID of
      FIDSArrivals:
        frmEdit.Caption := 'Arrival details ';
      FIDSDepartures:
        frmEdit.Caption := 'Departure details ';
      FIDSCheckins:
        frmEdit.Caption := 'Check-ins details ';
      FIDSGates:
        frmEdit.Caption := 'Gates details ';
      FIDSBays:
        frmEdit.Caption := 'Bays details ';
      FIDSBelts:
        frmEdit.Caption := 'Belts details ';
    end;

    if frmEdit.ShowModal = mrOk then
    begin

      { Update flight details here }
      if afkWindowKind = fkArrivals then
      begin
        UpdateFlightData('cmbStatus', TComboBox.ClassName, ffAStatus);
        UpdateFlightData('cmbRaceway', TComboBox.ClassName, ffRaceway);
        UpdateFlightData('txtOnBlock', TComboBox.ClassName, ffOnBlock);
        UpdateFlightData('txtSlotTime', TEdit.ClassName, ffSlotTime);
      end;

      if afkWindowKind = fkDepartures then
      begin
        UpdateFlightData('cmbStatus', TComboBox.ClassName, ffDStatus);
        UpdateFlightData('txtScheduledCheckinCounters', TEdit.ClassName,
          ffScheduledCheckinCounters);
        UpdateFlightData('txtCheckinOpeningTime', TEdit.ClassName,
          ffCheckinOpeningTime);
        UpdateFlightData('txtCheckinClosingTime', TEdit.ClassName,
          ffCheckinClosingTime);
        UpdateFlightData('txtOffBlock', TComboBox.ClassName, ffOffBlock);
      end;

      UpdateFlightData('cmbGates', TComboBox.ClassName, ffGates);
      UpdateFlightData('cmbBays', TComboBox.ClassName, ffBays);
      UpdateFlightData('cmbBelts', TComboBox.ClassName, ffBelts);
      UpdateFlightData('cmbCheckins', TComboBox.ClassName, ffCheckins);
      UpdateFlightData('cmbTerminals', TComboBox.ClassName, ffTerminal);
      UpdateFlightData('cmbCarrierTypes', TComboBox.ClassName, ffCarrier);
      UpdateFlightData('txtRego', TEdit.ClassName, ffRego);
      UpdateFlightData('txtPorts', TEdit.ClassName, ffPorts);
      UpdateFlightData('dtETDate', TDateTimePicker.ClassName, ffET);
      UpdateFlightData('dtSTDate', TDateTimePicker.ClassName, ffST);
      UpdateFlightData('dtATDate', TDateTimePicker.ClassName, ffAT);
      UpdateFlightData('cmbComment', TComboBox.ClassName, ffComment);
      UpdateFlightData('chkNonPublic', TCheckBox.ClassName, ffNonPublic);
      UpdateFlightData('chkCrawling', TCheckBox.ClassName, ffCrawling);

      UpdateFlightData('cmbAircrafts', TComboBox.ClassName, ffAirCraft);
      UpdateFlightData('cmbRelatedFlight', TComboBox.ClassName,
        ffRelatedFlight);
      {
        UpdateFlightData('dtETDate', TDateTimePicker.ClassName, ffET);
        UpdateFlightData('dtETTime', TDateTimePicker.ClassName, ffETime);


        UpdateFlightData('txtRego', TEdit.ClassName, ffRego);
        UpdateFlightData('txtPorts', TEdit.ClassName, ffPorts);

        UpdateFlightData('txtRego', TEdit.ClassName, ffRego);
        UpdateFlightData('txtPorts', TEdit.ClassName, ffPorts);
      }

    end;

  end
  else
  begin
    ShowMessage('No flight chosen');
  end;
end;

procedure TfrmWindow.tbClearSearchClick(Sender: TObject);
begin
  SearchField := -1;
  SearchString := '';

  // Reset window mode
  ControllerMode := FIDSListingMode;

  PopulateGrid();

  self.tbClearSearch.Visible := false;
  self.pnlSearchPanel.Visible := false;
end;

procedure TfrmWindow.tbDummyButtonClick(Sender: TObject);
begin

  { Set status from caption of the button }
  if (afkWindowKind = fkArrivals) then
  begin
    if (LowerCase(TToolButton(Sender).Caption) = 'clear') then
    begin
      self.SetDetail(ffAStatus, '')
    end
    else
    begin
      self.SetDetail(ffAStatus, TToolButton(Sender).Caption);
    end;
  end
  else if (afkWindowKind = fkDepartures) then
  begin
    if (LowerCase(TToolButton(Sender).Caption) = 'clear') then
    begin
      self.SetDetail(ffDStatus, '')
    end
    else
    begin
      self.SetDetail(ffDStatus, TToolButton(Sender).Caption);
    end;
  end;

end;

function TfrmWindow.ExpandNodes(): Integer;
var
  TableData, tmpLoopData, PDataRecord: PTreeData;
  XNode, tmpLoopNode: PVirtualNode;
  CSPrimary: PVirtualNode;

begin
  try
    tmpLoopNode := VST.GetFirst();
    while assigned(tmpLoopNode) do
    begin
      VST.Expanded[tmpLoopNode] := True;
      tmpLoopNode := VST.GetNext(tmpLoopNode);
    end;
  except
  end;

end;

procedure TfrmWindow.VSTAfterItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  NodeData: PTreeData;
  I: Int8;
  Paths: TStringList;
  ProcessingFlight: cFlight;
  RowColor: Cardinal;
begin

  {                       }
  { Color Strips Function }
  {                       }

  { Default row color }
  RowColor := RGB(255, 255, 255);

  if (ControllerType = FIDSVerticallyPopulated) then
  begin
    NodeData := VST.GetNodeData(Node);

    ProcessingFlight := cFlight.Create(DB, DB.id);
    ProcessingFlight.DBPath := NodeData.DBPath;

    { Flight must not be nil or a child of a parent }
    if (ProcessingFlight.DbNode <> nil) AND (ProcessingFlight.CodeShare = 0) then
    begin

      if Odd(Node.Index) then
      begin
        RowColor := RGB(255, 255, 185);

        { Keep track of the last node highlighted }
        ParentNodes[VST.AbsoluteIndex(Node)] := RowColor;
      end
      else
      begin
        ParentNodes[VST.AbsoluteIndex(Node)] := RowColor;
      end;

    end

    { If processing node is a child, replicate it's parent color }
    else if (ProcessingFlight.CodeShare > 0)  then
    begin
      RowColor := ParentNodes[VST.AbsoluteIndex(Node.Parent)];

//      Following condition will enable zebra strips for child nodes.  
//      if (Node.Index >= 1) then
//      begin
//        if Odd(Node.Index) then
//        begin
//          if (RowColor = RGB(255, 255, 185)) then
//            RowColor := RGB(255, 255, 255)
//          else
//            RowColor := RGB(255, 255, 185);
//        end;
//      end;
    
    end;

    { Set row color }
    TargetCanvas.Brush.Color := RowColor;
    TargetCanvas.FillRect(ItemRect);

    {                       }
    { Color Strips Function }
    {                       }

    for I := 0 to VST.Header.Columns.Count - 1 do
    begin
      if LowerCase(VST.Header.Columns.Items[I].Text) = 'status' then
      begin
        ItemRect.Left := VST.Header.Columns.Items[I].Left;
        ItemRect.Right := VST.Header.Columns.Items[I + 1].Left;
      end;
    end;

    // Draw status colors on "Status" column
    fcWindow.SetStatusRect(NodeData.Status, ItemRect, TargetCanvas);
  end;
end;


procedure TfrmWindow.VSTAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  NodeData: PTreeData;
  I: Int8;
begin
  NodeData := VST.GetNodeData(Node);

  if (Sender as TBaseVirtualTree).Selected[Node] then
  begin

    for I := 0 to VST.Header.Columns.Count - 1 do
    begin
      if UpperCase(VST.Header.Columns.Items[I].Text) = 'STATUS' then
      begin
        ItemRect.Left := VST.Header.Columns.Items[I].Left;
        ItemRect.Right := VST.Header.Columns.Items[I + 1].Left;

        // Draw status colors on "Status" column
        fcWindow.SetStatusRect(NodeData.Status, ItemRect, TargetCanvas);

        // Redraw status on top of ItemRect
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.TextOut(ItemRect.Left + 10, ItemRect.Top + 2, NodeData.Status);

      end;
    end;

    // Draw focus rect for a row
    TargetCanvas.DrawFocusRect(ItemRect);
  end;

end;

procedure TfrmWindow.VSTCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
begin
  if (DisableNodeCollapsing) then
  begin
    Allowed := False;
  end;

end;


procedure TfrmWindow.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  VData1: PTreeData;
  VData2: PTreeData;
  Compare1: String;
  Compare2: String;
begin

  // Exit when the Column is -1
  if (Column < 0) then
  begin
    Exit;
  end;

  // retrive VData from VT and load them to Flight VVData Structure
  VData1 := VST.GetNodeData(Node1);
  VData2 := VST.GetNodeData(Node2);
ShowMessage(VData1.Flight);
  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'flight') then
  begin
    Compare1 := VData1.Flight;
    Compare2 := VData2.Flight;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'ports') then
  begin
    Compare1 := VData1.ports;
    Compare2 := VData2.ports;
  end;
  {
    if (LowerCase( vst.Header.Columns.Items[Column].Text ) = 'st') then
    Compare1 := VData1.ScheduledTime; Compare2 := VData2.ScheduledTime;

    if (LowerCase( vst.Header.Columns.Items[Column].Text ) = 'et') then
    Compare1 := VData1.EstimatedTime; Compare2 := VData2.EstimatedTime;
  }
  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'status') then
  begin
    Compare1 := VData1.Status;
    Compare2 := VData2.Status;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'related flight') then
  begin
    Compare1 := VData1.RelatedFlight;
    Compare2 := VData2.RelatedFlight;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'check-in') then
  begin
    Compare1 := VData1.CheckIns;
    Compare2 := VData2.CheckIns;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'gate') then
  begin
    Compare1 := VData1.Gates;
    Compare2 := VData2.Gates;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'bay') then
  begin
    Compare1 := VData1.Bays;
    Compare2 := VData2.Bays;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'belts') then
  begin
    Compare1 := VData1.Belts;
    Compare2 := VData2.Belts;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'carrier') then
  begin
    Compare1 := VData1.Carrier;
    Compare2 := VData2.Carrier;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'terminal') then
  begin
    Compare1 := VData1.Terminal;
    Compare2 := VData2.Terminal;
  end;

  if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'rego') then
  begin
    Compare1 := VData1.Rego;
    Compare2 := VData2.Rego;
  end;

  // Improve below code so TimeStamp, Integers and Characters can be sorted
  if (not Assigned(VData1)) or (not Assigned(VData2)) then
    Result := 0
  else
  begin

    if (IsStrANumber(Compare1) AND IsStrANumber(Compare2)) then
    begin
      if (StrToInt(Compare1) < StrToInt(Compare2)) then
        Result := -1;
      if (StrToInt(Compare1) > StrToInt(Compare2)) then
        Result := 1;
      if (StrToInt(Compare1) = StrToInt(Compare2)) then
        Result := 0;
    end
    else
    begin
      Result := CompareStr(Compare1, Compare2);
    end;

  end;
end;

procedure TfrmWindow.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TfrmWindow.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  VData: PTreeData;
  HData: PCGBB;
  Flight: String;

  Counter: string;
  I: Integer;
begin

  CellText := '';

  // If Column is -1, Ignore
  if (Column < 0) then
  begin
    Exit;
  end;

  if (ControllerID = FIDSCheckins) OR (ControllerID = FIDSGates) OR
    (ControllerID = FIDSBays) OR (ControllerID = FIDSBelts) then
  begin
    if (ControllerType = FIDSHorizontallyPopulated) then
    begin
      HData := VST.GetNodeData(Node);
      I := 1;

      if (Column = 0) then
        CellText := HData^.CGBB;

      if (Column >= 1) then
        if (HData^.FlightList.Count >= 1) then
          for Flight In HData^.FlightList Do
          begin
            if (Column = I) then
              CellText := Flight;

            Inc(I);
          end;

    end;
  end;

  if ((ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures)) then
  begin
    if (ControllerType = FIDSVerticallyPopulated) then
    begin

      { Careful! this directly relates to the ColumnFields array defined in
        uCommon.pas, which is sent from the main window to name column names }
      VData := VST.GetNodeData(Node);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'flight') then
        CellText := VData^.Flight;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'ports') then
        CellText := uCommon.TrimPorts(VData^.ports);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'sta') OR
        (LowerCase(VST.Header.Columns.Items[Column].Text) = 'std') then
        CellText := uCommon.TimeTo12Hour(VData^.STime);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'sta[date]') OR
        (LowerCase(VST.Header.Columns.Items[Column].Text) = 'std[date]') then
        CellText := DateToStr(VData^.STDate);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'eta') OR
        (LowerCase(VST.Header.Columns.Items[Column].Text) = 'etd') then
        CellText := uCommon.TimeTo12Hour(VData^.ETime);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'eta[date]') OR
        (LowerCase(VST.Header.Columns.Items[Column].Text) = 'etd[date]') then
        CellText := DateToStr(VData^.ETDate);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'status') then
        CellText := VData^.Status;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'related flight') then
        CellText := VData^.RelatedFlight;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'check-in') then
        CellText := VData^.CheckIns;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'gate') then
        CellText := VData^.Gates;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'bay') then
        CellText := VData^.Bays;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'belts') then
        CellText := VData^.Belts;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'carrier') then
        CellText := VData^.Carrier;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'terminal') then
        CellText := VData^.Terminal;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'rego') then
        CellText := VData^.Rego;

    end;
  end;


  if (ControllerID = FIDSDepartures) OR (ControllerID = FIDSArrivals) OR
    (ControllerID = FIDSTArrivals) OR (ControllerID = FIDSTDepartures) then
  begin
    if (ControllerType = FIDSVerticallyPopulated) then
    begin

      { Careful! this directly relates to the ColumnFields array defined in
        uCommon.pas, which is sent from the main window to name column names }
      VData := VST.GetNodeData(Node);

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'flight') then
        CellText := VData^.Flight;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'ports') then
        CellText := uCommon.TrimPorts(VData^.ports);

      if (isDateEmpty(VData^.STDate) = false) then
      begin
        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'sta[date]') then
          CellText := DateToStr(VData^.STDate);
        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'sta') then
          CellText := uCommon.TimeTo12Hour(VData^.STime);

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'std[date]') then
          CellText := DateToStr(VData^.STDate);
        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'std') then
          CellText := uCommon.TimeTo12Hour(VData^.STime);
      end;

      if (isDateEmpty(VData^.ETDate) = false) then
      begin
        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'eta') then
        begin
          if not(VData^.STime = VData^.ETime) then
            CellText := uCommon.TimeTo12Hour(VData^.ETime);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'eta[date]') then
        begin
          if not(VData^.STDate = VData^.ETDate) then
            CellText := DateToStr(VData^.ETDate);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'etd') then
        begin
          if not(VData^.STime = VData^.ETime) then
            CellText := uCommon.TimeTo12Hour(VData^.ETime);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'etd[date]') then
        begin
          if not(VData^.STDate = VData^.ETDate) then
            CellText := DateToStr(VData^.ETDate);
        end;
      end;

      if (isDateEmpty(VData^.ATDate) = false) then
      begin
        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'ata') then
        begin
          if not(VData^.STime = VData^.ATime) then
            CellText := uCommon.TimeTo12Hour(VData^.ATime);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'ata[date]') then
        begin
          if not(VData^.STDate = VData^.ATDate) then
            CellText := DateToStr(VData^.ATDate);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'atd') then
        begin
          if not(VData^.STime = VData^.ATime) then
            CellText := uCommon.TimeTo12Hour(VData^.ATime);
        end;

        if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'atd[date]') then
        begin
          if not(VData^.STDate = VData^.ATDate) then
            CellText := DateToStr(VData^.ATDate);
        end;
      end;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'status') then
        CellText := VData^.Status;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'related flight') then
        CellText := VData^.RelatedFlight;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'checkin') then
        CellText := VData^.CheckIns;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'gate') then
        CellText := VData^.Gates;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'bay') then
        CellText := VData^.Bays;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'belts') then
        CellText := VData^.Belts;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'carrier') then
        CellText := VData^.Carrier;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'terminal') then
        CellText := VData^.Terminal;

      if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'rego') then
        CellText := VData^.Rego;

    end;
  end;

end;

procedure TfrmWindow.VSTHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  VST.Header.SortColumn := HitInfo.Column;

  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;

  VST.SortTree(HitInfo.Column, Sender.SortDirection, True);
  VST.SortTree(HitInfo.Column, Sender.SortDirection, True);
  VST.SortTree(HitInfo.Column, Sender.SortDirection, True);
end;

procedure TfrmWindow.VSTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tmpNode: PVirtualNode;
  tmpData: PTreeData;

  PCGBBItem: PCGBB;
  PCGBBItemMouseLoc: PCGBB;
  I: Int16;
begin
  tmpNode := VST.GetNodeAt(X, Y);

  if (self.ControllerType = FIDSHorizontallyPopulated) then
  begin
    if (VST.FocusedColumn <= 0) then
      Exit;

    if not(assigned(VST.FocusedNode)) then
      Exit;

    if not(assigned(tmpNode)) then
      Exit;

    PCGBBItem := VST.GetNodeData(VST.FocusedNode);
    PCGBBItemMouseLoc := VST.GetNodeData(tmpNode);
    // Flight path = PCGBBItem.FlightPathList[VST.FocusedColumn -1];

    if (PCGBBItem.CGBB = PCGBBItemMouseLoc.CGBB) then
    begin
      try
        for I := 0 to length(fcWindow.Table) - 1 do
        begin
          if fcWindow.Table[I].DBPath = PCGBBItem.FlightPathList
            [VST.FocusedColumn - 1] then
          begin
            tmpData := @fcWindow.Table[I];
            SelectedFlightPath := tmpData.DBPath;

            ErrorOccuredVSTMouseUP := false;
          end;
        end;
      except
        ErrorOccuredVSTMouseUP := True;
        Beep;
      end;
    end;

  end;

  if (self.ControllerType = FIDSVerticallyPopulated) then
  begin

    if (assigned(tmpNode)) then
    begin
      tmpData := VST.GetNodeData(tmpNode);
      SelectedFlightPath := tmpData^.DBPath;
    end;
  end;


end;

procedure TfrmWindow.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
Var
  Data: PTreeData;
begin
  // This section is important when enabling on the spot editing
  // NOT YET IMPLEMENTED
  Data := VST.GetNodeData(Node);
  Case Column of
    0:
      Data^.Flight := NewText;
    1:
      Data^.ports := NewText;
  End;
end;

procedure TfrmWindow.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
Var
  Data: PTreeData;
begin
  Data := VST.GetNodeData(Node);

  if (ControllerType = FIDSVerticallyPopulated) then
  begin
    // Change ST Date color if ET or AT are different
    if (LowerCase(VST.Header.Columns.Items[Column].Text) = 'std[date]') OR
      (LowerCase(VST.Header.Columns.Items[Column].Text) = 'sta[date]') then
    begin
      if not(Data^.STDate = Data^.ATDate) AND (Data^.STDate = Data^.ETDate) then
      begin
        TargetCanvas.Font.Color := clRed;
        TargetCanvas.Font.Style := [fsBold];
      end;
    end;

    if LowerCase(VST.Header.Columns.Items[Column].Text) = 'status' then
    begin
      if LowerCase(Data^.Status) = 'closed' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'diverted' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'cancelled' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'boarding' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'final call' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'closed' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'departed' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

      if LowerCase(Data^.Status) = 'landed' then
      begin
        TargetCanvas.Font.Color := clWhite;
        TargetCanvas.Font.Style := [fsBold];
      end;

    end;

  end; // controller type if


end;

procedure TfrmWindow.ConnectionEvent(event: aConnectionEvent; param: string);
begin
  case event of
    ceConnected:
      begin

      end;
    ceDbReady:
      ;
    ceLogin:
      ;
    ceEdit:
      begin
        Poller.OnTimeOut(1, procedure()
          begin  // aOnTimeOutProc
            // retrieve data and redraw grid
            PopulateGrid;
            VST.Repaint;
          end
        );
      end;
    ceDisconnected:
      ;
    ceShutdown:
      Close;
  end;
end;

end.
