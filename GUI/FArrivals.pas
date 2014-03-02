unit FArrivals;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uController, Grids, StdCtrls, uGlobalDefs, uFlight, uFidsTags, Registry,
  ImgList, Menus, ExtCtrls, ComCtrls, ToolWin, VirtualTrees, usettingsmanager,
  uCommon, Buttons;

type
  TfrmArrivals = class(TForm)
    VST: TVirtualStringTree;
    tmrBackground: TTimer;
    tmrContinuous: TTimer;
    stMain: TStatusBar;
    clbrMain: TCoolBar;
    tbBasic: TToolBar;
    tbbAdd: TToolButton;
    tbbDelete: TToolButton;
    tbbModify: TToolButton;
    tbExtended: TToolBar;
    panelSensors: TFlowPanel;
    mnuMain: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Find1: TMenuItem;
    View1: TMenuItem;
    View2: TMenuItem;
    mnuHelp: TMenuItem;
    imlSensors: TImageList;
    imlMain: TImageList;
    bhManager: TBalloonHint;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Past1: TMenuItem;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Redo1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    Modify1: TMenuItem;
    Find2: TMenuItem;
    FindNext1: TMenuItem;
    N3: TMenuItem;
    Preferences1: TMenuItem;
    Index1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    mnuNavigate: TMenuItem;
    Arrivals1: TMenuItem;
    Departures1: TMenuItem;
    Gates1: TMenuItem;
    Bays1: TMenuItem;
    Baggages1: TMenuItem;
    imetable1: TMenuItem;
    Arrivals2: TMenuItem;
    Departures2: TMenuItem;
    tbDummyButton: TToolButton;
    pnlSearchPanel: TPanel;
    tbClearSearch: TSpeedButton;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tmrBackgroundTimer(Sender: TObject);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrContinuousTimer(Sender: TObject);
    procedure tbDummyButtonClick(Sender: TObject);
    procedure Find2Click(Sender: TObject);
    procedure VSTScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure tbClearSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    {Window specific variables}
    ControllerName : String;

    { Private declarations }
    procedure SetDetail( Field:aFlightField; Detail : string);
		procedure ReloadGrid(SearchField : SmallInt; SearchInput : Variant);

  public
    { Public declarations }
  end;

var
  frmArrivals: TfrmArrivals;
  cfArrivals: CFlightController;


implementation

uses FSearch;

{$R *.dfm}

type
  PTreeData = ^TTreeData;
  TTreeData = record
    Flight: String;
    Ports: String;
    ScheduledTime: String;
    EstimatedTime: String;
    Status: String;
    Gates: String;
    Bays: String;
    Belts: String;
    Carrier: String;
    Terminal: String;
    Rego: String;
    CodeShare: String;
    DBPath : String;
end;

var
  ArrFields : array [ 0..12 ] of aFlightField = ( ffFlight, ffPorts, ffSTdate,ffSTime, ffETdate, ffETime, ffAStatus, ffGates, ffBays, ffBelts, ffCarrier, ffTerminal, ffRego);
  ArrColumn : array [ 0..10 ] of String = ( 'Flight', 'Ports', 'ST', 'ET', 'Status', 'Gates', 'Bays', 'Belts', 'Carrier', 'Terminal', 'Reg');
  { Following vars to keep track of selected nodes, scroll position, sorting when redraw grid every second }
  SelectedFlightPath : String;
  SelectedColumn: TColumnIndex;
  SelectedSortDirection: TSortDirection;
  ScrollDeltaX, ScrollDeltaY: Integer;

  {Remember Filters/Searched strings}
  SearchString : string;
  SearchField : smallint;
  SearchCaseSensitive	: Boolean;


procedure TfrmArrivals.Find2Click(Sender: TObject);
var
	I : Int16;
  strCol : String;
begin
  Application.CreateForm(TfrmSearch, frmSearch);

  // Add columns to search types
  frmsearch.Fields := TStringList.Create;
  I := 0;
  for strCol in ArrColumn do
  begin
    frmsearch.Fields.Add(strCol);
    inc(i);
  end;

  if (frmSearch.ShowModal = SHOW_OPENWINDOW) then
  	if  (frmsearch.ModalResult = mrOk)  then
  	begin
      {when search returns string and field}
      SearchString := frmSearch.cmbsearch.text;
      SearchField := frmSearch.cmbFieldList.ItemIndex;
      SearchCaseSensitive := frmSearch.chkCase.Checked;
      //ShowMessage(inttostr(searchfield));
      Self.pnlSearchPanel.Visible := True;
      Self.tbClearSearch.Visible := True;
      Self.ReloadGrid(SearchField, SearchString);

    end;


end;

procedure TfrmArrivals.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  cfArrivals.Free;

end;

procedure TfrmArrivals.FormCreate(Sender: TObject);
begin
  cfArrivals := CFlightController.Create(afkArrivals, ArrFields);
  uCommon.RHSMenu(Self, mnuNavigate);

  SearchField := -1;
  SearchString := '';
  {Set controller name}
  Self.controllername := 'Arrivals';

end;

procedure TfrmArrivals.FormShow(Sender: TObject);
begin
  Caption := ControllerName;
end;

procedure TfrmArrivals.ReloadGrid(SearchField : SmallInt ; SearchInput : Variant);
var
  TableData, tmpLoopData: PTreeData;
  XNode, tmpLoopNode: PVirtualNode;
  CSPrimary: PVirtualNode;

  strCol: String;
  I,J : Integer;
  CS, DBPath: String;

  {search related}
  TranslatedSearch : String;
label
  SearchActive, SearchInActive, Populate;
begin

	{Redraw Sensors}
  cfArrivals.ImplementSensors(self, imlSensors);

	{
  dont clear as it tries to redraw the whole table everytime and users lose
  window state, what node focused on, etc. try to match and change content of VST
  Call populategrid in uController so Table will be updated
  }
  VST.Clear;

  // Draw Columns
  I := 0;
  for strCol in ArrColumn do
  begin
    //ShowMessage(oFlight.Title[ ff ]);
    vst.Header.Columns.Items[i].Text := strCol;
    inc(i);
  end;

  J := 0;
  for I := 1 to Length(cfArrivals.Table) - 1 do
  begin
    CS := cfArrivals.Table[I][ High(cfArrivals.Table[I])-1 ];
    DBPath := cfArrivals.Table[I][ high(cfArrivals.Table[I]) ];

    {Cleanup LATER!}
    if (SearchField <= -1) then
    begin
      {search function}
        if not (strtoint(CS) = 0) then
        begin
          XNode := VST.AddChild(CSPrimary);
          VST.Expanded[CSPrimary] := true;
        end
        else
        begin
          XNode := VST.AddChild(nil);
          CSPrimary := Xnode;
        end;

        if VST.AbsoluteIndex(XNode) > -1 then
        begin
         TableData := VST.GetNodeData(Xnode);

         TableData^.Flight := cfArrivals.Table[I][0];
         TableData^.Ports := cfArrivals.Table[I][1];
         TableData^.ScheduledTime :=  cfArrivals.Table[I][2] + ' ' + cfArrivals.Table[I][3];
         TableData^.EstimatedTime :=  cfArrivals.Table[I][4] + ' ' + cfArrivals.Table[I][5];
         TableData^.Status :=  cfArrivals.Table[I][6];
         TableData^.Gates :=  cfArrivals.Table[I][7];
         TableData^.Bays :=  cfArrivals.Table[I][8];
         TableData^.Belts :=  cfArrivals.Table[I][9];
         TableData^.Carrier :=  cfArrivals.Table[I][10];
         TableData^.Terminal :=  cfArrivals.Table[I][11];
         TableData^.Rego :=  cfArrivals.Table[I][12];
         TableData^.CodeShare := CS ;    {CODESHARE}
         TableData^.DBPath := DBPath ;    {CODESHARE}
        End;
    end
    else
    begin
      {search function}

      {Spliting and combining ET/ST fields for search
      this is necessary coz ETDate + ETTime = ET in GRID, users search
      on GRID using search functionality so combining fields is neccesary}
      case (SearchField) of
        0..1: TranslatedSearch := cfArrivals.Table[I][SearchField];
        2: TranslatedSearch := cfArrivals.Table[I][2] + ' ' + cfArrivals.Table[I][3]; {ST}
        3: TranslatedSearch := cfArrivals.Table[I][4] + ' ' + cfArrivals.Table[I][5]; {ET}
        4..12: TranslatedSearch := cfArrivals.Table[I][SearchField + 2]; {+2 as we combine ETDate + ETTime / STDate + STTime}
      end;

      {apply search criteria}
      	{case sensitive}
      if not(SearchCaseSensitive) then
      begin
      	SearchInput := LowerCase(SearchInput);
        TranslatedSearch := LowerCase(TranslatedSearch);
      end;

      if ( Pos(String( SearchInput ), TranslatedSearch) > 0 ) then
      begin
        {Count how many search occurences triggerd}
        Inc(J);

        if not (strtoint(CS) = 0) then
        begin
          XNode := VST.AddChild(CSPrimary);
          VST.Expanded[CSPrimary] := true;
        end
        else
        begin
          XNode := VST.AddChild(nil);
          CSPrimary := Xnode;
        end;

        if VST.AbsoluteIndex(XNode) > -1 then
        begin
         TableData := VST.GetNodeData(Xnode);

         TableData^.Flight := cfArrivals.Table[I][0];
         TableData^.Ports := cfArrivals.Table[I][1];
         TableData^.ScheduledTime :=  cfArrivals.Table[I][2] + ' ' + cfArrivals.Table[I][3];
         TableData^.EstimatedTime :=  cfArrivals.Table[I][4] + ' ' + cfArrivals.Table[I][5];
         TableData^.Status :=  cfArrivals.Table[I][6];
         TableData^.Gates :=  cfArrivals.Table[I][7];
         TableData^.Bays :=  cfArrivals.Table[I][8];
         TableData^.Belts :=  cfArrivals.Table[I][9];
         TableData^.Carrier :=  cfArrivals.Table[I][10];
         TableData^.Terminal :=  cfArrivals.Table[I][11];
         TableData^.Rego :=  cfArrivals.Table[I][12];
         TableData^.CodeShare := CS ;    {CODESHARE}
         TableData^.DBPath := DBPath ;    {CODESHARE}
        End;
      end; {end of search if}
    end;

  end;

  {if no occurences matched in search}
  if (SearchField >= 0) AND (J <= 0) then
  begin
  	tmrContinuous.Enabled := false;{disable otherwise you will get messageboxes}
    MessageBox(Handle, 'No results returned', 'Arrival | FIDS', MB_ICONINFORMATION );
    tbClearSearchClick(tbClearSearch);
    tmrContinuous.Enabled := true; {Enable so normal routine will go on}
    Exit;
  end;

  // Following restore saved state after redrawing the table
  VST.SortTree(SelectedColumn, SelectedSortDirection,True);

  // Select previously selected item from GRID
  tmpLoopNode := VST.GetFirst();
  tmpLoopData := VST.GetNodeData(tmpLoopNode);
  if (tmpLoopData^.DBPath = SelectedFlightPath) then
  begin
    vst.Selected[tmpLoopNode] := true;
  end;

  try
    while Assigned(tmpLoopNode) do
    begin
      tmpLoopNode := VST.GetNext(tmpLoopNode);
      tmpLoopData := VST.GetNodeData(tmpLoopNode);
      if (tmpLoopData^.DBPath = SelectedFlightPath) then
      begin
        vst.Selected[tmpLoopNode] := true;
        //Break;
      end;
    end;
  except
    // DO NOTHING; EXPECTED ISSUE
    // beep;
  end;

end;

procedure TfrmArrivals.SetDetail( Field : aFlightField; Detail : string);
var
  tmpLoopData: PTreeData;
  tmpLoopNode: PVirtualNode;

begin
  // Select previously selected item from GRID
  tmpLoopNode := VST.GetFirst();
  tmpLoopData := VST.GetNodeData(tmpLoopNode);
  if (vst.Selected[tmpLoopNode] = true) then
  begin
		cfArrivals.SetDetail(tmpLoopData^.DBPath, Field, Detail);
  end;

  try
    while Assigned(tmpLoopNode) do
    begin
      tmpLoopNode := VST.GetNext(tmpLoopNode);
      tmpLoopData := VST.GetNodeData(tmpLoopNode);
      if (vst.Selected[tmpLoopNode] = true) then
      begin
      	cfArrivals.SetDetail(tmpLoopData^.DBPath, Field, Detail);
      end;
    end;
  except
    // DO NOTHING; EXPECTED ISSUE
    // beep;
  end;

	self.ReloadGrid(SearchField, SearchString);
end;

procedure TfrmArrivals.tbClearSearchClick(Sender: TObject);
begin
  SearchField := -1;
  SearchString := '';
  self.ReloadGrid(SearchField, SearchString);
  Self.tbClearSearch.Visible := false;
	Self.pnlSearchPanel.Visible := false;
end;

procedure TfrmArrivals.tbDummyButtonClick(Sender: TObject);
begin
	{Set status from caption of the button}
  self.SetDetail(ffAStatus, TToolButton(Sender).Caption );
end;

procedure TfrmArrivals.tmrBackgroundTimer(Sender: TObject);
var
	i : SmallInt;

  {Statuses related}
  status : String;
  button : TToolButton;
begin
  tmrBackground.Enabled := false;
  cfArrivals.NewConnection();

  {Build status change buttons}
  for status in cfArrivals.GetStatuses do
  begin
    button := TToolButton.Create(tbExtended);
    button.SetParentComponent(tbExtended);
    button.AutoSize := true;
    button.Caption := status;
    button.OnClick := tbDummyButton.OnClick;
  end;


	{Populate colums of VST}
  for i := 0 to high(ArrColumn) do
  begin
  	VST.Header.Columns.Add();
    vst.Header.Columns.Items[i].Width := 100;
  end;

	{Init sensors}
  cfArrivals.InitSensors(panelSensors);

	self.ReloadGrid(SearchField, SearchString);

end;

procedure TfrmArrivals.tmrContinuousTimer(Sender: TObject);
begin
	self.ReloadGrid(SearchField, SearchString);
end;

procedure TfrmArrivals.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh;
end;

procedure TfrmArrivals.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PTreeData;
  Data2: PTreeData;
  Compare1: String;
  Compare2: String;
begin
  // retrive data from VT and load them to Flight Data Structure
  Data1:=vst.GetNodeData(Node1);
  Data2:=vst.GetNodeData(Node2);

  case Column of
    0: begin Compare1 := Data1.Flight; Compare2 := Data2.Flight; end;
    1: begin Compare1 := Data1.Ports; Compare2 := Data2.Ports; end;
    2: begin Compare1 := Data1.ScheduledTime; Compare2 := Data2.ScheduledTime; end;
    3: begin Compare1 := Data1.EstimatedTime; Compare2 := Data2.EstimatedTime; end;
    4: begin Compare1 := Data1.Status; Compare2 := Data2.Status; end;
    5: begin Compare1 := Data1.Gates; Compare2 := Data2.Gates; end;
    6: begin Compare1 := Data1.Bays; Compare2 := Data2.Bays; end;
    7: begin Compare1 := Data1.Belts; Compare2 := Data2.Belts; end;
    8: begin Compare1 := Data1.Carrier; Compare2 := Data2.Carrier; end;
    9: begin Compare1 := Data1.Terminal; Compare2 := Data2.Terminal; end;
    10: begin Compare1 := Data1.Rego; Compare2 := Data2.Rego; end;
  end;

  // Improve below code so TimeStamp, Integers and Characters can be sorted
  if (not Assigned(Data1)) or (not Assigned(Data2)) then
    Result := 0
  else
    Result := CompareStr(Compare1, Compare2);

end;

procedure TfrmArrivals.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh;
end;

procedure TfrmArrivals.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PTreeData;
begin

  // Free all nodes used.
  Data:=VST.GetNodeData(Node);

  if Assigned(Data) then begin
    Data^.Flight := '';
    Data^.Ports := '';
    Data^.ScheduledTime := '';
    Data^.EstimatedTime := '';
    Data^.Status := '';
    Data^.Gates := '';
    Data^.Bays := '';
    Data^.Belts := '';
    Data^.Carrier := '';
    Data^.Terminal := '';
    Data^.Rego := '';
    Data^.CodeShare := '';
    Data^.DBPath := '';
  end;

end;

procedure TfrmArrivals.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
    NodeDataSize := SizeOf(TTreeData);
end;

procedure TfrmArrivals.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PTreeData;
begin

  Data := VST.GetNodeData(Node);
  case Column of
    0: CellText := Data^.Flight;
    1: CellText := Data^.Ports;
    2: CellText := Data^.ScheduledTime;
    3: CellText := Data^.EstimatedTime;
    4: CellText := Data^.Status;
    5: CellText := Data^.Gates;
    6: CellText := Data^.Bays;
    7: CellText := Data^.Belts;
    8: CellText := Data^.Carrier;
    9: CellText := Data^.Terminal;
    10: CellText := Data^.Rego;
//    11: CellText := Data^.CodeShare;
//    12: CellText := Data^.DBPath;
  end;

end;

procedure TfrmArrivals.VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  vst.Header.SortColumn := Column;

  if Sender.SortDirection=sdAscending then
    Sender.SortDirection:=sdDescending
  else
    Sender.SortDirection:=sdAscending;

  // following is to remember state when redrawing every sec.
  SelectedColumn := Column;
  SelectedSortDirection := Sender.SortDirection;

  vst.SortTree(Column,Sender.SortDirection,True);

end;

procedure TfrmArrivals.VSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tmpNode : PVirtualNode;
  tmpData : PTreeData;
begin

  tmpNode := VST.GetNodeAt(X,Y);
  if (Assigned(tmpNode)) then
  begin
  	tmpData := VST.GetNodeData(tmpNode);
  	SelectedFlightPath := TmpData^.DBPath;
  end;
end;

procedure TfrmArrivals.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
Var
   Data: PTreeData;
begin

  // This section is important when enabling on the spot editing
  // NOT YET IMPLEMENTED
  Data := VST.GetNodeData(Node);
  Case Column of
    0: Data^.Flight := NewText;
    1: Data^.Ports := NewText;
    2: Data^.ScheduledTime := NewText;
  End;

end;

procedure TfrmArrivals.VSTScroll(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
begin
	{following is to remember scroll pos of X,Y directions}
  ScrollDeltaX := DeltaX;
  ScrollDeltaY := DeltaY;

end;

end.



