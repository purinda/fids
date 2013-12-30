unit FMain;

interface

uses
  Windows,  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, uFlight, uFidsTags, uCommon, uController, FWindow,
  FSearch, Menus, ExtCtrls, ImgList, VrControls, VrLcd, ButtonGroup;

type
  TfrmMain = class(TForm)
    cmdArrivals: TButton;
    cmdDepartures: TButton;
    mnuMain: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Past1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    Modify1: TMenuItem;
    Find1: TMenuItem;
    Find2: TMenuItem;
    FindNext1: TMenuItem;
    View1: TMenuItem;
    N3: TMenuItem;
    Preferences1: TMenuItem;
    View2: TMenuItem;
    mnuHelp: TMenuItem;
    Index1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    panelSensors: TFlowPanel;
    tmrBackground: TTimer;
    tmrContinuous: TTimer;
    imlSensors: TImageList;
    cmdCheckins: TButton;
    cmdGates: TButton;
    cmdBays: TButton;
    cmdBelts: TButton;
    cmdSchedules: TButton;
    imlMain: TImageList;
    mnuShedules: TPopupMenu;
    Arrivals3: TMenuItem;
    Departures3: TMenuItem;
    Panel1: TPanel;
    VrClock1: TVrClock;
    Label1: TLabel;
    Manage1: TMenuItem;
    Button1: TButton;
    mnuGanttChart: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Belts1: TMenuItem;
    imgGanttImg: TImageList;
    mnuCrawlingLine: TMenuItem;
    lblHostUnavailable: TLabel;
    procedure cmdArrivalsClick(Sender: TObject);
    procedure cmdDeparturesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrContinuousTimer(Sender: TObject);
    procedure cmdCheckinsClick(Sender: TObject);
    procedure cmdGatesClick(Sender: TObject);
    procedure cmdBaysClick(Sender: TObject);
    procedure cmdBeltsClick(Sender: TObject);
    procedure tmrBackgroundTimer(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Arrivals3Click(Sender: TObject);
    procedure Departures3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Manage1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmdSchedulesClick(Sender: TObject);
    procedure mnuCrawlingLineClick(Sender: TObject);
  private
    { Private declarations }
    fcWindow : CFlightController;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  frmWindows : array  [0..7]  of TfrmWindow;
  frmX	: TfrmWindow;
implementation

uses FIndicators, FCrawlineLines, uConnection;

{$R *.dfm}

procedure TfrmMain.cmdArrivalsClick(Sender: TObject);
begin
    if (ucommon.ArrivalsRunning) then
    begin
        ShowWindow(ucommon.ArrivalsHandle, SW_RESTORE);
        Exit;
    end;

  {Set arrival window}
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
    frmWindows[0].SetController('Arrivals - ' + Caption, fkArrivals, uCommon.ArrivalSortedFields, uCommon.ArrivalSortedColumns );
    frmWindows[0].Show;
  end;

end;


procedure TfrmMain.cmdCheckinsClick(Sender: TObject);
begin
    if (ucommon.CheckinsRunning) then
    begin
        ShowWindow(ucommon.CheckinsHandle, SW_RESTORE);
        Exit;
    end;

    {Set checkkins window}
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
        frmWindows[2].SetController('Check-in - '+ Caption, fkDepartures, uCommon.CheckinsFields, uCommon.CheckinsColumns );
        frmWindows[2].Show;
    end;
end;

procedure TfrmMain.cmdDeparturesClick(Sender: TObject);
begin
    if (ucommon.DeparturesRunning) then
    begin
        ShowWindow(ucommon.DeparturesHandle, SW_RESTORE);
        Exit;
    end;

    {Set arrival window}
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
        frmWindows[1].SetController('Departures - '+ Caption, fkDepartures, uCommon.DeparturesSortedFields, uCommon.DeparturesSortedColumns );
        frmWindows[1].Show;
    end;

end;

procedure TfrmMain.cmdGatesClick(Sender: TObject);
begin
    if (ucommon.GatesRunning) then
    begin
        ShowWindow(ucommon.GatesHandle, SW_RESTORE);
        Exit;
    end;

    {Set checkkins window}
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
        frmWindows[3].SetController('Gates - '+ Caption, fkDepartures, uCommon.GatesFields, uCommon.GatesColumns );
        frmWindows[3].Show;
    end;
end;

procedure TfrmMain.cmdSchedulesClick(Sender: TObject);
begin
    mnuShedules.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;

procedure TfrmMain.Departures3Click(Sender: TObject);
begin

    if (ucommon.TDepartuesRunning) then
    begin
        ShowWindow(ucommon.TDepartuesHandle, SW_RESTORE);
        Exit;
    end;

    {Set checkkins window}
    if (assigned(frmWindows[7])) then
    begin
        frmWindows[7].Show;
        frmWindows[7].WindowState := wsNormal;
    end
    else
    begin
        frmWindows[7] := TfrmWindow.Create(nil);
        frmWindows[7].ControllerType := FIDSVerticallyPopulated;
        frmWindows[7].ControllerID := FIDSTDepartures;
        frmWindows[7].SetController('Timetable Departures - '+ Caption, fkDepartures, uCommon.TDeparturesFields, uCommon.TDeparturesColumns );
        frmWindows[7].Show;
    end;

    {Set arrival window}


end;

procedure TfrmMain.cmdBaysClick(Sender: TObject);
begin
    //ShowMessage('Still improving bays as a summary window, Not finished');


    {Set checkkins window}

    if (assigned(frmWindows[4])) then
    begin
        frmWindows[4].Show;
        frmWindows[4].WindowState := wsNormal;
    end
    else
    begin
        frmWindows[4] := TfrmWindow.Create(nil);
        frmWindows[4].ControllerType := FIDSHorizontallyPopulated;
        frmWindows[4].ControllerID := FIDSBays;
        frmWindows[4].SetController('Bays - '+ Caption, fkArrivals, uCommon.BaysFields, uCommon.BaysColumns );
        frmWindows[4].Show;
    end;

end;

procedure TfrmMain.cmdBeltsClick(Sender: TObject);
begin
    if (ucommon.BeltsRunning) then
    begin
        ShowWindow(ucommon.BeltsHandle, SW_RESTORE);
        Exit;
    end;

    {Set checkkins window}
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
        frmWindows[5].SetController('Belts - '+ Caption, fkArrivals, uCommon.BeltsFields, uCommon.BeltsColumns );
        frmWindows[5].Show;
    end;
end;

procedure TfrmMain.Arrivals3Click(Sender: TObject);
begin
    if (ucommon.TArrivalsRunning) then
    begin
        ShowWindow(ucommon.TArrivalsHandle, SW_RESTORE);
        Exit;
    end;

    {Set checkkins window}
    if (assigned(frmWindows[6])) then
    begin
        frmWindows[6].Show;
        frmWindows[6].WindowState := wsNormal;
    end
    else
    begin
        frmWindows[6] := TfrmWindow.Create(nil);
        frmWindows[6].ControllerType := FIDSVerticallyPopulated;
        frmWindows[6].ControllerID := FIDSTArrivals;
        frmWindows[6].SetController('Timetable Arrivals - '+ Caption, fkArrivals, uCommon.TArrivalFields, uCommon.TArrivalColumns  );
        frmWindows[6].Show;
    end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
    mnuGanttChart.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;

procedure TfrmMain.Button9Click(Sender: TObject);
begin
  //fcWindow.NewConnection();
  //fcWindow.InitSensors(panelSensors);
  //fcWindow.ImplementSensors(self, imlSensors);

end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

    if MessageDlg('Close FIDS GUI?', mtconfirmation, [mbNo, mbYes], 0, mbNo) = mrYes then
    begin
    	CanClose:= True;
      Application.Terminate;
    end
    else
    	CanClose:= False;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
    fcWindow := CFlightController.Create(fkArrivals, ArrivalFields);
    Connect;
    //WindowState := wsMinimized;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
    VrClock1.Color := rgb(0,5,5);
    Vrclock1.Palette.Low := rgb(0,20,20);
    Vrclock1.Palette.High := rgb(0,255,255);
    Panel1.Color := rgb(0,0,0);

end;

procedure TfrmMain.Manage1Click(Sender: TObject);
begin
  frmManageIndicators.Show();
end;

procedure TfrmMain.mnuCrawlingLineClick(Sender: TObject);
begin
    FCrawlineLinesAllocator := TFCrawlineLinesAllocator.Create(nil);
    FCrawlineLinesAllocator.Show;

end;

procedure TfrmMain.tmrBackgroundTimer(Sender: TObject);
begin
    fcWindow.NewConnection(FIDSArrivals);
    Caption := fcWindow.JobName;
    {Init sensors}
    fcWindow.InitSensors(panelSensors);
    {Redraw Sensors}
    fcWindow.ImplementSensors(self, imlSensors);

    tmrBackground.Enabled := false;

end;

procedure TfrmMain.tmrContinuousTimer(Sender: TObject);
var
    _control : TControl;
    i : integer;
begin
    if not (fcWindow.isHostRunning) then
    begin
        // ShowMessage('host closed');

        for i := 0 to panelSensors.ControlCount-1 do
        begin
            _control := panelSensors.Controls[i];

            if (_control.ClassName = 'TLabel') then
                _control.Visible := false;

            if (_control.ClassName = 'TFlowPanel') then
                _control.Visible := false;

            if (_control.Name = 'lblHostUnavailable') then
            begin
                _control.Visible := true;
            end;

        end;
    end;


    //tmrContinuous.Enabled := false;


end;

end.
