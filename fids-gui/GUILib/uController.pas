unit uController;

interface

uses
    Controls, Variants, Forms, ComCtrls, Windows, Classes, Graphics,
    ButtonGroup, ASCII,
    uXmlParser, ExtCtrls, StdCtrls, uUtils, uPoller, uPacket, uFlight,
    uGlobalDefs,
    uGT, uMessageHub, udbTree, uMirrorDB, uFidsTags, SysUtils,
    uFIDSXml, uCommon, Dialogs,  Math, uTTRules, ColorButton;

type
    // Set sensor types so we can use them in any window require.
    TSensorType = (Hosts = 0, Feeds = 1, Workstations = 2, Timefeed = 3,
      WebFeeds = 4);
    TSensorDataType = (Name = 0, IP = 1, Status = 2);

    CFlightController = class
    public
        JobName: string;
        Table: array of TTreeData;
        // Used only for timetables
        oTTRulesList: cTTRulesList;
        oRules: array [0 .. 20] of apNode;
        oRule: cTTRule;
        FXml: TFIDSxml;

        { Basic functionality }
        procedure PopulateGrid();
        procedure PopulateTTGrid();
        procedure fOnIdle(Sender: TObject; var Done: Boolean);
        procedure NewConnection(cID: TFIDSWindowID);
        procedure SetDetail(flightPath: string; ff: aFlightField;
          detail: string);
        procedure ImplementSensors(formName: TForm; imgSensors: TImageList);
        procedure InitSensors(panelSensors: TControl);

        { Sensor related }
        function GetSensorTypes(): TStringList;
        function StringToSensorType(str: string): TSensorType;
        function GetSensorCount(const typeofsensor: TSensorType): int16;
        function GetSensorData(const typeofsensor: TSensorType;
          const readwhat: TSensorDataType; index: Int): string;
        procedure SensorClick(Sender: TObject);
        { Dynamic statuse messages and other attributes retrival
          functions }
        function GetStatuses(): TStringList;
        function GetBelts(): TStringList;
        function GetGates(): TStringList;
        function GetCheckinCounters(): TStringList;
        function GetBays(): TStringList;
        function GetCarrierTypes(): TStringList;
        function GetAircrafts(): TStringList;
        function GetTerminals(): TStringList;
        function GetListOfCGBBUsed(ControllerID: TFIDSWindowID): TStringList;
        function GetUsers(): TStringList;
        function isHostRunning():Boolean;

        constructor Create(afkInput: aFlightKind;
          affFields: array of aFlightField);
        destructor Destroy; override;

    private
        ControllerID: TFIDSWindowID;
        // generic flight manupulation
        oFlight: cFlight;
        oFlights: cFlightList;

        // Variables to customize Controller object
        Fields: array of aFlightField;
        afKind: aFlightKind;

        { Sensor Groups }
        SensorGroup: array [0 .. 5] of TFlowPanel;
        SensorLabel: array [0 .. 5] of TLabel;
        SensorsInitialized: Boolean;

        procedure TNewData(const xml: string);
    end;

implementation
uses FIndicators, uConnection;

const
    // Extra numbers to extend row and field capacity of Table
    exRows = 1;

constructor CFlightController.Create(afkInput: aFlightKind;
  affFields: array of aFlightField);
var
    ff: aFlightField;
    c: Int;
begin
    afKind := afkInput;
    setlength(Fields, High(affFields) + 10);

    FXml := FXml.Instance;
    // if ( FXml.oHub.Connected ) then
    // Set flightkind for the Object, afkDepartures/afkArrivals/afkNone
    // so data will be populated accordingly.

    c := 0;
    for ff in affFields do
    begin
        Fields[c] := ff;
        inc(c);
    end;

end;

function CFlightController.isHostRunning():Boolean;
begin
    Result := DB.Ready;  // not FXml.mShutDown;
end;

destructor CFlightController.Destroy();
begin
    //FXml.oHub.Disconnect(true);
    FreeAndNil( Xml_Connection );
    FreeAndNil(FXml);
    oFlight.Free;
end;

procedure CFlightController.TNewData(const xml: string);
// aDeltaNotify - called whenever DB changes
begin
    oTTRulesList.Build(tfNone, '');
    // node list should always be safe as long as Build is not decoupled from NewData events
end;

function CFlightController.GetUsers(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsUsersPath;

    // Result := TStringList.Create;
    showmessage(FXml.GetDataTree.GetNode(path).Content);

end;

procedure CFlightController.NewConnection(cID: TFIDSWindowID);
var
    i: integer;
begin
    // get the jobname (airport name)
    JobName := FXml.GetDataTree.GetNode(fidsJobNamePath).Content;
    ControllerID := cID;

    if (FXml.mInit) then
    begin
        if (cID = FIDSCheckins) OR (cID = FIDSArrivals) OR
          (cID = FIDSDepartures) OR (cID = FIDSGates) OR (cID = FIDSBays) OR
          (cID = FIDSBelts) then
        begin
            // LogIn( FeedID, SysPW, FeedID );
            // FXml.oDataTree.RegisterReader( PopulateGrid );
            oFlight := cFlight.Create( DB, 'Feed');
            // need a logged in id ('EgGUI') to update DB
            oFlight.Kind := afKind;
            PopulateGrid();
        end;

        if (cID = FIDSTArrivals) OR (cID = FIDSTDepartures) then
        begin
            // LogIn( FeedID, SysPW, FeedID );
            // FXml.oDataTree.RegisterReader( PopulateGrid );
            // oRule := oRule.Create( fxml.oDataTree, 'Feed' );   // need a logged in id ('EgGUI') to update DB
            oRule := cTTRule.Create( DB, 'Feed');
            // need a logged in id ('EgGUI') to update DsB;
            oRule.oTemplate.Kind := afKind;
            PopulateTTGrid();
        end;

    end
    else
    begin
        FXml.mShutDown := true;
        //showmessage('FIDSXml Server not running or experianced a connection problem');
    end;

end;

function CFlightController.GetStatuses(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin
    if (afKind = fkDepartures) then
        path := uCommon.fidsDStatusPath
    else
        path := uCommon.fidsAStatusPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetGates(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsGatesPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetBays(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsBaysPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetCheckinCounters(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsCheckinsPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetCarrierTypes(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsCarrierTypesPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetBelts(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsBeltsPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetAircrafts(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsAircraftTypesPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

function CFlightController.GetTerminals(): TStringList;
var
    path: string;
    tmp: apNode;
    i: Int;
begin

    path := uCommon.fidsTerminalsPath;

    Result := TStringList.Create;
    uCommon.ParseDelimited(Result, FXml.GetDataTree.GetNode(path).Content, ',');

end;

procedure CFlightController.InitSensors(panelSensors: TControl);
var
    i: integer;
    SensorType: string;
    x: TFont;
begin
    // panelSensors.Height := (fidsSensorHeight * fidsHowManyVertically) + 20;
    panelSensors.Height := uCommon.fidsSensorsPanelHeight;
    i := 0;
    { Sensor Specific Routines }
    { Initializing sensor bars on top layer }
    for SensorType in self.GetSensorTypes do
    begin
        { Label }
        SensorLabel[i] := Tlabel.Create(panelSensors);
        SensorLabel[i].Name := 'lbl' + SensorType;
        SensorLabel[i].SetParentComponent(panelSensors);
        SensorLabel[i].Transparent := true;
        SensorLabel[i].Canvas.Font.Style := [fsBold];
        SensorLabel[i].Height := panelSensors.Height;

        SensorLabel[i].Width := SensorLabel[i].Canvas.Font.Size * Length(SensorType);

        SensorLabel[i].Caption := SensorType;
        { Sensors }
        SensorGroup[i] := TFlowPanel.Create(panelSensors);
        SensorGroup[i].SetParentComponent(panelSensors);
        SensorGroup[i].BorderStyle := bsNone;
        SensorGroup[i].Color := TFlowPanel(panelSensors).Color;
        SensorGroup[i].AlignWithMargins := true;
        SensorGroup[i].Height := panelSensors.Height;

        inc(i);
    end;

    SensorsInitialized := true;
end;

procedure CFlightController.SensorClick(Sender: TObject);
var
    StatusMsg: array [0 .. 255] of WideChar;
    StrStatus: String;
    btnSensorIndex: smallint;
    ButtonX, ButtonY: Int;
    WindowX, WindowY: Int;
    IsWorking: Boolean;
    TextColor: TColor;
begin

    IsWorking := true; // default value
    WindowX := TForm(TFlowPanel(TButton(Sender).GetParentComponent)
      .GetParentComponent).Left;
    WindowY := TForm(TFlowPanel(TButton(Sender).GetParentComponent)
      .GetParentComponent).Top;
    ButtonX := Mouse.CursorPos.x - (TButton(Sender).Left + WindowX);
    ButtonY := Mouse.CursorPos.Y - (TButton(Sender).Top + WindowY);

    if (TButton(Sender).Caption = '»') then
    begin
        frmManageIndicators.Show();
    end

    // ShowMessage(Sender.ClassName);
end;

procedure CFlightController.ImplementSensors(formName: TForm;
  imgSensors: TImageList);
var
    { Sensor Related }
    Sensor: TColorButton;
    SensorType: String;
    SensorCount, i, J: integer;
    SensorStatus, Location : String;

begin
    if (SensorsInitialized = False) then
        exit();

    i := 0;
    J := 0;
    { Sensor Specific Routines }
    { Initializing sensor bars on top layer }

    for SensorType in self.GetSensorTypes do
    begin

        { if a group of sensors is focused then ignore refreshing, coz you need to show tooltips }
        if not SensorGroup[J].Focused then
        begin
            SensorCount :=
              (self.GetSensorCount(self.StringToSensorType(SensorType)));

            SensorGroup[J].Margins.Left := 0;
            SensorGroup[J].Margins.Bottom := 0;
            SensorGroup[J].Margins.Top := 0;
            SensorGroup[J].Margins.Right := 10;
            SensorGroup[J].BorderStyle := bsNone;
            SensorGroup[J].BevelOuter := bvNone;

            { Width should be sensor_count / howmany vertically so you get how many you need horizontally, muliplying it by
              width give you controller width }

            if (SensorCount <= 5) then
            begin
                SensorGroup[J].Width :=  fidsSensorWidth *
                  Ceil(SensorCount+1 / fidsHowManyVertically) ;
            end
            else
            begin
                { if greater than 5 items then use 5 coz we dont list anymore on the top layer }
                SensorGroup[J].Width := fidsSensorWidth * 7;
            end;

            for i := 0 to (SensorCount - 1) do
            begin
                // if (i < 5) then
                begin

                    Sensor := TColorButton.Create(nil);
                    Sensor.Parent := SensorGroup[J];
                    Sensor.ShowHint := true;


                    Sensor.Width := uCommon.fidsSensorWidth;
                    Sensor.Height := uCommon.fidsSensorHeight;

                    if (i = 5) then
                    begin
                        Sensor.Caption := '»';
                        Sensor.OnClick := SensorClick;
                        break;
                    end; // end of sensorcount = 5 if

                    Sensor.OnClick := SensorClick;

                    // if () then
                    if (uCommon.isSensorDown
                      (self.GetSensorData((self.StringToSensorType(SensorType)),
                      Status, i)) = 0) then
                    begin
                        Sensor.Color := clRed;
                        SensorStatus := 'Not working';
                    end
                    else
                    begin
                        Sensor.Color := clGreen;
                        SensorStatus := 'Active';
                    end;

                    if (SensorType = 'Workstations') then
                        Location := 'Location: Room ' + inttostr(i)
                    else
                        Location := '';

                    Sensor.Hint := 'Name: ' +
                      self.GetSensorData
                      ((self.StringToSensorType(SensorType)), Name, i) + #10#13 +
                      'IP: ' +
                      self.GetSensorData
                      ((self.StringToSensorType(SensorType)), IP, i) + #10#13 +
                      'Status: ' + SensorStatus  + #10#13 +
                     Location;

                end; // end of sensorcount if

            end;
            inc(J);
        end; { endif FOCUSED }
    end;

    { END - Sensor Specific Routines }
end;

function CFlightController.StringToSensorType(str: string): TSensorType;
begin

    if (str = 'Hosts') then
        Result := Hosts;

    if (str = 'Feeds') then
        Result := Feeds;

    if (str = 'Workstations') then
        Result := Workstations;

    if (str = 'Timefeed') then
        Result := Timefeed;

    if (str = 'WebFeeds') then
        Result := WebFeeds;

end;

function CFlightController.GetSensorTypes(): TStringList;
var
    tmp: apNode;
    i: int16;
begin

    Result := TStringList.Create;
    for i := 0 to SubNodeCount( FXml.GetDataTree.GetNode(fidsSensorPath) ) - 1 do
    begin

        tmp := FXml.GetDataTree.GetNode(fidsSensorPath).SubNodes.Items[i];
        Result.Add(tmp.NodeName);
    end;

end;


function CFlightController.GetSensorCount(const typeofsensor
  : TSensorType): int16;
var
    ret: int16;
    path: string;
    i: int16;
begin

    case typeofsensor of
        Hosts:
            path := '|Hosts';
        Feeds:
            path := '|Feeds';
        Workstations:
            path := '|Workstations';
        Timefeed:
            path := '|Timefeed';
        WebFeeds:
            path := '|WebFeeds';
    end;

    ret := FXml.GetDataTree.GetNode(fidsSensorPath + path).SubNodes.Count;

    Result := ret;
end;

function CFlightController.GetSensorData(const typeofsensor: TSensorType;
  const readwhat: TSensorDataType; index: Int): string;
var
    ret: string;
    path: string;
    tmp, Sensor: apNode;
    i: int16;
begin

    case typeofsensor of
        Hosts:
            path := '|Hosts';
        Feeds:
            path := '|Feeds';
        Workstations:
            path := '|Workstations';
        Timefeed:
            path := '|Timefeed';
        WebFeeds:
            path := '|WebFeeds';
    end;

    Sensor := FXml.GetDataTree.GetNode(fidsSensorPath + path)
      .SubNodes.Items[index];

    case readwhat of
        Name:
            tmp := Sensor.SubNodes.First;
        IP:
            tmp := Sensor.SubNodes.Items[1];
        Status:
            tmp := Sensor.SubNodes.Last;
    end;

    Result := tmp.Content;
end;

procedure CFlightController.PopulateTTGrid();
var
    ff: aFlightField;
    strKind: string;
    r, i, ffSTA, ffETA: Int;
begin

    oTTRulesList := cTTRulesList.Create(FXml.GetDataTree);
    oTTRulesList.Build(tfNone, '');

    setlength(Table, oTTRulesList.Count + exRows);

    if (ControllerID = FIDSTArrivals) then
    begin
        strKind := 'Arrivals';
    end;

    if (ControllerID = FIDSTDepartures) then
    begin
        strKind := 'Departures';
    end;

    oRule := cTTRule.Create( FXml.GetDataTree, 'Feed');
    // need a logged in id ('EgGUI') to update DsB;
    oRule.oTemplate.Kind := afKind;

    i := 0;
    for r := 0 to oTTRulesList.Count - 1 do
    begin
        oRule.DbNode := oTTRulesList[r];

        // need own object since vst calls GetText a lot
        oRule.oTemplate.DbNode := oRule.oTemplate.DbNode;

        if (oRule.Presentation[tfPath] = strKind) then
        begin
            oRules[i] := oRule.DbNode;

            Table[i].Flight := oRule.Presentation[tfRuleName];
            Table[i].Ports := oRule.oTemplate.Presentation[ffPorts];

            Table[r].STDate := uCommon.FIDS_StrToDT
              (oRule.oTemplate.Presentation[ffSTdate]);
            Table[r].STime := uCommon.FIDS_StrTOTime
              (oRule.oTemplate.Presentation[ffSTime]);

            Table[r].ETDate := uCommon.FIDS_StrToDT
              (oRule.oTemplate.Presentation[ffETdate]);
            Table[r].ETime := uCommon.FIDS_StrTOTime
              (oRule.oTemplate.Presentation[ffETime]);

            Table[r].ATDate := uCommon.FIDS_StrToDT
              (oRule.oTemplate.Presentation[ffATdate]);
            Table[r].ATime := uCommon.FIDS_StrTOTime
              (oRule.oTemplate.Presentation[ffATime]);

            Table[i].Gates := oRule.oTemplate.Presentation[ffGates];
            Table[i].Bays := oRule.oTemplate.Presentation[ffBays];
            Table[i].CheckIns := oRule.oTemplate.Presentation[ffCheckIns];
            Table[i].Belts := oRule.oTemplate.Presentation[ffBelts];
            Table[i].Carrier := oRule.oTemplate.Presentation[ffCarrier];
            Table[i].Terminal := oRule.oTemplate.Presentation[ffTerminal];
            Table[i].Rego := oRule.oTemplate.Presentation[ffRego];
            Table[i].CodeShare := oRule.oTemplate.CodeShare;
            Table[i].Comment := oRule.oTemplate.Presentation[ffComment];
            Table[i].NonPublic := oRule.oTemplate.Presentation[ffNonPublic];

            //Newly Added
            {
            Table[i].Slottime := oRule.oTemplate.Presentation[ffSlotTime];
            Table[i].CheckinOpeningTime := oRule.oTemplate.Presentation[ffCheckinOpeningTime];
            Table[i].CheckinClosingTime := oRule.oTemplate.Presentation[ffCheckinClosingTime];
            Table[i].RelatedFlight := oRule.oTemplate.Presentation[ffRelatedFlight];
            Table[i].Raceway := oRule.oTemplate.Presentation[ffRaceway];
            Table[i].Aircraft := oRule.oTemplate.Presentation[ffAirCraft];
            Table[i].OffBlock := oRule.oTemplate.Presentation[ffOffBlock];
            }
            Table[i].DBPath := oRule.oTemplate.DBPath;

            inc(i);
        end;
    end;

    setlength(Table, i + exRows);
    oTTRulesList.Free;
end;

procedure CFlightController.PopulateGrid();
var
    p: apNode;
    // made the following public in order to use for update
    ff: aFlightField;
    r, ffSTA, ffETA: Int;
begin

    oFlights := cFlightList.Create( DB );
    if oFlight = nil then  oFlight:= cFlight.Create( DB, Xml_Connection.iDB.ID );

    // the last parameter in Build function works like a filter
    // flights.Build( afkDepartures, ffGates, '3' );
    oFlights.Build(afKind, ffNone, '');

    // set grid size for data storage
    // + 1 Row for column names and + 1 column for codeshare + extra field to compensate population
    setlength(Table, oFlights.Count + exRows);
    r := 0;

    for p in oFlights do
    begin
        oFlight.DbNode := p;
        Table[r].Flight := oFlight.Presentation[ffFlight];
        Table[r].Ports := oFlight.Presentation[ffPorts];

        //
        // TODO: Clean this up later
        //

        // ST
        Table[r].STDate := uCommon.FIDS_StrToDT(oFlight.Presentation[ffSTdate]);
        Table[r].STime  := FIDS_StrTOTime(oFlight.Presentation[ffSTime]);

        // ET
        Table[r].ETDate := uCommon.FIDS_StrToDT(oFlight.Presentation[ffETdate]);
        Table[r].ETime  := uCommon.FIDS_StrTOTime(oFlight.Presentation[ffETime]);

        // AT
        Table[r].ATDate := uCommon.FIDS_StrToDT(oFlight.Presentation[ffATdate]);
        Table[r].ATime  := uCommon.FIDS_StrTOTime(oFlight.Presentation[ffATime]);

        if (afKind = fkArrivals) then
            Table[r].Status := oFlight.Presentation[ffAStatus];
        if (afKind = fkDepartures) then
            Table[r].Status := oFlight.Presentation[ffDStatus];

        Table[r].Gates := oFlight.Presentation[ffGates];
        Table[r].Bays := oFlight.Presentation[ffBays];
        Table[r].CheckIns := oFlight.Presentation[ffCheckIns];
        Table[r].Belts := oFlight.Presentation[ffBelts];
        Table[r].Carrier := oFlight.Presentation[ffCarrier];
        Table[r].Terminal := oFlight.Presentation[ffTerminal];
        Table[r].Rego := oFlight.Presentation[ffRego];
        Table[r].CodeShare := oFlight.CodeShare;
        Table[r].Comment := oFlight.Presentation[ffComment];
        // ShowMessage( oFlight.Presentation[ffNonPublic] );

        //Newly Added
        Table[r].Slottime := oFlight.Presentation[ffSlotTime];
        Table[r].ScheduledCheckinCounters := oFlight.Presentation[ffScheduledCheckinCounters];
        Table[r].CheckinOpeningTime := oFlight.Presentation[ffCheckinOpeningTime];
        Table[r].CheckinClosingTime := oFlight.Presentation[ffCheckinClosingTime];
        Table[r].RelatedFlight := oFlight.Presentation[ffRelatedFlight];
        Table[r].Raceway := oFlight.Presentation[ffRaceway];
        Table[r].Aircraft := oFlight.Presentation[ffAirCraft];
        Table[r].OffBlock := oFlight.Presentation[ffOffBlock];
        Table[r].OnBlock := oFlight.Presentation[ffOnBlock];


        Table[r].NonPublic := oFlight.Presentation[ffNonPublic];
        Table[r].Crawling := oFlight.Presentation[ffCrawling];
        Table[r].DBPath := oFlight.DBPath;

        inc(r);
    end;

    { Potential memory leak? }
    oFlights.Free;

end;

procedure CFlightController.fOnIdle(Sender: TObject; var Done: Boolean);
begin
    // if mShutDown then  Destroy;
end;

{
  Possible overload
  - Make this the global update routine, so all fields can be updated with the same function
  - parameters would be FieldType and data
}
procedure CFlightController.SetDetail(flightPath: string; ff: aFlightField;
  detail: string);
begin

    oFlight.DBPath := flightPath;

    // Update fields
    if (oFlight.DbNode <> nil) then
    begin
	    oFlight.Presentation[ff] := detail
    end
    else
    begin
        showmessage('Error 554: Flight db connectivity lost');
    end;

end;

function CFlightController.GetListOfCGBBUsed(ControllerID: TFIDSWindowID)
  : TStringList;
var
    TmpFlight: TTreeData;

    i: integer;
    Counter: TStringList;
    FlightCGBBValue: String;
    tmp1, tmp2: string;
    sortNumeric : Boolean;
begin

    { Unique checkin counters list }
    Result := TStringList.Create;
    { Following counts how many flights per counter
      get the count and later it is used as columns count }
    for TmpFlight in Table do
    begin
        { Differentiate what data extract from flight info }
        case (ControllerID) of
            FIDSGates:
                FlightCGBBValue := trim(TmpFlight.Gates);
            FIDSBays:
                FlightCGBBValue := trim(TmpFlight.Bays);
            FIDSCheckins:
                FlightCGBBValue := trim(TmpFlight.CheckIns);
            FIDSBelts:
                FlightCGBBValue := trim(TmpFlight.Belts);
        end;
        { StringList is for flights with multiple checkin counters, so we split them and get
          individual counters }
        Counter := TStringList.Create;
        try
            if (Length(FlightCGBBValue) > 0) then
            begin
                Split(fidsSplitField, FlightCGBBValue, Counter);
            end;

            if (Counter.Count > 0) then
            begin
                for tmp1 in Counter do
                begin
                    i := 0;
                    if Result.Count > 0 then
                    begin
                        for tmp2 in Result do
                        begin

                            { if tmp1 is not in counters then add it to counters }
                            if not((tmp1) = (trim(tmp2))) then
                            // if not AnsiCompareStr ( trim(tmp1), trim(tmp2) ) = 0 then
                            begin
                                inc(i);
                            end;

                            if (i <> 0) AND (i = Result.Count) then
                                Result.Add(trim(tmp1));

                        end; { END of tmp2 in Counters }

                    end { END of if counter Count }
                    else { ELSE of if counter Count }
                    begin
                        //Result.Add(inttostr(strtoint(trim(tmp1))));
                        Result.Add(trim(tmp1));
                        // showMessage(tmp1);
                    end;

                end; { END of tmp1 in Counter }
            end; { Counter.count >= 0 }

        finally
            FreeAndNil(Counter);
        end;

    end;

    sortNumeric := true;
    for FlightCGBBValue in Result do
    begin

        if (IsStrANumber(trim(FlightCGBBValue)) = false) then
        begin // if at least one item is not numeric, use ascii based sort
            sortNumeric := false;
        end;

    end;

    if (sortNumeric) then
        Result := ucommon.BubbleSort(Result)
    else
        Result.Sort;

    // Sorting list of numbers: http://delphi.about.com/od/adptips2003/a/bltip1103_3.htm

    // http://delphi.about.com/od/delphitips2008/qt/return_strings.htm
    // Result.Free;

end;

end.
