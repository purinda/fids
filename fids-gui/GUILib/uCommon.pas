unit uCommon;

interface

uses
    Windows, Forms, Dialogs, Menus, Messages, Commctrl, SysUtils, Classes,
    Graphics, Controls, uGT, uDbTree, uFlight, uTTRules;

{ Type information for data exchanged between windows }
type
    PTreeData = ^TTreeData;

    TTreeData = record
        Flight: String;
        Ports: String;
        STDate: TDate;
        STime: TTime;
        ETDate: TDate;
        ETime: TTime;
        ATDate: TDate;
        ATime: TTime;
        Status: String;
        CheckIns: String;
        Gates: String;
        Bays: String;
        Belts: String;
        Carrier: String;
        Terminal: String;
        Rego: String;
        CodeShare: SmallInt;
        Comment: string;
        Crawling: string;
        NonPublic: string;
        RelatedFlight: string;
        OffBlock:String;
        OnBlock:String;
        Raceway:String;
        Slottime:String;
        ScheduledCheckinCounters: String;
        CheckinOpeningTime: String;
        CheckinClosingTime: String;
        Aircraft:String;
        DBPath: String;
    end;

    { Type information for Checkins, Baggages, Bays and Belts }
type
    PCGBB = ^TCGBB;

    TCGBB = record
        CGBB: String;
        FlightList: TStringList;
        FlightPathList: TStringList;
    end;

    TFIDSWindowMode = (FIDSListingMode = 0, FIDSSearchMode = 1);
    TFIDSWindowType = (FIDSVerticallyPopulated = 0,
      FIDSHorizontallyPopulated = 1);
    TFIDSWindowID = (FIDSArrivals = -3, FIDSDepartures = -2, FIDSTArrivals = -1,
      FIDSTDepartures = 0, FIDSGates = 1, FIDSBays = 2, FIDSBelts = 3,
      FIDSCheckins = 4);

var
    ArrivalsRunning: Boolean;
    DeparturesRunning: Boolean;
    BaysRunning: Boolean;
    CheckinsRunning: Boolean;
    GatesRunning: Boolean;
    BeltsRunning: Boolean;
    TArrivalsRunning :Boolean;
    TDepartuesRunning :Boolean;

    DeparturesHandle : HWND;
    ArrivalsHandle : HWND;
    BaysHandle : HWND;
    CheckinsHandle : HWND;
    GatesHandle : HWND;
    BeltsHandle : HWND;
    TArrivalsHandle :HWND;
    TDepartuesHandle :HWND;



    { Different data types and arrays used in different windows }
    ArrivalFields: array [0 .. 17] of aFlightField = (
        ffFlight,
        ffRego,
        ffAirCraft,
        ffPorts,
        ffAStatus,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        //ffOnBlock,
        ffATime,
        ffATdate,
        //ffSlotTime,
        ffGates,
        ffBays,
        ffTerminal,
        ffBelts,
        ffCarrier,
        ffNonPublic,
        //ffRelatedFlight,//relatedflight
        //ffRaceway,
        ffComment
        //ffCrawling
    );

    ArrivalColumns: array [0 .. 17] of String = (
        'Flight',
        'Rego',
        'Aircraft Type',
        'Ports',
        'Status',
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        //'On Blocks',
        'ATA',
        'ATA[Date]',
        //'Slot Time',
        'Gate',
        'Bay',
        'Terminal',
        'Belts',
        'Related Flight',
        'Carrier',
        //'Public',
        //'Lateral',
        'Message'
        //'Crawling'
    );

    { Different data types and arrays used in different windows }
    ArrivalSortedFields: array [0 .. 14] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffETime,
        ffATime,
        ffSTdate,
        ffAStatus,
        ffGates,
        ffBays,
        ffBelts,
        ffCarrier,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );

    ArrivalSortedColumns: array [0 .. 14] of String = (
        'Flight',
        'Ports',
        'STA',
        'ETA',
        'ATA',
        'STA[Date]',
        'Status',
        'Gate',
        'Bay',
        'Belts',
        'Carrier',
        'Terminal',
        'Rego',
        'Message',
        'NonPublic'
    );



    TArrivalFields: array [0 .. 8] of aFlightField = (
        ffFlight,
        ffPorts,
      {
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
      }
        ffGates,
        ffBays,
        ffBelts,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );
    TArrivalColumns: array [0 .. 8] of String = (
        'Flight',
        'Ports',
      {
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
      }
        'Gate',
        'Bay',
        'Belts',
        'Terminal',
        'Rego',
        'Message',
        'NonPublic'
    );

    TDeparturesFields: array [0 .. 8] of aFlightField = (
        ffFlight,
        ffPorts,
      {
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
      }
        ffCheckIns,
        ffGates,
        ffBays,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );
    TDeparturesColumns: array [0 .. 8] of String = (
        'Flight',
        'Ports',
      {
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
      }
        'Check-in',
        'Gate',
        'Bay',
        'Terminal',
        'Rego',
        'Message',
        'NonPublic'
    );

    DeparturesFields: array [0 .. 17] of aFlightField = (
        ffFlight,
        ffRego,
        ffAirCraft,
        ffPorts,
        ffDStatus,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        //ffOffBlock,//offblock
        ffATime,
        ffATdate,
        //ffSlotTime,//slottime
        ffGates,
        ffBays,
        ffTerminal,
        //ffRelatedFlight,//relatedflight
        ffCheckIns,
        ffCarrier,
        ffNonPublic,
        //ffScheduledCheckinCounters,//scheduledcheckincounters
        //ffCheckinOpeningTime,   //checkinopeningtime
        //ffCheckinClosingTime,   //checkinclosingtime
        ffComment
        //ffCrawling
    );
    DeparturesColumns: array [0 .. 17] of String = (
        'Flight',
        'Rego',
        'Aircraft Type',
        'Ports',
        'Status',
        'STD',
        'STD[Date]',
        'ETD',
        'ETD[Date]',
        //'Off Blocks',
        'ATD',
        'ATD[Date]',
    //    'Slot Time',
        'Gate',
        'Bay',
        'Terminal',
        //'Related Flight',
        'Check-in',
        'Carrier',
        'Public',
        //'Scheduled Checkin Counters',
        //'Checkin Opening time',
        //'Checkin Closing time',
        'Message'
        //'Crawling'
    );

    DeparturesSortedFields: array [0 .. 14] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffETime,
        ffATime,
        ffSTdate,
        ffDStatus,
        ffCheckIns,
        ffGates,
        ffBays,
        ffCarrier,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );
    DeparturesSortedColumns: array [0 .. 14] of String = (
        'Flight',
        'Ports',
        'STD',
        'ETD',
        'ATD',
        'STD[Date]',
        'Status',
        'Check-in',
        'Gate',
        'Bay',
        'Carrier',
        'Terminal',
        'Rego',
        'Message',
        'NonPublic'
    );

    CheckinsFields: array [0 .. 15] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
        ffDStatus,
        ffCheckIns,
        ffBays,
        ffCarrier,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );
    CheckinsColumns: array [0 .. 15] of String = (
        'Flight',
        'Ports',
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
        'Status',
        'Check-in',
        'Bay',
        'Carrier',
        'Terminal',
        'Rego',
        'Message',
        'Public'
    );

    GatesFields: array [0 .. 14] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
        ffDStatus,
        ffCheckIns,
        ffGates,
        ffBays,
        ffCarrier,
        ffTerminal,
        ffRego
    );
    GatesColumns: array [0 .. 14] of String = (
        'Flight',
        'Ports',
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
        'Status',
        'Check-in',
        'Gate',
        'Bay',
        'Carrier',
        'Terminal',
        'Rego'
    );

    BaysFields: array [0 .. 16] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
        ffAStatus,
        ffGates,
        ffBays,
        ffBelts,
        ffCarrier,
        ffTerminal,
        ffRego,
        ffComment,
        ffNonPublic
    );
    BaysColumns: array [0 .. 16] of String = (
        'Flight',
        'Ports',
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
        'Status',
        'Gate',
        'Bay',
        'Belts',
        'Carrier',
        'Terminal',
        'Rego',
        'Message',
        'Public'
    );
    BeltsFields: array [0 .. 13] of aFlightField = (
        ffFlight,
        ffPorts,
        ffSTime,
        ffSTdate,
        ffETime,
        ffETdate,
        ffATime,
        ffATdate,
        ffAStatus,
        ffBays,
        ffBelts,
        ffCarrier,
        ffTerminal,
        ffRego
    );
    BeltsColumns: array [0 .. 13] of String = (
        'Flight',
        'Ports',
        'STA',
        'STA[Date]',
        'ETA',
        'ETA[Date]',
        'ATA',
        'ATA[Date]',
        'Status',
        'Bay',
        'Belts',
        'Carrier',
        'Terminal',
        'Rego'
    );

const
    { System wide variables or definitions }
    Server = 'FIDSxml';

    { Edit window Date Time Formats }
    fidsGUITimeFormat = 'HH:mm';

    // Date & Time Formats
    fidsGUIDateFormat = 'dd/mm/yyyy';
    fidsXMLDateFormat = 'yyyymmdd';
    fidsGUIDateTimeFormat = 'dd/mm/yyyy hh:mm am/pm';
    fidsXMLDateTimeFormat = 'yyyymmdd hhmmss';
    { Config Paths }
    fidsDisplayConfigPath = 'DisplayConfig|Formats';

    fidsSensorPath = 'Sensors';
    fidsAStatusPath = 'SystemSettings|Strings|StatusA';
    fidsDStatusPath = 'SystemSettings|Strings|StatusD';
    fidsCarrierTypesPath = 'SystemSettings|Strings|CarrierTypes';
    fidsAircraftTypesPath = 'SystemSettings|Strings|Aircraft';
    fidsTerminalsPath = 'SystemConfig|Terminals';
    fidsBaysPath = 'SystemConfig|Bays';
    fidsGatesPath = 'SystemConfig|Gates';
    fidsBeltsPath = 'SystemConfig|Belts';
    fidsCheckinsPath = 'SystemConfig|CheckIns';
    fidsJobNamePath = 'SystemConfig|JobName';
    fidsUsersPath = 'Users';

    { Sensor Specific info }
    fidsSensorHeight = 16;
    fidsSensorsPanelHeight = 25;
    fidsSensorWidth = 24;
    fidsHowManyVertically = 1;

    { Edit window related }
    ComponentTop = 10;
    ComponentLeft = 15;
    ComponentAndButtonsGap = 80;

    { SPLIT Field for gates/checkins/bays/belts/etc }
    fidsSplitField = ',';

    { Paths of icons }
    icoCancelled = 'cancelled.gif';
    icoLanded = 'landed.gif';
    icoDiverted = 'diverted.gif';
    icoOnBlocks = 'onblocks.gif';
    icoBoarding = 'boarding.gif';
    icoFinalCall = 'finalcall.gif';
    icoClosed = 'closed.gif';
    icoDeparted = 'departed.gif';
    icoOffBlocks = 'offblocks.gif';

    { function stdDate2fidsDate(inp: TDate) : String;
      function fidsDate2stdDate(inp: String) : TDate;
      function stdTime2fidsTime(inp: TTime) : String;
      function fidsTime2stdTime(inp: String) : TTime;
    }
procedure ShowBalloonTip(Control: TWinControl; Icon: integer; Title: PWideChar;
  Text: PWideChar; BackCL, TextCL: TColor);
procedure AngleTextOut(ACanvas: TCanvas; Angle, X, Y: integer; Str: string);
procedure RHSMenu(Form: TForm; MenuItem: TMenuItem);
procedure ParseDelimited(const sl: TStrings; const value: string;
  const delimiter: string);
procedure Split(const delimiter: Char; Input: string; const Strings: TStrings);

function MeasureTextLengthInPixels(Text: string; font: TFont): Int32;
function FIDS_TimeTOStr(St: TTime): String;
function FIDS_StrTOTime(St: string): TDateTime;
function FIDS_StrToDT(St: string): TDate;
function FIDS_DtTOStr(St: TDateTime): String;
function StrGetD(c: Char): Int;
function StripNonAlphaNumeric(const AValue: string): string;
function SetPorts(const AValue: string): string;
function IsSensorDown(Str: String): integer;
function CompareNodes(Node1: PTreeData; Node2: PTreeData): Boolean;
function CompareFlightPath(Node1: PTreeData; Node2: PTreeData): Boolean;
function TimeTo12Hour(time: TTime): String;
function TrimPorts(Ports: string): String;
function isEmpty(inp: string): Boolean;
function isDateEmpty(inp: TDate): Boolean;
function IsStrANumber(s: string): Boolean;
function BubbleSort( list: TStringList ): TStringList;

implementation

function BubbleSort( list: TStringList ): TStringList;
var
  i, j: Integer;
  temp: string;
begin

  // bubble sort
  for i := 0 to list.Count - 1 do begin
    for j := 0 to ( list.Count - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = list.Count ) then
        continue;
      if ( StrToInt(list.Strings[j]) > StrToInt(list.Strings[j+1]) ) then begin
        temp              := list.Strings[j];
        list.Strings[j]   := list.Strings[j+1];
        list.Strings[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
  Result := list;
end;


function IsStrANumber(s: string): boolean;
var
    E, I: Integer;
begin
    Val(s, I, E);
    Result := E = 0;
end;


function isEmpty(inp: string): Boolean;
begin
    if (Trim(inp) <> '') then
        Result := FALSE
    else
        Result := TRUE;
end;

function isDateEmpty(inp: TDate): Boolean;
begin

    if (Pos('1899', datetostr(inp)) <> 0) then
        Result := TRUE
    else
        Result := FALSE;

end;

{ Splits an string to an array }
procedure Split(const delimiter: Char; Input: string; const Strings: TStrings);
begin
    Assert(Assigned(Strings));
    Strings.Clear;
    Strings.delimiter := delimiter;
    Strings.DelimitedText := Input;
end;

function TimeTo12Hour(time: TTime): String;
begin
    Result := FormatDateTime('HH:mm', time);
end;

function CompareFlightPath(Node1: PTreeData; Node2: PTreeData): Boolean;
var
    Ret: Boolean;
begin
    Ret := TRUE;

    if not(Node1^.DBPath = Node2^.DBPath) then
        Ret := FALSE;

    Result := Ret;
end;

function CompareNodes(Node1: PTreeData; Node2: PTreeData): Boolean;
var
    Ret: Boolean;
begin
    Ret := TRUE;

    if not(Node1^.Flight = Node2^.Flight) then
        Ret := FALSE;
    if not(Node1^.Ports = Node2^.Ports) then
        Ret := FALSE;
    {
      if not (Node1^.ScheduledTime = Node2^.ScheduledTime) then Ret := false;
      if not (Node1^.EstimatedTime = Node2^.EstimatedTime) then Ret := false;
    }
    if not(Node1^.Status = Node2^.Status) then
        Ret := FALSE;
    if not(Node1^.Gates = Node2^.Gates) then
        Ret := FALSE;
    if not(Node1^.CheckIns = Node2^.CheckIns) then
        Ret := FALSE;
    if not(Node1^.Bays = Node2^.Bays) then
        Ret := FALSE;
    if not(Node1^.Belts = Node2^.Belts) then
        Ret := FALSE;
    if not(Node1^.Carrier = Node2^.Carrier) then
        Ret := FALSE;
    if not(Node1^.Terminal = Node2^.Terminal) then
        Ret := FALSE;
    if not(Node1^.Rego = Node2^.Rego) then
        Ret := FALSE;

    Result := Ret;
end;

procedure ShowBalloonTip(Control: TWinControl; Icon: integer; Title: PWideChar;
  Text: PWideChar; BackCL, TextCL: TColor);
const
    TOOLTIPS_CLASS = 'tooltips_class32';
    TTS_ALWAYSTIP = $01;
    TTS_NOPREFIX = $02;
    TTS_BALLOON = $40;
    TTF_SUBCLASS = $0010;
    TTF_TRANSPARENT = $0100;
    TTF_CENTERTIP = $0002;
    TTM_ADDTOOL = $0400 + 50;
    TTM_SETTITLE = (WM_USER + 32);
    ICC_WIN95_CLASSES = $000000FF;
type
    TOOLINFO = packed record
        cbSize: integer;
        uFlags: integer;
        hwnd: THandle;
        uId: integer;
        rect: TRect;
        hinst: THandle;
        lpszText: PWideChar;
        lpszTitle: PWideChar;
        lParam: integer;
    end;
var
    hWndTip: THandle;
    ti: TOOLINFO;
    hwnd: THandle;
begin
    hwnd := Control.Handle;
    hWndTip := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_NOPREFIX or
      TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, hwnd, 0, HInstance, nil);
    if hWndTip <> 0 then
    begin
        SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or
          SWP_NOMOVE or SWP_NOSIZE);
        ti.cbSize := SizeOf(ti);
        ti.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
        ti.hwnd := hwnd;
        ti.lpszText := Text;
        ti.lpszTitle := Title;
        Windows.GetClientRect(hwnd, ti.rect);
        SendMessage(hWndTip, TTM_SETTIPBKCOLOR, BackCL, 0);
        SendMessage(hWndTip, TTM_SETTIPTEXTCOLOR, TextCL, 0);
        SendMessage(hWndTip, TTM_SETTITLE, Icon mod 4, integer(ti.lpszTitle));
        SendMessage(hWndTip, TTM_ADDTOOL, 1, integer(@ti));
    end;
end;

function TrimPorts(Ports: string): String;
begin
    Result := stringreplace(stringreplace(Ports, ' ', '',
      [rfReplaceAll, rfIgnoreCase]), ',', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

procedure ParseDelimited(const sl: TStrings; const value: string;
  const delimiter: string);
var
    dx: integer;
    ns: string;
    txt: string;
    delta: integer;
begin
    delta := Length(delimiter);
    txt := value + delimiter;
    sl.BeginUpdate;
    sl.Clear;
    try
        while Length(txt) > 0 do
        begin
            dx := Pos(delimiter, txt);
            ns := Copy(txt, 0, dx - 1);
            sl.Insert(0, Trim(ns));
            txt := Copy(txt, dx + delta, MaxInt);
        end;
    finally
        sl.EndUpdate;
    end;
end;

procedure RHSMenu(Form: TForm; MenuItem: TMenuItem);
var
    mii: TMenuItemInfo;
    MainMenu: hMenu;
    Buffer: array [0 .. 79] of Char;
begin
    MainMenu := Form.Menu.Handle;

    // GET Help Menu Item Info
    mii.cbSize := SizeOf(mii);
    mii.fMask := MIIM_TYPE;
    mii.dwTypeData := Buffer;
    mii.cch := SizeOf(Buffer);
    GetMenuItemInfo(MainMenu, MenuItem.Command, FALSE, mii);

    // SET Help Menu Item Info
    mii.fType := mii.fType or MFT_RIGHTJUSTIFY;
    SetMenuItemInfo(MainMenu, MenuItem.Command, FALSE, mii);
end;

procedure AngleTextOut(ACanvas: TCanvas; Angle, X, Y: integer; Str: string);
var
    LogRec: TLogFont;
    OldFontHandle, NewFontHandle: hFont;
begin
    ACanvas.Brush.Color := Application.MainForm.Color;
    ACanvas.FillRect(rect(0, 0, 100, 100));

    GetObject(ACanvas.font.Handle, SizeOf(LogRec), Addr(LogRec));
    LogRec.lfEscapement := Angle * 10;
    NewFontHandle := CreateFontIndirect(LogRec);
    OldFontHandle := SelectObject(ACanvas.Handle, NewFontHandle);
    ACanvas.TextOut(X, Y, Str);
    NewFontHandle := SelectObject(ACanvas.Handle, OldFontHandle);
    DeleteObject(NewFontHandle);
end;

function IsSensorDown(Str: String): integer;
begin
    if Str = 'down' then
        Result := 0
    else
        Result := 1;
end;

function MeasureTextLengthInPixels(Text: string; font: TFont): Int32;
var
    W: integer;
    BM: TBitmap;
begin
    BM := TBitmap.Create;
    BM.Canvas.font := font;
    W := BM.Canvas.TextWidth(Text);
    BM.Free;
    Result := W + 8;
end;

function StripNonAlphaNumeric(const AValue: string): string;
var
    SrcPtr, DestPtr: PChar;
begin
    SrcPtr := PChar(AValue);
    SetLength(Result, Length(AValue));
    DestPtr := PChar(Result);
    while SrcPtr <> #0 do
    begin
        if SrcPtr[0] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9'] then
        begin
            DestPtr[0] := SrcPtr[0];
            Inc(DestPtr);
        end;
        Inc(SrcPtr);
    end;
    SetLength(Result, DestPtr - PChar(Result));
end;

function SetPorts(const AValue: string): string;
var
    SrcPtr, DestPtr: PChar;
begin
    SrcPtr := PChar(AValue);
    SetLength(Result, Length(AValue));
    DestPtr := PChar(Result);
    while SrcPtr <> #0 do
    begin
        if SrcPtr[0] in ['a' .. 'z', 'A' .. 'Z'] then
        begin
            // if (((DestPtr+1) div 3) = 0) then
            // DestPtr[0] := '+';
            DestPtr[0] := SrcPtr[0];
            Inc(DestPtr);
        end;
        Inc(SrcPtr);
    end;
    SetLength(Result, DestPtr - PChar(Result));
end;

function StrGetD(c: Char): Int;
begin
    if (c >= '0') and (c <= '9') then
        Result := Ord(c) - Ord('0')
    else
        raise Exception.Create('Time table invalid decimal (' + c + ')');
end;

function StrGetDD(const St: string; p: Int): Int;
begin
    Result := 0;
    if p < Length(St) then
    begin
        Result := StrGetD(St[p]) * 10 + StrGetD(St[p + 1]);
    end;
end;

function StrGetDDDD(const St: string; p: Int): Int;
begin
    Result := 0;
    if p + 3 <= Length(St) then
    begin
        Result := StrGetDD(St, p) * 100 + StrGetDD(St, p + 2);
    end;
end;

function FIDS_StrToDT(St: string): TDate; // ST
var
    yr, mo, dy: word;
    hr, mn, se: word;
    p: integer;
begin
    Result := 0.0;

    if St <> '' then
    begin
        yr := StrToInt(Copy(St, 7, 4));
        mo := StrToInt(Copy(St, 4, 2));
        dy := StrToInt(Copy(St, 1, 2));

        try
            Result := EncodeDate(yr, mo, dy);
        except
        end;
    end
    else
    begin
        Result := 0;
    end;
end;

function FIDS_DtTOStr(St: TDateTime): String; // ST
var
    yr, mo, dy: word;
    mm, dd: String;
    hr, mn, se, ms: word;
    hour, min, sec, mis: String;
    return: String;
begin

    Result := '';

    DecodeDate(St, yr, mo, dy);
    return := return + IntToStr(yr);
    mm := IntToStr(mo);
    dd := IntToStr(dy);

    if (Length(mm) < 2) then
        return := return + '0' + mm
    else
        return := return + mm;

    if (Length(dd) < 2) then
        return := return + '0' + dd
    else
        return := return + dd;

    Result := return;
end;

function FIDS_StrTOTime(St: string): TDateTime; // ST
var
    hr, mn, se: word;
    p: integer;

begin
    Result := 0.0;
    p := 1;
    if St <> '' then
    begin

        hr := StrGetDD(St, p);
        p := p + 2;
        mn := StrGetDD(St, p);
        // se := StrGetDD (St, p);     //p := p + 2;
        se := 0;
        try
            Result := EncodeTime(hr, mn, se, 0);
        except

        end;
    end;
end;

function FIDS_TimeTOStr(St: TTime): String; // ST
var
    hr, mn, se, ms: word;
    hour, min, sec, mis: String;
    return: String;
begin

    Result := '';

    DecodeTime(St, hr, mn, se, ms);
    hour := IntToStr(hr);
    min := IntToStr(mn);
    sec := IntToStr(se);

    if (Length(hour) < 2) then
        return := return + '0' + hour
    else
        return := return + hour;

    if (Length(min) < 2) then
        return := return + '0' + min
    else
        return := return + min;

    Result := return;
end;

function IntToBin(value: LongInt; digits: integer): string;
begin
    Result := StringOfChar('0', digits);
    while value > 0 do
    begin
        if (value and 1) = 1 then
            Result[digits] := '1';
        dec(digits);
        value := value shr 1;
    end;
end;

function BinToInt(value: String): LongInt;
var
    i: integer;
begin
    Result := 0;
    // remove leading zeroes
    while Copy(value, 1, 1) = '0' do
        value := Copy(value, 2, Length(value) - 1);
    // do the conversion
    for i := Length(value) downto 1 do
        if Copy(value, i, 1) = '1' then
            Result := Result + (1 shl (Length(value) - i));
end;

end.
