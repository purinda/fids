unit uFlight;

// this unit is the sole writer of Arrivals and Departures (.xml)
// it provides all input validation for flights - via timetable, user or feed
// all content must be ready for formatter/DisplayConfig use.
// see http://fishbowl.pastiche.org/2003/05/28/the_ghetto_minipattern/

interface

uses
    Classes, Generics.Collections, Generics.Defaults, uFidsTags, uGlobalDefs,
    uMirrorDB, uDBTree, uGT, Dialogs;

type
    // see also field sets below
    aFlightField = (ffNone, ffSTime, ffSTdate, ffETime, ffETdate,
      ffATime, ffATdate,
      // for split update UI eg separate date and time

      ffFlightKey, ffFlight, ffFeedKey,
      // typically qantas feed db key value eg QF123/100823 no relation to ffST
      ffST, ffET, ffAT, // real fields from here
      ffCorresponding, // ffCorrFeedKey,  arrival/departure pair mapping
      ffDStatus, ffGates, ffBays, ffRego, ffCarrier, ffTerminal, ffAirCraft, ffOffBlock, ffSlotTime, ffRelatedFlight,
      ffScheduledCheckinCounters,ffCheckinOpeningTime,ffCheckinClosingTime,ffOnBlock,ffRaceway,
      ffComment, ffStaffNote, ffStaffComment, ffPax, ffPaxJoin, ffPaxDisembark,
      ffPaxTransit, ffPaxTranship, // Souls/Infants passenger count pairs
      ffPaxWheelChair, ffFuel, ffBurn, ffAStatus, ffBelts, // arrival only
      ffNonPublic, ffCrawling, // boolean
      ffCheckIns, ffPorts); // code share

    aFlightValidationError = (feNone, feNoFlightName, feInvalidScheduledTime,
      feFlightAlreadyExists, feInvalidFlightKind, feNotAllowedToRenameFlight,
      feDateTimeOutOfRange, feInvalidDateTimeFormat);

const // field attribute sets
    // BooleanField = [ffNonPublic];
    BooleanField = [];
    SplitField = [ffSTime, ffSTdate, ffETime, ffETdate, ffATime, ffATdate];
    CodeShareField = [ffCheckIns, ffPorts]; // sub flight fields
    ListField = [ffGates, ffBelts, ffCheckIns, ffPorts];
    // see AddItemToList and FinaliseListUpdates  , ffBays ?
    DateTimeField = [ffST, ffET, ffAT];
    UpperCaseField = [ffFlight, ffET, ffRego, ffTerminal, ffAirCraft, ffPorts];

type
    cFlight = class(TInterfacedObject, IComparer<apNode>)
        // god class warning - todo ?  dynamically add cField
        constructor Create(db: cMirrorDB { or nil }; ReqID: string);
    private
        mDB: cMirrorDB; // allows db update and read
        mReqID: string; // id string for requests
        mDbNode: apNode; // or nil - current db flight name node
        mLog: aLogProc; // reporting
        mVals: array [aFlightField] of string; // for new flight building
        mEr: aFlightValidationError; // error flag
        mSortField: aFlightField;
        mSortAscend: boolean;
        mRangeCheck: boolean;
        mKind: aFlightKind; // could extend to handle time table templates
        procedure LogEr(ErrorNo: integer; const s: string);
        function GetField(field: aFlightField): string;
        function GetDBTagName(field: aFlightField): string;
        function GetTitleField(field: aFlightField): string;
        function GetRawField(field: aFlightField): string;
        procedure SetRawField(field: aFlightField; val: string);
        // function   GetKind : aFlightKind;
        procedure FieldDelta(pfk: apNode; field: aFlightField; val: string);
        procedure FlightDelta(field: aFlightField);
        function FixTime(field, date, time: aFlightField): boolean;
        procedure SetField(field: aFlightField; val: string);
        function GetDbPAth: string;
        procedure SetDbPAth(path: string);
        function GetCodeShareIndex: int;
        function KindPath: string;
        // function   ValidTD( const td : string ) : boolean;
        // function MakeValidTD(var td: string): boolean;
        function MakeValidFlight(var name: string): boolean;
        function MakeValid(field: aFlightField): boolean;
    public
        Template: boolean;
        function FlightExists(pArrDep, pFltKey: apNode): boolean; overload;
        function FlightExists(kind: aFlightKind; const name, st: string)
          : boolean; overload;
        function FlightExists: boolean; overload; // overload - used by New;
        function FlightValid(pArrDep, pFlt: apNode): boolean;
        function FieldID(const field: string): aFlightField; // see uFidsTags
        // function   List( field : aFlightField; const item : string ) : TList< apNode >;   deprecated;
        function Match(field: aFlightField; const item: string): boolean;
        procedure Find(kind: aFlightKind; field: aFlightField;
          const item: string); overload;
        procedure Find(kind: aFlightKind; const name, st: string); overload;
        procedure Clear; // clear all stored values ready for random updates
        procedure AddItemToList(field: aFlightField; const item: string);
        // used by qantas feed
        procedure FinaliseListUpdates;

        // procedure  InitCompare( field : aFlightField; ascend : boolean );
        function Compare(const pfltA, pfltB: apNode): int;
        // IComparer<apNode>  ie can compare flights
        function AddFlightToTree(db: cDbTree; base: apNode; name: string)
          : boolean; // handles template use of flight

        procedure Delete;
        procedure DeleteSub;
        function New: boolean;
        function NewCodeShare(primaryFlight: apNode): boolean;

        property Log: aLogProc read mLog write mLog;
        property Presentation[field: aFlightField]: string read GetField
          write SetField; default; // db Update
        property DBTag[field: aFlightField]: string read GetDBTagName;
        // use var	DBTag [ field ]  below
        property Title[field: aFlightField]: string read GetTitleField;
        property Raw[field: aFlightField]: string read GetRawField
          write SetRawField;

        property DbNode: apNode read mDbNode write mDbNode;
        property DbPath: string read GetDbPAth write SetDbPAth;

        property Error: aFlightValidationError read mEr write mEr;
        property kind: aFlightKind read mKind write mKind;
        property CodeShare: int read GetCodeShareIndex; // 0 => primary
        property RangeCheck: boolean read mRangeCheck write mRangeCheck;
        // turn off date validation
    end;

    cFlightList = class(TList<apNode>)
        constructor Create(db: cMirrorDB);
        destructor Destroy; override;
    private
        mDB: cMirrorDB;
        oFlight: cFlight;
    public
        procedure Build(kind: aFlightKind; field: aFlightField;
          const item: string);
        procedure Sort(const field: aFlightField; ascend: boolean);
        function FlightNamesN: TStringList; // mark repeats as QF 123 (2)  etc
        function GetEnumerator: TEnumerator<apNode>;
        // supports compiler  'for pFlt in FlightList do'
        property Flight: cFlight read oFlight write oFlight;
    end;

function FieldName(pFlt: apNode; ff: aFlightField): string; // a uFidsTags
function MatchPattern(const Inp, Pat: string): boolean;
function TimeField(field: aFlightField): boolean;
function MakeValidTime(var val: string): boolean;
// function   CodeShareField( field : aFlightField ) : boolean;   use  CodeShareField set  ie ( field in CodeShareField )
function KeepEmptyField(field: aFlightField): boolean;

function AirLineCode(flt: string): string; overload;
function AirLineCode(flt: apNode): string; overload;
function SizeSuffix(width, height: int): string;
function GraphicName(const txt: string; db: cMirrorDB): string;

var
    DBTagName: array [aFlightField] of string;

implementation

uses SysUtils, uUtils, uXmlParser;

const
    nLateST = 2.0; // acceptance range for TD  - 2 day ago
    nAdvancedST = 14.0; // in 2 weeks time


    // ________________________ misc ________________________________________________

    { function   CodeShareField( field : aFlightField ) : boolean;      obs

      begin
      result := field >= ffCheckins;   // relies on order in aFlightField
      end; }

function TimeField(field: aFlightField): boolean;

begin
    result := field in [ffSTime, ffSTdate, ffETime, ffETdate, ffATime, ffATdate]
      + DateTimeField;
end;

function DateField(field: aFlightField): boolean;

begin
    result := (field = ffSTdate) or (field = ffATdate) or (field = ffETdate);
end;

function KeepEmptyField(field: aFlightField): boolean;

begin
    result := field <> ffFlight;
end;

function FieldName(pFlt: apNode; ff: aFlightField): string; // a uFidsTags

begin // derrive field name from enumerator name using RTTI
    result := EnumToStr(Ord(ff), System.TypeInfo(aFlightField), false);
    result := UnCamel(result, #0);
end;

function DBFieldName(pFlt: apNode; field: aFlightField): string;

// var
// fk : aFlightKind;
begin // todo splitfields
    { if field = ffStatus then  begin   // status is generic name - adjust for flight type
      fk := GetFlightKind( Back( pflt, 2 ) );
      if fk = afkArrivals then  result := tagAStatus
      else if fk = afkDepartures then  result := tagDStatus;
      end
      else } result := FieldName(pFlt, field);
end;

function IsDecimal(c: char): boolean;

begin
    result := (c >= '0') and (c <= '9');
end;

function IsUpAlpha(c: char): boolean;

begin
    result := (c >= 'A') and (c <= 'Z');
end;

{ ***************************************************************** }
{ * This function implements a subset of regular expression based * }
{ * search and is based on the translation of PattenMatch() API   * }
{ * of common.c in MSDN Samples\VC98\sdk\sdktools\tlist           * }
{ ***************************************************************** }
{ * MetaChars are  :                                              * }
{ *            '*' : Zero or more chars.                          * }
{ *            '?' : Any one char.                                * }
{ *         [adgj] : Individual chars (inclusion).                * }
{ *        [^adgj] : Individual chars (exclusion).                * }
{ *          [a-d] : Range (inclusion).                           * }
{ *         [^a-d] : Range (exclusion).                           * }
{ *       [a-dg-j] : Multiple ranges (inclusion).                 * }
{ *      [^a-dg-j] : Multiple ranges (exclusion).                 * }
{ *  [ad-fhjnv-xz] : Mix of range & individual chars (inclusion). * }
{ * [^ad-fhjnv-xz] : Mix of range & individual chars (exclusion). * }
// GT extensions - hides some single unicode character tests
// — : any decimal
// √ : any uppercase alpha
{ ***************************************************************** }
// http://www.delphi3000.com/articles/article_1561.asp?SK=
// Paramjeet Reen   - adjusted for unicode delphi and —, √

function MatchPattern(const Inp, Pat: string): boolean;

var
    InpStr, Pattern: PChar;
begin
    InpStr := PChar(Inp);
    Pattern := PChar(Pat);
    result := false;
    while (True) do
    begin
        case Pattern[0] of
            #0:
                begin // End of pattern reached.
                    result := (InpStr[0] = #0); // TRUE if end of InpStr.
                    Exit;
                end;

            '*':
                begin // Match zero or more occurances of any char.
                    if (Pattern[1] = #0) then
                    begin // Match any number of trailing chars.
                        result := True;
                        Exit;
                    end
                    else
                        Inc(Pattern);

                    while (InpStr[0] <> #0) do
                    begin // Try to match any substring of InpStr.
                        if (MatchPattern(InpStr, Pattern)) then
                        begin
                            result := True;
                            Exit;
                        end;

                        // Continue testing next char...
                        Inc(InpStr);
                    end;
                end;

            '?':
                begin // Match any one char.
                    if (InpStr[0] = #0) then
                    begin
                        result := false;
                        Exit;
                    end;

                    // Continue testing next char...
                    Inc(InpStr);
                    Inc(Pattern);
                end;

            '—':
                begin
                    if not IsDecimal(InpStr[0]) then
                    begin
                        result := false;
                        Exit;
                    end
                    else
                    begin
                        Inc(InpStr);
                        Inc(Pattern);
                    end;
                end;
            '√':
                begin
                    if not IsUpAlpha(InpStr[0]) then
                    begin
                        result := false;
                        Exit;
                    end
                    else
                    begin
                        Inc(InpStr);
                        Inc(Pattern);
                    end;
                end;

            '[':
                begin // Match given set of chars.
                    if CharInSet(Pattern[1], [#0, '[', ']']) then
                    // if(Pattern[1] in [#0,'[',']']) then
                    begin // Invalid Set - So no match.
                        result := false;
                        Exit;
                    end;

                    if (Pattern[1] = '^') then
                    begin // Match for exclusion of given set...
                        Inc(Pattern, 2);
                        result := True;
                        while (Pattern[0] <> ']') do
                        begin
                            if (Pattern[1] = '-') then
                            begin // Match char exclusion range.
                                if (InpStr[0] >= Pattern[0]) and
                                  (InpStr[0] <= Pattern[2]) then
                                begin // Given char failed set exclusion range.
                                    result := false;
                                    Break;
                                end
                                else
                                    Inc(Pattern, 3);
                            end
                            else
                            begin // Match individual char exclusion.
                                if (InpStr[0] = Pattern[0]) then
                                begin // Given char failed set element exclusion.
                                    result := false;
                                    Break;
                                end
                                else
                                    Inc(Pattern);
                            end;
                        end;
                    end
                    else
                    begin // Match for inclusion of given set...
                        Inc(Pattern);
                        result := false;
                        while (Pattern[0] <> ']') do
                        begin
                            if (Pattern[1] = '-') then
                            begin // Match char inclusion range.
                                if (InpStr[0] >= Pattern[0]) and
                                  (InpStr[0] <= Pattern[2]) then
                                begin // Given char matched set range inclusion. Continue testing...
                                    result := True;
                                    Break;
                                end
                                else
                                    Inc(Pattern, 3);
                            end
                            else
                            begin // Match individual char inclusion.
                                if (InpStr[0] = Pattern[0]) then
                                begin // Given char matched set element inclusion. Continue testing...
                                    result := True;
                                    Break;
                                end
                                else
                                    Inc(Pattern);
                            end;
                        end;
                    end;

                    if (result) then
                    begin // Match was found. Continue further.
                        Inc(InpStr);

                        // Position Pattern to char after "]"
                        while (Pattern[0] <> ']') and (Pattern[0] <> #0) do
                            Inc(Pattern);

                        if (Pattern[0] = #0) then
                        begin // Invalid Pattern - missing "]"
                            result := false;
                            Exit;
                        end
                        else
                            Inc(Pattern);
                    end
                    else
                        Exit;
                end;

        else
            begin // Match given single char.
                if (InpStr[0] <> Pattern[0]) then
                begin
                    result := false;
                    Break;
                end;

                // Continue testing next char...
                Inc(InpStr);
                Inc(Pattern);
            end;
        end;
    end;
end;

function MakeValidPorts(var val: string): boolean;

begin
    val := UpperCase(val);
    result := True; // todo valid list of √√√
end;

function MakeValidTime(var val: string): boolean;

begin
    if Length(val) = 3 then
        Insert('0', val, 1);
    result := MatchPattern(val, '————');
end;


// ____________________________ cFlight _________________________________________

constructor cFlight.Create(db: cMirrorDB { or nil }; ReqID: string);

begin
    mDB := db; // allows db updates
    mReqID := ReqID;
    mRangeCheck := True;
end;

function cFlight.MakeValidFlight(var name: string): boolean;

var
    i: int;
begin
    for i := Length(name) downto 1 do
    begin
        if name[i] <= ' ' then
            System.Delete(name, i, 1);
    end;
    name := UpperCase(name);
    result := name <> '';
end;
{
  function cFlight.MakeValidTD(var td: string): boolean;

  var
  t: TDateTime;
  // s : string;
  // i : int;
  begin
  result := True;
  LeaveOnly('0123456789', td); // strip out any '/- ' etc
  if Length(td) = 12 then
  td := td + '00';
  Insert(' ', td, 9);

  if not MatchPattern(td, '———————— ——————') then
  begin
  result := false;
  Error := feInvalidDateTimeFormat;
  end;
  if result then
  begin
  t := StrToDT(td);
  if mRangeCheck then
  begin
  if (t < Now() - nLateST) or (t > Now() + nAdvancedST) then
  begin
  result := false;
  Error := feDateTimeOutOfRange;
  end;
  end
  else
  begin
  if (t < 30000) or (t > 50000) then
  result := false; // realy wrong date
  end;
  end;
  end;
}

function cFlight.MakeValid(field: aFlightField): boolean;

begin // convert from shorthand / presentation form to full form
    mVals[field] := Trim(mVals[field]);
    if field in UpperCaseField then
        mVals[field] := UpperCase(mVals[field]);
    if field = ffFlight then
        result := MakeValidFlight(mVals[field])
        { [PG] Commented out as I never could actually update date time (ET,ST,AT) correctly till I comment this out
          else if field in DateTimeField then
          begin
          if field = ffET then
          begin

          if mVals[field] = 'TBA' then
          begin
          result := True;
          Exit; // watch out
          end;
          end;
          result := MakeValidTD(mVals[field])
          end
        }
    else if field = ffPorts then
        result := MakeValidPorts(mVals[field])
    else
        result := True; // todo  user is always right ?
end;

procedure cFlight.FieldDelta(pfk: apNode; field: aFlightField; val: string);
// low level - pfk might be flight or sub-flight

var
    pt: apNode;
    r, name: string;
begin
    if pfk <> nil then
    begin
        name := GetDBTagName(field);
        pt := FindName(pfk, name);
        if pt <> nil then
        begin // already exists
            if field in BooleanField then
            begin
                if val <> '1' then
                begin
                    r := FormatDelete(ResolvePathStr(pt), mReqID);
                    mDB.SendRequest(r, True);
                end;
            end
            else
            begin
                r := FormatEditRequest(ResolvePathStr(pt), val,
                  pt.Content, mReqID);
                mDB.SendRequest(r, True);
            end;
        end
        else
        begin
            if field in BooleanField then
            begin
                if val = '1' then
                begin
                    StartRequestNew(name);
                    r := EndRequestNew(ResolvePathStr(pfk), '', '', mReqID);
                    mDB.SendRequest(r, True);
                end;
            end
            else
            begin // need to make a new one
                StartRequestNew(name);
                r := EndRequestNew(ResolvePathStr(pfk), '',
                  mVals[field], mReqID);
                mDB.SendRequest(r, True);
            end;
        end;
    end
end;

procedure cFlight.FlightDelta(field: aFlightField);

var // returns request string for a field update
    pfk: apNode;
begin
    pfk := Back(mDbNode, 2);
    if (pfk <> nil) and (mDbNode <> nil) and not(field in SplitField) then
    begin
        if field = ffFlight then
            Error := feNotAllowedToRenameFlight
            // not allowed to modify flight name
        else if field in CodeShareField then
        begin // sub flight / code shares
            FieldDelta(mDbNode, field, mVals[field]);
        end
        else
        begin // a base field ( flight key field )
            FieldDelta(pfk, field, mVals[field]);
        end
    end;
end;

function cFlight.AddFlightToTree(db: cDbTree; base: apNode; name: string)
  : boolean; // handles template use of flight

var
    pParent, pFltKey, psubflt, p: apNode;
    field: aFlightField;
begin
    result := false;
    pFltKey := base; // f.NewNode( name + '-', false, nil );  // flight key
    p := db.NewNode(tagFlights, false, pFltKey);
    psubflt := db.NewNode(name, false, p); // primary subflight codeshare

    for field := Succ(ffFlight) to High(aFlightField) do
    begin // build flight 1 field at a time
        if mVals[field] <> '' then
        begin // ignore blank fields
            if field in CodeShareField then
                pParent := psubflt
            else
                pParent := pFltKey;
            if field in BooleanField then
            begin
                if mVals[field] = '1' then
                begin // empty node if true - none if false
                    db.NewNode(DBTag[field], false, pParent);
                end;
            end
            else
            begin
                p := db.NewNode(DBTag[field], false, pParent);
                p.Content := mVals[field];
            end;
        end;
    end;
    if psubflt <> nil then
        result := True;
end;

procedure cFlight.Delete;

var
    pf: apNode;
    r: string;
begin
    if (mDB <> nil) and (mDbNode <> nil) then
    begin
        pf := Back(mDbNode, 1);
        if NodeName(pf) = tagFlights then
        begin
            LogEr(0, 'DELETE ' + Presentation[ffFlight]);
            r := FormatDelete(ResolvePathStr(Back(pf, 1)), mReqID);
            if r <> '' then
                mDB.SendRequest(r, True);
            mDbNode := nil;
        end;
    end;
end;

procedure  cFlight.DeleteSub;

     var
         pf : apNode;
         r : string;
     begin
     if ( mDB <> nil ) and ( mDbNode <> nil ) then  begin
         pf := Back( mDbNode, 1 );
         if NodeName( pf ) = tagFlights then  begin
             LogEr( 0, 'DELETE SUB ' + Presentation[ ffFlight ] );
             r := FormatDelete( ResolvePathStr( mDbNode ), mReqID );
             if r <> ''  then  mDB.SendRequest( r, true );
             mDbNode := nil;
             end;
         end;
     end;


function cFlight.KindPath: string;

begin
    result := '';
    if mKind <= High(aFlightKind) then
        result := tagFlightKind[mKind];
end;

function cFlight.New: boolean;

var
    pFltKey: apNode;
    path, name, st: string;
    f: cDbTree;
label
    DontBother;
begin

    result := false;
    f := nil;

    if mVals[ffFlight] = '' then
    begin
        Error := feNoFlightName;
        goto DontBother;
    end;

    if not MakeValid(ffST) then
    begin
        Error := feInvalidScheduledTime;
        goto DontBother;
    end;

    if FlightExists then
    begin
        Error := feFlightAlreadyExists;
        goto DontBother;
    end;

    if KindPath = '' then
    begin
        Error := feInvalidFlightKind;
        goto DontBother;
    end;
    st := mVals[ffST];
    name := mVals[ffFlight];
    { if (name <> '') and (st <> '') and not FlightExists then //original }
    if (name <> '') and not FlightExists then // PG: Modified
    begin // required fields
        f := cDbTree.Create;
        pFltKey := f.NewNode(name + '-', false, nil); // flight key

        if AddFlightToTree(f, pFltKey, mVals[ffFlight]) then
        begin

            path := KindPath;
            if path <> '' then
            begin
                StartRequestNew(pFltKey.NodeName);
                AddToRequestNew(FormatAllSubNodes(pFltKey, 2));
                mDB.SendRequest(EndRequestNew(path, '', '', mReqID), True);
                result := True;
            end;
        end;
    end;

DontBother:
    FreeAndNil(f);
end;

function cFlight.NewCodeShare(primaryFlight: apNode): boolean;

var
    psubflt, pFlt, p: apNode;
    name: string;
    f: cDbTree;
    field: aFlightField;
    x: int;
label
    DontBother;
begin
    result := false;
    name := mVals[ffFlight];
    if name = '' then
        goto DontBother; // required field
    p := primaryFlight.Back;
    if NodeName(p) <> tagFlights then
        goto DontBother; // in the wrong place
    x := -1;
    while EachSubNode(p, x, pFlt) do
    begin
        if NodeName(pFlt) = name then
            goto DontBother; // name is not unique
    end;

    f := cDbTree.Create;
    psubflt := f.NewNode(name, false, f.GetRoot); // built a subflight tree

    for field := Succ(ffFlight) to High(aFlightField) do
    begin // add subflight fields
        if (field in CodeShareField) and (mVals[field] <> '') then
        begin // ignore blank fields
            p := f.NewNode(DBTag[field], false, psubflt);
            p.Content := mVals[field];
        end;
    end;
    if psubflt <> nil then
    begin // send new request
        // LogEr( 0, 'NEW CODE SHARE ' + Presentation[ ffFlight ] );
        StartRequestNew(psubflt.NodeName);
        AddToRequestNew(FormatAllSubNodes(psubflt, 2));
        mDB.SendRequest(EndRequestNew(ResolvePathStr(Back(primaryFlight, 1)),
          '', '', mReqID), True);
        result := True;
    end;
    f.Free;

DontBother:
end;

function cFlight.GetCodeShareIndex: int; // 0 => primary

var
    pt: apNode;
begin
    result := 0;
    pt := Back(mDbNode, 1);
    if (pt <> nil) and (pt.SubNodes <> nil) then
    begin
        result := pt.SubNodes.IndexOf(mDbNode);
    end;
end;

function cFlight.GetField(field: aFlightField): string; // presentation style

var
    s, name: string;
    pfk: apNode;
begin

    result := mVals[field];
    if mDbNode <> nil then
    begin
        name := GetDBTagName(field); // FieldName( pflt, field );
        pfk := Back(mDbNode, 2);
        { if ( pfk <> nil ) and ( field <> ffNone ) and ( name <> '' ) then  begin }
        // Original
        if (pfk <> nil) and (field <> ffNone) then
        begin
            if field = ffFlight then
                result := Gap(NodeName(mDbNode))
            else if field in CodeShareField then
            begin // sub flight / code shares
                result := ReadContent(mDbNode, name);
            end
            else if field in [ffSTime, ffST] then
            begin
                s := ReadContent(pfk, 'ST');
                result := s;

                if (field = ffSTime) then
                    result := Copy(s, 10, 4);

            end
            else if field in [ffETime, ffET] then
            begin
                s := ReadContent(pfk, 'ET');
                result := s;

                if (field = ffETime) then
                    result := Copy(s, 10, 4);

            end
            else if field in [ffATime, ffAT] then
            begin
                s := ReadContent(pfk, 'AT');
                result := s;

                if (field = ffETime) then
                    result := Copy(s, 10, 4);

            end
            else if field in [ffETdate, ffSTdate, ffATdate] then
            begin
                if (field = ffSTdate) then
                    s := ReadContent(pfk, 'ST');
                if (field = ffETdate) then
                    s := ReadContent(pfk, 'ET');
                if (field = ffATdate) then
                    s := ReadContent(pfk, 'AT');

                if Length(s) >= 15 then
                begin
                    result := Copy(s, 1, 8); // can be 'TBA' etc
                    { Insert( '/', result, 7 );
                      Insert( '/', result, 5 );
                    }
                end;
            end
            else if DateField(field) then
            begin
                name := Copy(name, 1, 2); // eg chop STdate down to ST
                s := ReadContent(pfk, name);
                result := Copy(s, 1, 8);
            end
            else
                result := ReadContent(pfk, name);
        end;
    end;
end;

function cFlight.FixTime(field, date, time: aFlightField): boolean;

begin // make a composite date/time from split field pairs
    result := false;
    if (mVals[date] <> '') and (mVals[time] <> '') then
    begin // got the whole set
        mVals[field] := mVals[date] + ' ' + mVals[time];
        // ShowMessage(mVals[field]);
        if MakeValid(field) then
        begin
            result := True;
        end;
    end;
end;

procedure cFlight.SetField(field: aFlightField; val: string);
// does DB update if  mDbNode set
begin
    if field in SplitField then
    begin // reassemble split fields here
        mVals[field] := val;

        if (field = ffETime) and (UpperCase(val) = 'TBA') then
        begin
            val := 'TBA';
            field := ffET;
        end
        else

            case field of
                ffSTime, ffSTdate:
                    if FixTime(ffST, ffSTdate, ffSTime) then
                    begin
                        // ShowMessage('SetField: ' + mVals[ffST] + ' - ' + val);
                        field := ffST;
                        val := mVals[ffST];
                    end;
                ffETime, ffETdate:
                    if FixTime(ffET, ffETdate, ffETime) then
                    begin
                        field := ffET;
                        val := mVals[ffET];
                    end;
                ffATime, ffATdate:
                    if FixTime(ffAT, ffATdate, ffATime) then
                    begin
                        field := ffAT;
                        val := mVals[ffAT];
                    end;
            end;
    end;

    if not(field in SplitField) then
    begin
        if val <> Presentation[field] then
        begin // new field value maybe
            mVals[field] := val;
            if MakeValid(field) then
            begin
                if val <> Raw[field] then
                begin // added to avoid redundant time updates  (see PG 9 Jun 2010)
                    if (mDB <> nil) and (mDbNode <> nil) then
                    begin
                        FlightDelta(field);
                    end;
                end;
            end;
        end
        else
            mVals[field] := val;
    end;
end;

procedure cFlight.SetRawField(field: aFlightField; val: string);
begin
    if val <> Presentation[field] then
    begin // new field value maybe
        // ShowMessage('0-'+val);
        mVals[field] := val;
        if MakeValid(field) then
        begin
            // ShowMessage('1-'+val);
            if val <> Raw[field] then
            begin // added to avoid redundant time updates  (see PG 9 Jun 2010)
                if (mDB <> nil) and (mDbNode <> nil) then
                begin
                    // ShowMessage('2-'+val);
                    FlightDelta(field);
                end;
            end;
        end;
    end
    else
    begin
        mVals[field] := val;
        // ShowMessage('3-'+val);
    end;

end;

function cFlight.GetRawField(field: aFlightField): string;

var
    name: string;
    pfk: apNode;
begin
    result := mVals[field];
    if mDbNode <> nil then
    begin
        name := GetDBTagName(field); // FieldName( pflt, field );
        pfk := Back(mDbNode, 2);
        if (pfk <> nil) and (field <> ffNone) and (name <> '') then
        begin
            if field = ffFlight then
                result := NodeName(mDbNode)
            else if field in CodeShareField then
            begin // sub flight / code shares
                result := ReadContent(mDbNode, name);
            end
            else
                result := ReadContent(pfk, name);
        end;
    end;
end;

function cFlight.GetDBTagName(field: aFlightField): string;

begin
    case field of
        ffFlight:
            begin
                if mDbNode = nil then
                begin
                    result := mVals[ffFlight];
                    if not MakeValid(ffFlight) then
                        result := '';
                end
                else
                    result := NodeName(mDbNode);
            end;
    else
        begin
            result := DBTagName[field]; // faster than RTTI
        end;
    end;
end;

function cFlight.FieldID(const field: string): aFlightField; // see uFidsTags
var
    s: string;
begin // reverse search for matching enum
    result := High(result);
    while result > ffNone do
    begin
        s := EnumToStr(Ord(result), System.TypeInfo(aFlightField), false);
        System.Delete(s, 1, 2);
        if SameStr(field, s) then
            Break;
        Dec(result);
    end;
end;

procedure cFlight.Clear; // clear all stored values ready for new flight updates

var
    field: aFlightField;
begin
    for field := Low(aFlightField) to High(aFlightField) do
    begin
        mVals[field] := '';
    end;
end;

procedure cFlight.AddItemToList(field: aFlightField; const item: string);

begin // comma separated list builder for list fields
    if field in ListField then
    begin
        if (item <> '') and (mVals[field] <> '') then
            mVals[field] := mVals[field] + ',' + item
        else
            mVals[field] := item;
    end;
end;

procedure cFlight.FinaliseListUpdates;

var
    field: aFlightField;
    val: string;
begin
    for field := Succ(ffFlight) to High(aFlightField) do
    begin
        if (field in ListField) and (mVals[field] <> '') then
        begin
            val := mVals[field]; // fiddle due to double use of mVals storage
            mVals[field] := '';
            SetField(field, val);
        end;
    end;
end;

function cFlight.Match(field: aFlightField; const item: string): boolean;

var
    x: int;
    vl: TStringList;
    val: string;
begin
    result := field = ffNone; // match all if no field specified
    if not result then
    begin // else check specifics
        if item = '' then
            result := True
        else
        begin
            if mDbNode <> nil then
            begin
                val := Raw[field];
                if Pos(',', val) > 0 then
                begin // a list
                    x := 1;
                    vl := BuildParamsL(val, x);
                    if vl.IndexOf(item) >= 0 then
                        result := True;
                end
                else if val = item then
                    result := True;
            end;
        end;
    end;
end;

procedure cFlight.Find(kind: aFlightKind; field: aFlightField;
  const item: string); // overload;

var // set node to first matching flight
    x, f: int;
    pfkey, pf, pFlt: apNode;
    base: string;
    found: boolean;
begin
    mKind := kind;
    found := false;
    base := tagFlightKind[kind];
    x := -1;
    while EachSubNode(FindName(mDB.GetRoot, base), x, pfkey) do
    begin // each flight key
        f := -1;
        pf := FindName(pfkey, tagFlights);
        while EachSubNode(pf, f, pFlt) do
        begin // each code share
            DbNode := pFlt; // connect to db flight
            if Match(field, item) then
            begin
                found := True;
                Break;
            end;
        end;
        if found then
            Break;
    end;
    if not found then
        DbNode := nil;
end;

procedure cFlight.Find(kind: aFlightKind; const name, st: string); // overload;

var // set node to first matching flight  typically  Find( afkDepartures,  'QF 123', '20100605 1123' )
    x, f: int;
    pfkey, pf, pFlt: apNode;
    base, sDate: string;
    found: boolean;
begin
    mKind := kind;
    found := false;
    base := tagFlightKind[kind];
    sDate := Copy(st, 1, 8); // date part only
    // if Length( sDate ) < 8 then  sDate := '20' + sDate;  // handle qantas feed sort date
    x := -1;
    while EachSubNode(FindName(mDB.GetRoot, base), x, pfkey) do
    begin // each flight key
        f := -1;
        pf := FindName(pfkey, tagFlights);
        while EachSubNode(pf, f, pFlt) do
        begin // each code share
            DbNode := pFlt; // connect to db flight
            if Match(ffFlight, name) then
            begin // and Match( ffST, ST )
                if Copy(ReadContent(pfkey, DBTag[ffST]), 1, 8) = sDate then
                begin
                    if Length(st) = 8 then
                    begin // qantas sheduled day only match
                        found := True;
                        Break;
                    end
                    else if ReadContent(pfkey, DBTag[ffST]) = st then
                    begin // complete date/time match
                        found := True;
                        Break;
                    end;
                end;
            end;
        end;
        if found then
            Break;
    end;
    if not found then
        DbNode := nil;
end;

function cFlight.GetTitleField(field: aFlightField): string;

// var
// fk : aFlightKind;
begin
    result := EnumToStr(Ord(field), System.TypeInfo(aFlightField));
    case field of
        ffST, ffSTime:
            result := 'S T';
        ffET, ffETime:
            result := 'E T';
        ffAT, ffATime:
            result := 'A T';

        ffDStatus, ffAStatus:
            result := 'Status';
    end;
    if TimeField(field) then
    begin
        // fk := GetKind;
        if mKind = fkArrivals then
            result := result + ' A' // eg E T A
        else if mKind = fkDepartures then
            result := result + ' D';
    end;
end;

function cFlight.GetDbPAth: string;

begin
    result := ResolvePathStr(mDbNode);
end;

procedure cFlight.SetDbPAth(path: string);

begin
    mDbNode := FollowPath(path, mDB.GetRoot);
end;

procedure cFlight.LogEr(ErrorNo: integer; const s: string);

begin
    // if ErrorNo > mEr then  begin
    // mEr := ErrorNo;
    // if Assigned( mLog ) then  mLog( ErrorNo, s );
    // end;
end;

function cFlight.FlightExists(kind: aFlightKind; const name, st: string)
  : boolean; // overload - used by New;

var // requires Kind to be set
    pArrDep, pf, pfkey, pFlt: apNode;
    x, f: int;
begin
    result := false;

    pArrDep := mDB.GetNode(tagFlightKind[kind]);
    x := -1;
    while EachSubNode(pArrDep, x, pfkey) do
    begin // search all base flights
        if ReadContent(pfkey, tagST) = st then
        begin // matching scheduled time so worth checking the names
            f := -1;
            pf := FindName(pfkey, tagFlights);
            while EachSubNode(pf, f, pFlt) do
            begin // and each code share
                if NodeName(pFlt) = name then
                begin // and matching flight name - we have a winner
                    result := True;
                    Break;
                end;
            end;
        end;
    end;
end;

function cFlight.FlightExists(pArrDep, pFltKey: apNode): boolean;
// pFlt not conventional - is flight key

var // typically pArrDep is either arrivals or departures,  used by timetable
    pf, pST, pST2: apNode;
    x: int;
begin
    result := false;
    pST := FindName(pFltKey, tagST);
    if pST <> nil then
    begin // does the flight have an STD
        x := -1;
        while EachSubNode(pArrDep, x, pf) do
        begin // search all flights
            if Pos(pFltKey.NodeName, pf.NodeName) = 1 then
            begin // first part of flight name matches
                pST2 := FindName(pf, tagST);
                if (pST2 <> nil) and (pST2.Content = pST.Content) then
                begin // and STD matches
                    result := True;
                    Break;
                end;
            end;
        end;
    end;
end;

function cFlight.FlightExists: boolean; // overload - used by New;

var // requires Kind to be set
    pArrDep, pfkey: apNode;
    x: int;
    tag: string;
begin
    result := false;
    if mDbNode = nil then
    begin
        if mVals[ffFeedKey] <> '' then
        begin // match (qantas db) external key
            pArrDep := mDB.GetNode(KindPath);
            x := -1;
            tag := DBTag[ffFeedKey];
            while EachSubNode(pArrDep, x, pfkey) do
            begin // search all base flights
                if ReadContent(pfkey, tag) = mVals[ffFeedKey] then
                begin // matching key so exists
                    result := True;
                    Break;
                end;
            end;
        end
        else
            result := FlightExists(mKind, mVals[ffFlight], mVals[ffST]);
    end;
end;

function cFlight.FlightValid(pArrDep, pFlt: apNode): boolean;

begin
    result := True; // todo
end;

function cFlight.Compare(const pfltA, pfltB: apNode): int; // for sort  -1,0,+1

var // work out how to compare any field
    vala, valb, x: integer;
    name, str: string;
begin // return +1 if pap > pbp
    result := 0; // todo handle specific field values  esp ST/ET
    if (pfltA <> nil) and (pfltA <> nil) then
    begin
        if mSortField in DateTimeField then
        begin // raw db string compare
            name := DBFieldName(pfltA, mSortField);
            result := CompareText(ReadContent(Back(pfltA, 2), name),
              ReadContent(Back(pfltB, 2), name));
        end
        else
        begin
            if mSortField in [ffGates, ffBays, ffCheckIns] then
            begin // numeric ?  todo sort by place in resource list ?
                mDbNode := pfltA;
                x := 1;
                vala := GetInt(Presentation[mSortField], x);
                mDbNode := pfltB;
                x := 1;
                valb := GetInt(Presentation[mSortField], x);
                if vala > valb then
                    result := 1
                else if vala < valb then
                    result := -1;
            end
            else
            begin // just Presentation string sort
                mDbNode := pfltA;
                str := Presentation[mSortField];
                mDbNode := pfltB;
                result := CompareText(str, Presentation[mSortField]);
            end;
        end;

        if not mSortAscend then
            result := result * -1;
    end;
end;



// ______________________________ cFlightList __________________________________

type
    cFlightListEnum = class(TEnumerator<apNode>)
        constructor Create(fl: cFlightList);
        // is destroyed at end enumeration by compiler
        // destructor    Destroy;  override;
    private
        mIndex: int;
        mFlightList: cFlightList;
    protected
        function DoGetCurrent: apNode; override;
        function DoMoveNext: boolean; override;
    end;

constructor cFlightListEnum.Create(fl: cFlightList);

begin
    mFlightList := fl;
    mIndex := 0;
end;


// destructor  cFlightListEnum.Destroy;  // override;
//
// begin
// mFlightList.mIndex := mIndex;
// end;

function cFlightListEnum.DoGetCurrent: apNode; // override;

begin
    result := mFlightList[mIndex];
    Inc(mIndex);
    mFlightList.oFlight.mDbNode := result;
end;

function cFlightListEnum.DoMoveNext: boolean; // override;

begin
    if mIndex < mFlightList.Count then
        result := True
    else
        result := false;
end;

constructor cFlightList.Create(db: cMirrorDB);

begin
    inherited Create;
    mDB := db;
end;

destructor cFlightList.Destroy;

begin
    FreeAndNil(oFlight);
    inherited
end;

procedure cFlightList.Build(kind: aFlightKind; field: aFlightField;
  const item: string);

var
    x, f: int;
    pfkey, pf, pFlt, pbase: apNode;
    base: string;
begin
    if oFlight = nil then
        oFlight := cFlight.Create(mDB, '');
    oFlight.kind := kind;
    Clear;
    base := tagFlightKind[kind];
    x := -1;
    pbase := FindName(mDB.GetRoot, base);
    while EachSubNode(pbase, x, pfkey) do
    begin // each flight key
        f := -1;
        pf := FindName(pfkey, tagFlights);
        while EachSubNode(pf, f, pFlt) do
        begin // each code share
            oFlight.DbNode := pFlt; // connect to db flight
            if oFlight.Match(field, item) then
                Add(pFlt);
        end;
    end;
end;

procedure cFlightList.Sort(const field: aFlightField; ascend: boolean);

begin
    if oFlight <> nil then
    begin
        oFlight.mSortField := field;
        oFlight.mSortAscend := ascend;
        inherited Sort(oFlight);
    end;
end;
{
  USAGE :-
  var	flights : cFlightList;

  flights := cFlightList.Create( mContext.Data );   // set mirrorDB to use
  flights.Build( afkDepartures, ffNone, '' );  // list all departures
  flights.Sort( ffFlight, true );              // sort by flight name
  flights.Free;
}

function cFlightList.FlightNamesN: TStringList;
// mark repeats as QF 123 (2)  etc

var
    x, rpt, fl: int;
    fn: string;
    flightNames: TStringList;
begin
    result := TStringList.Create;
    flightNames := TStringList.Create;
    for x := 0 to Count - 1 do
    begin
        oFlight.DbNode := Items[x];
        fn := oFlight.Presentation[ffFlight];
        rpt := 1;
        for fl := 0 to flightNames.Count - 1 do
        begin // count repeats of this flight name
            if flightNames[fl] = fn then
                Inc(rpt);
        end;
        flightNames.Add(fn);
        if rpt = 1 then
            result.Add(fn)
        else
            result.Add(fn + ' (' + IntToStr(rpt) + ')');
        // allows later correlation of name -> path
    end;
    FreeAndNil(flightNames);
end;

function cFlightList.GetEnumerator: TEnumerator<apNode>;

begin
    result := cFlightListEnum.Create(self);
end;


// ______________________________ other _________________________________________

function AirLineCode(flt: string): string;

var // typically put a space into flight name before numeric part
    x: int;
begin
    result := '';
    if flt <> '' then
    begin
        result := Copy(flt, 1, 2);
        for x := 3 to Length(flt) do
        begin
            if not IsAlpha(flt[x]) then
            begin
                Break;
            end;
            result := result + flt[x];
        end;
    end;
end;

function AirLineCode(flt: apNode): string;

begin
    result := '';
    if flt <> nil then
    begin
        result := AirLineCode(flt.NodeName);
    end;
end;

function SizeSuffix(width, height: int): string;

begin
    result := IntToStr(width) + 'x' + IntToStr(height);
end;

function GraphicName(const txt: string; db: cMirrorDB): string;

var
    pt: apNode;
    n, lo, hi, x: int;
    // id, siz : string;
begin
    result := '';
    if Length(txt) >= 2 then
    begin
        result := AirLineCode(txt);
        if result <> '' then
        begin
            x := Length(result) + 1; // get numeric part of flight name
            n := GetInt(txt, x);
            pt := db.GetNode('|SystemSettings|Logo|' + result + '|Range|');
            if pt <> nil then
            begin
                x := 1;
                lo := GetInt(pt.Content, x);
                SkipSpace(pt.Content, x);
                if pt.Content[x] = '-' then
                begin
                    Inc(x);
                    hi := GetInt(pt.Content, x);
                    if (n >= lo) and (n <= hi) then
                    begin
                        pt := FindName(pt, 'Use');
                        if pt <> nil then
                            result := pt.Content;
                    end;
                end;
            end;
        end;
    end;
end;

var
    field: aFlightField;

initialization

// build DBTag[  names for flight fields table
for field := Succ( Low(aFlightField)) to High(aFlightField) do
begin
    if not(field in SplitField) then
    begin // splits aren't real fields
        if (field <> ffFlight) and (field <> ffFlightKey) then
        begin // leave blank
            DBTagName[field] := EnumToStr(Ord(field),
              System.TypeInfo(aFlightField), false); // was FieldName
            DBTagName[field] := UnCamel(DBTagName[field], #0); // no spaces
        end;
    end;
end;

{

  function   FlightValid( pArrDep, pFlt : apNode ) : boolean;

  var
  STD : string;
  begin
  result := false;
  if ( pArrDep <> nil ) and ( pFlt <> nil ) then  begin
  if pArrDep.NodeName = tagDepartures then  begin
  STD := ReadContent( pFlt, tagST );
  if ValidTD( STD ) then  result := true;
  end;
  end;
  end; }

{
  An introduction, or Things I actually use :-

  see uHttpServer.pas  uFlight.pas ( has replaced/absorbed uValidation )


  function   cSession.FillOutTable( p : string ) : string;

  var
  field : aFlightField;
  fieldList : array of aFlightField;
  pfk, pflt, psubflt, pdep : apNode;
  f, s, x : int;
  r, ports, prevPorts, linkStub : string;
  begin
  ........




  pdep := FollowPath( mBase, mContext.Data.GetRoot );

  f := -1;
  while EachSubNode( pdep, f, pfk ) do  begin   // each flight key
  pflt := FindName( pfk, 'Flights' );
  s := -1;
  while EachSubNode( pflt, s, psubflt ) do  begin  // each code share ( subflight )
  oFlight.DbNode := psubflt;

  explanation :-

  this is real boiler plate code to run through every flight/codeshare

  mContext.Data points to the Feeds global cMirrorDB.
  I can read it directly, but only changes by <EditRequest> go to all mirrors ( back to server )

  .GetRoot get me the address of _ROOT_ of this data tree. Now I can read everything.
  cMirrorDB inherits from cDbTree so you can do all the tree stuff.

  mBase is a member string typically 'Departures' or 'Arrivals' - makes this universal.

  pdep := FollowPath( "Departures', from root ) look for a node named "Departures' on the root.
  ie equivqlent to departures.xml effectively a list of flights.

  f := -1;  // list counter, start from beginning
  while EachSubNode( pdep, f, pfk ) do  begin   // each child - each thing in list - each flight key
  loop through each node of the tree (flight) returning pfk = flight key pointer ie QF123-747843

  pflt := FindName( pfk, 'Flights' );
  FindName - find child node with name 'Flights'

  s := -1;
  while EachSubNode( pflt, s, psubflt ) do  begin  // inside loop for each code share ( subflight )


  oFlight.DbNode := psubflt;
  link the flight object to THIS specific flight/codeshare. Now I can use oFlight like a (locally cached) record.

  if ( f = 0 ) and ( s = 0 ) then  begin  // heading - first in lists

  oFlight.Title[ field ]
  field is aFlightField enumeration of standard fields.
  .Title returns the title (column heading) as a string for this field - any special case code is buried in cFlight.

  + oFlight.Presentation[ ffFlight ]
  ffFlight is aFlightField meaning flight name.

  .Presentation returns the standard presentation form of a flight field.
  eg [ffFlight] is typically 'QF 52672'. It provides the fashionable gap.

  CodeShareField ( field ) - there is a crop of field description functions in cFlight





  function   cSession.DbOptions( const list, sel : string ) : string;
  var      // eg <#DBoptions SystemSettings|Strings|StatusD DStatus>
  plist : apNode;
  sl : TStringList;
  x : int;
  field : aFlightField;

  ...
  field := oFlight.FieldID( sel );
  .FieldID( sel ) returns the aFlightField field indentifier from the string 'sel' eg 'ST' -> ffST

  x := 1;  sl := BuildParamsL( NodeContent( plist ), x );
  uUtils.BuildParamsL build a list from a comma (etc) separated list - lots of string handling junk in uUtils.
  NodeContent( plist ) - trivial read node.content string - corresponds to <NodeName> Content </NodeName>





  procedure  cSession.DeleteFlight;

  var
  pf : apNode;
  begin
  if oFlight.DbNode <> nil then  begin
  pf := Back( oFlight.DbNode, 1 );
  if pf.NodeName = tagFlights then  begin
  WrLog( 'DELETE ' + oFlight.Presentation[ ffFlight ] );
  oFlight.Delete;
  end;
  end;
  end;

  .Delete all there is to deleting a flight ( see above  oFlight.DbNode := psubflt; )



  procedure  cSession.ProcessParams( op : aOperation;  params : TStrings );  // does everything else

  if field <> ffNone then  begin
  oFlight.Presentation[ field ] := val;   // collect field vals
  thats all there is to updating the data base field for the linked flight. ( see above  oFlight.DbNode := psubflt; )



  if ( op = opNew ) or ( op = opNewSubFlt )  then  oFlight.DBNode := nil;  // disable updates
  unlink flight ready for a new.
  Now oFlight.Presentation[ field ] := val accumulates the field values in its own members waiting for...

  oFlight.New;  that's all there is to create a new flight in the DB. ( new code share is similar)
  oFlight.Kind := afkDepartures was done earlier to indicate where to put the new flight.



  constructor cSession.Create( id : string; cont : apContext );

  begin
  mID := id;
  mContext := cont;
  oFlight := cFlight.Create( cont.Data, HTTP_ID );
  mBase := tagDepartures;
  end;

  in the beginning we told oFlight cont.Data the mirrorDB object and the ID to use in requests


}

end.
