unit uFlight;

// this unit is the sole writer of Arrivals and Departures (.xml)
// it provides all input validation for flights - via timetable, user or feed
// all content must be ready for formatter/DisplayConfig use.
// see http://fishbowl.pastiche.org/2003/05/28/the_ghetto_minipattern/

interface

uses
	Classes, Generics.Collections, Generics.Defaults, uFidsTags, uGlobalDefs, uMirrorDB, uDBTree, uGT;

type
    // see also field sets below
    aFlightField = ( ffNone,
    	ffSTime, ffSTdate, ffETime, ffETdate, ffATime, ffATdate,// DUMMY for split update UI eg separate date and time

    	ffFlightKey, ffFlight,
        ffFeedKey,					// typically qantas feed db key value eg QF123/100823 no relation to ffST
        ffST, ffET, ffAT,         	// real fields from here
        ffCorresponding, // ffCorrFeedKey,  arrival/departure pair mapping
        ffDStatus, ffGates, ffBays,
        ffRego, ffCarrier, ffTerminal, ffAirCraft, ffComment, ffStaffNote, ffStaffComment,
        ffPax, ffPaxJoin, ffPaxDisembark, ffPaxTransit, ffPaxTranship,  // Souls/Infants passenger count pairs
        ffPaxWheelChair, ffFuel, ffBurn,
      ffOffBlock, ffSlotTime, ffRelatedFlight,
      ffScheduledCheckinCounters,ffCheckinOpeningTime,ffCheckinClosingTime,ffOnBlock,ffRaceway,

        ffAStatus, ffBelts, // arrival only

      ffNonPublic, ffCrawling, // BOOLEAN

        ffCheckIns, ffPorts    //  CODE SHARE ONLY
         );

    aFlightValidationError = ( feNone, feNoFlightName, feInvalidScheduledTime, feFlightAlreadyExists,
    	feInvalidFlightKind, feNotAllowedToRenameFlight, feDateTimeOutOfRange, feInvalidDateTimeFormat, feDataEntry );

const       // field attribute sets
    BooleanField = [ ffNonPublic ];
    SplitField = [ ffSTime, ffSTdate, ffETime, ffETdate, ffATime, ffATdate ];
    CodeShareField = [ ffCheckIns, ffPorts ];   // sub flight fields
    ListField = [ ffGates, ffBelts, ffCheckIns, ffPorts ];  // see AddItemToList and FinaliseListUpdates  , ffBays ?
    DateTimeField = [ ffST, ffET, ffAT ];
    UpperCaseField = [ ffFlight, ffET, ffRego, ffTerminal, ffAirCraft, ffPorts ];
    RequiredField = [ ffFlight, ffSTime, ffSTdate, ffST, ffPorts ];

type
	//apDeltaHandler = function( update : string ) : boolean;  // returns handled

	cFlight = class( TInterfacedObject, IComparer<apNode> )   // controller / verifier for all FIDS flights
        constructor  Create( db : cMirrorDB {or nil }; ReqID : string );
        private
            mDB : cMirrorDB;  // allows db update and read
            //mOnDelta : apDeltaHandler;
            mReqID : string;  // id string for requests
            mDbNode : apNode; // current db flight name node -  or nil eg <Flights><QF426>
            mFlightKey : apNode;  // base node, eg <QF426-1925>
            mLog : aLogProc;  // reporting
            mNewVals : array [ aFlightField ] of string;  // for new flight building
            mValueSet : set of aFlightField;
            mEr : aFlightValidationError;                                 // error flag
            mSortField : aFlightField;
            mSortAscend : boolean;
            mRangeCheck : boolean;
            mKind : aFlightKind;
    		mFlightNo: Integer;
    		mCodeShareNo: Integer;  // could extend to handle time table templates
            procedure  LogEr( ErrorNo : integer; const s : string );
            function   GetField( field : aFlightField ) : string;
            function   GetDBTagName( field : aFlightField ) : string;
            function   GetTitleField( field : aFlightField ) : string;
            function   GetRawField( field : aFlightField ) : string;
            //function   GetKind : aFlightKind;
			// procedure  HandleDelta( req : string );
            procedure  FieldDelta( pbase : apNode;  field : aFlightField; val : string );
            procedure  FlightDelta( field : aFlightField );
			function   FixTime( field, date, time : aFlightField ) : boolean;
            procedure  SetField( field : aFlightField; val : string );
            function   GetDbPAth : string;
            procedure  SetDbPAth( path : string );
            procedure	SetDbNode( fn : apNode );
            function   GetCodeShareIndex : int;
            function   KindPath : string;
            // function   ValidTD( const td : string ) : boolean;
			function   MakeValidDate( var val : string; field : aFlightField ) : boolean;
			function    MakeValidTime( var time : string; field : aFlightField ) : TTime;  // -ve is invalid
            function   MakeValidTD( var td : string; field : aFlightField ) : boolean;
			function   MakeValidFlight( var name : string ) : boolean;
            function   MakeValid( field : aFlightField; var val : string ) : boolean;
            //function	GetBufferUpdates : boolean;
            //procedure	SetBufferUpdates( buf : boolean );
		public
    		Template	: boolean;
            ErrorMesg	: string;
            function   FlightExists( pArrDep, pFltKey : apNode ) : boolean;  overload;
            function   FlightExists( kind : aFlightKind;  const name, st : string ) : boolean;  overload;
			function   FlightExists : boolean;    overload;// overload - used by New;
            function   FlightValid( pArrDep, pFlt : apNode ) : boolean;
			function   FieldID( const field : string ) : aFlightField;  // see uFidsTags
            //function   List( field : aFlightField; const item : string ) : TList< apNode >;   deprecated;
			function   Match( field : aFlightField; const item : string ) : boolean;
            procedure  Find( kind : aFlightKind;  field : aFlightField;  const item : string );  overload;
            procedure  Find( kind : aFlightKind;  const name, ST : string );  overload;
            procedure  Clear;  // clear all stored values ready for random updates
            procedure	AdjustList( field : aFlightField; const item : string; include : boolean = true );  // used by qantas feed
            procedure	FinaliseListUpdates;

            procedure	First( kind : aFlightKind );
            procedure	Next;
            procedure	FirstCodeShare;
            procedure	NextCodeShare;
            //procedure  InitCompare( field : aFlightField; ascend : boolean );
            function	Compare( const pfltA, pfltB : apNode ) : int;  // IComparer<apNode>  ie can compare flights
            function	AddFlightToTree( db : cDbTree; base : apNode; name : string ) : boolean;   // handles template use of flight

            procedure	Delete;
            procedure	DeleteSub;
            function	New : boolean;
            function	NewCodeShare( primaryFlight : apNode ) : boolean;

            //property	OnDelta : apDeltaHandler  read mOnDelta  write mOnDelta;
            //property	BufferUpdates : boolean  read GetBufferUpdates  write  SetBufferUpdates;
        	property   Log : aLogProc  read mLog  write mLog;
        	property   Presentation [ field : aFlightField ] : string  read GetField  write SetField;  default; // db Update
        	property   DBTag [ field : aFlightField ] : string  read GetDBTagName; //    use var	DBTag [ field ]  below
        	property   Title [ field : aFlightField ] : string  read GetTitleField;
        	property   Raw [ field : aFlightField ] : string  read GetRawField;

        	property   DbNode : apNode  read mDbNode  write  SetDbNode;  // current db flight name node -  or nil eg <Flights><QF426>
        	property   FlightBase : apNode  read mFlightKey  write  mFlightKey;
        	property   DbPath : string  read GetDbPAth  write  SetDbPAth;

            property   Error : aFlightValidationError  read mEr  write mEr;
            property   Kind : aFlightKind read mKind  write mKind;
            property   CodeShare : int  read GetCodeShareIndex;     // 0 => primary
            property   RangeCheck : boolean  read mRangeCheck  write mRangeCheck;  // turn off date validation
			end;

	cFlightList = class( TList< apNode > )
        constructor   Create( db : cMirrorDB );
        destructor    Destroy;  override;
        private
            mDB : cMirrorDB;
            oFlight : cFlight;
        public
            procedure Build( kind : aFlightKind; field : aFlightField; const item : string );
			procedure Sort( const field : aFlightField; ascend : boolean );
			function  FlightNamesN : TStringList;  // mark repeats as QF 123 (2)  etc
			function   FindFlightN( name : string ) : apNode;
			function  GetEnumerator : TEnumerator< apNode >;  // supports compiler  'for pFlt in FlightList do'
            property  Flight : cFlight read oFlight write oFlight;
        end;


function   FieldName( pflt : apNode;  ff : aFlightField ) : string;  // a uFidsTags
function   MatchPattern( const Inp, Pat : string ) : Boolean;
function   TimeField( field : aFlightField ) : boolean;
function   MakeValidTime( var val : string ) : boolean;
// function   CodeShareField( field : aFlightField ) : boolean;   use  CodeShareField set  ie ( field in CodeShareField )
function   KeepEmptyField( field : aFlightField ) : boolean;

function   AirLineCode( flt : string ) : string;  overload;
function   AirLineCode( flt : apNode ) : string;  overload;
function   SizeSuffix( width, height : int ) : string;
function   GraphicName( const txt : string; db : cMirrorDB ) : string;


var
	DBTagName : array [ aFlightField ] of string;


implementation

uses	SysUtils, uUtils, uXmlParser;

const
    nLateST = 2.0;       	//  acceptance range for TD  - 2 day ago
    nAdvancedST = 14.0;    // in 2 weeks time


//________________________ misc ________________________________________________



{function   CodeShareField( field : aFlightField ) : boolean;      obs

	begin
    result := field >= ffCheckins;   // relies on order in aFlightField
    end;    }


function   TimeField( field : aFlightField ) : boolean;

	begin
    result := field in [ ffSTime, ffSTdate, ffETime, ffETdate ] + DateTimeField;
    end;


function   DateField( field : aFlightField ) : boolean;

	begin
    result := ( field = ffSTdate ) or ( field = ffETdate );
    end;


function   KeepEmptyField( field : aFlightField ) : boolean;

	begin
    result := field <> ffFlight;
    end;


function   FieldName( pflt : apNode;  ff : aFlightField ) : string;  // a uFidsTags

	begin      // derrive field name from enumerator name using RTTI
    result := EnumToStr( Ord( ff ), System.TypeInfo( aFlightField ), false );
    result := UnCamel( result, #0 );
    end;


function   DBFieldName( pflt : apNode;  field : aFlightField ) : string;

//    var
//        fk : aFlightKind;
	begin                // todo splitfields
    {if field = ffStatus then  begin   // status is generic name - adjust for flight type
        fk := GetFlightKind( Back( pflt, 2 ) );
        if fk = afkArrivals then  result := tagAStatus
        else if fk = afkDepartures then  result := tagDStatus;
        end
    else }result := FieldName( pflt, field );
    end;


function   IsDecimal( c : char ) : boolean;

    begin
    Result := ( c >= '0' ) and ( c <= '9' );
    end;


function   IsUpAlpha( c : char ) : boolean;

    begin
    Result := ( c >= 'A' ) and ( c <= 'Z' );
    end;

{*****************************************************************}
{* This function implements a subset of regular expression based *}
{* search and is based on the translation of PattenMatch() API   *}
{* of common.c in MSDN Samples\VC98\sdk\sdktools\tlist           *}
{*****************************************************************}
{* MetaChars are  :                                              *}
{*            '*' : Zero or more chars.                          *}
{*            '?' : Any one char.                                *}
{*         [adgj] : Individual chars (inclusion).                *}
{*        [^adgj] : Individual chars (exclusion).                *}
{*          [a-d] : Range (inclusion).                           *}
{*         [^a-d] : Range (exclusion).                           *}
{*       [a-dg-j] : Multiple ranges (inclusion).                 *}
{*      [^a-dg-j] : Multiple ranges (exclusion).                 *}
{*  [ad-fhjnv-xz] : Mix of range & individual chars (inclusion). *}
{* [^ad-fhjnv-xz] : Mix of range & individual chars (exclusion). *}
//   GT extensions - hides some single unicode character tests
//              — : any decimal
//              √ : any uppercase alpha
{*****************************************************************}
// http://www.delphi3000.com/articles/article_1561.asp?SK=
// Paramjeet Reen   - adjusted for unicode delphi and —, √

function MatchPattern( const Inp, Pat : string ) : Boolean;

    var
        InpStr,Pattern :PChar;
begin
  InpStr := PChar( Inp );
  Pattern := PChar( Pat );
  result := false;
  while(True) do
  begin
    case Pattern[0] of
      #0 :begin //End of pattern reached.
            Result := (InpStr[0] = #0); //TRUE if end of InpStr.
            Exit;
          end;

      '*':begin //Match zero or more occurances of any char.
            if(Pattern[1] = #0)then
            begin //Match any number of trailing chars.
              Result := True;
              Exit;
            end else Inc(Pattern);

            while(InpStr[0] <> #0)do
            begin //Try to match any substring of InpStr.
              if(MatchPattern(InpStr,Pattern))then
              begin
                Result := True;
                Exit;
              end;

              //Continue testing next char...
              Inc(InpStr);
            end;
          end;

      '?':begin //Match any one char.
            if(InpStr[0] = #0)then
            begin
              Result := False;
              Exit;
            end;

            //Continue testing next char...
            Inc(InpStr);
            Inc(Pattern);
          end;

      '—' : begin
            if not IsDecimal( InpStr[0] ) then    begin
                Result := False;
                Exit;
            	end
            else  begin
                Inc(InpStr);
                Inc(Pattern);
            	end;
      		end;
      '√' : begin
            if not IsUpAlpha( InpStr[0] ) then    begin
                Result := False;
                Exit;
            	end
            else  begin
                Inc(InpStr);
                Inc(Pattern);
            	end;
      		end;

      '[':begin //Match given set of chars.
            if CharInSet( Pattern[1], [#0,'[',']'] ) then // if(Pattern[1] in [#0,'[',']']) then
            begin //Invalid Set - So no match.
              Result := False;
              Exit;
            end;

            if(Pattern[1] = '^')then
            begin //Match for exclusion of given set...
              Inc(Pattern,2);
              Result := True;
              while(Pattern[0] <> ']')do
              begin
                if(Pattern[1] = '-')then
                begin //Match char exclusion range.
                  if(InpStr[0] >= Pattern[0])and(InpStr[0] <= Pattern[2])then
                  begin //Given char failed set exclusion range.
                    Result := False;
                    Break;
                  end else Inc(Pattern,3);
                end else
                begin //Match individual char exclusion.
                  if(InpStr[0] = Pattern[0])then
                  begin //Given char failed set element exclusion.
                    Result := False;
                    Break;
                  end else Inc(Pattern);
                end;
              end;
            end else
            begin //Match for inclusion of given set...
              Inc(Pattern);
              Result := False;
              while(Pattern[0] <> ']')do
              begin
                if(Pattern[1] = '-')then
                begin //Match char inclusion range.
                  if(InpStr[0] >= Pattern[0])and(InpStr[0] <= Pattern[2])then
                  begin //Given char matched set range inclusion. Continue testing...
                    Result := True;
                    Break;
                  end else Inc(Pattern,3);
                end else
                begin //Match individual char inclusion.
                  if(InpStr[0] = Pattern[0])then
                  begin //Given char matched set element inclusion. Continue testing...
                    Result := True;
                    Break;
                  end else Inc(Pattern);
                end;
              end;
            end;

            if(Result)then
            begin //Match was found. Continue further.
              Inc(InpStr);

              //Position Pattern to char after "]"
              while(Pattern[0] <> ']')and(Pattern[0] <> #0)do Inc(Pattern);

              if(Pattern[0] = #0)then
              begin //Invalid Pattern - missing "]"
                  Result := False;
                  Exit;
              end else Inc(Pattern);
            end else Exit;
          end;

     else begin //Match given single char.
            if(InpStr[0] <> Pattern[0])then
            begin
              Result := False;
              Break;
            end;

            //Continue testing next char...
            Inc(InpStr);
            Inc(Pattern);
          end;
		end;
	end;
end;


function   MakeValidPorts( var val : string ) : boolean;

	begin
    val := UpperCase( val );
    result := true; // todo valid list of √√√
    end;


function   MakeValidTime( var val : string ) : boolean;

	begin
    if Length( val ) = 3 then  Insert( '0', val, 1 );
    result := MatchPattern( val, '————' );
    end;


//____________________________ cFlight _________________________________________


constructor  cFlight.Create( db : cMirrorDB {or nil }; ReqID : string );

	begin
    mDB := db;          // allows db updates
    mReqID := ReqID;
    mRangeCheck := true;
    end;


function   cFlight.  MakeValidFlight( var name : string ) : boolean;

    var
    	i : int;
    begin
    for i := Length( name ) downto 1 do  begin
        if name[ i ] <= ' ' then  System.Delete( name, i, 1 );
        end;
    name := UpperCase( name );
    result := Length( name ) >= 2;
    if not result then  begin  mEr := feNoFlightName;  ErrorMesg := 'Flight name must be at least 2 characters long';  end;
    end;


function   cFlight.MakeValidDate( var val : string; field : aFlightField ) : boolean;

	var
    	d, n : TDate;
        day, m, y : word;
        original : string;
	begin
    val := Trim( val );
    if ( not ( field in RequiredField ) ) and ( val = '' )  then  result := true
    else  begin
        n := Now;    d := 0;    result := true;    original := val;   val := Trim( val );
        if val = '' then  val := DateToStr( n );   // blank => today
        try  begin
            d := StrToDate( val );                 // try 21/2/2013  system form
            DecodeDate( d, y, m, day );
            end;
        except  result := false;  end;

        if not result then  begin  					// try yyyymmnn form
            LeaveOnly( '0123456789', val );    // strip out any '/- ' etc
            result := ( Length( val ) = 8 ) and MatchPattern( val, '————————' );
            if result then  begin
                y := StrToInt( Copy( val, 1, 4 ) );
                if ( y > 2012 ) and ( y < 3000 ) then  begin
                    m := StrToInt( Copy( val, 5, 2 ) );
                    day := StrToInt( Copy( val, 7, 2 ) );
                    result := true;
                    end
                else begin                              // try ddmmyyyy form
                    y := StrToInt( Copy( val, 5, 4 ) );
                    if ( y > 2012 ) and ( y < 3000 ) then  begin
                        m := StrToInt( Copy( val, 3, 2 ) );
                        day := StrToInt( Copy( val, 1, 2 ) );
                        result := true;
                        end
                    //else  result := false;
                    end;
                try  begin
                    d := EncodeDate( y, m, day );
                    end;
                except  result := false;  end;
                end;
            end;

        if result and mRangeCheck then  result := ( d > n - nLateST ) and ( d < n + nAdvancedST );
        if result then  begin
            val := IntToStrN( y, 4 ) + IntToStrN( m, 2 ) + IntToStrN( day, 2 );
            end
        else  begin  mEr := feInvalidDateTimeFormat;  ErrorMesg := 'Invalid Date format "' + original + '"';  end;
        end;
    end;


function    cFlight.MakeValidTD( var td : string; field : aFlightField ) : boolean;

    var
        t : TDateTime;
        original : string;
	begin
    result := true;  original := td;   td := Trim( td );
    if ( not ( field in RequiredField ) ) and ( td = '' ) then  result := true
    else  begin
        LeaveOnly( '0123456789', td );    // strip out any '/- ' etc
        if Length( td ) = 12 then  td := td + '00';
        Insert( ' ', td, 9 );
        {if ( Length( td ) = 4 ) and MatchPattern( td, '————' ) then  begin  // promote 4 digit time to 8 + 6  full DT
            s := DTToStr( Now() );
            for i := 1 to 4 do  s[ 9 + i ] := td[ i ];
            s[ 14 ] := '0';
            s[ 15 ] := '0';
            td := s;
            end; }
        if not MatchPattern( td, '———————— ——————' ) then  begin
            result := false;
            Error := feInvalidDateTimeFormat;
            ErrorMesg := '"' + original + '" is not a valid time';
            end;
        if result then  begin
            t := StrToDT( td );
            if mRangeCheck then  begin
                if ( t < Now() - nLateST ) or ( t > Now() + nAdvancedST ) then  begin
                    result := false;
                    Error := feDateTimeOutOfRange;
                    if not result then  begin  mEr := feDateTimeOutOfRange;  ErrorMesg := 'Flight time is too distant ' + original;  end;
                    end;
                end
            else  begin
                if ( t < 30000 ) or ( t > 60000 ) then  begin
                    result := false;    // realy wrong date
                    mEr := feDateTimeOutOfRange;
                    ErrorMesg := 'Flight time is radically distant ' + original;
                    end;
                end;
            end;
        end;
    end;


function    cFlight.MakeValidTime( var time : string; field : aFlightField ) : TTime;

    var
        original : string;
        hr, min : card;
	begin

    result := -1;   original := time;   time := Trim( time );
    if ( not ( field in RequiredField ) ) and ( time = '' ) then  result := 0
    else  begin
        LeaveOnly( '0123456789', time );    // strip out any '/- ' etc
        if Length( time ) = 4 then  time := time + '00';
        if Length( time ) <> 6 then  begin
            Error := feInvalidDateTimeFormat;
            ErrorMesg := '"' + original + '" is not a valid time';
            end
        else  begin
            hr := StrGetDD( time, 1 );
            min := StrGetDD( time, 3 );
            if ( hr < 24 ) and ( min < 60 ) then  result := hr / 24 + min / ( 24 * 60 )
            else  begin
                Error := feInvalidDateTimeFormat;
                ErrorMesg := '"' + original + '" is not a valid time';
                end;
            end;
        end;
    end;


function  cFlight.MakeValid( field : aFlightField; var val : string ) : boolean;

	begin                         // convert from shorthand / presentation form to full form
    result := true;
    val := Trim( val );
    if field in UpperCaseField then  val := UpperCase( val );
    if field = ffFlight then  begin
    	result := MakeValidFlight( val );
        end
    else if field in DateTimeField then  begin
        if field = ffET then  begin
            if val <> 'TBA' then  result := MakeValidTD( val, field );
            end;
        end
    else if field = ffPorts then result := MakeValidPorts( val );
    end;


//procedure  cFlight.HandleDelta( req : string );
//
//	begin
//    // if Assigned( oUpdates ) then  oUpdates.Add( req )  // store updates multi thread style
//      mDB.BroadcastRequest( req );                 // or do now    else
//    end;


procedure  cFlight.FieldDelta( pbase : apNode;  field : aFlightField;  val : string );   // low level - pfk might be flight or sub-flight

    var
        name : string;
	begin
	if pbase <> nil then  begin
        name := GetDBTagName( field );
        if field in BooleanField then  mDB.GlobalFlag( pbase, name, val <> '', mReqID )
        else   mDB.GlobalEdit( pbase, name, val, mReqID );
        end;

//        pt := FindName( pbase, name );
//        if pt <> nil then  begin   // already exists
//        	if field in BooleanField then  begin
//            	if val <> '1' then  begin
//                    r := FormatDelete( ResolvePathStr( pt ), mReqID );
//            		HandleDelta( r );
//                	end;
//                end
//            else  begin
//                r := FormatEditRequest( ResolvePathStr( pt ), val, pt.Content, mReqID );
//                HandleDelta( r );
//                end;
//            end
//        else  begin
//        	if field in BooleanField then  begin
//            	if val = '1' then  begin
//                    r := StartRequestNew( name );
//                    r := EndRequestNew( r, ResolvePathStr( pbase ), '', '', mReqID );
//                    HandleDelta( r );
//                    end;
//                end
//        	else  begin                // need to make a new one
//                r := StartRequestNew( name );
//                r := EndRequestNew( r, ResolvePathStr( pbase ), '', mNewVals[ field ], mReqID );
//                HandleDelta( r );
//                end;
//            end;
//        end
    end;


procedure  cFlight.FlightDelta( field : aFlightField );

    var                   // returns request string for a field update
        pfk : apNode;
	begin
    pfk := Back( mDbNode, 2 );
    if ( pfk <> nil ) and ( mDbNode <> nil ) and not ( field in SplitField ) then  begin
        if field = ffFlight then  Error := feNotAllowedToRenameFlight // not allowed to modify flight name
        else if field in CodeShareField then  begin   // sub flight / code shares
            FieldDelta( mDbNode, field, mNewVals[ field ] );
            end
        else begin  // a base field ( flight key field )
            FieldDelta( pfk, field, mNewVals[ field ] );
            end
        end;
    end;


function   cFlight.AddFlightToTree( db : cDbTree; base : apNode; name : string ) : boolean;   // handles template use of flight

    var
        pParent, pfltkey, psubflt, p : apNode;
        field : aFlightField;
    begin
    result := false;
    pfltkey := base;  // f.NewNode( name + '-', false, nil );  // flight key
    p := db.NewNode( tagFlights, false, pfltkey );
    psubflt := db.NewNode( name, false, p );          // primary subflight codeshare

    for field := Succ( ffFlight ) to High( aFlightField ) do  begin  // build flight 1 field at a time
        if mNewVals[ field ] <> '' then  begin   // ignore blank fields
            if field in CodeShareField then  pParent := psubflt
            else                             pParent := pfltkey;
            if field in BooleanField then  begin
                if mNewVals[ field ] = '1' then  begin    // empty node if true - none if false
                    db.NewNode( DBTag[ field ], false, pParent );
                    end;
                end
            else  begin
                p := db.NewNode( DBTag[ field ], false, pParent );
                p.Content := mNewVals[ field ];
                end;
            end;
        end;
    if psubflt <> nil then  result := true;
    end;


procedure  cFlight.Delete;

	var
    	pf : apNode;
	begin
    if ( mDB <> nil ) and ( mDbNode <> nil ) then  begin
        pf := Back( mDbNode, 1 );
        if NodeName( pf ) = tagFlights then  begin
        	LogEr( 0, 'DELETE ' + Presentation[ ffFlight ] );
        	//r := FormatDelete( ResolvePathStr( Back( pf, 1 ) ), mReqID );
        	//if r <> ''  then  HandleDelta( r );
            mDB.GlobalDelete( Back( pf, 1 ), mReqID );
            mDbNode := nil;
        	end;
    	end;
    end;


procedure  cFlight.DeleteSub;

     var
         pf : apNode;
     begin
     if ( mDB <> nil ) and ( mDbNode <> nil ) then  begin
         pf := Back( mDbNode, 1 );
         if NodeName( pf ) = tagFlights then  begin
             LogEr( 0, 'DELETE SUB ' + Presentation[ ffFlight ] );
             //r := FormatDelete( ResolvePathStr( mDbNode ), mReqID );
             //if r <> ''  then  HandleDelta( r );
             mDB.GlobalDelete( mDbNode, mReqID );
             mDbNode := nil;
             end;
         end;
     end;


function   cFlight.KindPath : string;

	begin
    result := '';
    if mKind <= High( aFlightKind ) then  result := tagFlightKind[ mKind ];
    end;


function  cFlight.New : boolean;

    var
        pfltkey : apNode;
        path, name, st, r : string;
        f : cDbTree;
    label
    	DontBother;
    begin
    result := false;     f := nil;

    if not MakeValid( ffFlight, mNewVals[ ffFlight ] ) then  begin  Error := feNoFlightName;  goto DontBother;  end;
    if mNewVals[ ffFlightKey ] = '' then  mNewVals[ ffFlightKey ] := mNewVals[ ffFlight ];  // set flight key
    if not MakeValid( ffFlightKey, mNewVals[ ffFlightKey ] ) then  begin  Error := feNoFlightName;  goto DontBother;  end;
    if not MakeValid( ffST, mNewVals[ ffST ] ) then  begin  Error := feInvalidScheduledTime;  goto DontBother;  end;
    if FlightExists then  begin  Error := feFlightAlreadyExists;  goto DontBother;  end;
    if KindPath = '' then  begin  Error := feInvalidFlightKind;  goto DontBother;  end;

    st := mNewVals[ ffST ];   name := mNewVals[ ffFlightKey ];
    f := cDbTree.Create;
    pfltkey := f.NewNode( name + '-', false, nil );  // flight key
    if AddFlightToTree( f, pfltkey, mNewVals[ ffFlight ] ) then  begin
        path := KindPath;
        if path <> '' then  begin
            r := StartRequestNew( pfltkey.NodeName );
            r := AddToRequestNew( r, FormatAllSubNodes( pfltkey, 2 ) );
            r := EndRequestNew( r, path, '', '', mReqID );
            mDB.BroadcastRequest( r, mReqID );
            result := true;
            end;
        end;

DontBother :
    FreeAndNil( f );
    end;


function   cFlight.NewCodeShare( primaryFlight : apNode ) : boolean;

    var
        psubflt, pFlt, p : apNode;
        name, r : string;
        f : cDbTree;
        field : aFlightField;
        x : int;
    label
    	DontBother;
	begin
    result := false;
    name := mNewVals[ ffFlight ];
    if name = ''  then  goto DontBother;  // required field
    p := primaryFlight.Back;
    if NodeName( p ) <> tagFlights then  goto DontBother;  // in the wrong place
    x := -1;
    while EachSubNode( p, x, pFlt ) do  begin
        if NodeName( pFlt ) = name then  goto DontBother;  // name is not unique
        end;

    f := cDbTree.Create;
    psubflt := f.NewNode( name, false, f.GetRoot );          // built a subflight tree

    for field := Succ( ffFlight ) to High( aFlightField ) do  begin  // add subflight fields
        if ( field in CodeShareField ) and ( mNewVals[ field ] <> '' ) then  begin   // ignore blank fields
            p := f.NewNode( DBTag[ field ], false, psubflt );
            p.Content := mNewVals[ field ];
            end;
        end;
    if psubflt <> nil then  begin      // send new request
        // LogEr( 0, 'NEW CODE SHARE ' + Presentation[ ffFlight ] );
        r := StartRequestNew( psubflt.NodeName );
        r := AddToRequestNew( r, FormatAllSubNodes( psubflt, 2 ) );
        r := EndRequestNew( r, ResolvePathStr( Back( primaryFlight, 1 ) ), '', '', mReqID );
        mDB.BroadcastRequest( r, mReqID );
        result := true;
        end;
    f.Free;

	DontBother :
    end;


function   cFlight.GetCodeShareIndex : int;    // 0 => primary

    var
    	pt : apNode;
    begin
    result := 0;
    pt := Back( mDbNode, 1 );
    if ( pt <> nil ) and ( pt.SubNodes <> nil ) then  begin
        result := pt.SubNodes.IndexOf( mDbNode );
        end;
    end;


function   cFlight.GetField( field : aFlightField ) : string;  // presentation style

    var
        s, name : string;
        pfk : apNode;
	begin
    result := mNewVals[ field ];
    if mDbNode <> nil then  begin
        name := GetDBTagName( field );  // FieldName( pflt, field );
        pfk := Back( mDbNode, 2 );
        if ( pfk <> nil ) and ( field <> ffNone ) and ( name <> '' ) then  begin
            if field = ffFlight then  result := Gap( NodeName( mDbNode ) )
            else if field in CodeShareField then  begin   // sub flight / code shares
                result := ReadContent( mDbNode, name );
                end
            else if field in [ ffSTime, ffST ] then  begin
                s := ReadContent( pfk, name );
                result := Copy( s, 10, 4 );
                end
            else if field in [ ffETime, ffET ] then  begin
                s := ReadContent( pfk, name );
                if Length( s ) >= 15 then  result := Copy( s, 10, 4 )   // can be 'TBA' etc
                else  result := s;
                end
            else if field in [ ffETdate, ffSTdate, ffATdate ] then  begin
                s := ReadContent( pfk, name );
                if Length( s ) >= 15 then  begin
                	result := DbDateToStr( s );
                	//result := IntToStr( Copy( s, 7, 2 ) ) + '/' + IntToStr( Copy( s, 1, 8 ) + '/'Copy( s, 1, 8 ) + '/';   // can be 'TBA' etc
                    //Insert( '/', result, 7 );
                    //Insert( '/', result, 5 );
                    end;
                end
            else if DateField( field ) then  begin
                name := Copy( name, 1, 2 );   // eg chop STdate down to ST
                s := ReadContent( pfk, name );
                result := Copy( s, 1, 8 );
                end
            else result := ReadContent( pfk, name );
            end;
	    end;
    end;


function   cFlight.FixTime( field, date, time : aFlightField ) : boolean;

	var
    	dt : string;
        t, n : TTime;
        d : TDate;
	begin              // make a composite date/time from split field pairs
    result := false;
    // if ( mNewVals[ date ] <> '' ) and ( mNewVals[ time ] <> '' ) then  begin   // got the whole set
    if ( date in mValueSet ) and ( time in mValueSet ) then  begin
        t := MakeValidTime( mNewVals[ time ], field );
        if ( mNewVals[ date ] = '' ) and ( mNewVals[ time ] <> '' ) then  begin  // default data is today/tomorrow
            if t >= 0 then  begin
                n := Now();
                if t > Frac( n ) then  d := Trunc( n )  else  d := Trunc( n ) + 1;  // if earlier than now assume tomorrow
                dt := DTtoStr( d + t );  // eg '20131225 123000'
                mNewVals[ date ] := Copy( dt, 1, 8 );  // keep the date portion
        		end;
            end;
        dt := mNewVals[ date ] + ' ' + mNewVals[ time ];
        if MakeValidTD( dt, field ) then  begin
        	mNewVals[ field ] := dt;
        	result := true;
            end;
        end;
    end;


procedure  cFlight.SetField( field : aFlightField; val : string );   // does DB update if  mDbNode set

    begin
    Include( mValueSet, field );
    try  begin                             // call twice - time and date to set a time-date field
        if field in SplitField then  begin   // reassemble split fields here    ie combine time with date to make complete field
            mNewVals[ field ] := val;
            if ( field = ffETime ) and ( UpperCase( val ) = 'TBA' ) then  begin
                val := 'TBA';
                field := ffET;
                end
            else  begin
                case field of
                    ffSTdate, ffETdate, ffATdate :  begin   // accept multiple date formats
                        if MakeValidDate( val, field ) then  mNewVals[ field ] := val  else  mNewVals[ field ] := '';
                        end;
                    end;

                case field of
                    ffSTime, ffSTdate : begin
                        if FixTime( ffST, ffSTdate, ffSTime ) then  begin  // if both halves available combine them
                            field := ffST;
                            val := mNewVals[ field ];
                            end;
                        end;
                    ffETime, ffETdate :  begin
                        if FixTime( ffET, ffETdate, ffETime ) then  begin
                            field := ffET;
                            val := mNewVals[ field ];
                            end;
                        end;
                    ffATime, ffATdate : begin
                        if FixTime( ffAT, ffATdate, ffATime ) then  begin
                            field := ffAT;
                            val := mNewVals[ field ];
                            end;
                        end;
                    end;
                end;
            end;

        if not ( field in SplitField ) then  begin
            if val <> Presentation[ field ] then  begin  // new field value maybe
                if MakeValid( field, val ) then  begin
                    mNewVals[ field ] := val;
                    if val <> Raw[ field ] then  begin  // added to avoid redundant time updates  (see PG 9 Jun 2010)
                        if ( mDB <> nil ) and ( mDbNode <> nil ) then  begin
                            FlightDelta( field );
                            end;
                        end;
                    end;
                end
            else  mNewVals[ field ] := val;
            end;
    	end;
    except  mEr := feDataEntry;  ErrorMesg := 'Invalid Data';  end;
    end;


function   cFlight.GetRawField( field : aFlightField ) : string;

    var
        name : string;
        pfk : apNode;
	begin
    result := mNewVals[ field ];
    if mDbNode <> nil then  begin
        name := GetDBTagName( field );  // FieldName( pflt, field );
        pfk := Back( mDbNode, 2 );
        if ( pfk <> nil ) and ( field <> ffNone ) and ( name <> '' ) then  begin
            if field = ffFlight then  result := NodeName( mDbNode )
            else if field in CodeShareField then  begin   // sub flight / code shares
                result := ReadContent( mDbNode, name );
                end
            else result := ReadContent( pfk, name );
            end;
	    end;
    end;


function   cFlight.GetDBTagName( field : aFlightField ) : string;

	begin
    case field of
        ffFlight :  begin
            if mDbNode = nil then  begin
                result := mNewVals[ ffFlight ];
                end
            else result := NodeName( mDbNode );
            end;
        else  begin
        	result := DBTagName[ field ];  // faster than RTTI
            end;
	    end;
    end;


function   cFlight.FieldID( const field : string ) : aFlightField;  // see uFidsTags
    var
    	s : string;
    begin                        // reverse search for matching enum
    result := High( result );
    while result > ffNone do  begin
    	s := EnumToStr( Ord( result ), System.TypeInfo( aFlightField ), false );
        System.Delete( s, 1, 2 );
        if SameStr( field, s ) then  break;
        Dec( result );
    	end;
    end;


procedure cFlight.  Clear;  // clear all stored values ready for new flight updates

    var
    	field : aFlightField;
	begin
    for field := Low( aFlightField ) to High( aFlightField ) do  begin
    	mNewVals[ field ] := '';
        end;
    end;


procedure cFlight.AdjustList( field : aFlightField; const item : string; include : boolean = true );

	var
    	val : string;
	begin           // comma separated list builder for list fields
    if field in ListField then  begin
    	if mDbNode = nil then  begin  // build new flight - assume include
            if ( item <> '' ) and ( mNewVals[ field ] <> '' ) then  mNewVals[ field ] := mNewVals[ field ] + ',' + item
            else  mNewVals[ field ] := item;
            end
        else  begin
        	val := GetRawField( field );
            if include then  val := IncludeInList( item, val )
            else  val := ExcludeFromList( item, val );
        	SetField( field, val );
        	end;
    	end;
    end;


procedure cFlight.FinaliseListUpdates;

	var
    	field : aFlightField;
        val : string;
	begin
    for field := Succ( ffFlight ) to High( aFlightField ) do  begin
    	if ( field in ListField ) and ( mNewVals[ field ] <> '' ) then  begin
        	val := mNewVals[ field ];      // fiddle due to double use of mVals storage
            mNewVals[ field ] := '';
            SetField( field, val );
            end;
        end;
    end;


procedure	cFlight.First( kind : aFlightKind );

	begin
    mKind := kind;
    mFlightNo := 0;
    mFlightKey := SubNode( mDB.FollowPath_( pathFlightKind[ kind ] ), 0 );
    end;


procedure	cFlight.Next;

	begin
    Inc( mFlightNo );
    mFlightKey := SubNode( mDB.FollowPath_( pathFlightKind[ kind ] ), mFlightNo );
    end;


procedure	cFlight.FirstCodeShare;

	begin
    mCodeShareNo := 0;
    mDbNode := SubNode( FindName( mFlightKey, tagFlights ), mCodeShareNo  );
    end;


procedure	cFlight.NextCodeShare;

	begin
    Inc( mCodeShareNo );
    mDbNode := SubNode( FindName( mFlightKey, tagFlights ), mCodeShareNo  );
    end;


function   cFlight.Match( field : aFlightField; const item : string ) : boolean;

    var
    	x : int;
        vl : TStringList;
        val : string;
	begin
    result := field = ffNone;  // match all if no field specified
    if not result then  begin  // else check specifics
        if item = '' then  result := true
        else  begin
            if mDbNode <> nil then  begin
                val := Raw[ field ];
                if Pos( ',', val ) > 0 then  begin  // a list
                    x := 1;   vl := BuildParamsL( val, x );
                    if vl.IndexOf( item ) >= 0 then  result := true;
                	end
                else if val = item then  result := true;
            	end;
        	end;
    	end;
    end;


procedure  cFlight.Find( kind : aFlightKind;  field : aFlightField;  const item : string );  // overload;

    var                       // set node to first matching flight
        x, f : int;
        pfkey, pf, pflt : apNode;
        base : string;
        found : boolean;
	begin
    mKind := kind;  found := false;
    base := tagFlightKind[ Kind ];
    x := -1;
    while EachSubNode( FindName( mDB.GetRoot, base ), x, pfkey ) do  begin  // each flight key
        f := -1;   pf := FindName( pfkey, tagFlights );
        while EachSubNode( pf, f, pflt ) do  begin   // each code share
            DbNode := pflt;  // connect to db flight
            if Match( field, item ) then  begin
            	found := true;
                break;
                end;
    		end;
        if found then  break;
    	end;
    if not found then  DbNode := nil;
    end;


procedure  cFlight.Find( kind : aFlightKind;  const name, ST : string );  // overload;

    var                       // set node to first matching flight  typically  Find( afkDepartures,  'QF 123', '20100605 1123' )
        x, f : int;
        pfkey, pf, pflt : apNode;
        base, sDate : string;
        found : boolean;
	begin
    mKind := kind;  found := false;
    base := tagFlightKind[ Kind ];
    sDate := Copy( ST, 1, 8 ); // date part only
    // if Length( sDate ) < 8 then  sDate := '20' + sDate;  // handle qantas feed sort date
    x := -1;
    while EachSubNode( FindName( mDB.GetRoot, base ), x, pfkey ) do  begin  // each flight key
        f := -1;   pf := FindName( pfkey, tagFlights );
        while EachSubNode( pf, f, pflt ) do  begin   // each code share
            DbNode := pflt;  // connect to db flight
            if Match( ffFlight, name )  then  begin  //  and Match( ffST, ST )
            	if Copy( ReadContent( pfkey, DBTag[ ffST ] ), 1, 8 ) = sDate then  begin
                	if Length( ST ) = 8 then  begin    // qantas sheduled day only match
                        found := true;
                        break;
                        end
                    else if ReadContent( pfkey, DBTag[ ffST ] ) = ST then  begin  // complete date/time match
                        found := true;
                        break;
                        end;
                	end;
                end;
    		end;
        if found then  break;
    	end;
    if not found then  DbNode := nil;
    end;


function   cFlight.GetTitleField( field : aFlightField ) : string;

//    var
//        fk : aFlightKind;
	begin
    result := EnumToStr( Ord( field ), System.TypeInfo( aFlightField ) );
    case field of
    	ffSTime : result := 'Schd';
    	ffETime : result := 'Estm';
    	ffATime : result := 'Act';
        ffST	: result := 'Scheduled';
        ffET	: result := 'Estimated';
        ffAT	: result := 'Actual';
        ffDStatus, ffAStatus : result := 'Status';
        ffPorts	: begin
        	if mKind = fkDepartures then  result := 'Destination'  else  result := 'Orign';
            end;
	    end;
    end;


function   cFlight.GetDbPAth : string;

    begin
    result := ResolvePathStr( mDbNode );
    end;


procedure  cFlight.SetDbPAth( path : string );

	begin
    DbNode := FollowPath( path, mDB.GetRoot );
    end;


procedure	cFlight.SetDbNode( fn : apNode );

	var
    	pn : apNode;
	begin

    if fn = nil then  mDbNode := nil
    else  begin
        pn := Back( fn, 1 );
        if NodeName( pn ) = tagFlights then  begin
        	mDbNode := fn;
        	pn := Back( fn, 3 );
            if NodeName( pn ) = tagArrivals then  begin
                mKind := fkArrivals;
                end
            else if NodeName( pn ) = tagDepartures then  begin
                mKind := fkDepartures;
                mDbNode := fn;
                end;
            end
        else  raise Exception.Create('Invalid Flight Node ' + NodeName( fn ) );
        end;
    mValueSet := [];  // no new data entered yet
    end;


procedure  cFlight.LogEr( ErrorNo : integer; const s : string );

	begin
//    if ErrorNo > mEr then  begin
//        mEr := ErrorNo;
//        if Assigned( mLog ) then  mLog( ErrorNo, s );
//    	end;
    end;


function   cFlight.FlightExists( kind : aFlightKind;  const name, st : string ) : boolean;  // overload - used by New;

    var                           // requires Kind to be set
    	pArrDep, pf, pfkey, pflt : apNode;
        x, f : int;
	begin
    result := false;

    pArrDep := mDB.GetNode( tagFlightKind[ kind ] );
    x := -1;
    while EachSubNode( pArrDep, x, pfkey ) do begin   // search all base flights
        if ReadContent( pfkey, tagST ) = st then  begin  // matching scheduled time so worth checking the names
            f := -1;   pf := FindName( pfkey, tagFlights );
            while EachSubNode( pf, f, pflt ) do  begin   // and each code share
                if NodeName( pflt ) = name then  begin   // and matching flight name - we have a winner
                    result := true;
                    break;
                    end;
                end;
            end;
        end;
    end;


function   cFlight.FlightExists( pArrDep, pFltKey : apNode ) : boolean;   // pFlt not conventional - is flight key

    var                           // typically pArrDep is either arrivals or departures,  used by timetable
    	pf, pST, pST2 : apNode;
        x : int;
	begin
    result := false;
    pST := FindName( pFltKey, tagST );
    if pST <> nil then  begin        // does the flight have an STD
        x := -1;
        while EachSubNode( pArrDep, x, pf ) do begin   // search all flights
            if Pos( pFltKey.NodeName, pf.NodeName ) = 1 then  begin  // first part of flight name matches
            	pST2 := FindName( pf, tagST );
                if ( pST2 <> nil ) and ( pST2.Content = pST.Content ) then  begin  // and STD matches
                    result := true;
                    break;
                	end;
            	end;
        	end;
    	end;
    end;


function   cFlight.FlightExists : boolean;  // overload - used by New;

    var                           // requires Kind to be set
    	pArrDep, pfkey : apNode;
        x : int;
        tag : string;
	begin
    result := false;
    if mDbNode = nil then   begin
    	if mNewVals[ ffFeedKey ] <> '' then  begin  // match (qantas db) external key
            pArrDep := mDB.GetNode( KindPath );
            x := -1;     tag := DBTag[ ffFeedKey ];
            while EachSubNode( pArrDep, x, pfkey ) do begin   // search all base flights
                if ReadContent( pfkey, tag ) = mNewVals[ ffFeedKey ] then  begin  // matching key so exists
                    result := true;
                    break;
                    end;
                end;
            end
        else  result := FlightExists( mKind, mNewVals[ ffFlight ], mNewVals[ ffST ] );
        end;
    end;


function   cFlight.FlightValid( pArrDep, pFlt : apNode ) : boolean;

	begin
    result := true;  // todo
    end;


function   cFlight.Compare( const pfltA, pfltB : apNode ) : int;  // for sort  -1,0,+1

    var                                                     // work out how to compare any field
    	vala, valb : integer;
        name, str : string;
    begin               // return +1 if pap > pbp
    result := 0;        // todo handle specific field values  esp ST/ET
    if ( pfltA <> nil ) and ( pfltB <> nil ) then  begin
        if mSortField in DateTimeField then  begin  //  raw db string compare
        	name := DBFieldName( pfltA, mSortField );
			result := CompareText( ReadContent( Back( pfltA,2 ), name ), ReadContent( Back( pfltB,2 ), name ) );
	        end
        else  begin     // see cGridBox.DBCompare
        	if mSortField in [ ffGates, ffBays, ffCheckIns ] then  begin  // numeric ?  todo sort by place in resource list ?
                mDbNode := pfltA;  //x := 1;
                vala := AnyDecimal( Presentation[ mSortField ] );  // GetInt( Presentation[ mSortField ], x );
                mDbNode := pfltB;  //x := 1;
                valb := AnyDecimal( Presentation[ mSortField ] );  // GetInt( Presentation[ mSortField ], x );
                if vala > valb then  result := 1
                else if vala < valb then  result := -1;
                end
        	else  begin  // just Presentation string sort
            	mDbNode := pfltA;
                str := Presentation[ mSortField ];
            	mDbNode := pfltB;
                result := CompareText( str, Presentation[ mSortField ] );
	            end;
            end;

        if not mSortAscend then  result := result * -1;
        end;
    end;



// ______________________________ cFlightList __________________________________

type
    cFlightListEnum = class( TEnumerator<apNode> )
        constructor   Create( fl : cFlightList );  // is destroyed at end enumeration by compiler
        //destructor    Destroy;  override;
    	private
        	mIndex : int;
    		mFlightList: cFlightList;
        protected
            function  DoGetCurrent: apNode; override;
            function  DoMoveNext: Boolean;  override;
        end;


constructor cFlightListEnum.Create( fl : cFlightList );

	begin
    mFlightList := fl;
    mIndex := 0;
    end;


//destructor  cFlightListEnum.Destroy;  // override;
//
//	begin
//    mFlightList.mIndex := mIndex;
//    end;


function  cFlightListEnum.DoGetCurrent: apNode; // override;

	begin
    result := mFlightList[ mIndex ];
    Inc( mIndex );
    mFlightList.oFlight.mDbNode := result;
    end;


function  cFlightListEnum.DoMoveNext: Boolean;  // override;

	begin
    if mIndex < mFlightList.Count then  result := true
    else  result := false;
    end;


constructor  cFlightList.Create( db : cMirrorDB );

    begin
    inherited  Create;
    mDB := db;
    end;


destructor   cFlightList.Destroy;

    begin
    FreeAndNil( oFlight );
    inherited
    end;


procedure   cFlightList.Build( kind : aFlightKind; field : aFlightField; const item : string );

    var
        x, f : int;
        pfkey, pf, pflt, pbase : apNode;
        base : string;
	begin
    if oFlight = nil then  oFlight := cFlight.Create( mDB, '' );
    oFlight.Kind := kind;
    Clear;
    base := tagFlightKind[ Kind ];
    x := -1;    pbase := FindName( mDB.GetRoot, base );
    while EachSubNode( pbase, x, pfkey ) do  begin  // each flight key
        f := -1;   pf := FindName( pfkey, tagFlights );
        while EachSubNode( pf, f, pflt ) do  begin   // each code share
            oFlight.DbNode := pflt;  // connect to db flight
            if oFlight.Match( field, item ) then  Add( pflt );
    		end;
    	end;
    end;


procedure  cFlightList.Sort( const field : aFlightField; ascend : boolean );

	begin
    if oFlight <> nil then  begin
        oFlight.mSortField := field;  oFlight.mSortAscend := ascend;
        inherited Sort( oFlight );
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


function   cFlightList.FlightNamesN : TStringList;  // mark repeats as QF 123 (2)  etc

	var
    	x, rpt, fl : int;
        fn : string;
        flightNames : TStringList;
	begin
    result := TStringList.Create;
    flightNames := TStringList.Create;   // names without (2) etc
    for x := 0 to Count - 1 do   begin
        oFlight.DbNode := Items[ x ];
        fn := oFlight.Presentation[ ffFlight ];     rpt := 1;
        for fl := 0 to flightNames.Count - 1 do  begin  // count repeats of this flight name
            if flightNames[ fl ] = fn then  Inc( rpt );
            end;
        flightNames.Add( fn );
        if rpt = 1 then  result.Add( fn )
        else   result.Add( fn + ' (' + IntToStr( rpt ) + ')' );    // allows later correlation of name -> path
        end;
    FreeAndNil( flightNames );
	end;


function   cFlightList.FindFlightN( name : string ) : apNode;

	var
    	x, rpt : int;
        baseName : string;
	begin                                 // parse name as QF123 (2) form
    x := 1;     rpt := 1;    result := nil;   baseName := name;
    if name <> '' then  begin
        if name[ Length( name ) ] = ')' then  begin
            while x <= Length( name ) do  begin
                if name[ x ] = '(' then  begin
                    baseName := Copy( name, 1, x - 2 );
                    Inc( x );
                    break;
                    end;
                Inc( x );
                end;
            if x < Length( name ) then  begin
                rpt := GetInt( name, x );
                end;
            end;

        for x := 0 to Count - 1 do   begin
            oFlight.DbNode := Items[ x ];
            if baseName = oFlight.Presentation[ ffFlight ] then  begin
                if rpt = 1 then  begin
                    result := oFlight.mDbNode;
                    break;
                    end
                else  Dec( rpt );
                end;
        	end;
        end;
    end;


function  cFlightList.GetEnumerator : TEnumerator< apNode >;

	begin
    result := cFlightListEnum.Create( self );
    end;


//______________________________ other _________________________________________


function   AirLineCode( flt : string ) : string;

	var             // typically put a space into flight name before numeric part
		x : int;
	begin
	result := '';
    if flt <> '' then  begin
        result := Copy( flt, 1, 2 );
        for x := 3 to Length( flt ) do  begin
        	if not IsAlpha( flt[ x ] ) then  begin
            	break;
            	end;
            result := result + flt[ x ];
			end;
		end;
    end;


function   AirLineCode( flt : apNode ) : string;

	begin
	result := '';
    if flt <> nil then  begin
        result := AirLineCode( flt.NodeName );
		end;
    end;


function  SizeSuffix( width, height : int ) : string;

	begin
    result := IntToStr( width ) + 'x' + IntToStr( height );
    end;


function  GraphicName( const txt : string; db : cMirrorDB ) : string;

    var
        pt : apNode;
        n, lo, hi, x : int;
        //id, siz : string;
    begin
    result := '';
    if Length( txt ) >= 2 then  begin
        result := AirLineCode( txt );
        if result <> '' then  begin
            x := Length( result ) + 1;  // get numeric part of flight name
            n := GetInt( txt, x );
            pt := db.GetNode( '|SystemSettings|Logo|' + result + '|Range|' );
            if pt <> nil then  begin
                x := 1;
                lo := GetInt( pt.Content, x );
                SkipSpace( pt.Content, x );
                if pt.Content[ x ] = '-' then  begin
                    Inc( x );
                    hi := GetInt( pt.Content, x );
                    if ( n >= lo ) and ( n <= hi ) then  begin
            			pt := FindName( pt, 'Use' );
                        if pt <> nil then  result := pt.Content;
                        end;
                    end;
                end;
            end;
        end;
    end;


var field, f : aFlightField;

initialization

// build DBTag[  names for flight fields table
    for field := Succ( Low( aFlightField ) ) to High( aFlightField ) do  begin
        f := field;
        if field in SplitField then  begin    // splits aren't real fields
        	case field of
                ffSTime, ffSTdate : f := ffST;
                ffETime, ffETdate : f := ffET;
                ffATime, ffATdate : f := ffAT;
            	end;
    		end;
        if ( f <> ffFlight ) and ( f <> ffFlightKey ) then  begin   // leave blank
            DBTagName[ field ] := EnumToStr( Ord( f ), System.TypeInfo( aFlightField ), false );   // was FieldName
            DBTagName[ field ] := UnCamel( DBTagName[ field ], #0 );  // no spaces
            end;
        end;




{
An introduction, or Things I actually use :-

see uEHTMLhandler  uFlight.pas ( has replaced/absorbed uValidation )



        if mBase = tagDepartures then  oFlight.First( fkDepartures )  else  oFlight.First( fkArrivals );
        while oFlight.FlightBase <> nil do  begin   // each flight key
            oFlight.FirstCodeShare;   s := 0;
            while oFlight.DbNode <> nil do  begin   // each code share ( subflight )
            	.....
				r := r + '<td>' + oFlight.Presentation[ field ] + '</td>';   // read DB field value
                .....
                oFlight.Presentation[ field ] := val;   // set DB field to val
                .....
                oFlight.NextCodeShare;   Inc( s );
                end;
            oFlight.Next;
            end;

explanation :-

this is real boiler plate code to run through every flight/codeshare



procedure  cSession.DeleteFlight;

    var
        pf : apNode;
	begin
    if oFlight.DbNode <> nil then  begin
        pf := Back( oFlight.DbNode, 1 );
        if pf.NodeName = tagFlights then  begin
        	WrLog( 'DELETE ' + oFlight.Presentation[ ffFlight ] );
            oFlight.Delete;   //  all there is to deleting a flight ( see above  oFlight.DbNode := psubflt; )
        	end;
    	end;
    end;





procedure  cSession.ProcessParams( op : aOperation;  params : TStrings );  // does everything else

        if field <> ffNone then  begin
        	oFlight.Presentation[ field ] := val;   // collect field vals
thats all there is to updating the data base field for the linked flight. ( see above  oFlight.DbNode := psubflt; )



	if ( op = opNew ) or ( op = opNewSubFlt )  then  oFlight.DBNode := nil;  // disable updates
unlink flight ready for a new.
Now oFlight.Presentation[ field ] := val accumulates the field values in its own members waiting for...

oFlight.New;  that's all there is to create a new flight in the DB. ( new code share is similar)
oFlight.Kind := afkDepartures was done earlier to indicate where to put the new flight.



}



end.
