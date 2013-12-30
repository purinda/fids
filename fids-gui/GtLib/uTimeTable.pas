unit uTimeTable;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, uGlobalDefs, uMirrorDB, uMessageHub, uDBTree, uFlight, uFidsTags, uGT;

{	runs every minute
	keeps mLastRun as bookmark - if zero runs for each timetabled start from now to now + ttAdvance mins
		generating any calls not already in data
	also rolls out departed calls to external DB ( AODB ) after ttRollout mins
}
type
    cFlush = class
        	constructor  Create( log : aLogProc; dataTree : cMirrorDB );
            destructor   Destroy;  override;
        private
            //mHub : cMessageHub;
            mDB : cMirrorDB;
            mLog : aLogProc;
			mST : TDateTime;	// <ST_min> 2160 </ST_min>   in <SystemSettings><Timetable><AutoFlush>
			mET : TDateTime;	// <ET_min> 120 </ET_min>
			mDep_sec : int;		// <Dep_min> 15 </Dep_min>
			mArr_sec : int;		// <Arr_min> 60 </Arr_min>
			mCan_sec : int;		// <Can_min> 60 </Can_min>

            oStatusD : TStringList;   // .Objects holds index into original list
            oStatusA : TStringList;
		public
            procedure   Initialize;
            function    Execute( pFlt : apNode; fk : aFlightKind ) : boolean;

    		end;

	cTimeTable = class
        constructor  Create;
        destructor   Destroy;  override;
        private
            mEr : int;
            mTTEnabled, mInit, mLoggedIn : boolean;
            mLookAheadDT : TDateTime;
            // mXML : string;
            // mLastFlightCreated: string;  // for flight generation
            mLastTTScan, mLastFlushScan : int;
            mLastStopDT : TDateTime;
            mHub : cMessageHub;
            mDB : cMirrorDB;
            mLog : aLogProc;
            // oValidator : cValidation;
            oFlight : cFlight;
            oFlush : cFlush;
            //function   GetHHMM( const s : string ) : int;
            procedure  SetDataTree( dt : cMirrorDB );
            procedure  SetLog( log : aLogProc );

			procedure  FlushFlights;

            function   CheckConstraints( pRules : apNode; day : int ) : bool;
            function   NewFlight( pArrDep, pFlt : apNode; st : TDateTime ) : boolean;
            procedure  ScanTimetable;
			procedure  PollEntry();
            procedure  GenerateNewFlight( pArrDep, pFlt : apNode );
		public
            procedure  Initialize( re : boolean = false );
			procedure  Reset;
            property 	Hub : cMessageHub read mHub  write mHub;
            property	DataTree : cMirrorDB  read mDB  write SetDataTree;
            property	Log : aLogProc  read mLog  write SetLog;
			end;

//var
	//fTimeTable : cTimeTable;

implementation


uses	Math, uGlobal, ASCII, uXmlParser, uFeedMain, uPoller, uUtils;

const

    TimeTableID = 'TT';   // todo add server id number
	Bit: array [0..7] of byte = (1, 2, 4, 8, 16, 32, 64, 128);   // typically ST use Monday = bit0
	MinPerDay = 24 * 60;
    v30sec = 1.0 / 24 / 60 / 2;  // 30 seconds as a TDateTime for rounding - esp D2010
type

	TTimetableDate = (tdNo, tdYes, tdException);

	TTimetableItem = record
	DateStart: TDate;
	DateEnd: TDate;
	WeekDays: byte;   // bit0=Mon .. bit6=Sun
	DateStartX: TDate;
	DateEndX: TDate;
	WeekDaysX: byte;   // bit0=Mon .. bit6=Sun
	DateExceptions: array of array [0..1] of TDate;
	end;


constructor  cFlush.Create( log : aLogProc; dataTree : cMirrorDB );

    var
        x : int;
	begin
    // mHub := hub;
    mLog := log;
    mDB := dataTree;
    if mDB <> nil then  begin
        oStatusA := LoadContentList( FollowPath( pathStatusA, mDB.GetRoot ) );
        for x := 0 to oStatusA.Count - 1 do  oStatusA.Objects[ x ] := TObject( x );  // remember original ordering
        oStatusA.Sort;
        oStatusD := LoadContentList( FollowPath( pathStatusD, mDB.GetRoot ) );
        for x := 0 to oStatusD.Count - 1 do  oStatusD.Objects[ x ] := TObject( x );  // remember original ordering
        oStatusD.Sort;       // enable find in list
    	end;
    end;


destructor   cFlush.Destroy;

	begin
    oStatusA.Free;
    oStatusD.Free;
    end;


procedure   cFlush.Initialize;

    var
        paFlush : apNode;     // load rules from db or use defaults
        min : int;
	begin
    mST := 2160 / MinPerDay;
    mET := 120 / MinPerDay;
    mDep_sec := 15  * 60;
    mArr_sec := 60  * 60;
    mCan_sec := 60  * 60;
    paFlush := FollowPath( pathAutoFlush, mDB.GetRoot );
    if paFlush <> nil then  begin
    	if TryStrToInt( ReadContent( paFlush, 'ST_min' ), min ) then  mST := min / MinPerDay;
    	if TryStrToInt( ReadContent( paFlush, 'ET_min' ), min ) then  mET := min / MinPerDay;
    	if TryStrToInt( ReadContent( paFlush, 'Dep_min' ), min ) then  mDep_sec := min * 60;
    	if TryStrToInt( ReadContent( paFlush, 'Arr_min' ), min ) then  mArr_sec := min * 60;
    	if TryStrToInt( ReadContent( paFlush, 'Can_min' ), min ) then  mCan_sec := min * 60;
	    end;
    end;

function   cFlush.Execute( pFlt : apNode; fk : aFlightKind ) : boolean;

    var
        t, nw : TDateTime;
        pst : apNode;
        sx : int;
        del : boolean;
	begin
    del := false;
    nw := Now();
    t := StrToDT( ReadContent( pFlt, tagST ) );
    if ( t +  mST < nw )  then  del := true;       // if way past expected
    t := StrToDT( ReadContent( pFlt, tagET ) );
    if ( t > 1.0 ) and ( t +  mET < nw )  then  del := true;   // if valid time and well past expected
    case fk of
    	fkNone : del := true;
        fkArrivals : begin
            pst := FindName( pFlt, tagAStatus );
            if ( pst <> nil ) and ( oStatusA <> nil ) then  begin
                if oStatusA.Find( pst.Content, sx ) then  begin
                    if Integer( oStatusA.Objects[ sx ] ) >= 3 then  begin  // at least landed
                        if Seconds - pst.Modified > mArr_sec then  del := true;  // arrived status timeout
                        end;
                    end
	            else  del := true;  // dud status
                end
            // else  del := true;  // no status
	        end;

        fkDepartures : begin
            pst := FindName( pFlt, tagDStatus );
            if pst <> nil then  begin
                if oStatusD.Find( pst.Content, sx ) then  begin
                    if Integer( oStatusD.Objects[ sx ] ) >= 4 then  begin  // at least closed
                        if Seconds - pst.Modified > mDep_sec then  del := true;  // arrived status timeout
                        end;
                    end
	            else  del := true;  // dud status
                end
            // else  del := true;  // no status
	        end;
    	end;
    result := del;
    end;


//______________________________ cTimeTable ____________________________________


constructor  cTimeTable.Create;

	begin
    //oValidator := cValidation.Create;
    end;


destructor   cTimeTable.Destroy;

    begin
    //oValidator.Free;
    oFlush.Free;
    inherited Destroy;
    end;


procedure  cTimeTable.SetDataTree( dt : cMirrorDB );

	begin
    mDB := dt;
    if oFlight = nil then  oFlight := cFlight.Create( dt, TimeTableID );
   // oValidator.DataTree := dt;
    end;


procedure  cTimeTable.SetLog( log : aLogProc );

    begin
    mLog := log;
    //oValidator.Log := log;
    end;


(*

var
	TimetableDate: TDate;
	TimetableTime: word;
	TimetableDeparture: boolean;
	TimetableFlightPath: string;
	//TimetableTick: TDateTime;


function D (Val: byte): char;
	begin
		D := chr ((Val) + ord ('0'));
	end;

function DD (Val: byte): string;
	begin
		//if Val <= 99 then
			DD := D (Val div 10) + D (Val mod 10)
		//else
			//DD := '??';
	end;

function DDD (Val: word): string;
	begin
		DDD := DD (Val div 10) + D (Val mod 10);
	end;

function DDDD (Val: word): string;
	begin
		DDDD := DD (Val div 100) + DD (Val mod 100);
	end;

const
	Hex: array [0..15] of char = '0123456789ABCDEF';

function H (Val: byte): char;
	begin
		H := Hex [Val mod 16];
	end;

function HH (Val: byte): string;
	begin
		HH := Hex [Val div 16] + Hex [Val mod 16];
	end;

function HHHH (Val: word): string;
	begin
		HHHH := HH (Hi (Val)) + HH (Lo (Val));
	end;

function HHHHHHHH (Val: cardinal): string;
	begin
		HHHHHHHH := HHHH (Val shr 16) + HHHH (Val and $0000FFFF);
	end;

function DTToStr (dt: TDateTime): string;
	var
		yr, mo, dy: word;
		hr, mn, se, ms: word;
	begin
		Result := '';
		if dt <> 0.0 then
			begin
				try
					DecodeDate (dt, yr, mo, dy);
					DecodeTime (dt, hr, mn, se, ms);
				except
				end;
		Result := DDDD (yr) + DD (mo) + DD (dy);
				if hr + mn + se <> 0 then
					Result := Result + DD (hr) + DD (mn) + DD (se);
			end;
	end;

procedure ScanTimetableItemFlights (Node: PNode);
	begin
	ChangeData (TimetableFlightPath + '|Flights|' + Node^.Parent^.Name + '|' + Node^.Name, Node^.Data);
	end;



procedure ScanTimetableItems (Node: PNode);
	var
	Item: TTimetableItem;
	sa: TStringArray;
	i, j: integer;
	n: PNode;
	t: word;
	//FltPath: string;
	s: string;
	begin
	// Fill Item from Data Tree
	// Make sure flight is not already in the system
	TimetableFlightPath := Node^.Name;
	FlightAddDate (TimetableFlightPath, TimetableDate);
	TimetableFlightPath := '|' + Node^.Parent^.Name + '|' + TimetableFlightPath;
	n := DataFind (TimetableFlightPath, false, false);
	if n = nil then
		begin
		// Check Time for this item
		t := StrToNum (DataReadFrom (Node, 'Time'));
		if t = TimetableTime then
			begin
			// Check Date for this item
			with Item do
				begin
				DateStart := StrToDT (DataReadFrom (Node, 'DateStart'));
				DateEnd := StrToDT (DataReadFrom (Node, 'DateEnd'));
				WeekDays := StrGetHH (DataReadFrom (Node, 'Days'), 1);
				DateStartX := StrToDT (DataReadFrom (Node, 'DateStartExcept'));
				DateEndX := StrToDT (DataReadFrom (Node, 'DateEndExcept'));
				WeekDaysX := StrGetHH (DataReadFrom (Node, 'DaysExcept'), 1);
				CommaFieldsToStringArray (sa, DataReadFrom (Node, 'DateException'));
				SetLength (DateExceptions, Length (sa));
				for i := 0 to Length (sa) - 1 do
					begin
					j := StrPosCh (1, '-', sa [i]);
					if j = 0 then
						begin
						DateExceptions [i][0] := StrToDT (sa [i]);
						DateExceptions [i][1] := DateExceptions [i][0];
						end
					else
						begin
						DateExceptions [i][0] := StrToDT (Copy (sa [i], 1, j - 1));
						DateExceptions [i][1] := StrToDT (Copy (sa [i], j + 1, 999));
						end;
					end;
				end;
			if DateInTimetableItem (TimetableDate, Item) = tdYes then
				begin
				// Add Flight To Database
				HostXMLCreatePath (TimetableFlightPath, '');
								// Add Physical Flight Items
								s := DTToStr (TimetableDate) + DataReadFrom (Node, 'Time');
								if TimetableDeparture then
									ChangeData (TimetableFlightPath + '|STD', s)
								else
									ChangeData (TimetableFlightPath + '|STA', s);
								ChangeData (TimetableFlightPath + '|Bay', DataReadFrom (Node, 'Bay'));
								ChangeData (TimetableFlightPath + '|Gate', DataReadFrom (Node, 'Gate'));
								ChangeData (TimetableFlightPath + '|Terminal', DataReadFrom (Node, 'Terminal'));
								ChangeData (TimetableFlightPath + '|Belt', DataReadFrom (Node, 'Belt'));
								ChangeData (TimetableFlightPath + '|Aircraft', DataReadFrom (Node, 'Aircraft'));
								s := DataReadFrom (Node, 'Ports');
								if TimetableDeparture then
									ChangeData (TimetableFlightPath + '|Destination', s)
								else
									ChangeData (TimetableFlightPath + '|Origin', s);
								ChangeData (TimetableFlightPath + '|CarrierType', DataReadFrom (Node, 'CarrierType'));
								// Add Code Shares
								DataTreeScan  (NodeToPath (Node) + '|Flights|*|*', ScanTimetableItemFlights);
							end;
					end;
			end;
	end;

 *)


function  StrGetH_ (Ch: char): byte;

	begin
	Ch := UpCase (Ch);
	// Result := 0;
	case Ch of
		'0'..'9': Result := ord (Ch) - ord ('0');
		'A'..'F': Result := ord (Ch) - ord ('A') + 10;
        else  raise Exception.Create( 'Time table invalid hex' );
		end;
	end;


function StrGetH (St: string; Pos: integer): byte;
	begin
	Result := 0;
	if Pos <= Length (St) then
		Result := StrGetH_ (St [Pos]);
	end;


function StrGetHH (St: string; Pos: integer): byte;
	begin
	StrGetHH := (StrGetH (St, Pos) * $10) + StrGetH (St, Pos + 1);
	end;


function   GetHHMM( const s : string ) : int;  // minutes

	begin
	result := 0;
	if Length( s ) = 4 then  begin
		result := StrGetD( s[ 1 ] ) * 10 * 60 + StrGetD( s[ 2 ] ) * 60;
		result := result + StrGetD( s[ 3 ] ) * 10 +  StrGetD( s[ 4 ] );
		end;
	end;

{
procedure  cTimeTable.StartRequestNew( const key : string );

	begin
	mXML := BracketATag( TagEditRequest, ttStart ) + EOL;
	mXML := mXML + TAB + BracketATag( TagNewTag, ttStart ) + '|' + key + '| ';
	end;


procedure  cTimeTable.AddToRequestNew( const add : string );

	begin
	mXML := mXML + add;
	end;


procedure  cTimeTable.EndRequestNew( const path : string );

//	var
//	  req: cXmlParser;
	begin
	mXML := mXML + TAB + BracketATag( TagNewTag, ttEnd ) + EOL;

	mXML := mXML + TAB + BracketATag( TagPath, ttStart ) + path + BracketATag( TagPath, ttEnd ) + EOL;
	mXML := mXML + TAB + BracketATag( TagReqID, ttStart ) + ' ' + TimeTableID + '  '
				 + BracketATag( TagReqID, ttEnd ) + EOL;
	mXML := mXML + BracketATag( TagEditRequest, ttEnd );
//	req := cXmlParser.Create( 13 );
//	req.LoadFromString( mXML );
	Hub.Broadcast( mXML );
//	req.Free;
	end; }



function DateInTimetableItem (Date: TDate; var Item: TTimetableItem): TTimetableDate;
	var
	i: integer;
	begin
	Result := tdNo;
	// Test Date range and day of week
	if (Bit [(Trunc (Date) - 2) mod 7] and Item.WeekDays) <> 0 then   // Week day selected
		if (Date >= Item.DateStart) or (Item.DateStart = 0.0) then
		if (Date <= Item.DateEnd) or (Item.DateEnd = 0.0) then
			Result := tdYes;
	// Test for Bernard exceptions
	if Result = tdYes then
		if (Bit [(Trunc (Date) - 2) mod 7] and Item.WeekDaysX) <> 0 then
		if (Date >= Item.DateStartX) and (Item.DateStartX <> 0) then
			if (Date <= Item.DateEndX) and (Item.DateEndX <> 0) then
			Result := tdException;
	// Test for Date Exceptions
	for i := 0 to Length (Item.DateExceptions) - 1 do
		if Result = tdYes then
		if Date >= Item.DateExceptions [i, 0] then
			if Date <= Item.DateExceptions [i, 1] then
				Result := tdException;
	end;


function   BuildParamsL( const params : string ) : TStringList;   // use uutils

	const
		sep = ',';
	var
		pl : TStringList;
		nam, p : string;
		x : integer;

	procedure SkipSpace;

		begin
		while ( x <= Length( p ) ) and ( p[ x ] = ' ' )  do  Inc( x );
		end;

	begin
	pl := TStringList.Create;    x := 1;
	if params <> '' then  begin
		p := params;
		if p[ Length( p ) ] <> sep then   p := p + sep;
		SkipSpace;
		while x <= Length( p )  do  begin
			if p[ x ] = sep then  begin
				if nam <> '' then  begin
					pl.Add( nam );
					nam := '';
					end;
				Inc( x );
				SkipSpace;
				end
			else  begin
				nam := nam + p[ x ];
				Inc( x );
				end;
			end;
		end;
	Result := pl;
	end;


procedure  cTimeTable.FlushFlights;

    var
        //fltknd : string;
        fk : aFlightKind;
		pArrDep, pFlt : apNode;
		f : int;

	begin
    if oFlush <> nil then  begin
        oFlush.Initialize;  // load parameters
        for fk := Low( aFlightKind ) to High( aFlightKind ) do  begin  // for arrivals and departures
            //fltknd := tagFlightKind[ fk ];
            pArrDep := FindName( mDB.GetRoot(), tagFlightKind[ fk ] );
            f := -1;
            while EachSubNode( pArrDep, f, pFlt ) do  begin  // each timetables flight
                if oFlush.Execute( pFlt, fk ) then  begin
                    if mHub <> nil then  mHub.Broadcast( FormatDelete( ResolvePathStr( pFlt ), TimeTableID ) );
                    fFeedMain.Display( 'Timetable', 'delete flight', NodeName( FollowPath( '|Flights|[0]|', pFlt ) ),
                        ReadContent( pFlt, 'STD' ) );
                	end;
                end;
            end;
    	end;
    end;


function   cTimeTable.CheckConstraints( pRules : apNode; day : int ) : bool;

	var
	Item : TTimetableItem;
	dx : TStringList;
	i, j : integer;
	//n : PNode;
	//FltPath: string;
	begin

    try  begin
        with Item do
            begin
            DateStart := StrToDT( ReadContent( pRules, 'DateStart'));
            DateEnd := StrToDT( ReadContent( pRules, 'DateEnd'));
            WeekDays := StrGetHH( ReadContent( pRules, 'Days'), 1);
            DateStartX := StrToDT( ReadContent( pRules, 'DateStartExcept'));
            DateEndX := StrToDT( ReadContent( pRules, 'DateEndExcept'));
            WeekDaysX := StrGetHH( ReadContent( pRules, 'DaysExcept'), 1);
            // CommaFieldsToStringArray ( sa, ReadContent( ptt, 'DateException'));
            dx := BuildParamsL( ReadContent( pRules, 'DateException') );
            SetLength( DateExceptions, dx.Count );
            for i := 0 to dx.Count - 1 do
                begin
                j := Pos( '-', dx[i]);
                if j = 0 then
                    begin         // single so make a range
                    DateExceptions [i][0] := StrToDT ( dx[i] );
                    DateExceptions [i][1] := DateExceptions [i][0];
                    end
                else
                    begin         // split out range
                    DateExceptions [i][0] := StrToDT (Copy ( dx[i], 1, j - 1));
                    DateExceptions [i][1] := StrToDT (Copy ( dx[i], j + 1, 999));
                    end;
                end; // for i
            dx.Free;
            end;
        result := DateInTimetableItem ( day, Item ) = tdYes;
	    end;
    except
        result := false;  // bad constraints
    	end;
	end;


procedure  cTimeTable.GenerateNewFlight( pArrDep, pFlt : apNode );

	begin
    StartRequestNew( pFlt.NodeName );
    AddToRequestNew( FormatAllSubNodes( pFlt, 2 ) );
    Hub.Broadcast( EndRequestNew( pArrDep.NodeName, '', '', TimeTableID ) );
    fFeedMain.Display( 'Timetable', 'new flight', NodeName( FollowPath( '|Flights|[0]|', pFlt ) ),
    	ReadContent( pFlt, 'STD' ) );
    end;


function  cTimeTable.NewFlight( pArrDep, pFlt : apNode; st : TDateTime ) : boolean;

	var		// creates a new flight based on time table template ( not cFlight style )
		pTemplate, pST, pET, key : apNode;
        flight : cXmlParser;
	begin
    result := false;
	if pArrDep <> nil then  begin
        pTemplate := FindName( pFlt, tagTemplate );
        if pTemplate <> nil then  begin  // got a template section
            flight := cXmlParser.Create();
            //key := flight.NewNode( pFlt.NodeName, false, nil );
            flight.CopyBranch( pTemplate );    // transfer template part to flight as is

            key := FindName( flight.GetRoot, pTemplate.NodeName );
            flight.Rename( key, pFlt.NodeName + '-' );   // assumes flight key is 'name-autoinc'
            pST := FindName( key, tagST );   	// initialize STD/ETD values from rules/date
            if ( pST <> nil ) and ( pST.Content = '' ) then  begin
               	pST.Content := ShortDateTime( st );
               	// pST.Content := DTToStr( st );
                pET := FindName( key, tagET );
            	if ( pET <> nil ) and ( pET.Content = '' ) then  pET.Content := pST.Content;
                if not oFlight.FlightExists( pArrDep, key ) then  begin
                    if oFlight.FlightValid( pArrDep, key ) then  begin
                       GenerateNewFlight( pArrDep, key );
                       result := true;
	                    end;
                    end;
                end;
            flight.Free;
            end;
        end;
	end;


procedure  cTimeTable.ScanTimetable;	// looks through all the rules once a minute looking for a secheduled time match

	var                  // scan tt rules from startDT to Now() + mLookAheadDT
		startDT, stopDT, timeDT : TDateTime;
		pl : TStringList;
		pArrDep, pFlt, ptt, pTime, pRules : apNode;
		time, day, x, f : int;
		done, active : bool;
	begin
	mEr := 0;
    startDT := mLastStopDT;
	stopDT := Now() + mLookAheadDT;
    timeDT := startDT - 1;  // end catch value
	day := Floor( startDT );
    mLastStopDT := stopDT;
	//stop :=  Round( Frac( StopTD ) * MinPerDay );  // stop as minutes

	ptt := mDB.GetRoot();
	ptt := FindName( ptt, tagTimeTable );     // for each /tt/*
    x := -1;
    while EachSubNode( ptt, x, pArrDep ) do  begin
        pl := TStringList.Create;
        pl.Add( tagRules );           	// typically  Timetable|Departures|flight|Rules|Time
        pl.Add( tagTime );
        mDB.Sort( pArrDep, pl );  		// sort flights by scheduled time
        FreeAndNil( pl );
        done := false;      active := false;
        repeat   // for days - may have to start at top of list - ie search after midnight
            f := -1;
            while EachSubNode( pArrDep, f, pFlt ) do  begin  // each timetables flight
                pRules := FindName( pFlt, tagRules );
                pTime := FindName( pRules, tagTime );
                if pTime <> nil then  begin
                    try  begin
                        time := GetHHMM( pTime.Content ); // minutes
                        timeDT := day + time / MinPerDay; //  + v30sec;
                        if timeDT > stopDT then  begin
                            done := true;
                            mInit := true;  // reached end of window so once a minute now
                            break;
                            end;
                        end;
                    except
                        break;  // bad time value so skip rule   todo log
                        end;

                    if not active then  begin
                        if timeDT >= startDT then  active := true;    //	scan till after start time
                        end;
                    if active and CheckConstraints( pRules, day ) then  begin
                        if NewFlight( FollowPath( pArrDep.NodeName, DataTree.GetRoot ), pFlt, timeDT ) then  begin
    						mLastStopDT := timeDT;
                            done := true;
                            break;
                            end;
                        end;
                    end
                else  begin  done := true;  break;  end;
                end;  // each timetables flight
            Inc( day );
            until done or ( day > Floor( stopDT ) );  // loop days
        end;  // for arrive and depart
	end;
(*
	Stop := Start + mLookAheadMin / 24 / 60;
	repeat
		if Start > Stop then
		break;
		mDate := Trunc (Start);
		mTime := MinsToHHMM (Round (Frac (Start) * 24 * 60));
		TimetableDeparture := false;
		DataTreeScan  ('|Timetable|Arrivals|*', ScanTimetableItems);
		TimetableDeparture := true;
		DataTreeScan  ('|Timetable|Departures|*', ScanTimetableItems);
		Start := Start + 1 / 24 / 60;   // Next Minute
	until false;   *)


procedure  cTimeTable.PollEntry();  // comes here every second

	begin              // tt every second until no more new calls generated - then every minute
    if mTTEnabled then  begin
        if not mInit then  ScanTimetable
        else if Seconds - mLastTTScan >= 60 then  begin
            mLastTTScan := Seconds;            // scan tt every minute
            ScanTimetable; // scan with only 1 minute window
            end;
	    end;

    if oFlush <> nil then  begin
        if Seconds - mLastFlushScan >= 60 then  begin
            mLastFlushScan := Seconds;  // scan flush rules every minute
            FlushFlights;
            end;
    	end;
    end;


procedure  cTimeTable.Reset;

	begin
    Initialize( true );    // obs
    end;


procedure  cTimeTable.Initialize( re : boolean = false );
	var
		pt : apNode;
		lookah : int;
	begin
    if DataTree <> nil then  begin
        if DataTree.GetNode( '|SystemConfig|TimeTable|' ) <> nil then  begin
            fFeedMain.Display( 'Timetable', 'enabled', '', '' );
            fFeedMain.LogIn( 'TimeTable', SysPW, TimeTableID );
            mLoggedIn := true;
            mTTEnabled := true;
        	oFlight.Error := 0;  // allow new error reporting
        	mInit := false;

            // kick start DB if empty
            if FindName( DataTree.GetRoot, tagDepartures ) = nil then  begin
            	fFeedMain.Display( 'Timetable', 'create empty Departures DB', '', '' );
                StartRequestNew( tagDepartures );
                Hub.Broadcast( EndRequestNew( '', 'KeyTag="true" AutoInc="1"', '', TimeTableID ) );
            	end;
            if FindName( DataTree.GetRoot, tagArrivals ) = nil then  begin
            	fFeedMain.Display( 'Timetable', 'create empty Arrivals DB', '', '' );
                StartRequestNew( tagArrivals );
                Hub.Broadcast( EndRequestNew( '', 'KeyTag="true" AutoInc="1"', '', TimeTableID ) );
            	end;
            if FindName( DataTree.GetRoot, tagTimeTable ) = nil then  begin
            	fFeedMain.Display( 'Timetable', 'create empty Timetable Rules DB', '', '' );
                StartRequestNew( tagTimeTable );
                AddToRequestNew( '	<Departures KeyTag="true"/> ' + EOL );
                AddToRequestNew( '	<Arrivals KeyTag="true"/> ' + EOL );
                Hub.Broadcast( EndRequestNew( '', 'KeyTag="true"', '', TimeTableID ) );
            	end;

            lookah := 16;   // 16 hour default
            pt := FollowPath( pathLookAhead, DataTree.GetRoot );
            if pt <> nil then  TryStrToInt( pt.Content, lookah );
            mLookAheadDT := Round( lookah ) / 24;
            // mLastFlightCreated := '';
            mLastStopDT := Now();         // start point for next scan
            if not re then  Poller.PollMe( PollEntry );  // once is enough
            end;
        if DataTree.GetNode( '|SystemConfig|AutoFlush|' ) <> nil then  begin
            fFeedMain.Display( 'Flight Romoval', 'enabled', '', '' );
            if oFlush = nil then  begin
            	oFlush := cFlush.Create( mLog, mDB );
	            end;
            // mFlushEnabled := true;
            if not mLoggedIn then  begin
                fFeedMain.LogIn( 'TimeTable', SysPW, TimeTableID );
                mLoggedIn := true;
            	end;
            if not mTTEnabled then  Poller.PollMe( PollEntry );
        	end;
    	end;
    end;
{
	<UseAutoFlush/>
    <FlushToDB/>

	<Timetable>
		<LookAhead_hrs> 16 </LookAhead_hrs>
		<AutoFlush>
			<ST_min> 2160 </ST_min>
			<ET_min> 120 </ET_min>
			<Dep_min> 15 </Dep_min>
			<Arr_min> 60 </Arr_min>
			<Can_min> 60 </Can_min>
		</AutoFlush>
	</Timetable>


<?xml version="1.0" encoding="UTF-8"?>
// timetable flight generation rules. Controlled by GUI or separate resource control app.
<Timetable KeyTag="true">
	<Departures KeyTag="true">
		<ST1234>		// base flight name
			<Rules>
				<DateStart> 20081201 </DateStart>
				<DateEnd> 20090131 </DateEnd>
				<Time> 0700 </Time>
				<Days> 77 </Days>		// 2 hex digits representing a set of days. bit0=Mon .. bit6=Sun

				<DaysExcept> 00 </DaysExcept>	// 2 hex digits representing a set of days. bit0=Mon .. bit6=Sun
												// <DaysExcept> qualifies <DateException>
				<DateException> 20081218-20081220 </DateException>
			</Rules>
			<Template>
				<Flights>		// 'code share' sub tree
					<ST1234/>
				</Flights>
				<STD/>	// timetable knows how to complete STD and ETD from <Time> and timetable date
				<ETD/>
				<Ports> PER </Ports>	// any pre assigned resources
				<Gates> 13 </Gates>
				<Bay/>
			</Template>
		</ST1234>
	</Departures>
</Timetable>

<EditRequest>
	<ReqNewTag> |ST1234-|||
		<Flights>
			<ST1234/>
		</Flights>
		<STD> 20100122 070000 </STD>
		<ETD> 20100122 070000 </ETD>
		<Ports> PER </Ports>
		<Gates> 13 </Gates>
		<Bay/>
	</ReqNewTag>
	<Path> |Departures| </Path>
	<ReqID> Feed </ReqID>
</EditRequest>
}

end.
