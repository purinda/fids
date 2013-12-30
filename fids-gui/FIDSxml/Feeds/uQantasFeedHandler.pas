unit uQantasFeedHandler;

// (C) Digital Images

interface

uses
	SysUtils, Classes,
    uGT, uMirrorDB, uHashTable, uFlight, uFidsTags, Generics.Collections;

type
	apLog = procedure( er : int; const txt : string ) of object;
    apWrLog  = procedure( const txt : string ) of object;

    aQantasHdr = ( qhNone, qhAF, qhDF );
	aQantasField = ( qfNone, FLTN, FLTT, PDFL, ROUT, SKTM, COTM, TATM, ATYP, AREG, TERM,
 		GAT1, GAT2, PBAY, PUDC, STDN, STDC, FLST, PAXC, PFLT, AUDN, ORGN, TMZN, SZCL,
		ATBG, NXOP, CAR1, CAR2, CARP, PDIS, PTRN, PTRS, MAAS, UMIN, YPER, WCHR,
		FUEL, BURN, ACFG, BDST, PBTM, BDTM, FCTM, LCTM, PJNR, BNOP, BNCL,
		BPOP, BPCL, BLCL, BDFL, BDOL, BP1T, BP1C, BP2T, {add 3..7?} BP8T,
		BSTL, BEAL, BLTL, BPBL );

	aFieldHandler = record
        public
        	QantasField : aQantasField;
            Field : aFlightField;
            Handler : procedure( f : aFieldHandler; params : TStringList ) of object;
    		end;

    aFixCorresponding = record
    	Kind : aFlightKind;
        Flight : string;
        FeedKey : string;
        Corresponding : string;
    	CorrFeedKey : string;
        TimeStamp : int;
        end;
    apFixCorresponding = ^ aFixCorresponding;


	cQantasFeedHandler = class
    	constructor Create( DB : cMirrorDB; id : string; log : apLog );
        destructor  Destroy;  override;
        private
        	mDB : cMirrorDB;
            mID : string;
            mLog : apLog;
            mEr : int;
            oStr : TStringBuilder;
            oFieldSet : cHashTable< aFieldHandler >;   // field despatch table
    		oInStream: TFileStream;                    // test data file
            oFlight : cFlight;
            oFixCorresponding : TList< apFixCorresponding >;
    		mIndex : Integer;
            mPkt : TBytes;
            mPktCount, mErCount : integer;
    		mQantasHdr : aQantasHdr;
    		mFlightIdentified : Boolean;
    		mParentFlight : string;
    		mFLST : char;
    		// mPendingDelete: Boolean;
            mTestData : boolean;
            mBaseDate : TDateTime;
            mToday : TDateTime;
            mMesgNo: string;
            mUpdate: Boolean;
            mAllowUpdates: Boolean;
			procedure  LogError( er : int; const mesg : string );
			function   FlightKind : aFlightKind;
			function   FixDate( const date : string ) : string;
			function   GetField( var val : string ) : string;
			function   UseablePacket : boolean;
            procedure  FlightKeyEsiField( f : aFieldHandler; params : TStringList );
            procedure  PrimaryFlightEsiField( f : aFieldHandler; params : TStringList );
            procedure  CorrespondingFlightEsiField( f : aFieldHandler; params : TStringList );
            procedure  StatusEsiField( f : aFieldHandler; params : TStringList );
            procedure  DefaultEsiField( f : aFieldHandler; params : TStringList );
            procedure  CarrierEsiField( f : aFieldHandler; params : TStringList );
            procedure  DateTimeEsiField( f : aFieldHandler; params : TStringList );
            procedure  PortsEsiField( f : aFieldHandler; params : TStringList );
            procedure  BooleanEsiField( f : aFieldHandler; params : TStringList );
            procedure  StringEsiField( f : aFieldHandler; params : TStringList );
            procedure  AddToListEsiField( f : aFieldHandler; params : TStringList );
            procedure  CountPairEsiField( f : aFieldHandler; params : TStringList );
            procedure  MesgNoEsiField( f : aFieldHandler; params : TStringList );
			procedure  GetPktFromStream;
			procedure  LinkCorrespondingFlights( const xml : string );
			procedure  EndOfPkt;
        public
            procedure  ProcessEsiPkt;
            procedure  Initialize;
        end;


implementation

uses
	Dialogs, uGlobalDefs, uUtils, uDbTree, uPoller, ASCII, Windows;

type
	EInvalidESIMessage = class( Exception );   // raise to force packet abort

var
    FieldName : array[ aQantasField ] of string;


constructor cQantasFeedHandler.  Create( DB : cMirrorDB; id : string; log : apLog );

	var
    	qf : aQantasField;
        f : aFieldHandler;
	begin
    mDB := DB;
    mDB.RegisterReader( LinkCorrespondingFlights );  // db changed so try to resolve corresponding links
    mID := id;
    mLog := log;
    oStr := TStringBuilder.Create;
    oFlight := cFlight.Create( DB, id );
    oFixCorresponding := TList< apFixCorresponding >.Create;
    oFieldSet := cHashTable< aFieldHandler >.Create(
    	function ( fh : aFieldHandler ) : string  begin result := FieldName[ fh.QantasField ];  end,     // nameFn
        procedure( var fh : aFieldHandler )  begin  fh.QantasField := qfNone;  fh.Handler := nil;  end,  // UndefFn
        function( fh : aFieldHandler ) : boolean  begin  result := fh.QantasField = qfNone;  end,        // testUndefFn
        Ord( High( aQantasField )) * 3 );

    FieldName[ qfNone ] := '????';
    for qf := Succ( qfNone ) to High( aQantasField ) do  begin    // create a list and handler table of aQantasField field names
        FieldName[ qf ] := UnCamel( EnumToStr( Ord( qf ), System.TypeInfo( aQantasField ), false ), #0 );
        f.QantasField := qf;     f.Field := ffNone;     f.Handler := nil;
        case qf of            // build field despatch table
            FLTN : begin  f.Field := ffFlight;  f.Handler := FlightKeyEsiField;  end;  // must be first in packet
            FLTT : begin  f.Field := ffCarrier;  f.Handler := CarrierEsiField;  end;
            PFLT : begin  f.Field := ffFlight;  f.Handler := PrimaryFlightEsiField;  end;
            NXOP : begin  f.Field := ffCorresponding;  f.Handler := CorrespondingFlightEsiField;  end;
            PDFL : begin  f.Field := ffNonPublic;  f.Handler := BooleanEsiField;  end;
            ROUT : begin  f.Field := ffPorts;  f.Handler := PortsEsiField;  end;
            SKTM : begin  f.Field := ffST;  f.Handler := DateTimeEsiField;  end;
            COTM : begin  f.Field := ffET;  f.Handler := DateTimeEsiField;  end;
            TATM : begin  f.Field := ffAT;  f.Handler := DateTimeEsiField;  end;
            ATYP : begin  f.Field := ffAirCraft;  f.Handler := StringEsiField;  end;
            AREG : begin  f.Field := ffRego;  f.Handler := StringEsiField;  end;
            TERM : begin  f.Field := ffTerminal;  f.Handler := StringEsiField;  end;
            GAT1, GAT2 : begin  f.Field := ffGates;  f.Handler := AddToListEsiField;  end;
            PBAY : begin  f.Field := ffBays;  f.Handler := StringEsiField;  end;
            PUDC : begin  f.Field := ffComment;  f.Handler := StringEsiField;  end;
            STDN : begin  f.Field := ffStaffComment;  f.Handler := StringEsiField;  end;
            STDC : begin  f.Field := ffStaffComment;  f.Handler := StringEsiField;  end;
            FLST, BDST : begin  f.Handler := StatusEsiField;  end;
            PAXC : begin  f.Field := ffPax;  f.Handler := CountPairEsiField;  end;
            PJNR : begin  f.Field := ffPaxJoin;  f.Handler := CountPairEsiField;  end;
            PDIS : begin  f.Field := ffPaxDisembark;  f.Handler := CountPairEsiField;  end;
            PTRS : begin  f.Field := ffPaxTranship;  f.Handler := CountPairEsiField;  end;
            PTRN : begin  f.Field := ffPaxTransit;  f.Handler := CountPairEsiField;  end;
            WCHR : begin  f.Field := ffPaxWheelChair;  f.Handler := StringEsiField;  end;
            FUEL : begin  f.Field := ffFuel;  f.Handler := StringEsiField;  end;
            BURN : begin  f.Field := ffBurn;  f.Handler := StringEsiField;  end;
            AUDN : begin  f.Field := ffFlightKey; 	f.Handler := MesgNoEsiField;  end;
        	else   begin  f.Field := ffFlightKey; 	f.Handler := DefaultEsiField;  end;
	        end;
        if not oFieldSet.Add( f ) then  ShowMessage( 'Duplicate aQantasField ' + FieldName[ f.QantasField ] );
	    end;
    end;


destructor  cQantasFeedHandler.  Destroy;  // override;

	begin
    FreeAndNil( oStr );
    FreeAndNil( oFieldSet );
    FreeAndNil( oInStream );
    FreeAndNil( oFlight );
    FreeAndNil( oFixCorresponding );
    end;


procedure  cQantasFeedHandler.  LogError( er : int; const mesg : string );

	begin
    if ( er = 0 ) or ( er > mEr ) then  begin
        if Assigned( mLog ) then  begin
            mLog( er, 'Qantas feed ' + mesg );
        	end;
        if er <> 0 then  mEr := er;
    	end;
    end;


procedure  cQantasFeedHandler.  DefaultEsiField( f : aFieldHandler; params : TStringList );

	begin
    // ignored
    end;


function   cQantasFeedHandler.  FlightKind : aFlightKind;

	begin
    if      mQantasHdr = qhAF then  result := fkArrivals
    else if mQantasHdr = qhDF then  result := fkDepartures
    else                            result := fkNone;
    end;


function   cQantasFeedHandler.  FixDate( const date : string ) : string;

    var                     // guess a century to handle any old test data
        yr : int;
        d : TDateTime;
	begin
    if TryStrToInt( Copy( date, 1, 2 ), yr ) then  begin
        if yr > 90 then  result := '19' + date
        else             result := '20' + date;
		if mTestData then  begin     // adjust test data to current date
            if mBaseDate = 0 then  begin
                mBaseDate := StrToDT( result + ' 000000' );
                mToday := Trunc( Now() );
                end;
            if mBaseDate <> 0 then  begin
                d := mToday + StrToDT( result + ' 000000' ) - mBaseDate;
                result := DTToStr( d );
                Delete( result, 9, 7 );
            	end;
            end;
        end
    else  result := '';
    end;


// NOTE - current design assumption is that qantas will continue to send fight number at beginning of message

procedure  cQantasFeedHandler.  FlightKeyEsiField( f : aFieldHandler; params : TStringList );

    var
    	fn, date, key : string;
	begin
    if mFlightIdentified then  raise EInvalidESIMessage.Create( 'multiple keys in message ' + oFlight[ ffFlight ] );   // no parent so abort packet
    if params.Count >= 3 then  begin
    	if ( FlightKind <> fkNone ) then  begin    // correct params and inside an arrival or departure mesg
            fn := params[ 0 ] + params[ 1 ];
            if fn = 'JST30' then
                date := FixDate( params[ 2 ] );    // this flight has key <> scheduled date
            date := FixDate( params[ 2 ] );
            key := fn + '/' + params[ 2 ];          // qantas db key - not connected to ffST ie can be rescheduled
            oFlight.Clear;                          // start building a new flight
            oFlight.Kind := FlightKind;
            oFlight.Find( FlightKind, ffFeedKey, key );
            // oFlight.Find( FlightKind, fn, date );  	// date is redundant - also in SKTM field
            mFlightIdentified := true;     			// other flight fields are now meaningful
            if oFlight.DbNode = nil then  begin     // need a new flight so remember the name and direction
                oFlight[ ffFlight ] := fn;
                oFlight[ ffFeedKey ] := key;
            	// LogError( 0, 'new flight ' + oFlight.Presentation[ ffFlight ] );  might be shode share
                end
            else mUpdate := true;
	        end;
        end
    else  raise EInvalidESIMessage.Create( 'Invalid flight number type field' );
    end;


function   cQantasFeedHandler.  UseablePacket : boolean;

	begin
    result := mFlightIdentified and ( mAllowUpdates or ( oFlight.DbNode = nil ) );  // controversial - selectively limit feed updates
    end;


procedure  cQantasFeedHandler.  PrimaryFlightEsiField( f : aFieldHandler; params : TStringList );

    var
    	fn, date : string;
        flt : cFlight;
	begin
    if mFlightIdentified then  begin  // have a flight name
        if params.Count >= 3 then  begin
            if ( FlightKind <> fkNone ) then  begin  // correct params and inside an arrival or departure mesg
                fn := params[ 0 ] + params[ 1 ];
                date := FixDate( params[ 2 ] );
                flt := cFlight.Create( mDB, mID );
                flt.Find( FlightKind, fn, date );
            	mParentFlight := flt.DbPath;         // establish pending new code share
                if mParentFlight = '' then
                  raise  EInvalidESIMessage.Create( 'invalid parent flight for ' + fn );   // no parent so abort packet
                flt.Free;
                end;
	        end;                                        // new, not codeshare flight
    	end
    else  LogError( erInvalidFeedField, 'Invalid PrimaryFlight type field' );
    end;


procedure  cQantasFeedHandler.  CorrespondingFlightEsiField( f : aFieldHandler; params : TStringList );

    var                               // synonym related flight
    	fn, date, key : string;
        flt : cFlight;
        knd : aFlightKind;
        fix : apFixCorresponding;
        done : boolean;
	begin
    flt := nil;
    if mFlightIdentified then  begin  // have a flight name
        if params.Count >= 3 then  begin
            if ( FlightKind <> fkNone ) then  begin  // correct params and inside an arrival or departure mesg
                fn := params[ 0 ] + params[ 1 ];
                date := FixDate( params[ 2 ] );
            	key := fn + '/' + params[ 2 ];          // qantas db key - not connected to ffST ie can be rescheduled
                // oFlight[ ffCorrFeedKey ] := key;
                done := false;
                knd := CorrespondingKind( FlightKind );     // attempt to find corresponding flight - doesn't necessarily exist yet
                if knd <> fkNone then  begin           // todo - might be code share level
                    flt := cFlight.Create( mDB, mID );
                    flt.Find( knd, fn, date );
                    if flt.DbNode <> nil then  begin
                        oFlight[ ffCorresponding ] := flt.DbPath;   // set forward link
                        if oFlight.DbNode <> nil then  begin
                        	flt[ ffCorresponding ] := oFlight.DbPath;   // set back link
                        	done := true;      // both links set so don't do any more
                            end;
	                    end;
                	end;
                if not done then  begin  // keep to fix/link later - at least one link undefined
                    New( fix );
                    oFixCorresponding.Add( fix );
                    fix.Kind := oFlight.Kind;
                    fix.Flight := oFlight.DbPath;   // typically ''
                    if ( flt <> nil ) and ( flt.DbNode <> nil ) then  fix.Corresponding := flt.DbPath;
                    fix.FeedKey := oFlight[ ffFeedKey ];
                    fix.CorrFeedKey := key;
                    fix.TimeStamp := Seconds;
                	end;
                end;
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid PrimaryFlight type field' );
    flt.Free;
    end;


procedure  cQantasFeedHandler.  StatusEsiField( f : aFieldHandler; params : TStringList );

	begin
    if UseablePacket then  begin
    	if params.Count >= 1 then  begin
    		if ( f.QantasField = FLST ) then  begin    // todo catch early delete ?
                if params[ 0 ] <> '' then  begin
                	mFLST := params[ 0 ][ 1 ];
                    if mQantasHdr = qhAF then  begin       // todo use SystemSettings.xml strings
                        if mFLST = 'X' then  oFlight[ ffAStatus ] := 'Cancelled'
                        else if mFLST = 'N' then  oFlight[ ffAStatus ] := 'Delayed'
                        else if mFLST = 'L' then  oFlight[ ffAStatus ] := 'Landed'
                    	else                      oFlight[ ffAStatus ] := '';
		                end
                    else if mQantasHdr = qhDF then  begin            // wait for BDST field to establish other values
                        if mFLST = 'X' then  oFlight[ ffDStatus ] := 'Cancelled'
                        else if mFLST = 'N' then  oFlight[ ffDStatus ] := 'Delayed'
                        else if mFLST = 'E' then  oFlight[ ffDStatus ] := 'Delayed'
                        else if mFLST = 'D' then  oFlight[ ffDStatus ] := 'Departed'
                        end;
                    end
                else  mFLST := #0; // retain flight status code
            	// if params[ 0 ] = 'Z' then  mPendingDelete := true;
    			end
            else if ( f.QantasField = BDST ) and ( mQantasHdr = qhDF ) then  begin  // pick up detailed boarding status
            	if ( mFLST = 'S' ) or (  mFLST = 'R' ) then  begin  // normal flight status
                    if params[ 0 ] <> '' then  begin
                        case params[ 0 ][ 1 ] of
                            'N', 'P' : oFlight[ ffDStatus ] := '';  // 'P' : oFlight[ ffDStatus ] := 'Pre-boarding';
                            'B' : oFlight[ ffDStatus ] := 'Boarding';
                            'F' : oFlight[ ffDStatus ] := 'Final Call';
                            'L' : oFlight[ ffDStatus ] := 'Closed';
                        	end;
                    	end;
                    end;
                end;
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid status type field' );
    end;


procedure  cQantasFeedHandler.  CarrierEsiField( f : aFieldHandler; params : TStringList );

    var
    	val : string;
	begin
    if UseablePacket then  begin
    	if params.Count >= 1 then  begin
            if params[ 0 ]= 'D' then  val := 'Domestic'
            else if params[ 0 ]= 'I' then  val := 'International';
            oFlight[ ffCarrier ] := val;   // <CarrierTypes>[ 0 ]
            end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid carrier type field' );
    end;


procedure  cQantasFeedHandler.  PortsEsiField( f : aFieldHandler; params : TStringList );

    var
    	ports : string;
	begin
    if UseablePacket then  begin
    	ports := ListToStr( params );  // make comma separated list of ports
        oFlight[ ffPorts ] := ports;
    	end
    else  LogError( erInvalidFeedField, 'Invalid ports type field' );
    end;


procedure  cQantasFeedHandler.  DateTimeEsiField( f : aFieldHandler; params : TStringList );

    var
    	date : string;
	begin
    if UseablePacket then  begin
    	if params.Count >= 2 then  begin
            date := FixDate( params[ 0 ] );
            oFlight[ f.Field ] := date + ' ' + params[ 1 ];
        	end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid date/time type field' );
    end;


procedure  cQantasFeedHandler.  BooleanEsiField( f : aFieldHandler; params : TStringList );

    var
    	val : string;
	begin
    if UseablePacket then  begin
    	if ( params.Count >= 1 ) and ( f.Field = ffNonPublic ) then  begin
            if params[ 0 ]= 'Y' then  val := '0'          // inverting value
            else if params[ 0 ]= 'N' then  val := '1';
            oFlight[ ffNonPublic ] := val;
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid public type field' );
    end;


procedure  cQantasFeedHandler.  StringEsiField( f : aFieldHandler; params : TStringList );

	begin
    if UseablePacket then  begin
    	if params.Count >= 1 then  begin
            oFlight.Presentation[ f.Field ] := params[ 0 ];
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid string type field' );
    end;


procedure  cQantasFeedHandler.  AddToListEsiField( f : aFieldHandler; params : TStringList );

	begin
    if UseablePacket then  begin
    	if params.Count >= 1 then  begin
            oFlight.AddItemToList( f.Field, params[ 0 ] );
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid list type field in' );
    end;


procedure  cQantasFeedHandler.  CountPairEsiField( f : aFieldHandler; params : TStringList );

	begin
    if mFlightIdentified then  begin
    	if params.Count >= 1 then  begin
            oFlight[ f.Field ] := ListToStr( params );
	        end;
    	end
    else  LogError( erInvalidFeedField, 'Invalid passenger count field' );
    end;


procedure  cQantasFeedHandler.  MesgNoEsiField( f : aFieldHandler; params : TStringList );

	begin
    if params.Count >= 1 then  begin
        mMesgNo := params[ 0 ];
        end;
    end;


function   cQantasFeedHandler.  GetField( var val : string ) : string;

    var
        x : int;
        c : char;
	begin
    result := '';  val := '';   x := mIndex;
    oStr.Clear;
    while x < Length( mPkt ) do  begin
        c := Char( mPkt[ x ] );  Inc( x );
        if ( c = Char( $1D )) or ( c = Char( $1E )) then  begin  // field/record terminator
        	if result = '' then  result := oStr.ToString  else  val := oStr.ToString;
            oStr.Clear;
            break;
	        end
        else if c = Char( $1C ) then  begin  // header terminator
        	result := oStr.ToString;
            oStr.Clear;
	        end
        else  oStr.Append( c );
    	end;
    mIndex := x;
    end;


procedure  cQantasFeedHandler.  LinkCorrespondingFlights( const xml : string );  // called every db delta

    var
        x : int;
    	fix : apFixCorresponding;
        // link : cFlight;
	begin   // attempt to resolve links associated with current flight
    x := 0;
    while x < oFixCorresponding.Count do  begin    // todo - wait for new flight
        fix := oFixCorresponding[ x ];
        if Seconds - fix.TimeStamp > 16 * 60 * 60 then  begin  // garbage collect
            oFixCorresponding.Delete( x );     Dispose( fix );
            continue;   // don't inc x
        	end;
        if fix.Flight = '' then  begin  // identify flight path
            oFlight.Find( fix.Kind, ffFeedKey, fix.FeedKey );
            if oFlight.DbNode <> nil then  begin
                fix.Flight := oFlight.DbPath;                 // escapes the request $$$
                if fix.Corresponding <> '' then  oFlight[ ffCorresponding ] := fix.Flight;  // set forward link
            	end;
        	end;
        if fix.Corresponding = '' then  begin  // identify flight path
            oFlight.Find( CorrespondingKind( fix.Kind ), ffFeedKey, fix.CorrFeedKey );
            if oFlight.DbNode <> nil then  begin
                fix.Corresponding := oFlight.DbPath;
                if fix.Flight <> '' then  oFlight[ ffCorresponding ] := fix.Corresponding;  // set back link
                end;
        	end;
        if ( fix.Flight <> '' ) and ( fix.Corresponding <> '' ) then  begin  // both ends of link are now resolved
            oFixCorresponding.Delete( x );   Dispose( fix );
            continue;  // don't inc x
        	end;
        Inc( x );
    	end;
    end;


procedure  cQantasFeedHandler.  EndOfPkt;

	begin
    if ( mEr = 0 ) and ( mFlightIdentified ) then  begin
        if ( oFlight.DbNode = nil ) then  begin   // need to create a new flight
        	if mParentFlight = '' then  begin
            	if oFlight.New then  begin
                	LogError( 0, 'new flight ' + oFlight.Presentation[ ffFlight ] );
	                end;
	            end
            else  begin
            	if oFlight.NewCodeShare( mDB.GetNode( mParentFlight ) ) then  begin
                	LogError( 0, 'new code share ' + oFlight.Presentation[ ffFlight ] );
	                end;
	            end;
        	end
        else  if mAllowUpdates then  oFlight.FinaliseListUpdates;
    	end
    else  Inc( mErCount );
    end;


procedure  cQantasFeedHandler.  ProcessEsiPkt;

	var
    	name, val : string;
        vals : TStringList;
        x : int;
        qh : aQantasHdr;
        // qf : aQantasField;
		f : aFieldHandler;
        abort : boolean;
	begin
    mIndex := 0;   mFlightIdentified := false;   mEr := 0;   abort := false;
    mMesgNo := '';  oFlight.DbNode := nil;    mFLST := #0;   mParentFlight := '';
    if Length( mPkt ) >= 8 then  begin
        name := GetField( val );
        mQantasHdr := qhNone;
        for qh := Succ( Low( aQantasHdr )) to High( aQantasHdr )  do  begin   // find matching header name  typically DF or AF
            if name = UnCamel( EnumToStr( Ord( qh ), System.TypeInfo( aQantasHdr ), false ), #0 ) then  begin
                mQantasHdr := qh;
                break;
                end;
            end;
        if mQantasHdr <> qhNone then  begin
            Inc( mPktCount );
            while ( mIndex < Length( mPkt ) ) and not abort do  begin
                name := GetField( val );
                f := oFieldSet.Find( name );
                if f.Field <> ffNone then  begin
                    x := 1;
                    vals := BuildParamsL( val, x, '/' );    // qantas field value separator
                    try  try    f.Handler( f, vals );
                    except
                        // on e : Exception do  begin      // too boring
                            // LogError( 0, e.Message );
                            abort := true;   // break doesn't work here due to finally
                            // end;
                        end;
                    finally  vals.Free;   end;
                    end;
                end;
            if not abort then  EndOfPkt;
            end;
        end;
    end;


procedure  cQantasFeedHandler.  GetPktFromStream;   // comes here every tenth to read test packets

	var
        b : byte;
        x, n : int;
    begin
    x := 0;
    while true do  begin
    	n := oInStream.Read( b, 1 );
        if n > 0 then  begin
            if ( b = $0B ) then  begin   // start of  next packet
                if x > 1 then   begin
                    SetLength( mPkt, x );
                    ProcessEsiPkt;
                    end;
                break;
                end;
            if x >= Length( mPkt ) then  SetLength( mPkt, Length( mPkt ) + 256 );  // extend if necessary
            mPkt[ x ] := b;
            Inc( x );
            end
    	else  begin
            LogError( 0, 'end of test file' );
            Poller.UnPollMe( GetPktFromStream );
            break;
        	end;
	    end;
    end;

// <File> QantasESI-4.qfd </File>  derived from capturefile-4.doc
// the 0B packet start is not part of the serial protocol
// 0B 4446 1D 464C544E 1C 4A53542F363638312F303831303331 1D 464C5454 1C 44 1D 5044464C 1C 59 1D
// 37383934 1E 2F 0B 4446 1D 464C544E 1C
// 4E 1C 3331353538333633 1E 3D 0B 4446 1D 464C544E 1C 5146
// 4E 1C 3331353538333631 1E 3D 0B 4446 1D 464C544E 1C 5146412F34


procedure  cQantasFeedHandler.  Initialize;

    var
    	p : apNode;
        fn : string;
	begin
    if mDB <> nil then  begin
        p := mDB.GetNode( '|SystemConfig|Feed|QANTAS_ESI|' );
    	if p <> nil then  begin
    	// todo - init terminal server and UMP connection handler
        	mAllowUpdates := FindName( p, 'AllowUpdates' ) <> nil;  // otherwise new flights only accepted
            fn := ReadContent( p, 'File' );      // typically  QantasESI-4.qfd ( has no ump packaging )
            if FileExists( fn ) then  begin
                // oFlight.RangeCheck := false;  // allow old flights into db for testing   todo remap flights to today ?
                mTestData := true;
               	oInStream := TFileStream.Create( fn, fmShareDenyWrite or fmOpenRead ); // can open as read only by editpadpro etc
                oInStream.Seek( 0, soFromBeginning );
                Poller.PollMeTenths( GetPktFromStream );
                LogError( 0, 'processing test data in ' + fn );
                end;
        	end;
	    end;
    end;


{
apparently qantas feed is FMS/ESI messages inside UMP packets - no test data available for UMP

latest spec :-
com.qantas.esi2.ts.nn		over view
com.qantas.esi2.dd.08.pdf  	data dictionary  31 Oct 08
com.ingennia.fms.ts.nn		field name / data pairs
com.ingennia.umpa.ts.nn     packet protocol
see MODULE UMPProtocol;
    A packet looks like...   (MS)
  syn syn syn.. stx length flag num ackFlag ackNum <data block> crc
          bytes  1    2      1   1     1      1      0..1023     2

    The CRC covers the everything except the DC2 and STX.

SYNC SYNC STX     (SYN = 0x16)
CRC-16/CCITT Polynomial: x16+x12+x5+x0  = 1021
qftrace.out :-
1 0 0 0 0 0 0  7 byte header

AF gs
FLTNQFA/796/100609FLTTDPDFLYROUTMELSKTM100609/1105COTM100609/1114ATYP73HAREGVHVZEGAT16GAT2PBAY5PUDCSTDNFKLFT OFFSTDCFLSTLPFLTPAXC144/0NXOPQFA/797/100609CAR12CAR2CARPTATM100609/1114MAASUMINYPERWCHRPDIS144/PTRN/PTRS/AUDN4121707


capturefile-4.doc  is biggest esi sample but no ump packaging
DFFLTNJST/6681/081031FLTTDPDFLYROUTADLSKTM081031/1110COTM081031/1109ATYP734AREGVHTJIGAT1GAT2PBAYPUDCSTDNSTDCFLSTAPFLTQFA/681/081031FUEL6600BURN3600ACFG0/12/132PAXC144/0BDSTNBDTM081031/1054TATM081031/1123PBTM081031/1024FCTM081031/1101LCTM081031/1106PTRN/PJNR144/AUDN31557894‑/
DFFLTNSIA/238/081031FLTTIPDFLYROUTSINSKTM081031/1120COTM081031/1112ATYP744AREG9VSPNGAT1GAT2PBAYPUDCSTDNSTDCFLSTAPFLTFUELBURNACFG24/53/316PAXC/BDSTNBDTM081031/1042TATM081031/1131PBTM081031/0912FCTM081031/1057LCTM081031/1104PTRN/PJNR/AUDN31550119‑L

AFFLTNQFA/5775/081101FLTTDPDFLYROUTADLSKTM081101/1630COTM081101/1630ATYP320AREGVHVQEGAT1GAT2PBAYPUDCSTDNSTDCFLSTSPFLTJST/775/081101PAXC170/0NXOP//CAR1CAR2CARPTATMMAASUMINYPERWCHRPDIS170/PTRN/PTRS/AUDN28454753‑

AFFLTNJST/35/081101FLTTIPDFLYROUTSYDSKTM081101/1645COTM081101/1645ATYP332AREGVHEBFGAT1GAT2PBAYPUDCSTDNSTDCFLSTSPFLTPAXC251/NXOPJST/35/081101CAR1CAR2CARPTATMMAASUMINYPERWCHRPDIS251/PTRN0/0PTRS0/0AUDN28454691‑


easidump.dat  apparently lowest level binary capture  but wrong protocol :(

1901000048F1EC31					NOT a UMP/A packet

19 01 - len = 281                   all guesses
00 00	pf = 0 => idle
48 F1   pn = 0  no seq no
EC 31

19 01 - len = 281   				start FMS message  ie ESI message
00 00	nu - first segment
00 00	ctl - last segment
00 00	res
									data
44461D								DF<GS> message type

464C544E 1C							FLTN<FS> handle (text)
5146412F323335322F393630373137 1D	QFA/2352/960717<GS>

464C5454 1C							FLTT<FS>
44 1D								D<GS>
}

end.
