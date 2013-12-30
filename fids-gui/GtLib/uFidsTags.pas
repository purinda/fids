unit uFidsTags;

interface

uses  uGT, uDbTree;


const
    tagDepartures = 'Departures';	// <Departures KeyTag="true">
    	tagST = 'ST';             // better if ST and ET so arrivals and departures match
    	tagET = 'ET';
        tagDStatus = 'DStatus';
        tagPorts = 'Ports';
        tagGates = 'Gates';
        tagBays = 'Bays';
        tagCheckIns = 'CheckIns';
        tagRego = 'Rego';
        tagCarrierType = 'CarrierType';
        tagFlights = 'Flights';

    tagArrivals = 'Arrivals';	// <Departures KeyTag="true">
    	// tagSTA = 'STA';     obs
    	// tagETA = 'ETA';
        tagAStatus = 'AStatus';

    tagGround = 'Ground';  // ask paul

    tagTimetable = 'Timetable';   	// <Timetable KeyTag="true">
    	tagRules = 'Rules';
    		tagTime = 'Time';
        tagTemplate = 'Template';

        tagLookAhead = 'LookAhead_hrs';
		tagFlushToDB = 'FlushToDB';

    pathDataDictionary = '|SystemConfig|DataDictionary|';    // todo all pathXXX end in '|'
    pathDataShape = '|SystemConfig|DataShape|';
    tagSystemConfig = 'SystemConfig';
    tagDataShape = 'DataShape';

    tagRefreshUDC = 'RefreshUDC';
    tagDevices = 'Devices';

    tagSTdate = tagST + 'date';     // presentation 'fields' only
    tagETdate = tagET + 'date';

	pathKeyPads_UDC = '|SystemConfig|KeyPads_UDC|';

    pathAutoFlush = 'SystemSettings|Timetable|AutoFlush';
    pathStatusA = 'SystemSettings|Strings|StatusA';
    pathStatusD = 'SystemSettings|Strings|StatusD';
    pathLookAhead = '|SystemSettings|Timetable|LookAhead_hrs|';
    pathStandAlone = 'SystemConfig|Servers|StandAlone';
    pathNotResponding = '|SystemSettings|NotResponding|';

	pathFormats = '|DisplayConfig|Formats|';

	pathAirlines = '\Graphics\GIFs\Airlines\';       // NOTE file (sub) paths
	pathClasses = '\Graphics\GIFs\Classes\';
	pathCheckinFullScreens = '\Graphics\GIFs\FullScr\';
	pathCheckinBody = '\Graphics\GIFs\Body\';
	pathCheckinFooter = '\Graphics\GIFs\Classes\';


type
	aFlightKind = ( fkNone, fkArrivals, fkDepartures, fkGround );  // breaking change - fixed inconsistent 'afk' prefix - sorry

var
    tagFlightKind : array [ aFlightKind ] of string = ( '', tagArrivals, tagDepartures, tagGround );


function   StrToDT (St: string): TDateTime;
function	DTtoStr( dt : TDateTime ) : string;
function   StrGetD( c : char ) : int;
function   GetFlightKind( pFlt : apNode ) : aFlightKind;
function   CorrespondingKind( kind : aFlightKind ) : aFlightKind;
function   StripTail( name : string ) : string;       // QF123-123 -> QF123


implementation


uses  SysUtils, uUtils;


function   GetFlightKind( pFlt : apNode ) : aFlightKind;

	begin
    result := fkNone;
    pflt := Back( pflt, 1 );
    if pflt <> nil then  begin
        if pflt.NodeName = tagFlights then  pflt := Back( pflt, 2 );
        if pflt <> nil then  begin
            if pFlt.Back.NodeName = tagFlightKind[ fkArrivals ] then  result := fkArrivals;
            if pFlt.Back.NodeName = tagFlightKind[ fkDepartures ] then  result := fkDepartures;
            end;
	    end;
    end;


function   CorrespondingKind( kind : aFlightKind ) : aFlightKind;

	begin
    case kind of
    	fkArrivals   : result := fkDepartures;
        fkDepartures : result := fkArrivals;
        else           result := fkNone;
        end
    end;


function   StrGetD( c : char ) : int;

	begin
	if ( c >= '0' ) and ( c <= '9' ) then  result := Ord( c ) - Ord( '0' )
	else  raise Exception.Create( 'Time table invalid decimal' );
	end;


function  StrGetDD ( const St : string; p : int ) : int;

	begin
	result := 0;
	if p < Length( St ) then  begin
		result := StrGetD( St[ p ] ) * 10 + StrGetD( St[ p + 1 ] );
		end;
	end;


function  StrGetDDDD ( const St : string; p : int ) : int;

	begin
	result := 0;
	if p + 3 <= Length( St ) then  begin
		result := StrGetDD( St, p ) * 100 + StrGetDD( St, p + 2 );
		end;
	end;


function StrToDT (St: string): TDateTime;     //  ST
	var
	yr, mo, dy: word;
	hr, mn, se: word;
	p: integer;
	begin
	Result := 0.0;
	if St <> '' then
		begin
		p := 1;
		yr := StrGetDDDD (St, p);   p := p + 4;
		mo := StrGetDD (St, p);     p := p + 2;
		dy := StrGetDD (St, p);     p := p + 3;  // skip space
		try
			Result := EncodeDate (yr, mo, dy);
		except
			end;
		if Length (St) > p then   // IsDigit (St [p]) then
			begin
			hr := StrGetDD (St, p);     p := p + 2;
			mn := StrGetDD (St, p);     p := p + 2;
			se := StrGetDD (St, p);     //p := p + 2;
			try
            	Result := Result + EncodeTime (hr, mn, se, 0);
			except
            	end;
			end;
        end;
	end;


function	DTtoStr( dt : TDateTime ) : string;

	var
		fs : TFormatSettings;
	begin
    fs.ShortDateFormat := 'yyyymmdd ';
    fs.LongTimeFormat := 'hhnnss';
    result := DateTimeToStr( dt, fs );
    end;


function   StripTail( name : string ) : string;    // QF123-123 -> QF123

	var
    	x : int;
	begin
    result := '';
    for x := 1 to Length( name ) do  begin
        if not IsAlphaNumeric( name[ x ] ) then  break;
        result := result + name[ x ];
    	end;
    end;

end.
