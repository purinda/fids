unit uCheckInControl;

// this unit is the sole writer of CheckIns (.xml)
// it is responsible for resolving variable style and graphic selection in CheckIns ready for formatter/DisplayConfig

interface

uses
    uDbTree, uMirrorDB, uFlight, uGT, Classes;

const
	tagAirline = 'Airline';
    stFullGraphic = 'FullGraphic';
    stListFlights = 'ListFlights';
    stDualGraphic = 'DualGraphic';

type

    cicField = ( cicNone, cicAirline, cicClass, cicService, cicFullGraphic, cicDefaultGraphic,
        {derrived fields}
    	cicStyle, cicFullScrn, cicHeaderScrn, cicBodyScrn, cicFooterScrn );

	cCheckInControl = class
        private
            mDB : cMirrorDB;  // allows db update and read
            mReqID : string;  // id string for requests
            mName : string;
            mNode : apNode;
            oFlight : cFlight;
            //procedure SetPath( path : string );
			procedure   FieldDelta( pcic : apNode;  const field, val : string );
            function    GetField( field : cicField ) : string;
            procedure   SetField( field : cicField; val : string );
            procedure   SetName( name : string );
            function    GetNumbFlights : int;
			function    DeCode( const path, item : string ) : string;
            function    AirLineLogo : string;
            function    AirLineService : string;
            function    AirLineClass : string;
			//function    GraphicName( const txt : string; width, height : int ) : string;
        public
            constructor Create( db : cMirrorDB; ReqID : string );
            destructor  Destroy;   override;
			function    GetCode( const path, item : string ) : string;
			function    FieldName( field : cicField ) : string;
            procedure   FieldUpdate( const name, val : string );
			procedure   SortOutTheHorribleMess;  // operational 'logic'
            function    FieldValue( const field : string ) : string;
        // property     Path : string  read mPath  write SetPath;

        property     DB [ field : cicField ] : string  read GetField  write SetField;  // db read / Update
        property     Name : string  read mName  write SetName;
        property     NumbFlights : int  read  GetNumbFlights;

    end;

function    TrimFileName( fn : string ) : string;


implementation

uses
    uFidsTags, uGlobalDefs, uUtils, Generics.Collections, uXmlParser, SysUtils;


constructor  cCheckInControl.Create( db : cMirrorDB; ReqID : string );

	begin
    mDB := db;
    mReqID := ReqID;
    oFlight := cFlight.Create( db, ReqID );
    oFlight.Kind := fkDepartures;
    end;


destructor  cCheckInControl.Destroy;

	begin
    FreeAndNil( oFlight );
    end;


procedure  cCheckInControl.FieldDelta( pcic : apNode;  const field, val : string );

    var
    	pt : apNode;
        r : string;
	begin
    if ( field <> '' ) and ( ReadContent( pcic, field ) <> val ) then  begin
        if pcic <> nil then  begin
            pt := FindName( pcic, field );
            if pt <> nil then  begin   // already exists
                r := FormatEditRequest( ResolvePathStr( pt ), val, pt.Content, mReqID );
                end
            else  begin                // need to make a new one
                StartRequestNew( field );
                r := EndRequestNew( ResolvePathStr( pcic ), '', val, mReqID );
                end;
            mDB.SendRequest( r, true );
            end
    	end;
    end;


procedure    cCheckInControl.FieldUpdate( const name, val : string );

    var
        p : int;
        // pflt : apNode;
	begin
    if mNode <> nil then  begin
    	p := Pos( 'Flight', name );   // don't want flights here
        if p = 0 then  FieldDelta( mNode, name, val );  // not a flight field - just write it
	    end;
    end;


function    cCheckInControl.FieldValue( const field : string ) : string;

    begin
    result := ReadContent( mNode, field );
    end;


function    cCheckInControl.FieldName( field : cicField ) : string;

	begin
    if field = cicNone then  result := ''
    else  begin
        result := EnumToStr( Ord( field ), System.TypeInfo( cicField ), false );
        result := UnCamel( result, #0 );  // no spaces
    	end;
    end;


function    cCheckInControl.GetField( field : cicField ) : string;

    begin
    result := FieldValue( FieldName( field ) );
    end;


procedure   cCheckInControl.SetField( field : cicField; val : string );

	begin
    FieldUpdate( FieldName( field ), val );
    end;


function    cCheckInControl.GetCode( const path, item : string ) : string;

    var
        pt : apNode;
        names : TStringList;
        x : int;
	begin
    result := '';
    pt := mDB.GetNode( path );
    names := LoadContentList( pt );
    if names <> nil then  begin
        x := names.IndexOf( item );
        if x >= 0 then  begin
            names.Free;
            names := LoadContentList( FindName( pt, 'Codes' ) );
            if ( names <> nil ) and ( names.Count > x ) then  result := names[ x ];
            end;
        names.Free;
    	end;
    end;


function    cCheckInControl.DeCode( const path, item : string ) : string;

    var
        pt : apNode;
        names : TStringList;
        x : int;
	begin
    result := '';
    pt := mDB.GetNode( path );
    names := LoadContentList( pt );
    if names <> nil then  begin
        x := names.IndexOf( item );
        if x >= 0 then  begin
            names.Free;
            names := LoadContentList( pt.back );
            if ( names <> nil ) and ( names.Count > x ) then  result := names[ x ];
            end;
        names.Free;
        end;
    end;


function    cCheckInControl.AirLineLogo : string;

    var
        pt : apNode;
	begin
    result := '';
    pt := mDB.GetNode( 'SystemConfig|DefaultScreenSize|TopLogo|' );
    if pt <> nil then  begin
        result := GetCode( '|SystemSettings|Strings|Airlines|', DB[ cicAirline ] );
    	result := GraphicName( result, mDB ) + pt.Content + '.gif';
        end;
    end;


function    cCheckInControl.AirLineService : string;

    var
        pt : apNode;
        air : string;
	begin
    result := '';
    pt := mDB.GetNode( 'SystemConfig|DefaultScreenSize|Body|' );
    if pt <> nil then  begin
    	air := GetCode( '|SystemSettings|Strings|Airlines|', DB[ cicAirline ] );
        result := GetCode( '|SystemSettings|Strings|CheckInServices|', DB[ cicService ] );
    	result := GraphicName( air, mDB ) + '_' + result + pt.Content + '.gif';
	    end;
    end;


function    cCheckInControl.AirLineClass : string;

    var
        pt : apNode;
        air : string;
	begin
    pt := mDB.GetNode( 'SystemConfig|DefaultScreenSize|Classes|' );
    if pt <> nil then  begin
    	air := GetCode( '|SystemSettings|Strings|Airlines|', DB[ cicAirline ] );
        result := GetCode( '|SystemSettings|Strings|CheckInClasses|', DB[ cicClass ] );
        if result = '' then  result := 'EC';  // default to economy footer
    	result := GraphicName( air, mDB ) + '_' + result + NodeContent( pt ) + '.gif';
	    end;
    end;


procedure    cCheckInControl.SortOutTheHorribleMess;

	var
		flights : cFlightList;
//        pflt : apNode;
//        x : int;
//    	fn, ciList : string;
//        found : boolean;
	begin
    // problems here (bad UI spec - see the stewart wars of 2008/9) :-
    // CI assignment to fight is indirect - obscure UI operation
    // CI style control is very indirect - obscure UI operation

    if mNode <> nil then  begin
        if ( DB[ cicAirline ] = '' ) and ( NumbFlights <> 0 ) then  begin  // if no explicit airline claim then use first flight
            flights := cFlightList.Create( mDB );
            flights.Build( fkDepartures, ffCheckIns, Name );
            if flights.Count > 0 then  begin
            	DB[ cicAirline ] := DeCode( '|SystemSettings|Strings|Airlines|Codes|', AirLineCode( flights[ 0 ] ) );
	            end;
            FreeAndNil( flights );
        	end;

        if DB[ cicAirline ] = '' then  begin  // closed so show default
            DB[ cicStyle ] := stFullGraphic;
            DB[ cicFullScrn ] := DB[ cicDefaultGraphic ];
            DB[ cicBodyScrn ] := '';
            DB[ cicHeaderScrn ] := '';
            DB[ cicFooterScrn ] := '';
        	end
        else if DB[ cicFullGraphic ] <> '' then  begin  // explicit airline full graphic
            DB[ cicStyle ] := stFullGraphic;
            DB[ cicFullScrn ] := DB[ cicFullGraphic ];
            DB[ cicBodyScrn ] := '';
            DB[ cicHeaderScrn ] := '';
            DB[ cicFooterScrn ] := '';
        	end
        else if DB[ cicService ] <> '' then  begin  // service style
            DB[ cicStyle ] := stDualGraphic;
            DB[ cicHeaderScrn ] := AirLineLogo();
            DB[ cicBodyScrn ] := AirLineService();
            DB[ cicFullScrn ] := '';
            DB[ cicFooterScrn ] := '';
        	end
        else   begin
            DB[ cicStyle ] := stListFlights;        // flight list
            DB[ cicHeaderScrn ] := AirLineLogo();
            DB[ cicFooterScrn ] := AirLineClass();
            DB[ cicFullScrn ] := '';
            DB[ cicBodyScrn ] := '';
        	end
    	end;

    end;


function  TrimFileName( fn : string ) : string;

    var
    	p : int;
	begin
    result := fn;
    while true do  begin            // strip out path
    	p := Pos( '\', result );
        if p <= 0 then  break;
        result := Copy( result, p + 1, 1000 );
        end;
    p := Pos( '.', result );
    if p > 0 then  result := Copy( result, 1, p - 1 );   // strip off extension
    for p := 1 to Length( result ) do  begin
        if IsDecimal( result[ p ] ) then  begin
            result := Copy( result, 1, p - 1 );
            break;
        	end;
    	end;
    end;


{procedure cCheckInControl.SetPath( path : string );  // typically CheckIns|1

	begin
    mPath := path;
    mNode := FollowPath( path, mDB.GetRoot );
    end; }


procedure cCheckInControl.SetName( name : string );  // CI name typically '7'

    var
    	r : string;
	begin
    mName := name;
    mNode := FollowPath( tagCheckins + LSep + name, mDB.GetRoot );
    if mNode = nil then  begin  // doesn't exist so make one
        StartRequestNew( name );
        r := EndRequestNew( tagCheckins, '', '', mReqID );
        mDB.SendRequest( r, true );
    	end;
    end;


function  cCheckInControl.GetNumbFlights : int;

    var
    	fl : cFlightList;
	begin
    fl := cFlightList.Create( mDB );
    fl.Build( fkDepartures, ffCheckIns, Name );
    result := fl.Count;
    fl.Free;
    end;


end.
