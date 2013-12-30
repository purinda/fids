unit uHttpServer;

// this unit is the 'web application' brouser UI for FIDS
// it uses cookie based session ID for session mgt.
// see   Daniel Wischnewski   http://www.delphi3000.com/articles/article_3081.asp  for indy server demo

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, Contnrs, Generics.Collections, SysUtils,
  IdBaseComponent, IdComponent, IdTCPServer, IdHTTPServer, IdContext, IdCustomHTTPServer, IdCookieManager,
  StdCtrls, ExtCtrls, HTTPProd, IdCustomTCPServer, uGT, uMessageHub, uMirrorDB, uDbTree, uFidsTags, uFlight, uCheckInControl;


const
	MaxCIFlights = 5;   // why 5 ?!? di spec

type
	apLog = procedure( er : int; const txt : string ) of object;
    apWrLog  = procedure( const txt : string ) of object;
    aOperation = ( opNone, opEdit, opDelete, opNew, opNewSubFlt );

    aContext = record    // RO info applicable to all sessions
        Log : apWrLog;
        Data : cMirrorDB;
        Root, DefaultDoc : string;
        LogInRequired : boolean;
    	end;
    apContext = ^ aContext;

    cSession = class
        constructor Create( id : string; cont : apContext );
        destructor  Destroy;  override;
        private
            mID : string;  // session ID
            mUsrID : string;  // HTTP_ + login_name
            mAccess : string;  // access permissions
            mLastActiveSeconds, mLastLoginSeconds : int; // seconds
            mContext : apContext;
            mTaskContext: TIdContext;
            mRequestInfo: TIdHTTPRequestInfo;
            mResponseInfo: TIdHTTPResponseInfo;
            mDocState : ( dsNone, dsLogin );
            mResult, mBase : string;
            oFlight : cFlight;
        	oCIC : cCheckInControl;
            oReqDoc : TStringList;

            oFlightList, oFlightPath : TStringList;              // CI flight list server state
            mPrevCIFlight : array[ 0..MaxCIFlights ] of string;  // name of prev selected flights

            mLoggedIn : boolean;
			function   HandleEditParams( const doc : string ) : string;
            procedure  Handler;
            procedure  WrLog( const txt : string );
			function   ParseParam( const param : string;  out val : string ) : string;  // Ports=SYD
			function   ParseParamFF( const param : string;  out val : string ) : aFlightField;
			procedure  DeleteFlight;
			procedure  ProcessLogIn( params : TStrings );
			procedure  ProcessCICParams( params : TStrings );
			procedure  ProcessParams( op : aOperation;  params : TStrings );
			function   CheckInControlTable : string;
			function   SummaryTable( p : string ) : string;
            function   FillOutTable( p : string ) : string;
			function   DbOptions( const list, sel : string ) : string;
			function   FlightList( const list, sel : string ) : string;   // <#Flights CIC 3>  produce a filtered flight list
            procedure  EHTMLtagHandler(Sender: TObject; Tag: TTag;
            		   const TagString: String; TagParams: TStrings; var ReplaceText: String);
        public
			function   HandleRequest() : string;
    		end;

	cHttpServer = class( TIdHTTPServer )
    	constructor  Create( log : apLog; data : cMirrorDB );
        destructor   Destroy;  override;
        public
            Root : string;
            DefaultDoc : string;
            procedure  Initialize;
            procedure  LogInOK( const id, access : string );
        private
            oSessions : TList< cSession >;
            oSessionsLock : TMultiReadExclusiveWriteSynchronizer;
            mSessionContext : aContext;
            mSessionKeepAlive : int;
            pLog : apLog;
            mSessions : int;
        	mActive : boolean;
            procedure  SetActive( act : boolean );    reintroduce;
            procedure  WrLog( const txt : string );
            procedure  HTTPCommandGet(AContext: TIdContext;
                		ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
        public
        	property   ServerActive : boolean  read mActive  write SetActive;
        end;


implementation

uses
	ASCII, uUtils, uXmlParser, uFeedMain, uPoller, IdCookie;


const
	EdDepart = 'EdDepart';
    HTTP_ID = 'HTTP_Server';
    tagSTdate = tagST + 'date';
    tagETdate = tagET + 'date';
    Version = 'vers 0.80';
    SessionCookie = 'FIDS_SesID';
    KeepAliveSec = 2 * 3600;  // 2 hours  default


procedure  cSession.WrLog( const txt : string );

	begin
    if Assigned( mContext.Log ) then  mContext.Log( txt );
    end;


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


function   cSession.ParseParam( const param : string;  out val : string ) : string;  // Ports=SYD

    var
        p : int;
	begin
    result := '';
    p := Pos( '=', param );
    if ( p > 0 ) then  begin
        result := Copy( param, 1, p - 1 );
        val := Copy( param, p + 1, 10000 );
        end;
    end;


function   cSession.ParseParamFF( const param : string;  out val : string ) : aFlightField;  // Ports=SYD

    var
        fld : string;
	begin
    fld := ParseParam( param, val );
    result := oFlight.FieldID( fld );
    end;


procedure  cSession.ProcessLogIn( params : TStrings );

    var
    	usr, pw: string;
	begin
    usr := params.Values[ 'UserID' ];   // exploits  'UserID=GT' parameter format
    pw := params.Values[ 'PassWord' ];
    if ( usr <> '' ) and ( pw <> '' ) then  begin
        mUsrID := 'HTTP_' + usr;
    	fFeedMain.LogIn( usr, pw, mUsrID );  // send login request to FIDSxml
        mLastLoginSeconds := Seconds;
    	end;
   end;


procedure  cSession.ProcessCICParams( params : TStrings );

    var
        param, val, field, ciList : string;
        fltNo, x : int;
	begin
    if oReqDoc.Count >= 2 then  begin  // EditCIC_3 for check in 3
        oCIC.Name := oReqDoc[ 1 ];  // check in name from request doc URL
        for param in params do  begin
        	field := ParseParam( param, val );
    		if Pos( 'Flight', field ) = 0 then  begin
            	if Pos( 'Record', field ) = 0 then  oCIC.FieldUpdate( field, val );
	            end
            else  begin
                x := 7;  // 'Flight3  want the 3 ie 3rd flight in list
                fltNo := GetInt( field, x );
                if val <> mPrevCIFlight[ fltNo ] then  begin  // something changed
                    if mPrevCIFlight[ fltNo ] <> '' then begin   // remove from a list
                        x := oFlightList.IndexOf( mPrevCIFlight[ fltNo ] );
                        if x >= 0 then  begin
                            oFlight.DBNode := mContext.Data.GetNode( oFlightPath[ x ] );
                            ciList := oFlight.Presentation[ ffCheckIns ];
                            ciList := ExcludeFromList( oReqDoc[ 1 ], ciList );
                            //oCIC.FieldDelta( oFlight.DbNode, tagCheckIns, ciList );  // set new CI list
                            oFlight.Presentation[ ffCheckIns ] := ciList;
                        	end;
                    	end;
                    if val <> '' then  begin   // add to a list
                        x := oFlightList.IndexOf( val );
                        if x >= 0 then  begin
                            oFlight.DBNode := mContext.Data.GetNode( oFlightPath[ x ] );
                            ciList := oFlight.Presentation[ ffCheckIns ];
                            ciList := IncludeInList( oReqDoc[ 1 ], ciList );
                            // oCIC.FieldDelta( oFlight.DbNode, tagCheckIns, ciList );  // set new CI list
                            oFlight.Presentation[ ffCheckIns ] := ciList;
                        	end;
                    	end;
	                end;
            	end;
	    	end;
        oCIC.SortOutTheHorribleMess;   // turn settings into style info for formatter / DisplayConfig.xml
        WrLog( 'EDIT Check In Control ' + oCIC.Name );
	    end;
    end;


procedure  cSession.ProcessParams( op : aOperation;  params : TStrings );

    var
        param, val : string;
        x, c : int;
        field : aFlightField;
        pf : apNode;
	begin
    oFlight.Clear;
    pf := oFlight.DBNode;
    if ( op = opNew ) or ( op = opNewSubFlt )  then  oFlight.DBNode := nil;  // disable updates
    c := params.Count;
    for x := 0 to c - 1 do  begin
        param := params[ x ];
        field := ParseParamFF( param, val );
        if field <> ffNone then  begin
        	oFlight.Presentation[ field ] := val;   // collect field vals
            end;
        end;
    if op = opNew  then  begin
    	oFlight.New;
        WrLog( 'NEW ' + oFlight.Presentation[ ffFlight ] );
	    end
    else if op = opNewSubFlt then  begin
    	// oFlight.DBNode := pf;   // need base flight to add to now
    	oFlight.NewCodeShare( pf );
        oFlight.DBNode := nil;
        WrLog( 'NEW code share ' + oFlight.Presentation[ ffFlight ] );
	    end
    else if op = opEdit then  WrLog( 'EDIT ' + oFlight.Presentation[ ffFlight ] );
    end;


function   cSession.CheckInControlTable : string;

	var
        resourceList : TStringList;
        resource, r : string;
	begin   //cols = CheckIn - Airline - Style - Full Graphic - Mid Graphic - Footer Graphic - Numb Flights - Closed/Default Graphic
    r := '';
    // use list of checkins
    resourceList := LoadContentList( FollowPath( 'SystemConfig|' + tagCheckins, mContext.Data.GetRoot ) );
    if resourceList <> nil then  begin
        r := '<tr class="CIC"> <th> Check In </th> <th> Airline </th> <th class="CICwideCol"> Layout Style </th> <th> Numb Flights </th> <th> Full Graphic </th>' + EOL;
        r := r + '<th> Header Graphic </th> <th> Body Graphic </th> <th> Footer Graphic </th> <th> Default </th> </tr>' + EOL;    // table heading
        for resource in resourceList do  begin   // for each row
            r := r + '<tr>';
            oCIC.Name := resource;
            r := r + '<td> <a href=EditCIC_' +  resource + '> ' + resource + '</a></td>';
            r := r + '<td>' + oCIC.DB[ cicAirline ] + '</td>';
            r := r + '<td>' + oCIC.DB[ cicStyle ] + '</td>';
            r := r + '<td>' + IntToStr( oCIC.NumbFlights ) + '</td>';
            r := r + '<td>' + TrimFileName( oCIC.DB[ cicFullScrn ] ) + '</td>';
            r := r + '<td>' + TrimFileName( oCIC.DB[ cicHeaderScrn ] ) + '</td>';
            r := r + '<td>' + TrimFileName( oCIC.DB[ cicBodyScrn ] ) + '</td>';
            r := r + '<td>' + TrimFileName( oCIC.DB[ cicFooterScrn ] ) + '</td>';
            r := r + '<td>' + TrimFileName( oCIC.DB[ cicDefaultGraphic ] ) + '</td>';
            r := r + '</tr>' + EOL;
            end;
        end;
    result := r;
    end;


function   cSession.SummaryTable( p : string ) : string;

    const
        nCols = 8;
	var
        //pfkey, pflt : apNode;
        //x, f,
        col : int;
        displaylist : cFlightList;
        resourceList : TStringList;
        resource, linkStub, r : string;
        field : aFlightField;
    begin
    field := oFlight.FieldID( p );
    if field <> ffNone then  begin
        if mBase = tagDepartures then  linkStub := 'Dep_'
        else if mBase = tagArrivals then  linkStub := 'Arr_';
        // use list of gates etc
        resourceList := LoadContentList( FollowPath( 'SystemConfig|' + oFlight.DBTag[ field ], mContext.Data.GetRoot ) );

        if resourceList <> nil then  begin
        	displaylist := cFlightList.Create( mContext.Data );
			r := '<tr> <th> ' + oFlight.Title[ field ] + '</th> <th> Flights </th>';    // table heading
            for col := 3 to nCols do  r := r + ' <th/>';
            r := r + '</tr>' + EOL;

            for resource in resourceList do  begin   // for each gate
                //displaylist := oFlight.List( field, resource );
                displaylist.Build( oFlight.Kind, field, resource );
                // fill out grid row
                r := r + '<tr> <td> '+ resource + ' </td>';  // gate name
                for col := 2 to nCols do  begin
                	r := r + '<td> ';
                    if col - 1 <= displaylist.Count then  begin
                    	oFlight.DbNode := displaylist[ col - 2 ];  // connect to db flight
                    	r := r + '<a href=Edit' + linkStub + oFlight.DbPath + '>'
                        + oFlight.Presentation[ ffFlight ] + ' </a>';
                    	end;
                	r := r + ' </td> ';
                    end;
                end;
        	displaylist.Free;
            end;
	    end;
    result := r;
    end;

var
    DepFields : array [ 0..6 ] of aFlightField = ( ffFlight, ffPorts, ffSTime, ffETime, ffDStatus, ffCheckIns, ffGates );
    ArrFields : array [ 0..5 ] of aFlightField = ( ffFlight, ffPorts, ffSTime, ffETime, ffAStatus, ffBelts );


function   cSession.FillOutTable( p : string ) : string;

    var
        field : aFlightField;
        fieldList : array of aFlightField;
        pfk, pflt, psubflt, pdep : apNode;
        f, s, x : int;
        r, ports, prevPorts, linkStub : string;
	begin
	r := '';
    if p = tagDepartures then  begin
        SetLength( fieldList, Succ( High( DepFields ) ) );
        for f := 0 to High( DepFields ) do   fieldList[ f ] := DepFields[ f ];
        linkStub := 'Dep_';
    	end
    else if p = tagArrivals then  begin
        SetLength( fieldList, Succ( High( ArrFields ) ) );
        for f := 0 to High( ArrFields ) do   fieldList[ f ] := ArrFields[ f ];
        linkStub := 'Arr_';
    	end
    else  if p = 'CheckInControl' then  r := CheckInControlTable
    else  r := SummaryTable( p );
    if linkStub <> '' then  begin
        pdep := FollowPath( mBase, mContext.Data.GetRoot );
        f := -1;     //n := 1;
        while EachSubNode( pdep, f, pfk ) do  begin   // each flight key
            s := -1;
            pflt := FindName( pfk, 'Flights' );
            prevPorts := '';
            while EachSubNode( pflt, s, psubflt ) do  begin  // each code share ( subflight )
                oFlight.DbNode := psubflt;
                if ( f = 0 ) and ( s = 0 ) then  begin  // heading
                    r := r + '<tr>';
                    for field in fieldList do  begin
                        r := r + '<th>' + oFlight.Title[ field ] + '</th>';
                        end;
                    r := r + '</tr>' + EOL;
                    end;
                if Odd( f ) then  r := r + '<tr class="odd">'  else  r := r + '<tr>';   // alternate line colour

                r := r + '<td><a href=Edit' + linkStub + oFlight.DbPath + '>'   // eg  href=EditDep_...path...
                       + oFlight.Presentation[ ffFlight ] + '</a></td>';   // label link

                for x := 1 to High( fieldList ) do  begin
                    field := fieldList[ x ];
                    if field = ffPorts then  begin
                        ports := oFlight.Presentation[ ffPorts ];
                        if ports <> prevPorts then  begin
                            prevPorts := ports;
                            r := r + '<td>' + ports + '</td>';
                            end
                        else  r := r + '<td/>';
                        end
                    else  if ( s = 0 ) or ( field in CodeShareField ) then  begin
                        r := r + '<td>' + oFlight.Presentation[  field ] + '</td>';
                        end
                    else  r := r + '<td/>';   // most of code share is empty
                    //Inc( n );
                    end;
                r := r + '</tr>' + EOL;
                end;
            end;
	    end;
    result := r;
    end;


function   Option( const itm : string; sel : boolean ) : string;

	begin
    if sel then  result := '<option selected> ' + itm +  '</option>' + EOL
    else  result := '<option> ' + itm +  '</option>' + EOL;
    end;


function   cSession.DbOptions( const list, sel : string ) : string;

    var      // eg <#DBoptions SystemSettings|Strings|StatusD DStatus>
        plist : apNode;
        sl : TStringList;
        x : int;
        itm, selected : string;
        field : aFlightField;
	begin
    plist := FollowPath( list, mContext.Data.GetRoot );
    x := 1;    result := '';
    if Pos( 'CIC', oReqDoc[ 0 ] ) = 0 then  begin  // flight data
        field := oFlight.FieldID( sel );
        selected := oFlight.Presentation[ field ];
    	end
    else  selected := oCIC.FieldValue( sel );

    sl := BuildParamsL( NodeContent( plist ), x );
    for itm in sl do  begin
    	result := result + Option( itm, itm = selected );
	    end;
    sl.Free;
    end;


function   cSession.FlightList( const list, sel : string ) : string;   // <#Flights CIC 3>  produce a drop down of flights
                                                                       // ie 3rd flight assigned to current CIC
    var
        flights : cFlightList;
        //flightNames : TStringList;
        x, fltNo, index : int;
        flt : apNode;

	begin
    result := '';
    if TryStrToInt( sel, fltNo ) then  begin
        if ( list = 'CIC' ) and ( fltNo <= MaxCIFlights ) then  begin  // Check In control flight list [1..4]
            if oReqDoc.Count >= 2 then  begin  // editing a CIC
            	oFlightPath.Clear;
                // flights := oFlight.List( ffNone, '' );   // list all flights
                flights := cFlightList.Create( mContext.Data );
                flights.Build( oFlight.Kind, ffNone, '' );
                oFlightList.Free;  oFlightList := flights.FlightNamesN;         {$HINTS OFF kill 'flt not used' hint mesg }

                for flt in flights do  begin
                    oFlightPath.Add( flights.Flight.DbPath );        // parallel list of flight paths
                	end;

                result := Option( '', false );  // need a blank option to delete flight from list
                index := 1;
                for x := 0 to oFlightList.Count - 1 do   begin
                    oFlight.DbNode := flights[ x ];
                    if oFlight.Match( ffCheckIns, oReqDoc[ 1 ] ) then  begin   // flight uses this CI
                    	result := result + Option( oFlightList[ x ], index = fltNo );   // 'Flight3' only wants 3rd matching flight
                        if index = fltNo then  mPrevCIFlight[ fltNo ] := oFlightList[ x ];  // and save to handle delta
                    	Inc( index );
	                    end
                    else  result := result + Option( oFlightList[ x ], false );
                    end;
                flights.Free;
                end;
            end;
    	end;
    end;                                                                        {$HINTS ON }


procedure  cSession.EHTMLtagHandler(Sender: TObject; Tag: TTag;
  const TagString: String; TagParams: TStrings; var ReplaceText: String);

	var
  		param, param1: string;
        n : int;
        l : cFlightList;
        pflt : apNode;
        //p : int;
    begin
    if TagParams.Count >= 1 then  param := TagParams[ 0 ];
    if TagParams.Count >= 2 then  param1 := TagParams[ 1 ];
    if TagString = 'Date' then
    	ReplaceText := DateToStr(Now)
    else if TagString = 'Time' then
    	ReplaceText := TimeToStr(Now)
    else if TagString = 'DateTime' then
    	ReplaceText := DateTimeToStr(Now)
    else if TagString = 'Server' then
    	ReplaceText := 'Indi/Alphasoft'  // IdHTTPServer1.ServerSoftware;
    else if TagString = 'Context' then  begin    // <#Context Arrivals>
        mBase := param;
        if param = tagArrivals then  oFlight.Kind := fkArrivals
        else if param = tagDepartures then  oFlight.Kind := fkDepartures;
    	//ReplaceText := '';
	    end
    else if TagString = 'Heading' then  begin
        if param = 'CIC' then  begin
            if oReqDoc.Count >= 2 then  ReplaceText := oReqDoc[ 1 ]; // oCIC.Name
        	end
    	else  begin
        	ReplaceText := UpperCase( mBase );
        	if oFlight.DbNode <> nil then ReplaceText := ReplaceText + ' &nbsp &nbsp ' + oFlight.Presentation[ ffFlight ];
	        end;
	    end
    else if TagString = 'Include' then  begin    // <#Include MainNav.html>
        if ( Length( param ) > 0 ) and ( param[ 1 ] <> '\' ) then  Insert( '\', param, 1 );
    	ReplaceText := FileToStr( mContext.Root + param );
	    end

    else if TagString = 'FillOutTable' then  begin
    	if TagParams.Count >= 1 then  ReplaceText := FillOutTable( TagParams[ 0 ] );
	    end
    else if TagString = 'DB' then  begin
        ReplaceText := '';
        for n := 0 to TagParams.Count - 1 do  begin
            if n > 1 then  ReplaceText := ReplaceText + ' ';
            param := TagParams[ n ];
        	ReplaceText := ReplaceText + NodeContent( FollowPath( param, mContext.Data.GetRoot ) );
            end;
    	end
    else if TagString = 'Data' then  begin  // eg <#Data ST>
        ReplaceText := '';
        for n := 0 to TagParams.Count - 1 do  begin
            if n > 1 then  ReplaceText := ReplaceText + ' ';
            param := TagParams[ n ];
            ReplaceText := ReplaceText + oFlight.Presentation[ oFlight.FieldID( param ) ];
            end;
    	end
    else if TagString = 'DBoptions' then  begin // <#DBoptions SystemSettings|Strings|StatusD>
        if TagParams.Count >= 2 then  ReplaceText := DbOptions( param, param1 );
	    end
    else if TagString = 'Flight' then  begin // <#Flights CIC 3>  produce a filtered flight list
        if TagParams.Count >= 2 then  ReplaceText := FlightList( param, param1);
	    end
    else if TagString = 'CIC' then  begin
        if param = 'Flights' then  begin
            // l := oFlight.List( ffCheckIns, param1 );
            l := cFlightList( mContext.Data );
            l.Build( oFlight.Kind, ffCheckIns, param1 );
            if l.Count > 0 then  begin
                for pflt in l do  begin
                	oFlight.DbNode := pflt;
                    ReplaceText := ReplaceText + '<a href=EditDep_' + oFlight.DbPath + '>'
                        + oFlight.Presentation[ ffFlight ] + ' </a> &nbsp ';
                	end;
                ReplaceText := ReplaceText + EOL;
            	end;
        	l.Free;
        	end;
    	end;
    end;


function   cSession.HandleEditParams( const doc : string ) : string;  // eg doc = '/EditArr'

	var
        c : Integer;
        path : String;
	begin
    result := doc;
    if doc = '/LogIn' then  begin    // if mDocState = dsLogin then  begin
        ProcessLogIn( mRequestInfo.Params );
        mDocState := dsNone;
        result := '\' + mBase;
    	end
    else if doc = '/EditCIC' then  begin
        ProcessCICParams( mRequestInfo.Params );
        result := '/CheckInControl';  // flow back to CIC page
    	end
    else  begin
        if oReqDoc.Count >= 2 then  begin
            path := oReqDoc[ 1 ];
            result := oReqDoc[ 0 ];
            if Pos( 'Arr', result ) > 0 then  begin
                mBase := tagArrivals;
                oFlight.Kind := fkArrivals;
                end
            else  begin     // Dep and CIC
                mBase := tagDepartures;
                oFlight.Kind := fkDepartures;
                end;
            oFlight.DbNode := FollowPath( path, mContext.Data.GetRoot );
            c := mRequestInfo.Params.Count;
            if c > 0 then  begin  // an info post - ie DB write
                if Pos( 'Delete', mRequestInfo.Params[ c - 1 ] ) = 1 then  DeleteFlight
                else  if Pos( 'NewFlight', mRequestInfo.Params[ c - 1 ] ) = 1 then  ProcessParams( opNew, mRequestInfo.Params )
                else  if Pos( 'AddSubflight', mRequestInfo.Params[ c - 1 ] ) = 1 then  ProcessParams( opNewSubFlt, mRequestInfo.Params )
                else  if Pos( 'Record', mRequestInfo.Params[ c - 1 ] ) = 1 then  ProcessParams( opEdit, mRequestInfo.Params );
                end;
            end;
        result := '\' + mBase;  //'.';  return default table page
        oFlight.DbNode := nil;  // release flight
        end;
    end;


procedure  cSession.Handler;    // main thread to handle dynamic / db content
                                // eg http://localhost/EdDepart_Departures|LT681-49|Flights|GT681
	var
        doc : String;
        EHTMLParser: TPageProducer;
	begin
    if ( oReqDoc <> nil ) and ( oReqDoc.Count > 0 ) then  begin
    	doc := oReqDoc[ 0 ];
        if mRequestInfo.Params.Count > 0 then  begin  // a post
            if Pos( 'Edit', doc ) > 0 then  begin
                doc := HandleEditParams( doc );
                oReqDoc.Clear;
                end;
        	end;

        if doc = '.' then  begin
            mResult := doc;   // finished with DB session - hand back to reader thread
	        end
        else  begin
            try  begin
            if Pos( 'Edit', doc ) > 0 then  begin
                if not mLoggedIn then  begin
                    mDocState := dsLogin;
					doc := '\LogIn';
                	end;
                end;
                WindowsPath( doc );  if doc = '' then  doc := '\';
                if doc[ Length( doc ) ] = '\' then  begin  // requested a directory
                    doc := doc + mContext.DefaultDoc;
                    end;
                if Pos( '.', doc ) = 0 then  doc := doc + '.ehtml';  // default ext
                doc := mContext.Root + doc;
                if FileExists( doc ) then  begin
                    if LowerCase(ExtractFileExt( doc )) = '.ehtml' then  begin
                        if oReqDoc.Count >= 2 then  begin
                            if Pos( 'CIC', oReqDoc[ 0 ] ) = 0 then  begin
                            	oFlight.DbNode := FollowPath( oReqDoc[ 1 ], mContext.Data.GetRoot );
	                            end
                            else oCIC.Name := oReqDoc[ 1 ];
	                        end;
                        // Extended HTML - send through internal tag parser
                        EHTMLParser := TPageProducer.Create( nil );  // <#TagName Param1=Value1 Param2=Value2 ...>
                        try
                            EHTMLParser.HTMLFile := doc;  // set source file name
                            EHTMLParser.OnHTMLTag := EHTMLtagHandler;  // set event handler
                            mResponseInfo.ContentText := EHTMLParser.Content; // parse !
                        finally
                            EHTMLParser.Free;
                            end;
                        end
                    else  begin
                        // return file as-is
                        // open file stream
                        mResponseInfo.ContentStream := TFileStream.Create( doc, fmOpenRead or fmShareDenyWrite );
                        end;
                    end;
                end;
            finally
     			mResult := doc;    // finished with DB content - hand back to reader thread
               end;
            end;
	    end;
    end;

//_____________________________ reader threads _________________________________


constructor cSession.Create( id : string; cont : apContext );

	begin
    mID := id;
    mLastActiveSeconds := Seconds;
    mContext := cont;
    mLoggedIn := not cont.LogInRequired;
    oFlight := cFlight.Create( cont.Data, HTTP_ID );
    oCIC := cCheckInControl.Create( cont.Data, HTTP_ID );
    oFlightList := TStringList.Create;
    oFlightPath := TStringList.Create;
    mBase := tagDepartures;
    end;


destructor  cSession.Destroy;

	begin
    FreeAndNil( oFlight );
    FreeAndNil( oCIC );
    FreeAndNil( oReqDoc );
    FreeAndNil( oFlightList );
    FreeAndNil( oFlightPath );
    inherited;
    end;


function   cSession.HandleRequest() : string;

	var
        p : Integer;
        doc : String;
	begin
    result := '';
    if mRequestInfo <> nil then  begin

        try  begin
        	p := 1;
            oReqDoc := BuildParamsL( mRequestInfo.Document, p, '_' );
        	if oReqDoc.Count > 0 then  doc := oReqDoc[ 0 ];
            if ( doc = '' ) or ( doc[ 1 ] <> '/' ) then  begin
                // invalid request
                raise Exception.Create('invalid request: ' + doc);
                end;
            WindowsPath( doc );  // / -> \
            if doc[ Length( doc ) ] = '\' then  begin  // requested a directory
                doc := doc + mContext.DefaultDoc;
                end;
            if Pos( '.', doc ) = 0 then  doc := doc + '.ehtml';  // default ext
            doc := mContext.Root + doc;

            if ( Pos( '.ehtml', doc ) > 0 ) or ( mRequestInfo.Params.Count > 0 ) then  begin  // db stuff so handle in main thread
                mResult := '';
                Poller.DoInMainThread( Handler, true );   // Synchronize( Handler );
                while  mResult = '' do  Sleep( 10 );
                result := mResult;
                end

            else if FileExists( doc ) then  begin    // just a static file so safe to handle in this thread
                mResponseInfo.ContentStream := TFileStream.Create( doc, fmOpenRead or fmShareDenyWrite );
                result := doc;
                end;
            end;
        finally
            if Assigned(mResponseInfo.ContentStream) then  begin
                // response stream does exist
                // set length
                mResponseInfo.ContentLength := mResponseInfo.ContentStream.Size;
                // write header
                mResponseInfo.WriteHeader;
                // return content
                mResponseInfo.WriteContent;
                // free stream
                mResponseInfo.ContentStream.Free;
                mResponseInfo.ContentStream := nil;
                end
            else  if mResponseInfo.ContentText <> '' then  begin
                // set length
                mResponseInfo.ContentLength := Length(mResponseInfo.ContentText);
                // write header
                mResponseInfo.WriteHeader;
                // return content
                end
            else  if mResult = '.' then   begin
            	mResponseInfo.Redirect( '.' );   // redirect browser to directory default
                end

            else  begin
                if not mResponseInfo.HeaderHasBeenWritten then  begin
                    // set error code
                    mResponseInfo.ResponseNo := 404;
                    mResponseInfo.ResponseText := 'Document not found';
                    // write header
                    mResponseInfo.WriteHeader;
                    end;
                WrLog( '404 - Document not found' );
                mResponseInfo.ContentText := 'The document requested is not available.';
                mResponseInfo.WriteContent;
                end;
            end;
        // return content
        end;
    end;


//procedure cHttpServer.HttpStart(Sender: TIdHTTPSession);
//
//	begin   // never gets here
//    oSessions.Add( cSession.Create( Sender.SessionID, @ mSessionContext ) );
//    end;



		// incoming client request for download - get or post
procedure cHttpServer.HTTPCommandGet(AContext: TIdContext;
  		ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    var
        resp, sesID : string;
        ses, s : cSession;
        cook : TIdNetscapeCookie;
        x : int;
        guid : TGuid;
        garbCollect : boolean;
	begin
    ses := nil;    garbCollect := false;
    cook := aRequestInfo.Cookies.Cookie[ SessionCookie ];  // cookie based session mgt
    if cook <> nil then  begin
        sesID := cook.Value;
        oSessionsLock.BeginRead;
        try  begin                                         // active cookie / session ?
            for x := 0 to oSessions.Count - 1 do  begin
                s := oSessions[ x ];
                if s <> nil then  begin
                    if Seconds - s.mLastActiveSeconds > mSessionKeepAlive then  begin  // check for garbage collect old sessions
                    	garbCollect := true;     //
                        end
                    else if s.mID = sesID then  begin  // an active session with matching id string
                        s.mLastActiveSeconds := Seconds;
                        ses := s;
                        break;
                        end;
                    end;
                end;
        	end;
        finally
        	oSessionsLock.EndRead;
        	end;
        end;

    if ses = nil then  begin  // create a new session / cookie
        Inc( mSessions );
        CreateGUID( guid );
        ses := cSession.Create( GUIDToString( guid ), @ mSessionContext );
        WrLog( 'New Session: ' + ARequestInfo.RemoteIP );
    	aResponseInfo.Cookies.AddSrcCookie( SessionCookie + '=' + ses.mID );
        s := ses;
        oSessionsLock.BeginWrite;
        try  begin
            for x := 0 to oSessions.Count - 1 do  begin   // stick it into list
                if  oSessions[ x ] = nil then  begin oSessions[ x ] := s;  s := nil;  break;  end;
                end;
            if s <> nil then  oSessions.Add( s );
	        end;
        finally
            oSessionsLock.EndWrite;
            end;
        end;
    ses.mTaskContext := aContext;
    ses.mRequestInfo := aRequestInfo;
    ses.mResponseInfo := aResponseInfo;
    resp := ses.HandleRequest;

    if garbCollect then  begin
        oSessionsLock.BeginWrite;
        try  begin
            for x := 0 to oSessions.Count - 1 do  begin
                s := oSessions[ x ];
                if s <> nil then  begin
                    if Seconds - s.mLastActiveSeconds > mSessionKeepAlive then  begin  // garbage collect old sessions
                        oSessions[ x ] := nil;
                        s.Free;
                        end;
                    end;
                end;
            end;
        finally
            oSessionsLock.EndWrite;
            end;
    	end;
  	end;


// _____________________________ main thread ___________________________________


constructor  cHttpServer.Create( log : apLog; data : cMirrorDB );

	begin
    inherited Create( nil );
    oSessions := TList< cSession >.Create;
    oSessionsLock := TMultiReadExclusiveWriteSynchronizer.Create;
    pLog := log;
    DefaultPort := 80;
    OnCommandGet := HTTPCommandGet;
    //OnSessionStart := HttpStart;
    mSessionContext.Log := WrLog;
    mSessionContext.Data := data;
    mSessionContext.Root := ExtractFilePath(Application.ExeName) + 'WebSite';
    mSessionContext.DefaultDoc := 'index.ehtml';
    ServerSoftware := 'Indy/Alphasoft';
    ForceDirectories( mSessionContext.Root );
    //AutoStartSession := true;
    end;


destructor   cHttpServer.Destroy;

	begin
    SetActive( false );
    inherited;
    end;


procedure  cHttpServer.WrLog( const txt : string );

	begin
    if Assigned( pLog ) then  pLog( 0, ServerSoftware + '  ' + txt );
    end;


procedure  cHttpServer.SetActive( act : boolean );

    var
        line : string;
	begin
    if act <> Active then  begin
        Active := act;
        if Active then  begin
        	line := uHttpServer.Version + ' is ACTIVE port:' + IntToStr( DefaultPort );
            if mSessionContext.LogInRequired then  line := line + ' with log in';
        	WrLog( line );
	        end
        else  WrLog( ' is not active' );
	    end;
	end;


procedure  cHttpServer.LogInOK( const id, access : string );

	var
    	x : int;
        s : cSession;
    begin
    oSessionsLock.BeginRead;
    try  begin
        for x := 0 to oSessions.Count - 1 do  begin
            s := oSessions[ x ];
            if s <> nil then  begin
                if s.mUsrID = id then  begin  // an active session with matching user id string
                    if ( Seconds - s.mLastLoginSeconds < 3 ) and ( Seconds - s.mLastLoginSeconds >= 0 ) then  begin
                    	s.mLoggedIn := true;
                        s.mAccess := access;
                    	break;
                    	end;
                    end;
                end;
            end;
        end;
    finally
        oSessionsLock.EndRead;
        end;
    end;


procedure  cHttpServer.Initialize;

    var
        pt : apNode;
        p : int;
	begin
    pt := FollowPath( 'SystemConfig|HttpServer', mSessionContext.Data.GetRoot );
    if pt <> nil then  begin
        ServerSoftware := 'HTTP';
        if TryStrToInt( ReadContent( pt, 'Port' ), p ) then  DefaultPort := p;
        mSessionKeepAlive := KeepAliveSec;
        if TryStrToInt( ReadContent( pt, 'SessionKeepAliveMin' ), p ) then  mSessionKeepAlive := p * 60;
    	mSessionContext.LogInRequired := FindName( pt, 'LogInRequired' ) <> nil;
        SetActive( true );
        end;
    fFeedMain.LogIn( HTTP_ID, SysPW, HTTP_ID );
    end;


end.
