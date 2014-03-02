unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids,
  uGlobalDefs, uPoller, uGT, uMessageHub, uDbTree, uMirrorDB, uFidsTags;

type
  TfMain = class(TForm)
    sgFlights: TStringGrid;
    MainMenu1: TMainMenu;
    Departures1: TMenuItem;
    Gates1: TMenuItem;
    Bays1: TMenuItem;
    CheckIns1: TMenuItem;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgFlightsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Departures1Click(Sender: TObject);
    procedure Gates1Click(Sender: TObject);
    procedure Bays1Click(Sender: TObject);
    procedure CheckIns1Click(Sender: TObject);
    procedure sgFlightsDblClick(Sender: TObject);
    procedure sgFlightsClick(Sender: TObject);
  private
        oDataTree : cMirrorDB;
        oHub : cMessageHub;
        mRow, mCol : int;
        oRowList : TStringList;

        mSortCol : int;
        mSortAscend : boolean;

        mSummaryField : aFlightField;

        mFormType : ( ftDepartures, ftSummary );
		mShutDown : boolean;
	procedure   MessageReader( const mesg : string; link : apLinkID = nil );  // aReader
	procedure   NewData( const xml : string );
	procedure   LogIn( const usr, pw, id : string );
	procedure   NewConnection;
	procedure   Connect;
	procedure   Log( ErrorNo : integer; const s : string );
	procedure   fOnIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

uses
	uPacket, uXmlParser, uValidation, ufStringEntry;

const
	UI_ID = 'GUI_ID';
	Server = 'FIDSxml';
	FeedID = 'Feed';
    SysPW = '_DI_system_';

var
    Fields : array [ 0..6 ] of aFlightField = ( ffFlight, ffPorts, ffST, ffET, ffDStatus, ffGates, ffCheckIns );
    // adjustable list of displayed fields


procedure   TfMain.Log( ErrorNo : integer; const s : string );

	begin
	end;


procedure   TfMain.NewData( const xml : string );   // any time the data base changes

	begin
    FormPaint( nil );
    end;


procedure TfMain.sgFlightsClick(Sender: TObject);

    begin
    if mRow = 0 then  begin  // sort control
        if mCol = mSortCol then  mSortAscend := not mSortAscend
        else      mSortCol := mCol;
    	FormPaint( Sender );
	    end;
    end;


procedure TfMain.sgFlightsDblClick(Sender: TObject);    // edit any flight, any field

    var
        pflt : apNode;
        fld : aFlightField;
    begin
    if ( oRowList <> nil ) and ( mRow > 0 ) and ( mRow <= oRowList.Count ) and ( mCol < High( Fields ) ) then  begin
        pflt := FollowPath( oRowList[ mRow - 1 ], oDataTree.GetRoot );
        if pflt <> nil then  begin
        	fld := Fields[ mCol ];
            if fStrEntry.GetStr( PresentationData( pflt, ffFlight ) + '   ' + FieldTitle( fld, GetFlightKind( pflt ) )
            	, PresentationData( pflt, fld ) ) = mrOK then  begin
                oDataTree.SendRequest( FlightDelta( pflt, fld, fStrEntry.Str, UI_ID ) );
            	end;
            end;
        end;
    end;


procedure TfMain.sgFlightsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);

	begin
	CanSelect := false;
    mRow := ARow;        // remember what cell we are pointing to
    mCol := ACol;
    end;


procedure   TfMain.MessageReader( const mesg : string; link : apLinkID = nil );  // aReader

	begin
	if mesg = '<' + TagShutDown + '/>' then  begin   // allows FIDSxml to shutdown GUI etc
		mShutDown := true;
		end;
	end;


procedure TfMain.Departures1Click(Sender: TObject);

    begin
    mFormType := ftDepartures;
    FormPaint( Sender );
    end;


procedure TfMain.Gates1Click(Sender: TObject);

    begin
    mFormType := ftSummary;
    mSummaryField := ffGates;
    FormPaint( Sender );
    end;


procedure TfMain.Bays1Click(Sender: TObject);

    begin
    mFormType := ftSummary;
    mSummaryField := ffBays;
    FormPaint( Sender );
    end;


procedure TfMain.CheckIns1Click(Sender: TObject);

    begin
    mFormType := ftSummary;
    mSummaryField := ffCheckIns;
    FormPaint( Sender );
    end;


procedure   TfMain.fOnIdle(Sender: TObject; var Done: Boolean);

	begin
    if mShutDown then  Close;
    end;


{
procedure TfMain.FormPaint(Sender: TObject);      // stage 1 - basic loop through all code shares

	var
        pfkey, pflt : apNode;
        x, f, row : int;
    begin
    if oDataTree <> nil then  begin
        x := -1;  row := 1;
        while EachSubNode( FindName( oDataTree.GetRoot, tagDepartures ), x, pfkey ) do  begin  // each flight key
            f := -1;
        	while EachSubNode( FindName( pfkey, tagFlights ), f, pflt ) do  begin   // each code share
            	sgFlights.Cells[ 0, row ] := pflt.NodeName;
                Inc( row );
	            end;
        	end;
    	end;
    end;
}


{var                // stage 2 - generic field IDs based on aFlightField uFidsTags and uValidation
    Fields : array [ 0..6 ] of aFlightField = ( ffFlight, ffPorts, ffST, ffET, ffDStatus, ffGates, ffCheckIns );  // adjustable list of displayed fields

procedure TfMain.FormPaint(Sender: TObject);

	var
        pfkey, pflt : apNode;
        x, f, row, col : int;
    begin
    if oDataTree <> nil then  begin
    	// column headings
        sgFlights.ColCount := High( Fields ) + 1;
        for col := Low( Fields ) to High( Fields ) do sgFlights.Cells[ col, 0 ] := FieldTitle( Fields[ col ], afkDepartures );
        // fill out grid rows
        x := -1;  row := 1;
        while EachSubNode( FindName( oDataTree.GetRoot, tagDepartures ), x, pfkey ) do  begin  // each flight key
            f := -1;
        	while EachSubNode( FindName( pfkey, tagFlights ), f, pflt ) do  begin   // each code share
                // fill out grid row columns
		        for col := Low( Fields ) to High( Fields ) do  begin
                    if ( col = 0 ) or( f = 0 ) or CodeShareField( Fields[ col ] ) then  begin  // hide redundant code share info
            			sgFlights.Cells[ col, row ] := PresentationData( pflt, Fields[ col ] );
                        end;
                	end;

                Inc( row );
	            end;
        	end;
    	end;
    end;   }



{

     // stage 3 - sort by any field - up or down
var
    Fields : array [ 0..6 ] of aFlightField = ( ffFlight, ffPorts, ffST, ffET, ffDStatus, ffGates, ffCheckIns );
    // adjustable list of displayed fields

procedure TfMain.FormPaint(Sender: TObject);

	var
        pfkey, pflt : apNode;
        x, f, row, col : int;
        displaylist : TList; // of apNode;
    begin
    if oDataTree <> nil then  begin
        x := -1;
        displaylist := TList.Create;     oRowList.Clear;
        while EachSubNode( FindName( oDataTree.GetRoot, tagDepartures ), x, pfkey ) do  begin  // each flight key
            f := -1;
        	while EachSubNode( FindName( pfkey, tagFlights ), f, pflt ) do  begin   // each code share
                // collect list of flights   could filter out flight types here eg only 'primary' or public or 'QF' etc
                displaylist.Add( pflt );
                end;
            end;


        SortFlights( displaylist, Fields[ mSortCol ], mSortAscend );  // generic sort selected flights by field up or down

    	// column headings
        sgFlights.ColCount := High( Fields ) + 1;
        for col := Low( Fields ) to High( Fields ) do sgFlights.Cells[ col, 0 ] := FieldTitle( Fields[ col ], afkDepartures );
        // fill out grid rows
        row := 1;
        for pflt in displaylist do  begin  // fill rows from filtered, sorted flight list

            for col := Low( Fields ) to High( Fields ) do  begin
                sgFlights.Cells[ col, row ] := PresentationData( pflt, Fields[ col ] );
                end;
            oRowList.Add( ResolvePathStr( pflt) );  // save path for doubl clk reference to edit form
            Inc( row );
            end;
        displaylist.Free;
    	end;
    end;
}



     // stage 3 - sort by any field - up or down

procedure TfMain.FormPaint(Sender: TObject);

	var
        pfkey, pflt : apNode;
        x, f, row, col : int;
        displaylist : TList; // of apNode;
        resourceList : TStringList;
        resource : string;
    begin
    for row := 0 to sgFlights.RowCount - 1 do  sgFlights.Rows[ row ].Clear;   // clear grid

    if ( oDataTree <> nil ) and ( mFormType = ftDepartures ) then  begin

        x := -1;
        displaylist := TList.Create;     oRowList.Clear;
        while EachSubNode( FindName( oDataTree.GetRoot, tagDepartures ), x, pfkey ) do  begin  // each flight key
            f := -1;
        	while EachSubNode( FindName( pfkey, tagFlights ), f, pflt ) do  begin   // each code share
                // collect list of flights   could filter out flight types here eg only 'primary' or public or 'QF' etc
                displaylist.Add( pflt );
                end;
            end;


        SortFlights( displaylist, Fields[ mSortCol ], mSortAscend );  // generic sort selected flights by field up or down

    	// column headings
        sgFlights.ColCount := High( Fields ) + 1;
        for col := Low( Fields ) to High( Fields ) do sgFlights.Cells[ col, 0 ] := FieldTitle( Fields[ col ], afkDepartures );
        // fill out grid rows
        row := 1;
        for pflt in displaylist do  begin  // fill rows from filtered, sorted flight list

            for col := Low( Fields ) to High( Fields ) do  begin
                sgFlights.Cells[ col, row ] := PresentationData( pflt, Fields[ col ] );
                end;
            oRowList.Add( ResolvePathStr( pflt) );  // save path for doubl clk reference to edit form
            Inc( row );
            end;
        displaylist.Free;
    	end

    // stage 4 - summary style - todo hold flight lists for dbl click flight edit straight from summary
    else  if ( oDataTree <> nil ) and ( mFormType = ftSummary ) then  begin    // stage 4 - summary form

        // column headings
        sgFlights.Cells[ 0, 0 ] := FieldTitle( mSummaryField, afkDepartures );
        sgFlights.Cells[ 1, 0 ] := 'Flights';

        // use list of gates etc
        resourceList := LoadContentList( FollowPath( 'SystemConfig|' + DBFieldName( nil, mSummaryField ), oDataTree.GetRoot ) );

        if resourceList <> nil then  begin
            row := 1;
            for resource in resourceList do  begin   // for each gate
                x := -1;
                displaylist := TList.Create;     oRowList.Clear;
                while EachSubNode( FindName( oDataTree.GetRoot, tagDepartures ), x, pfkey ) do  begin  // each flight key
                    f := -1;
                    while EachSubNode( FindName( pfkey, tagFlights ), f, pflt ) do  begin   // each code share
                        if resource = PresentationData( pflt, mSummaryField )  then  begin  // if uses resource  - todo Match(..) needs to extract from list
                            displaylist.Add( pflt );                                        // add to list
                            end;
                        end;
                    end;

                // SortFlights( displaylist, Fields[ mSortCol ], mSortAscend );  // generic sort selected flights by field up or down
                // already sorted by ST - use above to sort by other fields

                // fill out grid row
                if displaylist.Count > 0 then  begin
                    sgFlights.Cells[ 0, row ] := resource;  // gate name
                    col := 1;
                    for pflt in displaylist do  begin  // fill rows from filtered, sorted flight list
                        sgFlights.Cells[ col, row ] := PresentationData( pflt, ffFlight );
                        Inc( col );
                        end;
                    displaylist.Free;
                    Inc( row );
                    end;
                end;
            end;
	    end;
    end;


procedure   TfMain.NewConnection;

	var
        sl : TStringList;
	begin
    if ( oDataTree = nil ) and ( oHub.Connected ) then  begin
        oDataTree := cMirrorDB.Create( oHub, UI_ID, false, true, 13 );
        oDataTree.RegisterReader( NewData );
        sl := TStringList.Create;
        sl.Add( 'SystemConfig' );
        sl.Add( 'SystemSettings' );
        sl.Add( 'Arrivals' );
        sl.Add( 'Departures' );
        sl.Add( 'Timetable' );
        // don't need to load big IATA or Users stuff ...
        oDataTree.InitMirror( sl );  // maintain a local copy of the data base
        Login( UI_ID, SysPW, UI_ID );
        sl.Free;
        end;
    end;


procedure   TfMain.LogIn( const usr, pw, id : string );

	begin
    oHub.Broadcast( FormatLogIn( usr, pw, id ) );
    end;


procedure   TfMain.Connect;

	begin  // 192.168.0.163 : 1666    todo load systemconfig.xml   todo encrypt password?
    if oHub = nil then  begin
        oHub := cMessageHub.Create( Log );
        oHub.OnConnection := NewConnection;
        oHub.RegisterReader( MessageReader );
        oHub.InitConnection( [ alLocal, alPipe ], UI_ID, Server, '',
            	tcpNone, TStringList( nil ), 0 );
        Poller.OnTimeOut( 40,
        	procedure() begin  //aOnTimeOutProc
            	if not oHub.Connected then  begin
                    if not oHub.Master then  begin
                        FreeAndNil( oHub );
            			ShowMessage( 'Could not find server ' + Server );
                        end;
                    end
                else NewConnection;
                end
            );
        end;
    end;


procedure TfMain.FormCreate(Sender: TObject);

    begin
    Application.OnIdle := fOnIdle;
    oRowList := TStringList.Create;
    Connect;
    end;


end.
