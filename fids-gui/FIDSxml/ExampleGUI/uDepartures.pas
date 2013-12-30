unit uDepartures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, uMirrorDB, uFlight, Menus;

type
  TfDepartures = class(TForm)
    sgFlights: TStringGrid;
    MainMenu1: TMainMenu;
    AddFlight1: TMenuItem;
    AddCodeShare1: TMenuItem;
    DeleteFlight1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure sgFlightsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sgFlightsDblClick(Sender: TObject);
    procedure AddFlight1Click(Sender: TObject);
    procedure AddCodeShare1Click(Sender: TObject);
    procedure DeleteFlight1Click(Sender: TObject);
  private
        mDataTree : cMirrorDB;   // my copy of a pointer to the data tree DB
        oFlight : cFlight;       // generic flight manipulator
        oFlightList : cFlightList;    // generic list of flights for sorting and filtering
        oRowList : TStringList;       // list of flight path values eg '|Departures|QF123-666|Flights|GT123|'
		function	CheckError : boolean;
        procedure   NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

  public
    { Public declarations }
  end;

var
  fDepartures: TfDepartures;

implementation

{$R *.dfm}

uses
	ufStringEntry, uGUIMain, uFidsTags, uDbTree, uGT, uUtils;



function	TfDepartures.CheckError : boolean;

	begin
    result := true;
	if oFlight.Error <> feNone then  begin
    	ShowMessage( 'ERROR ' + EnumToStr( Ord( oFlight.Error ), TypeInfo( aFlightValidationError ) ) );
        result := false;
    	end;
    end;


procedure TfDepartures.AddCodeShare1Click(Sender: TObject);

	var
    	row : int;
        existingFlight : apNode;
    begin
    row := sgFlights.Selection.Top;   // assumes a valid existing flight selected
    if ( oRowList <> nil ) and ( row > 0 ) and ( row <= oRowList.Count ) then  begin
    	oFlight.DbPath := oRowList[ row - 1 ];   // connect flight to a particular flight node in DB ready to add code share
        existingFlight := oFlight.DbNode;
        if existingFlight <> nil then  begin
    		fStrEntry.Caption := 'Enter Code Share Name eg PJ123 to ' + oFlight.Presentation[ ffFlight ];
    		oFlight.DbNode := nil;   // disassociate oflight from any particular flight  to stop field updates
            if fStrEntry.GetStr( 'New code share', '' ) = mrOK then  begin   // get new value from user
    			oFlight.Clear;
                oFlight.Presentation[ ffFlight ] := fStrEntry.Str;       // set flight name
        		// .....set any other fields of interest....  destinations and checkins are unique to each code share
                oFlight.NewCodeShare( existingFlight );  // and post it
                end;
            end;
        end;
    end;


procedure TfDepartures.AddFlight1Click(Sender: TObject);

    begin
    fStrEntry.Caption := 'Enter Flight Name eg QF123';
    oFlight.DbNode := nil;   // disassociate oflight from any particular flight to stop field updates
    if fStrEntry.GetStr( 'New flight', '' ) = mrOK then  begin   // get new value from user
    	oFlight.Clear;
        oFlight.Presentation[ ffFlight ] := fStrEntry.Str;       // set flight name
        oFlight.Presentation[ ffST ] := DTtoStr( Now() );		// set a scheduled time likely to pass validation
        // .....set any other fields of interest....
        oFlight.Error := feNone;
        oFlight.New;  // and post it
        CheckError();
        end;
    end;


procedure TfDepartures.DeleteFlight1Click(Sender: TObject);

    var
        row : int;
    begin
    row := sgFlights.Selection.Top;
    if ( oRowList <> nil ) and ( row > 0 ) and ( row <= oRowList.Count ) then  begin
    	oFlight.DbPath := oRowList[ row - 1 ];   // connect flight to a particular flight node in DB
        if oFlight.DbNode <> nil then  begin
        	oFlight.Delete;
            end;
        end;
    end;


procedure TfDepartures.FormShow(Sender: TObject);

    begin
	if mDataTree = nil then  begin    // initialize
    	mDataTree := fGUIMain.GetDB;

        if mDataTree <> nil  then  begin
            mDataTree.RegisterReader( NewData );
            oFlight := cFlight.Create( mDataTree, fGUIMain.GetID() );   // need a logged in id ('EgGUI') to update DB
            oFlight.Kind := fkDepartures;
            oFlightList := cFlightList.Create( mDataTree );
            oRowList := TStringList.Create;
            NewData( '' );
        	end;
        end;
    end;


var
    Fields : array [ 0..6 ] of aFlightField = ( ffFlight, ffPorts, ffST, ffET, ffDStatus, ffGates, ffCheckIns );
    // adjustable list of displayed fields


procedure   TfDepartures.NewData( const xml : string ); // catch DB tree changes here

	var
    	col, row : integer;
        pflt : apNode;
	begin
    oRowList.Clear;
    // column headings
    sgFlights.ColCount := High( Fields ) + 1;
    for col := Low( Fields ) to High( Fields ) do sgFlights.Cells[ col, 0 ] := oFlight.Title[ Fields[ col ] ];
    // fill out grid rows
    oFlightList.Build( fkDepartures, ffPorts, '' );   // make a list of all departure flights - can filter or Sort
    sgFlights.RowCount := oFlightList.Count + 1;
    row := 1;
    for pflt in oFlightList do  begin
        oFlight.DbNode := pflt;  // redundant - avoids a warning
        for col := Low( Fields ) to High( Fields ) do  begin
            sgFlights.Cells[ col, row ] := oFlightList.Flight.Presentation[ Fields[ col ] ];
            end;
        oRowList.Add( oFlightList.Flight.DbPath );  // save path for later double clk reference to edit form etc
        Inc( row );
        end;
    end;


procedure TfDepartures.sgFlightsDblClick(Sender: TObject);

    var
        fld : aFlightField;
        row, col : int;
    begin
    fStrEntry.Caption := 'Edit Flight Field';
    row := sgFlights.Selection.Top;   col := sgFlights.Selection.Left;
    if ( oRowList <> nil ) and ( row > 0 ) and ( row <= oRowList.Count ) and ( col <= High( Fields ) ) then  begin
    	oFlight.DbPath := oRowList[ row - 1 ];   // connect flight to a particular flight node in DB
        if oFlight.DbNode <> nil then  begin
        	fld := Fields[ col ];
            if fStrEntry.GetStr( oFlight.Presentation[ ffFlight ] + '   ' + oFlight.Title[ fld ]
            	, oFlight.Presentation[ fld ] ) = mrOK then  begin   // get new value from user
                oFlight.Error := feNone;
                oFlight.Presentation[ fld ] := fStrEntry.Str;        // and globally store it into the DB tree
                CheckError();
            	end;
            end;
        end;
    end;


procedure TfDepartures.sgFlightsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);

	begin
	CanSelect := ARow > 0;
    end;

end.