unit uAbstractController;

interface

uses
  ASCII, uXmlParser, uUtils, uPoller, uPacket, uFlight, uGlobalDefs,
  uGT, uMessageHub, udbTree ,uMirrorDB,  uFidsTags, Classes, SysUtils,
  uFIDSXml, uCommon;

type
	CAbstractController = class
	private
    oFlight : cFlight;
    oFlightList : TStringList;

	// Define concrete methods - implemnted in this parent class
	procedure Connect;

    // Define abstract methods - they must be implemented in a sub-class
    {function  GetSideCount : Byte;        virtual; abstract;}

	published
    // Properties used to access private data fields
    // They use private methods to do this
    {
    property count : Byte
     read GetSideCount
     write SetSideCount;
     }
    constructor Create();  virtual;

end;


implementation

 // Implement the TPolygon constructor
 constructor CAbstractController.Create();
 begin

 end;


procedure CAbstractController.Connect;
var
    sl : TStringList;
begin
{  if ( oDataTree = nil ) and ( oHub.Connected ) then  begin
    LogIn( FeedID, SysPW, FeedID );

    oDataTree := cMirrorDB.Create( oHub, FeedID, false, true, 13 );
    oDataTree.RegisterReader( NewData );   // I want to see incoming messages
    oFlight := cFlight.Create( oDataTree, FeedID );
    sl := TStringList.Create;              // tell mirror what I'm interested in
    sl.Add( 'SystemConfig' );
    sl.Add( 'SystemSettings' );
    sl.Add( 'Arrivals' );
    sl.Add( 'Departures' );
    sl.Add( 'Timetable' );
    sl.Add( 'CheckIns' );
    sl.Add( 'DisplayConfig' );   // for list of UDC IPs - see uKeyPadServer
    // don't need to load big IATA stuff ...
    oDataTree.InitMirror( sl );  // maintain a local copy of the data base
    oDataTree.RegisterReader( PopulateGrid );
    sl.Free;
  end;  }
end;




end.
