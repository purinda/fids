unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ASCII, uXmlParser, uUtils, uPoller, uPacket, uFlight,uGlobalDefs,
  uGT, uMessageHub, udbTree ,uMirrorDB,  uFidsTags, StdCtrls, VirtualTrees,
  uFIDSXml;

type
  TForm1 = class(TForm)
    table: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tableDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure tableSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
      mShutDown, mInit : boolean;
      oFlight : cFlight;
      oFlightList : TStringList;
      CRow, CCol : Integer;
      ETD, STD : TDate;
      ETT, STT : TTime;
      procedure   PopulateGrid( const xml : string );
      procedure   fOnIdle(Sender: TObject; var Done: Boolean);

      procedure   NewConnection;
      procedure   setDStatus(stat : string);
  public
    { Public declarations }
  end;


var
  Form1: TForm1;
  DepFields : array [ 0..12 ] of  aFlightField = ( ffFlight, ffPorts, ffSTime, ffSTdate, ffETime, ffETdate, ffDStatus, ffGates, ffBays, ffCheckIns, ffCarrier, ffTerminal, ffRego );

  FXml : TFIDSxml;


implementation

{$R *.dfm}


procedure   TForm1.NewConnection;
var
    sl : TStringList;
    i : integer;
begin

  if ( FXml.GetMessageHub().Connected ) then
  begin
    oFlight := cFlight.Create( FXml.oDataTree,  'Feed' );
    i := FXml.oDataTree.RegisterReader( PopulateGrid );
  	PopulateGrid('');
  end;

end;

procedure TForm1.PopulateGrid( const xml : string );
var
  p : apNode;
  //made the following public in order to use for update
  flights: cFlightList;
  ff : aFlightField;
  r, c, ffSTA, ffETA : int;
begin
  ShowMessage('xx');
  flights := cFlightList.Create(  FXml.oDataTree );
  // the last parameter in Build function works like a filter
  // flights.Build( afkDepartures, ffGates, '3' );
  flights.Build( afkDepartures, ffNone, '' );
  table.ColCount := High( DepFields ) + 1;
  table.RowCount := flights.Count + 1;
  c := 0;
  for ff in DepFields do begin
     table.Cells[ c, 0 ] := oFlight.Title[ ff ];

     Inc( c );
  end;

  r := 1;   oFlightList.Clear;
  for p in flights do begin
      oFlight.DbNode := p;
      c := 0;
       //DBPath is used for flight update function
      oFlightList.Add( oFlight.DbPath );

      for ff in DepFields do begin
         table.Cells[ c, r ] := oFlight.Presentation[ ff ];
         Inc( c );
      end;

      Inc( r );

      if (r = flights.Count-1) then
        table.FixedRows := 1;

    end;
    flights.Free;


end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  setDStatus('Boarding');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    r: string;
    imsg: integer;
begin
    if CRow > 0 then
      begin
        imsg:= MessageBox(Handle, 'Do you want to delete this flight?', 'Confirmation',MB_YESNO+MB_ICONQUESTION);
        if (imsg = 6) then
        begin
            //Delete Flight
            if (CRow > 0) AND (CRow <= oFlightList.Count) then begin
               oFlight.DbNode := FXml.GetDataTree().GetNode( oFlightList[ CRow - 1 ]);
               oFlight.Delete;
            end;
        end;
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  newVal : String;
begin
      ShowMessage('Test flight TS1234 will be added...');

      oFlight.DbNode := nil;
      oFlight.Kind := afkDepartures;
      //Update flight
      oFlight.Presentation[ffFlight] := 'TS1234';
      //Update PORTS
      oFlight.Presentation[ffPorts] := 'ADL, SYD';
      //Set Gates
      oFlight.Presentation[ffGates] := '5';
      //Set Bays
      oFlight.Presentation[ffBays] := '19';
      //Set Status
      oFlight.Presentation[ffDStatus] := 'Departed';
      //Set Carrier
      oFlight.Presentation[ffCarrier] := 'Domestic';
      //Set Terminal
      oFlight.Presentation[ffTerminal] := '1';
      //Set Checkins
      oFlight.Presentation[ffCheckIns] := '20';
      //Set Rego
      oFlight.Presentation[ffRego] := 'TEST1';
      //Create the flight
      oFlight.New;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin

	if (FXml.GetMessageHub().Connected) then
  	ShowMessage('connected');

  NewConnection;
end;

procedure   TForm1.fOnIdle(Sender: TObject; var Done: Boolean);
begin
    if mShutDown then  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := fOnIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FXml.FreeInstance;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  oFlightList := TStringList.Create;
  FXml := FXml.Instance;
end;

procedure TForm1.setDStatus(stat : string);
begin
    if (CRow > 0) AND (CRow <= oFlightList.Count) then begin
       oFlight.DbNode :=  FXml.oDataTree.GetNode( oFlightList[ CRow - 1 ]);
       //Update PORTS
       if (oFlight.DbNode <> nil) then begin
         oFlight.Presentation[ffDStatus] := stat;
       end;
    end;
end;

procedure TForm1.tableDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
	S: String;
	drawrect :trect;
begin
	S:= ' ' + (Sender As TStringgrid).Cells[ ACol, ARow ];
        if (odd(arow) = false) then
        begin
          table.Canvas.Brush.Color := RGB(210,210,235);
          table.Canvas.FillRect(Rect);
        end;


	If Length(S)>0 Then Begin
		drawrect := rect;
		DrawText((Sender As TStringgrid).canvas.handle, Pchar(S), Length(S), drawrect, dt_calcrect or dt_wordbreak or dt_left or dt_noprefix );
		If (drawrect.bottom - drawrect.top) > (Sender As TStringgrid).RowHeights[Arow] Then
			(Sender As TStringgrid).RowHeights[Arow] := 	(drawrect.bottom - drawrect.top)
		Else
		Begin
			drawrect.Right := rect.right;
			(Sender As TStringgrid).canvas.fillrect( drawrect );
			DrawText((Sender As TStringgrid).canvas.handle, Pchar(S), Length(S), drawrect, dt_wordbreak or dt_left or dt_noprefix);
		End;
	End;

end;

procedure TForm1.tableSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := ( ACol > 0 ) and ( ARow > 0 );
  if CanSelect then  begin
    CRow := ARow;
    cCol := ACol;
  end;

end;

end.
