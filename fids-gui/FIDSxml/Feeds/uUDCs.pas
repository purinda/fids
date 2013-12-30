unit uUDCs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  uStringGrid, ExtCtrls;

type
  TfUDCs = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
  public
    oGrid : cStringGrid;
    OnRequest : TNotifyEvent;
  end;

var
  fUDCs: TfUDCs;

implementation

{$R *.dfm}

procedure TfUDCs.FormClick(Sender: TObject);

    begin
    if Assigned( OnRequest ) then  OnRequest( self );
    end;

procedure TfUDCs.FormCreate(Sender: TObject);

    begin
    oGrid := cStringGrid.CreateSG( Panel1 );
    end;


procedure TfUDCs.FormDestroy(Sender: TObject);

    begin
    FreeAndNil( oGrid );
    end;


procedure TfUDCs.Panel1Click(Sender: TObject);

    begin
    FormClick( Sender );  // why isn't this default behaviour
    end;

end.
