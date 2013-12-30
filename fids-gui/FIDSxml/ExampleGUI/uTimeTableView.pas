unit uTimeTableView;

// vst => virtual string tree

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees,
  uGUIMain, uTTRules, uMirrorDB;


type
  TfTimeTable = class(TForm)
    vstTTV: TVirtualStringTree;  // just a grid
    procedure FormShow(Sender: TObject);
    procedure vstTTVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTTVEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
  private
		mDataTree : cMirrorDB;   // my copy of the data tree DB
        oTTRulesList : cTTRulesList;
        oRule : cTTRule;
        procedure   NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
  public
    { Public declarations }
  end;

var
  fTimeTable: TfTimeTable;

implementation

{$R *.dfm}
uses
	uFlight, ufRuleEdit, ufStringEntry, uFidsTags, uDbTree;


procedure   TfTimeTable.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    oTTRulesList.Build( tfNone, '' );
    // node list should always be safe as long as Build is not decoupled from NewData events
    vstTTV.RootNodeCount := oTTRulesList.Count;
    vstTTV.Invalidate;  // tell vst to redraw itself
    end;


procedure TfTimeTable.vstTTVEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

	begin     // some row selected for editing
    if Node.Index < Cardinal( oTTRulesList.Count ) then  begin
        oRule.DbNode := oTTRulesList[ Node.Index ];  // link oRule to vst row
        fRuleEdit.TTRule := oRule;
        fRuleEdit.ShowModal;
	    end;
	end;


procedure TfTimeTable.vstTTVGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

    begin     // data pull from vst
    if Node.Index < Cardinal( oTTRulesList.Count ) then  begin
    	oRule.DbNode := oTTRulesList[ Node.Index ];  // link oRule to vst row
        case Column of // flight, kind, time, ports, days
            0 : CellText := oRule.Presentation[ tfRuleName ];
            1 : CellText := oRule.Presentation[ tfPath ];
            2 : CellText := oRule.Presentation[ tfTime ];
            3 : CellText := oRule.oTemplate.Presentation[ ffPorts ];
            4 : CellText := oRule.Presentation[ tfDays ];
        	end;
    	end;
    end;


procedure TfTimeTable.FormShow(Sender: TObject);

    begin
	if mDataTree = nil then  begin    // delayed self initialize
    	mDataTree := fGUIMain.GetDB;
        if mDataTree <> nil  then  begin
            mDataTree.RegisterReader( NewData );
        	oTTRulesList := cTTRulesList.Create( mDataTree );
            oRule := cTTRule.Create( mDataTree, fGUIMain.GetID() );   // need a logged in id ('EgGUI') to update DB;
            NewData( '' );
        	end;
        end;
    end;

end.
