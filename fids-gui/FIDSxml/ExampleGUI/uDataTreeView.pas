unit uDataTreeView;

//	simple generic local tree viewer using virtual tree view

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees,
  uGUIMain, uMirrorDB;

type
  TfDataTree = class(TForm)
    vstData: TVirtualStringTree;    // note vst doesn't hold any data - pulls data as required
    procedure FormShow(Sender: TObject);
    procedure vstDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDataInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstDataExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
  private
		mDataTree : cMirrorDB;   // my copy of the data tree DB
        procedure   NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

  public
    { Public declarations }
  end;

var
  fDataTree: TfDataTree;

implementation

uses
	Generics.Collections, uDbTree, uGT;

{$R *.dfm}


function	VstAddress( vst : TVirtualStringTree; node: PVirtualNode ) : TList<integer>;    // a multi dimensional tree index

	begin                                 // calculate vst node address as a list of indexes - in vst already ?
    result := TList<integer>.Create;
    while node <> vst.RootNode do  begin    // vst bug? - parent of root should be nil
        result.Insert( 0, node.Index );
        node := node.Parent;
    	end;
    end;


procedure   TfDataTree.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    vstData.Invalidate;  // tell vst to redraw itself
    end;


procedure TfDataTree.vstDataInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

  	var                                  // init node indicating '+' expandable or not
        pt : apNode;
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( vstData, Node );
	pt := mDataTree.GetNode( oAddr );     // find matching item from data tree
    FreeAndNil( oAddr );  // avoid leak
    if SubNodeCount( pt ) > 0 then  Include( InitialStates, ivsHasChildren );
    end;


procedure TfDataTree.vstDataExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);

  	var                                  // create any required child-nodes to match shape of data tree
        pt : apNode;
        oAddr : TList<integer>;
        x, n : card;
    begin
    oAddr := VstAddress( vstData, Node );
	pt := mDataTree.GetNode( oAddr );     // find matching item from data tree
    FreeAndNil( oAddr );  // avoid leak
    if pt <> nil then  begin
    	n := Cardinal( SubNodeCount( pt ) );
        if Node.ChildCount <> n then  begin
        	n := n - Node.ChildCount;
        	// Node.ChildCount := pt.SubNodes.Count;  // vst needs to know how many children - bug? crashes if directly assigned
            for x := 1 to n do  vstData.AddChild( Node );   // add any additional sub nodes required
            end;
    	end;
    end;


procedure TfDataTree.vstDataGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  	var
        pt : apNode;
        oAddr : TList<integer>;
    begin                                  // vst pulls all data here
    oAddr := VstAddress( vstData, Node );
	pt := mDataTree.GetNode( oAddr );     // find matching item from data tree
    FreeAndNil( oAddr );  // avoid leak
    if pt <> nil then  begin
        case Column of
        	0 : CellText := pt.NodeName;
            1 : CellText := pt.Content;
        	end;
    	end
    else  CellText := '';
    end;


procedure TfDataTree.FormShow(Sender: TObject);

    begin
	if mDataTree = nil then  begin    // delayed self initialize
    	mDataTree := fGUIMain.GetDB;
        if mDataTree <> nil  then  begin
            mDataTree.RegisterReader( NewData );
            vstData.RootNodeCount := mDataTree.GetRoot.SubNodes.Count;   // properties - you could do this stuff in the object inspector
            vstData.Header.Columns.Add;
            vstData.Header.Columns[ 0 ].Text := 'Tag';
            vstData.Header.Columns[ 0 ].Width := 150;
            vstData.Header.Columns.Add;
            vstData.Header.Columns[ 1 ].Text := 'Content';
            vstData.Header.Columns[ 1 ].Width := 500;
            vstData.Header.Options := vstData.Header.Options + [ hoVisible ];  // display header
            //vstData.OnGetText := TfDataTree.vstDataGetText  etc for above event handlers
            // NewData( '' );
        	end;
        end;
    end;

end.
