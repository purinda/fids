unit uTreeBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees,
  uGT, uDataDictionary, uDbTree, uMirrorDB;

type
	cTreeBox = class
    	constructor	Create( form : TForm; VST : TVirtualStringTree );
        destructor	Destroy;  override;
        private
            miLink		: ifDB;  // DD links
            mOptions	: asListOption;
            mForm		: TForm;
			mUpdate		: Boolean;
            mVST		: TVirtualStringTree;
            mOnDblClick : aHandler;
            mOnClick	: aHandler;
			mBase		: apNode;
            mNodeTitle: string;
            mContentTitle: string;

            procedure	SetOptions( opt : asListOption );
            procedure	NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
            function	GetTop : int;

            procedure VSTInitChildren(Sender: TBaseVirtualTree;
              Node: PVirtualNode; var ChildCount: Cardinal);
            procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
              Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
            procedure VSTEditing(Sender: TBaseVirtualTree;
              Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
			procedure VSTDblClick(Sender: TObject);
            procedure VSTGetText(Sender: TBaseVirtualTree;
              Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
			  var CellText: string);
            procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
              Column: TColumnIndex; NewText: string);

		public
            BaseNode	: asDbPath;
            property	Options : asListOption  read mOptions  write SetOptions;
            property	OnClick : aHandler read mOnClick  write mOnClick;
            property	OnDblClick : aHandler read mOnDblClick  write mOnDblClick;
    		procedure	InitFrame( intf : ifDB; const nodeTitle : string = ''; const contTitle : string = '' );
            procedure	FrameReady;
            function	ErrorMesg( er : aDDErrorCode ) : boolean;
            property	Top	: int  read GetTop;
		end;

implementation   // _____________________________________________________________________



uses
	StdCtrls, uUtils, Generics.Collections;


function	VstAddress( vst : TVirtualStringTree; node: PVirtualNode ) : TList<integer>;    // a multi dimensional tree index

	begin                                 // calculate vst node address as a list of indexes - in vst already ?
    result := TList<integer>.Create;
    while node <> vst.RootNode do  begin
        result.Insert( 0, node.Index );
        node := node.Parent;
    	end;
    end;


constructor	cTreeBox.	Create( form : TForm; VST : TVirtualStringTree );

	begin
    mForm := form;
    mVST := VST;
    end;


destructor	cTreeBox.	Destroy;  //override;

	begin
    miLink.DB.DeRegisterReader( NewData );
    end;


procedure	cTreeBox.	InitFrame( intf : ifDB; const nodeTitle : string = ''; const contTitle : string = '' );

	begin
    miLink.DB := intf.DB;
    miLink.ID := intf.ID;
    miLink.DB.RegisterReader( NewData );
    mNodeTitle := nodeTitle;
    if mNodeTitle = '' then  mNodeTitle := 'Field';
    mContentTitle := contTitle;
    if mContentTitle = '' then  mContentTitle := 'Value';
    end;


procedure	cTreeBox.FrameReady;

	var
    	em : int;
	begin
    em := mForm.Canvas.TextWidth( 'M' );  // width of an em assuming vst is using default form font
    mUpdate := true;
    mVST.BeginUpdate;
    mVST.Clear;
    mBase := miLink.DB.GetNode( BaseNode );
    mVST.RootNodeCount := SubNodeCount( mBase );    //  SubNodeCount( oFields.BaseNode );
    mVST.OnEditing := VSTEditing;
    mVST.OnDblClick := VSTDblClick;
    mVST.OnGetText := VSTGetText;  //  link in various event handlers
    mVST.OnNewText := VSTNewText;
    mVST.OnInitChildren := VSTInitChildren;
    mVST.OnInitNode := VSTInitNode;
    mVST.OnDblClick := VSTDblClick;

    mVST.Header.Columns.Add;
    mVST.Header.Columns[ 0 ].Text := mNodeTitle;
    mVST.Header.Columns[ 0 ].Width := 20 * em;
    mVST.Header.Columns.Add;
    mVST.Header.Columns[ 1 ].Text := mContentTitle;
    mVST.Header.Columns[ 1 ].Width := 70 * em;
    mVST.Header.Options := mVST.Header.Options + [ hoVisible ];  // display header

    mVST.Header.Options := mVST.Header.Options + [ hoVisible ];  // display header
    mVST.TreeOptions.SelectionOptions := mVST.TreeOptions.SelectionOptions + [ toExtendedFocus ];
    mVST.TreeOptions.MiscOptions := mVST.TreeOptions.MiscOptions + [ toEditable , toGridExtensions  ];
    mVST.TreeOptions.PaintOptions := mVST.TreeOptions.PaintOptions + [ toShowHorzGridLines, toShowVertGridLines ];
    mVST.EndUpdate;
    mUpdate := false;
    end;




procedure cTreeBox.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

  	var                                  // init node indicating '+' expandable or not
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( mVST, Node );
	if SubNodeCount( GetNodeNumerically( oAddr, mBase ) ) > 0 then  Include( InitialStates, ivsHasChildren );    // find matching item from data tree
    FreeAndNil( oAddr );  // avoid leak
    end;



procedure cTreeBox.VSTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);

  	var
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( mVST, Node );
	ChildCount := SubNodeCount( GetNodeNumerically( oAddr, mBase ) );
    FreeAndNil( oAddr );  // avoid leak
    end;


procedure cTreeBox.VSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);

  	var
        oAddr : TList<integer>;
        pt : apNode;
    begin                                  // vst pulls all data here
    oAddr := VstAddress( mVST, Node );
    pt := GetNodeNumerically( oAddr, mBase );
    if pt <> nil then  begin
    	miLink.DB.GlobalEdit( Back( pt, 1 ), pt.NodeName, NewText, miLink.ID );
    	end;
    FreeAndNil( oAddr );  // avoid leak
    end;


procedure cTreeBox.VSTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

    begin                                  // vst pulls all data here
	Allowed := false;
    if Column = 1 then  begin
        Allowed := mBase <> miLink.DB.GetRoot;
    	end;
    end;


procedure cTreeBox.VSTDblClick(Sender: TObject);

  	var
        pt : apNode;
        oAddr : TList<integer>;
    begin
    if mVST.FocusedNode <> nil then  begin
        oAddr := VstAddress( mVST, mVST.FocusedNode );
    	pt := GetNodeNumerically( oAddr, mBase );
        FreeAndNil( oAddr );  // avoid leak
        if ( pt <> nil ) and Assigned( OnDblClick ) then  begin
            OnDblClick( pt );
            end;
        end;
    end;


//procedure cTreeBox.VSTBeforeItemErase(Sender: TBaseVirtualTree;
//  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
//  var ItemColor: TColor; var EraseAction: TItemEraseAction);
//
//    begin
//    if loAltColour in mOptions then  begin
//        if Odd( oFields.oListIndexNodes[ Node.Index ].row ) then  begin
//            ItemColor := $FFFFE8;// bbggrr pale cyan
//            EraseAction := eaColor;
//            end;
//        end;
//    end;


procedure cTreeBox.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  	var
        pt : apNode;
        oAddr : TList<integer>;
    begin                                  // vst pulls all data here
    oAddr := VstAddress( mVST, Node );
    pt := GetNodeNumerically( oAddr, mBase );
    FreeAndNil( oAddr );  // avoid leak

    if Column = 0 then  CellText := NodeName( pt )
    else if Column = 1 then  CellText := NodeContent( pt )
    else  CellText := '';
    end;


function	cTreeBox.	GetTop : int;

	begin
    result := 0; // mContainer.Top;
    end;


procedure	cTreeBox.SetOptions( opt : asListOption );

	begin
    mOptions := opt;
    end;


function	cTreeBox.ErrorMesg( er : aDDErrorCode ) : boolean;

	begin
    result := er <> ecNone;
    if result then  begin
        ShowMessage( 'ERROR ' + EnumToStr( Ord( er ), TypeInfo( aDDErrorCode ) ) );
    	end;
    end;


procedure	cTreeBox.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    mVST.ReinitNode( mVST.RootNode, true );
    mVST.RootNodeCount := SubNodeCount( mBase );    //  SubNodeCount( oFields.BaseNode );
    mVST.Invalidate;
    end;

end.
