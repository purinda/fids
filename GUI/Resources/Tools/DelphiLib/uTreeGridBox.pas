unit uTreeGridBox;
{
first cols are tree - then grid form
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees, Generics.Collections,
  uGT, uDataDictionary, uDbTree, uMirrorDB;

type
    aInitFieldHandler = procedure ( flds : cFieldList; base : apNode ) of object;

	cTreeGridBox = class
    	constructor	Create( form : TForm; VST : TVirtualStringTree );
        destructor	Destroy;  override;
        private
            miLink		: ifDB;  // DD links
            miEdits		: iEdits;    // call back
            mOptions	: asListOption;
            mForm		: TForm;
			mUpdate		: Boolean;
            mVST		: TVirtualStringTree;
            mOnDblClick : aHandler;
            mInitFieldHandler : aInitFieldHandler;
            oFieldsLists		: TList<cFieldList>;
            mBase		: apNode;

			function	GetFormValue( f : cField; out valid : boolean ) : string;

			procedure	FormPaint(Sender: TObject);
            procedure	NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
            function	GetLeft : int;
            function	GetTop : int;
			function	GetFields( addr : TList<integer> ) : cFieldList;

            procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
              Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
            procedure VSTInitChildren(Sender: TBaseVirtualTree;
              Node: PVirtualNode; var ChildCount: Cardinal);
            procedure VSTEditing(Sender: TBaseVirtualTree;
              Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
			procedure VSTDblClick(Sender: TObject);
            procedure VSTBeforeItemErase(Sender: TBaseVirtualTree;
              TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
              var ItemColor: TColor; var EraseAction: TItemEraseAction);
            procedure VSTGetText(Sender: TBaseVirtualTree;
              Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
			  var CellText: string);
            procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
              Column: TColumnIndex; NewText: string);

		public
			oFields		: cFieldList;
            TreeColumns	: int;
            property	Options : asListOption  read mOptions  write mOptions;
            property	OnDblClick : aHandler read mOnDblClick  write mOnDblClick;

			procedure	Clear;
    		procedure	InitFrame( intf : ifDB; ifEdit : iEdits );
            procedure	PreInitFields( base : asDbPath; proc : aInitFieldHandler );
            procedure	FrameReady;
            function	ErrorMesg( er : aDDErrorCode ) : boolean;
            property	Left : int  read GetLeft;
            property	Top	: int  read GetTop;
		end;

implementation   // _____________________________________________________________________


uses
	StdCtrls, uUtils;

var
  BPCount: Integer;


function	VstAddress( vst : TVirtualStringTree; node: PVirtualNode ) : TList<integer>;    // a multi dimensional tree index

	begin                                 // calculate vst node address as a list of indexes - in vst already ?
    result := TList<integer>.Create;
    while node <> vst.RootNode do  begin
        result.Insert( 0, node.Index );
        node := node.Parent;
    	end;
    end;


procedure  Breakpoint;

	begin
    Inc( BPCount );
    end;


constructor	cTreeGridBox.	Create( form : TForm; VST : TVirtualStringTree );

	begin
    mForm := form;
    mForm.OnPaint := FormPaint;
    mVST := VST;
    oFieldsLists := TList<cFieldList>.Create;
    end;


destructor	cTreeGridBox.Destroy;

	var
    	fl : cFieldList;
	begin
    FreeAndNil( oFields );
    for fl in oFieldsLists do  fl.Free;
    FreeAndNil( oFieldsLists );
    end;


procedure	cTreeGridBox.PreInitFields( base : asDbPath; proc : aInitFieldHandler );

	begin          // save ready to make lists on demand
    mInitFieldHandler := proc;
    mBase := miLink.DB.GetNode( base );
    end;


procedure	cTreeGridBox.	Clear;

	begin
    oFields.Clear;
    oFieldsLists.Clear;
    end;


procedure	cTreeGridBox.	InitFrame( intf : ifDB; ifEdit : iEdits );

	begin
    miLink.DB := intf.DB;
    miLink.ID := intf.ID;
    miEdits := ifEdit;  // back links from DD
    miEdits.Form := mForm;
    miEdits.NewData := NewData;
    miEdits.FormValue := GetFormValue;
    if oFields = nil then  oFields := cFieldList.Create( miLink, miEdits );
    //Clear;
    end;


procedure	cTreeGridBox.FrameReady;

	var
    	em : int;
        f : cField;
        col : TVirtualTreeColumn;
	begin
    em := mForm.Canvas.TextWidth( 'M' );  // width of an em assuming vst is using default form font
    mUpdate := true;
    mInitFieldHandler( oFields, nil );
    mVST.BeginUpdate;
    mVST.Clear;
    if TreeColumns = 0 then  begin
    	mVST.RootNodeCount := oFields.oListIndexNodes.Count;
    	end
    else  mVST.RootNodeCount := SubNodeCount( mBase );
    mVST.EditDelay := 500;  // 1/2 second instead of the default 1 second
    mVST.OnEditing := VSTEditing;
    mVST.OnDblClick := VSTDblClick;
    mVST.OnBeforeItemErase := VSTBeforeItemErase;
    mVST.OnGetText := VSTGetText;  //  link in various event handlers
    mVST.OnNewText := VSTNewText;
    mVST.OnInitChildren := VSTInitChildren;
    mVST.OnInitNode := VSTInitNode;
    for f in oFields do  begin     // create columns with headings
        col := mVST.Header.Columns.Add;
        col.Text := f.Title;
        col.Width := f.Width * em;
        end;
    mVST.Header.Options := mVST.Header.Options + [ hoVisible ];  // display header
    mVST.TreeOptions.SelectionOptions := mVST.TreeOptions.SelectionOptions + [ toExtendedFocus ];
    mVST.TreeOptions.MiscOptions := mVST.TreeOptions.MiscOptions + [ toEditable, toGridExtensions ];

    mVST.TreeOptions.PaintOptions := mVST.TreeOptions.PaintOptions + [ toShowHorzGridLines, toShowVertGridLines ];
    if TreeColumns = 0 then  begin
        mVST.TreeOptions.PaintOptions := mVST.TreeOptions.PaintOptions - [ toShowTreeLines, toShowRoot ];
    	end;
    mVST.EndUpdate;
    mUpdate := false;
    end;


procedure cTreeGridBox.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

  	var                                  // init node indicating '+' expandable or not
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( mVST, Node );
    if oAddr.Count <= TreeColumns then  begin
        if SubNodeCount( GetNodeNumerically( oAddr, mBase ) ) > 0 then  begin
            Include( InitialStates, ivsHasChildren );
            end
         else 	if oFields.SubNodeCount( oAddr ) > 0 then  begin
    		Include( InitialStates, ivsHasChildren );    // find matching item from data tree
            end;
        end;
    FreeAndNil( oAddr );  // avoid leak
    end;


procedure cTreeGridBox.VSTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);

  	var
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( mVST, Node );
    if oAddr.Count <= TreeColumns then  begin
        ChildCount :=  SubNodeCount( GetNodeNumerically( oAddr, mBase ) );
        end
    else 	ChildCount := oFields.SubNodeCount( oAddr );
    FreeAndNil( oAddr );  // avoid leak
    end;


function cTreeGridBox.GetFields( addr : TList<integer> ) : cFieldList;

  	var
        x : int;
        pt, bk, base : apNode;
        fl : cFieldList;
	begin
    result := nil;
    if addr.Count <= TreeColumns then  result := oFields  // root
    else  begin
        pt := oFields.GetNodeNumerically( addr );
        //pt := Back( pt, 1 );
        if pt <> nil then  begin
     		for x := 0 to oFieldsLists.Count - 1 do   begin
            	bk := pt;  base := oFieldsLists[ x ].BaseNode;
                if ( base = nil ) or ( base = pt ) then  break;
                while bk <> nil do  begin  // trace back down the tree for a matching base grid list
                    bk := Back( bk, 1 );
                    if ( base = bk ) then  begin
                        result := oFieldsLists[ x ];
                        break;
                        end;
                	end;
            	end;
            if result = nil then  begin
                fl := cFieldList.Create( miLink, miEdits );
                oFieldsLists.Add( fl );
                mInitFieldHandler( fl, pt );
                result := fl;
            	end;
        	end;
    	end;
    end;


procedure cTreeGridBox.VSTDblClick(Sender: TObject);

  	var
        pt		: apNode;
        row		: int;
        flds	: cFieldList;
        oAddr	: TList<integer>;
        Node	: PVirtualNode;
    begin
    if mVST.FocusedNode <> nil then  begin
        Node := mVST.FocusedNode;
        pt := nil;
        oAddr := VstAddress( mVST, Node );
        flds := GetFields( oAddr );
        if flds <> nil then  begin
            if oAddr.Count <= TreeColumns then  begin   // tree
                pt := flds.GetNodeNumerically( oAddr );
                end
            else   if TreeColumns > 0 then  begin      // grid
                row := Node.Index;
                if ( flds.oListIndexNodes <> nil ) and ( row < flds.oListIndexNodes.Count ) then  begin
                    pt := flds.oListIndexNodes[ row ].np;
                    end;
                end;
    		if Assigned( mOnDblClick ) and ( pt <> nil ) then  mOnDblClick( pt );
            end;
    	FreeAndNil( oAddr );  // avoid leak
    	end;
    end;


procedure cTreeGridBox.VSTBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);

	var
        oAddr : TList<integer>;
        colourIt : boolean;
    begin
    colourIt := false;
    oAddr := VstAddress( mVST, Node );
    if ( TreeColumns > 0 ) and ( oAddr.Count > TreeColumns ) then  begin  // a sub grid row so match parent colour
        if Odd( Node.Parent.Index ) then  colourIt := true;
        end
    else if Odd( Node.Index ) then  colourIt := true;

    if colourIt and ( loAltColour in mOptions ) then  begin
        ItemColor := $FFFFE8;// bbggrr pale cyan
        EraseAction := eaColor;
        end;
    end;


procedure cTreeGridBox.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  	var
        pt : apNode;
        row : int;
        f : cField;
        flds : cFieldList;
        oAddr : TList<integer>;
    begin
    CellText := '';
    oAddr := VstAddress( mVST, Node );
    if ( oAddr.Count >= 2 ) and ( oAddr[ 0 ] = 8 ) and ( oAddr[ 1 ] = 1 ) then  begin
        Breakpoint;
        end;
    flds := GetFields( oAddr );
    if flds <> nil then  begin
        if oAddr.Count <= TreeColumns then  begin   // tree
            pt := flds.GetNodeNumerically( oAddr );
            if Column = 0 then  CellText := NodeName( pt ) else  CellText := '';
            end
        else   if TreeColumns > 0 then  begin      // grid
            row := Node.Index;
            if ( flds.oListIndexNodes <> nil ) and ( row < flds.oListIndexNodes.Count )
                and ( Column >= 0 ) and ( Column < flds.Count ) then  begin
                pt := flds.oListIndexNodes[ row ].np;
                f := flds[ Column ];
                if not ( loSupressSubText in mOptions )  // simple code share field suppession - assumes BaseNode is key
                    or ( NodeIndex( pt ) = 0 )          // primary
                    or ( f.IndexStepBack = 0 ) then  begin  // not a base field
                    CellText := f.GetPresentation( pt );
                    end
                end;
            end
	    end;
    FreeAndNil( oAddr );  // avoid leak
    end;


procedure cTreeGridBox.VSTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

  	var
        pt : apNode;
        row : int;
        f : cField;
        flds : cFieldList;
        oAddr : TList<integer>;
    begin
	Allowed := false;
    oAddr := VstAddress( mVST, Node );
    if ( oAddr.Count >= 2 ) and ( oAddr[ 0 ] = 8 ) and ( oAddr[ 1 ] = 1 ) then  begin
        Breakpoint;
        end;
    if TreeColumns = 0 then  begin   // plain grid
        if Column < oFields.Count then  begin
            f := oFields[ Column ];
            Allowed := foEditable in f.Options;
            end;
    	end

    else if oAddr.Count <= TreeColumns then  begin 	// tree part
        if Column > 0 then  begin               	// don't allow edit of node name
            pt := oFields.GetNodeNumerically( oAddr );
            f := oFields.FindField( NodeName( pt ) );
            if f <> nil then  Allowed := foEditable in f.Options;
            end;
    	end
    else begin  //     grid part
    	flds := GetFields( oAddr );
    	row := Node.Index;
        if ( flds <> nil ) and ( flds.oListIndexNodes <> nil ) and ( row < flds.oListIndexNodes.Count )
            and ( Column >= 0 ) and ( Column < flds.Count ) then  begin
            f := flds[ Column ];
            if foEditable in f.Options then  Allowed := true;
            end;
        end;
    FreeAndNil( oAddr );  // avoid leak
    end;


procedure cTreeGridBox.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);

  	var
        pt : apNode;
        row : int;
        f : cField;
        flds : cFieldList;
        oAddr : TList<integer>;
    begin
    oAddr := VstAddress( mVST, Node );
    if ( oAddr.Count >= 2 ) and ( oAddr[ 0 ] = 8 ) and ( oAddr[ 1 ] = 1 ) then  begin
        Breakpoint;
        end;
    if TreeColumns = 0 then  begin   // plain grid
        if ( oFields.oListIndexNodes <> nil ) and ( Node.Index < Cardinal( oFields.oListIndexNodes.Count ) )
            and ( Column < oFields.Count ) then  begin
            pt := oFields.oListIndexNodes[ Node.Index ].np;
            f := oFields[ Column ];
            if pt <> nil then  begin
                ErrorMesg( f.SetPresentation( pt, NewText ) );
                end;
            end;
    	end

    else if oAddr.Count <= TreeColumns then  begin 	// tree part
        if Column > 0 then  begin               	// don't allow edit of node name
            pt := oFields.GetNodeNumerically( oAddr );
            f := oFields.FindField( NodeName( pt ) );
            if ( f <> nil ) and ( pt <> nil ) then  begin
                ErrorMesg( f.SetPresentation( pt, NewText ) );
                end;
            end;
    	end
    else begin  //     grid part
    	flds := GetFields( oAddr );
    	row := Node.Index;
        if ( flds <> nil ) and ( flds.oListIndexNodes <> nil ) and ( row < flds.oListIndexNodes.Count )
            and ( Column >= 0 ) and ( Column < flds.Count ) then  begin
	        pt := flds.oListIndexNodes[ Node.Index ].np;
            f := flds[ Column ];
            if ( pt <> nil ) and ( f <> nil ) and ( foEditable in f.Options ) then  begin
        		ErrorMesg( f.SetPresentation( pt, NewText ) );
            	end;
            end;
        end;
    FreeAndNil( oAddr );  // avoid leak
    end;


function	cTreeGridBox.	GetLeft : int;   // used internally for coordinate adjustment

	begin
    result := 0; // mContainer.Left;
    end;


function	cTreeGridBox.	GetTop : int;

	begin
    result := 0; // mContainer.Top;
    end;


function	cTreeGridBox.GetFormValue( f : cField; out valid : boolean ) : string;

	begin       // doesn't support new ?
    result := '';     valid := false;
    end;


function	cTreeGridBox.ErrorMesg( er : aDDErrorCode ) : boolean;

	begin
    result := er <> ecNone;
    if result then  begin
        ShowMessage( 'ERROR ' + EnumToStr( Ord( er ), TypeInfo( aDDErrorCode ) ) );
    	end;
    end;


procedure	cTreeGridBox.FormPaint(Sender: TObject);

	begin
    // oVST does its own thing
    end;


procedure	cTreeGridBox.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    //Clear;
    mVST.ReinitNode( mVST.RootNode, true );
    if TreeColumns = 0 then  begin
    	mVST.RootNodeCount := oFields.oListIndexNodes.Count;
    	end
    else  mVST.RootNodeCount := SubNodeCount( mBase );
    mVST.Invalidate;
    end;

end.
