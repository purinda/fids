unit uGridBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees,
  uGT, uDataDictionary, uDbTree, uMirrorDB;

type
	cGridBox = class
    	constructor	Create( form : TForm; VST : TVirtualStringTree );
        private
            miLink		: ifDB;  // DD links
            miEdits		: iEdits;    // call back
            mOptions	: asListOption;
            mForm		: TForm;
			mUpdate		: Boolean;
            mVST		: TVirtualStringTree;
            mOnDblClick : aHandler;

			function	GetFormValue( f : cField; out valid : boolean ) : string;

			procedure	FormPaint(Sender: TObject);
            procedure	NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
            function	GetLeft : int;
            function	GetTop : int;

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
            property	Options : asListOption  read mOptions  write mOptions;
            property	OnDblClick : aHandler read mOnDblClick  write mOnDblClick;
    		procedure	InitFrame( intf : ifDB );
            procedure	FrameReady;
            function	ErrorMesg( er : aDDErrorCode ) : boolean;
            property	Left : int  read GetLeft;
            property	Top	: int  read GetTop;
		end;

implementation   // _____________________________________________________________________


uses
	Generics.Collections, uUtils;

const
	xGap = 20;   // form element spacing
    yGap = 6;


constructor	cGridBox.	Create( form : TForm; VST : TVirtualStringTree );

	begin
    mForm := form;
    mForm.OnPaint := FormPaint;
    mVST := VST;
    end;


procedure	cGridBox.	InitFrame( intf : ifDB );

	begin
    miLink.DB := intf.DB;
    miLink.ID := intf.ID;
    miEdits.FormValue := GetFormValue;  // back links from DD
    miEdits.Form := mForm;
    miEdits.NewData := NewData;
    if oFields = nil then  oFields := cFieldList.Create( miLink, miEdits )
    else  oFields.Clear;
    end;


function	VstAddress( vst : TVirtualStringTree; node: PVirtualNode ) : TList<integer>;    // a multi dimensional tree index

	begin                                 // calculate vst node address as a list of indexes - in vst already ?
    result := TList<integer>.Create;
    while node <> vst.RootNode do  begin
        result.Insert( 0, node.Index );
        node := node.Parent;
    	end;
    end;


procedure	cGridBox.FrameReady;

	var
    	em : int;
        f : cField;
        col : TVirtualTreeColumn;
	begin
    em := mForm.Canvas.TextWidth( 'M' );  // width of an em assuming vst is using default form font
    mUpdate := true;
    oFields.BuildIndexList;
    mVST.BeginUpdate;
    mVST.Clear;
    //mVST.Align := alClient; // alBottom; // mContainer.Align;  //alClient;   only client works
    //mVST.Parent := mForm;    // set virtual string tree to look like a grid
    mVST.RootNodeCount := oFields.oListIndexNodes.Count;
    mVST.EditDelay := 500;  // 1/2 second instead of the default 1 second
    mVST.OnEditing := VSTEditing;
    mVST.OnDblClick := VSTDblClick;
    mVST.OnBeforeItemErase := VSTBeforeItemErase;
    mVST.OnGetText := VSTGetText;  //  link in various event handlers
    mVST.OnNewText := VSTNewText;
    for f in oFields do  begin     // create columns with headings
        col := mVST.Header.Columns.Add;
        col.Text := f.Title;
        col.Width := f.Width * em;
        end;
    mVST.Header.Options := mVST.Header.Options + [ hoVisible ];  // display header
    mVST.TreeOptions.SelectionOptions := mVST.TreeOptions.SelectionOptions + [ toExtendedFocus ];
    mVST.TreeOptions.MiscOptions := mVST.TreeOptions.MiscOptions + [ toEditable, toGridExtensions ];
    mVST.TreeOptions.PaintOptions := mVST.TreeOptions.PaintOptions + [ toShowHorzGridLines, toShowVertGridLines ]
        - [ toShowTreeLines, toShowRoot ];
    mVST.EndUpdate;
    mUpdate := false;
    end;



procedure cGridBox.VSTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

  	var
        f : cField;
    begin
    if Column < oFields.Count then  begin
        f := oFields[ Column ];
    	Allowed := foEditable in f.Options;
    	end;
    end;


procedure cGridBox.VSTDblClick(Sender: TObject);

  	var
        pt		: apNode;
        row		: int;
        Node	: PVirtualNode;
	begin
    if mVST.FocusedNode <> nil then  begin
        Node := mVST.FocusedNode;
        pt := nil;

        row := Node.Index;
        if ( oFields.oListIndexNodes <> nil ) and ( row < oFields.oListIndexNodes.Count ) then  begin
            pt := oFields.oListIndexNodes[ row ].np;
            end;
    	if Assigned( mOnDblClick ) and ( pt <> nil ) then  mOnDblClick( pt );
        end;
    end;


procedure cGridBox.VSTBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);

    begin
    if Cardinal( oFields.oListIndexNodes.Count ) > Node.Index then  begin
        if loAltColour in mOptions then  begin
            if Odd( oFields.oListIndexNodes[ Node.Index ].row ) then  begin
                ItemColor := $FFFFE8;// bbggrr pale cyan
                EraseAction := eaColor;
                end;
            end;
    	end;
    end;


procedure cGridBox.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  	var
        pt : apNode;
        row : int;
        f : cField;
    begin                                  // vst pulls all data here
    row := Node.Index;
    if ( oFields.oListIndexNodes <> nil ) and ( row < oFields.oListIndexNodes.Count ) and ( Column < oFields.Count ) then  begin
        pt := oFields.oListIndexNodes[ row ].np;
        f := oFields[ Column ];
        if not ( loSupressSubText in mOptions )  // simple code share field suppession - assumes BaseNode is key
            or ( NodeIndex( pt ) = 0 )          // primary
            or ( f.IndexStepBack = 0 ) then  begin  // not a base field
            CellText := f.GetPresentation( pt );
            end
        else  CellText := '';
        end;
    end;


procedure cGridBox.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);

  	var
        pt : apNode;
        f : cField;
	begin
    if ( oFields.oListIndexNodes <> nil ) and ( Node.Index < Cardinal( oFields.oListIndexNodes.Count ) )
    	and ( Column < oFields.Count ) then  begin
        pt := oFields.oListIndexNodes[ Node.Index ].np;
        f := oFields[ Column ];
        if pt <> nil then  begin
        	ErrorMesg( f.SetPresentation( pt, NewText ) );
        	end;
    	end;
    end;


function	cGridBox.	GetLeft : int;   // used internally for coordinate adjustment

	begin
    result := 0; // mContainer.Left;
    end;


function	cGridBox.	GetTop : int;

	begin
    result := 0; // mContainer.Top;
    end;


function	cGridBox.GetFormValue( f : cField; out valid : boolean ) : string;

	begin       // doesn't support new ?
    result := '';     valid := false;
    end;


function	cGridBox.ErrorMesg( er : aDDErrorCode ) : boolean;

	begin
    result := er <> ecNone;
    if result then  begin
        ShowMessage( 'ERROR ' + EnumToStr( Ord( er ), TypeInfo( aDDErrorCode ) ) );
    	end;
    end;


procedure	cGridBox.FormPaint(Sender: TObject);

	begin
    // oVST does its own thing
    end;


procedure	cGridBox.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    mVST.ReinitNode( mVST.RootNode, true );   // rebuild vst
    mVST.RootNodeCount := oFields.oListIndexNodes.Count;
    mVST.Invalidate;
    end;

end.
