unit uDataDictionary;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,   // todo reduce
  Dialogs, Generics.Collections, StdCtrls,
  uGT, uMessageHub, uDbTree, uMirrorDB;    // in ../../Tools/DelphiLib/


const
    pathDataDictionary = '|SystemConfig|DataDictionary|';    // todo all pathXXX end in '|'
    pathDataShape	= '|SystemConfig|DataShape|';
    tagSystemConfig = 'SystemConfig';
    tagDataShape	= 'DataShape';
    contAbstract	= 'Abstract';
    contKeep		= 'Keep';
    contKey			= 'Key';


type
	//             common form stuff   // here for convenience
	aListOption = ( loTree, loSupressSubText, loGroupSubs, loAltColour );
    asListOption = set of aListOption;
//	aFormOption = ( foSupressSubText_, , foGroupSubs_ );
//    asFormOption = set of aFormOption;
	aHandler = procedure( node : apNode ) of object;

    //__________________________________________________________________________

	cFieldList = class;    // a bit convoluted
    cField = class;

    aGetSelectedFn = function : asDbPath  of object;
	aGetFormValueFn = function( f : cField; out valid : boolean ) : string  of object;

    iFieldDB = record           // interface connecting field to its tree DB/DD stuff
        DB			: cMirrorDB;
        ID			: string;
        IndexField	: asDbNode; // eg 'FlightNo'    used as basis for list build and 'record' identification
        IndexNode	: apNode;	// datashape entry
        ShapeBase	: apNode;	// base of datashape, eg ..|DataShape|Timetable
        DataDict	: apNode;	// base of DD, eg ..|DataDictionary
        BaseNode	: apNode;	// base of data records, eg |Timetable
        GetSelected	: aGetSelectedFn;
        DefaultValue	: aGetFormValueFn;
        //Selected	: asDbPath;   // eg '|Timetable|some rule|Template|Flights|some flight'   currnet record to edit etc
        FieldList	: cFieldList;  // self
    	end;

    aFieldOption = ( foPath, foKeep, foAbstract, foDisplayNodeName, foEditable, foRequired, foNumeric, foNumericSort, foEnum, foSelector,
    	foLink, foPathPicker, foMemo );    // , foUnique
    asFieldOption = set of aFieldOption;

	aDDErrorCode = ( ecNone, ecKeyNameIsNotUnique, ecRequiredFieldIsBlank, ecDDFailedToResolvePath, ecInvalidFieldName
    	, ecInvalidDataField, ecFieldIsNotEditable );

	cField = class				// field descriptors
    	constructor	Create( var iLink : iFieldDB; name : string; opts : asFieldOption = []; widthEm : int = 0 );  reintroduce;
        destructor	Destroy;	override;
		private
        	mName		: asDbNode;
            mTag		: int;
            oShapeBasePath	: TStringList;	// relative path from base field to me
            oIndexPath	: TStringList;	// relative path from index field to me - include mIndexStepBack
            mIndexStepBack	: int;
    		mShape		: apNode;		// where field sits in the local branch
            miLink		: ^ iFieldDB;
			mDigits		: Integer;  // connect field to th rest of the world
            mLinkPath	: asDbPath;
			function	SearchNode( base : apNode; name : asDbNode ) : apNode;
			function	FetchLogicalPath( selected : apNode ) : apNode;
            function	GetTitle : string;
            class var	NextTag : int;
        public
            Options		: asFieldOption;
            Control		: TControl;
            Button		: TButton;
			Width		: Integer;   // in em s
            IsTyping	: boolean;
            property	IndexPath	: TStringList read oIndexPath;	// relative path from index field to me - include mIndexStepBack
            property	IndexStepBack	: int  read mIndexStepBack;
            property	Title : string  read GetTitle;
            property	Tag : int  read mTag;
            property	Name : string  read mName;
			function	SelectedNode : apNode;
			function	Above( field : asDbNode ) : boolean;  // or equal
            function    GetPresentation( selected : apNode ) : string;    overload;
            function    GetPresentation : string;    overload;          // of selected branch
            function	SetPresentation( selected : apNode; var val : string ) : aDDErrorCode;   // db update
			function	Validate( var val : string; selected : apNode ) : aDDErrorCode;
            function    GetRaw( selected : apNode ) : string;                 // db value
			function    GetNode : apNode;
            function	Shape( field : string ) : apNode;      // assumes miLink
			function	ShapePath( field : string ) : TStringList;  // assumes miLink
    	end;


    iEdits = record
        FormValue		: aGetFormValueFn;	// returns edited string value
        NewData 		: aDeltaNotify;		// notify editors to change their displayed data
        Form			: TForm;			// never used ?
        DefaultValue	: aGetFormValueFn;  // in the absence of real data
        GetSelected		: aGetSelectedFn;   // get selected
        SetSelected		: procedure ( sel : apNode ) of object;
        PickerClick		: procedure ( field : cField ) of object;
        EnterPressed	: procedure ( field : cField ) of object;
    	end;


    aFieldList = TList< cField >;

    aNodeRec = record
    	np : apNode;
        row : int;     // keep track of logical row number for alt colouring
        end;
    aNodeList = TList<aNodeRec>;

	cFieldList = class( aFieldList )   // list of field descriptors supports generic user data display/edit boxes
    	constructor	Create( DBlink : ifDB; Edits : iEdits );  reintroduce;
        destructor	Destroy;  override;
        private
            mID			: string;
            miEdits		: iEdits;
            miFieldDB	: iFieldDB;
			mIndexListBuilt	: Boolean;   // BuildIndexList state
			function	ReturnBlank : asDbPath;     // place holders for unimplemented edit i/f hooks
			function	ReturnBlank2( f : cField; out valid : boolean ) : string;
			function	FollowLogicalPath( path : TStringList; base : apNode; backStep : int = 0 ) : apNode;
			function	GeneratePhysicalPath( fieldFrom, fieldTo : asDbNode; pre : boolean = false ) : TStringList;
			procedure	NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
			function	TagName( logicalName : asDbNode ) : asDbNode;
            function	ResolveAbstractPath( name : asDbNode ) : TStringList;
 			function	AlignPaths( var orign, dest : TStringList ) : boolean;
			procedure	SetRelativePaths;
			function	ShapeEntry( name : asDbNode ) : apNode;

        public
            oListIndexNodes	: aNodeList;	// used extensively by cGridBox
            ListOption : asListOption;
            ErrorField	: int;              // indicates field that caused error code response
            property    IndexField : asDbNode  read  miFieldDB.IndexField; //  write SetIndexField;  // index field of data shape
            property	BaseNode : apNode  read miFieldDB.BaseNode;                           // data area base, eg 'Arrivals'
            property	ID : string  write mID;  // debug
			function	SelectedNode : apNode;
            function	EditNode( addr : TList<integer>; NewText : string ) : aDDErrorCode;  // data update push from editor, eg inplace grid edit
			function	GetNodeNumerically( addr : TList<integer> ) : apNode;             // data access straight to data, eg cTreeBox
            function	SubNodeCount( addr : TList<integer> ) : int;

			function	FindField( name : asDbNode ) : cField;  overload;
			function	FindField( tag : int ) : cField;   overload;
            function	GoToIndex( pt : apNode ) : apNode;

            procedure	BuildIndexList;
			function	UpHillPath( fromField : cField; toFieldName : asDbNode ): TStringList;   // supports row selectors in editors
            procedure	ClearIsTyping;

			function	UpdateFields : aDDErrorCode;
			function    NewBranch : aDDErrorCode;
			function	NewSub( newField : asDbNode ) : aDDErrorCode;
		   	procedure   DeleteBranch( fieldName : asDbNode );

            procedure	InitFields( base : asDbPath; shape : asDbStub; index : asDbNode );
            function	AddField( name : asDbNode; options : asFieldOption = []; widthEm : int = 0 ) : int;
			procedure	AddFieldPath( name : asDbNode; absPath : asDbPath; options : asFieldOption = []; widthEm : int = 0 );
            procedure	Clear;
            procedure	Ready;

			end;



implementation


uses
	uUtils, uGlobalDefs, uXmlParser;

const
	xGap = 20;   // form element spacing
    yGap = 6;


function	IsAbstract( pt : apNode ) : boolean;

	begin
	result := Pos( contAbstract, NodeContent( pt ) ) > 0;
    end;


function	IsKey( pt : apNode ) : boolean;

	begin
	result := Pos( contKey, NodeContent( pt ) ) > 0;
    end;

// _____________________________________________________________________________


constructor	cField.Create( var iLink : iFieldDB; name : string; opts : asFieldOption = []; widthEm : int = 0 );

	var                            // initialize field attributes from data dictionary
    	pt : apNode;
	begin
    miLink := addr( iLink );
    mName := name;
    Options := opts;
    Width := widthEm;
    mShape := Shape( name );  // get path from DataShape
    if IsAbstract( mShape ) then  begin
    	Include( Options, foAbstract );
    	Include( Options, foDisplayNodeName );
    	end;
    if Pos( contKeep, NodeContent( mShape ) ) > 0 then  Include( Options, foKeep );
    oShapeBasePath := ShapePath( name );
    pt := FindName( miLink.DataDict, name );
    if pt <> nil then  begin    // read data dictionary values for field type
        if Width = 0 then  TryStrToInt( ReadContent( pt, 'Width' ), Width );
        if FindName( pt, contKeep ) <> nil then  Include( Options, foKeep );
        // if FindName( pt, tagAbstract ) <> nil then  Include( Options, foAbstract );
        if FindName( pt, 'DisplayNodeName' ) <> nil then  Include( Options, foDisplayNodeName );
        if FindName( pt, 'Numeric' ) <> nil then  begin
        	Include( Options, foNumeric );
            TryStrToInt( ReadContent( pt, 'Numeric' ), mDigits );
        	end;
        if FindName( pt, 'NumericSort' ) <> nil then  Include( Options, foNumericSort );
        //if FindName( pt, 'Unique' ) <> nil then  Include( Options, foUnique );
        if FindName( pt, 'Required' ) <> nil then  Include( Options, foRequired );
        if FindName( pt, 'Link' ) <> nil then  begin
        	Include( Options, foLink );
            mLinkPath := ReadContent( pt, 'Link' );
        	end;
    	end;
    if Width = 0 then  Width := 20;
    mTag := NextTag;  Inc( NextTag );
    end;


destructor	cField.Destroy;

	begin
    oShapeBasePath.Free;
    oIndexPath.Free;
    Control.Parent := nil;   // todo not needed
    FreeAndNil( Control );
    Button.Parent := nil;    // todo not needed
    FreeAndNil( Button );
    inherited;
    end;


function	cField.Shape( field : string ) : apNode;

	begin
    result := miLink.DB.FindNodeMulti( miLink.ShapeBase, field );  // get path from DataShape
    end;


function	cField.Above( field : asDbNode ) : boolean;  // or equal

	var
    	pt : apNode;
    begin
    pt := mShape;   result := false;
    if pt <> nil then  begin
        while pt <> miLink.ShapeBase do  begin    // work back through the shape path
            if pt.NodeName = field then  begin
                result := true;
                break;
                end;
            pt := Back( pt, 1 );
            if pt = nil then  break;
            end;
    	end;
    end;


function	cField.ShapePath( field : string ) : TStringList;

	var                                     // returns relative path to base node
    	pt : apNode;
	begin
    result := nil;
    pt := miLink.DB.FindNodeMulti( miLink.ShapeBase, field );  // get path from DataShape
    if ( pt <> nil ) and not ( foPath in Options ) then  begin
        result := ResolvePath( pt );  // get path from DataShape
        while ( result.Count > 1 ) do  begin
            result.Delete( 0 );      // delete extraneous leading path
            if result[ 0 ] = NodeName( miLink.ShapeBase ) then  begin
                result.Delete( 0 );  // don't want base name in relative path
                break;
                end;
            end;
    	end;
    end;


function	cField.SelectedNode : apNode;

	var
    	sel : string;
	begin
    if Assigned( miLink.GetSelected ) then  sel := miLink.GetSelected;
    result := nil;
    if sel <> '' then  result := FollowPath( sel, miLink.DB.GetRoot );
    end;


function	cField.SearchNode( base : apNode; name : asDbNode ) : apNode;

//	var
//    	pt : apNode;
	begin
    result := nil;   //pt := miLink.DB.GetNode( miLink.GetSelected );
    if ( name <> '' ) and ( base <> nil ) then  begin
        result := miLink.DB.FindNodeMulti( base, name );
    	end;
    end;


function	cField.FetchLogicalPath( selected : apNode ) : apNode;

	var
    	hop, name : string;
        pathl : TStringList;
        pt : apNode;
        step : int;
	begin
    pt := selected;
    if foPath in Options then  begin
    	pathl := StrToPath( mLinkPath );
        for hop in pathl do  begin
            if hop[ 1 ] = '*' then  begin
            	name := hop;
            	System.Delete( name, 1, 1 );
                pt := SearchNode( pt, name );
                pt := miLink.DB.GetNode( NodeContent( pt ) );
            	end
            else if hop[ 1 ] = '-' then  begin
            	name := hop;
            	System.Delete( name, 1, 1 );
                if TryStrToInt( name, step ) then  begin
                    pt := Back( pt, step );
                	end
                else pt := nil;
            	end
            else  pt := FindName( pt, hop );
            if pt = nil then  break;
        	end;
        FreeAndNil( pathl );
    	end
    else  pt := Traverse( selected, mIndexStepBack, oIndexPath );
    result := pt;
    end;


function    cField.GetPresentation( selected : apNode ) : string;

	var
    	pt : apNode;
        valid : boolean;
	begin
    result := '';
    if selected <> nil then  begin
        //pt := Traverse( selected, mIndexStepBack, oIndexPath );
        pt := FetchLogicalPath( selected );
        if foDisplayNodeName in Options then  	result := NodeName( pt )
        else		  				 			result := NodeContent( pt );
    	end
    else  if Assigned( miLink.DefaultValue ) then  begin
        result := miLink.DefaultValue( self, valid );
        end;
    end;


function    cField.GetPresentation : string;    //overload;

	begin
    result := GetPresentation( SelectedNode );
    end;


function    cField.GetRaw( selected : apNode ) : string;

	begin                                                             // in case presentation does something one day
    result := GetPresentation( SelectedNode );
    end;


function    cField.GetNode : apNode;

	begin
    result := Traverse( SelectedNode, mIndexStepBack, oIndexPath );
    end;


function	cField.Validate( var val : string; selected : apNode ) : aDDErrorCode;

	var
    	base : apNode;
	begin
    result := ecNone;
    if ( val = '' ) and ( foRequired in Options ) then  result := ecRequiredFieldIsBlank;
    if selected <> nil then  begin
    	base := Traverse( selected, mIndexStepBack, oIndexPath, 1 );   // points to 1 before node being changed
        if foAbstract in Options then  begin
            if FindName( base, val ) <> nil then  result := ecKeyNameIsNotUnique;
            end;
        // more validation  ...todo
        end;
    end;


function	cField.SetPresentation( selected : apNode; var val : string ) : aDDErrorCode;

	var
    	pt, base : apNode;
	begin
    if ( selected <> nil ) and ( foEditable in Options ) then  begin
        result := Validate( val, selected );
        if result = ecNone then  begin
            pt := Traverse( selected, mIndexStepBack, oIndexPath );        // may not exist - doesn't matter
            base := Traverse( selected, mIndexStepBack, oIndexPath, 1 );   // points to 1 before node being changed
            if foDisplayNodeName in Options then  begin  // typicaly an abstract/selector
            	if val <> '' then  begin
            		miLink.DB.GlobalRename( selected, val, miLink.ID );
                	end
                else result := ecRequiredFieldIsBlank;
            	end
            else  miLink.DB.GlobalEdit( base, mName, val, NodeContent( pt ), miLink.ID );
        	end;
    	end
    else  result := ecFieldIsNotEditable;
    end;


function	cField.GetTitle;

	begin
    result := UnCamel( mName );
    end;


// ____________________________________________________________________________________________________________________




constructor	cFieldList.Create( DBlink : ifDB; Edits : iEdits );

	begin
    //miLink := DBlink;
    miFieldDB.DB := DBlink.DB;
    miFieldDB.ID := DBlink.ID;
    if Assigned( Edits.GetSelected ) then  miFieldDB.GetSelected := Edits.GetSelected
    else  miFieldDB.GetSelected := ReturnBlank;
    if Assigned( Edits.DefaultValue ) then  miFieldDB.DefaultValue := Edits.DefaultValue
    else  miFieldDB.DefaultValue := ReturnBlank2;
    miFieldDB.FieldList := self;  // in case field descriptors want to access list stuff
    miEdits := Edits;
    oListIndexNodes := aNodeList.Create;
    miFieldDB.DB.RegisterReader( NewData );  // i want to know if anything changes
    end;


destructor	cFieldList.Destroy;  //override;

	begin
    miFieldDB.DB.DeRegisterReader( NewData );
    FreeAndNil( oListIndexNodes );
    end;


procedure	cFieldList.InitFields( base : asDbPath; shape : asDbStub; index : asDbNode );

	begin
    Clear;
    mIndexListBuilt := false;
    miFieldDB.DataDict := FollowPath( pathDataDictionary, miFieldDB.DB.GetRoot );
    miFieldDB.BaseNode := FollowPath( base, miFieldDB.DB.GetRoot );
    miFieldDB.ShapeBase := FollowPath( pathDataShape + shape, miFieldDB.DB.GetRoot );  // breaking change   pathDataShape
    miFieldDB.IndexField := index;    // used as basis for list build
    miFieldDB.IndexNode :=  ShapeEntry( index );
    end;


function	cFieldList.ReturnBlank : asDbPath;

	begin  result := '';  end;


function	cFieldList.ReturnBlank2( f : cField; out valid : boolean ) : string;

	begin  valid := false;  result := '';  end;


procedure   cFieldList.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	begin
    if mIndexListBuilt and ( miFieldDB.IndexField <> '' ) then  BuildIndexList;  // need a new list after deletes etc
    if Assigned( miEdits.NewData ) then  miEdits.NewData( xml );
    end;


function	UpStream( lower, higher : apNode ) : boolean;  // or equal

    begin
    result := false;
    while higher <> nil do  begin    // work back through the shape path
        if higher = lower then  begin
            result := true;
            break;
            end;
        higher := Back( higher, 1 );
    	end;
    end;


function	UpStreamPath( lower, higher : apNode ) : TStringList;  // or equal

    begin
    result := TStringList.Create;
    while higher <> nil do  begin    // work back through the shape path
        if higher = lower then  break;
        if IsAbstract( higher ) then  result.Insert( 0, '[0]' )
        else  result.Insert( 0, NodeName( higher ) );
        higher := Back( higher, 1 );
    	end;
    end;


function	cFieldList.ShapeEntry( name : asDbNode ) : apNode;

	begin                               // finds name in the shape info
	result := miFieldDB.DB.FindNodeMulti( miFieldDB.ShapeBase, name );
    end;


function	cFieldList.ResolveAbstractPath( name : asDbNode ) : TStringList;

    var                                 // makes an abstract dd path, ie contains flattening '[0]' values
    	pt : apNode;
	begin
    result := nil;
    pt := ShapeEntry( name );
    if pt <> nil then  begin
    	result := TStringList.Create;
        while pt.Back <> nil do  begin    // work back through the shape path - exclude root
            if IsAbstract( pt ) then  result.Insert( 0, '[0]' )
            else  result.Insert( 0, NodeName( pt ) );
            pt := Back( pt, 1 );
            end;
    	end;
    end;


function	cFieldList.AlignPaths( var orign, dest : TStringList ) : boolean;

	var
    	o, d, x : int;
	begin
    result := false;
    if ( orign <> nil ) and ( dest <> nil ) then  begin
        for o := 0 to orign.Count - 1 do  begin
            for d := 0 to dest.Count - 1 do  begin
                if ( dest[ d ] = orign[ o ] ) and ( dest[ d ] <> '[0]' ) then  begin
                    result := true;
                    for x := 0 to d - 1 do  dest.Delete( 0 );
                    for x := 0 to o - 1 do  orign.Delete( 0 );
                    break;
                    end;
                end;
            if result then  break;
            end;
    	end;
	end;


function	cFieldList.GoToIndex( pt : apNode  ) : apNode;

	var
    	orign, dest, travers : TStringList;
        traverseBack : int;
    begin
    result := nil;
    orign := ResolvePath( pt );                           // data path to root
    dest := ResolveAbstractPath( miFieldDB.IndexField );   // shape path from index back to base
    if AlignPaths( orign, dest ) then  begin
        travers := PathDif( orign, dest, traverseBack );
        result := Traverse( pt, traverseBack, travers );
        travers.Free;
    	end;
    orign.Free;
    dest.Free;
    end;


function	cFieldList.FindField( name : asDbNode ) : cField;

    var
    	f	: cField;
	begin
	result := nil;
    if Count > 0 then  begin
        for f in self do  begin           	// correct fields so path is wrt enumeration field not base field
            if f.mName = name then  begin
                result := f;
                break;
                end;
            end;
    	end;
    end;


function	cFieldList.FindField( tag : int ) : cField;

    var
    	f	: cField;
	begin
	result := nil;
    if Count > 0 then  begin
        for f in self do  begin           	// correct fields so path is wrt enumeration field not base field
            if f.mTag = tag then  begin
                result := f;
                break;
                end;
            end;
    	end;
    end;


function	cFieldList.GetNodeNumerically( addr : TList<integer> ) : apNode;

    var
    	n : int;
	begin
    result := BaseNode;
    for n in addr do  result := SubNode( result, n );
    end;


function	cFieldList.SubNodeCount( addr : TList<integer> ) : int;

	begin
    result := uDbTree.SubNodeCount( GetNodeNumerically( addr ) );
    end;


function	cFieldList.EditNode( addr : TList<integer>; NewText : string ) : aDDErrorCode;

	var
    	pt : apNode;
        f : cField;
	begin
    result := ecInvalidFieldName;
    pt := GetNodeNumerically( addr );
    f := FindField( NodeName( pt ) );
    if f <> nil then  begin
    	if f.Validate( NewText, pt ) = ecNone then  begin
            miFieldDB.DB.GlobalEdit( pt.Back, pt.NodeName, NewText, miFieldDB.ID );
            result := ecNone;
        	end;
    	end;
    end;


function	cFieldList.SelectedNode : apNode;

	var
    	sel : string;
	begin
    sel := miEdits.GetSelected;
    result := nil;
    if sel <> '' then  result := FollowPath( sel, miFieldDB.DB.GetRoot );
    end;


procedure	cFieldList.SetRelativePaths;

    var
    	f, index	: cField;
        new : TStringList;
	begin
    if Count > 0 then  begin
        index := FindField( miFieldDB.IndexField );  // get path from DataShape
        if index <> nil then  begin
            for f in self do  begin           	// correct fields so path is wrt enumeration field not base field
                if f.oShapeBasePath <> nil then  begin
                    new := PathDif( index.oShapeBasePath, f.oShapeBasePath, f.mIndexStepBack );  // relative path from index field to me
                    f.oIndexPath.Free;  // without leaking
                    f.oIndexPath := new;
                    end;
                end;
        	end;
    	end;
    end;


procedure	cFieldList.BuildIndexList;

    var                // uses index node and data shape to build a list of nodes
    	f	: cField;
        enum : TStringList;
        name, relPath : string;
        node : aNodeRec;
        x, r : int;

	procedure	Enumerate( base : apNode; depth : int );   // recurse once per dimension

    	var
        	x : int;
            sub, next : apNode;
    	begin
        x := -1;
        while EachSubNode( base, x, sub ) do  begin
            if depth = enum.Count then  begin
                node.np := sub;
                node.row := 0;
                // filter here .....
            	oListIndexNodes.Add( node );
            	end
            else  begin
                next := FollowPath( enum[ depth ], sub );
                if next <> nil then  Enumerate( next, depth + 1 );  // dig in one dimension
            	end;
        	end;
        end;

	begin                 // enumerate rows on each key field in relative path
    SetRelativePaths;
    oListIndexNodes.Clear;
    if miFieldDB.IndexField <> '' then  begin
        for f in self do  begin
            if ( f.mName = miFieldDB.IndexField ) and ( f.oShapeBasePath <> nil ) then  begin
                enum := TStringList.Create;
                for name in f.oShapeBasePath do  begin  // work through path finding key nodes to enumerate
                    if foAbstract in f.Options then  begin
                        enum.Add( relPath );
                        relPath := '';
                        //if enum.Count = 2 then  mSharePath := enum[ 1 ];  // save first split for foSupressShares
                        end
                    else  relPath := relPath + LSep + name;
                    end;
                Enumerate( miFieldDB.BaseNode, 1 ); // poly dimensional enumeration
                enum.Free;
                break;
                end;
            end;
        // filter and sort oIndex to taste ......

        // simple code share field suppession - assumes BaseNode is Abstract
        x := -1;
        for r := 0 to oListIndexNodes.Count - 1 do  begin
            node := oListIndexNodes[ r ];
            if loGroupSubs in ListOption then  begin
                if NodeIndex( node.np ) = 0 then  Inc( x );   // primary
                end
            else Inc( x );
            node.row := x;
            oListIndexNodes[ r ] := node;
            end;
    	mIndexListBuilt := true;
        end;
    end;


function	cFieldList.AddField( name : asDbNode; options : asFieldOption = []; widthEm : int = 0 ) : int;

	begin
    result := -1;
    if ( miFieldDB.DB <> nil ) and ( name <> '' ) then  begin
    	result := Add( cField.Create( miFieldDB, name, options, widthEm ) );
    	end;
    end;


procedure	cFieldList.AddFieldPath( name : asDbNode; absPath : asDbPath; options : asFieldOption = []; widthEm : int = 0 );  // eg 'PartNumber', '*PartPath|Description|' );

	var
    	f : cField;
	begin
    if ( miFieldDB.DB <> nil ) and ( name <> '' ) then  begin
    	f := cField.Create( miFieldDB, name, [ foPath ] + options, widthEm );
        f.mLinkPath := absPath;
    	Add( f );
    	end;
    end;


procedure	cFieldList.Clear;

	begin
    oListIndexNodes.Clear;
    end;


procedure	cFieldList.ClearIsTyping;

	var
    	f : cField;
    begin
    for f in self do  begin
        f.IsTyping := false;
    	end;
    end;


function	cFieldList.UpHillPath( fromField : cField; toFieldName : asDbNode ): TStringList;

    var                          // follow data dictionary path up hill replacing abstracts with [0]
    	toField	: cField;
        back, x		: int;
        dif : TStringList;
        pt : apNode;
	begin
    dif := nil;
    toField := FindField( toFieldName );  // get path from DataShape
    if ( fromField <> nil ) and ( toField <> nil ) then  begin
    	dif := PathDif( fromField.oShapeBasePath, toField.oShapeBasePath, back );  // work out how to get to field from enumerator field
        if back = 0 then  begin   // up hill path
            for x := 0 to dif.Count - 1 do  begin
                pt := FindName( miFieldDB.DataDict, dif[ x ] );
                pt := FindName( pt, contAbstract );
                if pt <> nil then  dif[ x ] := '[0]';  // flatten higher Abstract fields ie pick the first one
                end;
        	end
        else  FreeAndNil( dif );   // down hil path
    	end;
    result := dif;
    end;


function	cFieldList.UpdateFields : aDDErrorCode;

	var
        f : cField;
        val : string;
        valid : boolean;
	begin
    result := ecNone;
    for f in self do  begin                                            // check each field
    	if ( foEditable in f.Options ) {and not ( foSelector in f.Options )} then  begin
        	val := miEdits.FormValue( f, valid );
            if f.GetPresentation( SelectedNode ) <> val then  begin  // has the data changed ?
                if valid then  begin
                	result := f.Validate( val, SelectedNode );
                    if result = ecNone then  f.SetPresentation( SelectedNode, val );  // initiate a global change request
                    end
                else  result := ecInvalidDataField;
                end;
            end;
    	end;
    end;


function   cFieldList.TagName( logicalName : asDbNode ) : asDbNode;

	var
        f : cField;
        valid : boolean;
	begin
    result := '';
    f := FindField( logicalName );
    if f <> nil then  begin
    	result := miEdits.FormValue( f, valid );
    	if not valid then  result := '';
    	end;
    end;


//procedure	cFieldList.SetSelected( sel : asDbPath );
//
//	begin
//    miFieldDB.Selected := sel;
//    miEdits.NewData( '' );
//    end;


//function	cFieldList.GetSelected : asDbPath;
//
//	begin
//	result := miFieldDB.Selected;
//    end;

function	cFieldList.FollowLogicalPath( path : TStringList; base : apNode; backStep : int = 0 ) : apNode;

	var                  // work up from a physical base using a logical path
    	name : asDbNode;
        x : int;
        f : cField;
        valid : boolean;
	begin
    result := nil;
    if ( path <> nil ) and ( base <> nil ) then  begin
        if path.Count <= backStep then  result := Back( base, backStep - path.Count )
        else  begin
            result := base;
            for x := 0 to path.Count - 1 - backStep do  begin
                name := path[ x ];
                f := FindField( name );
                if ( f <> nil ) and ( foDisplayNodeName in f.Options ) then  begin
                    name := miEdits.FormValue( f, valid );
                    if not valid then  begin  result := nil;  break;  end;
                	end;
                result := FindName( result, name );
                end;  // for path
        	end;
    	end;
    end;


function	cFieldList.GeneratePhysicalPath( fieldFrom, fieldTo : asDbNode; pre : boolean = false ) : TStringList;

	var                           // produce a real path from DD shape abstract path - pre excludes last node
        fTo, fFrom : cField;                                                        // not pre excludes first
        pt, from : apNode;
        name : string;
        valid : boolean;
	begin
    result := TStringList.Create;
    fFrom := FindField( fieldFrom );
    fTo := FindField( fieldTo );
    if fTo <> nil then  begin
        if fFrom <> nil then  from := fFrom.mShape
        else                  from := nil;
        pt := fTo.mShape;
        if not pre or ( pt <> from ) then  begin    // exclude last step
            if pre then  pt := Back( pt, 1 );
            while true do  begin    // work back through the shape path
                if not pre and ( pt = from ) then  break;
                if IsAbstract( pt ) then  begin  // get abstract path names from editors value
                    name := miEdits.FormValue( FindField( NodeName( pt ) ), valid );
                    end
                else   name := NodeName( pt );
                if IsKey( pt ) then  name := name + ' ' + Attrib_Key;
                result.Insert( 0, name );
                if pt = from then  break;
                pt := Back( pt, 1 );
                if pt = nil then  break;   // abnormal escape
                end;  // while path building
            end;
        end;
    end;


function   cFieldList.NewSub( newField : asDbNode ) : aDDErrorCode;

	var
        f : cField;
        new : cXmlParser;
        pt, base : apNode;
        path, basePath : TStringList;
        val, name : string;
        valid : boolean;
    label	DontBother;
	begin
    result := ecNone;     // so far so good
    path := nil;

    if miFieldDB.BaseNode = nil then  begin
    	result := ecDDFailedToResolvePath;
        goto DontBother;      // need a base to attach to
        end;

    f := FindField( newField );
    if f = nil then  begin
        result := ecInvalidFieldName;
        goto DontBother;      // need a valid logical field name as new point of attachment
        end;

    name := miEdits.FormValue( f, valid );
    name := MakeTagNameLegal( name, true );
    if not valid or ( name = '' ) then  begin
    	result := ecInvalidFieldName;
        goto DontBother;      // need a valid actual field name as new point of attachment
        end;

    if foAbstract in f.Options then  begin   // check uniqueness
        base := FollowLogicalPath( f.oShapeBasePath, miFieldDB.BaseNode, 1 );
        if ( base <> nil ) and ( FindName( base, name ) <> nil ) then  begin
            result := ecKeyNameIsNotUnique;
            ErrorField := f.Tag;
        	goto DontBother;      // need a unique field name as new point of attachment
            end
        else for f in self do  begin                    // check requiredness
            if ( foRequired in f.Options ) or ( foAbstract in f.Options ) then  begin
                if miEdits.FormValue( f, valid ) = '' then  begin
                    result := ecRequiredFieldIsBlank;
                    ErrorField := f.Tag;
        			goto DontBother;      // need a non-blank field value for this field
                    end;
                end;
            end;
        end;

    new := cXmlParser.Create;
    for f in self do  begin    // build a tree to match the DataShape tree for relevant nodes
        if f.Above( newField ) and not ( foPath in f.Options ) then  begin
            if f.mName <> newField then  begin   // ignore base/abstract field - it is handled as the node name in RequestNew
                path := GeneratePhysicalPath( newField, f.mName );
                val := miEdits.FormValue( f, valid );
                if ( val <> '' ) or ( foKeep in f.Options ) then  begin   // if field is blank then don't bother
                    pt := new.ForcePath( path );            // then make equivalent path in new tree
                    if not ( foDisplayNodeName in f.Options ) then  SetContent( pt, val );
                    end;
                end;
            end;  // above
        end;  // for f

     if result = ecNone then  begin
        path.Free;
        path := GeneratePhysicalPath( NodeName( SubNode( miFieldDB.ShapeBase, 0 ) ), newField, true );
        basePath := ResolvePath( miFieldDB.BaseNode );
        for name in path do  basePath.Add( name );
        StartRequestNew( TagName( newField ) );
        AddToRequestNew( FormatAllSubNodes( new.GetRoot, 2 ) );
        miFieldDB.DB.SendRequest( EndRequestNew(  PathToStr( basePath ), '', '', miFieldDB.ID ) );
        end;
    new.Free;
    path.Free;

DontBother :
    end;


function	cFieldList.NewBranch : aDDErrorCode;

	var
        newBase : apNode;
	begin
    newBase := SubNode( miFieldDB.ShapeBase, 0 );
    result := NewSub( NodeName( newBase ) );           // eg 'RuleName'
    end;


procedure	cFieldList.DeleteBranch( fieldName : asDbNode );

	var
        f : cField;
        pt : apNode;
        r : string;
	begin
    for f in self do  begin
        if ( f.mName = fieldName ) and ( foAbstract in f.Options ) then  begin  // probably intended target
            pt := f.GetNode;
            r := FormatDelete( ResolvePathStr( pt ), miFieldDB.ID );
            miFieldDB.DB.SendRequest( r );
            // miFieldDB.Selected := '_nil';   // follow path -> nil
            break;
        	end;
    	end;
    end;


procedure	cFieldList.Ready;

	begin
	SetRelativePaths;
    //BuildRows;
    end;


end.
