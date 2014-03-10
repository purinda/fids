unit uXmlDB;    deprecated

// (C) ALphasoft Pty Ltd

interface      // AllocMem/FreeMem    Main Thread only

uses
	Classes, uGlobalDefs, uHashTable;

//  define GLOBAL if using Global.Log etc
// todo A CDATA section starts with "<![CDATA[" and ends with "]]>"

type
    apTag = ^ aTag;
    aTag = record            // a tree node
        ID      : word;      // copied out from root   Global.Data ID=1
        TagName : string;
        Content : string;
        Attribs : string;    // of aAttribute
        SubTags : TList;     // of apTag
        Back    : apTag;     // back path
        Modified : int;      // Seconds at last change
        Table : cHashTable<apTag>;   // nil or subtag table
        end;

	aTagType = ( ttStart, ttEnd, ttEmpty );

	aDBReadop = ( opNone, opRead, opScan, opSearch );
	aDBEditop = ( opEdNone, opNew, opRename, opDelete, opEdit );
var
	OpReadTagName : array [ aDBReadop ] of string = ( '????', TagRead, TagScan, TagSearch );
	OpEditTagName : array [ aDBEditop ] of string = ( '????', TagNewTag, TagRenameTag, TagDelete, TagEdit );

type

	poOnNextTag = procedure ( pt : apTag; depth : integer ) of object;
	aScanTagType = ( ttProlog, ttTag, ttTagEnd, ttComment, ttContent );

	cXmlDB = class
		public
			mTagCount : integer;       // stats
			mEditCount : integer;
			mNewOpTagCount : integer;

			constructor  Create( treeID : word = 0; isKey : boolean = false );  // 1 for global.data to do statistics
			procedure  Clear;
			procedure  Free;
			procedure  InitXmlDB;
			function   GetRoot : apTag;
			function   GetTag( const path : string ) : apTag;
			function   ReadSub( pt : apTag; n : integer; var name : string; var content : string ) : apTag;
			function   ReadTag( pt : apTag; var name : string; var content : string  ) : integer;
			function   FindTagMulti( pt : apTag; const tn : string ) : apTag;  // multi level search from pt/nil to TagName tn
			function   FindSubTag( pt : apTag; const tn : string ) : apTag;  // single level search from pt/nil to TagName tn
			function   OpenTag( path : string; pt : apTag = nil ) : apTag;
			function   Scan( path : TStringList;  NextTag : poOnNextTag ) : boolean;  overload;
			function   Scan( pt : apTag;  NextTag : poOnNextTag ) : boolean;   overload;
			procedure  Sort( pt : apTag; pathl : TStringList );
			//function   CollectList( pt : apTag ) : TStringList;
			function   FollowPath( pathl : TStringList; pt : apTag = nil ) : apTag;  overload;
			function   FollowPath( path : string; pt : apTag = nil ) : apTag;  overload;
			function   ForcePath( pathl : TStringList ) : apTag;
			function   ResolvePath( pt : apTag ) : TStringList;
			function   ListNames( const path : string; pt : apTag = nil ) : TStringList;
			function   TrimReply() : boolean;

			function   FormatAllTags( pTag : apTag; indent : integer ) : string;

			procedure  LoadFromFile( fil : string );
			function   LoadFromString( const source : string ) : boolean;
			procedure  SaveDBToFile( pt : apTag );
			procedure  SaveDBToFiles( const match : string );
			procedure  HandleAEditRequest( req : cXmlDB );
			function   HandleADataRequest( req : cXmlDB ) : string;
			function   LocalRequestHandler( const note : aNotice; loc : boolean = true ) : string;   // all DBreq comes via here
			procedure  RequestHandler( const note : aNotice );   // aReader - all net DBreq comes via here
		private
			//mParse : TXmlParser;
			mFileName : string;
			mProlog : string;
			//mReqID : string;
			mRootTag : aTag; // root of the xml tree

			//mDBop : aDBop;
			mTagsKeyRenamed : integer;
			mOptionsL, mPathL, mTagL, mContentMatchL, mTagMatchL : TStringList;
			mBasePath : apTag;
			mRequestTag : string;

			mScanState : ( ssOutTag, ssInTag, ssInContent );       //
			mScanEscapeChars : boolean;
			mScanLeaveWhiteSpace : boolean;
			mScanEscapeASChars : boolean;
			mScanTagType   : aScanTagType;
			mScanEmptyTag  : boolean;
			mScanTagXMLRaw : string;
			mScanTagXML    : string;
			mScanAttributes : string;

			mStringX : int;      // string as stream stuff
			mStringBuffer : string;
			mEr : int;
			mEOF : boolean;
			procedure  UnEscape( var it : string );
			function   Escape( const it : string ) : string;  // escapes tag content
			procedure  StreamScan;
			function   NextChar() : char;
			//function   NextByte() : byte;

			procedure  LogEr( er : int; const s : string );
			procedure  Load;
			procedure  DoScan( pt : apTag; depth : integer;  NextTag : poOnNextTag );
			function   NewTag( const nam, atrib : string; parent : apTag ) : apTag; // and add to list
			procedure  DisposeTag( pt : apTag ); // and unlink - searches list
			procedure  DisposeTagFast( pt : apTag ); // without unlink
			procedure  BuildTag( parent : apTag );
			//procedure  BuildTable( pt : apTag );
			function   FormatATag( pTag : apTag; indent : integer; first : boolean ) : string;
			function   FormatADataReply( edit : boolean; er : int ) : string;
			function   FormatEndDataReply( edit : boolean ) : string;  // on request obj
			function   ForceEditPath( pathl : TStringList ) : apTag;
			function   IncludeTag( pt : apTag; req : cXmlDB ) : boolean;
			function   DoSearchOp( pt, apt : apTag; req, ans : cXmlDB ) : boolean;
			function   ReadOp( req : cXmlDB ) : string;
			function   ScanOp( req : cXmlDB ) : string;
			function   SearchOp( req : cXmlDB ) : string;
			procedure  Replicate( sTag, dTag : apTag );
			procedure  NewOp( req : cXmlDB );
			procedure  RenameOp( req : cXmlDB );
			procedure  DeleteAllFast( pt : apTag; base : boolean = true );
			procedure  DeleteOp( req : cXmlDB );
			procedure  EditOp( req : cXmlDB );
			function   SetReadOperationAndOptions( req : cXmlDB ) : aDBReadop;
			function   SetEditOperationAndOptions( req : cXmlDB ) : aDBEditop;
			procedure  LoadFromFile_( fil : string );
			//function   ValidateUser() : boolean;  // todo
		end;

	function   	FindName( pt : apTag; nam : string ) : apTag;  // single level search ( with hash table )
    function	MakeTagNameLegal( const TagName : string ) : string;
	function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
		 const options : TStringList; pathl : TStringList = nil ) : string;  overload;   // makes a request from a path list
	function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
		 const options : string; path : string ) : string;  overload;   // makes a request from a path list
	function    SplitOffAttributes( var nam : string ) : string;
	function    PathCoincides( p1, p2 : TStringList ) : boolean;  // is a change to p1 relevant to p2
	function    ParseList( const path : string ) : TStringList;
	function    CollectList( pt : apTag ) : TStringList;
	function    ReadContent( pt : apTag; const name : string ) : string;
	function    BracketATag( n : string; kind : aTagType = ttStart; indent : int = 0 ) : string;  // no attributes
	function    FmtTerminalTag( const tag : string; const cont : string; indent : int = 0 ) : string;
	function    FmtEmptyTag( const tag : string; indent : int = 0 ) : string;
	procedure   StartRequestNew( const key : string );
	procedure   AddToRequestNew( const add : string );
	procedure   EndRequestNew( const path : string; id : int );
	procedure   FormatDelete( const path : string; id : int );


implementation

uses
	{$ifdef GLOBAL }
	uGlobal,
	{$endif }
	uUtils, Windows, ASCII, Dialogs, SysUtils, StrUtils;

var
	sXML : string;  // request format buffer

// _____________________________ utilities _____________________________________


function   GetDecimal( const s : string; var index : integer ) : integer;

	var
		x : int;
	begin
	Result := 0;    x := index;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ',' ) ) do  begin
		Inc( x );
		end;
	while ( x <= Length( s ) ) and
			( s[ x ] >= '0' ) and ( s[ x ] <= '9' )  do  begin
		Result := Result * 10 + ord( s[ x ] ) - ord( '0' );
		Inc( x );
		end;
	index := x;
	end;


function  Slice( const src : string; index : integer; count : integer ) : string;

	var
		i, j : integer;
	begin
	i := Length( src ) - index + 1;
	if i > count then  i := count;
	SetLength( Result, i );
	for j := 1 to i do  Result[ j ] := src[ index + j - 1 ];
	end;


function  SplitPathName( var name : string ) : integer;

	var                  // deprcated
		x : integer;
		nam : string;
	begin
	nam := name;
	x := Pos( SeparatorIndex, nam );
	if x > 0 then  begin  // path#nn to resolve
		Delete( name, x, Length( name ) - x + 1 );
		Delete( nam, 1, x );
		if not TryStrToInt( nam, x ) then x := 1;
		end
	else x := 1;
	Result := x;
	end;


function  FindName( pt : apTag; nam : string ) : apTag;

	var                  // searches pt subtags for nam - only single level search
		pl : TList;
		x, c : integer;
		px : integer;
	begin
	Result := nil;
    if pt <> nil then  begin
        if pt.Table <> nil then  begin
            result := pt.Table.Find( nam );
            end
        else  begin
            px := SplitPathName( nam );
            if pt <> nil then  begin
                pl := pt.SubTags;   c := 1;
                if pl <> nil then if pl.Count > 0 then  begin
                    for x := 0 to pl.Count - 1 do  begin
                        pt := pl[ x ];
                        if pt.TagName = nam then  begin
                            if c = px then  begin
                                Result := pt;
                                break;
                                end
                            else  Inc( c );
                            end;
                        end;
                    end;
                end;
            end;
	    end;
	end;


function   ReadContent( pt : apTag; const name : string ) : string;

	begin
	result := '';
	pt := FindName( pt, name );
	if pt <> nil then  result := pt.Content;
	end;


function   PutDelimiters( const path : string ) : string;

	begin
	result := path;
	if result <> '' then  begin
		if result[ 1 ] <> PathDelim then   Insert( PathDelim, result, 1 );
		if result[ Length( result ) ] <> PathDelim then  result := result + PathDelim;
		end
	else  result := '\';
	end;


procedure  StartRequestNew( const key : string );

	var
		tagName : string;
	begin
	tagName := WithoutSpaces( key );
	sXML := BracketATag( TagEditRequest, ttStart ) + EOL;
	sXML := sXML + TAB + BracketATag( TagNewTag, ttStart ) + ' ' + PutDelimiters( tagName ) + EOL;
	end;


procedure  AddToRequestNew( const add : string );

	begin
	sXML := sXML + add + EOL;
	end;


procedure  EndRequestNew( const path : string; id : int );

	var
	  req: cXmlDB;
	begin
	sXML := sXML + TAB + BracketATag( TagNewTag, ttEnd ) + EOL;

	sXML := sXML + TAB + BracketATag( TagPath, ttStart ) + path + BracketATag( TagPath, ttEnd ) + EOL;
	sXML := sXML + TAB + BracketATag( TagReqID, ttStart ) + ' ' + IntToStr( id ) + ' '
				 + BracketATag( TagReqID, ttEnd ) + EOL;
	sXML := sXML + BracketATag( TagEditRequest, ttEnd );
	req := cXmlDB.Create( 10 );
	req.LoadFromString( sXML );
	Global.MessageHub.Broadcast( nDBreq, sXML, -1, req );
	req.Free;
	end;


procedure  FormatDelete( const path : string; id : int );

	var
		req: cXmlDB;
		p : string;
	begin
	p := PutDelimiters( path );
	sXML := BracketATag( TagEditRequest, ttStart ) + EOL +
			TAB + '<' + TagDelete + '/>' + EOL +
			TAB + FmtTerminalTag( TagPath, p ) + EOL +
			TAB + FmtTerminalTag( TagReqID, IntToStr( id ) ) + EOL +
			BracketATag( TagEditRequest, ttEnd );
	req := cXmlDB.Create( 11 );
	req.LoadFromString( sXML );
	Global.MessageHub.Broadcast( nDBreq, sXML, -1, req );
	req.Free;
	end;


//___________________________ cXmlDB ___________________________________________

function CreateHashTable() : cHashTable<apTag>;

	begin
    result := cHashTable<apTag>.Create(
		function( pt : apTag ) : string  begin  result := pt.TagName;  end,
        procedure( var pt : apTag )  begin  pt := nil;  end,
        function( pt : apTag ) : boolean  begin  result := pt = nil;  end );
    end;


constructor  cXmlDB.Create( treeID : word = 0; isKey : boolean = false );

	begin
	mRootTag.TagName := RootTagName;
    mRootTag.ID := treeID;    // debug and stats use
    if isKey then  begin
        mRootTag.Attribs := TagAttribute_Key;
        mRootTag.Table := CreateHashTable;
    	end;
	end;


procedure  cXmlDB.LogEr( er : int; const s : string );

	begin
	 mEr := er;
	{$ifdef GLOBAL } Global.Log( er, s ) {$endif };
	end;


procedure  cXmlDB.DisposeTagFast( pt : apTag );

	begin            // does not unlink - ie ignores .Back - must unlink externally
	if pt.ID = 1 then  Dec( mTagCount );
	if pt.SubTags <> nil then  pt.SubTags.Free;
    if pt.Table <> nil then  pt.Table.Free;
	Dispose( pt );  //  FreeMem( pt );  need dispose to free strings
	end;


procedure  cXmlDB.DisposeTag( pt : apTag ); // and unlink

	var
		ppt : apTag;
	begin
	ppt := pt.Back;
    if ppt <> nil then  begin

		if ppt.SubTags <> nil then ppt.SubTags.Delete( ppt.SubTags.IndexOf( pt ) );   // unlink
        if ppt.Table <> nil then   ppt.Table.Delete( pt );
        end;
    DisposeTagFast( pt );

	{if ppt.SubTags.Count = 0 then  begin                 // todo could defer this
		ppt.SubTags.Free;
		ppt.SubTags := nil;
		end;
	pt.TagName := '';
	pt.Content := '';
	pt.Attribs := '';
	pt.SubTags.Free;
	if pt.ID = 1 then  Dec( mTagCount );
    if pt.Table <> nil then  pt.Table.Free;
	Dispose( pt );  // FreeMem( pt ); }
	end;


function   cXmlDB.NewTag( const nam, atrib : string; parent : apTag ) : apTag; // and add to list

	var
		pTag : apTag;
		er : boolean;
	begin
	er := false;   result := nil;
    // pTag := AllocMem( SizeOf( Ptag^ ) );
	if parent.Table <> nil then  begin
		if FindName( parent, nam ) <> nil then  begin   // collides
			if mEr < erNonUniqueNewKeyTag then  begin
				mEr := erNonUniqueNewKeyTag;
				LogEr( erNonUniqueNewKeyTag, 'Non Unique NewTag( ' + nam + ' ) on a Key tag' );
				end;
			er := true;
			end
		end;
	if not er then  begin
        New( pTag );
        pTag.TagName := nam;
		pTag.ID := parent.ID;    // all tags in tree have same ID
		if parent.SubTags = nil then  parent.SubTags := TList.Create; // forward link
		parent.SubTags.Add( pTag );
		pTag.Back := parent;
        if parent.Table <> nil then  parent.Table.Add( pTag );
		pTag.Attribs := atrib;
		if Pos( TagAttribute_Key, atrib ) > 0 then  begin
			pTag.Table := CreateHashTable;
			end
        else  pTag.Table := nil;
        pTag.SubTags := nil;
		{$ifdef GLOBAL } pTag.Modified := Global.Seconds; {$endif }
		Result := pTag;
		if pTag.ID = 1 then  Inc( mTagCount );
		end;
	end;


{procedure cXmlDB.ClearTag( pTag : apTag );   // dispose of everything upstream

	var
		pt : apTag;  // the sub tags
		x : integer;
	begin
	if pTag <> nil then  begin
		if pTag.SubTags <> nil then  begin
			for x := 0 to pTag.SubTags.Count - 1 do  begin
				pt := pTag.SubTags[ x ];
				if pt.SubTags <> nil  then  begin
					ClearTag( pt );
					end;
				{$ifndef LEAK }{
				pt.SubTags.Free;
				Dispose( pt );  // FreeMem( pt );  leaks the strings
				{$endif }{
				end;
			end;
		end;
	end; }


procedure cXmlDB.Clear;

	var
		pt : apTag;
	begin
	mFileName := '';
	mProlog := '';
	if mRootTag.SubTags <> nil then  begin
		for pt in mRootTag.SubTags do  DeleteAllFast( pt );

//		for x := 0 to mRootTag.SubTags.Count - 1 do  begin
//			pt := mRootTag.SubTags[ x ];
//			DeleteAllFast( pt );
//			end;
		mRootTag.SubTags.Free;   mRootTag.SubTags := nil;
		end;
	end;


procedure  cXmlDB.Free;

	begin
	if self <> nil then  begin
		Clear;
		mOptionsL.Free;
		mPathL.Free;
		mTagL.Free;
		mContentMatchL.Free;
		mTagMatchL.Free;
		Destroy;
		end;
	end;


procedure  cXmlDB.InitXmlDB;

	begin
	{$ifdef GLOBAL }
	Global.MessageHub.RegisterReader( Global.Data.RequestHandler );
	{$endif }
	end;


function   cXmlDB.GetRoot : apTag;

	begin
	Result := @mRootTag;
	end;


function   cXmlDB.GetTag( const path : string ) : apTag;

	var
		pl : TStringList;
	begin
	pl := ParseList( path );
	result := FollowPath( pl );
	pl.Free;
	end;


{procedure  cXmlDB.BuildTable( pt : apTag );

	var  ps : apTag;
	begin
	pt.Table.Free;
	if pt.SubTags <> nil then  begin
		pt.Table := CreateHashTable;
		for ps in pt.SubTags do  pt.Table.Add( ps );
		end;
	end; }


function   cXmlDB.TrimReply() : boolean;

	var
		pt0, pt1, pt2 : apTag;
		x : int;
	begin       // removes the envelope to match the original.
				// remove 2 base tags and remove a layer of depth
				// from tree typically to remove request tags.
	Result := false;
	pt0 := @ mRootTag;
	if pt0.SubTags <> nil then  if pt0.SubTags.Count = 1 then  begin
		pt1 := pt0.SubTags[ 0 ];
		if pt1.TagName = TagDataReply then  begin
			if pt1.SubTags <> nil then  begin
				pt2 := FindName( pt1, TagReqID );    // strip off <ReqID>....
				DisposeTag( pt2 );
				pt2 := FindName( pt1, TagScan );    // strip off <ReqScan>....
				DisposeTag( pt2 );
				if pt1.SubTags <> nil then  begin
					// link out pt1 tag ie <DataReply>
					for x := 0 to pt1.SubTags.Count - 1 do  begin
						pt2 := pt1.SubTags[ x ];
						pt2.Back := pt0;        // fix all the back links
						end;
					pt0.SubTags.Free;         // replace the root subtag list
					pt0.SubTags := pt1.SubTags;
					pt1.SubTags := nil;
					// if pt1.Table <> nil then  BuildTable( pt0 );   // don't need keys
					DisposeTagFast( pt1 );
					Result := true;
					end;
				end;
			end;
		end;
	end;


// ___________________________ xml formatting __________________________________


function   FmtTerminalTag( const tag : string; const cont : string; indent : int = 0 ) : string;

	var
		i : int;
	begin
	result := '';
	for i := 1 to indent do  Result := Result + TAB;
	Result := Result + '<' + tag + '> ' + cont + ' </' + tag + '>';
	end;


function   FmtEmptyTag( const tag : string; indent : int = 0 ) : string;

	var
		i : int;
	begin
	result := '';
	for i := 1 to indent do  Result := Result + TAB;
	Result := Result + '<' + tag + '/>';
	end;


function   BracketATag( n : string; kind : aTagType = ttStart; indent : int = 0 ) : string;

	var
		i : int;
	begin
	Result := '';
	for i := 1 to indent do  Result := Result + TAB;
	if kind = ttEnd then	Result := Result + '</'
	else					Result := Result + '<';
	Result := Result + n;
	if kind = ttEmpty then	Result := Result + '/>'
	else					Result := Result + '>';
	end;


procedure  FormatAList( const list : TStringList; const tagname : string; indent : integer; var r : string );

	var
		x : integer;
	begin
	if list <> nil then  begin
		for x := 1 to indent do r := r + TAB;
		r := r + BracketATag( tagname ) + '  ';
		for x := 0 to list.Count - 1 do  begin
			r := r + SeparatorList + list[ x ];
			end;
		r := r + SeparatorList + ' ' + BracketATag( tagname, ttEnd ) + EOL;
		end;
	end;


function	MakeTagNameLegal( const TagName : string ) : string;

    var
    	x : int;
        r, n : string;
        c : char;
    begin
    n := Trim( TagName );
    r := '';
    for x := 1 to Length( n ) do  begin
		c := n[ x ];
        case c of
			'a'..'z' : r := r + c;
			'A'..'Z' : r := r + c;
			'0'..'9' : r := r + c;
			'-','_','@','$','%','.' : r := r + c;
			else  if ( Length( r ) > 0 ) and ( r[ Length( r ) ] <> '_' ) then  r := r + '_';   // replace space etc with _
        	end;
    	end;
    result := r;
    end;


function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
	 const options : TStringList; pathl : TStringList = nil ) : string;  // makes a request from a path list

	var
		r : string;
		l : TStringList;
	begin
	if ed then  r := BracketATag( TagEditRequest )
	else        r := BracketATag( TagDataRequest );
	r := r + EOL;
	if options = nil then  begin
		l := TStringList.Create;
		FormatAList( l, req, 1, r );
		l.Free;
		end
	else  FormatAList( options, req, 1, r );
	FormatAList( pathl, TagPath, 1, r );
	r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;

	if ed then  r := r + BracketATag( TagEditRequest, ttEnd )
	else        r := r + BracketATag( TagDataRequest, ttEnd );
	Result := r;
	end;


function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
	 const options : string; path : string ) : string;  // makes a request from a path list

	var
		r : string;
		l : TStringList;
	begin
	if ed then  r := BracketATag( TagEditRequest )
	else        r := BracketATag( TagDataRequest );
	r := r + EOL;
	if options = nil then  begin
		l := TStringList.Create;
		FormatAList( l, req, 1, r );
		l.Free;
		end
	else  FormatAList( options, req, 1, r );
	r := r + FmtTerminalTag( TagPath, path, 1 ) + EOL;
	// FormatAList( pathl, TagPath, 1, r );
	r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;

	if ed then  r := r + BracketATag( TagEditRequest, ttEnd )
	else        r := r + BracketATag( TagDataRequest, ttEnd );
	Result := r;
	end;


function   FormatADataRequest( ed : boolean; const req : string; const id : string;
	 const options : TStringList;
	 pathl : TStringList = nil; TagL : TStringList = nil; ContentMatchL : TStringList = nil;
	 TagMatch : TStringList = nil ) : string;   // makes a request from a path list

	var
		r : string;
		l : TStringList;
	begin
	if ed then  r := BracketATag( TagEditRequest )
	else        r := BracketATag( TagDataRequest );
	r := r + EOL;
	if options = nil then  begin
		l := TStringList.Create;
		FormatAList( l, req, 1, r );
		l.Free;
		end
	else  FormatAList( options, req, 1, r );
	FormatAList( pathl, TagPath, 1, r );
	FormatAList( TagL, TagList, 1, r );
	FormatAList( ContentMatchL, TagMatchCont, 1, r );
	FormatAList( TagMatch, TagMatchTag, 1, r );
	r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;

	if ed then  r := r + BracketATag( TagEditRequest, ttEnd )
	else        r := r + BracketATag( TagDataRequest, ttEnd );
	Result := r;
	end;


function  Tabs( depth : int ) : string;

	var
		t : int;
	begin
	result := '';
	for t := 1 to depth do  result := result + TAB;
	end;


function   StartPath( var depth : int; pt : apTag ) : string;

	var
		pl : TStringList;
		t : int;
	begin
	result := '';
	if pt <> nil then  begin
		pl := TStringList.Create;
		while pt.Back <> nil do  begin
			pl.Insert( 0, pt.TagName );
			pt := pt.Back
			end;
		for t := 0 to pl.Count - 1 do  begin
			result := result + BracketATag( pl[ t ], ttStart, depth + t ) + EOL;
			end;
		depth := depth + pl.Count;   // return base depth
		pl.Free;
		end;
	end;


function   EndPath( var depth : int; pt : apTag ) : string;

	begin
	result := '';
	if pt <> nil then  begin
		while pt.Back <> nil do  begin
			result := result + BracketATag( pt.TagName, ttEnd, depth ) + EOL;
			pt := pt.Back;
			if depth > 0 then  Dec( depth );
			end;
		end;
	end;


function   SplitOffAttributes( var nam : string ) : string;

	var
		x : integer;
	begin
	Result := '';
	x := 1;
	while x <= Length( nam ) do  begin
		if nam[ x ] <= ' ' then  begin
			Result := RightStr( nam, Length( nam ) - x );
			nam := LeftStr( nam, x - 1 );
			Trim( Result );
			break;
			end;
		Inc( x );
		end;
	end;


function   ParseList( const path : string ) : TStringList;

	var                  //  \Head\Subhead\  style list
		x : integer;
		tg : string;
	begin
	Result := TStringList.Create;
	x := 1;
	while x <= Length( path ) do  begin  // skip initial whitespace and \
		if path[ x ] = SeparatorList then  begin
			Inc( x );
			break;
			end;
		Inc( x );
		end;

	while x <= Length( path ) do  begin   // build list
		if path[ x ] = SeparatorList then  begin
			Result.Add( tg );
			tg := '';
			end
		else tg := tg + path[ x ];
		Inc( x );
		end;
	if tg <> '' then  Result.Add( tg );
	end;


function   cXmlDB.ResolvePath( pt : apTag ) : TStringList;   // makes a path list to pt

	var
		sl : TStringList;
		n : string;
		ppt : apTag;   // p parent tag
		pct : apTag;   // p contending match tag
		x, c : integer;
	begin
	sl := TStringList.Create;
	while pt.Back <> nil do  begin
		n := pt.TagName;
		sl.Insert( 0, n );
		ppt := pt.Back;   // step back
		c := 1;           // count matching names to make a path#N entry
		for x := 0 to ppt.SubTags.Count - 1 do begin
			if pt = ppt.SubTags[ x ] then  break
			else begin
				pct := ppt.SubTags[ x ];
				if n = pct.TagName then  Inc( c );
				end;
			end;
		if c <> 1 then  begin
			n := n + '#' + IntToStr( c );
			sl[ 0 ] := n;
			end;
		pt := ppt;  // move to parent tag
		end;
	Result := sl;
	end;


function   PathCoincides( p1, p2 : TStringList ) : boolean;  // is a change to p1 relevant to p2

	var
		n : int;
	begin
	Result := true;
	if ( p1 <> nil ) and ( p2 <> nil ) then  begin
		n := 0;
		repeat
			if n >= p1.Count then  break;  // change here affects p2
			if n >= p2.Count then  break;
			if p1[ n ] <> p2[ n ] then  begin  // paths have diverged
				Result := false;
				break;
				end;
			Inc( n );
			until false;
		end
	else  Result := false;
	end;


function   cXmlDB.ReadSub( pt : apTag; n : integer; var name : string; var content : string ) : apTag;

	begin
	if pt = nil then  pt := @mRootTag;
	if ( pt.SubTags <> nil ) and ( pt.SubTags.Count >= n ) then  begin
		pt := pt.SubTags[ n ];
		name := pt.TagName;
		content := pt.Content;
		end;
	Result := pt;
	end;


function   cXmlDB.ReadTag( pt : apTag; var name : string; var content : string  ) : integer;

	begin
	Result := 0;
	if pt = nil then  pt := @mRootTag;
	name := pt.TagName;
	content := pt.Content;
	if pt.SubTags <> nil  then  Result := pt.SubTags.Count;
	end;


function   cXmlDB.OpenTag( path : string; pt : apTag = nil ) : apTag;

	var
		pl : TStringList;
	begin
	pl := ParseList( path );
	Result := FollowPath( pl, pt );
	pl.Free;
	end;


function   cXmlDB.FindTagMulti( pt : apTag; const tn : string ) : apTag;

	var                  // multi level search from pt to TagName tn
		pl : TList;
		x : integer;
	begin
	if pt = nil then  pt := @mRootTag;
	Result := nil;
	pl := pt.SubTags;
	if ( pl <> nil ) and ( pl.Count > 0 ) then  begin
		for x := 0 to pl.Count - 1 do  begin
			pt := pl[ x ];
			if pt.TagName = tn then  begin Result := pt;  break;  end;
			Result := FindSubTag( pt, tn );
			if Result <> nil then  break;
			end;
		end;
	end;


function   cXmlDB.FindSubTag( pt : apTag; const tn : string ) : apTag;  // single level search from pt/nil to TagName tn

	begin
	if pt = nil then  pt := @mRootTag;
	Result := FindName( pt, tn );     // single level search   todo non-indexed
	{
	var
		pl : TList;
		x : integer;
    pl := pt.SubTags;
	if ( pl <> nil ) and ( pl.Count > 0 ) then  begin
		for x := 0 to pl.Count - 1 do  begin
			pt := pl[ x ];
			if pt.TagName = tn then  begin Result := pt;  break;  end;
			end;
		end;}
	end;


function   cXmlDB.ListNames( const path : string; pt : apTag = nil ) : TStringList;

	var
		n : int;
		pn : apTag;
	begin
	result := nil;
	pt := OpenTag( path, pt );
	if pt <> nil then  begin
		result := TStringList.Create;
		if pt.SubTags <> nil then  begin
			for n := 0 to pt.SubTags.Count - 1 do  begin
				pn := pt.SubTags[ n ];
				result.Add( pn.TagName );
				end;
			end;
		end;
	end;


function  cXmlDB.FollowPath( pathl : TStringList; pt : apTag = nil ) : apTag;

	var              // Follows the pathl list from root
		n, x, i : integer;
		nam : string;
	begin
	n := 0;
	result := nil;
	if pathl <> nil then  begin
	if pt = nil then  result := @mRootTag  else  result := pt;
		while n < pathl.Count do  begin
			// Result := nil;
			nam := Pathl[ n ];
			if nam <> '' then  begin
				if nam[ 1 ] = SubTagBracket then  begin
					x := 2;
					i := GetDecimal( nam, x );
					if ( i > 0 ) and ( i <= result.SubTags.Count ) then  begin
						result := result.SubTags[ i ];      // step in
						end
					else result := nil;
					end
				else  result := FindName( result, nam );    // step in
				end
			else  result := nil;
			if result <> nil then  Inc( n )    // next tag depth
			else  break;  // doesn't exist
			end;
		end;
	end;


function   cXmlDB.FollowPath( path : string; pt : apTag = nil ) : apTag;//  overload;

	var
		pl : TStringList;
	begin
	pl := ParseList( path );
	Result := FollowPath( pl, pt );
	pl.Free;
	end;


function  cXmlDB.ForcePath( pathl : TStringList ) : apTag;

	var                      // follows or creates pathl list
		pt, pn : apTag;
		n : integer;
		nam, atrib : string;
	begin
	n := 0;
	pt := @mRootTag;
	while n < pathl.Count do  begin
		if pt.SubTags = nil  then   begin    // create subtag list as well as new tag
			nam := pathl[ n ];
			atrib := SplitOffAttributes( nam );  // don't try to match attributes
			SplitPathName( nam );                // remove #
			pn := NewTag( nam, atrib, pt  );
			//pn.Attribs := atrib;
			end
		else  begin
			nam := pathl[ n ];
			atrib := SplitOffAttributes( nam );  // don't rty to match attributes
			pn := FindName( pt, nam );
			if pn = nil then  begin            // not found so create one
				pn := NewTag( nam, atrib, pt );
				//pn.Attribs := atrib;
				end;
			end;
		Inc( n );  pt := pn;
        if pt = nil then  break;
		end;
	Result := pt;
	end;


function  cXmlDB.ForceEditPath( pathl : TStringList ) : apTag;

	var                      // follows or creates pathl list if not key
		pt, pn : apTag;
		n : integer;
		nam, atrib : string;
	begin
	n := 0;
	if pathl.Count > 0 then  pt := @mRootTag  else  pt := nil;
	pn := nil;
	while n < pathl.Count do  begin
		if pt.SubTags = nil  then   begin    // create subtag list as well as new tag
			if pt.Table <> nil then  begin
				pt := nil;
				break;
				end;
			nam := pathl[ n ];
			atrib := SplitOffAttributes( nam );  // don't try to match attributes
			SplitPathName( nam );                // remove #
			pn := NewTag( nam, atrib, pt );
			end
		else  begin
			nam := pathl[ n ];
			atrib := SplitOffAttributes( nam );  // don't rty to match attributes
			pn := FindName( pt, nam );
			if pn = nil then  begin            // not found so create one
				if pt.Table <> nil then  begin
					pt := nil;
					break;
					end;
				pn := NewTag( nam, atrib, pt );
				end;
			end;
		Inc( n );  pt := pn;
        if pt = nil then  break;
		end;
	Result := pt;
	end;


procedure  cXmlDB.DoScan( pt : apTag; depth : integer;  NextTag : poOnNextTag );

	var
		x : integer;
	begin
	NextTag( pt, depth );
	if pt.SubTags <> nil then  begin
		for x := 0 to pt.SubTags.Count - 1 do  begin
			DoScan( pt.SubTags[ x ], depth + 1, NextTag );
			end;
		end;
	end;


function   cXmlDB.Scan( path : TStringList;  NextTag : poOnNextTag ) : boolean;

	var
		pt : apTag;
		depth : integer;
	begin
	Result := true;

	if path <> nil then  pt := FollowPath( path )
	else                 pt := GetRoot();
	if pt <> nil then  begin
		if path <> nil then  depth := path.Count
		else  depth := 0;
		DoScan( pt, depth, NextTag );
		end
	else  Result := false;
	end;


function   cXmlDB.Scan( pt : apTag;  NextTag : poOnNextTag ) : boolean;

	var
		x : integer;
	begin
	Result := true;
	if pt.SubTags <> nil then  begin
		for x := 0 to pt.SubTags.Count - 1 do  begin
			DoScan( pt.SubTags[ x ], 1, NextTag );
			end;
		end;
	end;


function   NamedContent( pt : apTag; const nam : string ) : string;

	begin
	pt := FindName( pt, nam );
	if pt = nil then  Result := ''
	else              Result := pt.Content;
	end;


procedure  cXmlDB.Sort( pt : apTag; pathl : TStringList );

	var
		x, y : int;
		pta, ptb : apTag;
		sl : TList;  // of apTag
		done : boolean;
		nam : string;
	begin
	if pt = nil then  pt := @mRootTag;
	if ( pathl <> nil ) then if pathl.Count > 0 then  begin
		sl := TList.Create;
		if pathl.Count = 1 then  begin  // simpler faster case
			nam := pathl[ 0 ];
			if pt.SubTags <> nil then for x := 0 to pt.SubTags.Count - 1 do  begin
				pta := pt.SubTags[ x ];   done := false;
				for y := 0 to sl.Count - 1 do  begin  // slip it into the right spot  todo index or halve interval
					ptb := sl[ y ];
					if NamedContent( ptb, nam ) > NamedContent( pta, nam ) then  begin  // insert before
						sl.Insert( y, pta );
						done := true;
						break;
						end;
					end;  // for y
				if not done then  sl.Add( pta );
				end;  // for x
			end;  // todo pathl > 1  probably never wanted
		pt.SubTags.Free;
		pt.SubTags := sl;  // use sorted list now
		end; 
	end;




// ______________________________ format XML ___________________________________

function   cXmlDB.FormatATag( pTag : apTag; indent : integer; first : boolean ) : string;

	const                            // makes XML r from a single tag - call twice - first and not first
		MaxSingleLineContent = 40;
	var
		r : string;
		i, c : integer;
		t, cont : string;
	begin
	r := '';
	if pTag.SubTags = nil then  c := 0
	else                        c := pTag.SubTags.Count;
	if first then  begin
		for i := 1 to indent do  r := r + TAB;
		t := pTag.TagName;
        cont := Escape( pTag.Content );
		if pTag.Attribs <> '' then
        	t := t + ' ' + pTag.Attribs;
		if ( c = 0 ) and ( cont = '' ) then  begin  // empty tag
			r := r + '<' + t + '/>' + EOL;
			end
		else  begin
			r := r + BracketATag( t );
			if ( c = 0 ) and ( Length( cont ) <= MaxSingleLineContent ) then  begin      // pack onto one line
				r := r + ' ' + cont + ' ' + BracketATag( pTag.TagName, ttEnd ) + EOL;    // tag .... /tag case
				end
			else if cont <> '' then begin  // multi line content type
				r := r + EOL;
				for i := 1 to indent + 1 do  r := r + TAB;
				r := r + cont + EOL;
				end
			else if c > 0 then  r := r + EOL;
			end;
		end
	else  begin  // possible end tag
		if ( c <> 0 ) or ( Length( pTag.Content ) > MaxSingleLineContent ) then  begin
			for i := 1 to indent do  r := r + TAB;
			r := r + BracketATag( pTag.TagName, ttEnd ) + EOL;
			end;
		end;
	Result := r;
	end;


function  cXmlDB.FormatAllTags( pTag : apTag; indent : integer ) : string;

	var                     // multi level XML maker starting with pTag
		pt : apTag;
		x  : integer;
	begin
	result := '';
	if pTag <> nil then  begin
        if pTag <> @ mRootTag then  Result := FormatATag( pTag, indent, true )
        else Dec( indent );
		if pTag.SubTags <> nil  then  begin
			for x := 0 to pTag.SubTags.Count - 1 do  begin
				pt := pTag.SubTags[ x ];
				Result := Result + FormatAllTags( pt, indent + 1 );
				end;
			end;
		if pTag <> @ mRootTag then  Result := Result + FormatATag( pTag, indent, false );
		end;
	end;


function  cXmlDB.FormatADataReply( edit : boolean; er : int ) : string;  // on request obj

	var
		r : string;
		pr, pt : apTag;
	begin
	if edit then  r := BracketATag( TagEditReply )
	else          r := BracketATag( TagDataReply );
	r := r + EOL + BracketATag( TagResult, ttStart, 1 );
	if er <> 0 then  	r := r + ' ERROR ' + IntToStr( er )
	else         		r := r + ' OK';
	r := r + EOL;
	pr := GetRoot();
	if pr.SubTags <> nil then  begin
		pr := pr.SubTags[ 0 ];
		if ( pr <> nil ) and ( pr.SubTags <> nil ) then  begin
			pt := pr.SubTags[ 0 ];
			r := r + FormatAllTags( pt, 2 );
			pt := FindName( pr, 'Path' );
			if pt <> nil then  begin
				r := r + FormatAllTags( pt, 2 );
				end;
			pt := FindName( pr, 'ReqID' );
			if pt <> nil then  begin
				r := r + FormatAllTags( pt, 2 );
				end;
			end;
		end;
	r := r + BracketATag( TagResult, ttEnd, 1 ) + EOL;
	result := r;
	end;


function  cXmlDB.FormatEndDataReply( edit : boolean ) : string;  // on request obj

	begin
	if edit then  result := BracketATag( TagEditReply, ttEnd )
	else          result := BracketATag( TagDataReply, ttEnd );
	end;


// _____________________________ Req handler ___________________________________

procedure  CopyTag( const src : aTag; var dest : aTag );

	begin
	// dest.TagName := src.TagName;  done by build
	dest.Content := src.Content;
	dest.Attribs := src.Attribs;
	// dest.fPtr    := src.fPtr;
	// dest.KeyTag  := src.KeyTag;    todo Table <> nil
	end;


function   CollectList( pt : apTag ) : TStringList;

	begin
	if pt <> nil then  Result := ParseList( pt.Content )
	else               Result := TStringList.Create;
	end;


function   cXmlDB.IncludeTag( pt : apTag; req : cXmlDB ) : boolean;

	var
		x : integer;
		ok : boolean;
	begin
	ok := true;
	if req.mTagL.Count <> 0 then  begin // must be in the required tags list
		ok := req.mTagL.Find( pt.TagName, x );
		end;
	if ok then  begin
		if req.mContentMatchL.Count <> 0 then  begin // matchl must be in tag content
			ok := false;
			for x := 0 to req.mContentMatchL.Count - 1 do  begin
				if Pos( req.mContentMatchL[ x ], pt.Content ) > 0 then  begin
					ok := true;
					break;
					end;
				end;
			end;
		if ok then  begin
			if req.mTagMatchL.Count <> 0 then  begin // matchl must be in tag name
				ok := false;
				for x := 0 to req.mTagMatchL.Count - 1 do  begin
					if Pos( req.mTagMatchL[ x ], pt.TagName ) > 0 then  begin
						ok := true;
						break;
						end;
					end;
				end;
			end;
		end;
	Result := ok;
	end;


function  cXmlDB.ReadOp( req : cXmlDB ) : string;

	var
		x, d : integer;
		pt, pBase : apTag;
		r : string;
	begin
	result := '';
	pBase := mBasePath;
	if pBase <> nil then  begin
		d := 1;
		r := StartPath( d, pBase );
		if pBase.Content <> '' then	r := r + ' ' + pBase.Content;
		if pBase.SubTags <> nil then  begin
			for x := 0 to pBase.SubTags.Count - 1 do begin
				pt := pBase.SubTags[ x ];
				if pt.Attribs <> '' then  begin
					r := r + BracketATag( pt.TagName + ' ' + pt.Attribs, ttEmpty, d ) + EOL;
					end
				else  r := r + BracketATag( pt.TagName, ttEmpty, d ) + EOL;
				end;
			end;
		result := r + EndPath( d, pBase );
		end;
	end;


function   cXmlDB.ScanOp( req : cXmlDB ) : string;

	var
		d : integer;
		pBase : apTag;
	begin
	result := '';
	pBase := mBasePath;
	if pBase <> nil then  begin
		d := 1;   result := '';
		if pBase.Back <> nil then  result := StartPath( d, pBase.Back );
		result := result + FormatAllTags( pBase, d );
		if pBase.Back <> nil then  result := result + EndPath( d, pBase.Back );
		end;
	end;


function   cXmlDB.DoSearchOp( pt, apt : apTag; req, ans : cXmlDB ) : boolean;
{                 pt=this tag in data, apt=ans parent
if terminal
	if  incl
		make new answer and copy
		retn true        to keep
	else
		pass back false   to maybe demolish temp answer chain
else
	make temp answer tag
	if not ( keep deeper  or incl(this) )
		dispose
		pass back false
}
	var
		pa : apTag;  // p to current answer tag
		pn : apTag;  // p to next downstream data
		x : integer;
		keep, k : boolean;
	begin
	Result := false;
	if pt.SubTags = nil then  begin        // if terminal
		if IncludeTag( pt, req ) then  begin   // if keep me
			// keep := true;
			pa := ans.NewTag( pt.TagName, pt.Attribs, apt );
            if pa <> nil then  begin
				CopyTag( pt^, pa^ );
				Result := true;                      // and pass back keep
				end;
	        end;
		end
	else  begin                            // not terminal
		pa := ans.NewTag( pt.TagName, pt.Attribs, apt );   // make temp answer tag to chain to possible deeper answers
		// CopyTag( pt^, pa^ );  defer
		keep := IncludeTag( pt, req );
		for x := 0 to pt.SubTags.Count - 1 do  begin
			pn := pt.SubTags[ x ];
			k := DoSearchOp( pn, pa, req, ans );
			keep := keep or k;
			end;
		if keep then  begin
			Result := true;
			CopyTag( pt^, pa^ );  // defered keepind ans tag so fill it in
			end
		else  begin
			ans.DisposeTag( pa );
			end;
		end;
	end;


function  cXmlDB.SearchOp( req : cXmlDB ) : string;

	var
		x : integer;
		pt, pBase, pAnsBase : apTag;
		ans : cXmlDB;
	begin
	pBase := mBasePath;
	ans := cXmlDB.Create( 6 );
	ans.mBasePath := ans.ForcePath( mPathL );  // force a corresponding path in the ans set
	pAnsBase := ans.mBasePath;
	if ( pBase <> nil ) and ( pAnsBase <> nil ) then  begin
		req.mTagL.Sort;  //  do faster tag name matching
		if pBase.SubTags <> nil then  begin
			for x := 0 to pBase.SubTags.Count - 1 do begin
				pt := pBase.SubTags[ x ];
				DoSearchOp( pt, pAnsBase, req, ans );
				end;
			end;
		end;
	result := ans.FormatAllTags( nil, 1 );
	ans.Free;
	end;


procedure  cXmlDB.Replicate( sTag, dTag : apTag );

	var
		x : integer;
		ps, pt : apTag;
	begin
	if ( sTag <> nil ) and ( dTag <> nil ) then  begin
		if sTag.SubTags <> nil then  begin
			for x := 0 to sTag.SubTags.Count - 1 do  begin
				ps := sTag.SubTags[ x ];
				pt := NewTag( ps.TagName, ps.Attribs, dTag );
                if pt <> nil then  begin
					CopyTag( ps^, pt^ );
					Replicate( ps, pt );
	                end;
				end;
			end;
		end;
	end;


procedure  cXmlDB.NewOp( req : cXmlDB );

	var
		pt : apTag;    // end of path
		pr : apTag;    // request tag
		pn : apTag;	// newly created
		atr, nam : string;
	begin
	mBasePath := ForcePath( mPathL );       // find base path in data - adjusted for new and edit
	pt := mBasePath;
	if mOptionsL <> nil then  begin
		if mOptionsL.Count = 1 then  mOptionsL.Add( '' );  // can set content to empty
		if mOptionsL.Count >= 2 then  begin
			nam := mOptionsL[ 0 ];
			atr := SplitOffAttributes( nam );
			pn := NewTag( nam, atr, pt );
            if pn <> nil then  begin
                pn.Content := mOptionsL[ 1 ];
                Inc( mNewOpTagCount );
                pr := req.mBasePath;            // and replicate all sub tags from request
                Replicate( pr, pn );
            	end
            else  LogEr( erAttemptToCreateNonuniqueKeyTag, ' NewOp: Attempt to create Nonunique Key Tag' );
			end;
		end;
	end;


procedure  cXmlDB.RenameOp( req : cXmlDB );


	var
		pt : apTag;
		nam : string;
	begin
	pt := mBasePath;
	if ( pt <> nil ) and ( pt <> @mRootTag ) then  begin
		if mOptionsL <> nil then  begin
			if mOptionsL.Count >= 1 then  begin
				nam := mOptionsL[ 0 ];
				nam := MakeTagNameLegal( nam );
				if nam <> '' then  begin
					if pt.Back.Table <> nil then  begin
						pt.Back.Table.Delete( pt );
						pt.TagName := nam;
						pt.Back.Table.Add( pt );
						end
					else  pt.TagName := nam;
					if mOptionsL.Count >= 2 then  pt.Attribs := mOptionsL[ 1 ];
					Inc( mEditCount );
					{$ifdef GLOBAL } pt.Modified := Global.Seconds; {$endif }
					end
				else  LogEr( erRenNul, 'RenameOp to null' );
				end
			else  LogEr( erRenToEmpty, 'RenameOp To Empty' );
			end
		else  LogEr( erRenToEmpty, 'RenameOp To Empty' );
		end
	else  LogEr( erRenNonExistent, 'RenameOp Non Existent Tag' );
	end;


procedure  cXmlDB.DeleteAllFast( pt : apTag; base : boolean = true );

	var
		pd : apTag;
		x  : integer;
	begin        // only unlinks the base tag
	if ( pt <> nil ) and ( pt.SubTags <> nil ) then  begin
		for x := pt.SubTags.Count - 1 downto 0 do  begin
			pd := pt.SubTags[ x ];
			DeleteAllFast( pd, false );
			end;
		// if pt.SubTags <> nil then  pt.SubTags.Free;
        // if pt.Table <> nil then  pt.Table.Free;
		end;
	if pt <> nil then  begin
		if base then  DisposeTag( pt )       // and unlink
		else          DisposeTagFast( pt );
		end;
	end;


procedure  cXmlDB.DeleteOp( req : cXmlDB );

	var
		pt : apTag;    // base tag to be deleted
		ppt : apTag;   // parent of pt
	begin
	pt := mBasePath;           // unlink base tag to and use fast dispose
	if pt <> nil then  begin
    	ppt := pt.Back;
        if ppt <> nil then  begin
//			x := ppt.SubTags.IndexOf( pt );
//			ppt.SubTags.Delete( x );
//			if ppt.Table <> nil then  ppt.Table.Delete( pt );  // remove reference to me
			mBasePath := pt.Back;
            if ( pt <> @mRootTag ) and ( pt <> nil ) then  begin        // dispose of all deeper
				DeleteAllFast( pt );
				end;
			end
		else  LogEr( erDelNonExistent, 'DeleteOp Non Existent Tag' );
		end
	else  LogEr( erDelNonExistent, 'DeleteOp Non Existent Tag' );
	end;


procedure  cXmlDB.EditOp( req : cXmlDB );   // option[0] = new content  { option[1] = old content to match }

	var
		pt : apTag;
	begin
	pt := ForceEditPath( mPathL );       // find base path in data
	mBasePath := pt;
	if ( pt <> nil ) and ( pt <> @mRootTag ) then  begin
		if mOptionsL <> nil then  begin
			if mOptionsL.Count = 1 then  begin
				pt.Content := Trim( mOptionsL[ 0 ] );
				{$ifdef GLOBAL } pt.Modified := Global.Seconds; {$endif }
				Inc( mEditCount );
				end
			else  begin
				if mOptionsL.Count >= 2 then  begin
					if pt.Content = Trim( mOptionsL[ 1 ] ) then  begin
						pt.Content := Trim( mOptionsL[ 0 ] );  // todo Trim ?, test is problematic
						{$ifdef GLOBAL } pt.Modified := Global.Seconds; {$endif }
						Inc( mEditCount );
						end
					else  LogEr( erEditMismatch, 'EditOp Mismatch' );
					end;
				end;
			end;
		end
	else  LogEr( erEditNonExistent, 'EditOp NonExistent' );
	end;


function   cXmlDB.SetReadOperationAndOptions( req : cXmlDB ) : aDBReadop;

	var
		o : aDBReadop;
		pt, pr : apTag;
	begin
	pr := @req.mRootTag;                      // get operation type
	Result := opNone;
	for o := Low( aDBReadop ) to High( aDBReadop ) do  begin
		if o <> opNone then  begin
			pt := req.FindTagMulti( pr, OpReadTagName[ o ] );
			if pt <> nil then  begin
				mOptionsL := CollectList( pt );
				Result := o;
				req.mBasePath := pt;        // $$$ redundant ?
				req.mRequestTag := pt.TagName;
				break;
				end;
			end;
		end;
	pt := req.FindTagMulti( pr, TagPath );        // get parameter lists
	mPathL := CollectList( pt );
	mBasePath := FollowPath( mPathL );       // find base path in data - adjusted for new and edit
	end;


function   cXmlDB.HandleADataRequest( req : cXmlDB ) : string;

	var
		op : aDBReadop;
	begin
	result := '';
	mEr := 0;
	op := SetReadOperationAndOptions( req );
	case op of
		opRead   : result := ReadOp( req );
		opScan   : result := ScanOp( req );
		opSearch : result := SearchOp( req );
		else       LogEr( erUndefinedDataOp, 'Undefined Data Op' );
		end;
	end;


function   cXmlDB.SetEditOperationAndOptions( req : cXmlDB ) : aDBEditop;

	var
		o : aDBEditop;
		pt, pr : apTag;
	begin
	pr := @req.mRootTag;                      // get operation type
	Result := opEdNone;
	for o := Low( aDBEditop ) to High( aDBEditop ) do  begin
		if o <> opEdNone then  begin
			pt := req.FindTagMulti( pr, OpEditTagName[ o ] );
			if pt <> nil then  begin
				mOptionsL := CollectList( pt );
				Result := o;
				req.mBasePath := pt;        // $$$ redundant ?
				req.mRequestTag := pt.TagName;
				break;
				end
			end;
		end;
	pt := req.FindTagMulti( pr, TagPath );
	mPathL := CollectList( pt );
	mBasePath := FollowPath( mPathL );       // find base path in data - adjusted for new and edit
	end;


procedure  cXmlDB.HandleAEditRequest( req : cXmlDB );

	var
		op : aDBEditop;
	begin
	mEr := 0;
	op := SetEditOperationAndOptions( req );
	case op of
		opNew    : NewOp( req );
		opRename : RenameOp( req );
		opDelete : DeleteOp( req );
		opEdit   : EditOp( req );
		else       LogEr( erUndefinedEditOp, 'Undefined Edit Op' );
		end;
	end;


{function   cXmlDB.ValidateUser() : boolean;  // todo

	begin
	Result := true;
	end;  }


procedure  cXmlDB.RequestHandler( const note : aNotice );   // all DBreq comes via here

	begin
	{$ifdef GLOBAL } if Global.ID = Global.HostID then  begin {$endif }
		LocalRequestHandler( note, false );
		{$ifdef GLOBAL } end;   {$endif }
	end;


function  cXmlDB.LocalRequestHandler( const note : aNotice; loc : boolean = true ) : string;   // all DBreq comes via here

	var
		req : cXmlDB;
		pTag : apTag;
		response : string;
		ed : bool;
	begin
	result := '';
	if ( note.Note = nDBreq ) and ( note.NoteDB <> nil ) then  begin
		req := cXmlDB( note.NoteDB );    ed := false;
		if ( req.mRootTag.SubTags <> nil ) and ( req.mRootTag.SubTags.Count > 0 ) then  begin
			pTag := req.mRootTag.SubTags[ 0 ];
			if pTag.TagName = TagDataRequest then  begin
				ed := false;
				response := HandleADataRequest( req )   // xml data read
				end
			else  if pTag.TagName = TagEditRequest then  begin
				ed := true;
				HandleAEditRequest( req );
				end;
			result := req.FormatADataReply( ed, mEr ) + response + FormatEndDataReply( ed );
			{$ifdef GLOBAL } if not loc then  Global.MessageHub.Broadcast( nDB, result );  {$endif }
			mOptionsL.Free;   	mOptionsL := nil;
			mPathL.Free;        mPathL := nil;
            // mTagL.Free;			mTagL := nil;
			end;
		end;
	end;


	//______________________________________ parser ______________________________


function   cXmlDB.NextChar() : char;

	begin
	Result := nul;
	if mStringX > Length( mStringBuffer ) then
		mEOF := true
	else  Result := mStringBuffer[ mStringX ];
	Inc( mStringX );
	end;


procedure cXmlDB.UnEscape( var it : string );

//       \ -> &.
//       # -> &n

	var
		x : integer;
		code : string;
	begin
	if not mScanLeaveWhiteSpace then  begin
		it := Trim( it );    // strip off lead and trail white space
		end;

	if mScanEscapeChars then  begin
		x := 1;
		while x <= Length( it ) do  begin    // unescape std xml codes
			if it[ x ] = '&' then  begin
				code := Slice( it, x + 1, 4 );
				if Pos( 'lt', code ) = 1  then  begin
					Delete( it, x, 2 );
					it[ x ] := '<';
					end
				else if Pos( 'gt', code ) = 1  then  begin
					Delete( it, x, 2 );
					it[ x ] := '>';
					end
				else if Pos( 'amp', code ) = 1  then  begin
					Delete( it, x, 3 );
					it[ x ] := '&';
					end
				else if Pos( 'apos', code ) = 1  then  begin
					Delete( it, x, 4 );
					it[ x ] := '''';
					end
				else if Pos( 'quot', code ) = 1  then  begin
					Delete( it, x, 4 );
					it[ x ] := '"';
					end;
				end;
			Inc( x );
			end;
		end;  // do escapes

	if mScanEscapeASChars then  begin  // escape the Alphasoft data request characters
		x := 1;
		while x <= Length( it ) do  begin
			if it[ x ] = SeparatorList then  begin  // '\'
				Insert( '&', it, x );
				Inc( x );
				it[ x ] := 'b';
				end;
			{ else if it[ x ] = SeparatorIndex then  begin
				Insert( '&', it, x );
				Inc( x );
				it[ x ] := 'n';
				end;  }
			Inc( x );
			end;
		end;  // do escapes

	{if mScanTagType = ttTag then  begin       // separate attributes
		mScanAttributes := SplitOffAttributes( it );
		end; }
	end;


function   cXmlDB.Escape( const it : string ) : string;  // escapes tag content

    var
    	x : int;
        r : string;
	begin
    r := '';
    for x := 1 to Length( it ) do  begin
        case it[ x ] of
            '&'   : r := r + '&amp';
            '<'   : r := r + '&lt';
            '>'   : r := r + '&gt';
            '\'   : if mScanEscapeASChars then  r := r + '&bks' else  r := r + '\';
            //''''  : r := r + '&apos';
            //'"'   : r := r + '&quot';    // only escape in tag names
            else    r := r + it[ x ];
            end;
    	end;
    result := r;
    end;


procedure  cXmlDB.StreamScan;

	var
		it : string;
		c : char;
		attrib : boolean;
	begin
	it := '';   attrib := false;
	mScanAttributes := '';
	if mScanState = ssOutTag then  begin  //  find next <
		while true  do  begin  // to eof
			c := NextChar();
			if mEOF then  break;
			if c = '<' then  begin
				mScanState := ssInTag;
				break;
				end;
			end;
		end;

	if mScanState = ssInTag then  begin   // collect tag name
		while true  do  begin  // to eof
			c := NextChar();
			if mEOF then  break;
			if c = '>' then  begin
				mScanState := ssInContent;
				break;
				end
			else  begin
				if attrib then  mScanAttributes := mScanAttributes + c
                else  begin
					if c = ' ' then  begin
						if it <> '' then  attrib := true   // this fails to collect prolog but we don't care
						end
					else  it := it + c;
	                end;
            	end;
			end;

		case it[ 1 ] of                      // got name so resolve tag type
			'?' : begin
				mScanTagType := ttProlog;
				mScanState := ssOutTag;
				end;
			'!' : begin
				mScanTagType := ttComment;
				mScanState := ssOutTag;
				end;
			else  begin
				if it[ 1 ] = '/' then  begin   // end tag
					mScanTagType := ttTagEnd;
					Delete( it, 1, 1 );
					mScanState := ssOutTag;
					end
				else  begin                    // normal tag
					mScanEmptyTag := false;
					mScanTagType := ttTag;
					if mScanAttributes <> '' then  begin
						if mScanAttributes[ Length( mScanAttributes ) ] = '/' then  begin
							mScanEmptyTag := true;
							Delete( mScanAttributes, Length( mScanAttributes ), 1 );  // strip off trailing /
							mScanState := ssOutTag;
							end;
						end
					else if it[ Length( it ) ] = '/' then  begin
						mScanEmptyTag := true;
						Delete( it, Length( it ), 1 );  // strip off trailing /
						mScanState := ssOutTag;
						end;
					end;
				end;
			end;
		end

	else if mScanState = ssInContent then  begin        // collect content
		while true  do  begin  // to eof
			c := NextChar();
			if mEOF then  break;
			if c = '<' then  begin
				mScanState := ssInTag;
                mScanAttributes := '';
				break;
				end
			else it := it + c;
			end;
		mScanTagType := ttContent;
		end;

	mScanTagXMLRaw := it;
    if ( mScanTagType = ttContent ) or ( mScanTagType = ttProlog ) then  begin
		UnEscape( it );
        mScanTagXML := it;
    	end
    else  begin
		mScanTagXML := MakeTagNameLegal( it );
    	end;
	end;

//____________________________ loader __________________________________________

procedure   cXmlDB.BuildTag( parent : apTag );

	var
		pTag : apTag;
		n : string;
	begin
	pTag := NewTag( mScanTagXML, mScanAttributes, parent );
    if pTag <> nil then  begin
        repeat
            StreamScan;
            case mScanTagType of

                ttTag : begin
                    if mScanEmptyTag then  begin
                        // pt := NewTag( mScanTagXML, mScanAttributes, pTag );
						NewTag( mScanTagXML, mScanAttributes, pTag );
                        end
                    else  BuildTag( pTag );  // go in a level
                    end;

                ttTagEnd :  begin
                    if mScanTagXML <> pTag.TagName then  begin
                        n := pTag.TagName;
                        if mScanTagXML <> n then
                            LogEr( erUnmatchedXMLEndTag, 'Unmatched end tag ' + n + ' and ' + mScanTagXMLRaw );
                        end;
                    break;   // drop back a level
                    end;

                ttContent : pTag.Content := mScanTagXML;
                end;
            until mEOF or ( mEr > 0 );
	    end;
	end;


procedure  cXmlDB.Load;

	var
		base : string;
		pb : apTag;
	begin
	mScanState := ssOutTag;
	repeat
    	StreamScan;
		case mScanTagType of
			ttProlog : mProlog := '?xml version="1.0" encoding="UTF-8"?';  // mScanTagXML; not used anyway
			ttTag : begin
				if mRootTag.SubTags = nil then  mRootTag.SubTags := TList.Create;
				base := mScanTagXML;
				BuildTag( apTag( @mRootTag ) );
				end;
			end;
		until mEOF or ( mEr > 0 );
	if ( mEr > 0 ) and ( mRootTag.SubTags <> nil ) and ( base <> '' ) then  begin  // delete partial load
		pb := FindSubTag( nil, base );
		if pb <> nil then  begin
			DeleteAllFast( pb );
			end;
		end;
	end;


procedure cXmlDB.LoadFromFile_( fil : string );

	var
		f : file of AnsiChar;
		fb : file of byte;
		raw : RawByteString; // aUTF8;
		s : string;
		c0, c1 : AnsiChar;
		b0, b1 : byte;
		ch : char;

// 11101111 10111011 10111111    EF BB BF  utf-8 encoded BOM
//     1111   111011   111111
//  check byte order mark FEFF or '<' char at start of file
//  supports UTF-8 and UTF-16 big or little endian

	begin
	if ( fil <> '' ) and FileExists( fil ) then  begin      // todo D2009 proly does all this
		mFileName := fil;
		mTagsKeyRenamed := 0;
		try  begin
			AssignFile( f, fil );
			Reset( f );
			System.Read( f, c0 );
			System.Read( f, c1 );
			Closefile( f );
			if ( c0 = '<' ) and ( c1 >= ' ' ) or        // 8 bit ansi
				 ( c0 = AnsiChar( $EF )) and ( c1 = AnsiChar( $BB )) then  begin  // utf-8
				AssignFile( f, fil );
				Reset( f );
				while not EOF( f ) do  begin
					System.Read( f, c0 );
					raw := raw + c0;
					end;
				Closefile( f );
				s := UTF8ToString( raw );
				end
			else  if (( c0 = nul ) and ( c1 = '<' ) or
				( c0 = AnsiChar( $FE )) and ( c1 = AnsiChar( $FF ))) then  begin  // utf-16 big end
				AssignFile( fb, fil );
				Reset( fb );
				while not EOF( fb ) do  begin
					System.Read( fb, b0 );
					System.Read( fb, b1 );
					ch := Char( ( b0 shl 8 ) or b1 );
					s := s + ch;
					end;
				Closefile( fb );
				end
			else  if (( c0 = '<' ) and ( c1 = nul ) or
				( c0 = AnsiChar( $FF )) and ( c1 = AnsiChar( $FE ))) then  begin  // utf-16 little end
				AssignFile( fb, fil );
				Reset( fb );
				while not EOF( fb ) do  begin
					System.Read( fb, b0 );
					System.Read( fb, b1 );
					ch := Char( ( b1 shl 8 ) or b0 );
					s := s + ch;
					end;
				Closefile( fb );
				end
			end;
		finally        // todo handle the non-utf-8 and 2 flavours of utf-16 input
			end;
		// if ( b = AnsiChar( $EF )) and ( b1 = AnsiChar( $BB )) then begin  // utf-8
			//end
		//else  s := String( raw );   // leave as ansi
		LoadFromString( s );
		if mTagsKeyRenamed <> 0 then    // obsolete
			LogEr( erRenamedKeyTags, IntToStr( mTagsKeyRenamed ) + ' tags renamed for KEYING' );
		mTagsKeyRenamed := 0;
		end;
	end;


procedure cXmlDB.LoadFromFile( fil : string );

	begin
	LoadFromFile_( fil );
	if mEr > 0 then  begin
		LogEr( erRevertingToBackupFile, IntToStr( mTagsKeyRenamed ) + 'Reverting To Backup File ' + fil );
		mEr := 0;
		LoadFromFile_( BackUpFileName( fil ) );
		LogEr( erFailedToLoadBackupFile, IntToStr( mTagsKeyRenamed ) + 'Failed To Load Backup File ' + BackUpFileName( fil ) );
		end;
	end;


function   cXmlDB.LoadFromString( const source : string ) : boolean;

	begin
	Result := true;
	mStringX := 1;
    mScanState := ssOutTag;
	mStringBuffer := source;
	mEOF := false;
    mEr := 0;
	try  begin
		Load;
		end;
	except
		Result := false;
		LogEr( erLoadFSfailure, ' invalid XML input' );
		end;
	end;


//_________________________________ save _______________________________________


procedure  BackUpFile( fn : string );   // rename to file.~ext

	var
		ext, bu : string;
	begin
	if FileExists( fn ) then  begin
		ext := ExtractFileExt( fn );
		if ( ext <> '' ) and ( ext[ 1 ] = '.' ) then  begin
			Insert( '~', ext, 2 );
			bu := ChangeFileExt( fn, ext );
			end
		else  bu := fn + '.bak';
		if FileExists( bu )  then  DeleteFile( bu );
		RenameFile( fn, bu );
		end;
	end;


procedure  cXmlDB.SaveDBToFile( pt : apTag );

	var
		f : file of AnsiChar;
		raw : RawByteString;  // aUTF8;
		fn : string;
		s : string;
		n : int;
	begin
	fn := pt.TagName;
	if fn <> '' then  begin
		if Pos( '.', fn ) = 0 then  fn := fn + '.xml';
		BackUpFile( fn );
		s := BOM;
		try  begin
			if mProlog <> '' then  begin
				s := s + '<' + mProlog + '>' + EOL;
				end;
			s := s + FormatAllTags( pt, 0 );
			end;
		except
			ShowMessage( 'cXmlDB: ERROR could not save ' + fn );
			end;

		AssignFile( f, fn );
		try  begin
			raw := UTF8Encode( s );
			Rewrite( f );
			for n := 1 to Length( raw ) do  begin
				Write( f, raw[ n ] );
				end;
			end;
		finally
			Closefile( f );
			end;
		end;
	end;


procedure  cXmlDB.SaveDBToFiles( const match : string );     // todo escape AS chars

	var
		pt, pr : apTag;
		x : integer;
	begin             // assume that each tag off root is a separate 'table' ie file
	pr := @mRootTag;
	if pr.SubTags <> nil then  begin
		for x := 0 to pr.SubTags.Count - 1 do  begin
			pt := pr.SubTags[ x ];
			if ( match = '*' ) or ( match = pt.TagName )  then  begin
				SaveDBToFile( pt );
				end;
			end;
		end;
	end;


{   sample traffic

<DataReply>
	<Result> OK
		<ReqRead> \ </ReqRead>
		<Path> \Users\Alphasoft\ </Path>
		<ReqID> 2 </ReqID>
	</Result>
	<Users>
		<Alphasoft>
			<PassWord/>
			<Access/>
			<LoggedIn/>
        </Alphasoft>
    </Users>
</DataReply>
<DataRequest>
	<ReqRead>  \ </ReqRead>
	<Path>  \Users\Alphasoft\ </Path>
	<ReqID> 2 </ReqID>
</DataRequest>


<EditReply>
	<Result> OK
		<ReqEdit> \Hello again\Hello\ </ReqEdit>
		<Path> \Users\Alphasoft\PassWord\ </Path>
		<ReqID> 2 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqEdit>  \Hello again\Hello\ </ReqEdit>
	<Path>  \Users\Alphasoft\PassWord\ </Path>
	<ReqID> 2 </ReqID>
</EditRequest>

<EditReply>
	<Result> ERROR 105
		<ReqEdit> \5\\ </ReqEdit>
		<Path> \Departures\ST1234-20090114\Bay\ </Path>
		<ReqID> 2 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqEdit>  \5\\ </ReqEdit>
	<Path>  \Departures\ST1234-20090114\Bay\ </Path>
	<ReqID> 2 </ReqID>
</EditRequest>


<EditReply>
	<Result> OK
		<ReqDelete/>
		<Path> \DisplayConfig\Groups\Dep-Pic\ </Path>
		<ReqID> 13 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqDelete/>
	<Path> \DisplayConfig\Groups\Dep-Pic\ </Path>
	<ReqID> 13 </ReqID>
</EditRequest>

<EditReply>
	<Result> OK
		<ReqNewTag>
			\Dep-Pic\
			<Format> Departures,1,5 </Format>
			<Format> Departures,2,5 </Format>
			<Format> Departures,3,5 </Format>
			<Image> \Graphics\GIFs\Misc\ttopcl.gif,5 </Image>
		</ReqNewTag>
		<Path> \DisplayConfig\Groups\ </Path>
		<ReqID> 13 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqNewTag> \Dep-Pic\
		<Format> Departures,1,5</Format>
		<Format> Departures,2,5</Format>
		<Format> Departures,3,5</Format>
		<Image> \Graphics\GIFs\Misc\ttopcl.gif,5</Image>
	</ReqNewTag>
	<Path>\DisplayConfig\Groups\</Path>
	<ReqID> 13 </ReqID>
</EditRequest>

out <<  <LoginRequest>
	 <ReqID> 9901 </ReqID>
	 <UserName> Feed </UserName>
	 <PassWord> _DI_system_ </PassWord>
</LoginRequest>

out <<  <EditRequest>
	<ReqDelete/>
	 <Path> \Departures\ </Path>
	 <ReqID> 9901 </ReqID>
</EditRequest>

in >>  <LoginReply> Success   <Access> 9,* </Access> </LoginReply>


in >>  <EditReply>
	<Result> OK
		<ReqDelete/>
		<Path> \Departures\ </Path>
		<ReqID> 9901 </ReqID>
	</Result>
</EditReply>



}
end.

