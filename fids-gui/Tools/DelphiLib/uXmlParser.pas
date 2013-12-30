unit uXmlParser;
// (C) ALphasoft Pty Ltd

interface      // AllocMem/FreeMem    Main Thread only

uses
	Classes, uGlobalDefs, uDbTree, uGT;

//  define GLOBAL if using Global.Log etc

	// TagAttribute_Key = 'KeyTag="true"';    // SPECIAL XML ATTRIBUTES

type

	aTagType = ( ttStart, ttEnd, ttEmpty );

	aScanTagType = ( ttNone, ttProlog, ttTag, ttTagEnd, ttComment, ttContent );

	cXmlParser = class( cDbTree )
		private
			mFileName : string;
			mProlog : string;
			mScanState : ( ssOutTag, ssInTag, ssInContent );
			mScanEscapeChars : boolean;
			mScanLeaveWhiteSpace : boolean;
			mEscapeASChars : boolean;   // obs, default is false, change '|' to &b control
			mScanTagType   : aScanTagType;
			mScanEmptyTag  : boolean;
			mScanTagXMLRaw : string;
			mScanTagXML    : string;
			mScanAttributes : string;

			mStringX : int;      // string as stream stuff  todo fix stream i/f
			mStringBuffer : string;
			mLoadBase : apNode;
			mEOF : boolean;
			procedure  UnEscape( var it : string );
			procedure  StreamScan;
			function   NextChar() : char;

			procedure  Load;
			procedure  BuildTag( parent : apNode );
			procedure  LoadFromFile_( fil : string );
		public
            Directory : string;
            property   Error : int  read mEr;
			function   Escape( const it : string ) : string;  // escapes xml special chars from content
			function   LoadFromFile( fil : string ) : apNode;
			function   LoadFromString( const source : string ) : boolean;
			procedure  SaveDBToFile( pt : apNode; const FileName : string = '' );
			procedure  SaveDBToFiles( const match : string );
			function   FormatADataReply( edit : boolean; er : int ) : string;  // public because its called on a request
			function   FormatEndDataReply( edit : boolean ) : string;  // on request obj
			function   TrimReply() : boolean;
			function   FormatAllNodes( pNode : apNode; indent : integer ) : string;
		end;

	function	MakeTagNameLegal( const TagName : string; AllowNumeric : boolean = false ) : string;
	function    Escape( const it : string; EscapeASChars : boolean = false ) : string;  // escapes tag content
	function    MakeAList( const list : TStringList; sep : char = LSep ) : string;    // path style  |saf|sfd|
	function    BracketATag( n : string; kind : aTagType = ttStart; indent : int = 0 ) : string;  // no attributes
	function    FmtTerminalTag( const tag : string; const cont : string; indent : int = 0 ) : string;
	function    FmtEmptyTag( const tag : string; indent : int = 0 ) : string;
	function    FormatATag( pNode : apNode; indent : integer; first : boolean ) : string;
	function    FormatAllSubNodes( pNode : apNode; indent : integer ) : string;
	function    FormatAllNodes( pNode : apNode; indent : integer ) : string;
	function    TimeStamp : string;
	procedure   StartRequestNew( const name : string );
	procedure   AddToRequestNew( const add : string );
	function    EndRequestNew( const path, attrib, cont : string; id : string ) : string;
	function    FormatDelete( const path : string; id : string ) : string;
	//procedure   FormatDelete( const path : string; id : int );
	function	FormatEditRequest( path, new, old, id : string ) : string;
	function	FormatShortEditRequest( path, new, id : string ) : string;
	function	FormatRenameRequest( path, new, id : string ) : string;
//	function    FormatADataRequest( ed : boolean; const req : string;  const id : string;
//		 const options : TStringList; pathl : TStringList = nil ) : string;  overload;   // makes a request from a path list
	function    FormatADataRequest( ed : boolean; const req : string;  const id : string;
		 const name : string; path : string ) : string;  overload;   // makes a request from a path list
	function    StartPath( var depth : int; pt : apNode ) : string;
	function    EndPath( var depth : int; pt : apNode ) : string;
	function    ResultOK( const pr : apNode ) : boolean;
	function    FormatLogIn( const user, password, id : string ) : string;
	function    FormatLogOut( user, id : string ) : string;
	function    FormatLogInReply( const id, access : string; er : int ) : string;
    function    FullNodeName( pt : apNode ) : string;


implementation

uses
	uUtils, Windows, ASCII, Dialogs, SysUtils, StrUtils;

const
    CharNumericPrefix = 'ñ';  // 0xF1 to make numeric tag names leading alpha and therfore legal xml

var
	sXML : string;  // request building buffer


// _____________________________ utilities _____________________________________


function   PutDelimiters( const path : string ) : string;

	begin
	result := path;
	if result <> '' then  begin
		if result[ 1 ] <> LSep then   Insert( LSep, result, 1 );
		if result[ Length( result ) ] <> LSep then  result := result + LSep;
		end
	else  result := LSep;
	end;


function   FmtEmptyTag( const tag : string; indent : int = 0 ) : string;

	var
		i : int;
	begin
	result := '';
    if tag <> '' then  begin
        for i := 1 to indent do  Result := Result + TAB;
        Result := Result + '<' + tag + '/>';
    	end;
	end;


function   FmtTerminalTag( const tag : string; const cont : string; indent : int = 0 ) : string;

	var
		i : int;
	begin
    result := '';
	if cont <> '' then  begin
        if tag <> '' then  begin
            for i := 1 to indent do  result := result + TAB;
            result := result + '<' + tag + '> ' + cont + ' </' + tag + '>';
        	end;
        end
	else  result := FmtEmptyTag( tag, indent );
	end;


function   BracketATag( n : string; kind : aTagType = ttStart; indent : int = 0 ) : string;

	var
		i : int;
	begin
	Result := '';
    if n <> '' then  begin
        for i := 1 to indent do  Result := Result + TAB;
        if kind = ttEnd then	Result := Result + '</'
        else					Result := Result + '<';
        Result := Result + n;
        if kind = ttEmpty then	Result := Result + '/>'
        else					Result := Result + '>';
    	end
    {$ifdef DEBUG }else  ShowMessage( 'BracketATag: Empty tag name' );  {$endif }
	end;


function    FullNodeName( pt : apNode ) : string;

	begin
	result := pt.NodeName;
	if pt.Table <> nil then
		result := result + ' ' + Attrib_Key;  // not editable
	if pt.AutoInc > 0 then
		result := result + ' ' + Attrib_AutoInc + IntToStr( pt.AutoInc ) +'"';  // not editable
    end;


function  TimeStamp : string;

	begin
    result := '<TimeStamp> ' + TimeDateToStr( Now(), 'yyyymmdd HHmmss' ) + ' </TimeStamp>' + EOL;
    end;


procedure  StartRequestNew( const name : string );

	var
		tagName : string;
	begin
	tagName := MakeTagNameLegal( name, true );
	sXML := BracketATag( TagEditRequest, ttStart ) + EOL;
	sXML := sXML + BracketATag( TagNewTag, ttStart, 1 ) + ' ' + tagName + EOL;
	end;


procedure  AddToRequestNew( const add : string );  // build up any sub tags to add

	begin
	sXML := sXML + add;
	end;


function  EndRequestNew( const path, attrib, cont : string; id : string ) : string;
	begin
	sXML := sXML + BracketATag( TagNewTag, ttEnd, 1 ) + EOL;
	if attrib <> '' then  sXML := sXML + FmtTerminalTag( 'Attributes', attrib, 1 ) + EOL;
    if cont <> '' then  sXML := sXML + FmtTerminalTag( 'Content', Escape( cont ), 1 ) + EOL;
 	sXML := sXML + FmtTerminalTag( TagPath, PutDelimiters( path ), 1 ) + EOL;
	sXML := sXML + FmtTerminalTag( TagReqID, id, 1 ) + EOL;
	sXML := sXML + BracketATag( TagEditRequest, ttEnd );
	result := sXML;     // and return the whole accummulation
	end;
{	eg
    StartRequestNew( pFlt.NodeName, '', '' );
    AddToRequestNew( FormatAllSubNodes( pFlt, 2 ) );
    Hub.Broadcast( EndRequestNew( pArrDep.NodeName, FeedID ) );

<EditRequest>
	<ReqNewTag> GT4444-
    	<Flights>
			<GT4444/>
		</Flights>
		<STD> 20100121 170000 </STD>
		<ETD> 20100121 170000 </ETD>
		<Ports> PER </Ports>
		<Gates> 13 </Gates>
		<Bay/>
	</ReqNewTag>
	<Path> |Departures| </Path>
	<ReqID> Feed </ReqID>
</EditRequest>
}


function   FormatDelete( const path : string; id : string ) : string;

	begin
    if ( path <> '' ) and ( id <> '' ) then  begin
        result := BracketATag( TagEditRequest, ttStart ) + EOL +
                FmtEmptyTag( TagDelete, 1 ) + EOL +
                FmtTerminalTag( TagPath, PutDelimiters( path ), 1 ) + EOL +
                FmtTerminalTag( TagReqID, id, 1 ) + EOL +
                BracketATag( TagEditRequest, ttEnd );
    	end
    else result := '';
	end;


procedure   AdjustPath( var path : string );

	begin
	if path[ 1 ] <> LSep then  Insert( LSep, path, 1 );
	if path[ Length( path ) ] <> LSep then  path := path + LSep;
	end;


function	FormatEditRequest( path, new, old, id : string ) : string;

	var
		r : string;
	begin
	if path <> '' then  begin
		AdjustPath( path );
		r := BracketATag( TagEditRequest, ttStart ) + EOL;
		r := r + FmtTerminalTag( TagEdit, Escape( new ), 1 ) + EOL;
        r := r + FmtTerminalTag( 'Prev', Escape( old ), 1 ) + EOL;
		r := r + FmtTerminalTag( TagPath, path, 1 ) + EOL;
		r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;
		r := r + BracketATag( TagEditRequest, ttEnd ) + EOL;
		end;
	result := r;
	end;


function	FormatShortEditRequest( path, new, id : string ) : string;

	var
		r : string;
	begin
	if path <> '' then  begin
		AdjustPath( path );
		r := BracketATag( TagEditRequest, ttStart ) + EOL;
		r := r + FmtTerminalTag( TagEdit, Escape( new ), 1 ) + EOL;
		r := r + FmtTerminalTag( TagPath, path, 1 ) + EOL;
		r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;
		r := r + BracketATag( TagEditRequest, ttEnd ) + EOL;
		end;
	result := r;
	end;


function	FormatRenameRequest( path, new, id : string ) : string;

	var
		r : string;
	begin
	if path <> '' then  begin
		AdjustPath( path );
		r := BracketATag( TagEditRequest, ttStart ) + EOL;
		r := r + FmtTerminalTag( TagRenameTag, MakeTagNameLegal( new ), 1 ) + EOL;
		r := r + FmtTerminalTag( TagPath, path, 1 ) + EOL;
		r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;
		r := r + BracketATag( TagEditRequest, ttEnd ) + EOL;
		end;
	result := r;
	end;


function   MakeAList( const list : TStringList; sep : char = LSep ) : string;  // path style  |saf|sfd|

	var
		x : integer;
	begin
	if list <> nil then  begin
		for x := 0 to list.Count - 1 do  begin
			result := result + sep + list[ x ];
			end;
		result := result + sep;
		end
	else  result := sep;
	end;


procedure  FormatAList( const list : TStringList; const tagname : string; indent : integer; var r : string );

	var
		x : integer;
	begin
	if list <> nil then  begin
		for x := 1 to indent do r := r + TAB;
		r := r + BracketATag( tagname ) + '  ';
		r := r + MakeAList( list, LSep );
		r := r + ' ' + BracketATag( tagname, ttEnd ) + EOL;
		end;
	end;


function	MakeTagNameLegal( const TagName : string; AllowNumeric : boolean = false ) : string;

    var                        // NOTE official xml requires leading char is alpha
    	x : int;
        r, n : string;
        c : char;
    begin
    result := '';
    n := Trim( TagName );
    if Length( n ) >= 1 then  begin
        if AllowNumeric then  begin
            if ( Length( n ) >= 2 ) and ( n[ 1 ] = CharNumericPrefix ) and
               ( IsDecimal( n[ 2 ] ) ) then  begin
                Delete( n, 1, 1 );
            	end;
        	end
        else  if IsDecimal( n[ 1 ] ) then  r := CharNumericPrefix;    // insert CharNumericPrefix

        for x := 1 to Length( n ) do  begin
            c := n[ x ];
            case c of
                'a'..'z' : r := r + c;
                'A'..'Z' : r := r + c;
                '0'..'9' : r := r + c;
                '-','_',':','.' : r := r + c;
                else  if ( Length( r ) > 0 ) and ( r[ Length( r ) ] <> '_' ) then  r := r + '_';   // replace space etc with _
                end;
            end;
    	result := r;
	    end;
	end;


function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
	 const name : string; path : string ) : string;  // makes a request from a path string

	var
		r : string;
	begin
	if ed then  r := BracketATag( TagEditRequest )
	else        r := BracketATag( TagDataRequest );
	r := r + EOL;
	r := r + FmtTerminalTag( req, name, 1 ) + EOL;
	r := r + FmtTerminalTag( TagPath, path, 1 ) + EOL;
	r := r + FmtTerminalTag( TagReqID, id, 1 ) + EOL;

	if ed then  r := r + BracketATag( TagEditRequest, ttEnd )
	else        r := r + BracketATag( TagDataRequest, ttEnd );
	Result := r;
	end;


{function   FormatADataRequest( ed : boolean; const req : string;  const id : string;
	 const options : TStringList; pathl : TStringList = nil ) : string;  // makes a request from a path list

	begin
	result := FormatADataRequest( ed, req, id, MakeAList( options ), MakeAList( pathl ) );
	end; }


function  Tabs( depth : int ) : string;

	var
		t : int;
	begin
	result := '';
	for t := 1 to depth do  result := result + TAB;
	end;


function   StartPath( var depth : int; pt : apNode ) : string;

	var
		pl : TStringList;
		t : int;
	begin
	result := '';
	if pt <> nil then  begin
		pl := TStringList.Create;
		while pt.Back <> nil do  begin
			pl.Insert( 0, pt.NodeName );
			pt := pt.Back
			end;
		for t := 0 to pl.Count - 1 do  begin
			result := result + BracketATag( pl[ t ], ttStart, depth + t ) + EOL;
			end;
		depth := depth + pl.Count;   // return base depth
		pl.Free;
		end;
	end;


function   EndPath( var depth : int; pt : apNode ) : string;

	begin
	result := '';
	if pt <> nil then  begin
		while pt.Back <> nil do  begin
			result := result + BracketATag( pt.NodeName, ttEnd, depth ) + EOL;
			pt := pt.Back;
			if depth > 0 then  Dec( depth );
			end;
		end;
	end;


function   ParseList( const path : string ) : TStringList;

	var                  //  |Head|Subhead|  style list
		x : integer;
		tg : string;
	begin
	Result := TStringList.Create;
	x := 1;
	while x <= Length( path ) do  begin  // skip initial whitespace and |
		if path[ x ] = LSep then  begin
			Inc( x );
			break;
			end;
		Inc( x );
		end;

	while x <= Length( path ) do  begin   // build list
		if path[ x ] = LSep then  begin
			Result.Add( tg );
			tg := '';
			end
		else tg := tg + path[ x ];
		Inc( x );
		end;
	if tg <> '' then  Result.Add( tg );
	end;




//___________________________ cXmlParser ___________________________________________

{constructor cXmlParser.Create( treeID : word = 0; isKey : boolean = true; log : aLogProc = nil );

	begin
	inherited  Create( treeID, isKey, log );
	end; }


function   ResultOK( const pr : apNode ) : boolean;

	var
		pok : apNode;
	begin
	result := false;
	if ( pr <> nil ) and ( pr.SubNodes <> nil ) and ( pr.SubNodes.Count > 0 ) then  begin
		pok := pr.SubNodes[ 0 ];
		pok := FindName( pok, 'Result' );
		if ( pok <> nil ) and ( pok.Content = 'OK' ) then  result := true;
		end;
	end;


function   FormatLogIn( const user, password, id : string ) : string;

	var
		r : string;
	begin
	r := BracketATag( TagLoginRequest, ttStart ) + EOL +
		FmtTerminalTag( TagReqID, id, 1 ) + EOL +
		FmtTerminalTag( TagUserName, user, 1 ) + EOL +
		FmtTerminalTag( TagPassWord, password, 1 ) + EOL +
		BracketATag( TagLoginRequest, ttEnd );
	result := r;
	end;


function   FormatLogInReply( const id, access : string; er : int ) : string;

	var
		ok : string;
	begin
	if er = erNone then  ok := 'OK'  else  ok := 'ERROR ' + IntToStr( er );
	result := BracketATag( TagLoginReply, ttStart ) + EOL +
		FmtTerminalTag( TagResult, ok, 1 ) + EOL +
		FmtTerminalTag( TagAcess, access, 1 ) + EOL +
		FmtTerminalTag( TagReqID, id, 1 ) + EOL +
		BracketATag( TagLoginReply, ttEnd );
	end;


function   FormatLogOut( user, id : string ) : string;

	begin
	result := BracketATag( TagLoginRequest, ttStart ) + EOL +
		FmtTerminalTag( TagUserName, user, 1 ) + EOL +
		FmtTerminalTag( TagReqID, id, 1 ) + EOL +
		FmtTerminalTag( TagLogout, '', 1 ) + EOL +
		BracketATag( TagLoginRequest, ttEnd );
	end;


function   FormatTagName( pt : apNode ) : string;

	begin
    result := MakeTagNameLegal( pt.NodeName );
    if pt.Table <> nil then  result := result + ' ' + Attrib_Key;
    if pt.AutoInc > 0 then   result := result + ' ' + Attrib_AutoInc + IntToStr( pt.AutoInc ) + '"';
    end;


function   FormatATag( pNode : apNode; indent : integer; first : boolean ) : string;

	const                            // makes XML r from a single tag - call twice - first and not first
		MaxSingleLineContent = 160;   // was 40
	var
		r : string;
		i, c : integer;
		t, cont : string;
	begin
	r := '';
	if pNode.SubNodes = nil then  c := 0
	else                          c := pNode.SubNodes.Count;
	if first then  begin
		for i := 1 to indent do  r := r + TAB;
        t := FormatTagName( pNode );
		//t := pNode.NodeName;
		cont := Escape( pNode.Content );
		if ( c = 0 ) and ( cont = '' ) then  begin  // empty tag
			r := r + '<' + t + '/>' + EOL;
			end
		else  begin
			r := r + BracketATag( t );
			if ( c = 0 ) and ( Length( cont ) <= MaxSingleLineContent ) then  begin      // pack onto one line
				r := r + ' ' + cont + ' ' + BracketATag( pNode.NodeName, ttEnd ) + EOL;    // tag .... /tag case
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
		if ( c <> 0 ) or ( Length( pNode.Content ) > MaxSingleLineContent ) then  begin
			for i := 1 to indent do  r := r + TAB;
			r := r + BracketATag( MakeTagNameLegal( pNode.NodeName ), ttEnd ) + EOL;
			end;
		end;
	Result := r;
	end;


function  FormatAllSubNodes( pNode : apNode; indent : integer ) : string;

 	var                     // multi level XML maker starting with pNode subs
		pt : apNode;
		x  : integer;
	begin
	result := '';
	if pNode <> nil then  begin            // don't show _root_
        x := -1;
        while EachSubNode( pNode, x, pt ) do  begin
            result := result + FormatAllNodes( pt, indent );
            end;
        end;
    end;


function  FormatAllNodes( pNode : apNode; indent : integer ) : string;

	                     // multi level XML maker starting with pNode
	begin
	result := '';
	if pNode <> nil then  begin            // don't show _root_
        if pNode.Back <> nil then  Result := FormatATag( pNode, indent, true ) // todo optimise terminals
        else if indent > 0 then  Dec( indent );
        result := result + FormatAllSubNodes( pNode, indent + 1 );
        if pNode.Back <> nil then  Result := Result + FormatATag( pNode, indent, false );
	    end;
	end;



function  cXmlParser.FormatADataReply( edit : boolean; er : int ) : string;  // on request obj

	var                    // its here because its based on the request ie a cXmlParser
		r : string;
		pr, pt : apNode;
	begin
	if edit then  r := BracketATag( TagEditReply )
	else          r := BracketATag( TagDataReply );
	r := r + EOL + BracketATag( TagResult, ttStart, 1 );
	if er <> 0 then  	r := r + ' ERROR ' + IntToStr( er )
	else         		r := r + ' OK';
	r := r + EOL;
	pr := GetRoot();                    // echo request
	if ( pr.SubNodes <> nil ) and ( pr.SubNodes.Count >= 1 ) then  begin
		pr := pr.SubNodes[ 0 ];
		if ( pr <> nil ) and ( pr.SubNodes <> nil ) then  begin
        	if edit then  begin
                r := r + FormatAllSubNodes( pr, 2 );
            	end
            else  begin
                pt := pr.SubNodes[ 0 ];

                r := r + FormatAllNodes( pt, 2 );
                pt := FindName( pr, 'Path' );
                if pt <> nil then  begin
                    r := r + FormatAllNodes( pt, 2 );
                    end;
                pt := FindName( pr, 'ReqID' );
                if pt <> nil then  begin
                    r := r + FormatAllNodes( pt, 2 );
                    end;
            	end;
			end;
		end;
	r := r + BracketATag( TagResult, ttEnd, 1 ) + EOL;
	result := r;
	end;


function  cXmlParser.FormatEndDataReply( edit : boolean ) : string;

	begin
	if edit then  result := BracketATag( TagEditReply, ttEnd )
	else          result := BracketATag( TagDataReply, ttEnd );
	end;


function   cXmlParser.TrimReply() : boolean;

	var
		pt0, pt1, pt2 : apNode;
		x : int;
	begin       // removes the reply envelope to match the original tree.
				// remove 2 base tags and remove a layer of depth
				// from tree typically to remove request tags.
	Result := false;
	pt0 := @ mRootNode;
	if pt0.SubNodes <> nil then  if pt0.SubNodes.Count = 1 then  begin
		pt1 := pt0.SubNodes[ 0 ];
		if pt1.NodeName = TagDataReply then  begin
			if pt1.SubNodes <> nil then  begin
				pt2 := FindName( pt1, TagReqID );    // strip off <ReqID>....
				DisposeNode( pt2 );
				pt2 := FindName( pt1, TagScan );    // strip off <ReqScan>....
				DisposeNode( pt2 );
				if pt1.SubNodes <> nil then  begin
					// link out pt1 tag ie <DataReply>
					for x := 0 to pt1.SubNodes.Count - 1 do  begin
						pt2 := pt1.SubNodes[ x ];
						pt2.Back := pt0;        // fix all the back links
						end;
					pt0.SubNodes.Free;         // replace the root subtag list
					pt0.SubNodes := pt1.SubNodes;
					pt1.SubNodes := nil;
					// if pt1.Table <> nil then  BuildTable( pt0 );   // <DataReply> never has keys
					DisposeNodeFast( pt1 );
					Result := true;
					end;
				end;
			end;
		end;
	end;


function  cXmlParser.FormatAllNodes( pNode : apNode; indent : integer ) : string;

	{var                     // multi level XML maker starting with pNode
		pt : apNode;
		x  : integer; }
	begin
	result := '';
	if pNode = nil then  pNode := GetRoot;
    result := uXmlParser.FormatAllNodes( pNode, indent );

	{if pNode <> GetRoot then  Result := FormatATag( pNode, indent, true )
	else Dec( indent );
	if pNode.SubNodes <> nil  then  begin
		for x := 0 to pNode.SubNodes.Count - 1 do  begin
			pt := pNode.SubNodes[ x ];
			Result := Result + FormatAllNodes( pt, indent + 1 );
			end;
		end;
	if pNode <> GetRoot then  Result := Result + FormatATag( pNode, indent, false ); }
	end;


//______________________________________ parser ______________________________________


function   cXmlParser.NextChar() : char;

	begin
	Result := nul;
	if mStringX > Length( mStringBuffer ) then
		mEOF := true
	else  Result := mStringBuffer[ mStringX ];
	Inc( mStringX );
	end;


procedure cXmlParser.UnEscape( var it : string );

//       | -> &.
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
				code := Copy( it, x + 1, 4 );// Slice( it, x + 1, 4 );
				if Pos( 'lt', code ) = 1  then  begin
					Delete( it, x, 2 );
					it[ x ] := '<';
					end
				else if Pos( 'gt', code ) = 1  then  begin
					Delete( it, x, 2 );
					it[ x ] := '>';
					end
				else if Pos( 'b', code ) = 1  then  begin
					Delete( it, x, 1 );
					it[ x ] := LSep;   // '|'
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

	if mEscapeASChars then  begin  // unescape the Alphasoft data request characters
		x := 1;
		while x <= Length( it ) do  begin
			if it[ x ] = LSep then  begin  // '|'
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


function   Escape( const it : string; EscapeASChars : boolean = false ) : string;  // escapes xml special chars from content

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
			LSep  : if EscapeASChars then  r := r + '&b' else  r := r + it[x];  // '|'
            //''''  : r := r + '&apos';
            //'"'   : r := r + '&quot';    // only need to escape in tag names
            else    r := r + it[ x ];
            end;
    	end;
    result := r;
    end;


function   cXmlParser.Escape( const it : string ) : string;  // escapes xml special chars from content

	begin
    result := uXmlParser.Escape( it, mEscapeASChars );
    end;


procedure  cXmlParser.StreamScan;

	var
		it : string;
		c : char;
		attrib : boolean;
	begin
	it := '';   attrib := false;
	mScanAttributes := '';
	if mScanState = ssOutTag then  begin  //  find next <
        mScanTagType := ttNone;
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
		mScanTagXML := MakeTagNameLegal( it, true );   // allow internal numeric tag names - ie strip numeric prefix char
    	end;
	end;

//____________________________ loader __________________________________________


procedure   cXmlParser.BuildTag( parent : apNode );

	var
		pNode : apNode;
		n : string;
	begin
	//pNode := NewNode( mScanTagXML, Pos( Attrib_Key, mScanAttributes ) > 0, parent );
    pNode := NewNode( mScanTagXML, mScanAttributes, parent );
    if pNode <> nil then  begin
    	if mLoadBase = nil then  mLoadBase := pNode;
        repeat
            StreamScan;
            case mScanTagType of

                ttTag : begin
                    if mScanEmptyTag then  begin
                    	NewNode( mScanTagXML, mScanAttributes, pNode );
                        // pt := NewTag( mScanTagXML, mScanAttributes, pNode );
                        {p := Pos( Attrib_AutoInc, mScanAttributes );    auto := 0;
                        if p > 0 then  begin
                            Inc( p );
                            auto := GetInt( mScanAttributes, p );
                        	end;
						NewNode( mScanTagXML, Pos( Attrib_Key, mScanAttributes ) > 0, pNode, auto );}
                        end
                    else  BuildTag( pNode );  // go in a level
                    end;

                ttTagEnd :  begin
                    if mScanTagXML <> pNode.NodeName then  begin
                        n := pNode.NodeName;
                        if mScanTagXML <> n then
                            LogEr( erUnmatchedXMLEndTag, 'Unmatched end tag ' + n + ' and ' + mScanTagXMLRaw );
                        end;
                    break;   // drop back a level
                    end;

				ttContent : pNode.Content := mScanTagXML;
                end;
            until mEOF or ( mEr > erWarning );
	    end;
	end;


procedure  cXmlParser.Load;

	var
		base : string;
	begin
	mScanState := ssOutTag;
    mLoadBase := nil;   mScanEscapeChars := true;
    try  begin
        repeat
            StreamScan;
            case mScanTagType of
                ttProlog : mProlog := '?xml version="1.0" encoding="UTF-8"?';
                ttTag : begin
                    // if GetRoot.SubNodes = nil then  GetRoot.SubNodes := TList.Create;
                    base := mScanTagXML;
                    BuildTag( GetRoot );
                    end;
                end;
            until mEOF or ( mEr > erWarning );
        if ( mEr > erWarning ) and ( mLoadBase <> nil ) then  begin  // delete partial load
            DeleteAllFast( mLoadBase );
            end;
	    end;
    finally
        mScanEscapeChars := false;
    	end;
	end;


procedure cXmlParser.LoadFromFile_( fil : string );

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
    if not FileExists( fil ) then  fil := Directory + fil;
	if FileExists( fil ) then  begin      // todo D2009 proly does all this   ( fil <> '' ) and
		mFileName := fil;
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
		end
    else  LogEr( erLoadFSfailure, fil + ' file not found.' );
	end;


function  cXmlParser.LoadFromFile( fil : string ) : apNode;

	begin
	LoadFromFile_( fil );
	if mEr > 0 then  begin
		LogEr( erRevertingToBackupFile, 'Reverting To Backup File ' + fil );
		mEr := 0;
		LoadFromFile_( BackUpFileName( fil ) );
		if mEr <> 0 then  LogEr( erFailedToLoadBackupFile, 'Failed To Load Backup File ' + BackUpFileName( fil ) );
		end;
    result := mLoadBase;
	end;


function   cXmlParser.LoadFromString( const source : string ) : boolean;

	begin
	mStringX := 1;
    mScanState := ssOutTag;
	mStringBuffer := source;
	mEOF := false;
    mEr := 0;
	try  begin
		Load;
		end;
	except
		LogEr( erLoadFSfailure, ' invalid XML input in ' + Slice( source, 1, 50  ) );
		end;
	Result := mEr = 0;
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


procedure  cXmlParser.SaveDBToFile( pt : apNode; const FileName : string = '' );

	var
		f : file of AnsiChar;
		raw : RawByteString;  // aUTF8;
		fn : string;
		s : string;
		n : int;
	begin
    if pt = nil then  begin
        pt := GetRoot;
        if ( pt.SubNodes <> nil ) and ( pt.SubNodes.Count > 0 ) then  pt := pt.SubNodes[ 0 ]
        else	pt := nil;
    	end;
    if pt <> nil then  begin
        if FileName <> '' then fn := Directory + FileName  else  fn := pt.NodeName;
        fn := Directory + fn;
        if fn <> '' then  begin
            if Pos( '.', fn ) = 0 then  fn := fn + '.xml';
            BackUpFile( fn );
            s := BOM;
            try  begin
                if mProlog <> '' then  begin
                    s := s + '<' + mProlog + '>' + EOL;
                    end;
                s := s + FormatAllNodes( pt, 0 );
                end;
            except
                ShowMessage( 'cXmlParser: ERROR could not save ' + fn );
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
	end;


procedure  cXmlParser.SaveDBToFiles( const match : string );     // todo escape AS chars

	var
		pt, pr : apNode;
		x : integer;
	begin             // assume that each tag off root is a separate 'table' ie file
	pr := GetRoot;
	if pr.SubNodes <> nil then  begin
		for x := 0 to pr.SubNodes.Count - 1 do  begin
			pt := pr.SubNodes[ x ];
			if ( match = '*' ) or ( match = pt.NodeName )  then  begin
				SaveDBToFile( pt );
				end;
			end;
		end;
	end;


{   sample traffic

<DataReply>
	<Result> OK
		<ReqRead> | </ReqRead>
		<Path> |Users|Alphasoft| </Path>
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
	<ReqRead>  | </ReqRead>
	<Path>  |Users|Alphasoft| </Path>
	<ReqID> 2 </ReqID>
</DataRequest>


<EditReply>
	<Result> OK
		<ReqEdit> |Hello again|Hello| </ReqEdit>
		<Path> |Users|Alphasoft|PassWord| </Path>
		<ReqID> 2 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqEdit>  |Hello again|Hello| </ReqEdit>
	<Path>  |Users|Alphasoft|PassWord| </Path>
	<ReqID> 2 </ReqID>
</EditRequest>

<EditReply>
	<Result> ERROR 105
		<ReqEdit> |5|| </ReqEdit>
		<Path> |Departures|ST1234-20090114|Bay| </Path>
		<ReqID> 2 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqEdit>  |5|| </ReqEdit>
	<Path>  |Departures|ST1234-20090114|Bay| </Path>
	<ReqID> 2 </ReqID>
</EditRequest>


<EditReply>
	<Result> OK
		<ReqDelete/>
		<Path> |DisplayConfig|Groups|Dep-Pic| </Path>
		<ReqID> 13 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqDelete/>
	<Path> |DisplayConfig|Groups|Dep-Pic| </Path>
	<ReqID> 13 </ReqID>
</EditRequest>

<EditReply>
	<Result> OK
		<ReqNewTag>
			|Dep-Pic|
			<Format> Departures,1,5 </Format>
			<Format> Departures,2,5 </Format>
			<Format> Departures,3,5 </Format>
			<Image> |Graphics|GIFs|Misc|ttopcl.gif,5 </Image>
		</ReqNewTag>
		<Path> |DisplayConfig|Groups| </Path>
		<ReqID> 13 </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqNewTag> |Dep-Pic|
		<Format> Departures,1,5</Format>
		<Format> Departures,2,5</Format>
		<Format> Departures,3,5</Format>
		<Image> |Graphics|GIFs|Misc|ttopcl.gif,5</Image>
	</ReqNewTag>
	<Path>|DisplayConfig|Groups|</Path>
	<ReqID> 13 </ReqID>
</EditRequest>

out <<  <LoginRequest>
	 <ReqID> 9901 </ReqID>
	 <UserName> Feed </UserName>
	 <PassWord> _DI_system_ </PassWord>
</LoginRequest>

out <<  <EditRequest>
	<ReqDelete/>
	 <Path> |Departures| </Path>
	 <ReqID> 9901 </ReqID>
</EditRequest>

in >>  <LoginReply> Success   <Access> 9,* </Access> </LoginReply>


in >>  <EditReply>
	<Result> OK
		<ReqDelete/>
		<Path> |Departures| </Path>
		<ReqID> 9901 </ReqID>
	</Result>
</EditReply>


<LoginReply>
	<Result> OK </Result>
	<Access> 9,* </Access>
	<ReqID> HTTP_Server </ReqID>
</LoginReply>
<LoginRequest>
	<ReqID> HTTP_Server </ReqID>
	<UserName> HTTP_Server </UserName>
	<PassWord> _DI_system_ </PassWord>
</LoginRequest>
<EditReply>
	<Result> OK
		<ReqNewTag> |Ports|| TEL| </ReqNewTag>
		<Path> |Departures|QF123-22|Flights|QF123| </Path>
		<ReqID> HTTP_Server </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqNewTag> |Ports|| TEL|
	</ReqNewTag>
	<Path> |Departures|QF123-22|Flights|QF123| </Path>
	<ReqID> HTTP_Server </ReqID>
</EditRequest>
<EditReply>
	<Result> OK
		<ReqEdit> | Boarding|| </ReqEdit>
		<Path> |Departures|QF123-22|DStatus| </Path>
		<ReqID> HTTP_Server </ReqID>
	</Result>
</EditReply>
<EditRequest>
	<ReqEdit> | Boarding|| </ReqEdit>
	<Path> |Departures|QF123-22|DStatus| </Path>
	<ReqID> HTTP_Server </ReqID>
</EditRequest>
}
end.

