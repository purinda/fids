unit uUtils;

interface

uses
	uGT, Classes, Graphics, Controls, TypInfo;

const
	ListSep = ',';   // formatter content list separator
	StringDelim = '"';

function   Min( a, b : int ) : int;
function   Max( a, b : int ) : int;
procedure  LeaveOnly( leave : string; var s : string );
procedure  SkipSpace( const s : string; var index : integer );
function   WithoutSpaces( const s : string ) : string;
function   GetString( const s : string; var index : integer ) : string;
function   Slice( const src : string; index : integer; count : integer ) : string;
procedure  WindowsPath( var s : string );
function   PosN( const substr, src : string; limit : integer ) : int;
function   IsAlpha( c : char ) : boolean;
function   IsDecimal( c : char ) : boolean;  overload;
function   IsLowerCase( c : char ) : boolean;
function	IsAlphaNumeric( c : char ) : boolean;
function   Gap( const s : string ) : string;
function   GetAlphas( const s : string; var index : integer ) : string;
function   GetHex( const s : string; var index  : integer ) : int;  //  F00BAA or , $F00BAA
function   GetInt( const s : string; var index : integer ) : integer;
function   Decimal( const s : string; var index : cardinal ) : cardinal;  overload;
function   Decimal( const s : string; var index : int ) : int;  overload;

function   InList( const item, list : string ) : boolean;
function   ListIndex( const item, list : string ) : integer;
function   ListItem( const list : TStringList; index : int ) : string;    overload;
function   ListItem( const list : string; index : int ) : string;         overload;
function   BuildParamsL( const params : string; var index : int; sep : char = ListSep ) : TStringList;
function   ListToStr( const list : TStringList ) : string;
function	ListsEqual( const listA, listB : TStrings ) : boolean;
function   IncludeInList( const item, list : string ) : string;
function   ExcludeFromList( const item, list : string ) : string;
function   StrToList( const s : string ) : TStringList;
function   GetLine( const s : string; var index : integer ) : string;
function   LineCount( const s : string ) : int;
function   NoGapLower( s : string ) : string;
function   UnCamel( const n : string; gapChar : char = ' ' ) : string;
function   EnumToStr( val : Integer; pEnumTypePtr : PTypeInfo; unCaml : boolean = true ) : string;  // eg TypeInfo( aSortPref )
function   EnumToSL( pEnumTypePtr : PTypeInfo ) : TStringList;
function   FindEnum( nam : string; pEnumTypePtr : PTypeInfo ) : int;
procedure  InitListControl( lc : TCustomListControl; pEnumTypePtr : PTypeInfo  );
function   Trim( const s : string ) : string;
procedure  TrimThis( var s : string );

procedure  CopyStrList( const src : TStringList; dest : TStringList );
function   ListFiles( const path, ext : string ) : TStringList;
function   FileOpenRead( name : string ) : THandle;
function   FileOpenWrite( name : string ) : THandle;
function   WinError( var er : int ) : string;  overload;
function   WinError() : string;  overload;

function   Minutes() : int;
function   DTToStr (dt: TDateTime): string;
function   ShortDateTime( dt : TDateTime ) : string;
function   TimeDateToStr( t : TDateTime; fmt : string = '' ) : string;  // 'hh:nn ddd d mmm yy'

function   ParseIPs( const ips : string ) : TList; // of int
function   IPtoStr( ipn : int ) : string;
procedure	SetEmptyPCharLength( var s : string; len : card );    // see ComputerName below
procedure	PCharToStr( var s : string; len : card );  // see ComputerName below
function	ComputerName : string;
function	GetIPFromHost( var HostName, IPaddr, WSAErr: string ): Boolean;

function   BackUpFileName( fn : string ) : string;   // .~ext
function   FileToStr( const fn : string ) : string;
function   SetPowerOf2( n : int ) : int;
function   NewZ( size : integer ) : pointer;    // get a zero filled mem block

function	NewestFile( directory, match : string; numbered : boolean = false  ) : string;
function	OldestFile( directory, match : string; numbered : boolean = false ) : string;
function	CountFiles( directory, match : string; numbered : boolean = false ) : int;
function	AnyDecimal( str : string ) : int;
function	ExtractOnlyFileName( fn : string ) : string;



implementation


uses
	Windows, SysUtils, Character, Winsock, ASCII;


function   Min( a, b : int ) : int;

	begin
    if a > b then result := b
    else  result := a;
    end;


function   Max( a, b : int ) : int;

	begin
    if a > b then result := a
    else  result := b;
    end;


function   NewZ( size : integer ) : pointer;    // get a zero filled mem block

	begin                        // use FreeMem to dispose of AllocMem
	Result := AllocMem( size );  // fills with zeros;  //GetMemory( size );
	//FillChar( Result^, size, 0 );
	end;


procedure   LeaveOnly( leave : string; var s : string );

    var
    	i : int;
	begin
    for i := Length( s ) downto 1 do  begin
        if Pos( s[ i ], leave ) <= 0 then  begin   // bum char
            Delete( s, i, 1 );
        	end;
    	end;
    end;


procedure  SkipSpace( const s : string; var index : integer );

	var
		x : int;
	begin
	x := index;
	while ( x <= Length( s ) ) and ( s[ x ] <= ' ' ) do  Inc( x );
	index := x;
	end;


function   WithoutSpaces( const s : string ) : string;

	var
		x : int;
		c : char;
	begin
	Result := '';
	for x := 1 to Length( s ) do  begin
		c := s[ x ];
		if c <= ' ' then  c := '_';
		Result := Result + c;
		end;
	end;


function   Trim( const s : string ) : string;

	var
		x : int;
	begin
	result := s;
	x := 1;
	SkipSpace( result, x );
	if x > 1 then  Delete( result, 1, x - 1 );
	for x := Length( result ) downto 1 do  begin
		if result[ x ] > ' ' then  begin
			if x < Length( result ) then  begin
				Delete( result, x + 1, Length( result ) - x );
				end;
			break;
			end;
		end;
	end;


procedure  TrimThis( var s : string );

	var
		x : int;
	begin
	x := 1;
	SkipSpace( s, x );
	if x > 1 then  Delete( s, 1, x - 1 );
	for x := Length( s ) downto 1 do  begin
		if s[ x ] > ' ' then  begin
			if x < Length( s ) then  begin
                Delete( s, x + 1, Length( s ) - x );
	            end;
        	break;
            end
		end;
	end;


procedure  WindowsPath( var s : string );

	var
        i : int;
    begin
    while true do  begin
    	i := Pos('/', s);
    	if i <= 0 then  break;
    	s[i] := '\';
	    end;
    end;


function   GetString( const s : string; var index : integer ) : string;

	var
		x : int;
	begin
	Result := '';    x := index;
	while ( x <= Length( s ) ) and ( ( s[ x ] = ListSep ) or ( s[ x ] = ' ' ) ) do  begin
		Inc( x );
		end;
	if ( x < Length( s ) ) and ( s[ x ] = StringDelim ) then  begin  // " asdfadfasf " style
		Inc( x );
		while ( x <= Length( s ) ) and ( s[ x ] <> StringDelim ) do  begin
			Result := Result + s[ x ];
			Inc( x );
			end;
		end
	else  begin
	  while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
			Result := Result + s[ x ];
			Inc( x );
			end;
		end;
	index := x;
	end;


function  Slice( const src : string; index : integer; count : integer ) : string;

	var
		i, j : integer;
	begin
    result := '';
    if index <= Length( src ) then  begin
        i := Length( src ) - index + 1;
        if i > count then  i := count;
        SetLength( result, i );
        for j := 1 to i do  result[ j ] := src[ index + j - 1 ];
    	end;
	end;


function   PosN( const substr, src : string; limit : integer ) : int;

	var
		i, x : int;
		f : char;
		found : boolean;
	begin
	result := 0;
	if ( substr <> '' ) and ( Length( src ) >= Length( substr ) ) then  begin
		f := substr[ 1 ];   result := 0;
		if Length( src ) < Length( substr ) + limit - 1 then  limit := Length( src ) - Length( substr ) + 1;

		for i := 1 to limit do  begin
			if src[ i ] = f then  begin  // first char match
				found := true;
				for x := 2 to Length( substr ) do  begin
					if src[ i + x - 1 ] <> substr[ x ] then  begin
						found := false;
						break;
						end;
					end;
				if found then  begin
					result := i;
					break;
					end;
				end;
			end;
		end;
	end;


function   IsAlpha( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	else if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true;
	end;


function   IsDecimal( c : char ) : boolean;

  begin
  Result := ( c >= '0' ) and ( c <= '9' );
  end;


function   IsLowerCase( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	end;


function   IsUpperCase( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true
	end;


function	IsAlphaNumeric( c : char ) : boolean;

	begin
	Result := false;
	if ( c >= 'A' ) and ( c <= 'Z' ) then   Result := true
	else if ( c >= 'a' ) and ( c <= 'z' ) then   Result := true
	else if ( c >= '0' ) and ( c <= '9' ) then   Result := true;
	end;


function   GetAlphas( const s : string; var index : integer ) : string;

	var
		x : int;
	begin
	Result := '';    x := index;
	if ( x <= Length( s ) ) and ( s[ x ] = ListSep ) then  begin
		Inc( x );
		end;
	while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
		if IsAlpha( s[ x ] ) then	Result := Result + s[ x ]
		else  break;
		Inc( x );
		end;
	index := x;
	end;


function   Gap( const s : string ) : string;

	var             // typically put a space into flight name before numeric part
		x : int;
	begin
	result := s;
	if ( Length( s ) > 1 ) and ( IsAlpha( s[ 1 ] ) )  then  begin
		for x := 3 to Length( s ) do  begin
			if not IsAlpha( s[ x ] ) then  begin
				Insert( ' ', result, x );
				break;
				end;
			end;
		end;
	end;


function   GetHex( const s : string; var index  : integer ) : int;  //  F00BAA or , $F00BAA

	var
		x : int;
		c : char;
	begin
	Result := 0;    x := index;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ListSep ) or ( s[ x ] = '$' ) ) do  begin
		Inc( x );
		end;
	while ( x <= Length( s ) ) and ( s[ x ] <> ListSep ) do  begin
		if ( s[ x ] >= '0' ) and ( s[ x ] <= '9' )  then  begin
			Result := Result * 16 + ord( s[ x ] ) - ord( '0' );
			end
		else  begin
			c := ToUpper( s[ x ] );
			if ( c >= 'A' ) and ( c <= 'F' ) then  begin
				Result := Result * 16 + ord( c ) + 10 - ord( 'A' );
				end
			else  break;
			end;
		Inc( x );
		end;
	index := x;
	end;


function   GetInt( const s : string; var index : integer ) : integer;

	var
		x : int;
        negate : boolean;
	begin
	Result := 0;    x := index;    negate := false;
	while ( x <= Length( s ) ) and
			( ( s[ x ] <= ' ' ) or ( s[ x ] = ListSep ) ) do  begin
		Inc( x );
		end;
	if x <= Length( s ) then  begin
        if s[ x ] = '-' then  begin
            negate := true;
            Inc( x );
        	end;
		if s[ x ] = '$' then  begin
			result := GetHex( s, x );
			end
		else  begin
			while ( x <= Length( s ) ) and
					( s[ x ] >= '0' ) and ( s[ x ] <= '9' )  do  begin
				result := result * 10 + ord( s[ x ] ) - ord( '0' );
				Inc( x );
				end;
			end;
		end;
	index := x;
    if negate then result := - result;
	end;


function  Decimal( const s : string; var index : cardinal ) : cardinal;

  begin
  Result := 0;
  while ( index <= cardinal( Length( s ) ) ) and
        ( ( s[ index ] = ' ' ) or ( s[ index ] = ',' ) ) do  begin
    Inc( index );
    end;
  while ( index <= cardinal( Length( s ) ) ) and
        ( s[ index ] >= '0' ) and ( s[ index ] <= '9' )  do  begin
    Result := Result * 10 + ord( s[ index ] ) - ord( '0' );
    Inc( index );
    end;
  end;


function  Decimal( const s : string;  var index : int ) : int;

  begin
  Result := 0;
  while ( index <= Length( s ) ) and
        ( ( s[ index ] = ' ' ) or ( s[ index ] = ',' ) ) do  begin
    Inc( index );
    end;
  while ( index <= Length( s ) ) and
        ( s[ index ] >= '0' ) and ( s[ index ] <= '9' )  do  begin
    Result := Result * 10 + ord( s[ index ] ) - ord( '0' );
    Inc( index );
    end;
  end;


function   InList( const item, list : string ) : boolean;

	var              // is mCheckInName in supplied list of checkins
		p : int;
	begin
	result := false;
	p := Pos( item, list );
	if p > 0 then  begin   // worth testing
		if ( p = 1 ) or ( list[ p-1 ] = ListSep ) then  begin
			p := p + Length( item );
			if ( p > Length( list ) ) or ( list[ p ] = ListSep ) then  result := true;
			end;
		end;
	end;


function   ListIndex( const item, list : string ) : integer;

	var              // is mCheckInName in supplied list of checkins
		p : int;
        it : string;
        sl : TStringList;
	begin
	result := -1;
    it :=  LowerCase( item );
	p := 1;  sl := BuildParamsL( list, p );
    for p := 0 to sl.Count - 1 do  begin
        if it = LowerCase( sl[ p ] ) then  begin
            result := p;
            break;
        	end;
    	end;
	end;


function   ListItem( const list : string; index : int ) : string;

	var              // is mCheckInName in supplied list of checkins
		p : int;
        sl : TStringList;
	begin
	p := 1;  sl := BuildParamsL( list, p );
    result := ListItem( sl, index );
    sl.Free;
	end;


function   ListItem( const list : TStringList; index : int ) : string;

	begin
	result := '';
    if list <> nil then  begin
        if ( index >= 0 ) and ( index < list.Count ) then  result := list[ index ];
    	end;
	end;


function   BuildParamsL( const params : string; var index : int; sep : char = ListSep ) : TStringList;

	var
		pl : TStringList;
		nam, p : string;
		x : integer;
	begin
	pl := TStringList.Create;    x := index;
	if params <> '' then  begin
		p := params;
		if p[ Length( p ) ] = ')' then  p[ Length( p ) ] := sep
		else if p[ Length( p ) ] <> sep then   p := p + sep;

		SkipSpace( p, x );
		while x <= Length( p )  do  begin
			if p[ x ] = sep then  begin
                nam := Trim( nam );
				if ( nam <> '' ) or ( sep = ListSep ) then  begin   // , lists allow empty item
					pl.Add( nam );
					nam := '';
					end;
				Inc( x );
				SkipSpace( p, x );
				end
			else  begin
				nam := nam + p[ x ];
				Inc( x );
				end;
			end;
		end;
	Result := pl;   index := x;
	end;


function   StrToList( const s : string ) : TStringList;

	var
		x : int;
	begin
	x := 1;
	result := BuildParamsL( s, x );
	end;

function   ListToStr( const list : TStringList ) : string;

	var
		x : int;
	begin
	result := '';
	//result := list.CommaText;  // quotes anything with a space
	for x := 0 to list.Count - 1 do  begin
        if x = 0 then	result := list[ x ]
        else            result := result + ',' + list[ x ]
		end;
	end;


function	ListsEqual( const listA, listB : TStrings ) : boolean;

    var
    	x : int;
	begin
    result := true;
    if listA.Count <> listB.Count then  result := false
    else  for x := 0 to listA.Count - 1 do  begin
        if listA[ x ] <> listB[ x ] then    begin
            result := false;
            break;
        	end;
    	end;
    end;


function    ExcludeFromList( const item, list : string ) : string;

    var
        x : int;
        l : TStringList;
	begin
    x := 1;
    l := BuildParamsL( list, x );
    x := 0;
    while x < l.Count do  begin
        if l[ x ] = item then  l.Delete( x )
        else Inc( x );
    	end;
    result := ListToStr( l );
    l.Free;
    end;


function    IncludeInList( const item, list : string ) : string;

    var
        x : int;
        l : TStringList;
	begin
    x := 1;
    l := BuildParamsL( list, x );
    x := 0;
    while true do  begin
        if x = l.Count then  begin
            l.Add( item );
            break;
        	end;
        if l[ x ] > item then  begin
            l.Insert( x, item );
            break;
        	end;
        Inc( x );
    	end;
    result := ListToStr( l );
    l.Free;
    end;


function   GetLine( const s : string; var index : integer ) : string;

	var
		x : int;
	begin
	x := index;  result := '';
	while x <= Length( s ) do  begin
        if s[ x ] = #13 then  begin  Inc( x );  break;  end;
        if s[ x ] = #10 then    begin  end
		else  Result := Result + s[ x ];
		Inc( x );
		end;
	index := x;
	end;


function   LineCount( const s : string ) : int;

	var
		x : int;
        trailingCh : boolean;
	begin
	result := 0;  trailingCh := false;   x := 1;
	while x <= Length( s ) do  begin
        if s[ x ] = #13 then  begin  Inc( result ); trailingCh := false; end
        else if s[ x ] = #10 then  begin end
		else  trailingCh := true;      // catch any unterminated line
		Inc( x );
		end;
	if trailingCh then  Inc( result );
	end;


procedure  CopyStrList( const src : TStringList; dest : TStringList );

	var
		i : int;
	begin
	dest.Clear;
	for i := 0 to src.Count - 1 do  dest.Add( src[ i ] );
	end;


function    NoGapLower( s : string ) : string;

    var
        x : int;
        c : char;
	begin
    result := '';
    for x := 1 to Length( s ) do  begin
        c := s[ x ];
        if c <> ' ' then  begin
            result := result + LowerCase( c );
        	end;
        end;
    end;


function  UnCamel( const n : string; gapChar : char = ' ' ) : string;

	var
		x : int;
		first : boolean;
	begin
	x := 1;    result := '';   first := true;
	while ( x <= Length( n ) ) and ( IsLowerCase( n[ x ] ) ) do  Inc( x );  // throw leading lowers
	while x <= Length( n ) do  begin
		if IsUpperCase( n[ x ] ) then  begin
			if not first and ( gapChar <> #0 ) then  result := result + gapChar;
			first := false;
			end;
		result := result + n[ x ];
		Inc( x );
		end;
	if result = '' then  result := n;
	end;


function   EnumToStr( val : Integer; pEnumTypePtr : PTypeInfo; unCaml : boolean = true ) : string;  // stolen from Csi

	var           // eg pt.Content := EnumToStr( Ord( GetSortPref ), TypeInfo( aSortPref ) );
		lEnumDataPtr : PTypeData;
	begin
    result := '';
    if pEnumTypePtr.Kind = tkEnumeration then  begin
        lEnumDataPtr := GetTypeData( pEnumTypePtr );
        if ( val >= lEnumDataPtr.MinValue) and ( val <= lEnumDataPtr.MaxValue ) then  begin
            result := GetEnumName( pEnumTypePtr, val );
            if unCaml then  result := UnCamel( Result );
            end;
    	end;
	end;


function   EnumToSL( pEnumTypePtr : PTypeInfo ) : TStringList;

	var
		lEnumDataPtr : PTypeData;
        x : int;
	begin
    result := TStringList.Create;
	lEnumDataPtr := GetTypeData( pEnumTypePtr );
	for x := lEnumDataPtr.MinValue to lEnumDataPtr.MaxValue do  begin
        result.Add( UnCamel( GetEnumName( pEnumTypePtr, x ) ) );
    	end;
    end;


function   FindEnum( nam : string; pEnumTypePtr : PTypeInfo ) : int;

	var
		lEnumDataPtr : PTypeData;
        x : int;
	begin
    result := -1;
    if pEnumTypePtr.Kind = tkEnumeration then  begin
        lEnumDataPtr := GetTypeData( pEnumTypePtr );
        for x := lEnumDataPtr.MinValue to lEnumDataPtr.MaxValue do  begin
        	if nam = UnCamel( GetEnumName( pEnumTypePtr, x ) )  then  begin
            	result := x;
                break;
            	end;
            end;
	    end;
    end;


procedure  InitListControl( lc : TCustomListControl; pEnumTypePtr : PTypeInfo  );

	var
        sl : TStringList;
        s : string;
	begin
    sl := EnumToSL( pEnumTypePtr );

    for s in sl do  lc.AddItem( s, nil );
    lc.ItemIndex := 0;
    sl.Free;
    end;

// _________________________ TIME ______________________________________________



function D (Val: byte): char;
	begin
		D := chr ((Val) + ord ('0'));
	end;


function DD (Val: byte): string;
	begin
		//if Val <= 99 then
			DD := D (Val div 10) + D (Val mod 10)
		//else
			//DD := '??';
	end;


function DDD (Val: word): string;
	begin
		DDD := DD (Val div 10) + D (Val mod 10);
	end;


function DDDD (Val: word): string;
	begin
		DDDD := DD (Val div 100) + DD (Val mod 100);
	end;


function DTToStr (dt: TDateTime): string;
	var
		yr, mo, dy: word;
		hr, mn, se, ms: word;
	begin
    Result := '';
    if dt <> 0.0 then  begin
        try
            DecodeDate (dt, yr, mo, dy);
            DecodeTime (dt, hr, mn, se, ms);
        except
        	end;
		Result := DDDD (yr) + DD (mo) + DD (dy);
        // if hr + mn + se <> 0 then  begin   // todo remove test, round out seconds
            Result := Result + ' ' + DD (hr) + DD (mn) + DD (se);
	        // end;
    	end;
	end;


function   Minutes() : int;

	var
		t : tDateTime;
		Hour, Min, Sec, MSec : word;
	begin
	t := Now;
	DecodeTime( t, Hour, Min, Sec, MSec );
	Result := Min;
	end;


function   ShortDateTime( dt : TDateTime ) : string;

	var
		fs : TFormatSettings;
	begin
    fs.ShortDateFormat := 'yyyymmdd';
    fs.LongTimeFormat := 'hhnnss';
    result := DateTimeToStr( dt, fs );
    end;


function   TimeDateToStr( t : TDateTime; fmt : string = '' ) : string;  // 'hh:nn ddd d mmm yy'

	var
		st : string;
	begin
	if fmt = '' then  fmt := 'hh:nn ddd d mmm yy';
    if t = 0.0 then   t := Now();
    DateTimeToString( st, fmt, t );
	result := st;
	end;


// _____________________________ IP ____________________________________________

function   ParseIPs( const ips : string ) : TList;
	var

		x : int;
		ip, ip2 : int;
		ipl : TList;

	procedure  SkipSpace();

		begin
		while ( ips[ x ] = ' ' ) and ( x <= Length( ips ) ) do  Inc( x );
		end;

	function   GetByte() : int;

		var
			b : string;
			i : int;
		begin
		while ( ips[ x ] >= '0' ) and ( ips[ x ] <= '9' ) and ( x <= Length( ips ) ) do  begin
			b := b + ips[ x ];
			Inc( x );
			end;
		i := 1;
		Result := GetInt( b, i );
		if ( Result < 0 ) or ( Result > 255 ) then  begin
			// Result := 0;
			raise Exception.Create('Invalid IP');
			end;
		end;

	function   GetIP() : int;

		var
			n : int;
			begin
		SkipSpace;
		Result := 0;
		for n := 0 to 3 do  begin
			Result := Result * 256 + GetByte();
			if n < 3 then  begin
				if ips[ x ] <> '.' then  raise Exception.Create('Invalid IP');
				Inc( x );
				end;
			 end;
		end;

	begin  // 192.168.0.20-23, 192.168.0.34 -> list of ints
	ipl := TList.Create;
	x := 1;
	while x <= Length( ips ) do  begin
		ip := GetIP();
		ipl.Add( Pointer( ip ) );
		SkipSpace;
		if x <= Length( ips ) then  begin
			if ips[ x ] = '-' then  begin
				Inc( x );
				ip2 := ip and Int( $FFFFFF00 ) + GetByte();
				if ip2 <= ip then  raise Exception.Create('Invalid IP range');
				repeat
					Inc( ip );
					ipl.Add( Pointer( ip ) );
					until ip = ip2;
				SkipSpace;
				end;
			if x > Length( ips ) then  break;
			if ips[ x ] <> ',' then  break;
			Inc( x );
			end;
		end;
	Result := ipl;
	end;


function   IPtoStr( ipn : int ) : string;

	begin
	Result := IntToStr( ipn and $FF000000 shr 24 );
	Result := Result + '.' + IntToStr( ipn and $00FF0000 shr 16 );
	Result := Result + '.' + IntToStr( ipn and $0000FF00 shr 8 );
	Result := Result + '.' + IntToStr( ipn and $000000FF );
	end;


procedure	SetEmptyPCharLength( var s : string; len : card );    // use a string as an empty pchar buffer, see ComputerName below

	begin
    SetLength( s, len );
    s[ 1 ] := #0;
    end;


procedure	PCharToStr( var s : string; len : card );  // convert PChar buffer back to string, see ComputerName below

    var
        i : card;
	begin
    for i := 1 to Cardinal( Length( s ) ) do  begin
        if s[ i ] = #0 then  begin   // strlen()
            SetLength( s, i - 1 );
            break;
            end;
        end;
	end;


function	ComputerName : string;

    var
        i : card;
	begin
    i := 256;
    SetEmptyPCharLength( result, i );
    if GetComputerName( @result[ 1 ], i ) then  begin
    	PCharToStr( result, i );
    	end;
    end;



// http://delphi.about.com/od/networking/l/aa103100a.htm

function GetIPFromHost( var HostName, IPaddr, WSAErr: string ): Boolean;
type
  Name = array[0..100] of Char;
  PName = ^Name;
var
  HEnt: pHostEnt;
  AnsiHostName : Ansistring;
  WSAData: TWSAData;
  i: Integer;
begin
  Result := False;
  if WSAStartup($0101, WSAData) <> 0 then begin
    WSAErr := 'Winsock is not responding."';
    Exit;
  end;
  IPaddr := '';
  SetLength( AnsiHostName, 256 );  AnsiHostName[ 1 ] := #0;
  if GetHostName(@AnsiHostName[ 1 ], 256 ) = 0 then    //  GetHostName(HName^, SizeOf(Name)) = 0 then
  	begin
    for i := 1 to Cardinal( Length( AnsiHostName ) ) do  begin
        if AnsiHostName[ i ] = #0 then  begin   // strlen()
            SetLength( AnsiHostName, i - 1 );
            break;
            end;
        end;


    HostName := String( AnsiHostName );
    HEnt := GetHostByName(@AnsiHostName[ 1 ]);
    for i := 0 to HEnt^.h_length - 1 do
     IPaddr :=
      Concat(IPaddr,
      IntToStr(Ord(HEnt^.h_addr_list^[i])) + '.');
    SetLength(IPaddr, Length(IPaddr) - 1);
    Result := True;
  end
  else begin
   case WSAGetLastError of
    WSANOTINITIALISED:WSAErr:='WSANotInitialised';
    WSAENETDOWN      :WSAErr:='WSAENetDown';
    WSAEINPROGRESS   :WSAErr:='WSAEInProgress';
   end;
  end;
  WSACleanup;
end;


//________________________________ FILES _______________________________________



function  BackUpFileName( fn : string ) : string;   // .~ext

	var
		ext : string;
	begin
	result := fn;
	ext := ExtractFileExt( fn );
	if ( Length( ext ) >= 2 ) and ( ext[ 1 ] = '.' ) then  begin
		Insert( '~', ext, 2 );
		result := ChangeFileExt( fn, ext );
		end
	else  result := fn + '.bak';
	end;


function   ListFiles( const path, ext : string ) : TStringList;

	var
		sr: TSearchRec;
		FileAttrs: Integer;
		p : string;
	begin
	p := path;
	result := TStringList.Create;    result.Sorted := true;
	if ( path <> '' ) and ( path[ 1 ] = '\' ) then  p := GetCurrentDir + path;
	if ext <> '' then  begin
		if ext[ 1 ] = '.' then  p := p + '*' + ext  else  p := p + '*.' + ext;
		end
	else   p := p + '*.*';

	FileAttrs := faAnyFile;
	if SysUtils.FindFirst( p, FileAttrs, sr) = 0 then  begin
		repeat
			if sr.Attr and faDirectory = 0 then  result.Add( sr.Name );
			until FindNext(sr) <> 0;
		FindClose(sr);
		end;
	end;


function   FileToStr( const fn : string ) : string;

    var
        f : textfile;
        l : string;
	begin
    result := '';
    Assign( f, fn );
    Reset( f );
    while not EOF( f ) do  begin
        Readln( f, l );
        result := result + l + EOL;
    	end;
    CloseFile( f );
    end;


function  FileOpenRead( name : string ) : THandle;

	const
		GENERIC_WRITE =   $40000000;  // WinNT.h  CreateFile params
		GENERIC_READ  =   $80000000;
		FILE_SHARE_READ	= $00000001;  // WinNT.h
		OPEN_EXISTING   = 3;          // WinBase.h
		CREATE_ALWAYS   = 0;

	begin
	Result := CreateFile( PChar( name ), GENERIC_READ,
						FILE_SHARE_READ, Nil, OPEN_EXISTING, 0, 0);
	end;                  // use CloseHandle


function  FileOpenWrite( name : string ) : THandle;

	const
		GENERIC_WRITE =   $40000000;  // WinNT.h  CreateFile params
		GENERIC_READ  =   $80000000;
		FILE_SHARE_READ	= $00000001;  // WinNT.h
		OPEN_EXISTING   = 3;          // WinBase.h
		CREATE_ALWAYS   = 0;

	begin
	Result := CreateFile( PChar( name ), GENERIC_WRITE,
						FILE_SHARE_READ, Nil, CREATE_ALWAYS, 0, 0);
	end;                  // use CloseHandle


function	FileNameHasDecimal( fn : string ) : boolean;

	var
    	name : string;
        i : int;
	begin
    result := false;
	name := ExtractFileName( fn );
    for i := 1 to Length( name ) do  begin
        if IsDecimal( name[ i ] ) then  begin
            result := true;
            break;
        	end;
    	end;
    end;


function	NewestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;
        newest : int;
    begin
    if FindFirst( directory + '\' + match, $80, fil ) = 0  then  begin // faNormal
    	newest := 0;
        repeat
            if fil.Time > newest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    newest := fil.Time;
                    result := fil.Name;
                	end;
            	end;
            until ( FindNext( fil ) <> 0 )
    	end;
    end;


function	OldestFile( directory, match : string; numbered : boolean = false ) : string;

    var
        fil : TSearchRec;
        oldest : int;
    begin
    if FindFirst( directory + '\' + match, $80, fil ) = 0  then  begin // faNormal
    	oldest := MaxInt;
        repeat
            if fil.Time < oldest then  begin
            	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                    oldest := fil.Time;
                    result := fil.Name;
                	end;
            	end;
            until ( FindNext( fil ) <> 0 )
    	end;
    end;


function	CountFiles( directory, match : string; numbered : boolean = false ) : int;

    var
        fil : TSearchRec;
    begin
    result := 0;
    if FindFirst( directory + '\' + match, $80, fil ) = 0  then  begin // faNormal
        repeat
        	if not numbered or FileNameHasDecimal( fil.Name ) then  begin
                Inc( result );
            	end;
            until ( FindNext( fil ) <> 0 )
    	end;
    end;


function	AnyDecimal( str : string ) : int;

    var
        i, x : int;
    begin
    result := 0;
    for i := 1 to Length( str ) do  begin
        if IsDecimal( str[ i ] ) then  begin
        	x := i;
            result := GetInt( str, x );
            break;
        	end;
    	end;
    end;


function	ExtractOnlyFileName( fn : string ) : string;

	var              // strip directory, extension and decimal tail
    	p : int;
	begin
    result := ExtractFileName( fn );
    p := Pos( '.', fn );                        // strip extension
    if p > 0 then  Delete( result, p, 99 );
    for p := 1 to Length( result ) do  begin       // strip decimal
        if IsDecimal( result[ p ] ) then  begin
        	Delete( result, p, 99 );
            break;
        	end;
    	end;
    end;




function   WinError( var er : int ) : string;

	begin
	result := '';
	er := GetLastError();
	if er <> 0 then  begin
		result := SysErrorMessage( er ) + ' ER' + IntToStr( er );
		end;
	end;


function   WinError() : string;

	var
		error : int;
	begin
	result := WinError( error );
	end;


function   SetPowerOf2( n : int ) : int;

  	begin
    result := 1;
    while result < n do  begin
        result := result shl 1;
    	end;
  	end;

{
BOOL ReadFile(
  HANDLE hFile,
  LPVOID lpBuffer,
  DWORD nNumberOfBytesToRead,
  LPDWORD lpNumberOfBytesRead,
  LPOVERLAPPED lpOverlapped
);
}
end.
