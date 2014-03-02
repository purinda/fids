unit uHashTable;

interface

type
    int = integer;

	cHashTable<T> = class
    	type aGetNameFn = reference to function( item : T ) : string;
    	type aSetUndefinedFn = reference to procedure( var item : T );
    	type aTestUndefinedFn = reference to function( item : T ) : boolean;
        type aTable = array of T;
		public
            constructor Create( nameFn : aGetNameFn; UndefFn : aSetUndefinedFn; testUndefFn : aTestUndefinedFn; size : int = 32 );
			function  Add( item : T ) : boolean;
			function  Find( const s : string ) : T;
            procedure Delete( item : T );
			procedure Free;
		private
        	mContents : int;
            mSize : int;
			mMaxCollisions : int;
			mDelShifted : int;
			mTable : aTable;
            mGetNameFun : aGetNameFn;
            mSetUndefinedFn : aSetUndefinedFn;
            mTestUndefinedFn : aTestUndefinedFn;
			// function   QuickHashCode( const Str: string ): Integer;
			function   FindItem( item : T ) : int;
			procedure  NewSize( size : int );
			function   Hash( const key : string ) : integer;
		end;


implementation

uses  uUtils;


constructor  cHashTable<T>.Create( nameFn : aGetNameFn;
	UndefFn : aSetUndefinedFn; testUndefFn : aTestUndefinedFn; size : int = 32 );

	begin
    mGetNameFun := nameFn;
    mSetUndefinedFn := UndefFn;
    mTestUndefinedFn := testUndefFn;
    NewSize( size );
    end;


procedure  cHashTable<T>.Free;

	begin
    if self <> nil then  Destroy;
    end;


function  cHashTable<T>.Hash( const key : string ) : integer;

	var      // bob jenkins hash fun
    	h, l, i : integer;
	begin
    h := 0;
    l := Length( Key );
    // if l > 10  then  l := 10;
    for i := 1 to l  do  begin
        h := h + Ord( key[ i ] );
        h := h + ( h shl 10 );
        h := h xor ( h shr 6 );
    	end;
    h := h + ( h shl 3 );
    h := h xor ( h shr 11 );
    h := h + ( h shl 15 );
    result := h and ( mSize - 1 );    // effectively mod size
    end;
{
function cHashTable<T>.QuickHashCode( const Str: string ): Integer;

var          //http://www.scalabium.com/faq/dct0093.htm
  Off, Len, Skip, I: Integer;
begin
  Result := 0;
  Off := 1;
  Len := Length(Str);
  if Len < 16 then
    for I := (Len - 1) downto 0 do
    begin
      Result := (Result * 37) + Ord(Str[Off]);
      Inc(Off);
    end
  else
  begin
    // Only sample some characters
    Skip := Len div 8;
    I := Len - 1;
    while I >= 0 do
    begin
      Result := (Result * 37) + Ord(Str[Off]);
      Dec(I, Skip);
      Inc(Off, Skip);
    end;
  end;
end;

function  cHashTable<T>.Hash( const key : string ) : integer;

	var      // delphi hash fun
    	h, l, i : integer;
	begin
	h := QuickHashCode( key );
    result := h and ( mSize - 1 );
    end;   }


function cHashTable<T>.FindItem( item : T ) : int;

    var
    	h : integer;
        s : string;
    begin
    result := -1;
    s := mGetNameFun( item );
    h := Hash( s );
    while true  do  begin
        if mTestUndefinedFn( mTable[ h ] ) then  begin
        	break;
        	end
		else if mGetNameFun( mTable[ h ] ) = s  then  begin
        	result := h;
        	break;
        	end;
        Inc( h );
        if h > High( mTable )  then  h := 0;
        end;
    end;


procedure  cHashTable<T>.NewSize( size : int );

    var                      // rebuild table into new bigger size
        nt : aTable;
        x : int;
        h : integer;
        n : string;
	begin
    //old := mTable;
    mSize := SetPowerOf2( size );    mMaxCollisions := 0;
    if mContents = 0 then  SetLength( mTable, mSize )   // no need to rebuild
    else  begin
        SetLength( nt, mSize );
        for x := 0 to High( nt ) do  mSetUndefinedFn( nt[ x ] );  // clear table
        for x := 0 to High( mTable ) do  begin                    // build new bigger table
        	if not mTestUndefinedFn( mTable[ x ] ) then  begin
                n := mGetNameFun( mTable[ x ] );
                h := Hash( n );
                while true  do  begin
                    if mTestUndefinedFn( nt[ h ] ) then  begin
                        nt[ h ] := mTable[ x ];
                        break;
                        end;
                    Inc( h );
                    if h > High( nt )  then  h := 0;
                    end;
	            end;
        	end;
        mTable := nt;
    	end;
    end;


function cHashTable<T>.Add( item : T ) : boolean;

    var
        h, l, c : integer;
        n : string;
    begin
    result := false;   c := 0;
    if ( mContents * 10 ) div 7 > High( mTable ) then  begin   // 70% full so upsize table
        if High( mTable ) < 2048 then  h := High( mTable ) * 8  else  h := High( mTable ) * 2;
    	NewSize( h );
    	end;
    n := mGetNameFun( item );
    h := Hash( n );
    l := h;
    while true  do  begin
        if mTestUndefinedFn( mTable[ h ] )  then  begin
            mTable[ h ] := item;
            result := true;
            Inc( mContents );
            break;
            end
        else if mGetNameFun( mTable[ h ] ) = n  then  break;
        Inc( h );
        Inc( c );
        
        if h > High( mTable )  then  h := 0;
        if h = l  then  break;    // bad - table is full
        end;
    if c > mMaxCollisions then   mMaxCollisions := c;
    end;


function cHashTable<T>.Find( const s : string ) : T;

    var
    	h : integer;
    begin
    result := Default( T );
    h := Hash( s );
    while true  do  begin
		if mTestUndefinedFn( mTable[ h ] ) then  begin
			break;
        	end
        else if mGetNameFun( mTable[ h ] ) = s  then  begin
        	result := mTable[ h ];
        	break;
        	end;
		Inc( h );
		if h > High( mTable )  then  h := 0;
		end;
    end;


procedure  cHashTable<T>.Delete( item : T );

	var
		x0, x, h, mt : int;
		temp : T;
	begin
	x0 := FindItem( item );
	if x0 >= 0 then  begin
		mSetUndefinedFn( mTable[ x0 ] );
		x := x0;
		while true do  begin   // need to unshift any previous collided entries
			Inc( x );  if x > High( mTable )  then  x := 0;
			// unshift previouc collision shifted entries
			if mTestUndefinedFn( mTable[ x ] ) then  break;  // empty slot so finished
			h := Hash( mGetNameFun( mTable[ x ] ) );
			if x <> h then  begin  // possibly shift it back
				temp := mTable[ x ];
				mSetUndefinedFn( mTable[ x ] );
				Add( temp );
				Inc( mDelShifted );
				end;
			end;
		end;
	end;


end.
