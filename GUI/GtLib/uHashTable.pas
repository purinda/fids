unit uHashTable;

interface

uses
  uGT;

type
  int = integer;

  cHashTable<T> = class
  type
    aGetNameFn = reference to function(item: T): string;

  type
    aSetUndefinedFn = reference to procedure(var item: T);

  type
    aTestUndefinedFn = reference to function(item: T): boolean;

  type
    aTable = array of T;
  public
    constructor Create(nameFn: aGetNameFn; UndefFn: aSetUndefinedFn;
      testUndefFn: aTestUndefinedFn; size: int = 32);
    function Add(item: T): boolean;
    function Find(const s: string): T;
    procedure Delete(item: T);
    // procedure Free;
  private
    mContents: cardinal;
    mSize: cardinal;
    mMaxCollisions: cardinal;
    mDelShifted: cardinal;
    mTable: aTable;
    mGetNameFun: aGetNameFn;
    mSetUndefinedFn: aSetUndefinedFn;
    mTestUndefinedFn: aTestUndefinedFn;
    // function   QuickHashCode( const Str: string ): Integer;
    function FindItem(item: T): int;
    procedure NewSize(size: cardinal);
    // function   Hash( const key : string ) : cardinal;
  end;

function Ror1(val: card): card; // rotate right 1
function Hash(const key: string): cardinal;

var
  Primes: array [0 .. 255] of cardinal = { seed = 4242 } (
    $1D1B9AE3,
    $7C3AF24B,
    $960D6D2F,
    $B2EE5951,
    $C0E36CC9,
    $495B136D,
    $DFA2D0D1,
    $746FF015,
    $CE1745A3,
    $2239537D,
    $CE9F42A5,
    $0718CE2F,
    $E9791D81,
    $7613D763,
    $F30B0C0F,
    $49E6F853,
    $ABEA098F,
    $9ACA4A4F,
    $E57012E9,
    $D036C1C9,
    $9223E463,
    $9E868557,
    $8689D171,
    $05F1ED47,
    $4D06A5B9,
    $4BBAD44B,
    $6A0EA14D,
    $874C1633,
    $F953A2E9,
    $25AEB6B5,
    $DDD0BC1B,
    $6409A3FB,
    $C3630F43,
    $FE661C23,
    $065B08BB,
    $6C42B6A1,
    $4DE2D82F,
    $ED89907D,
    $8DA10E3F,
    $0E55C65B,
    $447F27A3,
    $92CA9A31,
    $6EBA26D9,
    $CC14FA23,
    $D58C7E23,
    $ADE906BD,
    $6AAA758B,
    $470F6E87,
    $FC7A40A1,
    $14DC0DCD,
    $5D744CCB,
    $89362BF7,
    $9E098F45,
    $D0CF3141,
    $11FA4565,
    $E92A12E7,
    $3D33690F,
    $FCC13441,
    $189D793B,
    $431329ED,
    $C03B719F,
    $56A1027F,
    $E4C4D81B,
    $5FEB0C83,
    $C54DE61D,
    $6E0D7D05,
    $5F7C9103,
    $4358B343,
    $6A27B4B1,
    $E89C62FD,
    $49084DFB,
    $B5042E53,
    $24F60955,
    $570269ED,
    $21EDBC2D,
    $98BCB1D9,
    $159A98F1,
    $93F2DAC9,
    $2AC4E1F3,
    $E782335B,
    $924D288F,
    $91426F4F,
    $99B1B053,
    $518C896D,
    $518BAAF1,
    $D2B3C669,
    $6A2A938D,
    $DE2F0A9D,
    $F6A57505,
    $A9EA884D,
    $BA35CB15,
    $3F393AC3,
    $4413FDA9,
    $0799AFD5,
    $43059F0B,
    $B23D92BD,
    $A8B449D5,
    $A984ABC3,
    $36486AE5,
    $6527EA35,
    $5016F227,
    $76BB294B,
    $4C0F65D7,
    $92BB78CB,
    $2DCA4BB5,
    $F960AE71,
    $7262DB99,
    $D1E8AA0F,
    $28F7B60D,
    $258D055B,
    $E2F6A18B,
    $FD85E621,
    $E841C1F3,
    $F8C005C7,
    $39E8013B,
    $A2F1F1A7,
    $AA0E5A7F,
    $1A5185F1,
    $D9A02C3D,
    $A47E1D49,
    $BF1CDE05,
    $F6AE124F,
    $1BB656B1,
    $21AEC64F,
    $F8AC9967,
    $6D38E2C9,
    $62FA58AB,
    $69300AE7,
    $52B64A8F,
    $019AD9F1,
    $2FE601B7,
    $1DE54C87,
    $E4975659,
    $A9ED6BC1,
    $797749C1,
    $1741E4B7,
    $23C53F8B,
    $057F07F3,
    $D151C165,
    $DE808E95,
    $0DF4448B,
    $B9584B7D,
    $87259FC9,
    $68EEC44B,
    $7405695D,
    $4D87DE05,
    $9805AD59,
    $5E99E16D,
    $9C9D1723,
    $016D4DF1,
    $AA1F714B,
    $5346567D,
    $E035D40D,
    $32D45C49,
    $61339EF5,
    $23A06ECB,
    $91B4E5DB,
    $2B58F3D5,
    $849EDAF9,
    $4EED4267,
    $2DDE5F05,
    $9664E30B,
    $65621B57,
    $2AB3683D,
    $01AE967F,
    $79B64C55,
    $628ACDAF,
    $13E7183F,
    $1DD16129,
    $AC44F1D5,
    $1E3D6753,
    $45277A77,
    $AAFF4941,
    $81F50DD1,
    $9B2CE907,
    $31B67367,
    $C61F5067,
    $2EB065C9,
    $5B2DA467,
    $A1E98827,
    $DCC8ACAF,
    $5E20F773,
    $17BD8151,
    $307A299D,
    $9315FB1D,
    $CDA647A5,
    $C4E1C97F,
    $D3A1DE41,
    $B8135729,
    $1A1A1BA5,
    $ECCA1591,
    $0D7AA1E3,
    $75C31D6B,
    $7E053125,
    $EBA2152D,
    $D7678D23,
    $22EC51B5,
    $7E49C6C3,
    $4F7D2D91,
    $84E7E395,
    $26F06E27,
    $0C589697,
    $07C3A4CD,
    $046BEB33,
    $D30376ED,
    $672136B1,
    $C58E0739,
    $E8BAD071,
    $CEAFA607,
    $17AA2F35,
    $215BC7D7,
    $B049A30F,
    $046F503B,
    $E7292931,
    $44256C9B,
    $4CF9C2CD,
    $CEB2856F,
    $09F2A317,
    $0E7C0371,
    $CAD0CE97,
    $5CEA9C09,
    $78A2F655,
    $6CEC3203,
    $19FEC9C5,
    $582B84E7,
    $630C8D5F,
    $AAE6D0E7,
    $90E06C3F,
    $D9656CA1,
    $D7E41313,
    $2E6330DB,
    $1592DDC7,
    $4C70F125,
    $0F647B79,
    $9035B547,
    $03D93757,
    $38B4B151,
    $395B2A8D,
    $905CC59F,
    $4954B835,
    $938773C3,
    $27212AA7,
    $4AB9E15D,
    $F74F01C7,
    $4F1CE093,
    $495BCA05,
    $1FAB48CD,
    $750F0B87,
    $AD64DC81,
    $C8B93AC3,
    $640B35E1,
    $12A1E13B,
    $2E930DDD,
    $282827BD,
    $9B25B6AD,
    $9755C5A3
  );

implementation

uses uUtils;

function Ror1(val: card): card; inline;

begin
  if Odd(val) then
  begin
    result := val shr 1 or $80000000;
  end
  else
  begin
    result := val shr 1;
  end;
end;

function Hash(const key: string): cardinal; inline;

var
  x: cardinal;
begin
  result := 0;
  for x := 1 to Length(key) do
  begin
    result := Ror1(result) xor uHashTable.Primes[Ord(key[x]) and $FF];
  end;
end;

constructor cHashTable<T>.Create(nameFn: aGetNameFn; UndefFn: aSetUndefinedFn;
  testUndefFn: aTestUndefinedFn; size: int = 32);

begin
  mGetNameFun := nameFn;
  mSetUndefinedFn := UndefFn;
  mTestUndefinedFn := testUndefFn;
  NewSize(size);
end;


// procedure  cHashTable<T>.Free;
//
// begin
// if self <> nil then  Destroy;
// end;


// function  cHashTable<T>.Hash( const key : string ) : cardinal;
//
// var
// x : cardinal;
// begin
// result := 0;
// for x := 1 to Length( key ) do  begin
// result := ( result shr 1 ) xor uHashTable.Primes[ Ord( key[ x ] ) and $FF ];
// end;
// end;


// function  cHashTable<T>.Hash( const key : string ) : integer;
//
// var      // bob jenkins hash fun
// h, l, i : integer;
// begin
// h := 0;
// l := Length( Key );
// // if l > 10  then  l := 10;
// for i := 1 to l  do  begin
// h := h + Ord( key[ i ] );
// h := h + ( h shl 10 );
// h := h xor ( h shr 6 );
// end;
// h := h + ( h shl 3 );
// h := h xor ( h shr 11 );
// h := h + ( h shl 15 );
// result := h and ( mSize - 1 );    // effectively mod size
// end;
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
  end; }

function cHashTable<T>.FindItem(item: T): int;

var
  h: integer;
  s: string;
begin
  result := -1;
  s := mGetNameFun(item);
  h := Hash(s) and (mSize - 1);
  while true do
  begin
    if mTestUndefinedFn(mTable[h]) then
    begin
      break;
    end
    else if mGetNameFun(mTable[h]) = s then
    begin
      result := h;
      break;
    end;
    Inc(h);
    if h > High(mTable) then
      h := 0;
  end;
end;

procedure cHashTable<T>.NewSize(size: cardinal);

var // rebuild table into new bigger size
  nt: aTable;
  x: cardinal;
  h: cardinal;
  n: string;
begin
  // old := mTable;
  mSize := SetPowerOf2(size);
  mMaxCollisions := 0;
  if mContents = 0 then
    SetLength(mTable, mSize) // no need to rebuild
  else
  begin
    SetLength(nt, mSize);
    for x := 0 to High(nt) do
      mSetUndefinedFn(nt[x]); // clear table
    for x := 0 to High(mTable) do
    begin // build new bigger table
      if not mTestUndefinedFn(mTable[x]) then
      begin
        n := mGetNameFun(mTable[x]);
        h := Hash(n) and (mSize - 1);
        while true do
        begin
          if mTestUndefinedFn(nt[h]) then
          begin
            nt[h] := mTable[x];
            break;
          end;
          Inc(h);
          if h > cardinal(High(nt)) then
            h := 0;
        end;
      end;
    end;
    mTable := nt;
  end;
end;

function cHashTable<T>.Add(item: T): boolean;

var
  h, l, c: cardinal;
  n: string;
begin
  result := false;
  c := 0;
  if (mContents * 10) div 7 > cardinal(High(mTable)) then
  begin // 70% full so upsize table
    if High(mTable) < 2048 then
      h := High(mTable) * 8
    else
      h := High(mTable) * 2;
    NewSize(h);
  end;
  n := mGetNameFun(item);
  h := Hash(n) and (mSize - 1);
  l := h;
  while true do
  begin
    if mTestUndefinedFn(mTable[h]) then
    begin
      mTable[h] := item;
      result := true;
      Inc(mContents);
      break;
    end
    else if mGetNameFun(mTable[h]) = n then
      break;
    Inc(h);
    Inc(c);

    if h > cardinal(High(mTable)) then
      h := 0;
    if h = l then
      break; // bad - table is full
  end;
  if c > mMaxCollisions then
    mMaxCollisions := c;
end;

function cHashTable<T>.Find(const s: string): T;

var
  h: integer;
begin
  result := Default (T);
  h := Hash(s) and (mSize - 1);

  while true do
  begin
    if mTestUndefinedFn(mTable[h]) then
    begin
      break;
    end
    else if mGetNameFun(mTable[h]) = s then
    begin
      result := mTable[h];
      break;
    end;
    Inc(h);
    if h > High(mTable) then
      h := 0;
  end;
end;

procedure cHashTable<T>.Delete(item: T);

var
  x0, x, h, mt: int;
  temp: T;
begin
  x0 := FindItem(item);
  if x0 >= 0 then
  begin
    mSetUndefinedFn(mTable[x0]);
    x := x0;
    while true do
    begin // need to unshift any previous collided entries
      Inc(x);
      if x > High(mTable) then
        x := 0;
      // unshift previouc collision shifted entries
      if mTestUndefinedFn(mTable[x]) then
        break; // empty slot so finished
      h := Hash(mGetNameFun(mTable[x])) and (mSize - 1);
      if x <> h then
      begin // possibly shift it back
        temp := mTable[x];
        mSetUndefinedFn(mTable[x]);
        Add(temp);
        Inc(mDelShifted);
      end;
    end;
  end;
end;
(*
  USEAGE

  mAtFunctionTable := cHashTable< aAtFunction >.Create(
  function( af : aAtFunction ) : string  begin  result := AFName[ af ];  end,
  procedure( var af : aAtFunction )  begin  af := afNone;  end,
  function( af : aAtFunction ) : boolean  begin  result := af = afNone;  end );
  for af := Succ( afNone ) to High( aAtFunction ) do  begin
  if not mAtFunctionTable.Add( af ) then ShowMessage( 'Format Instruction collision in cExpansion.Create ' + AFName[ af ] );
  end;

*)

end.
