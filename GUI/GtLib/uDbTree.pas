unit uDbTree;

interface

// (C) ALphasoft Pty Ltd

uses
  Classes, Generics.Collections, uGlobalDefs, uHashTable;

// define GLOBAL if using Global.Log etc

const
  RootNodeName = '_Root_';

type
  asDbPath = string; // typically '|Arrivals|QF123-864|Flight|QF123|'
  asDbNode = string; // typically 'Arrivals'
  asDbStub = string; // typically 'Arrivals|QF123-864|'

  apNode = ^aNode;

  aNode = record // a tree node
    ID: word; // copied out from root   Global.Data ID=1
    NodeName: string;
    Content: string;
    SubNodes: TList; // of apNode
    Back: apNode; // back path
    Modified: int; // Seconds at last change
    Table: cHashTable<apNode>; // nil or subtag table
    AutoInc: int; // 0 if not used
    UsrObj: TObject; // user data
  end;

type
  poOnNextNode = procedure(pt: apNode; depth: integer) of object;
  apSortCompareMethod = function(at, bt: apNode): integer of object;
  // < -1,  = 0,  > +1
  aScanTagType = (ttProlog, ttTag, ttTagEnd, ttComment, ttContent);
  aScanProc = reference to procedure(pn: apNode; depth: int);

  cDbTree = class
  public
    mEr: int;
    mNodeCount: integer; // stats
    mEditCount: integer;
    mNewOpNodeCount: integer;
    Log: aLogProc;

    constructor Create(treeID: word = 0; isKey: boolean = false;
      Log: aLogProc = nil); // 1 for global.data to do statistics
    procedure Clear;
    destructor Destroy; override;
    // procedure  Free;
    // procedure  InitXmlDB;
    function GetRoot: apNode; inline;
    function ReadNode(pt: apNode; var name: string;
      var Content: string): integer;
    function ReadSub(pt: apNode; n: integer; var name: string;
      var Content: string): apNode;
    function FindNodeMulti(pt: apNode; const tn: string): apNode;
    // multi level search from pt/nil to NodeName tn
    function Search(pt: apNode; name, cont: string; index: int = 1): apNode;
    // multi search for matching(s)

    function Scan(path: TStringList; NextNode: poOnNextNode): boolean; overload;
    function Scan(pt: apNode; NextNode: poOnNextNode): boolean; overload;
    function Scan(pt: apNode; proc: aScanProc): boolean; overload;
    procedure Sort(pt: apNode; pathl: TStringList);
    procedure QuickSort(pt: apNode; L, R: integer;
      compare: apSortCompareMethod);
    // function   CollectList( pt : apNode ) : TStringList;
    function NewNode(const nam: string; key: boolean; parent: apNode;
      AutoInc: int = 0): apNode; overload; // and add to tree
    function NewNode(const name, attribs: string; parent: apNode)
      : apNode; overload;
    function CopyNode(const ps, pd: apNode): apNode;
    function Rename(pt: apNode; const name: string): boolean;
    function SetFlag(pn: apNode; const flag: string; state: boolean): apNode;
    function SetNode(pn: apNode; var NodeName: string;
      const val: string): apNode;
    procedure DeleteAllFast(pt: apNode; base: boolean = true);
    function CountAllSubs(base: apNode): integer;
    function FollowPath_(pathl: TStringList; pt: apNode = nil): apNode;
      overload;
    function FollowPath_(path: string; pt: apNode = nil): apNode; overload;
    function GetNode(const path: string): apNode; overload;
    function GetNode(addr: TList<integer>): apNode; overload;
    function ListNames(const path: string; pt: apNode = nil): TStringList;
    // returns a TSlist from sub node names
    procedure MoveSubs(ps, pd: apNode);
    procedure CopySubs(const ps: apNode; pd: apNode = nil);
    procedure CopyBranch(const ps: apNode; pd: apNode = nil);
    // function   OpenNode( path : string; pt : apNode = nil ) : apNode;   use followpath
    function HasContent(const path: string; out value: string): boolean;

    function AddToList(pt: apNode; const name, SubName, SubCont: string;
      unique: boolean = false): apNode;
    procedure DelFromList(pt: apNode; const name, SubName, SubCont: string);

    function ForcePath(pathl: TStringList): apNode; overload;
    function ForcePath(path: asDbPath): apNode; overload;

  protected
    // mState : ( tsNone, tsLoading, tsNew );
    mRootNode: aNode; // root of the xml tree
    procedure LogEr(er: int; const s: string);
    function CanAdd(const nam: string; parent: apNode): boolean;
    function AddNode(pNode, parent: apNode): boolean;
    procedure DisposeNode(pt: apNode); // and unlink - searches list
    procedure DisposeNodeFast(pt: apNode); // without unlink
    procedure MoveBranch(pt: apNode);
    function ForceEditPath(pathl: TStringList): apNode;
  private
    mUseCount: int;
    mCount: integer;
    procedure IncAutoInc(parent: apNode);
    function AutoIncName(parent: apNode; const nam: string): string;
    // procedure  AutoIncName( pt : apNode );
    procedure CountNodes(pt: apNode; depth: integer);
    procedure LinkNode(pt, parent: apNode);
    procedure DoScan(pt: apNode; depth: integer; NextNode: poOnNextNode);
  end;

function FindRoot(pt: apNode): apNode;
function FindName(pt: apNode; nam: string): apNode;
// single level search ( with hash table )
function Back(pt: apNode; n: int = 1): apNode; inline;
function NodeContent(pt: apNode): string; inline;
function NodeName(pt: apNode): string; inline;
function TitleNodeName(pt: apNode): string; inline;
function NodeIndex(pt: apNode): int;
function SubNodeCount(pt: apNode): int; overload; inline;
function SubNodeCount(addr: TList<integer>; base: apNode): int; overload;
function SubNode(pt: apNode; n: int): apNode; // n is 0 .. count -1
function ReadContent(pt: apNode; const name: string): string;
function ReadFloat(pt: apNode; const name: string; var ok: boolean): currency;
function ReadInt(pt: apNode; const name: string; var ok: boolean): int;
  overload;
function ReadInt(pt: apNode; const name: string): int; overload;
function PathCoincides(p1, p2: TStringList): boolean;
// is a change to p1 relevant to p2
function PathToStr(path: TStringList): string;
function StrToPath(path: string): TStringList;
function MakeAList(const list: TStringList; sep: char = LSep): string;
// path style  |saf|sfd|
function ParseList(const path: string): TStringList; deprecated;
// see StrToPath  or buildparamL
function IDValue(n: string): int; // get ID decimal value from end of string
function SplitOffAttributes(var nam: string): string;
function ResolvePath(pt: apNode): TStringList;
function ResolvePathStr(pt: apNode): string;
function FollowPath(pathl: TStringList; pt: apNode; Back: integer = 0)
  : apNode; overload;
function FollowPath(path: string; pt: apNode): apNode; overload;
function GetNodeNumerically(addr: TList<integer>; base: apNode): apNode;
// function	PathDifference( pRef, path : TStringList; out back : int ) : TStringList;  // from reference to path
function TrimPathToBase(base: asDbNode; var path: TStringList): boolean;
function PathDif(fromPath, toPath: TStringList; var stepBack: int): TStringList;
function Traverse(start: apNode; backStep: int; path: TStringList;
  backAdjust: int = 0): apNode;
function EachSubNode(const base: apNode; var x: int; out ps1: apNode): boolean;
// subnode enumerator  start x := -1
function UniqueTagName(base: apNode; const SubName: string): string;
function LoadContentList(const pcl: apNode): TStringList;
// creates a list caller must free

procedure SetContent(pt: apNode; val: string); inline;
// LOCAL WRITE FUNCTIONS handle with care
procedure PutOnTop(top: apNode);
procedure Unlink(pt: apNode);
function NodeIndexParent(pt: apNode; out parent: apNode): int;
procedure NodeInsert(pt: apNode; base: apNode; index: int);

implementation

uses
  uUtils, uPoller, Windows, ASCII, Dialogs, SysUtils, StrUtils;

const
  MaxAutoInc = 999999;

var
  SortPath: TStringList; // temp parameter for sort list

  // _____________________________ utilities _____________________________________

function NodeContent(pt: apNode): string;

begin
  result := '';
  if pt <> nil then
    result := pt.Content;
end;

function NodeName(pt: apNode): string;

begin
  result := '';
  if pt <> nil then
    result := pt.NodeName;
end;

function TitleNodeName(pt: apNode): string;

var
  p: int;
begin
  result := UnCamel(NodeName(pt));
  p := Pos('Link', result); // eg AssetLink -> Asset
  if p > 0 then
    Delete(result, p, Length(result) - p + 1);
  repeat
    p := Pos('_', result);
    if p > 1 then
      result[p] := ' ';
  until p = 0;
end;

procedure SetContent(pt: apNode; val: string);

begin
  if (pt <> nil) and (pt.Back <> nil) then
    pt.Content := val;
end;

procedure PutOnTop(top: apNode);

var
  parent: apNode;
  x: int;
begin
  if (top <> nil) and (top.Back <> nil) then
  begin
    parent := top.Back;
    for x := 0 to parent.SubNodes.Count - 1 do
    begin
      if top = apNode(parent.SubNodes[x]) then
      begin
        parent.SubNodes.Delete(x);
        parent.SubNodes.Insert(0, top);
        break;
      end;
    end;
  end;
end;

function NodeIndex(pt: apNode): int;

begin
  result := -1;
  if (pt <> nil) and (pt.Back <> nil) then
  begin
    result := pt.Back.SubNodes.IndexOf(pt);
  end;
end;

function SubNodeCount(pt: apNode): int;

begin
  result := 0;
  if (pt <> nil) and (pt.SubNodes <> nil) then
    result := pt.SubNodes.Count;
end;

function SubNode(pt: apNode; n: int): apNode;

begin
  result := nil;
  if (pt <> nil) and (pt.SubNodes <> nil) and (n < pt.SubNodes.Count) then
  begin
    result := pt.SubNodes[n];
  end;
end;

function FindRoot(pt: apNode): apNode;

begin
  result := pt;
  if result = nil then
    exit;

  while result.Back <> nil do
  begin
    result := result.Back;
  end;
end;

function FindName(pt: apNode; nam: string): apNode;

var // searches pt subtags for nam - only single level search
  ps: apNode;
  x, i: integer;
begin
  result := nil;
  if (pt <> nil) and (nam <> '') then
  begin // and ( pt.SubNodes <> nil )
    if nam[1] = SubNodeBracket then
    begin // [n]
      x := 2;
      i := GetInt(nam, x);
      if // ( i >= 0 ) and ( i < pt.SubNodes.Count ) and
        (nam[x] = SubNodeEndBracket) then
      begin
        result := SubNode(pt, i); // pt.SubNodes[ i ];      // step in
      end
      else
        result := nil;
    end
    else if nam[1] = chIndirect then
    begin // ^
      result := FollowPath(NodeContent(pt), FindRoot(pt));
    end
    else if pt.Table <> nil then
    begin
      result := pt.Table.Find(nam);
    end
    else
    begin
      if pt.SubNodes <> nil then
      begin
        for x := 0 to pt.SubNodes.Count - 1 do
        begin
          ps := pt.SubNodes[x];
          if ps.NodeName = nam then
          begin
            result := ps;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function Back(pt: apNode; n: int = 1): apNode;

begin
  result := pt;
  if result <> nil then
  begin
    while n > 0 do
    begin
      result := result.Back;
      if result = nil then
        break;
      Dec(n);
    end;
  end;
end;

function ReadContent(pt: apNode; const name: string): string;

begin
  result := '';
  pt := FindName(pt, name);
  if pt <> nil then
    result := pt.Content;
end;

function ReadFloat(pt: apNode; const name: string; var ok: boolean): currency;

begin
  result := 0;
  ok := false;
  pt := FindName(pt, name);
  if pt <> nil then
  begin
    ok := TryCashToCurr(NodeContent(pt), result);
  end;
end;

function ReadInt(pt: apNode; const name: string; var ok: boolean): int;
  overload;

begin
  result := 0;
  ok := false;
  pt := FindName(pt, name);
  if pt <> nil then
  begin
    ok := TryStrToInt(NodeContent(pt), result);
  end;
end;

function ReadInt(pt: apNode; const name: string): int; overload;

begin
  result := 0;
  pt := FindName(pt, name);
  if pt <> nil then
  begin
    TryStrToInt(NodeContent(pt), result);
  end;
end;

function SplitOffAttributes(var nam: string): string;

var
  x: integer;
begin
  result := '';
  x := 1;
  while x <= Length(nam) do
  begin
    if nam[x] <= ' ' then
    begin
      result := RightStr(nam, Length(nam) - x);
      nam := LeftStr(nam, x - 1);
      Trim(result);
      break;
    end;
    Inc(x);
  end;
end;

function CreateHashTable(): cHashTable<apNode>;

begin
  result := cHashTable<apNode>.Create(
    function(pt: apNode): string
    begin
      result := pt.NodeName;
    end, // name match
    procedure(var pt: apNode)
    begin
      pt := nil;
    end, // clear slot
    function(pt: apNode): boolean
    begin
      result := pt = nil;
    end);
end;

constructor cDbTree.Create(treeID: word = 0; isKey: boolean = false;
Log: aLogProc = nil);

begin
  mUseCount := 1;
  Log := Log;
  mRootNode.NodeName := RootNodeName;
  mRootNode.ID := treeID; // debug and stats use
  if isKey then
  begin
    mRootNode.Table := CreateHashTable;
  end;
end;

procedure cDbTree.LogEr(er: int; const s: string);

begin
  if er > mEr then
    mEr := er;
  if Assigned(Log) then
    Log(er, s);
end;

procedure cDbTree.DisposeNodeFast(pt: apNode);

begin // does not unlink - ie ignores .Back - must unlink externally
  if pt.ID = 1 then
    Dec(mNodeCount);
  if pt.SubNodes <> nil then
    pt.SubNodes.Free;
  if pt.Table <> nil then
    pt.Table.Free;
  Dispose(pt); // FreeMem( pt );  need dispose to free strings
end;

procedure Unlink(pt: apNode);

var
  ppt: apNode;
begin
  if pt <> nil then
  begin
    ppt := pt.Back;
    if ppt <> nil then
    begin
      if ppt.SubNodes <> nil then
        ppt.SubNodes.Delete(ppt.SubNodes.IndexOf(pt)); // unlink
      if ppt.Table <> nil then
        ppt.Table.Delete(pt);
    end;
    pt.Back := nil;
  end;
end;

function NodeIndexParent(pt: apNode; out parent: apNode): int;

begin
  result := -1;
  parent := nil;
  if pt <> nil then
  begin
    parent := pt.Back;
    if parent <> nil then
    begin
      if parent.SubNodes <> nil then
        result := parent.SubNodes.IndexOf(pt);
    end;
  end;
end;

procedure NodeInsert(pt: apNode; base: apNode; index: int);
// assumes non key parent or already in table

begin
  if (pt <> nil) and (base <> nil) and (base.SubNodes <> nil) and (index >= 0)
    and (index < base.SubNodes.Count) then
  begin
    base.SubNodes.Insert(index, pt);
  end;
end;

procedure cDbTree.DisposeNode(pt: apNode); // and unlink

begin
  Unlink(pt);
  DisposeNodeFast(pt);
end;

function IDValue(n: string): int;

var
  x, val: int;
begin
  result := 0;
  val := 1;
  x := Length(n);
  while x > 0 do
  begin
    if not IsDecimal(n[x]) then
      break;
    result := result + ((Ord(n[x]) - Ord('0')) * val);
    Dec(x);
    val := val * 10;
  end;
end;

procedure cDbTree.IncAutoInc(parent: apNode);

begin
  if (parent <> nil) and (parent.AutoInc <> 0) then
  begin
    Inc(parent.AutoInc);
    if parent.AutoInc > MaxAutoInc then
    begin
      parent.AutoInc := 1;
      if mEr < stAutoIncWrapAround then
      begin
        LogEr(stAutoIncWrapAround, 'Wraparound on ' + parent.NodeName);
      end;
    end;
  end;
end;

{ procedure  cDbTree.AutoIncName( pt : apNode );

  begin
  if pt.Back.AutoInc > 0 then  begin
  if not IsDecimal( pt.NodeName[ Length( pt.NodeName ) ] ) then  begin  // if not already numbered
  pt.NodeName := pt.NodeName + IntToStr( pt.Back.AutoInc );
  IncAutoInc( pt );
  end;
  end;
  end; }

function cDbTree.AutoIncName(parent: apNode; const nam: string): string;

var
  n: int;
begin
  result := nam;
  if parent.AutoInc > 0 then
  begin
    if not IsDecimal(nam[Length(nam)]) then
    begin // if not already numbered
      result := nam + IntToStr(parent.AutoInc);
      n := 0;
      repeat // step until we find an empty one
        if FindName(parent, result) = nil then
          break;
        IncAutoInc(parent);
        result := nam + IntToStr(parent.AutoInc);
        Inc(n);
      until n > 1000;
    end;
  end;
end;

function cDbTree.CanAdd(const nam: string; parent: apNode): boolean;

begin
  result := false;
  if (parent <> nil) then
  begin
    result := true;
    if parent.Table <> nil then
    begin
      if FindName(parent, nam) <> nil then
      begin // collides
        result := false;
        if mEr < erNonUniqueNewKeyNode then
        begin
          LogEr(erNonUniqueNewKeyNode, 'Non Unique NewNode( ' + nam +
            ' ) on a Key tag');
        end;
      end
    end;
  end;
end;

procedure cDbTree.LinkNode(pt, parent: apNode);

begin
  pt.Back := parent;
  pt.ID := parent.ID; // set all tags in tree to same ID
  if parent.SubNodes = nil then
    parent.SubNodes := TList.Create; // forward link
  parent.SubNodes.Add(pt);
  if parent.Table <> nil then
    parent.Table.Add(pt);
  Inc(mNodeCount); // if pt.ID = 1 then    // wrong for AddNode
  pt.Modified := Seconds;
end;

function cDbTree.NewNode(const nam: string; key: boolean; parent: apNode;
AutoInc: int = 0): apNode; // and add to list

var
  pNode: apNode;
  name: string;
begin
  if parent = nil then
    parent := GetRoot;
  name := AutoIncName(parent, nam);
  result := nil;
  if CanAdd(name, parent) then
  begin
    New(pNode);
    pNode.NodeName := name;
    if name <> nam then
      IncAutoInc(parent);

    if key then
      pNode.Table := CreateHashTable
    else
      pNode.Table := nil;
    pNode.AutoInc := AutoInc;
    pNode.SubNodes := nil;
    pNode.UsrObj := nil;
    LinkNode(pNode, parent);
    result := pNode;
  end;
end;

function cDbTree.NewNode(const name, attribs: string; parent: apNode): apNode;

var
  p, auto: int;
begin
  p := Pos(Attrib_AutoInc, attribs);
  auto := 0;
  if p > 0 then
  begin
    p := p + Length(Attrib_AutoInc);
    auto := GetInt(attribs, p);
  end;
  result := NewNode(name, Pos(Attrib_Key, attribs) > 0, parent, auto);
end;

function cDbTree.CopyNode(const ps, pd: apNode): apNode;

begin
  result := NewNode(ps.NodeName, ps.Table <> nil, pd, ps.AutoInc);
  result.Content := ps.Content;
end;

function cDbTree.Rename(pt: apNode; const name: string): boolean;

var
  pParent: apNode;
begin
  result := false;
  pParent := pt.Back;
  if (pParent <> nil) and (CanAdd(name, pParent)) then
  begin
    if pParent.Table <> nil then
      pParent.Table.Delete(pt);
    pt.NodeName := name;
    if pParent.Table <> nil then
      pParent.Table.Add(pt);
    result := true;
  end;
end;

procedure cDbTree.MoveSubs(ps, pd: apNode);

var
  pt: apNode;
  doit: bool;
begin
  if (ps <> nil) and (ps.SubNodes <> nil) and (pd <> nil) then
  begin
    for pt in ps.SubNodes do
    begin
      doit := true;
      if Assigned(pd.Table) then
      begin // already on a key tag ?
        if FindName(pd, pt.NodeName) <> nil then
          doit := false;
      end;
      if doit then
      begin
        Unlink(pt);
        AddNode(pt, pd);
      end;
    end;
  end;
end;

procedure cDbTree.CopySubs(const ps: apNode; pd: apNode = nil);

var
  ps1: apNode;
  x: int;
begin
  if pd = nil then
    pd := GetRoot;

  if (ps <> nil) and (pd <> nil) then
  begin
    x := -1;
    while EachSubNode(ps, x, ps1) do
      CopyBranch(ps1, pd);
  end;
end;

procedure cDbTree.CopyBranch(const ps: apNode; pd: apNode = nil);

var
  pd1: apNode;
begin
  if pd = nil then
    pd := GetRoot;

  if (ps <> nil) and (pd <> nil) then
  begin
    pd1 := CopyNode(ps, pd);
    CopySubs(ps, pd1);
  end;
end;

function FollowPath(pathl: TStringList; pt: apNode; Back: integer = 0): apNode;

var // Follows the pathl list from pt
  n: integer;
begin
  result := nil;
  if pathl <> nil then
  begin
    result := pt;
    n := 0;
    while (n < pathl.Count - Back) and (result <> nil) do
    begin
      result := FindName(result, pathl[n]); // step in
      Inc(n) // next tag depth
    end;
  end;
end;

function FollowPath(path: string; pt: apNode): apNode; // overload;

var
  pl: TStringList;
begin
  pl := StrToPath(path);
  result := FollowPath(pl, pt);
  pl.Free;
end;

function cDbTree.HasContent(const path: string; out value: string): boolean;

var
  pt: apNode;
begin
  result := false;
  pt := FollowPath(path, @mRootNode);
  if pt <> nil then
  begin
    value := pt.Content;
    result := true;
  end;
end;

function cDbTree.SetNode(pn: apNode; var NodeName: string;
const val: string): apNode;

var
  pt: apNode;
begin
  if pn = nil then
    pn := GetRoot;
  pt := FindName(pn, NodeName);
  if pt = nil then
  begin // need to create one
    pt := NewNode(NodeName, '', pn);
    NodeName := pt.NodeName; // could be a numbered auto inc
  end;
  if pt <> nil then
    pt.Content := val;
  result := pt;
end;

function cDbTree.SetFlag(pn: apNode; const flag: string;
state: boolean): apNode;

begin // new or delete subnode acording to state
  result := nil;
  if pn <> nil then
  begin
    result := FindName(pn, flag);
    if state then
    begin
      if result = nil then
        result := NewNode(flag, false, pn);
    end
    else if result <> nil then
    begin
      DeleteAllFast(result);
      result := nil;
    end;
  end;
end;

function cDbTree.AddNode(pNode, parent: apNode): boolean;

begin
  result := false;
  if CanAdd(pNode.NodeName, parent) then
  begin
    LinkNode(pNode, parent);
    result := true;
  end;
end;

procedure cDbTree.MoveBranch(pt: apNode);

var
  pr: apNode;
  doit: bool;
begin
  if pt <> nil then
  begin
    doit := true;
    pr := GetRoot;
    if Assigned(pr.Table) then
    begin
      if FindName(pr, pt.NodeName) <> nil then
        doit := false;
    end;
    if doit then
    begin
      Unlink(pt);
      AddNode(pt, pr);
    end;
  end;
end;

procedure cDbTree.DeleteAllFast(pt: apNode; base: boolean = true);

var
  pd: apNode;
  x: integer;
begin // only unlinks the base tag
  if (pt <> nil) and (pt.SubNodes <> nil) then
  begin
    for x := pt.SubNodes.Count - 1 downto 0 do
    begin
      pd := pt.SubNodes[x];
      DeleteAllFast(pd, false);
    end;
    // if pt.SubNodes <> nil then  pt.SubNodes.Free;
    // if pt.Table <> nil then  pt.Table.Free;
  end;
  if pt <> nil then
  begin
    if base then
      DisposeNode(pt) // and unlink
    else
      DisposeNodeFast(pt);
  end;
end;

procedure cDbTree.CountNodes(pt: apNode; depth: integer);

begin
  Inc(mCount);
end;

function cDbTree.CountAllSubs(base: apNode): integer;

begin
  if base = nil then
    base := GetRoot;
  mCount := 0;
  Scan(base, CountNodes);
  result := mCount;
end;

{ procedure cDbTree.ClearNode( pNode : apNode );   // dispose of everything upstream

  var
  pt : apNode;  // the sub tags
  x : integer;
  begin
  if pNode <> nil then  begin
  if pNode.SubNodes <> nil then  begin
  for x := 0 to pNode.SubNodes.Count - 1 do  begin
  pt := pNode.SubNodes[ x ];
  if pt.SubNodes <> nil  then  begin
  ClearNode( pt );
  end;
  {$ifndef LEAK }{
  pt.SubNodes.Free;
  Dispose( pt );  // FreeMem( pt );  leaks the strings
  {$endif }{
  end;
  end;
  end;
  end;


  procedure  cDbTree.Claim;

  begin
  Inc( mUseCount );
  end; }

procedure cDbTree.Clear;

var
  pt: apNode;
begin
  if mRootNode.SubNodes <> nil then
  begin
    for pt in mRootNode.SubNodes do
      DeleteAllFast(pt);
    FreeAndNil(mRootNode.SubNodes);
    FreeAndNil(mRootNode.Table);
  end;
end;

destructor cDbTree.Destroy;

begin
  if self <> nil then
  begin
    Dec(mUseCount);
    if mUseCount = 0 then
    begin
      Clear;
      inherited Destroy;
    end;
  end;
end;

function cDbTree.GetRoot: apNode;

begin
  result := @mRootNode;
end;

function cDbTree.GetNode(const path: string): apNode; // overload;

begin
  if path <> '' then
    result := FollowPath(path, GetRoot)
  else
    result := nil;
end;

function cDbTree.GetNode(addr: TList<integer>): apNode; // overload;

var
  x: int;
begin
  result := GetRoot;
  for x in addr do
  begin
    result := SubNode(result, x);
    if result = nil then
      break;
  end;
end;

function PathToStr(path: TStringList): string;

var
  p: int;
begin
  result := '';
  if path <> nil then
  begin
    for p := 0 to path.Count - 1 do
    begin
      if p = 0 then
        result := LSep + path[0] + LSep
      else
        result := result + path[p] + LSep;
    end;
  end;
end;

function StrToPath(path: string): TStringList;

var // |Head|Subhead|  style list
  x: integer;
  tg, p: string;
begin
  result := TStringList.Create;
  x := 1;
  p := path;
  if p = '' then
    p := LSep;
  while x <= Length(p) do
  begin // skip initial whitespace and LSep
    if p[x] = LSep then
    begin
      Inc(x);
      break;
    end
    else if p[x] > ' ' then
      break;
    Inc(x);
  end;

  while x <= Length(p) do
  begin // build list
    if p[x] = LSep then
    begin
      result.Add(tg);
      tg := '';
    end
    else
      tg := tg + p[x];
    Inc(x);
  end;
  if tg <> '' then
    result.Add(tg); // in case there is no trailing LSep
end;

function MakeAList(const list: TStringList; sep: char = LSep): string;
// path style  |saf|sfd|

var
  x: integer;
begin
  if list <> nil then
  begin
    for x := 0 to list.Count - 1 do
    begin
      result := result + sep + list[x];
    end;
    result := result + sep;
  end
  else
    result := sep;
end;

function ParseList(const path: string): TStringList;

begin
  result := StrToPath(path);
end;

(*
  function   cDbTree.ResolvePath( pt : apNode ) : TStringList;   // makes a path list to pt

  var
  sl : TStringList;
  n : string;
  ppt : apNode;   // p parent tag
  pct : apNode;   // p contending match tag
  x, c : integer;
  begin
  sl := TStringList.Create;
  while pt.Back <> nil do  begin
  n := pt.NodeName;
  sl.Insert( 0, n );
  ppt := pt.Back;   // step back
  c := 1;           // count matching names to make a path#N entry
  for x := 0 to ppt.SubNodes.Count - 1 do begin
  if pt = ppt.SubNodes[ x ] then  break
  else begin
  pct := ppt.SubNodes[ x ];
  if n = pct.NodeName then  Inc( c );
  end;
  end;
  if c <> 1 then  begin
  n := n + '#' + IntToStr( c );   // todo deprecated
  sl[ 0 ] := n;
  end;
  pt := ppt;  // move to parent tag
  end;
  Result := sl;
  end; *)

function ResolvePath(pt: apNode): TStringList; // makes a path list to pt

var
  sl: TStringList;
begin
  sl := TStringList.Create;
  if pt <> nil then
  begin
    while pt.Back <> nil do
    begin
      sl.Insert(0, pt.NodeName);
      pt := pt.Back; // step back
    end;
  end;
  result := sl;
end;

function ResolvePathStr(pt: apNode): string;
// breaking change 28 apr 2011 - '|asdf|asdf|' form  was 'adsf|adf|adfs'

var
  pl: TStringList;
  t: int;
begin
  result := '';
  if pt <> nil then
  begin
    result := LSep;
    pl := ResolvePath(pt);
    for t := 0 to pl.Count - 1 do
    begin
      result := result + pl[t] + LSep;
    end;
    pl.Free;
  end;
end;


// function   ResolvePathStr( pt : apNode ) : string;     was  breaking change 28 apr 2011 -
//
// var
// pl : TStringList;
// t : int;
// begin
// result := '';
// if pt <> nil then  begin
// pl := ResolvePath( pt );
// for t := 0 to pl.Count - 1 do  begin
// result := result + pl[ t ];
// if t < pl.Count - 1 then  result := result + LSep;
// end;
// pl.Free;
// end;
// end;

function GetNodeNumerically(addr: TList<integer>; base: apNode): apNode;

var
  n: int;
begin
  result := base;
  for n in addr do
    result := SubNode(result, n);
end;

function SubNodeCount(addr: TList<integer>; base: apNode): int;

begin
  result := uDbTree.SubNodeCount(GetNodeNumerically(addr, base));
end;



// function	PathDifference( pRef, path : TStringList; out back : int ) : TStringList;  // from reference to path
//
// var
// x : int;
// begin
// x := 0;  back := 0;
// result := TStringList.Create;
// while true do  begin
// if ( x >= pRef.Count ) then  break;
// if ( x >= path.Count ) then  break;
// if pRef[ x ] <> path[ x ] then  break;
// Inc( x );
// end;
//
// if pRef.Count - 1 > x then  back := pRef.Count - x;
// while x < path.Count do  begin
// result.Add( path[ x ] );
// Inc( x );
// end;
// end;

function TrimPathToBase(base: asDbNode; var path: TStringList): boolean;

begin
  result := false;
  while (path.Count > 0) and (base <> path[0]) do
  begin
    path.Delete(0);
  end;
  if (path.Count > 0) and (base = path[0]) then
  begin // got a match
    path.Delete(0);
    result := true;
  end;
end;

function PathDif(fromPath, toPath: TStringList; var stepBack: int): TStringList;

// how should the chicken cross the road ?  back a few then up these steps - see Traverse below
var // assumes paths trimmed to a common base eg 'Timetable' and toPath may be flattened eg '[0]' from DS
  x, highestMatch: int;
begin
  x := 0;
  stepBack := 0;
  highestMatch := -1;
  result := TStringList.Create;
  if (toPath <> nil) and (fromPath <> nil) then
  begin
    while true do
    begin
      if x >= fromPath.Count then
        break;
      if x >= toPath.Count then
        break;
      if fromPath[x] = toPath[x] then
        highestMatch := x;
      if toPath[x] = '[0]' then
        highestMatch := x;
      Inc(x);
    end;

    stepBack := fromPath.Count - highestMatch - 1;
    x := highestMatch + 1;
    while x < toPath.Count do
    begin
      result.Add(toPath[x]);
      Inc(x);
    end;
  end;
end;

function Traverse(start: apNode; backStep: int; path: TStringList;
backAdjust: int = 0): apNode;

begin // handles up and down hill paths
  result := nil;
  if (start <> nil) and (path <> nil) then
  begin
    if path.Count <= backAdjust then
      result := Back(start, backStep + backAdjust - path.Count)
    else
    begin
      result := Back(start, backStep);
      result := FollowPath(path, result, backAdjust);
    end;
  end;
end;

function EachSubNode(const base: apNode; var x: int; out ps1: apNode): boolean;

begin
  result := false;
  Inc(x);
  if (base <> nil) and (base.SubNodes <> nil) and (x < base.SubNodes.Count) then
  begin
    ps1 := base.SubNodes[x];
    result := true;
  end;
end;

function UniqueTagName(base: apNode; const SubName: string): string;

var // make a unique tag name of the form  SubNameNNN - not multi user safe
  ip: apNode;
  n: int;
begin
  result := '';
  if base <> nil then
  begin
    ip := SubNode(base, SubNodeCount(base) - 1); // try most recent sub + 1
    n := AnyDecimal(NodeName(ip));
    repeat
      Inc(n);
      result := SubName + IntToStr(n);
    until FindName(base, result) = nil;
  end;
end;

function LoadContentList(const pcl: apNode): TStringList;
// creates a list caller must free

var
  i: int;
begin
  result := nil;
  if (pcl <> nil) and (pcl.Content <> '') then
  begin
    i := 1;
    result := BuildParamsL(pcl.Content, i);
  end;
end;

function PathCoincides(p1, p2: TStringList): boolean;
// is a change to p1 relevant to p2

var
  n: int;
begin
  result := true;
  if (p1 <> nil) and (p2 <> nil) then
  begin
    n := 0;
    repeat
      if n >= p1.Count then
        break; // change here affects p2
      if n >= p2.Count then
        break;
      if p1[n] <> p2[n] then
      begin // paths have diverged
        result := false;
        break;
      end;
      Inc(n);
    until false;
  end
  else
    result := false;
end;

function cDbTree.ReadSub(pt: apNode; n: integer; var name: string;
var Content: string): apNode;

begin
  if pt = nil then
    pt := @mRootNode;
  if (pt.SubNodes <> nil) and (pt.SubNodes.Count >= n) then
  begin
    pt := pt.SubNodes[n];
    name := pt.NodeName;
    Content := pt.Content;
  end;
  result := pt;
end;

function cDbTree.ReadNode(pt: apNode; var name: string;
var Content: string): integer;

begin
  result := 0;
  if pt = nil then
    pt := @mRootNode;
  name := pt.NodeName;
  Content := pt.Content;
  if pt.SubNodes <> nil then
    result := pt.SubNodes.Count;
end;

{ function   cDbTree.OpenNode( path : string; pt : apNode = nil ) : apNode;

  var
  pl : TStringList;
  begin
  pl := ParseList( path );
  Result := FollowPath( pl, pt );
  pl.Free;
  end; }

function FindSubNode(pt: apNode; const tn: string): apNode;
// multi level search from pt to NodeName tn

var
  pn: apNode;
  x: integer;
begin
  result := nil;
  if (pt <> nil) and (pt.SubNodes <> nil) then
  begin
    for x := 0 to pt.SubNodes.Count - 1 do
    begin
      pn := pt.SubNodes[x];
      result := FindName(pn, tn);
      if result <> nil then
        break;
      result := FindSubNode(pn, tn);
      if result <> nil then
        break;
    end;
  end;
end;

function cDbTree.FindNodeMulti(pt: apNode; const tn: string): apNode;

begin
  if pt = nil then
    pt := @mRootNode;
  if pt.NodeName = tn then
    result := pt
  else
  begin
    result := FindName(pt, tn);
    if result = nil then
      result := FindSubNode(pt, tn);
  end;
end;

function cDbTree.ListNames(const path: string; pt: apNode = nil): TStringList;

var // returns a TSlist from sub node names
  n: int;
  pn: apNode;
begin
  result := nil;
  if pt = nil then
    pt := @mRootNode;
  pt := FollowPath(path, pt);
  if pt <> nil then
  begin
    result := TStringList.Create;
    if pt.SubNodes <> nil then
    begin
      for n := 0 to pt.SubNodes.Count - 1 do
      begin
        pn := pt.SubNodes[n];
        result.Add(pn.NodeName);
      end;
    end;
  end;
end;

function cDbTree.AddToList(pt: apNode; const name, SubName, SubCont: string;
unique: boolean = false): apNode;

var
  pf, pn: apNode;
  i: int;
  rep: boolean;
begin
  rep := false;
  result := nil;
  if pt <> nil then
  begin
    pf := FindName(pt, Name); // create list header if necessary
    if pf = nil then
      pf := NewNode(Name, false, pt); // not key - multiple <file> s
    if unique then
    begin
      if (pf <> nil) and (pf.SubNodes <> nil) then
      begin // search for unique content
        for i := 0 to pf.SubNodes.Count - 1 do
        begin
          pn := pf.SubNodes[i];
          if pn.NodeName = SubName then
          begin
            if pn.Content = SubCont then
            begin // not unique
              rep := true;
              break;
            end;
          end;
        end;
      end;
    end;

    if not rep and (pf <> nil) then
    begin
      result := NewNode(SubName, false, pf);
      result.Content := SubCont;
    end;
  end;
end;
// mLatestFile := mTree.AddToList( pt, z.files, 'file', fname, true );
// <z.files>
// <file> C:\PHOTO\JPG 1990s\1996\96 APR MT rats zoo\1996 APR 08.jpg </file>
// <file> C:\PHOTO\JPG 1990s\1996\96 APR MT rats zoo\1996 APR 05.jpg </file>
// <file> C:\PHOTO\JPG 1990s\1996\96 APR MT rats zoo\1996 APR 10.jpg </file>

procedure cDbTree.DelFromList(pt: apNode; const name, SubName, SubCont: string);

var
  pf, pn: apNode;
  i: int;
begin
  if pt <> nil then
  begin
    pf := FindName(pt, Name); // find list header
    if (pf <> nil) and (pf.SubNodes <> nil) then
    begin
      for i := 0 to pf.SubNodes.Count - 1 do
      begin // search for correct item
        pn := pf.SubNodes[i];
        if (pn.NodeName = SubName) and (pn.Content = SubCont) then
        begin
          DeleteAllFast(pn);
          break;
        end;
      end;
    end;
  end;
end;

function cDbTree.FollowPath_(pathl: TStringList; pt: apNode = nil): apNode;

var // Follows the pathl list from root
  n: integer;
  nam: string;
begin
  n := 0;
  result := nil;
  if pathl <> nil then
  begin
    if pt = nil then
      result := @mRootNode
    else
      result := pt;
    while n < pathl.Count do
    begin
      // Result := nil;
      nam := pathl[n];
      if nam <> '' then
        result := FindName(result, nam) // step in
      else
        result := nil;

      if result <> nil then
        Inc(n) // next tag depth
      else
        break; // doesn't exist
    end;
  end;
end;

function cDbTree.FollowPath_(path: string; pt: apNode = nil): apNode;
// overload;

var
  pl: TStringList;
begin
  pl := StrToPath(path);
  result := FollowPath_(pl, pt);
  pl.Free;
end;

function cDbTree.ForcePath(pathl: TStringList): apNode; // overload;

var // follows or creates pathl list
  pt, pn: apNode;
  n: integer;
  nam, atrib: string;
begin
  n := 0;
  pt := @mRootNode;
  while n < pathl.Count do
  begin
    nam := pathl[n];
    atrib := SplitOffAttributes(nam); // don't try to match attributes
    { if pt.SubNodes = nil  then   begin    // create subnode list as well as new tag
      // SplitPathName( nam );                // remove #
      // pn := NewNode( nam, Pos( Attrib_Key, atrib ) > 0, pt  );
      pn := DoBuildTag( nam, atrib, pt );

      end
      else  begin }
    pn := FindName(pt, nam);
    if pn = nil then
    begin // not found so create one
      // pn := NewNode( nam, Pos( Attrib_Key, atrib ) > 0, pt );
      pn := NewNode(nam, atrib, pt);
    end;
    // end;
    Inc(n);
    pt := pn;
    if pt = nil then
      break;
  end;
  result := pt;
end;

function cDbTree.ForcePath(path: asDbPath): apNode; // overload;

var
  pathl: TStringList;
begin
  pathl := StrToList(path);
  result := ForcePath(pathl);
  FreeAndNil(pathl);
end;

function cDbTree.ForceEditPath(pathl: TStringList): apNode;

var // follows or creates pathl list if not key
  pt, pn: apNode;
  n: integer;
  nam, atrib: string;
begin
  n := 0;
  if pathl.Count > 0 then
    pt := @mRootNode
  else
    pt := nil;
  while n < pathl.Count do
  begin
    nam := pathl[n];
    atrib := SplitOffAttributes(nam); // don't rty to match attributes
    pn := FindName(pt, nam);
    if pn = nil then
    begin // not found so create one
      if pt.Table <> nil then
      begin
        pt := nil;
        break;
      end;
      pn := NewNode(nam, atrib, pt);
    end;

    { if pt.SubNodes = nil  then   begin    // create subtag list as well as new tag
      if pt.Table <> nil then  begin
      pt := nil;
      break;
      end;
      nam := pathl[ n ];
      atrib := SplitOffAttributes( nam );  // don't try to match attributes
      //SplitPathName( nam );                // remove #
      pn := NewNode( nam, Pos( Attrib_Key, atrib ) > 0, pt );
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
      pn := NewNode( nam, Pos( Attrib_Key, atrib ) > 0, pt );
      end;
      end; }
    Inc(n);
    pt := pn;
    if pt = nil then
      break;
  end;
  result := pt;
end;

procedure DoScanProc(pt: apNode; depth: integer; proc: aScanProc);

var
  x: integer;
begin
  proc(pt, depth);
  if pt.SubNodes <> nil then
  begin
    for x := 0 to pt.SubNodes.Count - 1 do
    begin
      DoScanProc(pt.SubNodes[x], depth + 1, proc);
    end;
  end;
end;

function cDbTree.Scan(pt: apNode; proc: aScanProc): boolean;

var
  depth: integer;
begin
  result := true;
  if pt = nil then
  begin
    pt := GetRoot();
    result := false;
  end;
  if pt <> nil then
  begin
    depth := 0;
    DoScanProc(pt, depth, proc);
  end;
end;

procedure cDbTree.DoScan(pt: apNode; depth: integer; NextNode: poOnNextNode);

var
  x: integer;
begin
  NextNode(pt, depth);
  if pt.SubNodes <> nil then
  begin
    for x := 0 to pt.SubNodes.Count - 1 do
    begin
      DoScan(pt.SubNodes[x], depth + 1, NextNode);
    end;
  end;
end;

function cDbTree.Scan(path: TStringList; NextNode: poOnNextNode): boolean;

var
  pt: apNode;
  depth: integer;
begin
  result := true;

  if path <> nil then
    pt := FollowPath(path, GetRoot())
  else
    pt := GetRoot();
  if pt <> nil then
  begin
    if path <> nil then
      depth := path.Count
    else
      depth := 0;
    DoScan(pt, depth, NextNode);
  end
  else
    result := false;
end;

function cDbTree.Scan(pt: apNode; NextNode: poOnNextNode): boolean;

var
  x: integer;
begin
  result := true;
  if pt = nil then
    pt := GetRoot;
  if pt.SubNodes <> nil then
  begin
    for x := 0 to pt.SubNodes.Count - 1 do
    begin
      DoScan(pt.SubNodes[x], 1, NextNode);
    end;
  end;
end;

function DoSearch(pt: apNode; name, cont: string; index: int): apNode;

var
  i: integer;
begin
  result := nil;
  if ((cont = '') or (cont = pt.Content)) and
    ((name = '') or (name = pt.NodeName)) then
  begin
    Dec(index);
    if index <= 0 then
      result := pt;
  end;
  if (result = nil) and (pt.SubNodes <> nil) then
  begin
    for i := 0 to pt.SubNodes.Count - 1 do
    begin
      result := DoSearch(pt.SubNodes[i], name, cont, index);
      if result <> nil then
        break;
    end;
  end;
end;

function cDbTree.Search(pt: apNode; name, cont: string; index: int = 1): apNode;
// multi search for matching(s)

var
  x: integer;
begin
  result := nil;
  if pt = nil then
    pt := GetRoot;
  if pt.SubNodes <> nil then
  begin
    for x := 0 to pt.SubNodes.Count - 1 do
    begin
      result := DoSearch(pt.SubNodes[x], name, cont, index);
      if result <> nil then
        break;
    end;
  end;
end;

function NamedContent(pt: apNode; const nam: string): string;

begin
  pt := FindName(pt, nam);
  if pt = nil then
    result := ''
  else
    result := pt.Content;
end;

function CompareNodes(pap, pbp: pointer): integer;

var
  pa, pb: apNode;
begin
  result := 0;
  pa := FollowPath(SortPath, pap);
  pb := FollowPath(SortPath, pbp);
  if (pa <> nil) and (pb <> nil) then
  begin
    result := CompareText(pa.Content, pb.Content);
  end;
end;

procedure cDbTree.Sort(pt: apNode; pathl: TStringList);

var // pathl is a relative path to the field to sort by
  x, y: int;
  pta, ptb: apNode;
  sl: TList; // of apNode
  done: boolean;
  nam: string;
begin
  if pt = nil then
    pt := @mRootNode;
  if (pathl <> nil) then
    if pathl.Count > 0 then
    begin
      if pathl.Count = 1 then
      begin // simpler faster case
        sl := TList.Create;
        nam := pathl[0];
        if pt.SubNodes <> nil then
          for x := 0 to pt.SubNodes.Count - 1 do
          begin
            pta := pt.SubNodes[x];
            done := false;
            for y := 0 to sl.Count - 1 do
            begin // slip it into the right spot  todo index or halve interval
              ptb := sl[y];
              if NamedContent(ptb, nam) > NamedContent(pta, nam) then
              begin // insert before
                sl.Insert(y, pta);
                done := true;
                break;
              end;
            end; // for y
            if not done then
              sl.Add(pta);
          end; // for x
        pt.SubNodes.Free;
        pt.SubNodes := sl; // use sorted list now
      end

      else if pathl.Count > 1 then
      begin
        if pt.SubNodes <> nil then
        begin
          SortPath := pathl;
          pt.SubNodes.Sort(CompareNodes);
          SortPath := nil;
        end;
      end;
    end;
end;

procedure cDbTree.QuickSort(pt: apNode; L, R: integer;
compare: apSortCompareMethod);

var // pinched from Classes TList
  i, J: integer;
  p, t: apNode;
begin
  if L < R then
  begin
    repeat
      // L := 0;
      // R := SubNodeCount( pt );
      i := L;
      J := R;
      p := SubNode(pt, (L + R) shr 1);
      repeat
        while compare(SubNode(pt, i), p) < 0 do
          Inc(i);
        while compare(SubNode(pt, J), p) > 0 do
          Dec(J);
        if i <= J then
        begin
          if i <> J then
          begin // swap out of order pair
            t := SubNode(pt, i);
            pt.SubNodes[i] := SubNode(pt, J);
            pt.SubNodes[J] := t;
          end;
          Inc(i);
          Dec(J);
        end;
      until i > J;
      if L < J then
        QuickSort(pt, L, J, compare);
      L := i;
    until i >= R;
  end;
end;

end.
