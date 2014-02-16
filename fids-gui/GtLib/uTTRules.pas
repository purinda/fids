unit uTTRules;

{
  The sole edit and verify unit for reading/writing the <TimeTable> part of data tree
}

interface

uses
	Classes, Generics.Collections, Generics.Defaults,
	uFidsTags, uGlobalDefs, uMirrorDB, uDBTree, uGT, uFlight;

type
	// see also field sets below
	aTimeTableField = (tfNone, tfRuleName, tfRules, tfPath, tfTemplate,
	  tfDateStart, tfDateEnd, tfTime, tfDays, tfDaysExcept, tfDateException);

	aTTValidationError = (teNone, teNoRuleName, teNonuniqueRuleName,
	  teInvalidTime, teInvalidFlight, teAttemptToChangeRuleName,
	  teTimeMustBeNumeric, teTimeMinutesMustBeLessThan60,
	  teTimeHoursMustBeLessThan24);

const // field attribute sets
	PrimaryFields = [tfRules, tfPath, tfTemplate];
	RulesSubField = [tfDateStart .. tfDateException];
	ListField = [tfDateException];
	DateFields = [tfDateStart, tfDateEnd];
	TimeFields = [tfTime];
	HexSetField = [tfDays, tfDaysExcept];

type
	cTTRule = class(TInterfacedObject, IComparer<apNode>)
		constructor Create(db: cMirrorDB { or nil }; ReqID: string);
	private
		mDB: cMirrorDB; // copy of db pointer
		mReqID: string;
		mDbNode: apNode; // or nil - current db flight name node
		mLog: aLogProc; // reporting
		mVals: array [aTimeTableField] of string; // for new flight building
		// mEr : int;
		function MakeValid(field: aTimeTableField): boolean;
		function GetField(field: aTimeTableField): string;
		procedure SetField(field: aTimeTableField; val: string);
		function GetDBTagName(field: aTimeTableField): string;
		function GetTitleField(field: aTimeTableField): string;
		function GetRawField(field: aTimeTableField): string;
		procedure SetDbNode(node: apNode);
		function GetDbPath(): string;
		procedure SetDbPath(path: string);
	public
		oTemplate: cFlight; // handle with care - todo re-export properties ?
		Error: aTTValidationError;
		property db: cMirrorDB read mDB;
		property ReqID: string read mReqID;

		function Compare(const pfltA, pfltB: apNode): int;
		// IComparer<apNode>  ie can compare rules for sort
		procedure Delete;
		function New: boolean;

		property Log: aLogProc read mLog write mLog;
		property Presentation[field: aTimeTableField]: string read GetField
		  write SetField; default; // db Update
		property DBTag[field: aTimeTableField]: string read GetDBTagName;
		// use var	DBTag [ field ]  below
		property Title[field: aTimeTableField]: string read GetTitleField;
		property Raw[field: aTimeTableField]: string read GetRawField;

		property DbNode: apNode read mDbNode write SetDbNode;
		// eg |Timetable|<GT4444>| ie rule name base tag
		property DbPath: string read GetDbPath write SetDbPath;

		// property   Error : int  read mEr  write mEr;
		class var TTTagName: array [aTimeTableField] of string;
	end;

	// aTTRuleRecord = record   could embed path string into list if Build is decoupled from NewData
	// node : apNode;
	// key  : string;
	// end;

	cTTRulesList = class(TList<apNode>) // < aTTRuleRecord >
		constructor Create(db: cMirrorDB);
		destructor Destroy; override;
	private
		mDB: cMirrorDB; // copy of db pointer
	public
		oRules: cTTRule;
		procedure Build(field: aTimeTableField; const val: string);
		// build a list now where field = val
		procedure Sort(const field: aTimeTableField; const ff: aFlightField;
		  ascend: boolean);
		function GetEnumerator: TEnumerator<apNode>;
		// supports compiler  'for pFlt in FlightList do'

	end;

implementation
// _______________________________________________________________________________

uses SysUtils, uUtils, uXmlParser;

constructor cTTRule.Create(db: cMirrorDB { or nil }; ReqID: string);

begin // make a template flight
	oTemplate := cFlight.Create(db, ReqID);
	mDB := db;
	mReqID := ReqID; // logged in ID needed for global updates
end;

function cTTRule.MakeValid(field: aTimeTableField): boolean;

var
	x, v: int;
begin // time field validator
	result := true; // todo - other fields - separate MakeValidTime function
	if field in TimeFields then
	begin
		result := false;
		if Length(mVals[field]) = 3 then
			Insert('0', mVals[field], 1);
		x := 1;
		v := Decimal(mVals[field], x);
		if x <> 5 then
			Error := teTimeMustBeNumeric
		else if v mod 100 >= 60 then
			Error := teTimeMinutesMustBeLessThan60
		else if v > 2359 then
			Error := teTimeHoursMustBeLessThan24
		else
			result := true;
	end;
end;

function cTTRule.GetField(field: aTimeTableField): string;

var
	pRules: apNode;
	s: string;
begin // pull data presentation grade strings from local db tree
	result := '';
	if field = tfRuleName then
		result := NodeName(mDbNode)
	else if field in [tfTime] then
	begin
		pRules := FindName(mDbNode, TTTagName[tfRules]);
		s := ReadContent(pRules, TTTagName[field]);
		if Length(s) >= 15 then
			result := Copy(s, 10, 4) // can be 'TBA' etc
		else
			result := s;
	end
	else if field in [tfDateStart, tfDateEnd] then
	begin
		pRules := FindName(mDbNode, TTTagName[tfRules]);
		s := ReadContent(pRules, TTTagName[field]);
		if Length(s) >= 15 then
		begin
			result := DbDateToStr(s);
			// result := IntToStr( Copy( s, 7, 2 ) ) + '/' + IntToStr( Copy( s, 1, 8 ) + '/'Copy( s, 1, 8 ) + '/';   // can be 'TBA' etc
			// Insert( '/', result, 7 );
			// Insert( '/', result, 5 );
		end;
	end
	else if field in PrimaryFields then
		result := ReadContent(mDbNode, TTTagName[field]) // path
	else if field in RulesSubField then
	begin
		pRules := FindName(mDbNode, TTTagName[tfRules]);
		result := ReadContent(pRules, TTTagName[field]);
	end;
end;

procedure cTTRule.SetField(field: aTimeTableField; val: string);

var
	pRules: apNode;
begin // push data into local field values and maybe do global upates
	mVals[field] := val;
	if MakeValid(field) then
	begin
		if mDbNode <> nil then
		begin // not building a new rule so do live updates
			if mVals[field] <> Presentation[field] then
			begin // changed
				if field in PrimaryFields then
				begin
					mDB.GlobalEdit(mDbNode, TTTagName[field],
					  mVals[field], mReqID);
				end
				else if field in RulesSubField then
				begin
					pRules := FindName(mDbNode, TTTagName[tfRules]);
					mDB.GlobalEdit(pRules, TTTagName[field],
					  mVals[field], mReqID);
				end
				else if field = tfRuleName then
					Error := teAttemptToChangeRuleName;
			end;
		end;
	end;
end;

function cTTRule.GetDBTagName(field: aTimeTableField): string;

begin // return tag name corresponding to field number
	result := TTTagName[field];
end;

function cTTRule.GetTitleField(field: aTimeTableField): string;

begin // return a suitable title for the field number
	result := TTTagName[field];
end;

function cTTRule.GetRawField(field: aTimeTableField): string;

begin // pull data raw strings from local db tree
	result := Presentation[field];
end;

procedure cTTRule.SetDbNode(node: apNode);

var
	pt: apNode;
begin // connect rule object with a db tree rule
	mDbNode := node;
	pt := FindName(node, TTTagName[tfTemplate]);
	// set template to rule's primary flight
	pt := FindName(pt, tagFlights);
	oTemplate.DbNode := SubNode(pt, 0); // primary flight name
end;

function cTTRule.GetDbPath(): string;

begin // return path of current rule
	result := ResolvePathStr(mDbNode);
end;

procedure cTTRule.SetDbPath(path: string);

begin // connect rule object with a db tree rule
	DbNode := FollowPath(path, mDB.GetRoot);
end;

function cTTRule.Compare(const pfltA, pfltB: apNode): int;
// IComparer<apNode>  ie can compare rules for sort

begin // future sorting stub
	result := 0;
end;

procedure cTTRule.Delete;

var
	r: string;
begin // delete current rule object from global db tree
	if mDbNode <> nil then
	begin
		r := FormatDelete(DbPath, mReqID);
		mDB.Broadcast(r);
		mDbNode := nil;
	end;
end;

function cTTRule.New: boolean;

// generate a complete rule into the global db tree
var
	pRulekey, prim, sec: apNode;
	path, name, r: string;
	ruleBranch: cDbTree;
	field, f: aTimeTableField;
label
	DontBother;
begin
	result := false;
	ruleBranch := nil;
	name := mVals[tfRuleName];
	if name = '' then
	begin
		Error := teNoRuleName;
		goto DontBother;
	end;
	if not MakeValid(tfTime) then
	begin
		Error := teInvalidTime;
		goto DontBother;
	end;
	if FollowPath(tagTimeTable + '|' + name, mDB.GetRoot) <> nil then
	begin
		Error := teNonuniqueRuleName;
		goto DontBother;
	end;

	ruleBranch := cDbTree.Create; // build tree to suit new rule
	pRulekey := ruleBranch.NewNode(name, false, nil); // rule name
	for field := Succ( Low(aTimeTableField)) to High(aTimeTableField) do
	begin
		if field in PrimaryFields then
		begin
			if not MakeValid(field) then
				goto DontBother;
		end;
		prim := ruleBranch.NewNode(TTTagName[field], false, pRulekey);
		// add <Rules>, <Path>, <Template>
		prim.Content := mVals[field];
		case field of
			tfRules:
				begin
					for f := Succ( Low(aTimeTableField))
					  to High(aTimeTableField) do
					begin
						if f in RulesSubField then
						begin // <DateStart> etc
							if not MakeValid(f) then
								goto DontBother;
							sec := ruleBranch.NewNode(TTTagName[f],
							  false, prim);
							sec.Content := mVals[f];
						end
					end;
				end;
			tfTemplate:
				begin
					if not oTemplate.AddFlightToTree(ruleBranch, prim,
					  StripTail(name)) then
					begin
						Error := teInvalidFlight;
						goto DontBother;
					end;
				end;
		end;
	end;
	path := tagTimeTable;
	r := StartRequestNew(pRulekey.NodeName);
	r := AddToRequestNew(r, FormatAllSubNodes(pRulekey, 2));
	mDB.Broadcast(EndRequestNew(r, path, '', '', mReqID));
	result := true;

DontBother:
	FreeAndNil(ruleBranch);
end;





// _________________________________________________________________________________

constructor cTTRulesList.Create(db: cMirrorDB);

begin
	inherited Create;
	mDB := db;
	oRules := cTTRule.Create(db, '');
end;

procedure cTTRulesList.Build(field: aTimeTableField; const val: string);

var
	x: int;
	ptt, ptr: apNode;
begin // make a list of rules from the local db tree TimeTable
	Clear;
	ptt := FindName(mDB.GetRoot, tagTimeTable);
	x := -1;
	while EachSubNode(ptt, x, ptr) do
	begin // each rule
		// oRules.DbNode := ptr;  // connect to a tt rule
		// if oRules.Match( field, val ) then      todo ?
		Add(ptr);
	end;
end;

destructor cTTRulesList.Destroy; // override;

begin
	oRules.Free;
end;

procedure cTTRulesList.Sort(const field: aTimeTableField;
  const ff: aFlightField; ascend: boolean);

begin

end;

function cTTRulesList.GetEnumerator: TEnumerator<apNode>;
// supports compiler  'for pFlt in FlightList do'

begin
	result := nil;
end;

var
	field: aTimeTableField;

initialization

// generate tag names from enum
for field := Succ( Low(aTimeTableField)) to High(aTimeTableField) do
begin
	cTTRule.TTTagName[field] := EnumToStr(Ord(field),
	  System.TypeInfo(aTimeTableField), false); // was FieldName
	cTTRule.TTTagName[field] := UnCamel(cTTRule.TTTagName[field], #0);
	// no spaces
end;

end.
