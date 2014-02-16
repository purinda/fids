unit uGlobalDefs;

// (C) ALphasoft Pty Ltd

interface

uses
	SysUtils, Messages, uGT;

const

	Attrib_Key = 'KeyTag="true"'; // SPECIAL XML ATTRIBUTES
	Attrib_AutoInc = 'AutoInc="'; // eg <Departures KeyTag="true" AutoInc="765">
	TagServerName = 'Server.';
	// eg <Server.2>  used as tick message every 5 seconds and DB name
	TagTick = 'Tick';
	// data request tags
	TagDataRequest = 'DataRequest';
	TagEditRequest = 'EditRequest';
	TagDataReply = 'DataReply';
	TagEditReply = 'EditReply';
	MesgEditReply = '<' + TagEditReply + '>';
	TagResult = 'Result';
	TagLoginRequest = 'LoginRequest';
	TagLoginReply = 'LoginReply';
	MesgLogin = '<' + TagLoginReply + '>';
	TagAcess = 'Access';
	TagReqID = 'ReqID';
	TagShutDown = 'ShutDown';
	MesgShutDown = '<' + TagShutDown + '/>';

	TagRead = 'ReqRead'; // TagRead     single level
	TagScan = 'ReqScan';
	TagSearch = 'ReqSearch'; // base path;  interesting subtags<=>value;

	TagRenameTag = 'ReqRenameTag';
	TagDelete = 'ReqDelete';
	TagNewTag = 'ReqNewTag';
	TagReplaceTag = 'ReplaceTag';
	TagEdit = 'ReqEdit';

	TagPath = 'Path'; // eg <Path>   |List|Item#23|Price|
	TagList = 'TagList'; // limit response to tags in list
	TagMatchTag = 'MatchTag'; // eg <Match>  SubTagName|contents|
	TagMatchCont = 'MatchCont'; // eg <Match>  SubTagName|contents|

	TagPassWord = 'PassWord';
	TagUserName = 'UserName';
	TagLogout = 'LogOut';
	TagConnect = 'Connect';

	LSep = '|';
	// tree path and list separator                 // SPECIAL AS CHARS in XML content
	SubNodeBracket = '[';
	SubNodeEndBracket = ']';
	BackPrefix = '-';
	chIndirect = '^';
	FSep = '\'; // windows file system separator

	EOL = #13 + #10;

type
	// int = integer;   // typing shortcut

	aConnectionEvent = (ceNone, ceConnected, ceDbReady, ceLogin, ceEdit,
	  ceDisconnected, ceShutdown);
	apConnectionEventReader = procedure(event: aConnectionEvent; param: string)
	  of object;

	aLogProc = procedure(ErrorNo: integer; const s: string) of object;
	aBytes = TBytes; // array of byte;
	// aConnectionEvent = ( ceNone, ceConnected, ceDbReady, ceLogin, ceEdit, ceDisconnected, ceShutdown );
	// apConnectionEventReader = procedure( event : aConnectionEvent; param : string ) of object;

const // error severity and id
	erNone = 0;
	erStatus = 1;
	stTCPevent = 2;
	stLogin = 3;
	stServerEvent = 4;
	stStartup = 5;
	stAttemptConnection = 6;
	stConnectionFailed = 7;
	stConnectionSucceded = 8;
	stNoConToServer = 9;
	stServerActive = 10;
	stLogOut = 11;
	stUDPRequest = 12;
	stUDPReply = 13;
	stUDPServer = 14;
	stAutoIncWrapAround = 15;
	stConnectionLost = 16;

	erWarning = 100;
	erRenToNonuniqueKeyNode = 101;
	erRenToEmpty = 102;
	erRenNonExistent = 103;
	erDelNonExistent = 104;
	erEditMismatch = 105;
	erEditNonExistent = 106;
	wnLogin = 107;
	wnLostServerEvent = 108;
	erInvalidTimeDate = 109;
	erCreatedParserNilTree = 110;
	erFailedToOpenUdpPort = 110;

	erForcedTCPReconfiguration = 120;

	erInvalidFeedField = 130;

	erSevere = 200;
	erUndefinedDataOp = 201;
	erUndefinedEditOp = 202;
	erUnmatchedXMLEndTag = 203;
	erInvalidXML = 204;
	erLoadXMLfailure = 205;
	erLoadStreamFailure = 206;
	erFormatName = 207;
	erInvalidIP = 208;
	erNonUniqueNewKeyNode = 209;
	erAttemptToCreateNonuniqueKeyNode = 210;
	erRevertingToBackupFile = 211;
	erInvalidNewNodeNameOnAutoInc = 212;
	erBadReqNew = 213;
	erInvalidDataFormat = 214;
	erXmlFileNotFound = 215;
	erUserNotLoggedIn = 216;

	erFatal = 300;
	erFailedToLoadBackupFile = 301;
	erConnectionTimeOut = 302;

	// var
	// gThreadCount : int = 0;       // global thread control stuff
	// gShutdown : boolean = false;

	{ WM_TCP = WM_USER + $123;    // $$$
	  WM_UDP = WM_USER + $124;
	  WM_Pipe = WM_USER + $125; }
	WM_Tick = WM_USER + $126;
	WM_Hub = WM_USER + $927;

function FormatDay(day: int = 0): string;
// function  CreateFlightKey( const flightName : string;  day : int = 0 ) : string;

implementation

function FormatInt(val, digits: int): string;

begin
	result := IntToStr(val);
	while Length(result) < digits do
		Insert('0', result, 1);
end;

function FormatDay(day: int = 0): string;

var
	yr, mo, dy, dow: word;
begin
	if day = 0 then
		day := Trunc(Now());
	DecodeDateFully(day, yr, mo, dy, dow);
	result := FormatInt(yr, 4) + FormatInt(mo, 2) + FormatInt(dy, 2);

end;

end.
