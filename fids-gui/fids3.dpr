program fids3;

uses
  Forms,
  FWindow in 'FWindow.pas' {frmWindow},
  FSearch in 'FSearch.pas' {frmSearch},
  FEditAnD in 'FEditAnD.pas' {frmEditAnD},
  FMain in 'FMain.pas' {frmMain},
  ufRuleEdit in 'ufRuleEdit.pas' {fRuleEdit},
  FLogin in 'FLogin.pas' {frmLogin},
  FIndicators in 'FIndicators.pas' {frmManageIndicators},
  FEdit in 'FEdit.pas' {frmEdit},
  FCrawlineLines in 'FCrawlineLines.pas' {FCrawlineLinesAllocator},
  CrawlingEdit in 'CrawlingEdit.pas' {FCrawlingLineEdit},
  ASCII in 'GtLib\ASCII.pas',
  uConnection in 'GtLib\uConnection.pas',
  uDbTree in 'GtLib\uDbTree.pas',
  uFidsTags in 'GtLib\uFidsTags.pas',
  uFlight in 'GtLib\uFlight.pas',
  ufStringEntry in 'GtLib\ufStringEntry.pas' {fStrEntry},
  uGlobalDefs in 'GtLib\uGlobalDefs.pas',
  uGT in 'GtLib\uGT.pas',
  uHashTable in 'GtLib\uHashTable.pas',
  uMessageHub in 'GtLib\uMessageHub.pas',
  uMirrorDB in 'GtLib\uMirrorDB.pas',
  uPacket in 'GtLib\uPacket.pas',
  uPoller in 'GtLib\uPoller.pas',
  uTCP in 'GtLib\uTCP.pas',
  uTTRules in 'GtLib\uTTRules.pas',
  uUDP in 'GtLib\uUDP.pas',
  uUtils in 'GtLib\uUtils.pas',
  uXmlParser in 'GtLib\uXmlParser.pas',
  ColorButton in 'GUILib\ColorButton.pas',
  uAbstractController in 'GUILib\uAbstractController.pas',
  uCommon in 'GUILib\uCommon.pas',
  uController in 'GUILib\uController.pas',
  uFIDSxml in 'GUILib\uFIDSxml.pas',
  uFlightInfo in 'GUILib\uFlightInfo.pas',
  uSettingsManager in 'GUILib\uSettingsManager.pas',
  uLogin in 'GUILib\uLogin.pas';

{$R *.res}

begin
	Application.Initialize;

    Application.MainFormOnTaskbar := True;
    Application.Title := 'DII FIDS GUI System v2';
    Application.CreateForm(TfrmLogin, frmLogin);
    Application.CreateForm(TfrmMain, frmMain);
    Application.CreateForm(TfRuleEdit, fRuleEdit);
    Application.CreateForm(TfrmManageIndicators, frmManageIndicators);
    Application.CreateForm(TfrmEdit, frmEdit);
    Application.CreateForm(TFCrawlingLineEdit, FCrawlingLineEdit);
    Application.CreateForm(TfStrEntry, fStrEntry);
    Application.Run;

end.
