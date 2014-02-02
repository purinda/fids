program fids3;

uses
  Forms,
  uFIDSxml in 'GUILib\uFIDSxml.pas',
  uCommon in 'GUILib\uCommon.pas',
  uController in 'GUILib\uController.pas',
  FWindow in 'FWindow.pas' {frmWindow},
  FSearch in 'FSearch.pas' {frmSearch},
  uSettingsManager in 'GUILib\uSettingsManager.pas',
  FEditAnD in 'FEditAnD.pas' {frmEditAnD},
  FMain in 'FMain.pas' {frmMain},
  ufRuleEdit in 'ufRuleEdit.pas' {fRuleEdit},
  FLogin in 'FLogin.pas' {frmLogin},
  FIndicators in 'FIndicators.pas' {frmManageIndicators},
  ColorButton in 'GUILib\ColorButton.pas',
  FEdit in 'FEdit.pas' {frmEdit},
  FCrawlineLines in 'FCrawlineLines.pas' {FCrawlineLinesAllocator},
  CrawlingEdit in 'CrawlingEdit.pas' {FCrawlingLineEdit},
  uConnection in 'GtLib\uConnection.pas';

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
  Application.Run;

end.
