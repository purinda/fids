program Feeds;

uses
  Forms,
  uPoller in '..\..\Tools\DelphiLib\uPoller.pas',
  uMessageHub in '..\..\Tools\DelphiLib\uMessageHub.pas',
  uMirrorDB in '..\..\Tools\DelphiLib\uMirrorDB.pas',
  uTCP in '..\..\Tools\DelphiLib\uTCP.pas',
  uPacket in '..\..\Tools\DelphiLib\uPacket.pas',
  uFeedMain in 'uFeedMain.pas' {fFeedMain},
  uFidsTags in '..\uFidsTags.pas',
  uXmlParser in '..\..\Tools\DelphiLib\uXmlParser.pas',
  uUtils in '..\..\Tools\DelphiLib\uUtils.pas',
  uTimeTable in 'uTimeTable.pas',
  uHttpServer in 'uHttpServer.pas',
  uDbTree in '..\..\Tools\DelphiLib\uDbTree.pas',
  uFlight in 'uFlight.pas',
  uCheckInControl in 'uCheckInControl.pas',
  uKeyPadServer in 'uKeyPadServer.pas',
  uUDCManagement in 'uUDCManagement.pas',
  uUDC in '..\uUDC.pas',
  uUDCTerminalServer in 'uUDCTerminalServer.pas',
  uUDCs in 'uUDCs.pas' {fUDCs},
  uStringGrid in '..\..\Tools\DelphiLib\uStringGrid.pas',
  uLantronixTerminalServer in 'uLantronixTerminalServer.pas',
  uThreadControl in '..\..\Tools\DelphiLib\uThreadControl.pas',
  uQantasFeedHandler in 'uQantasFeedHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfFeedMain, fFeedMain);
  Application.CreateForm(TfUDCs, fUDCs);
  Application.Run;
end.
