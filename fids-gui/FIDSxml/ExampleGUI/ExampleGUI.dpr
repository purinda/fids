program ExampleGUI;

uses
  Forms,
  uTimeTableView in 'uTimeTableView.pas' {fTimeTable},
  uDataTreeView in 'uDataTreeView.pas' {fDataTree},
  uDepartures in 'uDepartures.pas' {fDepartures},
  ufRuleEdit in 'ufRuleEdit.pas' {fRuleEdit},
  uGUIMain in 'uGUIMain.pas' {fGUIMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfGUIMain, fGUIMain);
  Application.CreateForm(TfTimeTable, fTimeTable);
  Application.CreateForm(TfDataTree, fDataTree);
  Application.CreateForm(TfDepartures, fDepartures);
  Application.CreateForm(TfRuleEdit, fRuleEdit);
  Application.Run;
end.
