program VTBasic;

uses
  Forms,
  VTDBExample in 'VTDBExample.pas' {frmVTDBExample},
  VTreeData in 'VTreeData.pas',
  VTCheckList in 'VTCheckList.pas' {frmVTCheckList},
  VTPropEdit in 'VTPropEdit.pas' {frmVTPropEdit},
  VTEditors in 'VTEditors.pas',
  Main in 'Main.pas' {frmMain},
  ViewCode in 'ViewCode.pas' {frmViewCode};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

