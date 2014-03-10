program GUI2;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uValidation in '..\uValidation.pas',
  uFidsTags in '..\uFidsTags.pas',
  uDbTree in '..\..\Tools\DelphiLib\uDbTree.pas',
  ufStringEntry in '..\..\Tools\DelphiLib\ufStringEntry.pas' {fStrEntry};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfStrEntry, fStrEntry);
  Application.Run;
end.
