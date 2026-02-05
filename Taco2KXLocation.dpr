program Taco2KXLocation;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uMain in 'Units\uMain.pas' {frmMain},
  uTypeConstructors in 'Units\uTypeConstructors.pas',
  uFunctions in 'Units\uFunctions.pas',
  uProcedures in 'Units\uProcedures.pas',
  uGlobalValues in 'Units\uGlobalValues.pas',
  uDataHelpers in 'Units\uDataHelpers.pas',
  uUIHelpers in 'Units\uUIHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
