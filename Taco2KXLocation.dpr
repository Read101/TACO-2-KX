program Taco2KXLocation;

uses
  Vcl.Forms,
  System.SysUtils,
  System.IniFiles,
  uMain in 'Units\uMain.pas' {frmTakoTOKX},
  uThreading in 'Units\uThreading.pas',
  uFunctions in 'Units\uFunctions.pas',
  uSettings in 'Units\uSettings.pas',
  Vcl.Themes,
  Vcl.Styles,
  uMumble in 'Units\uMumble.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTakoTOKX, frmTakoTOKX);
  if FileExists(ExtractFilePath(ParamStr(0)) + 'Settings.ini') then
  begin
    //Becouse of Darkmode we have to set the main form locatoin here
    var SettingsINI := TIniFile.Create( ExtractFilePath(ParamStr(0)) + 'Settings.ini');
    // Restore form position and size
    frmTakoTOKX.Left   := SettingsINI.ReadInteger('Form', 'Left',   0    );
    frmTakoTOKX.Top    := SettingsINI.ReadInteger('Form', 'Top',    0    );
  end;

  Application.Run;
end.
