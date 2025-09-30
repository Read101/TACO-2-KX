unit uSettings;

interface

uses
  Winapi.Windows,        // Windows API functions
  System.SysUtils,       // System utilities
  System.IniFiles,       // INI file handling
  System.Classes,        // Base classes
  Vcl.Forms,             // Form classes
  EasyListview;          // Enhanced list view control

// Application settings management procedures
procedure SaveApplicationSettings;  // Saves current application settings to INI file
procedure LoadApplicationSettings;  // Loads application settings from INI file

var
  SettingsINI: TIniFile;             // Global INI file object for settings

implementation

uses
  uMain;  // Main form unit

// Saves column widths to INI file
// Stores the width of each column in the specified list view
procedure AddColumWIDTHS( IniSection: String;  ListView: TEasyListview; var Ini: TIniFile );
begin
  for var I := 0 to ListView.Header.Columns.Count - 1 do
    Ini.WriteInteger( IniSection, I.ToString, ListView.Header.Columns[I].Width);
end;

// Loads column widths from INI file
// Restores the width of each column in the specified list view
procedure SetColumsWIDTH( IniSection: String;  ListView: TEasyListview; var Ini: TIniFile);
begin
  for var I := 0 to ListView.Header.Columns.Count - 1 do
    ListView.Header.Columns[I].Width := Ini.ReadInteger(IniSection, I.ToString, 100 );
end;

// Saves current application settings to INI file
// Stores form position, panel sizes, column widths, and user preferences
procedure SaveApplicationSettings;
begin
  SettingsINI := TIniFile.Create( ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  //SettingsINI := TIniFile.Create( ParamStr(0) + ':Settings.ini');
  // Save form position and size
  SettingsINI.WriteInteger( 'Form', 'Left',   frmTakoTOKX.Left   );
  SettingsINI.WriteInteger( 'Form', 'Top',    frmTakoTOKX.Top    );
  SettingsINI.WriteInteger( 'Form', 'Height', frmTakoTOKX.Height );
  SettingsINI.WriteInteger( 'Form', 'Width',  frmTakoTOKX.Width  );
  // Save panel widths
  SettingsINI.WriteInteger( 'LeftMenu', 'Width', frmTakoTOKX.pnlLeftMenu.Width );
  SettingsINI.WriteInteger('RightMenu', 'Width', frmTakoTOKX.pnlRightMenu.Width);
  // Save user preferences
  SettingsINI.WriteString( 'JSON', 'Auther', frmTakoTOKX.edtAuther.Text );
  AddColumWIDTHS('MI_Listview', frmTakoTOKX.lvMapList, SettingsINI );
  SettingsINI.WriteBool   ( 'Map Name','Include', frmTakoTOKX.chkMapID.Checked );
  SettingsINI.WriteBool( 'Form', 'Darkmode', frmTakoTOKX.chkDarkmode.Checked );
end;

// Loads application settings from INI file
// Restores form position, panel sizes, column widths, and user preferences
procedure LoadApplicationSettings;
begin
  if FileExists( ExtractFilePath(ParamStr(0)) + 'Settings.ini' ) = False then Exit;
  SettingsINI := TIniFile.Create( ExtractFilePath(ParamStr(0)) + 'Settings.ini');

  frmTakoTOKX.chkDarkmode.Checked := SettingsINI.ReadBool( 'Form', 'Darkmode', False );
  frmTakoTOKX.chkDarkmode.OnClick(nil);


  // Restore form position and size
  frmTakoTOKX.Left   := SettingsINI.ReadInteger('Form', 'Left',   0    );
  frmTakoTOKX.Top    := SettingsINI.ReadInteger('Form', 'Top',    0    );
  frmTakoTOKX.Height := SettingsINI.ReadInteger('Form', 'Height', 540  );
  frmTakoTOKX.Width  := SettingsINI.ReadInteger('Form', 'Width',  1080 );
  // Restore panel widths
  frmTakoTOKX.pnlLeftMenu.Width  := SettingsINI.ReadInteger( 'LeftMenu', 'Width', 250);
  frmTakoTOKX.pnlRightMenu.Width := SettingsINI.ReadInteger('RightMenu', 'Width', 250);
  // Restore user preferences
  frmTakoTOKX.edtAuther.Text := SettingsINI.ReadString( 'JSON', 'Auther', '' );
  SetColumsWIDTH('MI_Listview', frmTakoTOKX.lvMapList, SettingsINI );
  frmTakoTOKX.chkMapID.Checked    := SettingsINI.ReadBool( 'Map Name','Include', False );
end;

end.
