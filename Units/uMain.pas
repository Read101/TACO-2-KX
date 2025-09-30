unit uMain;

interface

uses
  Winapi.Windows,         // Windows API functions
  Winapi.Messages,        // Windows message handling
  Winapi.ShellAPI,        // Shell operations (drag & drop)
  System.SysUtils,        // System utilities
  System.Variants,        // Variant types
  System.Classes,         // Base classes
  System.IniFiles,        // INI file handling
  System.Zip,             // ZIP file operations
  System.ImageList,       // Image list support
  Vcl.Graphics,           // Graphics operations
  Vcl.Controls,           // VCL controls
  Vcl.Forms,              // Form classes
  Vcl.Dialogs,            // Dialog boxes
  Vcl.WinXCtrls,          // Windows 10 controls
  Vcl.StdCtrls,           // Standard controls
  Vcl.ExtCtrls,           // Extended panels
  Vcl.ComCtrls,           // Common controls
  Vcl.ImgList,            // Image lists
  Vcl.Samples.Gauges,     // Progress gauges
  Vcl.Themes,             // VCL themes
  MPCommonObjects,        // MPCommon library objects
  EasyListview,           // Enhanced list view control
  MPCommonUtilities,      // MPCommon utilities
  SynEdit,                // Syntax editor
  Vcl.Menus, Vcl.Buttons; // Menus and buttons

type
  // Main form class for the Taco2KX application
  // This form handles the conversion of TACO files to KX format for Guild Wars 2
  TfrmTakoTOKX = class(TForm)
    // UI Components
    SbMain: TStatusBar;                    // Main status bar
    pnlRightMenu: TPanel;                  // List view showing KX coordinates
    Splitter2: TSplitter;                  // Splitter between map info and KX preview
    Splitter3: TSplitter;                  // Splitter between file list and map info
    ilFileList: TImageList;                // Image list for file icons
    pnlProgress: TPanel;                   // Progress panel
    pmMap: TPopupMenu;                     // Popup menu for map list
    AddSelected1: TMenuItem;               // Menu item to add selected maps
    gProgress: TGauge;                     // Progress gauge
    pnlLeftMenu: TPanel;                   // Left panel for file list
    lvFileList: TEasyListview;             // List view showing TACO files
    pnlSearchFileList: TPanel;             // Panel for file search
    edtFileListSearch: TEdit;              // File list search edit box
    ilButton: TImageList;                  // Button image list
    pnlMapInfo: TPanel;                    // Panel for map information
    lvMapList: TEasyListview;              // List view showing map data
    pnlMapInfoSearch: TPanel;              // Panel for map search
    imgMapInfoSearch: TImage;              // Map search icon
    edtMapInfoSearch: TEdit;               // Map search edit box
    pmKXPreview: TPopupMenu;               // Popup menu for KX preview
    DeleateSelected1: TMenuItem;           // Menu item to delete selected items
    Clear1: TMenuItem;                     // Menu item to clear KX preview
    tmrAnnimation: TTimer;                 // Animation timer
    dlgSaveJason: TSaveDialog;             // Save JSON dialog
    chkDarkmode: TCheckBox;                // Dark mode checkbox
    imgFileListSearch: TImage;
    pnlStringSettings: TPanel;
    pnlJsonSettings: TPanel;               // String replacement settings panel
    lblOldString: TLabel;                  // Old string label
    lblNewString: TLabel;                  // New string label
    edtOldPattern: TEdit;                  // Old pattern edit box
    edtNewPattern: TEdit;                  // New pattern edit box
    btnReplace: TButton;                   // Replace button
    lblName: TLabel;                       // Name label
    lblAuther: TLabel;                     // Author label
    edtAuther: TEdit;                      // Author edit box
    edtScriptName: TEdit;                  // Script name edit box
    btnExport: TButton;                    // Export button
    pnlRightBottem: TPanel;             // String replace button
    AddString1: TMenuItem;                 // Add string menu item
    N1: TMenuItem;                         // Separator menu item
    Befor1: TMenuItem;                     // Before menu item
    After1: TMenuItem;                     // After menu item
    N2: TMenuItem;                         // Separator menu item
    Clear2: TMenuItem;                     // Clear menu item
    pnlPlayerLocation: TPanel;             // Player location panel
    lblXYZ: TLabel;                        // XYZ coordinates label
    lblCaption: TLabel;            // Player location button
    chkPlayerMonitor: TCheckBox;           // Player monitor checkbox
    btnOpenTaco: TBitBtn;                  // Open TACO button
    dlgOpenTaco: TOpenDialog;
    pmFileList: TPopupMenu;
    SearchInsideZIP1: TMenuItem;
    lvKXPreview: TEasyListview;
    gTotal: TGauge;
    Panel1: TPanel;
    btnJason: TButton;
    btnStringReplace: TButton;
    btnPlayerLocation: TButton;
    Panel2: TPanel;
    edtCustomName: TEdit;
    btnAdd: TButton;
    ChkNumerate: TCheckBox;
    chkMapID: TCheckBox;
    ClearName1: TMenuItem;
    N3: TMenuItem;                // Open TACO dialog
    // Event handlers for UI interactions
    procedure lvFileListClick(Sender: TObject);                     // File list click handler
    procedure Splitter3Moved(Sender: TObject);                      // Splitter moved handler
    procedure lvMapListColumnClick(Sender: TCustomEasyListview; Button: TCommonMouseButton; ShiftState: TShiftState; const Column: TEasyColumn);
    procedure lvFileListColumnClick(Sender: TCustomEasyListview; Button: TCommonMouseButton; ShiftState: TShiftState; const Column: TEasyColumn);
    procedure AddSelected1Click(Sender: TObject);                   // Add selected maps to KX preview
    procedure DeleateSelected1Click(Sender: TObject);               // Delete selected items from KX preview
    procedure Clear1Click(Sender: TObject);                         // Clear KX preview
    procedure edtMapInfoSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFileListSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChkNumerateClick(Sender: TObject);                    // Numerate checkbox handler
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure ChkNumerateMouseEnter(Sender: TObject);               // Mouse enter handler for numerate
    procedure ChkNumerateMouseLeave(Sender: TObject);               // Mouse leave handler for numerate
    procedure btnExportMouseEnter(Sender: TObject);                 // Mouse enter handler for export button
    procedure chkMapIDMouseEnter(Sender: TObject);                  // Mouse enter handler for map ID checkbox
    procedure FormCreate(Sender: TObject);                          // Form creation handler
    procedure edtFileListSearchMouseLeave(Sender: TObject);         // Mouse leave handler for file search
    procedure edtMapInfoSearchMouseLeave(Sender: TObject);          // Mouse leave handler for map search
    procedure FormClose(Sender: TObject; var Action: TCloseAction); // Form close handler
    procedure btnExportClick(Sender: TObject);                      // Export button click handler
    procedure btnReplaceClick(Sender: TObject);                     // Replace button click handler
    procedure chkDarkmodeClick(Sender: TObject);                    // Dark mode checkbox handler
    procedure tmrAnnimationTimer(Sender: TObject);                  // Animation timer handler
    procedure btnJasonClick(Sender: TObject);                       // JSON button click handler
    procedure FormShow(Sender: TObject);                            // Form show handler
    procedure Befor1Click(Sender: TObject);                         // Before menu item handler
    procedure Clear2Click(Sender: TObject);                         // Clear menu item handler
    procedure After1Click(Sender: TObject);                         // After menu item handler
    procedure chkPlayerMonitorClick(Sender: TObject);               // Player monitor checkbox handler
    procedure btnOpenTacoClick(Sender: TObject);
    procedure SearchInsideZIP1Click(Sender: TObject);
    procedure lvKXPreviewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvKXPreviewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure ClearName1Click(Sender: TObject);                    // Open TACO button handler
  private
    { Private declarations }
    // Animation state variables
    MapSearchAnimation: Integer;                                     // Animation state for map search panel
    FileSearchAnimation: Integer;                                    // Animation state for file search panel
    FDragging: Boolean;                                              // Drag operation flag
    FDragStartX, FDragStartY, FObjectStartX, FObjectStartY: Integer; // Drag coordinates
  public
    { Public declarations }
  protected
    { protected declarations }
    // Handles drag and drop of TACO files
    procedure TacoListMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;

var
  frmTakoTOKX: TfrmTakoTOKX;               // Main form instance

  // Animation state variables for panel animations
  ReplaceAnimation: Integer = -1;          // String replacement panel animation
  JSONAnimation : Integer = -1;            // JSON settings panel animation
  PlayerAnimation: Integer = -1;           // Player location panel animation
  Retract:Boolean = False;                 // Panel retraction flag

implementation

{$R *.dfm}

uses
  System.Math,       // Math functions
  uThreading,        // Threading unit for background operations
  uFunctions,        // Utility functions
  uSettings,         // Settings management
  uMumble;           // MumbleLink integration

var
  // Global variables for player monitoring
  LivePlayerInfo : TLivePlayerInfo;         // Live player information object
  gX, gY, gZ: Double;                       // Global coordinates
  MumbleLink: PLinkedMem;                   // MumbleLink shared memory pointer
  FileListSearchAnimation: Boolean = False; // File list search animation flag
  OldCaption: string;                       // Previous caption text

// Resets all items in a list view to be visible
// Used when clearing search filters
procedure ResetListview(ListView: TEasyListview);
begin
  ListView.BeginUpdate;
  for var I := 0 to ListView.Items.Count - 1 do
      ListView.Items[I].Visible := True;
  ListView.EndUpdate;
end;

// Adds selected map items to the KX preview list
// Converts coordinates from TACO format to KX format (multiplies by 1.23)
procedure TfrmTakoTOKX.AddSelected1Click(Sender: TObject);
begin //32.01 OKTW
  var J: Integer := 1;
  for var I := 0 to lvMapList.Items.Count - 1 do
  begin
    var MapID:String := '';
    var Numir:String := '';
    if not lvMapList.Items[I].Selected then Continue;
    var ListItem: TEasyItem := lvKXPreview.Items.Add;
    // Add map ID prefix if enabled
    if chkMapID.Checked then MapID := '[' + lvMapList.Items[I].Captions[1] + '] ';
    if ChkNumerate.Checked then Numir := '#' + FixNum(J) + ' ';
    ListItem.Captions[0] := Numir + MapID + lvMapList.Items[I].Captions[6];
    Inc(J);
    try
      // Convert coordinates from TACO to KX format (multiply by 1.23)
      ListItem.Captions[1] := RoundTo(StrToFloat(lvMapList.Items[I].Captions[2]) * 1.23, -2).ToString;
      ListItem.Captions[3] := RoundTo(StrToFloat(lvMapList.Items[I].Captions[3]) * 1.23, -2).ToString;
      ListItem.Captions[2] := RoundTo(StrToFloat(lvMapList.Items[I].Captions[4]) * 1.23, -2).ToString;
    except
      // Handle non-English locale decimal separators
      ListItem.Captions[1] := Fix(RoundTo(StrToFloat(NotEnglish(lvMapList.Items[I].Captions[2])) * 1.23, -2).ToString);
      ListItem.Captions[3] := Fix(RoundTo(StrToFloat(NotEnglish(lvMapList.Items[I].Captions[3])) * 1.23, -2).ToString);
      ListItem.Captions[2] := Fix(RoundTo(StrToFloat(NotEnglish(lvMapList.Items[I].Captions[4])) * 1.23, -2).ToString);
    end;
  end;
end;

procedure TfrmTakoTOKX.After1Click(Sender: TObject);
begin
  if lvKXPreview.Items.Count = 0 then Exit;
  var Result := InputBox('Add Befor', 'Please enter what you want added before text:', '');
  if Length(Result) = 0 then Exit;
  for var I := 0 to lvKXPreview.Items.Count -1 do
  begin
    var Buff := lvKXPreview.Items[I].Caption;
    lvKXPreview.Items[I].Caption := Buff + ' ' + Result;
  end;
end;

procedure TfrmTakoTOKX.Befor1Click(Sender: TObject);
begin
  if lvKXPreview.Items.Count = 0 then Exit;
  var Result := InputBox('Add After', 'Please enter what you want added After text:', '');
  if Length(Result) = 0 then Exit;
  for var I := 0 to lvKXPreview.Items.Count -1 do
  begin
    var Buff := lvKXPreview.Items[I].Caption;
    lvKXPreview.Items[I].Caption := Result + ' ' + Buff;
  end;
end;

// Exports the KX preview to a JSON file
// Creates a background thread to handle the export process
procedure TfrmTakoTOKX.btnAddClick(Sender: TObject);
begin
  if Assigned(LivePlayerInfo) = False then Exit;

  var ListItem: TEasyItem;
  ListItem := lvKXPreview.Items.Add;

  ListItem.Captions[0] := edtCustomName.Text;
  ListItem.Captions[1] := LivePlayerInfo.X.ToString;
  ListItem.Captions[2] := LivePlayerInfo.Y.ToString;
  ListItem.Captions[3] := LivePlayerInfo.Z.ToString;
end;

procedure TfrmTakoTOKX.btnExportClick(Sender: TObject);
var
  lpThreadId: DWORD;
  ThreadInfo: TBuildJSONFile;
begin
  if lvKXPreview.Items.Count = 0 then
    Exit;
  dlgSaveJason.Execute;
  if dlgSaveJason.FileName = '' then
    Exit;

  // Terminate any existing export thread
  TerminateThread(BuildJSONFile, 0);
  // Create new thread for JSON export
  ThreadInfo := TBuildJSONFile.Create(edtScriptName.Text, edtAuther.Text, dlgSaveJason.FileName, lvKXPreview);
  BuildJSONFile := BeginThread(nil, 0, Addr(ConvertToKX), ThreadInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.btnExportMouseEnter(Sender: TObject);
begin
  SbMain.Panels.Items[2].Text := 'Export KX Preview to .JSON file';
end;

procedure TfrmTakoTOKX.btnReplaceClick(Sender: TObject);
begin
  for var I := 0 to lvKXPreview.Items.Count - 1 do
  begin
    var Source := lvKXPreview.Items[I].Caption;
    lvKXPreview.Items[I].Caption := StringReplace(Source, edtOldPattern.Text, edtNewPattern.Text, [rfIgnoreCase, rfReplaceAll]);
  end;
end;

procedure TfrmTakoTOKX.btnJasonClick(Sender: TObject);
begin
  PlayerAnimation := 2;
  JSONAnimation := 2;
  ReplaceAnimation := 2;
  case (Sender as TButton).Tag of
    1: JSONAnimation := 1;
    2: ReplaceAnimation := 1;
    3: PlayerAnimation := 1;
  end;
end;

procedure TfrmTakoTOKX.btnOpenTacoClick(Sender: TObject);
begin
  dlgOpenTaco.Execute;
  var FilePath: String := dlgOpenTaco.FileName;
  if LowerCase(ExtractFileExt(FilePath)) <> '.taco' then Exit;
  edtFileListSearch.Clear;
  TerminateThread(DroAndDra, 0);
  var ThreadInfo := TDropAndDrag.Create(lvFileList, gProgress, pnlProgress);
  ThreadInfo.Path := FilePath;
  var lpThreadId: DWORD;
  DroAndDra := BeginThread(nil, 0, Addr(DropDragThread), ThreadInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.chkDarkmodeClick(Sender: TObject);
begin
  if chkDarkmode.Checked = True then
  begin
    imgFileListSearch.Picture.Graphic := ExtractBMP(ilButton, 2);
    imgMapInfoSearch.Picture.Graphic := ExtractBMP(ilButton, 2);
    TStyleManager.SetStyle('Windows11 Impressive Dark');
    gProgress.ForeColor := $00EF7251;
    gProgress.BackColor := $00351F12;
    gProgress.Color := clBlack;
    gTotal.ForeColor := $00EF7251;
    gTotal.BackColor := $00351F12;
    gTotal.Color := clBlack;
  end;
  if chkDarkmode.Checked = False then
  begin
    imgFileListSearch.Picture.Graphic := ExtractBMP(ilButton, 1);
    imgMapInfoSearch.Picture.Graphic := ExtractBMP(ilButton, 1);
    TStyleManager.SetStyle('Windows');
    gProgress.BackColor := clWhite;
    gProgress.Color := clWhite;
    gProgress.ForeColor := clBlack;
    gTotal.BackColor := clWhite;
    gTotal.Color := clWhite;
    gTotal.ForeColor := clBlack;
  end;
end;

procedure TfrmTakoTOKX.chkMapIDMouseEnter(Sender: TObject);
begin
  SbMain.Panels.Items[2].Text := 'Include Map Name.';
end;

procedure TfrmTakoTOKX.ChkNumerateClick(Sender: TObject);
begin
  if ChkNumerate.Checked then
    for var I := 0 to lvKXPreview.Items.Count - 1 do
    begin
//      if I < 9 then
//        lvKXPreview.Items[I].Caption := '#0' + IntToStr(I + 1) + ' ' + lvKXPreview.Items[I].Caption
//      else
      lvKXPreview.Items[I].Caption := '#' + FixNum(I + 1) + ' '  + lvKXPreview.Items[I].Caption;
    end;
  if ChkNumerate.Checked = False then
    for var I := 0 to lvKXPreview.Items.Count - 1 do
    begin
      var Caption: string := lvKXPreview.Items[I].Caption;
      lvKXPreview.Items[I].Caption := Copy(Caption, Pos(' ', Caption) + 1, Length(Caption));
    end;
end;

procedure TfrmTakoTOKX.ChkNumerateMouseEnter(Sender: TObject);
begin
  SbMain.Panels.Items[2].Text := 'Add Numaration To the Names';
end;

procedure TfrmTakoTOKX.ChkNumerateMouseLeave(Sender: TObject);
begin
  SbMain.Panels.Items[2].Text := '';
end;

procedure TfrmTakoTOKX.chkPlayerMonitorClick(Sender: TObject);
var
  lpThreadId: DWORD;
begin
  TerminateThread(ThreadLPI, 0);
  if chkPlayerMonitor.Checked = False then Exit;

  LivePlayerInfo := TLivePlayerInfo.Create( Self.lblCaption, Self.lblXYZ, Self.chkPlayerMonitor );
  ThreadLPI := BeginThread(nil, 0, Addr(LivePlayerInfomation), LivePlayerInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.Clear1Click(Sender: TObject);
begin
  lvKXPreview.Items.Clear;
end;

procedure TfrmTakoTOKX.Clear2Click(Sender: TObject);
begin
  lvMapList.Items.Clear;
end;

procedure TfrmTakoTOKX.ClearName1Click(Sender: TObject);
begin
  for var I := 0 to lvKXPreview.Items.Count -1 do
    lvKXPreview.Items[I].Caption := '';
end;

procedure TfrmTakoTOKX.DeleateSelected1Click(Sender: TObject);
begin
  for var I := lvKXPreview.Items.Count - 1 downto 0  do
  begin
    if lvKXPreview.Items[I].Selected then
      lvKXPreview.Items.Delete(I);
  end;
end;

procedure TfrmTakoTOKX.edtFileListSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lpThreadId: DWORD;
begin
  TerminateThread(SearchID, 0);
  if lvFileList.Items.Count = 0 then Exit;
  if (Sender as TEdit).Text = '' then
  begin
    ResetListview(lvFileList);
    exit;
  end;
  var ThreadInfo := TSearchListview.Create((Sender as TEdit).Text, lvFileList);
  SearchID := BeginThread(nil, 0, Addr(ListViewSearch), ThreadInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.edtFileListSearchMouseLeave(Sender: TObject);
begin
  FileSearchAnimation := 2;
end;

procedure TfrmTakoTOKX.edtMapInfoSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lpThreadId: DWORD;
begin
  TerminateThread(SearchID, 0);
  if self.lvMapList.Items.Count = 0 then
    Exit;
  if self.edtMapInfoSearch.Text = '' then
  begin
    ResetListview(self.lvMapList);
    exit;
  end;
  var ThreadInfo := TSearchListview.Create(self.edtMapInfoSearch.Text, self.lvMapList);
  SearchID := BeginThread(nil, 0, Addr(ListViewSearch), ThreadInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.edtMapInfoSearchMouseLeave(Sender: TObject);
begin
  MapSearchAnimation := 2;
end;

procedure TfrmTakoTOKX.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  SbMain.Panels.Items[0].Width := pnlLeftMenu.Width + 2;
  SbMain.Panels.Items[1].Width := pnlMapInfo.Width + 2;
  chkDarkmode.Left := Splitter3.Left + 1;

  pnlSearchFileList.Left := 0;
  pnlSearchFileList.Width := Splitter3.Left;
  pnlSearchFileList.top := SbMain.Top;


  pnlMapInfoSearch.Top := SbMain.Top;
  pnlMapInfoSearch.Left := Splitter2.Left - pnlMapInfoSearch.Width;

  pnlProgress.Top := SbMain.Top;
  pnlProgress.Left := chkDarkmode.Left + chkDarkmode.Width;
  pnlProgress.Width := Splitter2.Left - Splitter3.Left - chkDarkmode.Width - pnlMapInfoSearch.Width - 2;


  pnlStringSettings.Top := SbMain.Top - pnlStringSettings.Height;
  pnlStringSettings.Width := pnlRightMenu.Width;

  pnlJsonSettings.Top := SbMain.Top - pnlJsonSettings.Height;
  pnlJsonSettings.Width := pnlRightMenu.Width;

  pnlPlayerLocation.Top := SbMain.Top - pnlPlayerLocation.Height;
  pnlPlayerLocation.Width := pnlRightMenu.Width;


end;

procedure TfrmTakoTOKX.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveApplicationSettings;
end;

procedure TfrmTakoTOKX.FormCreate(Sender: TObject);
begin
  MarkerCategory := TStringList.Create;
  MapSearchAnimation := -1;
  FileSearchAnimation := -1;

  pnlSearchFileList.Left := 0;
  pnlSearchFileList.Top := SbMain.Top - 2;
  pnlSearchFileList.Width := Splitter3.Left;

  pnlMapInfoSearch.Top := SbMain.Top - 2;
  pnlMapInfoSearch.Left := Splitter2.Left - pnlMapInfoSearch.Width;

  pnlProgress.Left := chkDarkmode.Left + chkDarkmode.Width;

  pnlStringSettings.Left := - pnlStringSettings.Width;
  pnlJsonSettings.Left   := - pnlJsonSettings.Width;
  pnlPlayerLocation.Left := - pnlPlayerLocation.Width;

  LoadApplicationSettings;

end;

procedure TfrmTakoTOKX.FormShow(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, True);
end;

procedure TfrmTakoTOKX.lvFileListClick(Sender: TObject);
var
  lpThreadId: DWORD;
  ThreadInfo: TMapList;
begin
  if lvFileList.Selection.FocusedItem = nil then
    Exit;
  var LoadXML := TStringlist(lvFileList.Items[lvFileList.Selection.FocusedItem.Index].Data);
  if LoadXML.Count = 0 then
    Exit;

  TerminateThread(MapList, 0);
  edtMapInfoSearch.Clear;
  ThreadInfo := TMapList.Create(lvMapList, lvFileList, gProgress, pnlProgress, LoadXML);
  MapList := BeginThread(nil, 0, Addr(MapListThread), ThreadInfo, 0, lpThreadId);
end;

procedure TfrmTakoTOKX.lvFileListColumnClick(Sender: TCustomEasyListview; Button: TCommonMouseButton; ShiftState: TShiftState; const Column: TEasyColumn);
begin
  lvFileList.Sort.AutoSort := True;
end;

procedure TfrmTakoTOKX.lvKXPreviewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned((Sender as TEasyListview).DragManager.DragItem) = False then Exit;
  var LvDragDrop := (Sender as TEasyListview);
  try
    LvDragDrop.Items.Exchange(LvDragDrop.DragManager.DragItem.Index, LvDragDrop.DragManager.DropTarget.Index);
  except

  end;
end;

procedure TfrmTakoTOKX.lvKXPreviewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmTakoTOKX.lvMapListColumnClick(Sender: TCustomEasyListview; Button: TCommonMouseButton; ShiftState: TShiftState; const Column: TEasyColumn);
begin
  lvMapList.Sort.AutoSort := True;

end;

procedure TfrmTakoTOKX.SearchInsideZIP1Click(Sender: TObject);
begin
  if lvFileList.Items.Count = 0 then Exit;
  var Result := InputBox('Search Inside ZIP', 'What would you like to find?' + #13#10 + 'This algorithm will search the full Taco' + #13#10 + 'and only show your specific search phrase.', '' );
  if Length(Result) = 0 then Exit;

  var lpThreadId: DWORD;
  TerminateThread(ZIPSearch, 0);
  var ThreadInfo := TSearchZip.Create( Result, lvFileList, lvMapList, gProgress, gTotal);
  ZIPSearch := BeginThread(nil, 0, Addr(SearchZip), ThreadInfo, 0, lpThreadId);

end;

procedure TfrmTakoTOKX.Splitter3Moved(Sender: TObject);
begin
  gProgress.Width := Splitter3.Left;
end;

procedure TfrmTakoTOKX.TacoListMDropFiles(var Msg: TWMDropFiles);
var
  Path: array[0..MAX_PATH] of Char;
  lpThreadId: DWORD;
  ThreadInfo: TDropAndDrag;
begin
  var Count: Integer := DragQueryFileW(Msg.Drop, 0, Path, MAX_PATH);
  if LowerCase(ExtractFileExt(Path)) = '.taco' then
  begin
    edtFileListSearch.Clear;
    if lvFileList.Items.Count <> 0 then
      for var I := 0 to lvFileList.Items.Count - 1 do
        FreeAndNil(lvFileList.Items[I].Data);
    TerminateThread(DroAndDra, 0);
    ThreadInfo := TDropAndDrag.Create(lvFileList, gProgress, pnlProgress);
    ThreadInfo.Path := Path;
    TacoPath := Path;
    DroAndDra := BeginThread(nil, 0, Addr(DropDragThread), ThreadInfo, 0, lpThreadId);
  end;
  DragFinish(Msg.Drop);
  Msg.Result := 0;
  inherited;
end;

procedure ExpandPannel( Panel: TPanel; var AnnimatInt: Integer );
begin
  //if Retract = True then Exit;
  Panel.BringToFront;
  Panel.Left := Panel.Left + 20;
  if Panel.Left >= 0 then
  begin
    AnnimatInt := -1;
    Panel.Left := 0;
  end;
end;

procedure RetractPannels( Panel: TPanel; var AnnimatInt: Integer );
begin
  //Retract := True;
  //Panel.SendToBack;
  Panel.Left := Panel.Left - 20;
  if Panel.Left <= -Panel.Width then
  begin
    Panel.Left := -Panel.Width;
    AnnimatInt := -20;
    //Retract := False;
  end;
end;

procedure TfrmTakoTOKX.tmrAnnimationTimer(Sender: TObject);
begin
  case ReplaceAnimation of
    1: ExpandPannel(pnlStringSettings, ReplaceAnimation);
    2: RetractPannels(pnlStringSettings, ReplaceAnimation);
  end;
  case JSONAnimation of
    1: ExpandPannel(pnlJsonSettings, JSONAnimation);
    2: RetractPannels(pnlJsonSettings, JSONAnimation);
  end;
  case PlayerAnimation of
    1: ExpandPannel(pnlPlayerLocation, PlayerAnimation);
    2: RetractPannels(pnlPlayerLocation, PlayerAnimation);
  end;
end;

end.
