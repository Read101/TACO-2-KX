unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI, System.SysUtils,
  System.Classes, System.IniFiles, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ImgList, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Samples.Gauges, Vcl.Menus;

type
  TfrmMain = class(TForm)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    lvKXPreview: TListView;
    pnlMain: TPanel;
    lvMapList: TListView;
    SbStatus: TStatusBar;
    splLeft: TSplitter;
    splRight: TSplitter;
    pnlMenu: TPanel;
    ilButtonIcons: TImageList;
    EdtTacoPath: TEdit;
    btnOpenTaco: TSpeedButton;
    ilFileList: TImageList;
    pnlProgress: TPanel;
    gCurrentProgress: TGauge;
    gTotalProgress: TGauge;
    pmFileList: TPopupMenu;
    Searchall1: TMenuItem;
    pmMapInfo: TPopupMenu;
    Clear1: TMenuItem;
    AddSelected1: TMenuItem;
    pgcSettings: TPageControl;
    tsJsonExport: TTabSheet;
    splMiddle: TSplitter;
    tsStringReplace: TTabSheet;
    btnExport: TButton;
    lblAuther: TLabel;
    lblName: TLabel;
    edtScriptName: TEdit;
    edtAuther: TEdit;
    dlgSaveJason: TSaveDialog;
    chkOKTW: TCheckBox;
    lvFileList: TListView;
    dlgOpenTaco: TOpenDialog;
    lvFileListSearch: TListView;
    lvMapListSearch: TListView;
    EdtOldPatten: TEdit;
    EdtNewPatten: TEdit;
    lblOldPatten: TLabel;
    lblNewPatten: TLabel;
    btnUpdate: TButton;
    N1: TMenuItem;
    pmMapInfoSearch: TPopupMenu;
    AddSelected2: TMenuItem;
    N2: TMenuItem;
    Clear2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Searchall1Click(Sender: TObject);
    procedure lvKXPreviewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvKXPreviewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure btnExportClick(Sender: TObject);
    procedure chkOKTWClick(Sender: TObject);
    procedure lvFileListColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvMapListKeyPress(Sender: TObject; var Key: Char);
    procedure lvFileListKeyPress(Sender: TObject; var Key: Char);
    procedure btnOpenTacoClick(Sender: TObject);
    procedure lvMapListSearchExit(Sender: TObject);
    procedure lvFileListSearchExit(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure lvKXPreviewDblClick(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure AddSelected1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvFileListDblClick(Sender: TObject);
  private
    { Private declarations }
    MapSearch: string;
    FileSearch: string;
    // Centralized cleanup for list view item data (TStringList instances)
    procedure ClearFileListData;
    // Toggle UI elements that should not be interacted with during long work
    procedure SetUiBusy(IsBusy: Boolean);
    // UI state persistence helpers
    function StateFilePath: string;
    procedure LoadUiState;
    procedure SaveUiState;
  public
    { Public declarations }

  protected
    { protected declarations }
    // Handles drag and drop of TACO files
    procedure TacoListMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  uTypeConstructors, uGlobalValues, uFunctions, uProcedures, uUIHelpers;

procedure TfrmMain.AddSelected1Click(Sender: TObject);
begin
  // Add selected map entries into the preview list.
  var Offset: Double := 1.23;
  if chkOKTW.Checked then
    Offset := 32.01;
  var Source: TListView := lvMapList;
  if (Sender as TMenuItem).Tag = 2 then
    Source := lvMapListSearch;
  for var I := 0 to Source.Items.Count - 1 do
  begin
    if Source.Items[I].Selected = False then
      Continue;
    if not HasSubItems(Source.Items[I], 6) then
      Continue;
    var TargetItem := lvKXPreview.Items.Add;
    TargetItem.Caption := SafeSubItem(Source.Items[I], 5);
    TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(Source.Items[I], 1), Offset)); //X
    TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(Source.Items[I], 2), Offset)); //Y
    TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(Source.Items[I], 3), Offset)); //Z
    TargetItem.SubItems.Add(Source.Items[I].Caption);
  end;
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
begin
  // Export current preview list as JSON.
  if lvKXPreview.Items.Count = 0 then
    Exit;
  dlgSaveJason.Execute;
  if dlgSaveJason.FileName = '' then
    Exit;

  if chkOKTW.Checked then
  begin
    var OKTWExport := TOKTWExport.Create(edtScriptName.Text, edtAuther.Text, dlgSaveJason.FileName, lvKXPreview);
    OKTWExport.Free;
  end
  else
  begin
    var KXJsonExport := TKXJsonExport.Create(edtScriptName.Text, edtAuther.Text, dlgSaveJason.FileName, lvKXPreview);
    KXJsonExport.Free;
  end;
end;

procedure TfrmMain.btnOpenTacoClick(Sender: TObject);
begin
  if dlgOpenTaco.Execute = False then
    Exit;
  if dlgOpenTaco.FileName = '' then
    Exit;
  EdtTacoPath.Text := dlgOpenTaco.FileName;
  if FileExists(EdtTacoPath.Text) then
  begin
    // Disable UI while loading to avoid re-entrancy during ProcessMessages
    SetUiBusy(True);
    try
      ClearFileListData;
      ClearSearchList(lvFileListSearch);
      ClearSearchList(lvMapListSearch);
      lvMapList.Clear;
      // Load TACO file and populate file list
      var LoadTacoFile := TLoadTacoFile.Create(lvFileList, gCurrentProgress, pnlProgress, EdtTacoPath.Text);
      LoadTacoFile.Free;
    finally
      SetUiBusy(False);
    end;
  end;
end;

procedure TfrmMain.btnUpdateClick(Sender: TObject);
begin
  // Apply string replacement to all preview names.
  for var I := 0 to lvKXPreview.Items.Count - 1 do
    lvKXPreview.Items[I].Caption := StringReplace(lvKXPreview.Items[I].Caption, EdtOldPatten.Text, EdtNewPatten.Text, [rfReplaceAll, rfIgnoreCase]);
end;

procedure TfrmMain.chkOKTWClick(Sender: TObject);
begin
  // OKTW export uses a fixed author field.
  lvKXPreview.Clear;
  if chkOKTW.Checked = True then
    edtAuther.Enabled := False
  else
    edtAuther.Enabled := True;
end;

procedure TfrmMain.Clear1Click(Sender: TObject);
begin
  // Clear map search results.
  ClearSearchList(lvMapListSearch);
  lvMapList.Clear;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Persist UI state on exit
  SaveUiState;
  // Free any data attached to list view items
  ClearFileListData;
  if Assigned(MarkerCategory) then
    MarkerCategory.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Set up drag/drop and load cached map IDs.
  DragAcceptFiles(Self.Handle, True);
  MarkerCategory := TStringList.Create;
  if FileExists('MapID.101') then
  begin
    var LoadMapID := TStringList.Create;
    try
      LoadMapID.LoadFromFile('MapID.101');
      MapIDCache := LoadMapID.Text;
    finally
      LoadMapID.Free;
    end;
  end;
  // Restore window/splitter positions and edit values
  LoadUiState;
end;

procedure TfrmMain.lvFileListColumnClick(Sender: TObject; Column: TListColumn);
begin
  // Toggle sort direction for the selected column.
  if SortType = Column.Index then
  begin
    if SortMode = 0 then
      SortMode := 1
    else
      SortMode := 0;
  end
  else
  begin
    SortType := Column.Index;
    SortMode := 0;
  end;
  (Sender as TListView).CustomSort(@SortProc, 0);
end;

procedure TfrmMain.lvFileListDblClick(Sender: TObject);
begin
  if (Sender as TListview).Selected = nil then
    exit;
  var ListView := (Sender as TListview);
  var SourceItem := ListView.Selected;
  if ListView = lvFileListSearch then
  begin
    if SourceItem.Data = nil then
      Exit;
    var FileName := string(PChar(SourceItem.Data));
    SourceItem := FindItemByText(lvFileList, FileName, False, False);
    if SourceItem = nil then
      Exit;
  end;
  if SourceItem.Data = nil then
    Exit;
  // Disable UI while parsing and building the map list
  SetUiBusy(True);
  try
    var LoadXML := TStringlist(SourceItem.Data);
    var MapList := TMapList.Create(lvMapList, ListView, gCurrentProgress, pnlProgress, LoadXML);
    MapList.Free;
  finally
    SetUiBusy(False);
  end;
  (Sender as TListview).SetFocus;
end;

procedure TfrmMain.lvFileListKeyPress(Sender: TObject; var Key: Char);
begin
  // Incremental search for file list.
  if Ord(Key) = 27 then
  begin
    FileSearch := '';
    lvFileListSearch.SendToBack;
  end
  else
  begin
    if Ord(Key) = 8 then
    begin
      if Length(FileSearch) > 0 then
        Delete(FileSearch, Length(FileSearch), 1);
    end
    else
      FileSearch := FileSearch + Key;
    lvFileListSearch.BringToFront;
    var Indices := BuildSearchIndex(FileSearch, lvFileList, True);
    try
      ApplySearchResults(lvFileList, lvFileListSearch, Indices, True);
    finally
      Indices.Free;
    end;
  end;
  LOG(SbStatus, 0, FileSearch);
end;

procedure TfrmMain.lvFileListSearchExit(Sender: TObject);
begin
  if lvFileListSearch.Focused = False then
  begin
    lvFileListSearch.SendToBack;
    MapSearch := '';
  end;
end;

procedure TfrmMain.lvKXPreviewDblClick(Sender: TObject);
begin
  EdtOldPatten.Text := lvKXPreview.Selected.Caption;
end;

procedure TfrmMain.lvKXPreviewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceItem: TListItem;
  TargetItem: TListItem;
  DragItem, DropItem, CurrentItem, NextItem: TListItem;
begin
  var Offset: Double := 1.23;
  if chkOKTW.Checked then
    Offset := 32.01;
  if Sender <> Source then//(Source as TListView) then
  begin
    SourceItem := (Source as TListView).Selected;
    while SourceItem <> nil do
    begin
    if not HasSubItems(SourceItem, 6) then
    begin
      SourceItem := (Source as TListView).GetNextItem(SourceItem, sdAll, [isSelected]);
      Continue;
    end;
      TargetItem := (Sender as TListView).Items.Add;
      TargetItem.Caption := SafeSubItem(SourceItem, 5);
      TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(SourceItem, 1), Offset)); //X
      TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(SourceItem, 2), Offset)); //Y
      TargetItem.SubItems.Add(TacoXYZConvertion(SafeSubItem(SourceItem, 3), Offset)); //Z
      TargetItem.SubItems.Add(SourceItem.Caption);
      SourceItem := (Source as TListView).GetNextItem(SourceItem, sdAll, [isSelected]);
    end;
  end;
  if Sender = Source then
    with TListView(Sender) do
    begin
      DropItem := GetItemAt(X, Y);
      CurrentItem := Selected;
      while CurrentItem <> nil do
      begin
        NextItem := GetNextItem(CurrentItem, SdAll, [IsSelected]);
        if DropItem = nil then
          DragItem := Items.Add
        else
          DragItem := Items.Insert(DropItem.Index);
        DragItem.Assign(CurrentItem);
        CurrentItem.Free;
        CurrentItem := NextItem;
      end;
    end;
end;

procedure TfrmMain.lvKXPreviewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = (Sender as TListView);
end;

procedure TfrmMain.lvMapListKeyPress(Sender: TObject; var Key: Char);
begin
  // Incremental search for map list.
  if Ord(Key) = 27 then
  begin
    MapSearch := '';
    lvMapListSearch.SendToBack;
  end
  else
  begin
    if Ord(Key) = 8 then
    begin
      if Length(MapSearch) > 0 then
        Delete(MapSearch, Length(MapSearch), 1);
    end
    else
      MapSearch := MapSearch + Key;
    lvMapListSearch.BringToFront;
    var Indices := BuildSearchIndex(MapSearch, lvMapList);
    try
      ApplySearchResults(lvMapList, lvMapListSearch, Indices);
    finally
      Indices.Free;
    end;
  end;
  LOG(SbStatus, 0, MapSearch);
end;

procedure TfrmMain.lvMapListSearchExit(Sender: TObject);
begin
  if lvMapListSearch.Focused = False then
  begin
    lvMapListSearch.SendToBack;
    MapSearch := '';
  end;
end;

procedure TfrmMain.Searchall1Click(Sender: TObject);
begin
  // Search across all files for a term and build a map list.
  if lvFileList.Items.Count = 0 then
    Exit;
  var Result := InputBox('Search inside all files', 'What would you like to find?', '');
  if Length(Result) = 0 then
    Exit;
  // Disable UI while searching across all files
  SetUiBusy(True);
  try
    var SearchZip := TSearchZip.Create(Result, lvFileList, lvMapList, pnlProgress, gCurrentProgress, gTotalProgress);
    SearchZip.Free;
  finally
    SetUiBusy(False);
  end;
end;

procedure TfrmMain.TacoListMDropFiles(var Msg: TWMDropFiles);
var
  Path: array[0..MAX_PATH] of Char;
begin
  // Handle .taco drag/drop and load file list.
  DragQueryFileW(Msg.Drop, 0, Path, MAX_PATH);
  if Lowercase(ExtractFileExt(Path)) <> '.taco' then
    exit;
  EdtTacoPath.Text := Path;
  // Disable UI while loading to avoid re-entrancy during ProcessMessages
  SetUiBusy(True);
  try
    ClearFileListData;
    ClearSearchList(lvFileListSearch);
    ClearSearchList(lvMapListSearch);
    lvMapList.Clear;
    // Load TACO file and populate file list
    var LoadTacoFile := TLoadTacoFile.Create(lvFileList, gCurrentProgress, pnlProgress, EdtTacoPath.Text);
    LoadTacoFile.Free;
  finally
    SetUiBusy(False);
  end;
end;

procedure TfrmMain.ClearFileListData;
begin
  // Free per-item data stored in lvFileList.
  ClearListViewData(lvFileList);
end;

procedure TfrmMain.SetUiBusy(IsBusy: Boolean);
begin
  // Prevent user actions while we are processing data.
  btnOpenTaco.Enabled := not IsBusy;
  btnExport.Enabled := not IsBusy;
  btnUpdate.Enabled := not IsBusy;
  chkOKTW.Enabled := not IsBusy;
  lvFileList.Enabled := not IsBusy;
  lvMapList.Enabled := not IsBusy;
  lvKXPreview.Enabled := not IsBusy;
end;

function TfrmMain.StateFilePath: string;
begin
  // Store settings next to the executable
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmMain.LoadUiState;
begin
  // Restore persisted window/layout and edit fields.
  var Ini := TIniFile.Create(StateFilePath);
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    splLeft.Left := Ini.ReadInteger('Splitters', 'Left', splLeft.Left);
    splRight.Left := Ini.ReadInteger('Splitters', 'Right', splRight.Left);
    splMiddle.Top := Ini.ReadInteger('Splitters', 'MiddleTop', splMiddle.Top);

    var PageIndex := Ini.ReadInteger('Tabs', 'SettingsIndex', pgcSettings.ActivePageIndex);
    if (PageIndex >= 0) and (PageIndex < pgcSettings.PageCount) then
      pgcSettings.ActivePageIndex := PageIndex;

    EdtTacoPath.Text := Ini.ReadString('Edits', 'TacoPath', EdtTacoPath.Text);
    EdtOldPatten.Text := Ini.ReadString('Edits', 'OldPattern', EdtOldPatten.Text);
    EdtNewPatten.Text := Ini.ReadString('Edits', 'NewPattern', EdtNewPatten.Text);
    edtScriptName.Text := Ini.ReadString('Edits', 'ScriptName', edtScriptName.Text);
    edtAuther.Text := Ini.ReadString('Edits', 'Author', edtAuther.Text);
    chkOKTW.Checked := Ini.ReadBool('Edits', 'OKTW', chkOKTW.Checked);
    chkOKTWClick(nil);
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.SaveUiState;
begin
  // Persist window/layout and edit fields to INI.
  var Ini := TIniFile.Create(StateFilePath);
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteInteger('Splitters', 'Left', splLeft.Left);
    Ini.WriteInteger('Splitters', 'Right', splRight.Left);
    Ini.WriteInteger('Splitters', 'MiddleTop', splMiddle.Top);

    Ini.WriteInteger('Tabs', 'SettingsIndex', pgcSettings.ActivePageIndex);

    Ini.WriteString('Edits', 'TacoPath', EdtTacoPath.Text);
    Ini.WriteString('Edits', 'OldPattern', EdtOldPatten.Text);
    Ini.WriteString('Edits', 'NewPattern', EdtNewPatten.Text);
    Ini.WriteString('Edits', 'ScriptName', edtScriptName.Text);
    Ini.WriteString('Edits', 'Author', edtAuther.Text);
    Ini.WriteBool('Edits', 'OKTW', chkOKTW.Checked);
  finally
    Ini.Free;
  end;
end;

end.

