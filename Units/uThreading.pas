unit uThreading;

interface

uses
  {A}Winapi.Windows,        // Windows API functions
  {S}System.Math,           // Math functions
  {S}System.SysUtils,       // System utilities
  {S}System.Zip,            // ZIP file operations
  {S}System.Threading,      // Threading support
  {S}System.Classes,        // Base classes
  {S}Vcl.ComCtrls,          // Common controls
  {V}Vcl.Samples.Gauges,    // Progress gauges
  {V}Vcl.ExtCtrls,          // Extended panels
  {V}Vcl.StdCtrls,          // Standard controls
  {E}EasyListview;          // Enhanced list view control

type
  // Thread class for handling TACO file drag and drop operations
  // Processes ZIP files containing TACO data in a background thread
  TDropAndDrag = class(TObject)
  public
    Path: String;                    // Path to the TACO file
    Panel: TPanel;                   // Progress panel reference
    Progress: TGauge;                // Progress gauge reference
    Listview:TEasyListview;          // List view to populate with file data
    constructor Create( var pListview: TEasyListview;  var pProgress: TGauge; var pPanel: TPanel );
  end;

type
  // Thread class for processing map data from TACO files
  // Parses XML data and populates the map list view
  TMapList = class(TObject)
  public
    Panel: TPanel;                   // Progress panel reference
    Progress: TGauge;                // Progress gauge reference
    InString: TStringlist;           // Input XML data to process
    Listview:TEasyListview;          // Target list view for map data
    OldListView:TEasyListview;       // Source list view reference
    constructor Create( var pListview: TEasyListview;
                        var pOldListView:TEasyListview;
                        var pProgress: TGauge;
                        var pPanel: TPanel;
                        var pInString: TStringlist );
  end;

type
  // Thread class for searching list view items
  // Filters list view items based on search criteria
  TSearchListview = Class(TObject)
  public
    find:string;                     // Search string
    ListView    : TEasyListview;     // List view to search in
    constructor Create( pFind:string; var pListView: TEasyListview );
  end;

Type
  // Thread class for building JSON export files
  // Converts KX preview data to JSON format for export
  TBuildJSONFile = Class(TObject)
  public
    Name: string;                    // Script name
    Auther: String;                  // Author name
    SavePath: String;                // Output file path
    ListView: TEasyListview;         // Source list view with KX data
    constructor Create( pName: string; pAuther: String; pSavePath: String; var pListView: TEasyListview );
  end;

type
  // Thread class for live player information monitoring
  // Reads real-time player data from MumbleLink and updates UI
  TLivePlayerInfo = class(TObject)
    public
      X: Double;                     // Player X coordinate
      Y: Double;                     // Player Y coordinate
      Z: Double;                     // Player Z coordinate
      Map: string;                   // Current map name
      Name: String;                  // Player character name
      Label1:TLabel;                 // Caption label reference
      Label2: TLabel;                // Coordinates label reference
      Checkbox: TCheckBox;           // Monitor checkbox reference
      constructor Create( var pLabel1: TLabel; var pLabel2: TLabel; pCheckbox: TCheckBox);
  end;

type
  TSearchZip = class(TObject)
    public
      Search: String;
      Progress: TGauge;
      TotalProgress: TGauge;
      ListViewSource: TEasyListview;
      ListViewDestonation: TEasyListview;
      constructor Create( pSearch: string; var pListViewSource: TEasyListview; var pListViewDestonation: TEasyListview; var pProgress: TGauge; var pTotalProgress: TGauge );
  end;

var
  MarkerCategory: TStringlist;       // Global marker category data
  TacoPath: String;

threadvar
  // Thread handles for background operations
  MapList: DWORD;                    // Map list processing thread
  SearchID: DWORD;                   // Search operation thread
  DroAndDra: DWORD;                  // Drag and drop processing thread
  BuildJSONFile: DWORD;              // JSON export thread
  ThreadLPI: DWORD;                  // Live player info thread
  ZIPSearch: DWORD;                  // Search Zip Thread

// Thread procedure declarations for background operations
procedure DropDragThread ( Parameters: Pointer );      // TACO file processing thread
procedure MapListThread( Parameters: Pointer );        // Map data processing thread
procedure ListViewSearch( Parameters: Pointer );       // List view search thread
procedure ConvertToKX( Parameters: Pointer );          // JSON export thread
procedure LivePlayerInfomation(Parameters: Pointer);   // Live player monitoring thread
procedure SearchZip( Parameters: Pointer );            // Search inside Taco for spasific frase

implementation

uses
  uMain,        // Main form unit
  uMumble,      // MumbleLink integration
  uFunctions;   // Utility functions

{ TDropAndDrag }
// Constructor for TACO file drag and drop thread
constructor TDropAndDrag.Create( var pListview: TEasyListview;  var pProgress: TGauge; var pPanel: TPanel );
begin
  Panel := pPanel;
  Progress := pProgress;
  Listview := pListview;
end;

// Determines the image index for list view items based on content type
// Returns 0 for POI (Point of Interest) items, 1 for others
function ImageIndex( InString: TStringLIst ): Integer;
begin
  Result := 1;
  if Pos(LowerCase('<poi '), LowerCase(InString.Text)) > 0 then
    Result := 0
end;


// Converts a byte array to a memory stream
// Used for processing ZIP file contents
function BytesToStream(const B: TBytes): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  if Length(B) > 0 then
  begin
    Result.WriteBuffer(B[0], Length(B));
    Result.Position := 0; // rewind
  end;
end;

// Extracts filename from a path string
// Returns the filename portion after the last forward slash
function ExtractFN(InString: String): String;
begin
  for var I := Length(InString) downto 1 do
  begin
    if InString[I] <> '/' then Continue;
    Result := Copy(InString, I + 1, Length(InString));
    Exit;
  end;
end;

procedure DropDragThread( Parameters: Pointer );
begin
  var ThreadInfo := TDropAndDrag( Parameters );
  var OutByte: TBytes;
  var ListItem: TEasyItem;
  var ZIP := TZipFile.Create;
  Zip.Open(ThreadInfo.Path, zmRead);
  ThreadInfo.Listview.Items.Clear;
  ThreadInfo.Listview.BeginUpdate;
  ThreadInfo.Listview.Enabled := False;
  ThreadInfo.Progress.Visible := True;
  ThreadInfo.Progress.MaxValue := Zip.FileCount -1;
  for var I := 0 to Zip.FileCount -1 do
  begin
    ThreadInfo.Progress.Progress := I;
    if LowerCase(ExtractFileExt(Zip.FileName[I])) <> '.xml' then Continue;
    Zip.Read(I, OutByte);
    ListItem := ThreadInfo.Listview.Items.Add;
    ListItem.Captions[1] := ExtractFileName(Zip.FileName[I]);
    var OutString := TStringList.Create;
    OutString.Text := TEncoding.ASCII.GetString(OutByte);
    OutString.Text := StringReplace(OutString.Text, '<', #13#10 + '<', [rfReplaceAll, rfIgnoreCase] );
    ListItem.Data := OutString;
    ListItem.ImageIndexes[1] := ImageIndex(OutString);
    if  Pos( LowerCase('displayname="'), LowerCase(OutString.Text) ) > 0 then
      MarkerCategory.Text := MarkerCategory.Text + OutString.Text;
  end;
  ThreadInfo.Listview.Enabled := True;
  ThreadInfo.Listview.EndUpdate;
  ThreadInfo.Progress.Visible := False;
end;
{/TDropAndDrag }

{ TMapList }
constructor TMapList.Create( var pListview: TEasyListview;
                        var pOldListView:TEasyListview;
                        var pProgress: TGauge;
                        var pPanel: TPanel;
                        var pInString: TStringlist );
begin
  Panel := pPanel;
  Progress := pProgress;
  Listview := pListview;
  OldListView := pOldListView;
  InString := pInString;
end;

function BuildType(InString: String): string;
begin
  for var J := Length(InString) - 1 downto 0 do
    if InString[J] = '.' then
    begin
      Result := Copy(InString, J + 1, Length(InString));
      Break;
    end;
end;

function BuildMarkerCategory( lType: String; InList: TStringList ): String;
begin
  for var K := 0 to InList.Count - 1 do
    if Pos(LowerCase(lType), LowerCase(InList.Strings[K])) > 0 then
    begin
      Result := SeporateC(LowerCase(InList.Strings[K]), 'displayname="', '"');
      Break;
    end;
end;

procedure MapListThread( Parameters: Pointer );
begin
  var ThreadInfo := TMapList( Parameters );
  ThreadInfo.OldListView.Enabled := False;
  ThreadInfo.ListView.Items.Clear;
  ThreadInfo.Listview.BeginUpdate;
  ThreadInfo.Progress.Visible := True;
  ThreadInfo.Progress.MaxValue := ThreadInfo.InString.Count -1;
  for var I := 0 to ThreadInfo.InString.Count - 1 do
  begin
    ThreadInfo.Progress.Progress := I;
    if Pos('<poi ', LowerCase(ThreadInfo.InString.Strings[I])) = 0  then Continue;
    var MapID := SeporateC( LowerCase(ThreadInfo.InString.Strings[I]), 'mapid="', '"' );
    var lX    := SeporateC( LowerCase(ThreadInfo.InString.Strings[I]), 'xpos="',  '"' );
    var lY    := SeporateC( LowerCase(ThreadInfo.InString.Strings[I]), 'ypos="',  '"' );
    var lZ    := SeporateC( LowerCase(ThreadInfo.InString.Strings[I]), 'zpos="',  '"' );
    var lType := SeporateC( LowerCase(ThreadInfo.InString.Strings[I]), 'type="',  '"' );
    var Display: String := '(n/a)';
    var ListItem: TEasyItem := ThreadInfo.Listview.Items.Add;
    if Pos('.', lType) > 0 then lType := BuildType(lType);
    Display := BuildMarkerCategory(lType, MarkerCategory);
    ListItem.Captions[0] := MapID;
    ListItem.Captions[1] := IdToMap(MapID.ToInteger);
    ListItem.Captions[2] := lX;
    ListItem.Captions[3] := lY;
    ListItem.Captions[4] := lZ;
    ListItem.Captions[5] := lType;
    ListItem.Captions[6] := Display;
  end;
  ThreadInfo.OldListView.Enabled := True;
  ThreadInfo.Listview.EndUpdate;
  ThreadInfo.Progress.Visible := False;
end;
{ /TMapList }

{ TSearchListview }
constructor TSearchListview.Create( pFind:string; var pListView: TEasyListview );
begin
  Find := pFind;
  ListView := pListView;
end;

procedure ListViewSearch(Parameters: Pointer);
var
  LookAt: String;
  ThreadInfo: TSearchListview;
  I, J: Integer;
begin
  ThreadInfo := TSearchListview(Parameters);
  ThreadInfo.Listview.BeginUpdate;
  for I := 0 to ThreadInfo.Listview.Items.Count - 1 do
  begin
    LookAt := '';
    for J := 0 to ThreadInfo.Listview.Header.Columns.Count - 1 do
      LookAt := LookAt + ThreadInfo.Listview.Items[I].Captions[J];
    ThreadInfo.Listview.Items[I].Visible := True;
    if Pos(LowerCase(ThreadInfo.find), LowerCase(LookAt)) = 0 then
      ThreadInfo.Listview.Items[I].Visible := False;
  end;
  ThreadInfo.Listview.EndUpdate;
end;
{/TSearchListview }

{ TBuildJSONFile }
constructor TBuildJSONFile.Create(pName, pAuther, pSavePath: String;  var pListView: TEasyListview);
begin
  Name     := pName;
  Auther   := pAuther;
  SavePath := pSavePath;
  ListView := pListView;
end;

procedure ConvertToKX( Parameters: Pointer );
begin
  var ThreadInfo := TBuildJSONFile( Parameters );
  var SaveToJSON := TStringList.Create;
  SaveToJSON.Add('{');
  SaveToJSON.Add('  "Name": "' + ThreadInfo.Name + '",');
  SaveToJSON.Add('  "Author": "' + ThreadInfo.Auther + '",');
  SaveToJSON.Add('  "Coordinates": [');
  for var I := 0 to ThreadInfo.ListView.Items.Count -1 do
  begin
    SaveToJSON.Add('    {');
    SaveToJSON.Add('      "Name": "' + ThreadInfo.ListView.Items[I].Captions[0] + '",');
    SaveToJSON.Add('      "X": ' + ThreadInfo.ListView.Items[I].Captions[1] + ',');
    SaveToJSON.Add('      "Y": ' + ThreadInfo.ListView.Items[I].Captions[2] + ',');
    SaveToJSON.Add('      "Z": ' + ThreadInfo.ListView.Items[I].Captions[3] );
    if I < ThreadInfo.ListView.Items.Count -1 then
      SaveToJSON.Add('    },');
  end;
  SaveToJSON.Add('    }');
  SaveToJSON.Add('  ]');
  SaveToJSON.Add('}');
  SaveToJSON.SaveToFile(ThreadInfo.SavePath);
  SaveToJSON.Free;
end;
{ /TBuildJSONFile }


//        {
//          "name": "Ancestor Tree",
//          "coordinates": [ -38283.766, 22659.7, 2076.8164 ],
//          "map": 1301
//        },

{ TLivePlayerInfo }

constructor TLivePlayerInfo.Create( var pLabel1: TLabel; var pLabel2: TLabel; pCheckbox: TCheckBox);
begin
  Label1 := pLabel1;
  Label2 := pLabel2;
  Checkbox := pCheckbox;
end;

procedure LivePlayerInfomation(Parameters: Pointer);
var
  Mem: PLinkedMem;
  IdentityJson: string;
  NameStr: string;
  MapId: Integer;
  Gw2Context: TGw2Context;
  HasContext: Boolean;
begin
  var ThreadInfo := TLivePlayerInfo( Parameters );

  if not OpenMumbleLink(Mem) then
  begin
    Threadinfo.Label1.Caption := 'MumbleLink unavailable.' + #13#10 + ' Start Guild Wars 2 first.';
    ThreadInfo.Checkbox.Checked := False;
    Exit;
  end;
  Threadinfo.Label1.Caption := '  Your Current Player Posision';

  while True do
  begin
    IdentityJson := ReadIdentityJson(Mem);
    if not TryParseIdentity(IdentityJson, NameStr, MapId) then
    begin
      NameStr := 'Unknown';
      MapId := 0;
    end;
    HasContext := DecodeGw2Context(Mem, Gw2Context);
    ThreadInfo.X := RoundTo( Mem^.fAvatarPosition[0] * 1.23, -2);
    ThreadInfo.Y := RoundTo( Mem^.fAvatarPosition[2] * 1.23, -2);
    ThreadInfo.Z := RoundTo( Mem^.fAvatarPosition[1] * 1.23, -2);
    ThreadInfo.Map := IdToMap(MapId);
    ThreadInfo.Label2.Caption := 'Map: ' + ThreadInfo.Map + #13#10 + 'X: ' + ThreadInfo.X.ToString +  ' Y:' + ThreadInfo.Y.ToString + ' Z:' + ThreadInfo.Z.ToString;
    Sleep(200);
  end;
end;

constructor TSearchZip.Create( pSearch: string; var pListViewSource: TEasyListview; var pListViewDestonation: TEasyListview; var pProgress: TGauge; var pTotalProgress: TGauge );
begin
  Search := pSearch;
  ListViewSource := pListViewSource;
  ListViewDestonation := pListViewDestonation;
  Progress := pProgress;
  TotalProgress := pTotalProgress;
end;

{ TSearchZip }
procedure SearchZip( Parameters: Pointer );
begin
  var ThreadInfo := TSearchZip( Parameters );
  ThreadInfo.ListViewDestonation.Items.Clear;
  ThreadInfo.ListViewDestonation.BeginUpdate;
  ThreadInfo.ListViewDestonation.Enabled := False;
  ThreadInfo.Progress.Visible := True;
  ThreadInfo.Progress.MaxValue := ThreadInfo.ListViewSource.Items.Count -1;
  for var I := 0 to ThreadInfo.ListViewSource.Items.Count -1 do
  begin
    ThreadInfo.Progress.Progress := I;
    var OutString := TStringList( ThreadInfo.ListViewSource.Items[I].Data );
    if Pos(ThreadInfo.Search, OutString.Text) = 0  then Continue;
    OutString.Text := StringReplace(OutString.Text, '<', #13#10 + '<', [rfReplaceAll, rfIgnoreCase] );
    ThreadInfo.TotalProgress.MaxValue := OutString.Count - 1;
    ThreadInfo.TotalProgress.Visible := True;
    for var j := 0 to OutString.Count - 1 do
    begin
      ThreadInfo.TotalProgress.Progress := j;
      if Pos('<poi ', LowerCase(OutString.Strings[J])) = 0  then Continue;
      if Pos(ThreadInfo.Search, OutString.Strings[J])  = 0  then Continue;
      var MapID := SeporateC( LowerCase(OutString.Strings[J]), 'mapid="', '"' );
      var lX    := SeporateC( LowerCase(OutString.Strings[J]), 'xpos="',  '"' );
      var lY    := SeporateC( LowerCase(OutString.Strings[J]), 'ypos="',  '"' );
      var lZ    := SeporateC( LowerCase(OutString.Strings[J]), 'zpos="',  '"' );
      var lType := SeporateC( LowerCase(OutString.Strings[J]), 'type="',  '"' );
      var Display: String := '(n/a)';
      var ListItem: TEasyItem := ThreadInfo.ListViewDestonation.Items.Add;
      if Pos('.', lType) > 0 then lType := BuildType(lType);
      Display := BuildMarkerCategory(lType, MarkerCategory);
      ListItem.Captions[0] := MapID;
      ListItem.Captions[1] := IdToMap(MapID.ToInteger);
      ListItem.Captions[2] := lX;
      ListItem.Captions[3] := lY;
      ListItem.Captions[4] := lZ;
      ListItem.Captions[5] := lType;
      ListItem.Captions[6] := Display;
    end;
  end;
  ThreadInfo.ListViewDestonation.Enabled := True;
  ThreadInfo.ListViewDestonation.EndUpdate;
  ThreadInfo.Progress.Visible := False;
  ThreadInfo.TotalProgress.Visible := False;
end;

end.
