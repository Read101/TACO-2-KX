unit uTypeConstructors;

interface

uses
  {S}System.SysUtils,       // System utilities
  {S}System.JSON,           // JSON types
  {S}System.Classes,        // Base classes
  {S}System.Generics.Collections, // Generics
  {V}Vcl.Forms,             // Application.ProcessMessages
  {V}Vcl.ComCtrls,          // Common controls
  {V}Vcl.Samples.Gauges,    // Progress gauges
  {V}Vcl.Controls,          // used for align  eg: alBottom;
  {V}Vcl.ExtCtrls,          // Extended panels
  {V}Vcl.Dialogs;           // Message dialogs

type
  // Loads a TACO archive and populates the file list view.
  TLoadTacoFile = class(TObject)
  public
    constructor Create(var pListview: TListView; var pProgress: TGauge; var pPanel: TPanel; pPath: string);
  end;

  // Builds the map list from the selected XML contents.
  TMapList = class(TObject)
  public
    constructor Create(var pListview: TListview; var pOldListView: TListview; var pProgress: TGauge; var pPanel: TPanel; var pInString: TStringlist);
  end;

  // Searches across loaded XML files and builds a filtered map list.
  TSearchZip = class(TObject)
    public
      constructor Create( pSearch: string; var pListViewSource: TListview; var pListViewDestonation: TListview; var pPanel: TPanel; var pProgress: TGauge; var pTotalProgress: TGauge );
  end;

  //TKXJsonExport
  TOKTWExport= Class(TObject)
  public
    constructor Create( pName: string; pAuther: String; pSavePath: String; var pListView: TListview );
  end;

  TKXJsonExport = Class(TObject)
  public
    constructor Create( pName: string; pAuther: String; pSavePath: String; var pListView: TListview );
  end;


implementation

uses
  uProcedures,
  uGlobalValues,
  uDataHelpers,
  uUIHelpers;

constructor TLoadTacoFile.Create(var pListview: TListView; var pProgress: TGauge; var pPanel: TPanel; pPath: string);
begin
  var ListView := pListview;
  var Progress := pProgress;
  var Panel := pPanel;
  const UiPumpInterval = 100;
  var TotalFiles: Integer;
  var MarkerTextValue: string;
  var Entries: TList<TZipEntryInfo> := nil;
  try
    try
      Entries := ExtractZipEntries(pPath, MarkerTextValue, TotalFiles);
    except
      on E: Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Panel.Align := alNone;
        Panel.SendToBack;
        Exit;
      end;
    end;

    // Populate list view with extracted entries and keep UI responsive.
    ListView.Items.BeginUpdate;
    if TotalFiles > 0 then
      Progress.MaxValue := TotalFiles - 1
    else
      Progress.MaxValue := 0;
    Panel.Align := alBottom;

    for var I := 0 to Entries.Count - 1 do
    begin
      var Entry := Entries[I];
      Progress.Progress := Entry.ProgressIndex;
      var ListItem: TListItem := ListView.Items.Add;
      ListItem.Caption := Entry.FileName;
      ListItem.Data := Entry.Content;
      ListItem.Imageindex := Entry.ImageIndex;
      if (I mod UiPumpInterval) = 0 then
        Application.ProcessMessages;
    end;

    ListView.Items.EndUpdate;
    Panel.Align := alNone;
    Panel.SendToBack;
    if MarkerTextValue <> '' then
    begin
      var Temp := TStringList.Create;
      try
        Temp.Text := MarkerTextValue;
        MarkerCategory.AddStrings(Temp);
      finally
        Temp.Free;
      end;
    end;
  finally
    if Entries <> nil then
    begin
      for var Entry in Entries do
        Entry.Free;
      Entries.Free;
    end;
  end;
end;

constructor TMapList.Create(var pListview: TListview; var pOldListView: TListview; var pProgress: TGauge; var pPanel: TPanel; var pInString: TStringlist);
begin
  var ListView := pListview;
  var OldListView := pOldListView;
  var Progress := pProgress;
  var Panel := pPanel;
  const BatchSize = 200;
  const UiPumpInterval = 200;
  // Extract POI lines first, then build the list view in batches.
  var PoiLines := ExtractPoiLines(pInString);
  try
    TThread.Synchronize(nil,
      procedure
      begin
        OldListView.Enabled := False;
        ListView.Items.Clear;
        ListView.Items.BeginUpdate;
        if PoiLines.Count > 0 then
          Progress.MaxValue := PoiLines.Count - 1
        else
          Progress.MaxValue := 0;
        Panel.Align := alBottom;
      end);
    for var I := 0 to PoiLines.Count - 1 do
    begin
      if (I mod BatchSize) <> 0 then
        Continue;
      var BatchStart := I;
      var BatchEnd := I + BatchSize - 1;
      if BatchEnd > PoiLines.Count - 1 then
        BatchEnd := PoiLines.Count - 1;
      TThread.Synchronize(nil,
        procedure
        begin
          for var J := BatchStart to BatchEnd do
          begin
            Progress.Progress := J;
            BuildListview(ListView, PoiLines[J]);
            if (J mod UiPumpInterval) = 0 then
              Application.ProcessMessages;
          end;
        end);
    end;
    TThread.Synchronize(nil,
      procedure
      begin
        OldListView.Enabled := True;
        ListView.Items.EndUpdate;
        Panel.Align := alNone;
      end);
  finally
    PoiLines.Free;
  end;
end;

constructor TSearchZip.Create( pSearch: string; var pListViewSource: TListview; var pListViewDestonation: TListview; var pPanel: TPanel; var pProgress: TGauge; var pTotalProgress: TGauge );
begin
  var ListViewSource := pListViewSource;
  var ListViewDest := pListViewDestonation;
  var Panel := pPanel;
  var Progress := pProgress;
  var TotalProgress := pTotalProgress;
  const BatchSize = 200;
  const UiPumpInterval = 200;
  // Prepare UI state, then fill results in batches to keep UI responsive.
  TThread.Synchronize(nil,
    procedure
    begin
      Panel.Align := alBottom;
      ListViewDest.Items.Clear;
      ListViewDest.Items.BeginUpdate;
      if ListViewSource.Items.Count > 0 then
        Progress.MaxValue := ListViewSource.Items.Count - 1
      else
        Progress.MaxValue := 0;
      TotalProgress.MaxValue := 0;
      TotalProgress.Progress := 0;
    end);
  for var I := 0 to ListViewSource.Items.Count -1 do
  begin
    if ListViewSource.Items[I].Data = nil then
      Continue;
    var TotalLines := 0;
    var Matches := ExtractPoiMatches(pSearch, TStringList(ListViewSource.Items[I].Data), TotalLines);
    var LoopIndex := I;
    try
      TThread.Synchronize(nil,
        procedure
        begin
          Progress.Progress := LoopIndex;
          TotalProgress.MaxValue := TotalLines;
          TotalProgress.Progress := 0;
        end);
      for var J := 0 to Matches.Count - 1 do
      begin
        if (J mod BatchSize) <> 0 then
          Continue;
        var BatchStart := J;
        var BatchEnd := J + BatchSize - 1;
        if BatchEnd > Matches.Count - 1 then
          BatchEnd := Matches.Count - 1;
        TThread.Synchronize(nil,
          procedure
          begin
            for var K := BatchStart to BatchEnd do
            begin
              if K <= TotalProgress.MaxValue then
                TotalProgress.Progress := K;
              BuildListview(ListViewDest, Matches[K]);
              if (K mod UiPumpInterval) = 0 then
                Application.ProcessMessages;
            end;
          end);
      end;
      TThread.Synchronize(nil,
        procedure
        begin
          if TotalProgress.MaxValue >= 0 then
            TotalProgress.Progress := TotalProgress.MaxValue;
        end);
    finally
      Matches.Free;
    end;
  end;
  TThread.Synchronize(nil,
    procedure
    begin
      ListViewDest.Items.EndUpdate;
      Panel.Align := alNone;
    end);
end;

constructor TKXJsonExport.Create(pName, pAuther, pSavePath: String; var pListView: TListview);
begin
  var Root := TJSONObject.Create;
  try
    Root.AddPair('Name', pName);
    Root.AddPair('Author', pAuther);
    var Coordinates := TJSONArray.Create;
    Root.AddPair('Coordinates', Coordinates);
    for var I := 0 to PListView.Items.Count -1 do
    begin
      var Coord := TJSONObject.Create;
      Coord.AddPair('Name', pListView.Items[I].Caption);
      Coord.AddPair('X', JsonNumberOrZero(SafeSubItem(pListView.Items[I], 0)));
      Coord.AddPair('Y', JsonNumberOrZero(SafeSubItem(pListView.Items[I], 1)));
      Coord.AddPair('Z', JsonNumberOrZero(SafeSubItem(pListView.Items[I], 2)));
      Coordinates.AddElement(Coord);
    end;
    var SaveToJSON := TStringList.Create;
    try
      SaveToJSON.Text := JsonToPrettyString(Root.ToJSON);
      SaveToJSON.SaveToFile(pSavePath);
    finally
      SaveToJSON.Free;
    end;
  finally
    Root.Free;
  end;
end;

constructor TOKTWExport.Create(pName, pAuther, pSavePath: String; var pListView: TListview);
begin
  var Root := TJSONObject.Create;
  try
    Root.AddPair('name', pName);
    var Teleports := TJSONArray.Create;
    Root.AddPair('teleports', Teleports);
    for var I := 0 to PListView.Items.Count -1 do
    begin
      var Teleport := TJSONObject.Create;
      Teleport.AddPair('name', pListView.Items[I].Caption);
      var Coordinates := TJSONArray.Create;
      Coordinates.AddElement(JsonNumberOrZero(SafeSubItem(pListView.Items[I], 0)));
      Coordinates.AddElement(JsonNumberOrZero(SafeSubItem(pListView.Items[I], 1)));
      Coordinates.AddElement(JsonNumberOrZero(SafeSubItem(pListView.Items[I], 2)));
      Teleport.AddPair('coordinates', Coordinates);
      Teleport.AddPair('map', JsonNumberOrZero(SafeSubItem(pListView.Items[I], 3)));
      Teleport.AddPair('tooltip', TJSONNull.Create);
      Teleports.AddElement(Teleport);
    end;
    var SaveToJSON := TStringList.Create;
    try
      SaveToJSON.Text := JsonToPrettyString(Root.ToJSON);
      SaveToJSON.SaveToFile(pSavePath);
    finally
      SaveToJSON.Free;
    end;
  finally
    Root.Free;
  end;
end;


end.

