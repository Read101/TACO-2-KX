unit uUIHelpers;

interface

uses
  {S}System.SysUtils,             // SysUtils
  {S}System.StrUtils,             // ContainsText
  {S}System.Generics.Collections, // Generics
  {V}Vcl.ComCtrls;                // Common controls

function BuildSearchIndex(const Find: string; ListFind: TListView; CaptionOnly: Boolean = False): TList<Integer>;
procedure ApplySearchResults(ListFind, ListResult: TListView; Indices: TList<Integer>; CaptionOnly: Boolean = False);
function HasSubItems(Item: TListItem; MinCount: Integer): Boolean;
function SafeSubItem(Item: TListItem; Index: Integer): string;
procedure ClearSearchResultsData(ListView: TListView);
procedure ClearSearchList(ListView: TListView);
function FindItemByText(ListView: TListView; const Text: string; MatchPartial: Boolean = True; IncludeSubItems: Boolean = True): TListItem;

implementation

uses
  uDataHelpers;

function BuildSearchIndex(const Find: string; ListFind: TListView; CaptionOnly: Boolean = False): TList<Integer>;
begin
  // Build a search index from a list view snapshot.
  if (ListFind = nil) then
    Exit(TList<Integer>.Create);

  var LookAt: TArray<string>;
  SetLength(LookAt, ListFind.Items.Count);
  for var I := 0 to ListFind.Items.Count-1 do
  begin
    var Line: String := ListFind.Items[I].Caption;
    if CaptionOnly = False then
    begin
      for var J := 0 to ListFind.Columns.Count -2 do
        Line := Line + ListFind.Items[I].SubItems.Strings[J];
    end;
    LookAt[I] := Line;
  end;
  Result := BuildSearchIndexFromLookup(Find, LookAt);
end;

procedure ApplySearchResults(ListFind, ListResult: TListView; Indices: TList<Integer>; CaptionOnly: Boolean = False);
begin
  // Populate result list view without sharing Data pointers.
  ClearSearchResultsData(ListResult);
  if (Indices = nil) or (Indices.Count = 0) then
  begin
    ListResult.Clear;
    ListResult.SendToBack;
    Exit;
  end;

  ListResult.Clear;
  for var I := 0 to Indices.Count - 1 do
  begin
    var SourceIndex := Indices[I];
    if (SourceIndex < 0) or (SourceIndex >= ListFind.Items.Count) then
      Continue;
    var SourceItem := ListFind.Items[SourceIndex];
    var ListItem: TListItem := ListResult.Items.Add;
    ListItem.Caption := SourceItem.Caption;
    // Store caption copy for stable lookup without sharing raw Data pointers.
    ListItem.Data := StrNew(PChar(SourceItem.Caption));
    if CaptionOnly = False then
    begin
      for var K := 0 to ListFind.Columns.Count -2 do
        ListItem.SubItems.Add(SourceItem.SubItems.Strings[K]);
    end;
  end;
end;

function HasSubItems(Item: TListItem; MinCount: Integer): Boolean;
begin
  // Safe guard for SubItems access.
  Result := (Item <> nil) and (MinCount >= 0) and (Item.SubItems.Count >= MinCount);
end;

function SafeSubItem(Item: TListItem; Index: Integer): string;
begin
  // Safe SubItems accessor with empty fallback.
  if (Item = nil) or (Index < 0) then
    Exit('');
  if Item.SubItems.Count <= Index then
    Exit('');
  Result := Item.SubItems.Strings[Index];
end;

procedure ClearSearchResultsData(ListView: TListView);
begin
  // Free StrNew allocations stored in Data.
  if ListView = nil then
    Exit;
  for var I := 0 to ListView.Items.Count - 1 do
  begin
    if ListView.Items[I].Data <> nil then
    begin
      StrDispose(PChar(ListView.Items[I].Data));
      ListView.Items[I].Data := nil;
    end;
  end;
end;

procedure ClearSearchList(ListView: TListView);
begin
  // Clear search lists safely (data + items).
  ClearSearchResultsData(ListView);
  if ListView <> nil then
    ListView.Clear;
end;

function FindItemByText(ListView: TListView; const Text: string; MatchPartial: Boolean = True; IncludeSubItems: Boolean = True): TListItem;
  function Matches(const Candidate: string): Boolean;
  begin
    if MatchPartial then
      Result := ContainsText(Candidate, Text)
    else
      Result := SameText(Candidate, Text);
  end;
begin
  Result := nil;
  if (ListView = nil) or (Text = '') then
    Exit;
  for var I := 0 to ListView.Items.Count - 1 do
  begin
    var Item := ListView.Items[I];
    if Matches(Item.Caption) then
      Exit(Item);
    if IncludeSubItems then
    begin
      for var J := 0 to Item.SubItems.Count - 1 do
        if Matches(Item.SubItems.Strings[J]) then
          Exit(Item);
    end;
  end;
end;

end.

