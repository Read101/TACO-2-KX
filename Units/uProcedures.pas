unit uProcedures;

interface

uses
  {A}Winapi.Windows,        // Windows API functions
  {S}System.Math,           // Math functions
  {S}System.SysUtils,       // System utilities
  {S}System.Zip,            // ZIP file operations
  {S}System.Classes,        // Base classes
  {V}Vcl.ComCtrls,          // Common controls
  {V}Vcl.Controls,
  {V}Vcl.Samples.Gauges,    // Progress gauges
  {V}Vcl.ExtCtrls,          // Extended panels
  {V}Vcl.StdCtrls;          // Enhanced list view control

// Free any TStringList stored in ListView.Items[].Data and clear the list.
procedure ClearListViewData(var ListView: TListView);
// Parse a POI XML line and append a populated ListView item.
procedure BuildListview(var Listview: TListview; ToBuild: String );

implementation

uses
  uGlobalValues,
  uFunctions;

procedure ClearListViewData(var ListView: TListView);
begin
  for var I := 0 to ListView.Items.Count - 1 do
  begin
    if ListView.Items[I].Data = nil then Continue;
    TStringList(ListView.Items[I].Data).Free;
    ListView.Items[I].Data := nil;
  end;
  ListView.Items.Clear;
end;

procedure BuildListview(var Listview: TListview; ToBuild: String );
begin
  var MapID   := SeporateC( LowerCase(ToBuild), 'mapid="', '"' );
  var lX      := SeporateC( LowerCase(ToBuild), 'xpos="',  '"' );
  var lY      := SeporateC( LowerCase(ToBuild), 'ypos="',  '"' );
  var lZ      := SeporateC( LowerCase(ToBuild), 'zpos="',  '"' );
  var lType   := SeporateC( LowerCase(ToBuild), 'type="',  '"' );
  var ListItem: TListItem := Listview.Items.Add;
  if Pos('.', lType) > 0 then lType := BuildType(lType);
  ListItem.Caption := MapID;
  ListItem.SubItems.Add(IdToMap(MapID) );
  ListItem.SubItems.Add( lX );
  ListItem.SubItems.Add( lY );
  ListItem.SubItems.Add( lZ );
  ListItem.SubItems.Add( lType );
  ListItem.SubItems.Add( BuildMarkerCategory(lType, MarkerCategory) );
end;

end.
