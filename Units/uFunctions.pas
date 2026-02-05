unit uFunctions;

interface

uses
  Winapi.Windows,
  System.Math,
  System.SysUtils,
  System.Classes,
  Vcl.ComCtrls;

// Convert XYZ string with offset, handling locale-specific decimals.
function TacoXYZConvertion( XYZ:string; OffSet:Double ):String;
// Normalize a numeric string to dot decimal.
function Fix( Input: String ): String;
// Normalize a numeric string to comma decimal.
function NotEnglish( Input: String ): String;
// Determine icon index based on XML content.
function ImageIndex(InString: TStringLIst): Integer;
// Extract type suffix after the last dot.
function BuildType(InString: String): string;
// Extract substring between two markers; returns empty if missing.
function SeporateC( InStr, SubS1, SubS2: String ): string;
// Map a type string to a display name from category XML.
function BuildMarkerCategory( lType: String; InList: TStringList ): String;
// Map an ID to a map name from cached data.
function IdToMap( MapID: string ): string;
// Lowercase and split XML by tag for easier line scanning.
function FormatXML( Input: TStringList ): TStringList;
// Decode XML bytes to text and split by tag.
function FormatByteXML( OutByte: TBytes ): TStringList;
// ListView sort callback for caption/subitem sorting.
function SortProc(Item1, Item2: TListItem; ParamSort: integer): integer; stdcall;
// Convenience status bar logger.
procedure LOG( Statusbar: TStatusBar; P: Integer; Log: String );

var
  SortType: Integer;
  SortMode: Integer;

implementation

uses
  uGlobalValues;

function NotEnglish( Input: String ): String;
begin
  Input := StringReplace( Input, '.',',', [rfReplaceAll, rfIgnoreCase] );
  Input := StringReplace( Input, ' ','', [rfReplaceAll, rfIgnoreCase] );
  Result := Input;
end;

function Fix( Input: String ): String;
begin
  Input := StringReplace( Input, ',','.', [rfReplaceAll, rfIgnoreCase] );
  Input := StringReplace( Input, ' ','', [rfReplaceAll, rfIgnoreCase] );
  Result := Input;
end;

function TacoXYZConvertion( XYZ:string; OffSet:Double ):String;
begin
  try
    Result := RoundTo(StrToFloat( XYZ ) * OffSet, -2).ToString;
  except
    Result := Fix(RoundTo(StrToFloat(NotEnglish(XYZ)) * OffSet, -2).ToString);
  end;
end;

function ImageIndex(InString: TStringLIst): Integer;
begin
  Result := 1;
  if Pos(LowerCase('<poi '), LowerCase(InString.Text)) > 0 then
    Result := 0
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
  Result := '[Not Found]';
  for var K := 0 to InList.Count - 1 do
    if Pos(LowerCase(lType), LowerCase(InList.Strings[K])) > 0 then
    begin
      Result := SeporateC(LowerCase(InList.Strings[K]), 'displayname="', '"');
      Break;
    end;
end;

function FormatXML( Input: TStringList ): TStringList;
begin
  Result := TStringList.Create;
  Input.Text := LowerCase(Input.Text);
  Result.Text := StringReplace(Input.Text, '<', #13#10 + '<', [rfReplaceAll, rfIgnoreCase] );
end;

function FormatByteXML( OutByte: TBytes ): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := TEncoding.ASCII.GetString(OutByte);
  Result.Text := StringReplace(Result.Text, '<', #13#10 + '<', [rfReplaceAll, rfIgnoreCase]);
end;

function SeporateC( InStr, SubS1, SubS2: String ): string;
begin
  Result := '';
  var StartPos := Pos(SubS1, InStr);
  if StartPos = 0 then
    Exit;
  StartPos := StartPos + Length(SubS1);

  var Tail := Copy(InStr, StartPos, MaxInt);
  var EndPos := Pos(SubS2, Tail);
  if EndPos = 0 then
    Exit;
  Result := Copy(Tail, 1, EndPos - 1);
end;

function IdToMap( MapID: string ): string;
begin
  Result := SeporateC( MapIDCache, MapID + ':', ';' );
  if Length(Result) = 0 then
      Result := '[Not Found]';
end;

function SortProc(Item1, Item2: TListItem; ParamSort: integer): integer; stdcall;
begin
  Result := 0;
  if SortType = 0 then
    case SortMode of
      0: Result := lstrcmpi(pchar(Item1.Caption), pchar(Item2.Caption));
      1: Result := -(lstrcmpi(pchar(Item1.Caption), pchar(Item2.Caption)));
    end
  else
  begin
    if (SortType - 1) >= Item1.SubItems.Count then Exit;
    if (SortType - 1) >= Item2.SubItems.Count then Exit;
    case SortMode of
      0: Result := lstrcmpi(pchar(Item1.SubItems.Strings[SortType - 1]), pchar(Item2.SubItems.Strings[SortType - 1]));
      1: Result := -(lstrcmpi(pchar(Item1.SubItems.Strings[SortType - 1]), pchar(Item2.SubItems.Strings[SortType - 1])));
    end;
  end;
end;

procedure LOG( Statusbar: TStatusBar; P: Integer; Log: String );
begin
  Statusbar.Panels.Items[P].Text := Log;
end;

end.
