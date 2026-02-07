unit uDataHelpers;

interface

uses
  {S}System.SysUtils,             // System utilities
  {S}System.JSON,                 // JSON types
  {S}System.Zip,                  // ZIP file operations
  {S}System.Classes,              // Base classes
  {S}System.Generics.Collections; // Generics

type
  // Lightweight data holder for extracted XML entries from a TACO archive.
  TZipEntryInfo = class
  public
    FileName: string;
    Content: TStringList;
    ImageIndex: Integer;
    ProgressIndex: Integer;
  end;

function JsonNumberOrZero(const Value: string): TJSONNumber;
function JsonToPrettyString(const Value: string): string;
function ExtractZipEntries(const Path: string; out MarkerText: string; out TotalFiles: Integer): TList<TZipEntryInfo>;
function ExtractPoiMatches(const Search: string; Source: TStringList; out LineCount: Integer): TList<string>;
function ExtractPoiLines(Source: TStringList): TList<string>;
function BuildSearchIndexFromLookup(const Find: string; const LookAt: TArray<string>): TList<Integer>;

implementation

uses
  uFunctions;

function JsonNumberOrZero(const Value: string): TJSONNumber;
var
  Parsed: Double;
  Clean: string;
  Settings: TFormatSettings;
begin
  // Parse a numeric string safely; fallback to 0 if invalid.
  // Result is always rounded to a whole number.
  Settings := TFormatSettings.Create;
  Settings.DecimalSeparator := '.';
  Clean := Fix(Value);
  if not TryStrToFloat(Clean, Parsed, Settings) then
    Parsed := 0;
  Result := TJSONNumber.Create(Round(Parsed));
end;

function JsonToPrettyString(const Value: string): string;
var
  I: Integer;
  Ch: Char;
  InString: Boolean;
  Escape: Boolean;
  Indent: Integer;
begin
  // Pretty-print JSON while preserving string literals.
  Result := '';
  InString := False;
  Escape := False;
  Indent := 0;

  for I := 1 to Length(Value) do
  begin
    Ch := Value[I];
    if Escape then
    begin
      Escape := False;
      Result := Result + Ch;
      Continue;
    end;

    if (Ch = '\') and InString then
    begin
      Escape := True;
      Result := Result + Ch;
      Continue;
    end;

    if Ch = '"' then
    begin
      InString := not InString;
      Result := Result + Ch;
      Continue;
    end;

    if InString then
    begin
      Result := Result + Ch;
      Continue;
    end;

    case Ch of
      '{', '[':
        begin
          Inc(Indent);
          Result := Result + Ch + sLineBreak + StringOfChar(' ', Indent * 2);
        end;
      '}', ']':
        begin
          Dec(Indent);
          if Indent < 0 then Indent := 0;
          Result := Result + sLineBreak + StringOfChar(' ', Indent * 2) + Ch;
        end;
      ',':
        Result := Result + Ch + sLineBreak + StringOfChar(' ', Indent * 2);
      ':':
        Result := Result + Ch + ' ';
      ' ', #9, #10, #13:
        begin
          // skip whitespace outside strings
        end;
    else
      Result := Result + Ch;
    end;
  end;
end;

function ExtractZipEntries(const Path: string; out MarkerText: string; out TotalFiles: Integer): TList<TZipEntryInfo>;
var
  OutByte: TBytes;
  ZIP: TZipFile;
begin
  // Read .taco archive and collect XML entries plus marker category text.
  Result := TList<TZipEntryInfo>.Create;
  MarkerText := '';
  TotalFiles := 0;
  ZIP := TZipFile.Create;
  try
    try
      ZIP.Open(Path, zmRead);
      TotalFiles := ZIP.FileCount;
      for var I := 0 to ZIP.FileCount - 1 do
      begin
        if LowerCase(ExtractFileExt(ZIP.FileName[I])) <> '.xml' then
          Continue;
        ZIP.Read(I, OutByte);
        var OutString := FormatByteXML(OutByte);
        if Pos(LowerCase('displayname="'), LowerCase(OutString.Text)) > 0 then
          MarkerText := MarkerText + OutString.Text;
        var Entry := TZipEntryInfo.Create;
        Entry.FileName := ExtractFileName(ZIP.FileName[I]);
        Entry.Content := OutString;
        Entry.ImageIndex := ImageIndex(OutString);
        Entry.ProgressIndex := I;
        Result.Add(Entry);
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Unable to read TACO file "%s": %s', [Path, E.Message]);
    end;
  finally
    ZIP.Free;
  end;
end;

function ExtractPoiMatches(const Search: string; Source: TStringList; out LineCount: Integer): TList<string>;
var
  SearchLower: string;
begin
  // Return only POI lines that match the search term.
  Result := TList<string>.Create;
  LineCount := 0;
  if (Source = nil) or (Search = '') then
    Exit;

  SearchLower := LowerCase(Search);
  var OutString := FormatXML(Source);
  try
    if Pos(SearchLower, LowerCase(OutString.Text)) = 0 then
      Exit;
    LineCount := OutString.Count;
    if LineCount < 0 then
      LineCount := 0;
    for var J := 0 to OutString.Count - 1 do
    begin
      if Pos('<poi ', OutString.Strings[J]) = 0 then
        Continue;
      if Pos(SearchLower, OutString.Strings[J]) = 0 then
        Continue;
      Result.Add(OutString.Strings[J]);
    end;
  finally
    OutString.Free;
  end;
end;

function ExtractPoiLines(Source: TStringList): TList<string>;
begin
  // Extract all POI lines without filtering.
  Result := TList<string>.Create;
  if Source = nil then
    Exit;
  for var I := 0 to Source.Count - 1 do
  begin
    if Pos('<poi ', LowerCase(Source.Strings[I])) = 0 then
      Continue;
    Result.Add(Source.Strings[I]);
  end;
end;

function BuildSearchIndexFromLookup(const Find: string; const LookAt: TArray<string>): TList<Integer>;
var
  FindLower: string;
begin
  // Build a list of indices whose text matches the search term.
  Result := TList<Integer>.Create;
  if Find = '' then
    Exit;
  FindLower := LowerCase(Find);
  for var I := 0 to High(LookAt) do
  begin
    if Pos(FindLower, LowerCase(LookAt[I])) = 0 then
      Continue;
    Result.Add(I);
  end;
end;

end.

