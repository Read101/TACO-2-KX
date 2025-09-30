unit uMumble;

interface

uses
  Winapi.Windows,
  System.Math,
  System.JSON,
  System.SysUtils;

type
  // Pointer to the MumbleLink shared memory structure
  PLinkedMem = ^TLinkedMem;

  // MumbleLink shared memory structure - provides real-time game data from Guild Wars 2
  // This structure is shared between Guild Wars 2 and Mumble/other applications
  TLinkedMem = packed record
    uiVersion: Cardinal;                     // Version of the MumbleLink protocol
    uiTick: Cardinal;                        // Tick counter for change detection
    fAvatarPosition: array[0..2] of Single;  // Player position [X, Y, Z] in game coordinates
    fAvatarFront: array[0..2] of Single;     // Player's forward direction vector
    fAvatarTop: array[0..2] of Single;       // Player's up direction vector
    name: array[0..255] of WideChar;         // Player's character name
    fCameraPosition: array[0..2] of Single;  // Camera position [X, Y, Z]
    fCameraFront: array[0..2] of Single;     // Camera's forward direction vector
    fCameraTop: array[0..2] of Single;       // Camera's up direction vector
    identity: array[0..255] of WideChar;     // JSON string containing character info
    contextLen: Cardinal;                    // Length of the context data
    context: array[0..255] of Byte;          // Game-specific context data
    description: array[0..2047] of WideChar; // Additional description text
  end;

  // Pointer to Guild Wars 2 specific context data
  PGw2Context = ^TGw2Context;
  
  // Guild Wars 2 specific context structure - contains additional game state information
  // This data is stored in the context field of TLinkedMem when playing GW2
  TGw2Context = packed record
    serverAddress: array[0..27] of Byte;  // Server address (28 bytes)
    mapId: UInt32;                        // Current map ID
    mapType: UInt32;                      // Type of map (PvE, PvP, WvW, etc.)
    shardId: UInt32;                      // Server shard ID
    instance: UInt32;                     // Map instance number
    buildId: UInt32;                      // Game build version
    uiState: UInt32;                      // Bit flags for UI state (map open, in combat, etc.)
    compassWidth: UInt16;                 // Compass width in pixels
    compassHeight: UInt16;                // Compass height in pixels
    compassRotation: Single;              // Compass rotation angle
    playerX: Single;                      // Player X coordinate
    playerY: Single;                      // Player Y coordinate
    mapCenterX: Single;                   // Map center X coordinate
    mapCenterY: Single;                   // Map center Y coordinate
    mapScale: Single;                     // Map scale factor
    processId: UInt32;                    // GW2 process ID
    mountIndex: Byte;                     // Currently mounted creature index
  end;

// Function declarations for MumbleLink operations

// Returns the name of the mount based on its index
function GetMountName(MountIndex: Byte): string;

// Converts UI state bit flags to a human-readable string
function GetUIStateString(const Context: TGw2Context): string;

// Extracts and formats the server address from context data
function GetServerAddress(const Context: TGw2Context): string;

// Decodes GW2-specific context data from the MumbleLink memory
function DecodeGw2Context(const Mem: PLinkedMem; out Context: TGw2Context): Boolean;

// Parses the identity JSON string to extract character name and map ID
function TryParseIdentity(const IdentityJson: string; out CharacterName: string; out MapId: Integer): Boolean;

// Reads the identity JSON string from MumbleLink memory
function ReadIdentityJson(const Mem: PLinkedMem): string;

// Opens a connection to the MumbleLink shared memory
function OpenMumbleLink(out View: PLinkedMem): Boolean;

implementation

// Opens a connection to the MumbleLink shared memory
// Returns True if successful, False if GW2 is not running,
// MumbleLink douse not need to be running or installed.
function OpenMumbleLink(out View: PLinkedMem): Boolean;
var
  hMap: THandle;
begin
  View := nil;
  // Attempt to open the MumbleLink shared memory object
  hMap := OpenFileMappingW(FILE_MAP_READ, False, 'MumbleLink');
  if hMap = 0 then
    Exit(False);
  // Map the shared memory into our process address space
  View := MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, SizeOf(TLinkedMem));
  CloseHandle(hMap);
  Result := View <> nil;
end;

// Reads the identity JSON string from MumbleLink memory
// The identity field contains JSON data with character information
function ReadIdentityJson(const Mem: PLinkedMem): string;
var
  p: PWideChar;
  len: Integer;
begin
  p := @Mem^.identity[0];
  len := lstrlenW(p);
  SetString(Result, p, len);
end;

// Parses the identity JSON string to extract character name and map ID
// Returns True if parsing was successful, False if JSON is invalid
function TryParseIdentity(const IdentityJson: string; out CharacterName: string; out MapId: Integer): Boolean;
var
  V: TJSONValue;
  O: TJSONObject;
  F: TJSONValue;
begin
  CharacterName := '';
  MapId := 0;
  V := TJSONObject.ParseJSONValue(IdentityJson);
  try
    if (V <> nil) and (V is TJSONObject) then
    begin
      O := TJSONObject(V);
      // Extract character name from JSON
      F := O.Values['name'];
      if F <> nil then
        CharacterName := F.Value;

      // Extract map ID from JSON
      F := O.Values['map_id'];
      if (F <> nil) then
        MapId := StrToIntDef(F.Value, 0);

      Exit(True);
    end;
    Result := False;
  finally
    V.Free;
  end;
end;

// Decodes GW2-specific context data from the MumbleLink memory
// Returns True if context data is available and valid
function DecodeGw2Context(const Mem: PLinkedMem; out Context: TGw2Context): Boolean;
begin
  Result := (Mem^.contextLen >= SizeOf(TGw2Context));
  if Result then
    Move(Mem^.Context[0], Context, SizeOf(TGw2Context));
end;

// Extracts and formats the server address from context data
// Converts the byte array to a readable string
function GetServerAddress(const Context: TGw2Context): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Context.serverAddress) do
  begin
    if Context.serverAddress[i] = 0 then
      Break;
    Result := Result + Chr(Context.serverAddress[i]);
  end;
end;

// Converts UI state bit flags to a human-readable string
// Each bit in uiState represents a different UI state (map open, in combat, etc.)
function GetUIStateString(const Context: TGw2Context): string;
var
  States: TArray<string>;
begin
  States := [];
  // Check each bit flag and add corresponding state name
  if (Context.uiState and $00000001) <> 0 then States := States + ['MapOpen'];
  if (Context.uiState and $00000002) <> 0 then States := States + ['CompassTopRight'];
  if (Context.uiState and $00000004) <> 0 then States := States + ['CompassRotationEnabled'];
  if (Context.uiState and $00000008) <> 0 then States := States + ['GameFocus'];
  if (Context.uiState and $00000010) <> 0 then States := States + ['CompetitiveMode'];
  if (Context.uiState and $00000020) <> 0 then States := States + ['TextboxFocus'];
  if (Context.uiState and $00000040) <> 0 then States := States + ['InCombat'];
  if (Context.uiState and $00000080) <> 0 then States := States + ['MapWorldView'];
  if (Context.uiState and $00000100) <> 0 then States := States + ['MapSideView'];
  if (Context.uiState and $00000200) <> 0 then States := States + ['MapZoomedIn'];
  if (Context.uiState and $00000400) <> 0 then States := States + ['CharacterNameHidden'];
  if (Context.uiState and $00000800) <> 0 then States := States + ['HideInScreenshots'];
  if (Context.uiState and $00001000) <> 0 then States := States + ['Observer'];
  if (Context.uiState and $00002000) <> 0 then States := States + ['PartySync'];
  if (Context.uiState and $00004000) <> 0 then States := States + ['PvP'];
  if (Context.uiState and $00008000) <> 0 then States := States + ['WvW'];
  if (Context.uiState and $00010000) <> 0 then States := States + ['WvWObjective'];
  if (Context.uiState and $00020000) <> 0 then States := States + ['CombatLocked'];
  if (Context.uiState and $00040000) <> 0 then States := States + ['InGameCutscene'];
  if (Context.uiState and $00080000) <> 0 then States := States + ['ButtonPressed'];
  if (Context.uiState and $00100000) <> 0 then States := States + ['TextboxInput'];
  if (Context.uiState and $00200000) <> 0 then States := States + ['CompassPositionEnabled'];
  if (Context.uiState and $00400000) <> 0 then States := States + ['CompassRotationEnabled'];
  if (Context.uiState and $00800000) <> 0 then States := States + ['CompassAboveMap'];
  if (Context.uiState and $01000000) <> 0 then States := States + ['CompassNavigationEnabled'];
  if (Context.uiState and $02000000) <> 0 then States := States + ['CompassNavigationRotation'];
  if (Context.uiState and $04000000) <> 0 then States := States + ['CompassNavigationPosition'];
  if (Context.uiState and $08000000) <> 0 then States := States + ['CompassNavigationRotation'];
  if (Context.uiState and $10000000) <> 0 then States := States + ['CompassNavigationPosition'];
  if (Context.uiState and $20000000) <> 0 then States := States + ['CompassNavigationRotation'];
  if (Context.uiState and $40000000) <> 0 then States := States + ['CompassNavigationPosition'];
  if (Context.uiState and $80000000) <> 0 then States := States + ['CompassNavigationRotation'];

  if Length(States) = 0 then
    Result := 'None'
  else
    Result := string.Join(', ', States);
end;

// Returns the name of the mount based on its index
// Maps mount indices to their corresponding names in Guild Wars 2
function GetMountName(MountIndex: Byte): string;
const
  MountNames: array[0..9] of string = ('None', 'Jackal', 'Griffon', 'Springer', 'Skimmer', 'Raptor', 'Roller Beetle', 'Warclaw', 'Skyscale', 'Turtle');
begin
  if MountIndex <= High(MountNames) then
    Result := MountNames[MountIndex]
  else
    Result := 'Unknown (' + IntToStr(MountIndex) + ')';
end;

end.
