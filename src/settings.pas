unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, Registry, jsonConf, Dialogs, Controls,
  Common;

// Class to hold user settings and reading registry.
type

{ TSettings }

 TSettings = class
  private
    json_fname: string;

    procedure GetDirs();
    procedure LoadConf();

    //function GetMyDoc(): string;
    function FindGeneralsExe(): string;
    function GetInstallDir: string;

    // Property getter setters
    function GetGameDir(): string;
    procedure SetGameDir(AValue: string);

  public
    conf: TJsonConfig;
    game_exe: string;
    script: string;
    dscript: string;
    //moddir: string;

    constructor create;
    procedure free;

  property
    game_dir: string read GetGameDir write SetGameDir;
end;

implementation

constructor TSettings.create;
begin
  conf := TJSONConfig.Create(nil);
  LoadConf();
  GetDirs();
end;

procedure TSettings.free;
begin
  conf.Formatted := true;
  conf.free();
end;

procedure TSettings.LoadConf();
var
  config_dir: string;
begin
  // Compute JSON fname
  config_dir := GetAppConfigDir( false );
  // Check if config dir exists, create if not.
  if( not DirectoryExists( config_dir ) ) then
    begin
      CreateDir( config_dir );
    end;

  json_fname := 'mod4_config.json';
  json_fname := path_join(ExtractFileDir(Application.ExeName), json_fname);
  conf.Filename := json_fname; // Created if not exists. Loads it otherwise.
end;

function TSettings.GetInstallDir: string;
var
  filename: string;
  diag: TOpenDialog;
begin
  // Number 1, try registry.
  result := FindGeneralsExe();

  // installdir might point to Generals.exe!
  // If file, FileExists returns true.
  // If dir, returns False.
  if( FileExists( result ) ) then
    begin
      // Ask user confirmation
       if MessageDlg( '확인', '다음 제로아워 경로를 사용하겠습니까?:' + sLineBreak + result,
         mtConfirmation, [mbYes, mbNo],0) = mrYes
       then
         begin
           result := ExtractFilePath( result );
           exit;
         end
       else
         result := ''; // forget what we got
    end;

  // Still, we might have failed to find Generals.
  // Then, let the user find int.
  try
    diag := TOpenDialog.create( nil );
    diag.Options := [ofFileMustExist];
    diag.Filter := 'Exe파일|*.exe';
    if diag.Execute then
      begin
        filename := diag.Filename;
        result := ExtractFilePath( filename );
      end;
  finally
    diag.free;
  end;
end;

procedure TSettings.SetGameDir(AValue: string);
begin
  conf.SetValue( Utf8Decode('game_dir'), Utf8Decode(AValue) );
end;

// get directories we need for the launcher.
procedure TSettings.GetDirs();
var
  //mydoc: string;
  //dataleaf: string;
  installdir: string;
begin
  //mydoc := GetMyDoc();
  //dataleaf := FindGeneralsExe();
  //moddir := mydoc + dataleaf;

  // Well, config load failed. Attempt to read reg or read user input...
  if( game_dir = '' ) then
    begin
      game_dir := GetInstallDir();
    end;

  // we are ready to calculate now.
  installdir := IncludeTrailingPathDelimiter( game_dir );
  game_exe := installdir + 'Generals.exe';
  script := installdir + 'Data\Scripts';
  dscript := installdir + 'Data\_Scripts';

  // game_exe is the only parameter that is not checked by other routines.
  // checking it here...
  if( not FileExists( game_exe ) ) then
    Warning( game_exe + '가 존재하지 않음!' );
end;

function TSettings.GetGameDir(): string;
begin
  result := Utf8Encode( conf.GetValue( 'game_dir', '' ) );
end;

function TryReadRegKey(
  reg: TRegistry;
  key: string;
  subkey: string;
  exe_path: string
): string;
var
  tmp: string;
begin
  if reg.KeyExists(key) then
  begin
    reg.OpenKeyReadOnly(key);
    if reg.ValueExists(subkey) then
      tmp := reg.ReadString(subkey);
    reg.CloseKey();

    tmp := path_join(tmp, exe_path);
    if FileExists(tmp) then
      Result := tmp;
  end;
end;

function ResolveZH(reg: TRegistry): string;
var
  tmp: string;
begin
  // Try the Ultimatate Collection launcher first, if exists.
  Result := TryReadRegKey(reg, 'SOFTWARE\EA Games\Command and Conquer Generals Zero Hour', 'Install Dir', 'Command and Conquer Generals Zero Hour\Generals.exe');
  if Result <> '' then Exit(Result);
  Result := TryReadRegKey(reg, 'SOFTWARE\WOW6432Node\EA Games\Command and Conquer Generals Zero Hour', 'Install Dir', 'Command and Conquer Generals Zero Hour\Generals.exe');
  if Result <> '' then Exit(Result);

  Result := TryReadRegKey(reg, 'SOFTWARE\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour', 'InstallPath', 'Generals.exe');
  if Result <> '' then Exit(Result);
  Result := TryReadRegKey(reg, 'SOFTWARE\WOW6432Node\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour', 'InstallPath', 'Generals.exe');
  if Result <> '' then Exit(Result);

  // Try where mod4.exe is
  tmp := path_join(ExtractFileDir(Application.ExeName), 'Generals.exe');
  if FileExists(tmp) then
    Result := tmp;
end;

// read values from
// SOFTWARE\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour
// SOFTWARE\WOW6432Node\EA Games\Command and Conquer Generals Zero Hour
// ... etc.
function TSettings.FindGeneralsExe(): string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := ResolveZH(reg);
    if Result = '' then
      Warning( '레지스트리로 제로아워 찾기 실패, Generals.exe를 직접 골라주세요.' );
  finally
    reg.Free;
  end;
end;

// get mydocuments
//function TSettings.GetMyDoc(): string;
//var
//  PIDL : PItemIDList;
//  Folder : array[0..MAX_PATH] of Char;
//const
//  // CSIDL_APPDATA  = $001A;
//  CSIDL_PERSONAL = $0005;
//begin
//  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
//  SHGetPathFromIDList(PIDL, Folder);
//  result:=Folder;
//end;

end.

