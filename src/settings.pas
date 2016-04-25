unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, jsonConf, Dialogs,
  Common;

// Class to hold user settings and reading registry.
type

{ TSettings }

 TSettings = class
  private
    conf: TJsonConfig;
    json_fname: string;

    procedure GetDirs();
    procedure LoadConf();

    //function GetMyDoc(): string;
    function GenReg( t: string ): string;
    function GetInstallDir(): string;

    // Property getter setters
    function GetGameDir(): string;
    procedure SetGameDir(AValue: string);

  public
    game_exe: string;
    script: string;
    dscript: string;
    //moddir: string;

  property
    game_dir: string read GetGameDir write SetGameDir;
    constructor create();
    destructor destroy();
end;

implementation

constructor TSettings.create();
begin
  conf := TJSONConfig.Create(nil);
  LoadConf();
  GetDirs();
end;

destructor TSettings.destroy();
begin
  conf.free();
end;

procedure TSettings.LoadConf();
var
  config_dir: string;
begin
  // Compute JSON fname
  config_dir := GetAppConfigDir( false );
  // Check if config dir exists, create if not.
  if( DirectoryExists( config_dir ) ) then
    begin
      CreateDir( config_dir );
    end;

  json_fname := 'config.json';
  json_fname := path_join( config_dir, json_fname );
  if( FileExists( json_fname ) ) then
    begin
      // Load it
      conf.Filename := json_fname;
    end;
end;

function TSettings.GetInstallDir(): string;
var
  filename: string;
  diag: TOpenDialog;
begin
  // Number 1, try registry.
  result := GenReg( 'InstallPath' );

  // installdir might point to Generals.exe!
  // If file, FileExists returns true.
  // If dir, returns False.
  if( FileExists( result ) ) then
    begin
      result := ExtractFilePath( result );
    end;

  // Still, we might have failed to find Generals.
  // Then, let the user decide.
  if( result = '' ) then
    begin
      try
        diag := TOpenDialog.create( nil );
        diag.Options := [ofFileMustExist];
        diag.Filter := 'Exe파일|*.exe';
        if diag.Execute then
          begin
            filename := diag.Filename;
            ShowMessage(filename);
          end;
      finally
        diag.free;
      end;
    end;
end;

procedure TSettings.SetGameDir(AValue: string);
begin
  conf.SetValue( 'game_dir', AValue );
end;

// get directories we need for the launcher.
procedure TSettings.GetDirs();
var
  //mydoc: string;
  //dataleaf: string;
  installdir: string;
begin
  //mydoc := GetMyDoc();
  //dataleaf := GenReg( 'UserDataLeafName' );
  //moddir := mydoc + dataleaf;

  // Well, attempt to read reg if game_dir is empty.
  if( game_dir = '' ) then
    begin
      game_dir := GetInstallDir();
    end;

  // Attempt to read registry for installdir
  // we are ready to calculate now.
  installdir := IncludeTrailingPathDelimiter( game_dir );
  game_exe := installdir + 'generals.exe';
  script := installdir + 'Data\Scripts';
  dscript := installdir + 'Data\_Scripts';

  // game_exe is the only parameter that is not checked by other routines.
  // checking it here...
  if( not FileExists( game_exe ) ) then
    Warning( game_exe + '가 존재하지 않음!' );
end;

function TSettings.GetGameDir(): string;
begin
  result := conf.GetValue( 'game_dir', '' );
end;

// read values from
// SOFTWARE\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour
function TSettings.GenReg( t: string ): string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('SOFTWARE\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour', False);
    if reg.ValueExists( t ) then
      Result := reg.ReadString( t )
    else begin
      Result := '';
      Warning( '제로아워 레지스트리값 ' + t + ' 읽기 실패' );
    end;
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
