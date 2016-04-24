unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  // my additions
  Windows, Registry, Process;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnRun: TButton;
    BtnRunWin: TButton;
    BtnMod: TButton;
    BtnRestore: TButton;
    ModList: TComboBox;
    ImgLogo: TImage;
    procedure BtnModClick(Sender: TObject);
    procedure BtnRestoreClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnRunWinClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
    // private variable for convenience
    script: string;
    dscript: string;
    game_dir: string;
    game_exe: string;
    //moddir: string;

    // Moved files for running mod
    zbigs: TStringList; // remembers what files are moved to game dir.
    active_mod: string; // remembers what mod is active now.

    // jdj's own procedures
    procedure ScanMods();
    procedure GetDirs();
    //function GetMyDoc(): string;
    function GenReg( t: string ): string;
    procedure Warning( t: string );
    procedure ScanScripts();
    procedure LaunchGame( mod_name:string; params: string );

    procedure ActivateMod( mod_name: string );
    procedure DeactivateMod( mod_name: string );

  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

// see if scripts dir exests or _scripts exists.
// set button states correctly.
procedure TFormMain.ScanScripts();
begin
  if( DirectoryExists( script ) ) then begin
    BtnMod.Enabled := true;
    BtnRestore.Enabled:= false;
  end;

  if( DirectoryExists( dscript ) ) then begin
    BtnMod.Enabled := false;
    BtnRestore.Enabled:= true;
  end;
end;

procedure TFormMain.ImgLogoClick(Sender: TObject);
begin
  // we will open red2.net URL.
  ShellExecute( 0, 'open', 'http://red2.net', nil, nil, SW_SHOWNORMAL );
end;

// show warning message
procedure TFormMain.Warning( t:string );
begin
  Application.MessageBox( PChar(t), '경고', MB_ICONWARNING );
end;

// Scan mod directory.
procedure TFormMain.ScanMods();
var
  info : TSearchRec;
  mod_dir : string;
begin
  // scan the dirs in gamedir/Mods
  mod_dir := IncludeTrailingPathDelimiter(game_dir)+'Mods';
  If FindFirst( mod_dir + '\*', faAnyFile and faDirectory, info ) = 0 then
    begin
    Repeat
      if (info.Name <> '.') AND (info.Name <> '..') then
        begin
          ModList.Items.Add( info.Name );
        end;
    Until FindNext(info) <> 0;
    end;
  SysUtils.FindClose( info );

  if( ModList.Items.Count = 0 ) then
    Warning( '발견된 모드가 없습니다.' )
  else
    ModList.ItemIndex:= 0; // select 1st item.
end;

// read values from
// SOFTWARE\Electronic Arts\EA Games\Command and Conquer Generals Zero Hour
function TFormMain.GenReg( t: string ): string;
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

// get directories we need for the launcher.
procedure TFormMain.GetDirs();
var
  //mydoc: string;
  //dataleaf: string;
  installdir: string;
begin
  //mydoc := GetMyDoc();
  //dataleaf := GenReg( 'UserDataLeafName' );
  installdir := GenReg( 'InstallPath' );
  if( FileExists( installdir ) ) then
    begin
      // installdir might contain Generals.exe!
      // If file, FileExists returns true.
      // If dir, returns False.
      installdir := ExtractFilePath( installdir );
    end;

  // we are ready to calculate now.
  //mydoc := IncludeTrailingPathDelimiter( mydoc );
  //moddir := mydoc + dataleaf;

  installdir := IncludeTrailingPathDelimiter( installdir );
  game_dir := installdir;
  game_exe := installdir + 'generals.exe';
  script := installdir + 'Data\Scripts';
  dscript := installdir + 'Data\_Scripts';

  // game_exe is the only parameter that is not checked by other routines.
  // checking it here...
  if( not FileExists( game_exe ) ) then
    Warning( game_exe + '가 존재하지 않음!' );
end;

// get mydocuments
//function TFormMain.GetMyDoc(): string;
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

// Initialization of the program (?),
// at least this form.
procedure TFormMain.FormCreate(Sender: TObject);
begin
  zbigs := TStringList.create;
  GetDirs();
  ScanMods();
  ScanScripts();
end;

// Launch the game and wait for it to finish.
procedure TFormMain.LaunchGame( mod_name: string; params: string );
var
  output: string;
begin
  // Set the mod as active
  ActivateMod( mod_name );

  // Run the mod
  //ShellExecute( 0, 'open', PChar(game_exe), PChar(params), nil, SW_NORMAL );
  output := '';
  RunCommand( game_exe, params, output );

  // Wait for it to finish
  DeactivateMod( mod_name );
end;

procedure TFormMain.ActivateMod( mod_name: string );
var
  i : Integer;
  info : TSearchRec;
  fname, mod_path: string;
  src, dest: string;
begin
  active_mod := ModList.Text;

  // Find zbigs in the game/Mods/mod_name then move it to game dir.
  mod_path := IncludeTrailingPathDelimiter(game_dir) +
    'Mods\' + mod_name + '\';

  If FindFirst( mod_path + '*.zbig', faAnyFile, info ) = 0 then
    begin
    Repeat
      // Add them to the mod zbig files list.
      zbigs.Add( info.name );
    Until FindNext(info) <> 0;
    end;
  SysUtils.FindClose( info );

  // For each items in zbigs, move the file to the game dir.
  for i := 0 to zbigs.Count-1 do
  begin
    fname := zbigs[ i ];
    src := mod_path + fname;
    dest := game_dir + fname;
    dest := StringReplace( dest, '.zbig', '.big', [rfReplaceAll] );
    MoveFile( PChar(src), PChar(dest) );
  end;
end;

procedure TFormMain.DeactivateMod( mod_name: string );
var
  i : Integer;
  fname, mod_path: string;
  src, dest: string;
begin
  active_mod := '';

  // Find zbigs in the game/Mods/mod_name then move it to game dir.
  mod_path := IncludeTrailingPathDelimiter(game_dir) +
    'Mods\' + mod_name + '\';

    // For each items in zbigs, move the file to the game dir.
  for i := 0 to zbigs.Count-1 do
  begin
    fname := zbigs[ i ];
    dest := mod_path + fname;
    src := game_dir + fname;
    src := StringReplace( src, '.zbig', '.big', [rfReplaceAll] );
    //Warning( src + ' --> ' + dest );
    MoveFile( PChar(src), PChar(dest) );
  end;

  zbigs.clear;
end;

procedure TFormMain.BtnRunClick(Sender: TObject);
begin
  LaunchGame( ModList.Text, '' );
end;

procedure TFormMain.BtnRunWinClick(Sender: TObject);
begin
  LaunchGame( ModList.Text, '-win' );
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  zbigs.free;
end;

procedure TFormMain.BtnModClick(Sender: TObject);
begin
  MoveFile( PChar(script), PChar(dscript) );
  ScanScripts();
end;

procedure TFormMain.BtnRestoreClick(Sender: TObject);
begin
  MoveFile( PChar(dscript), PChar(script) );
  ScanScripts();
end;

initialization
  {$I mainform.lrs}

end.

