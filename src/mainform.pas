unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Process, Windows, jsonConf,
  // my additions
  Common, Settings, BIG_File;

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
    procedure FormShow(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
    zbigs: TStringList; // remembers what files are moved to game dir.
    settings: TSettings; // game location + UI settings
    active_mod: string; // remembers what mod is active now.

    procedure ScanMods();
    function HasModAI( mod_name: string ): integer;
    function ScanZbigsForAI( mod_path: string ): integer;
    procedure ProcessMod( info : TSearchRec );
    procedure ScanScripts(); // scan scripts folder in the game & update UI
    procedure LaunchGame( mod_name:string; params: string );

    procedure MakeAIGood( mod_name: string );
    procedure ActivateMod( mod_name: string );
    procedure DeactivateMod( mod_name: string );

    procedure SavePos();
    procedure LoadPos();

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
  if( DirectoryExists( settings.script ) ) then begin
    BtnMod.Enabled := true;
    BtnRestore.Enabled:= false;
  end;

  if( DirectoryExists( settings.dscript ) ) then begin
    BtnMod.Enabled := false;
    BtnRestore.Enabled:= true;
  end;
end;

procedure TFormMain.ImgLogoClick(Sender: TObject);
begin
  // we will open red2.net URL.
  ShellExecute( 0, 'open', 'http://red2.net', nil, nil, SW_SHOWNORMAL );
end;

// Scan mod directory.
procedure TFormMain.ScanMods();
var
  info : TSearchRec;
  mod_dir : string;
begin
  // scan the dirs in gamedir/Mods
  mod_dir := IncludeTrailingPathDelimiter(settings.game_dir)+'Mods';
  If FindFirst( mod_dir + '\*', faAnyFile and faDirectory, info ) = 0 then
    Repeat
      ProcessMod( info );
    Until FindNext(info) <> 0;
  SysUtils.FindClose( info );

  if( ModList.Items.Count = 0 ) then
    Warning( '발견된 모드가 없습니다.' )
  else
    ModList.ItemIndex:= 0; // select 1st item.
end;

// Scan .zbig files in the mod dir and determine if the mod has AI.
function TFormMain.HasModAI(mod_name: string): integer;
var
  mod_path: string;
  mconf: TJSONConfig;
begin
  result := 0; // by default, not have one.

  // First, see the config file first?
  // Find zbigs in the game/Mods/mod_name then move it to game dir.
  mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
    'Mods\' + mod_name + '\';

  mconf := TJSONConfig.Create(nil);
  mconf.Filename := mod_path + 'config.json';
  result := mconf.GetValue( 'has_ai', -1 );

  // If we get -1 as result, that means,
  // this is the first time scanning this mod.
  if result = -1 then
    begin
      result := ScanZbigsForAI( mod_path );

      // write it so that I don't have to do zbig scan again.
      mconf.SetValue( 'has_ai', result );
    end;
  // else, the result read from the config is correct.

  mconf.free;
end;

function TFormMain.ScanZbigsForAI(mod_path: string): integer;
var
  info: TSearchRec;
  big: TBIGPackage;
  f: TBIGFileUnit;
  i, cnt: integer;
begin
  result := 0;
  If FindFirst( mod_path + '*.zbig', faAnyFile, info ) = 0 then

    Repeat
      // Scan the contents
      big := TBIGPackage.Create;
      big.LoadFile( mod_path + info.name );
      if big.IsValid then
        begin
          // scan through file list, find AI related files.
          cnt := big.GetNumFiles;
          for i := 0 to cnt-1 do
          begin
            f := big.GetFileInfo( i );
            if pos( 'skirmishscripts', lowercase( f.filename ) ) <> 0 then
              begin
                result := 1; // found it!!
                break;
              end;
          end;
        end;
      big.Free;
    Until FindNext(info) <> 0;

  SysUtils.FindClose( info );
end;

procedure TFormMain.ProcessMod(info: TSearchRec);
var
  has_ai: integer; // I want this to be boolean but I can't
  // Cant make TObject contain boolean...
begin
  if (info.Name <> '.') AND (info.Name <> '..') then
  begin
    //ModList.Items.Add( info.Name );
    has_ai := HasModAI( info.Name );;
    ModList.AddItem( info.name, TObject( has_ai ) );
  end;
end;

// Initialization of the program (?),
// at least this form.
procedure TFormMain.FormCreate(Sender: TObject);
begin
  zbigs := TStringList.create;
  settings := TSettings.create;
  ScanMods();
  ScanScripts();
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  LoadPos();
end;

// Launch the game and wait for it to finish.
procedure TFormMain.LaunchGame( mod_name: string; params: string );
var
  output: string;
begin
  // Set the mod as active
  ActivateMod( mod_name );

  MakeAIGood( mod_name );

  // Run the mod
  //ShellExecute( 0, 'open', PChar(game_exe), PChar(params), nil, SW_NORMAL );
  output := '';
  RunCommand( settings.game_exe, params, output );

  // Wait for it to finish
  DeactivateMod( mod_name );
end;

procedure TFormMain.MakeAIGood( mod_name: string );
var
  has_ai: integer;
begin
  // Make AI good by activating the scripts folder in ZH dir.
  has_ai := integer( ModList.Items.Objects[ ModList.ItemIndex ] );

  // if has AI and mod ai needs activation...
  if (has_ai <> 0) and (BtnMod.Enabled) then
    BtnMod.Click
  else if (has_ai = 0) and (BtnRestore.Enabled) then
    BtnRestore.Click;
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
  mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
    'Mods\' + mod_name + '\';

  If FindFirst( mod_path + '*.zbig', faAnyFile, info ) = 0 then
    Repeat
      // Add them to the mod zbig files list.
      zbigs.Add( info.name );
    Until FindNext(info) <> 0;
  SysUtils.FindClose( info );

  // For each items in zbigs, move the file to the game dir.
  for i := 0 to zbigs.Count-1 do
  begin
    fname := zbigs[ i ];
    src := mod_path + fname;
    dest := settings.game_dir + fname;
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
  mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
    'Mods\' + mod_name + '\';

    // For each items in zbigs, move the file to the game dir.
  for i := 0 to zbigs.Count-1 do
  begin
    fname := zbigs[ i ];
    dest := mod_path + fname;
    src := settings.game_dir + fname;
    src := StringReplace( src, '.zbig', '.big', [rfReplaceAll] );
    //Warning( src + ' --> ' + dest );
    MoveFile( PChar(src), PChar(dest) );
  end;

  zbigs.clear;
end;

procedure TFormMain.SavePos;
begin
  settings.conf.SetValue( '/dialog/left', Left );
  settings.conf.SetValue( '/dialog/top', Top );
end;

procedure TFormMain.LoadPos;
begin
  Left := settings.conf.GetValue( '/dialog/left', Left );
  Top := settings.conf.GetValue( '/dialog/top', Top );
  SetBounds( Left, Top, Width, Height );
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
  SavePos();
  zbigs.free;
  settings.free;
end;

procedure TFormMain.BtnModClick(Sender: TObject);
begin
  MoveFile( PChar(settings.script), PChar(settings.dscript) );
  ScanScripts();
end;

procedure TFormMain.BtnRestoreClick(Sender: TObject);
begin
  MoveFile( PChar(settings.dscript), PChar(settings.script) );
  ScanScripts();
end;

initialization
  {$I mainform.lrs}

end.

