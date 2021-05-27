unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Windows, jsonConf,
  // my additions
  Common, Settings, BIG_File;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnRun: TButton;
    BtnRunWin: TButton;
    BtnMod: TButton;
    BtnRestore: TButton;
    BtnModOff: TButton;
    BtnModOn: TButton;
    ModList: TComboBox;
    ImgLogo: TImage;
    StatusBar1: TStatusBar;
    procedure BtnModClick(Sender: TObject);
    procedure BtnModOffClick(Sender: TObject);
    procedure BtnModOnClick(Sender: TObject);
    procedure BtnRestoreClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnRunWinClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
    settings: TSettings; // game location + UI settings

    procedure ScanMods();
    function ScanModAI( mod_name: string ): boolean;
    function ScanZbigsForAI( mod_path: string ): boolean;
    procedure ProcessMod( info : TSearchRec );
    procedure ScanScripts(); // scan scripts folder in the game & update UI
    procedure LaunchGame( params: string );

    procedure MakeAIGood( mod_name: string );
    procedure ActivateMod( mod_name: string );
    procedure DeactivateMod();
    procedure UpdateStatusBar();

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
function TFormMain.ScanModAI(mod_name: string): boolean;
var
  mod_path: string;
  tmp: integer;
begin
  result := false; // by default, not have one.

  // First, see the config file first.
  tmp := settings.conf.GetValue( '/has_ai/' + mod_name, -1 );

  // If we get -1 as result, that means,
  // this is the first time scanning this mod.
  if tmp <> -1 then
    result := boolean( tmp )
  else
    begin
      // Find zbigs in the game/Mods/mod_name then move it to game dir.
      mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
        'Mods\' + mod_name + '\';
      result := ScanZbigsForAI( mod_path );

      // write it so that I don't have to do zbig scan again.
      settings.conf.SetValue( '/has_ai/' + mod_name, Integer( result ) );
    end
end;

function TFormMain.ScanZbigsForAI(mod_path: string): boolean;
var
  info: TSearchRec;
  big: TBIGPackage;
  f: TBIGFileUnit;
  i, cnt: integer;
begin
  result := false;
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
                result := true; // found it!!
                break;
              end;
          end;
        end;
      big.Free;
    Until FindNext(info) <> 0;

  SysUtils.FindClose( info );
end;

procedure TFormMain.ProcessMod(info: TSearchRec);
begin
  if (info.Name <> '.') AND (info.Name <> '..') then
  begin
    ScanModAI( info.Name );;
    ModList.AddItem( info.name, nil );
  end;
end;

// Initialization of the program (?),
// at least this form.
procedure TFormMain.FormCreate(Sender: TObject);
begin
  settings := TSettings.create;
  ScanMods();
  ScanScripts();
  UpdateStatusBar();
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  LoadPos();
end;

procedure TFormMain.LaunchGame( params: string );
//var
//  s: ansistring;
begin
  // Just run the game. Don't wait for it.
  // The user knows how and when to actiave or deactivate stuff.
  ShellExecute( 0, nil, PChar(settings.game_exe), PChar(params), nil, SW_NORMAL );
  //RunCommand( settings.game_exe, [params], s );
  //SysUtils.ExecuteProcess(settings.game_exe, params, []);
end;

// Makes sure that mod AI is activated if required.
procedure TFormMain.MakeAIGood( mod_name: string );
var
  has_ai: integer;
begin
  // has AI? If so, turn it on.
  has_ai := settings.conf.GetValue( '/has_ai/' + mod_name, -1 );
  assert( has_ai <> -1 );

  // if has AI and mod ai needs activation...
  if (has_ai <> 0) and (BtnMod.Enabled) then
    BtnModClick( nil )
  else if (has_ai = 0) and (BtnRestore.Enabled) then
    BtnRestoreClick( nil );
end;

procedure TFormMain.ActivateMod( mod_name: string );
var
  i : Integer;
  info : TSearchRec;
  fname, mod_path: string;
  src, dest: string;
  zbigs: TStringList;
begin
  // Find zbigs in the game/Mods/mod_name then move it to game dir.
  mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
    'Mods\' + mod_name + '\';

  zbigs := TStringList.Create;
  If FindFirst( mod_path + '*.zbig', faAnyFile, info ) = 0 then
    Repeat
      // Add them to the mod zbig files list.
      zbigs.Add( info.name );
    Until FindNext(info) <> 0;
  SysUtils.FindClose( info );

  // Remember these settings so that it may be deactivated later,
  // even after the mod launhcher is terminated.
  settings.conf.SetValue( '/current_mod/name', mod_name );
  settings.conf.SetValue( '/current_mod/files/cnt', zbigs.count );
  for i := 0 to zbigs.count-1 do
	begin
	  settings.conf.SetValue( '/current_mod/files/' + IntToStr(i), zbigs[ i ] );
	end;

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

procedure TFormMain.DeactivateMod();
var
  i, cnt : Integer;
  mod_name, fname, mod_path: string;
  src, dest: string;
begin
  // From saved settings, determine what mod is active.
  mod_name := settings.conf.GetValue( '/current_mod/name', '' );
  if length( mod_name ) = 0 then
    exit;

  // Find zbigs in the game/Mods/mod_name then move it to game dir.
  mod_path := IncludeTrailingPathDelimiter(settings.game_dir) +
    'Mods\' + mod_name + '\';

  // Read the settings and move those into the mod dir.
  cnt := settings.conf.GetValue( '/current_mod/files/cnt', 0 );
  for i := 0 to cnt-1 do
  begin
    fname := settings.conf.GetValue( '/current_mod/files/'+IntToStr(i), '' );
    dest := mod_path + fname;
    src := settings.game_dir + fname;
    src := StringReplace( src, '.zbig', '.big', [rfReplaceAll] );
    // Warning( src + ' --> ' + dest );
    MoveFile( PChar(src), PChar(dest) );
  end;

  // Mark as deactive.
  settings.conf.SetValue( '/current_mod/name', '' );
end;

procedure TFormMain.UpdateStatusBar;
var
  mod_name: string;
begin
  mod_name := settings.conf.GetValue( '/current_mod/name', '' );
  if length( mod_name ) = 0 then
    StatusBar1.SimpleText := '모드 꺼짐'
  else
    StatusBar1.SimpleText := mod_name + ' 켜짐';
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
  LaunchGame( '' );
end;

procedure TFormMain.BtnRunWinClick(Sender: TObject);
begin
  LaunchGame( '-win' );
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavePos();
  settings.free;
end;

procedure TFormMain.BtnModClick(Sender: TObject);
begin
  MoveFile( PChar(settings.script), PChar(settings.dscript) );
  ScanScripts();
end;

procedure TFormMain.BtnModOffClick(Sender: TObject);
begin
  DeactivateMod();
  UpdateStatusBar();
end;

procedure TFormMain.BtnModOnClick(Sender: TObject);
begin
  DeactivateMod(); // Make way for the next mod.
  ActivateMod( ModList.Text );
  MakeAIGood( ModList.Text );
  UpdateStatusBar();
end;

procedure TFormMain.BtnRestoreClick(Sender: TObject);
begin
  MoveFile( PChar(settings.dscript), PChar(settings.script) );
  ScanScripts();
end;

initialization
  {$I mainform.lrs}

end.
