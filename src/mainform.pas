unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  // my additions
  Windows, Registry, Dos;

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
    procedure FormCreate(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
    // private variable for convenience
    script: string;
    dscript: string;
    game_exe: string;
    moddir: string;
    // jdj's own procedures
    procedure LoadLogo();
    procedure ScanMod();
    procedure GetDirs();
    function GetMyDoc(): string;
    function GenReg( t: string ): string;
    procedure Warning( t: string );
    procedure ScanScripts();
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

// Load Logo Image
procedure TFormMain.LoadLogo();
begin
  // we need to this, to properly use the transparency.
  ImgLogo.Transparent:= true;
  ImgLogo.Picture.PNG.LoadFromFile('logo.png');
end;

// show warning message
procedure TFormMain.Warning( t:string );
begin
  Application.MessageBox( PChar(t), '경고', MB_ICONWARNING );
end;

// Scan mod directory.
procedure TFormMain.ScanMod();
var
  Dir : SearchRec;
begin
  // scan the list
  FindFirst(IncludeTrailingPathDelimiter(moddir)+'*.big', archive, Dir);
  while (DosError=0) do
   begin
     ModList.Items.Add( Dir.Name );
     //Writeln(Dir.Name+Space(40-Length(Dir.Name)),Dir.Size:9);
     FindNext(Dir);
   end;
  FindClose(Dir);

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
  mydoc: string;
  dataleaf: string;
  installdir: string;
begin
  mydoc := GetMyDoc();
  dataleaf := GenReg( 'UserDataLeafName' );
  installdir := GenReg( 'InstallPath' );

  // we are ready to calculate now.
  mydoc := IncludeTrailingPathDelimiter( mydoc );
  moddir := mydoc + dataleaf;

  installdir := IncludeTrailingPathDelimiter( installdir );
  game_exe := installdir + 'generals.exe';
  script := installdir + 'Data\Scripts';
  dscript := installdir + 'Data\_Scripts';
end;

// get mydocuments
function TFormMain.GetMyDoc(): string;
var
  PIDL : PItemIDList;
  Folder : array[0..MAX_PATH] of Char;
const
  // CSIDL_APPDATA  = $001A;
  CSIDL_PERSONAL = $0005;
begin
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  result:=Folder;
end;

// Initialization of the program (?),
// at least this form.
procedure TFormMain.FormCreate(Sender: TObject);
begin
  LoadLogo();
  GetDirs();
  ScanMod();
  ScanScripts();
end;

procedure TFormMain.BtnRunClick(Sender: TObject);
begin
  // run the game with -mod parm.
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

procedure TFormMain.BtnRunWinClick(Sender: TObject);
begin
  // run the game with -mod and -win param.
end;

initialization
  {$I mainform.lrs}

end.

