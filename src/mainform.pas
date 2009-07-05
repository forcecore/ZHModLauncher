unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Windows;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnRun: TButton;
    BtnRunWin: TButton;
    BtnMod: TButton;
    BtnRestore: TButton;
    ModList: TComboBox;
    ImgLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
    // jdj's own procedures
    procedure LoadLogo();
    procedure ScanMod();
    function GetMyDoc(): string;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.ImgLogoClick(Sender: TObject);
begin
  // we will open red2.net URL.
end;

// Load Logo Image
procedure TFormMain.LoadLogo();
begin
  // we need to this, to properly use the transparency.
  ImgLogo.Transparent:= true;
  ImgLogo.Picture.PNG.LoadFromFile('logo.png');
end;

// Scan mod directory.
procedure TFormMain.ScanMod();
var
  mydoc: string;
begin
  mydoc := GetMyDoc();
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
  ScanMod();
end;

initialization
  {$I mainform.lrs}

end.

