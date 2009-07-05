unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnRun: TButton;
    BtnRunWin: TButton;
    BtnMod: TButton;
    BtnRestore: TButton;
    ComboBox1: TComboBox;
    ImgLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
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

// Initialization of the program (?),
// at least this form.
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Load Logo Image
  ImgLogo.Transparent:= true;
  ImgLogo.Picture.PNG.LoadFromFile('logo.png');
end;

initialization
  {$I mainform.lrs}

end.

