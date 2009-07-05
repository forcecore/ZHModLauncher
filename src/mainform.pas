unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnRun: TButton;
    BtnRunWin: TButton;
    BtnMod: TButton;
    BtnRestore: TButton;
    ComboBox1: TComboBox;
    ImgLogo: TImage;
    procedure ImgLogoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ImgLogoClick(Sender: TObject);
begin
  // we will open red2.net URL.
end;

initialization
  {$I mainform.lrs}

end.

