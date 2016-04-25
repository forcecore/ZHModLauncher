unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LCLType;

// Warning dialog box (helper func)
procedure Warning( t:string );
function path_join( a: string; b:string ): string;

implementation

// show warning message
procedure Warning( t:string );
begin
  Application.MessageBox( PChar(t), '경고', MB_ICONWARNING );
end;

function path_join( a:string; b:string ): string;
begin
  result := IncludeTrailingPathDelimiter( a );
  result += b;
end;

end.

