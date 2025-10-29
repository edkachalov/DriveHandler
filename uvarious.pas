unit uVarious;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils
 {$IfDef WINDOWS}
 , windows
 {$EndIf};

procedure SetTranslucent(ThehWnd: Longint; Color: Longint; nTrans: Integer);

implementation

procedure SetTranslucent(ThehWnd: Longint; Color: Longint; nTrans: Integer);
var
  attrib: longint;
begin
  {$IfDef WINDOWS}
  {SetWindowLong and SetLayeredWindowAttributes are API functions, see MSDN for details }
  attrib := GetWindowLongA(ThehWnd, GWL_EXSTYLE);
  SetWindowLong(ThehWnd, GWL_EXSTYLE, attrib Or WS_EX_LAYERED);
  {anything with color value color will completely disappear if flag = 1 or flag = 3  }
  SetLayeredWindowAttributes (ThehWnd, Color, nTrans, 1);
  {$EndIf}
end;

end.

