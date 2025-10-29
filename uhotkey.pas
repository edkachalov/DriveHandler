unit uHotkey;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, windows;

const
  MY_ID = 13;

var
  H: HANDLE; {set this before StartHotkey}
  //App: TApplication;
  FM: TForm;
  PrevWndProc: WNDPROC;

procedure StartHotkey;
procedure StopHotkey;

implementation

function WndCallback(hW: HWND; uMsg: UINT; wP: WParam; lP: LParam): LRESULT; stdcall;
begin
  if (uMsg=WM_HOTKEY) and (wP=MY_ID) then begin
    //if Application.Active
    //  then Application.Minimize
    //  else Application.Restore;
      if FM.Visible then begin
        if FM.Active
          then FM.Visible:= False
          else FM.BringToFront;
      end else
        FM.Visible:= True;

      //FM.Visible:= not FM.Visible;
      //Application.Restore;
    end;
  Result:= CallWindowProc(PrevWndProc,hW, uMsg, wP, lP);
end;

procedure StartHotkey;
begin
  PrevWndProc:= Windows.WNDPROC(SetWindowLongPtr(H, GWL_WNDPROC, PtrInt(@WndCallback)));
  RegisterHotKey(H, MY_ID, MOD_ALT, VK_E);
end;

procedure StopHotkey;
begin
  UnRegisterHotkey(H, MY_ID);
end;

end.

