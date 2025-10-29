program DriveHandler;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, ufmDrives, uHotkey, uVarious, UniqueInstanceRaw
 { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  if not InstanceRunning('DriveHandler') then begin
    Application.Initialize;
    Application.CreateForm(TfmDrives, fmDrives);
    Application.Run;
  end;
end.

