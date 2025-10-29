unit ufmRunner;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
 fpjson, jsonparser, process, LCLType;

const
  cConfigActions = 'actions.json';

type

 { TMyAction }

 TMyAction = object
   Name: String[63];
   Exe,
   WorkDir:  String;
   Params: TStringList;
   Priority: TProcessPriority;
   procedure Free;
 end;
 PMyAction = ^TMyAction;

 { TfmRunner }

 TfmRunner = class(TForm)
   sgRun: TStringGrid;
   procedure FormCreate(Sender: TObject);
   procedure FormDestroy(Sender: TObject);
   procedure sgRunKeyPress(Sender: TObject; var Key: char);
 private
   FActionFile: String;
   FActions: TList;
   FCMD: TProcess;
   function ReadConfig(Path: String): Boolean;
   function WriteConfig(Path: String): Boolean;
   procedure Draw(sg: TStringGrid);
 public

 end;

var
 fmRunner: TfmRunner;

implementation

uses uVarious;

{$R *.lfm}

function SetProcess(P: TProcess; const A: TMyAction): Boolean;
begin
  with A do begin
    P.Executable:= Exe;
    P.Priority:= Priority;
    if Assigned(Params)
      then P.Parameters.Assign(Params)
      else P.Parameters.Clear;
    if WorkDir = ''
      then P.CurrentDirectory:= ExtractFileDir(Exe)
      else P.CurrentDirectory:= WorkDir;
  end;
end;

function SListToJSON(sl: TStringList): TJSONArray;
var
  i: Integer;
begin
  Result:= TJSONArray.Create;
  for i:= 0 to sl.Count - 1 do
    Result.Add(sl[i]);
end;

function JSONToStr(A: TList; FormatedJSON: Boolean = True): String;
var
  jArr: TJSONArray;
  jObj: TJSONObject;
  i: Integer;
begin
  jArr:= TJSONArray.Create;

  for i:= 0 to A.Count - 1 do begin
    jObj:= TJSONObject.Create;
    with TMyAction( A[i]^ ) do begin
      jObj.Add('Name', Name);
      jObj.Add('WorkDir', WorkDir);
      jObj.Add('Exe',  Exe);
      jObj.Add('Params', SListToJSON(Params) );
      jArr.Add(jObj);
    end;
  end;

  if FormatedJSON
    then Result:= jArr.FormatJSON
    else Result:= jArr.AsJSON;
  jArr.Free;
end;

function JSONToSList(json: TJSONArray): TStringList;
var
  i: Integer;
begin
  if json.Count <> 0 then begin
    Result:= TStringList.Create;
    for i:= 0 to Pred(json.Count) do
      Result.Append(json.Items[i].AsString);
  end else
    Result:= nil;
end;

{convert JSON string to native array}
function StrToJSON(S: String): TList;
var
  jArr: TJSONArray;
  jValue: TJSONObject;
  i: Integer;
begin
  ShowMessage(S);
  Result:= TList.Create;
  jArr:= TJSONArray(GetJSON(S));
  Result.Count:= jArr.Count;

  for i:= 0 to jArr.Count-1 do begin
    Result[i]:= AllocMem(SizeOf(TMyAction));
    jValue:= TJSONObject(jArr.Items[i]);
    if Assigned(jValue.FindPath('Exe')) then
      with TMyAction(Result[i]^) do begin
        Exe:= jValue.FindPath('Exe').AsString;
        if Assigned(jValue.FindPath('Params')) then begin
          Params:= JSONToSList(TJSONArray( jValue.FindPath('Params') ));
        end;

        if Assigned(jValue.FindPath('WorkDir'))
          then WorkDir:= jValue.FindPath('WorkDir').AsString;

        if Assigned(jValue.FindPath('Priority'))
          then Priority:= TProcessPriority( jValue.FindPath('Priority').AsInteger )
          else Priority:= ppNormal;

      end;
  end;
  jArr.Free;
end;

{ TMyAction }

procedure TMyAction.Free;
begin
  Name:= '';
  Exe := '';
  Params.Free;
end;


{ TfmRunner }

procedure TfmRunner.FormCreate(Sender: TObject);
begin
  {$IfDef WINDOWS}
  SetTranslucent(Handle, Color, 0);
  {$EndIf}
  FActionFile:= ExtractFileDir(ParamStr(0)) + DirectorySeparator + cConfigActions;

  FCMD:= TProcess.Create(nil);
  FCMD.Options:= FCMD.Options - [poWaitOnExit];

  ReadConfig(FActionFile);
  Draw(sgRun);
end;

procedure TfmRunner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCMD);
end;

procedure TfmRunner.sgRunKeyPress(Sender: TObject; var Key: char);
var
  i: Integer;
begin
  case Byte(UpCase( Key )) of
  //VK_ESCAPE: Hide;
  VK_A..VK_Z: begin
    i:= Byte(UpCase( Key )) - VK_A;
    if i < FActions.Count then begin
      SetProcess(FCMD, TMyAction( FActions[i]^ ));
      FCMD.Execute;
      //Hide;
    end;
  end;
  end;
end;

function TfmRunner.ReadConfig(Path: String): Boolean;
begin
  with TStringList.Create do begin
    Result:= False;
    LoadFromFile(Path);
    FActions:= StrToJSON(Text);
    Free;
    Result:= True;
  end;
end;

function TfmRunner.WriteConfig(Path: String): Boolean;
begin
  with TStringList.Create do begin
    Result:= False;
    Text:= JSONToStr(FActions);
    SaveToFile(Path);
    Free;
    Result:= True;
  end;
end;

procedure TfmRunner.Draw(sg: TStringGrid);
var
  i: Integer;
begin
  sg.RowCount:= FActions.Count + 1;
  //sg.ColCount:= 3;
  for i:= 0 to FActions.Count - 1 do begin
    with TMyAction( FActions[i]^ ) do begin
      if Name <> ''
        then sg.Cells[1,i+1]:= Name
        else sg.Cells[1,i+1]:= Exe;
      sg.Cells[0,i+1]:= IntToStr(i) + ' ' + Char(ord('A') + i);
    end;
  end;
end;

end.

