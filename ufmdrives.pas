unit ufmDrives;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, windows,
 ExtCtrls, StdCtrls, process, fpjson, jsonparser;

const
  cBytesInMByte = 0.000001;
  cBytesInGByte = 0.000000001;
  cPanelHeight = 90;
  cPanelWidth  = 960 div 4;
  cBackgroundColor = TColor($107F10);
  cLetterColor = TColor($00FF00);

type
 TDriveSetting = object
   Size, MaxSize: Int64;
   Letter: Char;
   DriveType: Byte;
   _Reserved: array [0..5] of Byte;
 end;
 TDriveSetting32 = array [0..31] of TDriveSetting;

 { TDrivePanel }

 TDrivePanel = object
   Panel: TPanel;
   Image: TImage;
   lLetter,
   lSize,
   lMaxSize: TLabel;
   procedure Create(Parent: TWinControl = nil);
 end;

 TDrivePanel32 = array [0..31] of TDrivePanel;

 { TfmDrives }

 TfmDrives = class(TForm)
  pMark: TPanel;
  cmd: TProcess;
  RefreshTimer: TTimer;
  procedure FormActivate(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormDeactivate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FormHide(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormShow(Sender: TObject);
  procedure RefreshTimerTimer(Sender: TObject);

 private
  FDrivePanels: TDrivePanel32;
  FDriveSettings: TDriveSetting32;
  FDrivesIgnore: LongInt;
  FConfigFile: String;
  procedure CreatePanels;
  {return number of drives}
  procedure RefreshDrivePanels;
  procedure ReadConfig;
  procedure ParseParams;
 public

 end;

var
 fmDrives: TfmDrives;

implementation

uses uHotkey, uVarious;

{$R *.lfm}

function Read(aLetter: Char; var DS: TDriveSetting): Integer;inline;
var
  S: String[7];
begin
  S:= ' :\';
  S[4]:= Char(0);
  with DS do begin
    Letter:= aLetter;
    S[1]:=   aLetter;
    GetDiskFreeSpaceEx(@S[1], @Size, @MaxSize, nil);
    DriveType:= GetDriveType(@S[1]);
  end;
end;

function ReadDriveSettings(var Sett: TDriveSetting32; aIgnoreDrives: LongInt): Integer;
var
  dr, i: Integer;
  DS: TDriveSetting;
begin
{$IfDef WINDOWS}
  dr:= GetLogicalDrives and not aIgnoreDrives;
  Result:= 0;

  while dr <> 0 do begin
    i:= BsfDWord(dr);
    Read(Char(ord('A') + i), Sett[Result]); {gathering data for specific disk}
    dr:= dr and (dr - 1);
    inc(Result);
  end;
{$EndIf}
end;

function SetDrivePanel(var DP: TDrivePanel; const DS: TDriveSetting): Integer;
const
  cBackGroundColor: array[0..6]of TColor = (
    TColor($101010),
    TColor($202020),
    clRed,
    TColor($107F10),
    clYellow,
    clMaroon,
    clBlue);
begin
  with DP do begin
    lLetter.Caption:=  DS.Letter;
    lSize.Caption:=    IntToStr(Trunc( DS.Size * cBytesInMByte )) + ' M';
    lMaxSize.Caption:= FormatFloat('#.##', DS.MaxSize * cBytesInGByte);

    if Image.Canvas.Pixels[0,0] <> cBackGroundColor[DS.DriveType] then begin
      Image.Canvas.Brush.Color:= cBackGroundColor[DS.DriveType];
      Image.Canvas.Rectangle(0,0,Image.Width,Image.Height);
    end;
  end;
end;

{ TDrivePanel }

procedure TDrivePanel.Create(Parent: TWinControl);
const
  cPanelFont = 'Courier';
  cFontSizeBig = 28;
  cFontSizeSmall = 18;
  cLeftGap = 16;
var
  i: Integer;
begin
  Panel:= TPanel.Create(Parent);
  Image:= TImage.Create(nil);
  lSize:= TLabel.Create(nil);
  lMaxSize:= Tlabel.Create(nil);
  lLetter:= TLabel.Create(nil);

  Panel.Parent:= Parent;
  Panel.Width:= cPanelWidth;
  Panel.Height:= cPanelHeight;

  Image.Top:=    0;
  Image.Left:=   0;
  Image.Width:=  Panel.Width;
  Image.Height:= Panel.Height;
  Image.Canvas.Brush.Color:= clWindow;
  Image.Canvas.Brush.Style:= bsFDiagonal;
  Image.Canvas.Rectangle(0,0,cPanelWidth,cPanelHeight);

    //Panel.Canvas.Pen.Color:= clWhite;
    //Panel.Canvas.Line(0, 0, 50, 50);
    //Panel.Canvas.TextOut(10, 10, 'lalal');

  Image.Parent:= Panel;
  lSize.Parent:= Panel;
  lMaxSize.Parent:= Panel;
  lLetter.Parent:= Panel;

  lLetter.Font.Name:= cPanelFont;
  lLetter.Font.Size:= cFontSizeBig;
  lLetter.Font.Color:= cLetterColor;
  lLetter.Left:= cLeftGap;
  lLetter.AnchorVerticalCenterTo(Panel);

  lSize.Font.Name:= cPanelFont;
  lSize.Font.Size:= cFontSizeSmall;
  lSize.Font.Color:= clHighlight;
  i:= lSize.Canvas.TextHeight('');
  lSize.Top:= 5 + lSize.Canvas.TextHeight(''); {top position}
  lSize.Left:= cLeftGap + Panel.Canvas.TextWidth('A') + cLeftGap + 8;

  lMaxSize.Font.Name:= cPanelFont;
  lMaxSize.Font.Size:= cFontSizeSmall - 6;
  lMaxSize.Font.Color:= clHighlight;
  lMaxSize.Alignment:= taRightJustify;
  //lMaxSize.Top:= 50;
  lMaxSize.Left:= 50;
  lMaxSize.AutoSize:= False;
  lMaxSize.Width:= (cPanelWidth div 4) * 3;
    lMaxSize.Height:= lMaxSize.Canvas.TextHeight('A');
  lMaxSize.Left:= Panel.Width - lMaxSize.Width;
  lMaxSize.Top:= Panel.Height - 5 - lMaxSize.Canvas.TextHeight('A'); {Bottom position}
  //lMaxSize.Left:= cLeftGap + Panel.Canvas.TextWidth('A') + cLeftGap;

  //lSize.Caption:= '20';
  //lMaxSize.Caption:= '100';
  //lLetter.Caption:= Char(Ord('A') + Random(26));
end;

{ TfmDrives }

procedure TfmDrives.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Height:= cPanelHeight * Length(FDrivePanels) div 4;
  Width:= cPanelWidth * 4;

  for i:= 0 to High(FDrivePanels) do begin
    FDrivePanels[i].Create(self);
    FDrivePanels[i].Panel.Left:= (i mod 4) * cPanelWidth;
    FDrivePanels[i].Panel.Top:=  (i div 4) * cPanelHeight;
  end;
  ReadDriveSettings(FDriveSettings, FDrivesIgnore);

  {uncomment to make form transparent}
  SetTranslucent(Handle, Color, 0);

  uHotkey.H:= Handle;
  uHotkey.FM:= Self;
  StartHotkey;

  with pMark do begin
    BringToFront;
    Width:=  cPanelWidth * 4;
    Height:= cPanelHeight div 16;
    Color:=  cBackgroundColor;
    //Brush.Style:= bsFDiagonal;
    Brush.Color:= cLetterColor;
  end;
  Application.OnActivate:=   @FormActivate;
  Application.OnDeactivate:= @FormDeactivate;

  ParseParams;
end;

procedure TfmDrives.FormDeactivate(Sender: TObject);
begin
  pMark.Visible:= False;
end;

procedure TfmDrives.FormActivate(Sender: TObject);
begin
  pMark.Top:=  Self.Height - pMark.Height;
  pMark.Left:= (Self.Width - pMark.Width) div 2;
  pMark.Visible:= True;
end;

procedure TfmDrives.FormDestroy(Sender: TObject);
begin
  StopHotkey;
end;

procedure TfmDrives.FormHide(Sender: TObject);
begin
  RefreshTimer.Enabled:= fmDrives.Visible;
end;

procedure TfmDrives.FormKeyDown(Sender: TObject; var Key: Word;
 Shift: TShiftState);
var
  i: Integer;
begin
  OnKeyDown:= nil;
  case Word(UpCase( WideChar( Key ))) of
  VK_ESCAPE: Hide;
  VK_A..VK_Z: begin
    i:= GetLogicalDrives;
    if (i and (1 shl (Byte(Key) - VK_A))) <> 0 then begin
      cmd.Executable:= 'explorer.exe';
      cmd.Parameters.Clear;
      cmd.Parameters.Add(Char(Key) + ':\');
      cmd.Execute;
      Hide;
    end;
  end;
  //else ShowMessage(WideChar(Key) + ' ' + IntToStr(Key));
  end;
  OnKeyDown:= @FormKeyDown;
end;

procedure TfmDrives.FormShow(Sender: TObject);
begin
  RefreshTimer.Enabled:= fmDrives.Visible;
  RefreshTimerTimer(nil); {imediately refresh}
end;

procedure TfmDrives.RefreshTimerTimer(Sender: TObject);
begin
  RefreshDrivePanels;
end;

procedure TfmDrives.CreatePanels;
var
  i: Integer;
begin
  for i:= Low(FDrivePanels) to High(FDrivePanels) do begin
    //FDriveSettings[i].;
  end;
end;

procedure TfmDrives.RefreshDrivePanels;
var
  i: Integer;
begin
  i:= ReadDriveSettings(FDriveSettings, FDrivesIgnore);
  for i:= 0 to i do begin
    SetDrivePanel(FDrivePanels[i], FDriveSettings[i]);
    FDrivePanels[i].Panel.Visible:= True;
  end;
  Height:= ((i + 3) div 4) * cPanelHeight;
  Position:= poScreenCenter;
  for i:= i to High(FDrivePanels) do
    FDrivePanels[i].Panel.Visible:= False;
end;

procedure TfmDrives.ReadConfig;
begin
  { TODO -oEd : Read JSON file }
end;

procedure TfmDrives.ParseParams;
var
  Param, S: String;
  i, j: Integer;
begin
  for i:= 1 to ParamCount do begin
    Param:= ParamStr(i);
    case Param[1] of
      '-': case Param[2] of
        'c': begin
          S:= Copy(Param, 3, 65536);
          if FileExists(S)
            then FConfigFile:= S;
          {Read Config}
        end;
        {settings}
        's': case Param[3] of
          'd': begin {ignored drives}
            S:= LowerCase(Copy(Param, 4, 65536));
            for j:= 1 to Length(S) do
              if S[j] in ['a'..'z']
                then FDrivesIgnore:= FDrivesIgnore or 1 shl Byte(Byte(S[j]) - ord('a'))
                else {error, can't be unalphabetic chars};
          end;
        end;
      end;
    end;
  end;
end;

end.

