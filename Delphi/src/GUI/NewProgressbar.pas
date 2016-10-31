unit NewProgressbar;

{ tCustomProgressbar }
{ (c) 1998 Ulf Matthiesen, Bültenweg 91, 38106 Braunschweig                    }
{ -----------------------------------------------------------------------------}
{ FUNKTION:                                                                    }
{ Zeigt einen kontinuierlichen oder unterteilten Fortschrittsbalken an, sowie  }
{ den Fortschritt als Prozentzahl oder als Wert zwischen den beliebig festleg- }
{ baren Eigenschaften MIN und MAX                                              }
{                                                                              }
{ Dokumentation in mtx.hlp                                                                             }
{                                                                              }
{                                                                              }
{ letzte Aenderung:  InitProgress() eingefuehrt von Steffen Henker, 23.06.2003 }             


(*
  2011-08-03 : uma, lzh, aha: Neue Eigenschaft MaxNumBoxes: für Bugfix
                              Fortschrittsanzeige bei vielen Prüfplätzen!

*)



interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls;


type

  tGetTextStringEvent = Procedure(Value: LongInt; Var S :String) of object;

  tTextKind  = (tkNone, tkPercent, tkMinMax);
  tTextAlign = (taLeft, taCenter, taRight);


  tCustomProgressbar = class(TGraphicControl)
  private
    FMinValue   : Longint;
    FMaxValue   : Longint;
    FCurValue   : Longint;
    FCtl3D      : Boolean;
    FNumBoxes   : LongInt;
    FBorderStyle: TBorderStyle;
    FForeColor  : TColor;
    FBackColor  : TColor;
    FTextAlign  : tTextAlign;
    FTextKind   : tTextKind;
    FOnDrawText : tGetTextStringEvent;
    procedure PaintInnerBar(InnerImage: TBitmap);
    procedure PaintText(AnImage: TBitmap; InnerRect: tRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetForeColor(Value: TColor);
    procedure SetBackColor(Value: TColor);
    Procedure SetTextAlign(aAlign: tTextAlign);
    procedure SetTextKind(aKind: tTextKind);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    Procedure SetNumBoxes(Value: LongInt);
    procedure SetProgress(Value: Longint);
    procedure SetCtl3D(Value: Boolean);
    function GetPercentDone: Longint;
  protected
    procedure Paint; override;
    Function GetTextString: String;  virtual;
    Function BoxColorOf(aBox: Integer): tColor; virtual;
    Function GetMaxNumBoxes: LongInt;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddProgress(Value: Longint);
    procedure InitProgress(ValueMax: Longint);
    property PercentDone: Longint read GetPercentDone;
  published
    property Align;
    property Color;
    property Ctl3D: Boolean read FCtl3D write SetCtl3D;
    property Enabled;
    property Font;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ForeColor: TColor read FForeColor write SetForeColor default clBlack;
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 100;
    property NumBoxes: LongInt read FNumBoxes write SetNumBoxes default 0;
    property MaxNumBoxes: LongInt read GetMaxNumBoxes;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Progress: Longint read FCurValue write SetProgress;
    property ShowHint;
    property TextAlign: tTextAlign read FTextAlign write SetTextAlign;
    property TextKind : tTextKind read FTextKind write SetTextKind;
    property Visible;

    property OnDrawText: tGetTextStringEvent read FOnDrawText write FOnDrawText;
  end;

procedure Register;

implementation

uses Consts;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := Trunc( Z * (Y * 0.01) );
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0
  else Result := Trunc( (X * 100.0) / Z );
end;

{ tCustomProgressbar }

constructor tCustomProgressbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  FMinValue     := 0;
  FMaxValue     := 100;
  FCurValue     := 0;
  FCtl3D        := True;
  FTextAlign    := taCenter;
  FTextKind     := tkPercent;
  FNumBoxes     := 0;
  FBorderStyle  := bsSingle;
  FForeColor    := clBlue;
  FBackColor    := clWhite;
  Font.Color    := clBlue;
  Width := 100;
  Height := 30;
end;

function tCustomProgressbar.BoxColorOf;
begin
  Result:= ForeColor;
end;

function tCustomProgressbar.GetPercentDone: Longint;
begin
  Result := SolveForY(FCurValue - FMinValue, FMaxValue - FMinValue);
end;

function tCustomProgressbar.GetTextString: String;
var F : String;
    V : LongInt;
begin
  v:=0;
  Case FTextKind of
    tkPercent : Begin
                  F:= ' %d%% ';
                  V:= PercentDone;
                End;
    tkMinMax  : Begin
                  F:= ' %d ';
                  V:= Progress;
                End;
  End;
  Result := Format(F, [V]);
  IF Assigned(OnDrawText) Then OnDrawText(Progress, Result);
end;

procedure tCustomProgressbar.Paint;
var PaintBM   : tBitmap;
    InnerBM   : tBitmap;
    Co1, Co2  : tColor;
    InnerRect : tRect;

  procedure BevelRect(aCanvas: tCanvas; const R: TRect);
  begin
    With aCanvas do Begin
      Pen.Color := Co1;
      PolyLine([Point(R.Left, R.Bottom-1), Point(R.Left, R.Top),
        Point(R.Right-1, R.Top)]);
      Pen.Color := Co2;
      PolyLine([Point(R.Right-1, R.Top), Point(R.Right-1, R.Bottom-1),
        Point(R.Left, R.Bottom-1)]);
    end;
  end;

begin
  PaintBM:= tBitmap.Create;
  Try
    PaintBM.Width := Width;
    PaintBM.Height:= Height;
    With InnerRect Do Begin
      Left  := 0;
      Top   := 0;
      Right := Left + Self.Width;
      Bottom:= Top + Self.Height;
    End;
    With PaintBM.Canvas do Begin
      IF BorderStyle = bsSingle Then
        { draw border only if not bsNone selected }
        IF Ctl3D Then Begin
          { 3D-Border, lowered }
          Co1 := clBtnShadow; Co2 := clBtnHighlight;
          BevelRect(PaintBM.Canvas, Rect(0, 0, Width, Height));
          Co1 := cl3DDkShadow;     Co2 := cl3DLight;
          BevelRect(PaintBM.Canvas, Rect(1, 1, Width-1, Height-1));
          InflateRect(InnerRect, -2, -2); End
        Else Begin
          { Single Border }
          Co1:= clWindowFrame; Co2:= clWindowFrame;
          BevelRect(PaintBM.Canvas, Rect(0, 0, Width, Height));
          InflateRect(InnerRect, -1, -1);
        End;
      InnerBM:= tBitmap.Create;
      try
        InnerBM.Width := InnerRect.Right  - InnerRect.Left;
        InnerBM.Height:= InnerRect.Bottom - InnerRect.Top;
        PaintInnerBar(InnerBM);
        PaintBM.Canvas.Draw(InnerRect.Left, InnerRect.Top, InnerBM);
      finally
        InnerBM.Free;
      End;
      IF TextKind <> tkNone Then PaintText(PaintBM, InnerRect);
    End;
    Canvas.Draw(0, 0, PaintBM);
  Finally
    PaintBM.Free;
  end;
end;

procedure tCustomProgressbar.PaintText;
var S     : string;
    X, Y  : Integer;
    BM    : tBitmap;

  function Min(A, B : Integer): Integer;
  begin
    IF A > B Then Result:= B
             Else Result:= A;
  end;

begin
  BM := tBitmap.Create;
  X:=0;
  try
    With BM.Canvas Do Begin
      S:= GetTextString;
      Font := Self.Font;
      BM.Width:=  Min(TextWidth(S),  InnerRect.Right - InnerRect.Left);
      BM.Height:= Min(TextHeight(S), InnerRect.Bottom - InnerRect.Top);
      Brush.Color := clBlack;
      FillRect(Rect(0, 0, BM.Width, BM.Height));
      BM.Canvas.Font.Color := Self.Font.Color xor BackColor;
      With AnImage Do Begin
        Y := (Height - BM.Height) div 2;
        Case FTextAlign of
          taLeft   :  X:= InnerRect.Left;
          taCenter :  X:= (Width  - BM.Width) div 2;
          taRight  :  X:= Width - BM.Width;
        End;
      End;
      TextOut(0, 0, S);
    End;
    AnImage.Canvas.CopyMode := cmSrcInvert;
    AnImage.Canvas.Draw(X, Y, BM);
  finally
    BM.Free;
  end;
end;

procedure tCustomProgressbar.PaintInnerBar;
var
  FillSize : Longint;
  W, H, i  : Integer;
  ShowBoxes: LongInt;
begin
  W := InnerImage.Width;
  H := InnerImage.Height;
  with InnerImage.Canvas do
  begin
    Brush.Color := BackColor;
    FillRect(Rect(0, 0, Width, Height));
    Pen.Color := ForeColor;
    Pen.Width := 1;
    Brush.Color := ForeColor;
    if NumBoxes = 0 then
    begin
      FillSize := SolveForX(PercentDone, W);
      if FillSize > W then FillSize := W;
      if FillSize > 0 then FillRect(Rect(0, 0, FillSize, H));
    end
    else
    begin
      ShowBoxes:= Trunc((FCurValue - MinValue) / (MaxValue - MinValue) * NumBoxes);
      for i:= 0 to ShowBoxes-1 do
      begin
         Brush.Color := BoxColorOf(i+1);
         FillRect(Rect(Round( i   *(InnerImage.Width / NumBoxes))+1, 1,
                       Round((i+1)*(InnerImage.Width / NumBoxes))-1, H-1));
      end;
    end;
  end;
end;

procedure tCustomProgressbar.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Refresh;
  end;
end;

procedure tCustomProgressbar.SetForeColor(Value: TColor);
begin
  if Value <> FForeColor then
  begin
    FForeColor := Value;
    Refresh;
  end;
end;

procedure tCustomProgressbar.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Refresh;
  end;
end;

procedure tCustomProgressbar.SetCtl3D(Value: Boolean);
begin
  FCtl3D:= Value;
  Refresh;
end;

procedure tCustomProgressbar.SetMinValue(Value: Longint);
begin
  if Value <> FMinValue then
  begin
    if Value > FMaxValue then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMaxValue - 1]);
    FMinValue := Value;
    if FCurValue < Value then FCurValue := Value;
    Refresh;
  end;
end;

procedure tCustomProgressbar.SetMaxValue(Value: Longint);
begin
  if Value <> FMaxValue then
  begin
    if Value < FMinValue then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [FMinValue + 1, MaxInt]);
    FMaxValue := Value;
    if FCurValue > Value then FCurValue := Value;
    Refresh;
  end;
end;
(*
procedure tCustomProgressbar.SetNumBoxes(Value: LongInt);
var MaxNums : LongInt;
    W       : LongInt;
begin
  if Value <> FNumBoxes then
  begin
    W:= Width;
    if BorderStyle = bsSingle then
    begin
      Dec(W, 2);
      IF Ctl3D Then Dec(W, 2);
    end;
    MaxNums:= (W div 4);
    if Value > MaxNums then
      FNumBoxes:= MaxNums
    else
      FNumBoxes:= Value;
    Refresh;
  end;
end;
*)
procedure tCustomProgressbar.SetNumBoxes(Value: LongInt);
begin
  if Value <> FNumBoxes then
  begin
    if Value > MaxNumBoxes then
      FNumBoxes:= GetMaxNumBoxes
    else
      FNumBoxes:= Value;
    Refresh;
  end;
end;

function tCustomProgressbar.GetMaxNumBoxes: LongInt;
var W       : LongInt;
begin
  W:= Width;
  if BorderStyle = bsSingle then
  begin
    Dec(W, 2);
    IF Ctl3D Then Dec(W, 2);
  end;
  result:= (W div 4);
end;

procedure tCustomProgressbar.SetProgress(Value: Longint);
var
  TempFill: Longint;
begin
  TempFill := SolveForX(PercentDone, Width); { remember where we were }
  if Value < FMinValue then
    Value := FMinValue
  else if Value > FMaxValue then
    Value := FMaxValue;
  if FCurValue <> Value then
  begin
    FCurValue := Value;
    if TempFill <> SolveForX(PercentDone, Width) then { only refresh if percentage changed }
      Refresh;
  end;
end;

procedure tCustomProgressbar.AddProgress(Value: Longint);
begin
  Progress := FCurValue + Value;
  Refresh;
end;

procedure tCustomProgressbar.SetTextAlign(aAlign: tTextAlign);
begin
  if aAlign <> FTextAlign then Begin
    FTextAlign:= aAlign;
    Refresh;
  end;
end;

procedure tCustomProgressbar.SetTextKind(aKind: tTextKind);
begin
  if aKind <> FTextKind then
  begin
    FTextKind:= aKind;
    Refresh;
  end;
end;

// wird von jedem Pruefschritt benutzt
procedure tCustomProgressbar.InitProgress(ValueMax: Longint);
begin
  MaxValue := ValueMax;
  Progress := $00;
end;

procedure Register;
begin
  RegisterComponents('Metronix', [tCustomProgressbar]);
end;


end.

