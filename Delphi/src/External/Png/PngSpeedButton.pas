unit PngSpeedButton;

interface

uses
  Windows, Classes, Buttons, pngimage, PngFunctions;

type
  TPngSpeedButton = class(TSpeedButton)
  private
    FPngImage: TPngImage;
    FPngOptions: TPngOptions;
    FImageFromAction: Boolean;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure CreatePngGlyph;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PngImage: TPngImage read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    
    // 2015-11-19 /gsv/: Glyph und NumGlyphs in der dfm-Datei speicherbar (Begr�ndung s. Loaded) (stored false entfernt)
    property Glyph;
    property NumGlyphs;
  end;

implementation

uses
  Graphics, ActnList, PngButtonFunctions;

{ TPngSpeedButton }

constructor TPngSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPngImage := TPngImage.Create;
  FPngOptions := [pngBlendOnDisabled];
  FImageFromAction := False;
end;

destructor TPngSpeedButton.Destroy;
begin
  FPngImage.Free;
  inherited Destroy;
end;

procedure TPngSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do begin
      //Copy image from action's imagelist
      if (PngImage.Empty or FImageFromAction) and (ActionList <> nil) and
        (ActionList.Images <> nil) and (ImageIndex >= 0) and (ImageIndex <
        ActionList.Images.Count) then begin
        CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
        CreatePngGlyph;
        FImageFromAction := True;
      end;
    end;
end;

procedure TPngSpeedButton.Paint;
var
  PaintRect: TRect;
  GlyphPos, TextPos: TPoint;
begin
  inherited Paint;

  // 2015-12-09 /who/ Wenn kein PngImage gesetzt ist (Empty = true) soll nicht gezeichnet werden
  if ((FPngImage <> nil) and not FPngImage.Empty) then 
  begin
    //Calculate the position of the PNG glyph
    CalcButtonLayout(Canvas, FPngImage, ClientRect, FState = bsDown, Down,
      Caption, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0));
    PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);

    if csLoading in ComponentState then Exit;

    if Enabled then
      DrawPNG(FPngImage, Canvas, PaintRect, [])
    else
      DrawPNG(FPngImage, Canvas, PaintRect, FPngOptions);
  end;
end;

procedure TPngSpeedButton.Loaded;
begin
  inherited Loaded;
  
  // 2015-11-19 /gsv/: Wenn das Glyph in der dfm-Datei gesetzt ist und
  // das PngImage nicht gesetzt ist, dann das Glyph aus der dfm-Datei beibehalten.
  // Andernfalls hat das PngImage h�here Prio!
  if ( ( (not Assigned ( FPngImage ) ) or
         FPngImage.Empty ) and
         Assigned ( Glyph ) and (not Glyph.Empty) ) then
  begin
    // Glyph gesetzt, aber PngImage nicht ==> Glyph beibehalten
  end
  else
  begin
    // Glyph nicht gesetzt bzw. PngImage gesetzt ==> PngImage �bernehmen
    CreatePngGlyph();
  end;   
end;

function TPngSpeedButton.PngImageStored: Boolean;
begin
  Result := not FImageFromAction;
end;

procedure TPngSpeedButton.SetPngImage(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then begin
    FPngImage.Free;
    FPngImage := TPngImage.Create;
  end
  else
    FPngImage.Assign(Value);

  //To work around the gamma-problem
  with FPngImage do
    if Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE] then
      Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));

  FImageFromAction := False;
  CreatePngGlyph;
  Repaint;
end;

procedure TPngSpeedButton.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then begin
    FPngOptions := Value;
    CreatePngGlyph;
    Repaint;
  end;
end;

procedure TPngSpeedButton.CreatePngGlyph;
var
  Bmp: TBitmap;
begin
  //Create an empty glyph, just to align the text correctly
  Bmp := TBitmap.Create;
  try
    Bmp.Width := FPngImage.Width;
    Bmp.Height := FPngImage.Height;
    Bmp.Canvas.Brush.Color := clBtnFace;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    Glyph.Assign(Bmp);
    NumGlyphs := 1;
  finally
    Bmp.Free;
  end;
end;

end.