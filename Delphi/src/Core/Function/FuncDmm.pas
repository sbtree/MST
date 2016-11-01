// =============================================================================
// Module name  : $RCSfile: FuncDmm.pas,v $
// description  : The classes of script functions which are relevant to the device
//                of Digital Multimeter (DMM), are implemented in this unit.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncDmm;

interface
uses Classes, FuncBase, Multimeter, TextMessage;
type
//1. Measure
  TDmmFunction = class(TFunctionBase)
  protected
    d_range:double;
    t_dmm:  TMultimeter;
  protected
    //procedure SetDevMultimeter(const dmm: TMultimeter); virtual;
    procedure SetFunctionActors(const fntactors: TFunctionActors); override;
    function DoMeasurement(const meas: EMeasureAction): boolean;
  public
    constructor Create(); override;
    //property DevMultimeter: TMultimeter read t_dmm write SetDevMultimeter;
    function LoadParameters(const pars: TStrings): boolean; override;
  end;

  MeasureDCV = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureDCI = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureACV = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureACI = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureR = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureF = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureP = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

  MeasureT = class(TDmmFunction)
  public
    function DoTask(): boolean; override;
  end;

implementation
uses SysUtils, StrUtils, GenUtils;

{procedure TDmmFunction.SetDevMultimeter(const dmm: TMultimeter);
begin
  t_dmm := dmm;
end; }

procedure TDmmFunction.SetFunctionActors(const fntactors: TFunctionActors);
begin
  t_fctactors := fntactors;
  if assigned(t_fctactors) then t_dmm := TMultimeter(t_fctactors.ActorObject[FA_DMM])
  else t_dmm := nil;
end;

function TDmmFunction.DoMeasurement(const meas: EMeasureAction): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(meas, d_range);
    case meas of
      MA_RES: result := t_dmm.MeasureR(d_val);
      MA_DCV: result := t_dmm.MeasureDCV(d_val);
      MA_ACV: result := t_dmm.MeasureACV(d_val);
      MA_DCI: result := t_dmm.MeasureDCI(d_val);
      MA_ACI: result := t_dmm.MeasureACI(d_val);
      MA_FREQ: result := t_dmm.MeasureF(d_val);
      MA_PERI: result := t_dmm.MeasureP(d_val);
      MA_TEMP: result := t_dmm.MeasureT(d_val);
    end;
    v_resval := d_val;
  end else
    t_msgrimpl.AddMessage('A multimeter must be given for this function.', ML_ERROR);
end;

constructor TDmmFunction.Create();
begin
  inherited Create();
  d_range := -1.0;
end;

function TDmmFunction.LoadParameters(const pars: TStrings): boolean;
var s_par: string;
begin
  result := false;
  if (pars.Count <= 1) then begin
    result := true;
    if (pars.Count = 1) then begin
      s_par := TGenUtils.ReplaceDecimalSeparator(pars[0]);// ReplaceStr(pars[0], '.', DecimalSeparator);
      result := TryStrToFloat(s_par, d_range);
      if (not result) then d_range := -1.0; //set negative value for automatical range
    end;
  end;
end;

function MeasureDCV.DoTask(): boolean;
begin
  result := DoMeasurement(MA_DCV);
end;

function MeasureDCI.DoTask(): boolean;
begin
  result := DoMeasurement(MA_DCI);
end;

function MeasureACV.DoTask(): boolean;
begin
  result := DoMeasurement(MA_ACV);
end;

function MeasureACI.DoTask(): boolean;
begin
  result := DoMeasurement(MA_ACI);
end;

function MeasureF.DoTask(): boolean;
begin
  result := DoMeasurement(MA_FREQ);
end;

function MeasureP.DoTask(): boolean;
begin
  result := DoMeasurement(MA_PERI);
end;

function MeasureR.DoTask(): boolean;
begin
  result := DoMeasurement(MA_RES);
end;

function MeasureT.DoTask(): boolean;
begin
  result := DoMeasurement(MA_TEMP);
end;

initialization
  Classes.RegisterClass(MeasureDCV);
  Classes.RegisterClass(MeasureDCI);
  Classes.RegisterClass(MeasureACV);
  Classes.RegisterClass(MeasureACI);
  Classes.RegisterClass(MeasureF);
  Classes.RegisterClass(MeasureP);
  Classes.RegisterClass(MeasureR);
  Classes.RegisterClass(MeasureT);

finalization
  Classes.UnregisterClass(MeasureDCV);
  Classes.UnregisterClass(MeasureDCI);
  Classes.UnregisterClass(MeasureACV);
  Classes.UnregisterClass(MeasureACI);
  Classes.UnregisterClass(MeasureF);
  Classes.UnregisterClass(MeasureP);
  Classes.UnregisterClass(MeasureR);
  Classes.UnregisterClass(MeasureT);

end.
