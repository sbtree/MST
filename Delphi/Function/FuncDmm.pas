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
    procedure SetDevMultimeter(const dmm: TMultimeter); virtual;
  public
    constructor Create(); override;
    property DevMultimeter: TMultimeter read t_dmm write SetDevMultimeter;
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
uses SysUtils, StrUtils;

procedure TDmmFunction.SetDevMultimeter(const dmm: TMultimeter);
begin
  t_dmm := dmm;
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
      s_par := ReplaceStr(pars[0], '.', DecimalSeparator);
      result := TryStrToFloat(s_par, d_range);
      if (not result) then d_range := -1.0; //set negative value
    end;
  end;
end;

function MeasureDCV.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(MA_DCV, d_range);
    result := t_dmm.MeasureDCV(d_val);
    //todo:
  end;
end;

function MeasureDCI.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(MA_DCI, d_range);
    result := t_dmm.MeasureDCI(d_val);
    //todo:
  end;
end;

function MeasureACV.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(MA_ACV, d_range);
    result := t_dmm.MeasureACV(d_val);
    //todo:
  end;
end;

function MeasureACI.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(MA_ACI, d_range);
    result := t_dmm.MeasureACI(d_val);
    //todo:
  end;
end;

function MeasureF.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    result := t_dmm.MeasureF(d_val);
    //todo:
  end;
end;

function MeasureP.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    result := t_dmm.MeasureP(d_val);
    //todo:
  end;
end;

function MeasureR.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    t_dmm.SetMeasureRange(MA_RES, d_range);
    result := t_dmm.MeasureR(d_val);
    //todo:
  end;
end;

function MeasureT.DoTask(): boolean;
var d_val: double;
begin
  result := false;
  if assigned(t_dmm) then begin
    result := t_dmm.MeasureT(d_val);
    //todo:
  end;
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
