// =============================================================================
// Module name  : $RCSfile: FuncSys.pas,v $
// description  : The classes of script functions which are relevant to the
//                system of this program (SYS), are implemented in this unit, e.g.
//                Jump-Function, Repeat-Controll etw.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncSys;

interface
uses Classes, FuncBase, TextMessage, StepGroup, FuncAux;

type
  IStepControlImpl = interface
    function GotoStep(var stepnr: string): boolean;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TStepControlImpl = class(TInterfacedObject, IStepControlImpl)
  protected
    t_group: TStepGroup;
  public
    constructor Create();
    destructor Destroy; override;

    function GotoStep(var stepnr: string): boolean;
    function NextStep(): boolean;
    function CurStepNr(): string;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TConditionControl = class(TFunctionBase, IStepControlImpl, IExprParserImpl)
  protected
    t_sctrlimpl:  TStepControlImpl;
    t_parserimpl: TExprParserImpl;
    s_expr:     string; //conditional expression
    s_selfsnr:  string; //current step number
    s_tosnr:    string; //step number of destination
    b_valid:    boolean;  //indicates expected value of the condition
  protected
    function ValidStepNrs(const bforward: boolean = true): boolean; virtual;
    procedure AssignStepGroup();
    procedure SetFunctionActors(const fntactors: TFunctionActors); override;

  public
    constructor Create(); override;
    destructor Destroy; override;

    function DoTask(): boolean; override;
    function BooleanCondition(const expr: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; override;
    property StepControl: TStepControlImpl read t_sctrlimpl implements IStepControlImpl;
    property ParserService: TExprParserImpl read t_parserimpl implements IExprParserImpl;
  end;

  JumpIfFalse = class(TConditionControl)
  public
    function DoTask(): boolean; override;
  end;

  JumpIfTrue = class(JumpIfFalse)
  public
    constructor Create(); override;
  end;

  TRepeatControl = class(TConditionControl)
  protected
    c_elapse: cardinal; //initial value of timeout in millisecond
    c_tend:   cardinal; //end time
    i_ninit:  integer;  //initial number of repeating counter
    i_counter:integer;  //repeat counter
    b_reset:  boolean;  //indicates whether the repeating is restarted
  protected

  public
    constructor Create(); override;
    destructor Destroy; override;

    function LoadParameters(const pars: TStrings): boolean; override;
    function DoTask(): boolean; override;
    property ResetCondition: boolean read b_reset write b_reset;
    property CountRepeat: integer read i_counter;
  end;

  RepeatIfTrue = class(TRepeatControl)
  public
    constructor Create(); override;
  end;

  RepeatIfFalse = class(TRepeatControl)
  end;

implementation
uses Windows, SysUtils, StrUtils, Contnrs, GenUtils;
const
  CINT_ELAPSE_REPEAT = 10000; //default time for repeating

constructor TStepControlImpl.Create();
begin
  inherited Create();
  t_group := nil;
end;
destructor TStepControlImpl.Destroy;
begin
  inherited Destroy();
end;

function TStepControlImpl.GotoStep(var stepnr: string): boolean;
var i_idx: integer;
begin
  result := false;
  if assigned(t_group) then begin
    i_idx := t_group.IndexOfStep(stepnr);
    result := t_group.GotoStepIndex(i_idx);
  end;
end;

function TStepControlImpl.NextStep(): boolean;
begin
  result := false;
  if assigned(t_group) then result := assigned(t_group.NextStep);
end;

function TStepControlImpl.CurStepNr(): string;
begin
  result := '';
  if assigned(t_group) then
    result := t_group.CurStepNr();
end;

function TStepControlImpl.GetStepGroup(): TStepGroup;
begin
  result := t_group;
end;

procedure TStepControlImpl.SetStepGroup(const group: TStepGroup);
begin
  t_group := group;
end;

function TConditionControl.ValidStepNrs(const bforward: boolean): boolean;
var f_ssnr, f_tsnr: single; s_ssnr, s_tsnr: string;
begin
  //Jump is allowed only to forward
  s_ssnr := TGenUtils.ReplaceDecimalSeparator(s_selfsnr); //ReplaceStr(s_selfsnr, '.', DecimalSeparator);
  s_tsnr := TGenUtils.ReplaceDecimalSeparator(s_tosnr); //ReplaceStr(s_tosnr, '.', DecimalSeparator);
  result := (TryStrToFloat(s_ssnr, f_ssnr) and TryStrToFloat(s_tsnr, f_tsnr));
  if result then begin
    f_ssnr := abs(f_ssnr);
    f_tsnr := abs(f_tsnr);
    if bforward then result := (f_tsnr >= f_ssnr)
    else result := (f_tsnr <= f_ssnr);
    if (not result) then t_msgrimpl.AddMessage(format('The target step(%s) is not allowed from current step(%s).',[s_tosnr, s_selfsnr]), ML_ERROR);
  end else t_msgrimpl.AddMessage(format('The target step(%s) or current step(%s) is invalid', [s_tosnr, s_selfsnr]), ML_ERROR);
end;

procedure TConditionControl.AssignStepGroup();
begin
  if assigned(t_fntactors) then t_sctrlimpl.CurStepGroup := TStepGroup(t_fntactors.ActorObject[FA_TSEQ])
  else t_sctrlimpl.CurStepGroup := nil;
end;

procedure TConditionControl.SetFunctionActors(const fntactors: TFunctionActors);
begin
  t_fntactors := fntactors;
  AssignStepGroup();
end;

constructor TConditionControl.Create();
begin
  inherited Create();
  t_sctrlimpl := TStepControlImpl.Create();
  t_parserimpl := TExprParserImpl.Create();
  b_valid := false;
end;

destructor TConditionControl.Destroy;
begin
  t_parserimpl.Free();
  t_sctrlimpl.Free();
  inherited Destroy();
end;

function TConditionControl.BooleanCondition(const expr: string): boolean;
begin
  t_parserimpl.EvalAsBoolean(expr, result);
end;

function TConditionControl.DoTask(): boolean;
begin
  s_selfsnr := t_sctrlimpl.CurStepNr();
  result := (s_selfsnr <> '');
  if (not result) then t_msgrimpl.AddMessage('Failed to get current test step.', ML_ERROR);  
end;

function TConditionControl.LoadParameters(const pars: TStrings): boolean;
var s_par: string;
begin
  result := false;
  if (pars.Count >= 2) then begin
    s_expr := pars[0];
    //todo: replace #Pseudo
    //s_expr := '1'; //test
    s_par := pars[1];
    if StartsText('PS_', s_par) then
      s_tosnr := MidStr(s_par, 4, length(s_par) - 3)
    else
      s_tosnr := s_par;
    result := true;
  end;
end;

function JumpIfFalse.DoTask(): boolean;
begin
  result := inherited DoTask();
  if (result and ValidStepNrs(true)) then begin
    if (BooleanCondition(s_expr) = b_valid) then begin
      t_msgrimpl.AddMessage(format('The jumping condition is fulfilled( (%s) = %s ).', [s_expr, BoolToStr(b_valid, true)]));
      result := t_sctrlimpl.GotoStep(s_tosnr);
      if result then t_msgrimpl.AddMessage(format('Successful to jump from step %s to step %s', [s_selfsnr, s_tosnr]))
      else t_msgrimpl.AddMessage(format('Failed to jump from step %s to step %s', [s_selfsnr, s_tosnr]), ML_ERROR);
    end else begin
      t_msgrimpl.AddMessage(format('The jumping condition is not fulfilled( (%s) <> %s ).', [s_expr, BoolToStr(b_valid, true)]));
      result := t_sctrlimpl.NextStep();
      if result then t_msgrimpl.AddMessage('Successful to go to next step automatically')
      else t_msgrimpl.AddMessage('Failed to go to next step.', ML_ERROR);
    end;
  end;
end;

constructor JumpIfTrue.Create();
begin
  inherited Create();
  b_valid := true;;
end;

constructor TRepeatControl.Create();
begin
  inherited Create();
  b_reset := true;
  c_elapse := CINT_ELAPSE_REPEAT;
  i_ninit := -1;
  i_counter := 0;
end;

destructor TRepeatControl.Destroy;
begin
  inherited Destroy();
end;

function TRepeatControl.LoadParameters(const pars: TStrings): boolean;
var i, i_val: integer; s_sub: string; s_par: string;
const CSTR_INVALID_PARA = 'Found invalid number in parameter "%s"';
begin
  result := inherited LoadParameters(pars);
  if (result and (pars.Count > 2)) then begin
    for i := 2 to pars.Count - 1 do begin
      s_par := pars[i];
      if StartsText('TIMEOUT_', s_par) then begin
        s_sub := MidStr(s_par, 9, length(s_par) - 8);
        if TryStrToInt(s_sub,i_val) then begin
          if (i_val >= 0) then c_elapse := i_val
          else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
        end else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
      end else if StartsText('REPEAT_', s_par) then begin
        s_sub := MidStr(s_par, 8, length(s_par) - 7);
        if TryStrToInt(s_sub,i_val) then begin
          if (i_val >= 0) then i_ninit := i_val
          else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
        end else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
      end;
    end;
  end;
end;

function TRepeatControl.DoTask(): boolean;
var b_dorepeat, b_cond: boolean;
begin
  result := inherited DoTask();
  if b_reset then begin
    c_tend := GetTickCount() + c_elapse;
    i_counter := 0;
    b_reset := false;
  end;
  Inc(i_counter);
  if (result and ValidStepNrs(false)) then begin
    if (i_ninit >= 0) then begin
      b_dorepeat := (i_counter < i_ninit);
      if (not b_dorepeat) then t_msgrimpl.AddMessage(format('Repeating counter (%d) is reached.', [i_counter]));
    end else begin
      b_dorepeat := (GetTickCount() <= c_tend);
      if (not b_dorepeat) then t_msgrimpl.AddMessage(format('Timeout (%d ms) is reached.', [c_elapse]));
    end;
    b_cond := (BooleanCondition(s_expr) = b_valid);
    if (not b_cond) then t_msgrimpl.AddMessage(format('The repeating condition is not fulfilled( (%s) <> %s ).', [s_expr, BoolToStr(b_valid, true)]));

    b_dorepeat := (b_dorepeat and b_cond);
    if b_dorepeat then begin
      result := t_sctrlimpl.GotoStep(s_tosnr);
      if result then t_msgrimpl.AddMessage(format('Successful to go from step %s to step %s', [s_selfsnr, s_tosnr]))
      else begin
        b_reset := true; //ready for next run
        t_msgrimpl.AddMessage(format('Failed to go from step %s to step %s (repeating counter = %d)', [s_selfsnr, s_tosnr, i_counter]), ML_ERROR);
      end
    end else begin
      b_reset := true; //ready for next run
      result := t_sctrlimpl.NextStep();
      if result then t_msgrimpl.AddMessage(format('Successful to go to next step automatically.', [i_counter]))
      else t_msgrimpl.AddMessage(format('Failed to go to next step (repeating counter = %d).', [i_counter]), ML_ERROR);
    end;
  end;
end;

constructor RepeatIfTrue.Create();
begin
  inherited Create();
  b_valid := true;
end;

initialization
  Classes.RegisterClass(JumpIfTrue);
  Classes.RegisterClass(JumpIfFalse);
  Classes.RegisterClass(RepeatIfTrue);
  Classes.RegisterClass(RepeatIfFalse);

finalization
  Classes.UnregisterClass(JumpIfTrue);
  Classes.UnregisterClass(JumpIfFalse);
  Classes.UnregisterClass(RepeatIfTrue);
  Classes.UnregisterClass(RepeatIfFalse);

end.
