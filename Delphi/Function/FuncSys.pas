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
uses Classes, FuncBase, TextMessage, StepGroup;

type
  IStepControlImpl = interface
    function GotoStep(const stepnr: string): boolean;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TStepControlHelper = class(TInterfacedObject, IStepControlImpl)
  protected
    t_group: TStepGroup;
  public
    constructor Create();
    destructor Destroy; override;

    function GotoStep(const stepnr: string): boolean;
    function CurStepNr(): string;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TConditionControl = class(TFunctionBase, IStepControlImpl)
  protected
    t_schelper: TStepControlHelper;
    s_expr:     string; //conditional expression
    s_selfsnr:  string; //current step number
    s_tosnr:    string; //step number of destination
    b_valid:    boolean;  //indicates expected value of the condition
  protected
    function ValidStepNrs(const bforward: boolean = true): boolean; virtual;
    
  public
    constructor Create(); override;
    destructor Destroy; override;

    function DoTask(): boolean; override;
    function BooleanCondition(const expr: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; override;
    property StepControl: TStepControlHelper read t_schelper implements IStepControlImpl;
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
uses Windows, SysUtils, StrUtils, Contnrs;

constructor TStepControlHelper.Create();
begin
  inherited Create();
  t_group := nil;
end;
destructor TStepControlHelper.Destroy;
begin
  inherited Destroy();
end;

function TStepControlHelper.GotoStep(const stepnr: string): boolean;
begin
  result := false;
  if assigned(t_group) then result := t_group.GotoStepNr(stepnr);
end;

function TStepControlHelper.CurStepNr(): string;
begin
  result := '';
  if assigned(t_group) then
    result := t_group.CurStepNr();
end;

function TStepControlHelper.GetStepGroup(): TStepGroup;
begin
  result := t_group;
end;

procedure TStepControlHelper.SetStepGroup(const group: TStepGroup);
begin
  t_group := group;
end;

function TConditionControl.ValidStepNrs(const bforward: boolean): boolean;
var f_ssnr, f_tsnr: single; s_ssnr, s_tsnr: string;
begin
  //Jump is allowed only to forward
  s_ssnr := ReplaceStr(s_selfsnr, '.', DecimalSeparator);
  s_tsnr := ReplaceStr(s_tosnr, '.', DecimalSeparator);
  result := (TryStrToFloat(s_ssnr, f_ssnr) and TryStrToFloat(s_tsnr, f_tsnr));
  if result then begin
    f_ssnr := abs(f_ssnr);
    f_tsnr := abs(f_tsnr);
    if bforward then result := (f_tsnr >= f_ssnr)
    else result := (f_tsnr <= f_ssnr);
    if (not result) then t_msgrimpl.AddMessage('The step number of destination is allowed in this function.', ML_ERROR);
  end else t_msgrimpl.AddMessage('The step number of destination is invalid', ML_ERROR);
end;

constructor TConditionControl.Create();
begin
  inherited Create();
  t_schelper := TStepControlHelper.Create();
  b_valid := false;
end;

destructor TConditionControl.Destroy;
begin
  t_schelper.Free();
  inherited Destroy();
end;

function TConditionControl.BooleanCondition(const expr: string): boolean;
begin
  //todo
  result := false;
end;

function TConditionControl.DoTask(): boolean;
begin
  s_selfsnr := t_schelper.CurStepNr();
  result := (s_selfsnr <> '');
end;

function TConditionControl.LoadParameters(const pars: TStrings): boolean;
var s_par: string;
begin
  result := false;
  if (pars.Count >= 2) then begin
    s_expr := pars[0];
    //todo: replace #Pseudo
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
  if result then begin
    if (BooleanCondition(s_expr) = b_valid) then result := t_schelper.GotoStep(s_tosnr)
    else result := false;
  end;
  if (not result) then t_msgrimpl.AddMessage(format('Failed to go to step "%s"', [s_tosnr]), ML_ERROR);
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
  c_elapse := 10000;
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
          if (i_val >= 0) then c_elapse := i_val
          else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
        end else t_msgrimpl.AddMessage(format(CSTR_INVALID_PARA, [s_par]), ML_WARNING);
      end;
    end;
  end;
end;

function TRepeatControl.DoTask(): boolean;
var b_dorepeat: boolean;
begin
  result := inherited DoTask();
  if b_reset then begin
    c_tend := GetTickCount() + c_elapse;
    i_counter := 0;
    b_reset := false;
  end;
  if result then begin
    if (i_ninit >= 0) then b_dorepeat := (i_counter <= i_ninit)
    else b_dorepeat := (GetTickCount() <= c_tend);
    b_dorepeat := (b_dorepeat and (BooleanCondition(s_expr) = b_valid));

    if b_dorepeat then begin
      result := t_schelper.GotoStep(s_tosnr);
      if (not result) then b_reset := true; //ready for next run
    end else b_reset := true; //ready for next run
  end;
  Inc(i_counter);
  if (not result) then t_msgrimpl.AddMessage(format('Failed to go to step "%s"', [s_tosnr]), ML_ERROR);
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
