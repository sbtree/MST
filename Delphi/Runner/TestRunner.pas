unit TestRunner;

interface

uses  Classes, TypInfo, StepDescriptor, StepResult, StepContainer, TextMessage,
      FunctionCaller, GenType;

type
// =============================================================================
//    Description  : class of script runner, executes a script
//                   NOTE: Thread-Safety should be considered of this class if 
//                   an instance of this class runs in a work thread
//    First author : 2015-12-17 /bsu/
//    History      :
// =============================================================================
  TTestRunner = class
  private
     p_ptinfo:   PTypeInfo;    //a pointer to type info of EParseState

  protected
    t_fcaller:  TFunctionCaller;
    e_exemode:  EExecMode;
    t_container:TStepContainer;
    t_messenger:TTextMessenger;
    b_jumpmstep:boolean; //indicate, whether to run the steps, which have minus step number, e.g. '-11.02'
  protected
    procedure SetExecutionMode(const em: EExecMode);
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);

    function  StepInit(const val: string): boolean; virtual;
    function  StepInputM(const val: string): boolean; virtual;
    function  StepFunc(const func, par: string): boolean; virtual;
    function  StepEval(const val: string): boolean; virtual;
    function  StepSave(const val: string): boolean; virtual;
    function  StepFinal(const val: string): boolean; virtual;

    function RunStepT(tstep: TTestStep): boolean;
    function RunCaseT(tcase: TStepGroup): boolean;
    function RunSequenceT(tsequence: TTestSequence): boolean;

  public
    constructor Create();
    destructor  Destroy(); override;

    property ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property StepContainer: TStepContainer read t_container write t_container;
    property JumpMinusStep: boolean read b_jumpmstep write b_jumpmstep;

    function RunStep(const stepnr: string): boolean; overload;
    function RunStep(const stepidx: integer): boolean; overload;
    function RunCase(const casenr: string): boolean; overload;
    function RunCase(const caseidx: integer): boolean; overload;
    function RunSequence(const csincl, csexcl: string): boolean;
    function RepeatStep(): boolean;
    function RepeatCase(): boolean;
    function RepeatSequence(): boolean;
  end;

implementation
uses SysUtils;

constructor TTestRunner.Create();
begin
  inherited Create();
  t_fcaller := TFunctionCaller.Create();
  t_fcaller.Messenger := self.Messenger;
  b_jumpmstep := true;
  ExecutionMode := EM_NORMAL;
end;

destructor  TTestRunner.Destroy();
begin
  FreeAndNil(t_fcaller);
  inherited Destroy();
end;

procedure TTestRunner.SetExecutionMode(const em: EExecMode);
begin
  e_exemode := em;
  if assigned(t_messenger) then begin
    case e_exemode of
      EM_NORMAL, EM_SIMULATE: t_messenger.MessageThreshold := ML_ERROR;
      EM_DIAGNOSE: t_messenger.MessageThreshold := ML_INFO;
    end;
  end;
  t_fcaller.ExecutionMode := e_exemode;
end;

// =============================================================================
//    Description  : add a message into t_messenger if it exists
//    Parameter    : text, text of the message
//                   level, level of the message (see EMessageLevel in TextMessage)
//    Return       : --
//    First author : 2016-05-09 /bsu/
//    History      :
// =============================================================================
procedure TTestRunner.AddMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    //if (level in [ML_INFO, ML_WARNING]) then t_messenger.AddMessage(text, ClassName, level)
    t_messenger.AddMessage(format('%s', [text]), 'TTestRunner', level);
  end;
end;

function  TTestRunner.StepInit(const val: string): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) of field (r_on) is accepted.', [val]));
end;

function  TTestRunner.StepInputM(const val: string): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) of field (M) is accepted.', [val]));
end;

function  TTestRunner.StepFunc(const func, par: string): boolean; 
begin
  result := true;
  //todo:
  //AddMessage(format('The function (%s) with parameter (%s) is executed.', [func, par]));
end;

function  TTestRunner.StepEval(const val: string): boolean; 
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) is evaluated.', [val]));
end;

function  TTestRunner.StepSave(const val: string): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The resault (%s)  is saved.', [val]));
end;

function  TTestRunner.StepFinal(const val: string): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) of field (r_off) is accepted.', [val]));
end;


function TTestRunner.RunStepT(tstep: TTestStep): boolean;
var b_runstep: boolean;
begin
  result := false;
  if assigned(tstep) then begin
    if tstep.IsMinusStep then b_runstep := (not b_jumpmstep)
    else b_runstep := true;

    if b_runstep then begin
      //todo: 1. initialize this step and execute statements of 'init' or 'r_on'
      result := StepInit(tstep.GetFieldValue(SF_INIT));
      //todo: 2. read information of 'm' and control the execution of this step
      if result then result := StepInputM(tstep.GetFieldValue(SF_M));
      //todo: 3. call script function to execute 'fct' with 'par'
      if result then result := StepFunc(tstep.GetFieldValue(SF_FCT), tstep.GetFieldValue(SF_PAR));
      //t_fcaller.CallFunction('','');
      //4. save result string of this step //todo: consider of result pattern here
      //t_curstep.StepResult.ResultString := t_fcaller.ResultString;

      //todo: 5. evaluate the resualt of calling function with 'tol'
      if result then result := StepEval(tstep.GetFieldValue(SF_TOL_A));
      //todo: 6. finalize this step 'final' or 'r_off'
      if result then result := StepFinal(tstep.GetFieldValue(SF_FINAL));
      //t_curstep.StepResult.Resulted := result;
      AddMessage(format('The step (%s) is done successfully.', [tstep.GetFieldValue(SF_NR)]));
    end else begin
      result := true;
      AddMessage(format('The minus step (%s) is jumped.', [tstep.GetFieldValue(SF_NR)]));
    end;
  end;
end;

function TTestRunner.RunCaseT(tcase: TStepGroup): boolean;
var i: integer;
begin
  result := false;
  if (assigned(tcase) and assigned(t_container)) then begin
    b_jumpmstep := false;
    for i := tcase.IndexFrom to tcase.IndexTo do begin
      result := RunStepT(t_container.StepByIndex(i));
      if (not result) then break;
    end;
  end;
end;

function TTestRunner.RunSequenceT(tsequence: TTestSequence): boolean;
var i_stepidx: integer;
begin
  result := false;
  if assigned(t_container) then begin
    b_jumpmstep := true;
    i_stepidx := tsequence.FirstStepIndex;
    while (i_stepidx >= 0) do begin
      result := RunStepT(t_container.StepByIndex(i_stepidx));
      if (not result) then break;
      i_stepidx := tsequence.NextStepIndex;
    end;
  end;
end;

function TTestRunner.RunStep(const stepnr: string): boolean;
var t_step: TTestStep;
begin
  result := false;
  if assigned(t_container) then  begin
    b_jumpmstep := false;
    t_step := t_container.StepByNr(stepnr);
    result := RunStepT(t_step);
  end;
end;

function TTestRunner.RunStep(const stepidx: integer): boolean;
var t_step: TTestStep;
begin
  result := false;
  if assigned(t_container) then begin
    b_jumpmstep := false;
    t_step := t_container.StepByIndex(stepidx);
    result := RunStepT(t_step);
  end;
end;

function TTestRunner.RunCase(const casenr: string): boolean;
var t_case: TStepGroup;
begin
  result := false;
  if assigned(t_container) then begin
    t_case := t_container.CaseByNr(casenr);
    result := RunCaseT(t_case);
  end;
end;

function TTestRunner.RunCase(const caseidx: integer): boolean;
var t_case: TStepGroup;
begin
  result := false;
  if assigned(t_container) then begin
    t_case := t_container.CaseByIndex(caseidx);
    result := RunCaseT(t_case);
  end;
end;

function TTestRunner.RunSequence(const csincl, csexcl: string): boolean;
begin
  result := false;
  if assigned(t_container) then begin
    t_container.UpdateSequence(csincl, csexcl);
    result := RunSequenceT(t_container.TestSequence);
  end;
end;

function TTestRunner.RepeatStep(): boolean;
begin
  result := false;
  if assigned(t_container) then
    result := RunStepT(t_container.CurrentStep);
end;

function TTestRunner.RepeatCase(): boolean;
begin
  result := false;
  if assigned(t_container) then
    result := RunCaseT(t_container.CurrentCase);
end;

function TTestRunner.RepeatSequence(): boolean;
begin
  result := false;
  if assigned(t_container) then
    result := RunSequenceT(t_container.TestSequence);
end;

end.
