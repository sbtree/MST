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

    t_curcase:  TStepGroup;
    t_curstep:  TTestStep;
    t_fcaller:  TFunctionCaller;
    e_exemode:  EExecutionMode;
    t_container:TStepContainer;
    t_messenger:TTextMessenger;
  protected
    procedure SetExecutionMode(const em: EExecutionMode);
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);

    function  StepInit(const val: string): boolean; virtual;
    function  StepInputM(const val: string): boolean; virtual;
    function  StepFunc(const func, par: string): boolean; virtual;
    function  StepEval(const val: string): boolean; virtual;
    function  StepSave(const val: string): boolean; virtual;
    function  StepFinal(const val: string): boolean; virtual;

    function RunStepT(tstep: TTestStep): boolean;
    function RunCaseT(tcase: TStepGroup): boolean;

  public
    constructor Create();
    destructor  Destroy(); override;

    property ExecutionMode: EExecutionMode read e_exemode write SetExecutionMode;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property StepContainer: TStepContainer read t_container write t_container;

    function RunStep(const stepnr: string): boolean; overload;
    function RunStep(const stepidx: integer): boolean; overload;
    function RunCase(const casenr: string): boolean; overload;
    function RunCase(const caseidx: integer): boolean; overload;
    function RunSequence(const csincl, csexcl: string): boolean;
  end;

implementation
uses SysUtils;

constructor TTestRunner.Create();
begin
  inherited Create();
  t_fcaller := TFunctionCaller.Create();
  t_fcaller.Messenger := self.Messenger;
  ExecutionMode := EM_NORMAL;
end;

destructor  TTestRunner.Destroy();
begin
  FreeAndNil(t_fcaller);
  inherited Destroy();
end;

procedure TTestRunner.SetExecutionMode(const em: EExecutionMode);
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
begin
  result := false;
  if assigned(tstep) then begin
    t_curstep := tstep;
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
  end;
end;

function TTestRunner.RunCaseT(tcase: TStepGroup): boolean;
var i: integer;
begin
  result := false;
  if (assigned(tcase) and assigned(t_container)) then begin
    for i := tcase.IndexFrom to tcase.IndexTo do begin
      result := RunStepT(t_container.StepByIndex(i));
      if (not result) then break;
    end;
  end;
end;

function TTestRunner.RunStep(const stepnr: string): boolean;
begin
  result := false;
  if assigned(t_container) then  result := RunStepT(t_container.StepByNr(stepnr));
end;

function TTestRunner.RunStep(const stepidx: integer): boolean;
begin
  result := false;
  if assigned(t_container) then  result := RunStepT(t_container.StepByIndex(stepidx));
end;

function TTestRunner.RunCase(const casenr: string): boolean;
begin
  result := false;
  if assigned(t_container) then
    result := RunCaseT(t_container.CaseByNr(casenr));
end;

function TTestRunner.RunCase(const caseidx: integer): boolean;
begin
  result := false;
  if assigned(t_container) then
    result := RunCaseT(t_container.CaseByIndex(caseidx));
end;

function TTestRunner.RunSequence(const csincl, csexcl: string): boolean;
var i_stepidx: integer;
begin
  result := false;
  if assigned(t_container) then begin
    t_container.UpdateSequence(csincl, csexcl);
    i_stepidx := t_container.TestSequence.FirstStepIndex;
    while (i_stepidx >= 0) do begin
      result := RunStepT(t_container.StepByIndex(i_stepidx));
      if (not result) then break;
      i_stepidx := t_container.TestSequence.NextStepIndex;
    end;
  end;
end;

end.
