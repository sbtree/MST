unit ScriptRunner;

interface

uses  Classes, ScriptReader, ScriptTerm, StepResult, StepContainer, TextMessage,
      FunctionCaller, GenType;

type
// =============================================================================
//    Description  : class of script runner, executes a script
//                   NOTE: Thread-Safety should be considered of this class if 
//                   an instance of this class runs in a work thread
//    First author : 2015-12-17 /bsu/
//    History      :
// =============================================================================
  TScriptRunner = class(TTextMessager)
  protected
    t_curstep: TTestStep;
    t_fcaller: TFunctionCaller;
    e_exemode: EExecutionMode;
    t_allsteps:TStepContainer;
  protected
    procedure SetExecutionMode(const em: EExecutionMode);
    {function  StepInit(const par: string): boolean; virtual;
    function  StepInputM(const par: string): boolean; virtual;
    function  StepFunc(const func, par: string): boolean; virtual;
    function  StepEval(const par: string): boolean; virtual;
    function  StepSave(const par: string): boolean; virtual;
    function  StepFinal(const par: string): boolean; virtual; }

  public
    constructor Create();
    destructor  Destroy(); override;

    property  ExecutionMode: EExecutionMode read e_exemode write SetExecutionMode;

    function RunStep(var tstep: TTestStep): boolean;
    function RunCase(var tcase: TTestCase): boolean;
    function RunSequence(var troutine: TTestSequence): boolean;

  end;

implementation
uses SysUtils;

constructor TScriptRunner.Create();
begin
  inherited Create();
  t_fcaller := TFunctionCaller.Create();
  t_fcaller.Messages := self.Messages;
  ExecutionMode := EM_NORMAL;
end;

destructor  TScriptRunner.Destroy();
begin
  FreeAndNil(t_fcaller);
  inherited Destroy();
end;

procedure TScriptRunner.SetExecutionMode(const em: EExecutionMode);
begin
  e_exemode := em;
  case e_exemode of
    EM_NORMAL, EM_SIMULATE: e_threshold := ML_ERROR;
    EM_DIAGNOSE: e_threshold := ML_INFO;
  end;
  t_fcaller.ExecutionMode := e_exemode;
end;

function TScriptRunner.RunStep(var tstep: TTestStep): boolean;
begin
  result := false;
  t_curstep := tstep;
  //todo: 1. initialize this step and execute statements of 'init' or 'r_on'
  //todo: 2. read information of 'm' and control the execution of this step
  //todo: 3. call script function to execute 'fkt' with 'par'
  //t_fcaller.CallFunction('','');
  //4. save result string of this step //todo: consider of result pattern here
  tstep.StepResult.ResultString := t_fcaller.ResultString;

  //todo: 5. evaluate the resualt of calling function with 'tol'
  //todo: 6. finalize this step 'final' or 'r_off'
  tstep.StepResult.Resulted := result;
end;

function TScriptRunner.RunCase(var tcase: TTestCase): boolean;
begin
  result := false;
  //todo: 1. call RunStep in a loop till the last step in this case
  //todo: 2. decide to break or not if an error exists
end;

function TScriptRunner.RunSequence(var troutine: TTestSequence): boolean;
begin
  result := false;
  //todo: 1. call RunCase in a loop till the last case in this routine
  //todo: 2. decide to break or not if an error exists
end;

end.
