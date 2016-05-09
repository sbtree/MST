unit TestRunner;

interface

uses  Classes, TypInfo, TestStep, StepDescriptor, TestSequence, StepResult, StepContainer, TextMessage,
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
    t_curstep:  TTestStep;
    t_fcaller:  TFunctionCaller;
    e_exemode:  EExecutionMode;
    t_container:TStepContainer;
    t_messenger:TTextMessenger;
  protected
    procedure SetExecutionMode(const em: EExecutionMode);
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);

    function  StepInit(const fieldval: string): boolean; virtual;
    {function  StepInputM(const par: string): boolean; virtual;
    function  StepFunc(const func, par: string): boolean; virtual;
    function  StepEval(const par: string): boolean; virtual;
    function  StepSave(const par: string): boolean; virtual;
    function  StepFinal(const par: string): boolean; virtual; }

    function RunStep(step: TTestStep): boolean;

  public
    constructor Create();
    destructor  Destroy(); override;

    property ExecutionMode: EExecutionMode read e_exemode write SetExecutionMode;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property StepContainer: TStepContainer read t_container write t_container;

    function RunStepByIndex(const stepidx: integer): boolean;
    function RunStepByNr(const stepnr: string): boolean;
    function RunCase(const casenr: integer): boolean;
    function RunSequence(const casefrom, caseto: integer): boolean;

  end;

implementation
uses SysUtils;

constructor TTestRunner.Create();
begin
  inherited Create();
  t_fcaller := TFunctionCaller.Create();
  t_fcaller.Messages := self.Messages;
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
  case e_exemode of
    EM_NORMAL, EM_SIMULATE: e_threshold := ML_ERROR;
    EM_DIAGNOSE: e_threshold := ML_INFO;
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
    //else t_messenger.AddMessage(format('[r:%d, c:%d, s:%s] %s', [i_rowindex, i_colindex, GetEnumName(p_ptinfo, integer(e_curstate)), text]), ClassName, level);
  end;
end;

function  TTestRunner.StepInit(const fieldval: string): boolean;
begin
  result := true;
  //todo:
end;

function TTestRunner.RunStep(step: TTestStep): boolean;
begin
  result := false;
  if assigned(step) then begin
    t_curstep := step;
    //todo: 1. initialize this step and execute statements of 'init' or 'r_on'
    //todo: 2. read information of 'm' and control the execution of this step
    //todo: 3. call script function to execute 'fkt' with 'par'
    //t_fcaller.CallFunction('','');
    //4. save result string of this step //todo: consider of result pattern here
    t_curstep.StepResult.ResultString := t_fcaller.ResultString;

    //todo: 5. evaluate the resualt of calling function with 'tol'
    //todo: 6. finalize this step 'final' or 'r_off'
    t_curstep.StepResult.Resulted := result;
  end;
end;

function TTestRunner.RunStepByIndex(const stepidx: integer): boolean;
begin
  result := false;
  if assigned(t_container) then  result := RunStep(t_container.GetStepByIndex(stepidx));
end;

function TTestRunner.RunStepByNr(const stepnr: string): boolean;
begin
  result := false;
  if assigned(t_container) then  result := RunStep(t_container.GetStepByNr(stepnr));
end;

function TTestRunner.RunCase(const casenr: integer): boolean;
begin
  result := false;
  //todo: 1. call RunStep in a loop till the last step in this case
  //todo: 2. decide to break or not if an error exists
end;

function TTestRunner.RunSequence(var troutine: TTestSequence): boolean;
begin
  result := false;
  //todo: 1. call RunCase in a loop till the last case in this routine
  //todo: 2. decide to break or not if an error exists
end;

end.
