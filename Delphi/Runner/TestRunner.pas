unit TestRunner;

interface

uses  Classes, TypInfo, StepData, StepGroup, TextMessage,
      FuncCaller, GenType;

type
// =============================================================================
//    Description  : class of script runner, executes a script
//                   NOTE: Thread-Safety should be considered of this class if 
//                   an instance of this class runs in a work thread
//    First author : 2015-12-17 /bsu/
//    History      :
// =============================================================================
  TTestRunner = class(TInterfacedObject, ITextMessengerImpl)
  private
     p_ptinfo:   PTypeInfo;    //a pointer to type info of EParseState

  protected
    t_fcaller:  TFunctionCaller;
    e_exemode:  EExecMode;
    t_curseq:   TTestSequence;
    t_container:TStepContainer;
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface TTextMessengerImpl
    b_jumpmstep:boolean; //indicate, whether to run the steps, which have minus step number, e.g. '-11.02'
  protected
    procedure SetExecutionMode(const em: EExecMode);
    procedure SetStepContainer(const tcon: TStepContainer);

    function  StepInit(const val: string): boolean; virtual;
    function  StepInputM(const val: string): boolean; virtual;
    function  StepFunc(const func, par: string): boolean; virtual;
    function  StepEval(const val: string): boolean; virtual;
    function  StepSave(const val: string): boolean; virtual;
    function  StepFinal(const val: string): boolean; virtual;

  public
    constructor Create();
    destructor  Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    
    property ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property StepContainer: TStepContainer read t_container write SetStepContainer;
    property CurrentSequence: TTestSequence read t_curseq;
    property JumpMinusStep: boolean read b_jumpmstep write b_jumpmstep;

    function RunStep(tstep: TTestStep): boolean; overload;
    function RunGroup(tgroup: TStepGroup): boolean;
    
    function RunStep(const stepnr: string): boolean; overload;
    function RunStep(const stepidx: integer): boolean; overload;
    function RunCase(const casenr: string): boolean; overload;
    function RunCase(const caseidx: integer): boolean; overload;
    function RunSequence(): boolean;
    function SetSequence(const csincl, csexcl: string): boolean;
    function RepeatStep(): boolean;
    function RepeatCase(): boolean;
  end;

implementation
uses SysUtils;

constructor TTestRunner.Create();
begin
  inherited Create();
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
  //t_curseq := TTestSequence.Create();
  t_fcaller := TFunctionCaller.Create();
  ITextMessengerImpl(t_fcaller).Messenger := t_msgrimpl.Messenger;
  b_jumpmstep := true;
  ExecutionMode := EM_NORMAL;
end;

destructor  TTestRunner.Destroy();
begin
  t_msgrimpl.Free();
  FreeAndNil(t_fcaller);
  //FreeAndNil(t_curseq);
  inherited Destroy();
end;

procedure TTestRunner.SetExecutionMode(const em: EExecMode);
begin
  e_exemode := em;
  if assigned(t_msgrimpl.Messenger) then begin
    case e_exemode of
      EM_NORMAL, EM_SIMULATE: t_msgrimpl.Messenger.MessageThreshold := ML_ERROR;
      EM_DIAGNOSE: t_msgrimpl.Messenger.MessageThreshold := ML_INFO;
    end;
  end;
  t_fcaller.ExecutionMode := e_exemode;
end;

procedure TTestRunner.SetStepContainer(const tcon: TStepContainer);
begin
  t_container := tcon;
  if assigned(t_container) then t_curseq := t_container;
end;

function  TTestRunner.StepInit(const val: string): boolean;
begin
  result := true;
  //todo:
  //1. turn on relay
  //2. check relays
  //3. call other procedures if needed
  //AddMessage(format('The value (%s) of field (r_on) is accepted.', [val]));
end;

function  TTestRunner.StepInputM(const val: string): boolean;
begin
  result := true;
  //todo:
  //1. repeat mode, timeout or count, minus
  //2. do it with minus number
  //3. unit to display for result
  //AddMessage(format('The value (%s) of field (M) is accepted.', [val]));
end;

function  TTestRunner.StepFunc(const func, par: string): boolean;
begin
  result := true; //todo: t_fcaller.CallFunction(func, par);
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

function TTestRunner.RunStep(tstep: TTestStep): boolean;
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
      t_msgrimpl.AddMessage(format('The step (%s) is done successfully.', [tstep.GetFieldValue(SF_NR)]));
    end else begin
      result := true;
      t_msgrimpl.AddMessage(format('The minus step (%s) is jumped.', [tstep.GetFieldValue(SF_NR)]));
    end;
  end;
end;

function TTestRunner.RunGroup(tgroup: TStepGroup): boolean;
var i: integer;
begin
  result := false;
  if assigned(tgroup) then begin
    for i := 0 to tgroup.StepCount - 1 do begin
      result := RunStep(tgroup.StepByIndex(i));
      if (not result) then break;
    end;

    //update index of current step in current sequence if a case is done
    if (tgroup <> t_curseq) then begin
      if (i >= tgroup.StepCount) then Dec(i);
      t_curseq.GotoStepNr(tgroup.StepNrOf(i));
    end;
  end;
end;

function TTestRunner.RunStep(const stepnr: string): boolean;
begin
  result := RunStep(t_curseq.StepByNr(stepnr));
end;

function TTestRunner.RunStep(const stepidx: integer): boolean;
begin
  result := RunStep(t_curseq.StepByIndex(stepidx));
end;

function TTestRunner.RunCase(const casenr: string): boolean;
begin
  result := RunGroup(t_curseq.CaseGroup.CaseByNr(casenr));
end;

function TTestRunner.RunCase(const caseidx: integer): boolean;
begin
  result := RunGroup(t_curseq.CaseGroup.CaseByIndex(caseidx));
end;

function TTestRunner.RunSequence(): boolean;
begin
  result := false;
  if (t_curseq.StepCount > 0) then RunGroup(t_curseq)
  else t_msgrimpl.AddMessage('No test step is found in current test sequence.', ML_WARNING);
end;

function TTestRunner.SetSequence(const csincl, csexcl: string): boolean;
begin
  result := false;
  if assigned(t_container) then begin
    t_curseq := t_container.TestSequence(csincl, csexcl);
    result := assigned(t_curseq);
  end;
end;

function TTestRunner.RepeatStep(): boolean;
begin
  result := RunStep(t_curseq.CurrentStep);
end;

function TTestRunner.RepeatCase(): boolean;
begin
  result := RunGroup(t_curseq.CaseGroup.CurrentCase);
end;

end.
