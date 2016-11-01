//==============================================================================
// Module name  : $RCSfile: TestRunner.pas,v $
// Description  : This unit defines a class to run a test sequence
// Copyright    : (c) Metronix 2015
// Reversion    : $Revision 1.0$
// Compiler     : Delphi 2007
// Author       : 2015-06-10 /bsu/
// History      :
//==============================================================================
unit TestRunner;

interface

uses  Classes, Contnrs, TypInfo, StepData, StepGroup, TextMessage,
      FuncCaller, GenType, FuncBase;
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
    t_funcobjs: TObjectList;
    e_exemode:  EExecMode;
    t_curstep:  TTestStep;          //temporary variable to save the reference of test step, which is being done.
    t_curseq:   TTestSequence;
    t_container:TStepContainer;
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface TTextMessengerImpl
    b_jumpmstep:boolean; //indicate, whether to run the steps, which have minus step number, e.g. '-11.02'
  protected
    procedure UpdateMessageThreshold();
    procedure SetMessengerReim(tmessenger: TTextMessenger);
    procedure SetExecutionMode(const em: EExecMode);
    procedure SetStepContainer(const tcon: TStepContainer);
    procedure UpdateFuncObjs();
    function GetFunctionObj(const tstep: TTestStep): TFunctionBase;

    function RunStep(): boolean; overload;
    function StepInit(): boolean; virtual;
    function StepInputM(): boolean; virtual;
    function StepFunc(): boolean; virtual;
    function StepEval(): boolean; virtual;
    function StepSave(): boolean; virtual;
    function StepFinal(): boolean; virtual;
  public
    constructor Create();
    destructor  Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    procedure ITextMessengerImpl.SetMessenger = SetMessengerReim;

    property ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property StepContainer: TStepContainer read t_container write SetStepContainer;
    property TestSequence: TTestSequence read t_curseq;
    property JumpMinusStep: boolean read b_jumpmstep write b_jumpmstep;
    property FunctionCaller: TFunctionCaller read t_fcaller;

    function RunStep(tstep: TTestStep): boolean; overload;
    function RunGroup(tgroup: TStepGroup): boolean;

    function RunStep(const stepnr: string): boolean; overload;
    function RunStep(const stepidx: integer): boolean; overload;
    function RunCase(const casenr: string): boolean; overload;
    function RunCase(const caseidx: integer): boolean; overload;
    function RunSequence(): boolean;
    function UpdateSequence(const csincl, csexcl: string): boolean;
    function RepeatStep(): boolean;
    function RepeatCase(): boolean;
  end;

implementation
uses SysUtils;

constructor TTestRunner.Create();
begin
  inherited Create();
  b_jumpmstep := true;
  t_funcobjs := TObjectList.Create();
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());

  t_curseq := TTestSequence.Create();
  t_fcaller := TFunctionCaller.Create();
  IFunctionActors(t_fcaller).SetActorObject(FA_TSEQ, t_curseq);

  ExecutionMode := EM_NORMAL;
end;

destructor  TTestRunner.Destroy();
begin
  t_curseq.Free();
  t_msgrimpl.Free();
  t_fcaller.Free();
  t_funcobjs.Free();
  inherited Destroy();
end;

procedure TTestRunner.UpdateMessageThreshold();
begin
  case e_exemode of
    EM_NORMAL, EM_SIMULATE: t_msgrimpl.SetMessageThreshold(ML_ERROR);
    EM_DIAGNOSE: t_msgrimpl.SetMessageThreshold(ML_INFO);
  end;
end;

procedure TTestRunner.SetMessengerReim(tmessenger: TTextMessenger);
begin
  t_msgrimpl.Messenger := tmessenger;
  ITextMessengerImpl(t_curseq).Messenger := t_msgrimpl.Messenger;
  ITextMessengerImpl(t_fcaller).Messenger := t_msgrimpl.Messenger;
  UpdateMessageThreshold();
end;

procedure TTestRunner.SetExecutionMode(const em: EExecMode);
begin
  e_exemode := em;
  UpdateMessageThreshold();
end;

procedure TTestRunner.SetStepContainer(const tcon: TStepContainer);
begin
  t_container := tcon;
  if assigned(t_container) then begin
    t_container.UpdateSequence(t_curseq);
    UpdateFuncObjs();
  end;
end;

procedure TTestRunner.UpdateFuncObjs();
var i: integer; t_step: TTestStep; s_fname: string;
begin
  t_funcobjs.Clear();
  for i := 0 to t_curseq.StepCount - 1 do begin
    t_step := t_curseq.StepByIndex(i);
    if assigned(t_step) then begin
      s_fname := t_step.GetFieldValue(SF_FCT);
      t_funcobjs.Add(t_fcaller.CreateFunction(s_fname));
    end else t_funcobjs.Add(nil);
  end;
end;

function TTestRunner.GetFunctionObj(const tstep: TTestStep): TFunctionBase;
var i_idx: integer; s_snr: string;
begin
  result := nil;
  if assigned(tstep) then begin
    s_snr := tstep.GetFieldValue(SF_NR);
    i_idx := t_curseq.IndexOfStep(s_snr);
    if ((i_idx >= 0) and (i_idx < t_funcobjs.Count)) then
      result := TFunctionBase(t_funcobjs.Items[i_idx]);
  end;
end;

function TTestRunner.RunStep(): boolean;
var s_snr: string;
begin
  result := false;
  if assigned(t_curstep) then begin
    s_snr := t_curstep.GetFieldValue(SF_NR);
    t_msgrimpl.AddEmptyLine();
    t_msgrimpl.AddMessage(format('Step (%s) is starting ...', [s_snr]), ML_EVER);
    t_curseq.GotoStepIndex(t_curseq.IndexOfStep(s_snr));
    if ((not t_curstep.IsMinusStep) or (not b_jumpmstep)) then begin
      //todo: 1. initialize this step and execute statements of 'init' or 'r_on'
      result := StepInit();
      //todo: 2. read information of 'm' and control the execution of this step
      if result then
        result := StepInputM();
      //todo: 3. call script function to execute 'fct' with 'par'
      if result then
        result := StepFunc();
      //t_fcaller.CallFunction('','');
      //4. save result string of this step //todo: consider of result pattern here
      //t_curstep.StepResult.ResultString := t_fcaller.ResultString;

      //todo: 5. evaluate the resualt of calling function with 'tol'
      if result then
        result := StepEval();
      //todo: 6. finalize this step 'final' or 'r_off'
      if result then
        result := StepFinal();
      //t_curstep.StepResult.Resulted := result;
      if result then t_msgrimpl.AddMessage(format('The step (%s) is done successfully.', [t_curstep.GetFieldValue(SF_NR)]), ML_EVER)
      else t_msgrimpl.AddMessage(format('The step (%s) is not done exactly.', [t_curstep.GetFieldValue(SF_NR)]), ML_ERROR);
    end else begin
      result := true;
      t_msgrimpl.AddMessage(format('The minus step (%s) is jumped.', [t_curstep.GetFieldValue(SF_NR)]));
    end;
  end;
end;

function TTestRunner.StepInit(): boolean;
begin
  result := true;
  //todo:
  //1. turn on relay
  //2. check relays
  //3. call other procedures if needed
  //AddMessage(format('The value (%s) of field (r_on) is accepted.', [val]));
end;

function TTestRunner.StepInputM(): boolean;
begin
  result := true;
  //todo:
  //1. repeat mode, timeout or count, minus
  //2. do it with minus number
  //3. unit to display for result
  //AddMessage(format('The value (%s) of field (M) is accepted.', [val]));
end;

function TTestRunner.StepFunc(): boolean;
var t_func: TFunctionBase; s_fname, s_fpar: string;
begin
  s_fname := t_curstep.GetFieldValue(SF_FCT);
  if (SameText(s_fname, 'nil') or (s_fname = '')) then result := true
  else begin
    t_func := GetFunctionObj(t_curstep);
    s_fpar := t_curstep.GetFieldValue(SF_PAR);
    t_fcaller.CurStepResult := t_curstep.StepResult;
    result := t_fcaller.RunFunction(t_func, s_fpar);
  end;
end;

function TTestRunner.StepEval(): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) is evaluated.', [val]));
end;

function TTestRunner.StepSave(): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The resault (%s)  is saved.', [val]));
end;

function TTestRunner.StepFinal(): boolean;
begin
  result := true;
  //todo:
  //AddMessage(format('The value (%s) of field (r_off) is accepted.', [val]));
end;

function TTestRunner.RunStep(tstep: TTestStep): boolean;
begin
  t_curstep := tstep;
  result := RunStep();
end;

function TTestRunner.RunGroup(tgroup: TStepGroup): boolean;
var s_snr: string; i_cnr: integer;
begin
  result := false;
  if assigned(tgroup) then begin
    result := true;
    t_curstep := tgroup.FirstStep;
    while (result and (tgroup.CurStepIndex < tgroup.StepCount)) do begin
      result := RunStep(t_curstep);
      s_snr := t_curstep.GetFieldValue(SF_NR);
      if TStepGroup.CalcMainNr(s_snr, i_cnr) then
        t_curseq.CaseGroup.GotoCaseNr(IntToStr(i_cnr)); //update case index in the case group
      t_curstep := tgroup.NextStep;
    end;
  end;
end;

function TTestRunner.RunStep(const stepnr: string): boolean;
var s_snr: string; i_idx: integer;
begin
  s_snr := stepnr; i_idx := t_curseq.IndexOfStep(s_snr);
  result := RunStep(i_idx);
end;

function TTestRunner.RunStep(const stepidx: integer): boolean;
begin
  result := t_curseq.GotoStepIndex(stepidx);
  if result then begin
    t_curstep := t_curseq.StepByIndex(stepidx);
    result := RunStep(t_curstep);
  end;
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

function TTestRunner.UpdateSequence(const csincl, csexcl: string): boolean;
begin
  if assigned(t_container) then begin
    result := t_container.UpdateSequence(t_curseq, csincl, csexcl);
    UpdateFuncObjs();
  end else result := false;
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
