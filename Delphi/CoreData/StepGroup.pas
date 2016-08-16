// =============================================================================
// Module name  : $RCSfile: StepGroup.pas,v $
// Description  : This unit defines different groups of test steps, in order to
//                to manage all steps, which are read from a script.
//                script is composed of variable-value pairs and test steps.
// looks like:
//                |--test step--------------------------------------------------------------------------------------------------|
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                |[Field Name]:[Field Value]|-------------------------------------------------------------------------------------------|
//                (Nr:10.10; T:'  measurement of dc voltage'; R_on:'101'; Fkt:U_Mess_DC; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:1);),
//                ...
//                (Nr:10.99; T:'end of test case 1'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'101'; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:50.00; T:'beginning of test case n'; R_on:'202'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:50.99; T:'end of test case n'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'202'; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:990.00; T:'end of test script'; R_on:''; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);).
// Compiler     : Delphi 2007
// Author       : 2015-12-11 /bsu/
// History      :
//==============================================================================

unit StepGroup;

interface
uses Classes, Contnrs, StepData;

type
  TIndexSet = set of byte;

  IStepNavigator = interface
    function GetSteps(): integer;
    function GetFirstStep(): TTestStep;
    function GetPrevStep(): TTestStep;
    function GetCurStep(): TTestStep;
    function GetNextStep(): TTestStep;
    function GetLastStep(): TTestStep;
    function GetRelativeStep(const relidx: integer): TTestStep;

    property CurrentStep: TTestStep read GetCurStep;
    property PreviousStep: TTestStep read GetPrevStep;
    property NextStep: TTestStep read GetNextStep;
    property FirstStep: TTestStep read GetFirstStep;
    property LastStep: TTestStep read GetLastStep;
    property StepCount: integer read GetSteps;
  end;

  //a class representing a step group, which is composed of test steps in the container
  TStepGroup = class(TInterfacedObject, IStepNavigator)
  class function CalcCaseNr(const stepnr: string; var casenr: integer): boolean;
  protected
    t_steps:    TStrings;       //a list of test steps (step number and object pairs)
    i_curstep:  integer;        //indicate current index of t_steps

  protected
    function GetStepNumbers(): string;
    procedure RemoveStepByIndex(const idx: integer); virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    //basic methods of the interface
    function GetSteps(): integer;
    function GetFirstStep(): TTestStep;
    function GetPrevStep(): TTestStep;
    function GetCurStep(): TTestStep; virtual;
    function GetNextStep(): TTestStep;
    function GetLastStep(): TTestStep;
    function GetRelativeStep(const relidx: integer): TTestStep;

    //properties
    property CurrentStep: TTestStep read GetCurStep;
    property PreviousStep: TTestStep read GetPrevStep;
    property NextStep: TTestStep read GetNextStep;
    property FirstStep: TTestStep read GetFirstStep;
    property LastStep: TTestStep read GetLastStep;
    property StepCount: integer read GetSteps;
    property StepList: TStrings read t_steps;
    property StepNumbers: string read GetStepNumbers;

    //additional methods
    function AddStep(const step: TTestStep): boolean; virtual;
    function StepByIndex(const idx: integer): TTestStep;
    function StepByNr(const nr: string): TTestStep;
    function IndexOfStep(const stepnr: string): integer;
    function StepNrOf(const idx: integer): string;
    procedure RemoveStep(const stepnr: string);
    procedure Clear(); virtual;
  end;

  //a class representing a set of test steps in the container (continue from an index to another),
  //e.g. a test case, which have the same main number in field Nr (xxx of xxx.yyy)
  TTestCase = class(TStepGroup)
  protected
    i_casenr:     integer; //number of the group, e.g. test case number
    s_title:      string;  //title of this group

  public
    constructor Create();
    destructor Destroy(); override;

    //properties
    property CaseNr: integer read i_casenr write i_casenr;
    property CaseTitle: string read s_title write s_title;

    //reimplemented methods
    function AddStep(const step: TTestStep): boolean; override;
  end;

  //a class, represents a loop in a test case, which starts with script function
  //'LoopBegin' and ends with 'LoopEnd'
  //warning: step loops may be nested, but they are only allowed  in one test case.
  //         a loop is not allowed between two test cases, that means, the beginning
  //         and its corresponding end muss exist in one case.
  TStepLoop = class(TStepGroup)
  protected
    i_casenr: integer;  //indicates test case, in which the loop exists
    b_closed: boolean;  //indicates if the loop is closed

  public
    constructor Create();
    destructor Destroy(); override;

    //properties
    property LoopClosed : boolean read b_closed;

    //reimplemented methods
    function AddStep(const step: TTestStep): boolean; override;
  end;

  ICaseNavigator = interface
    function GetCases(): integer;
    function GetFirstCase(): TTestCase;
    function GetPrevCase(): TTestCase;
    function GetCurCase(): TTestCase;
    function GetNextCase(): TTestCase;
    function GetLastCase(): TTestCase;
    function GetRelativeCase(const relidx: integer): TTestCase;

    property CurrentCase: TTestCase read GetCurCase;
    property PreviousCase: TTestCase read GetPrevCase;
    property NextCase: TTestCase read GetNextCase;
    property FirstCase: TTestCase read GetFirstCase;
    property LastCase: TTestCase read GetLastCase;
    property CaseCount: integer read GetCases;
  end;

  TCaseGroup = class(TInterfacedObject, ICaseNavigator)
  protected
    t_cases:    TStrings; //a list of test cases (case number and object pairs)
    i_curcase:  integer;  //index, indicates current case in t_cases
    b_freecase: boolean;  //indicates if free object by removing

  protected
    function GetCaseNumbers(): string;
    procedure RemoveCaseByIndex(const idx: integer); virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    //basic methods of the interface
    function GetCases(): integer;
    function GetFirstCase(): TTestCase;
    function GetPrevCase(): TTestCase;
    function GetCurCase(): TTestCase;
    function GetNextCase(): TTestCase;
    function GetLastCase(): TTestCase;
    function GetRelativeCase(const relidx: integer): TTestCase;

    //properties
    property CurrentCase: TTestCase read GetCurCase;
    property PreviousCase: TTestCase read GetPrevCase;
    property NextCase: TTestCase read GetNextCase;
    property FirstCase: TTestCase read GetFirstCase;
    property LastCase: TTestCase read GetLastCase;
    property CaseCount: integer read GetCases;
    property CaseList: TStrings read t_cases;
    property CaseNumbers: string read GetCaseNumbers;
    property FreeByRemove: boolean read b_freecase write b_freecase;

    //additional methods
    function AddCase(const tcase: TTestCase): boolean;
    function CaseByIndex(const idx: integer): TTestCase;
    function CaseByNr(const casenr: string): TTestCase; overload;
    function CaseByNr(const nr: integer): TTestCase; overload;
    function IndexOfCase(const casenr: string): integer;
    function CaseNrOf(const idx: integer): string;
    procedure RemoveCase(const casenr: string);
    procedure Clear(); virtual;
  end;


  ILoopNavigator = interface
    procedure PushLoop(const tloop: TStepLoop);
    function PopLoop(): TStepLoop;
    function GetCurLoop(): TStepLoop;

    property CurrentLoop: TStepLoop read GetCurLoop;
  end;

  TLoopStack = class(TStepGroup, ILoopNavigator)
  protected
    t_loops:    TStrings;     //a list of TStepLoop (step number of loop beginning and loop object pairs)
    t_curloop:  TStepLoop;    //reference to current loop object
    t_stack:    TObjectStack; //
  public
    constructor Create();
    destructor Destroy(); override;

    procedure PushLoop(const tloop: TStepLoop);
    function PopLoop(): TStepLoop;
    function GetCurLoop(): TStepLoop;

    property CurrentLoop: TStepLoop read GetCurLoop;
  end;

  //a class representing a test sequence, which is composed of test groups in the container
  TTestSequence = class(TStepGroup)
  protected
    t_casegrp:  TCaseGroup; //an object of TCaseGroup, to save test cases
    t_loopstack:TLoopStack; //an object of TLoopStack, to order step loops
    i_lastcnr:  integer;    //to save last case number during reading test script

  protected
    procedure UpdateCaseByStep(const tstep: TTestStep);

  public
    constructor Create();
    destructor Destroy(); override;

    //properties
    property CurrentStep: TTestStep read GetCurStep;
    property PreviousStep: TTestStep read GetPrevStep;
    property NextStep: TTestStep read GetNextStep;
    property FirstStep: TTestStep read GetFirstStep;
    property LastStep: TTestStep read GetLastStep;
    property StepCount: integer read GetSteps;
    property CaseGroup: TCaseGroup read t_casegrp;

    //additional methods
    function AddStep(const tstep: TTestStep): boolean; override;
    function AddCase(const tcase: TTestCase): boolean;
    procedure Clear(); override;
  end;

  //a class composed of all test steps
  TStepContainer = class(TTestSequence)
  protected
//    t_cases:      TStrings;     //a list of test cases (case number and object pairs)
    t_sequence:   TTestSequence;//to save current test sequence, in which a case list and a step list are inclusive

    s_inclusive:  string;       //to save expression of inclusive cases, see function TestSequence
    s_exclusive:  string;       //to save expression of exclusive cases, see function TestSequence

  protected
    function CaseIndexSet(const casenrs: string): TIndexSet;
    procedure UpdateLoop(const stepnr, funcname: string);
    procedure RemoveStepByIndex(const idx: integer); override;
    procedure UpdateSequence(const incl: string; const excl: string = '');

  public
    constructor Create();
    destructor  Destroy(); override;

    function LoadStep(const fields: FieldStringArray): boolean;
    function GetTestSequence(const incl: string; const excl: string = ''): TTestSequence;

    procedure Clear(); override;
    procedure SaveFile(const sfile: string);
    procedure Assign(const source: TStepContainer);
  end;

const
  CINT_CASES_MAX : integer = 255;
  CSTR_LOOP_BEGIN:  string = 'LoopBegin';
  CSTR_LOOP_END:    string = 'LoopEnd';

implementation
uses SysUtils, StrUtils, Variants;

class function TStepGroup.CalcCaseNr(const stepnr: string; var casenr: integer): boolean;
var t_nrs: TStrings;
begin
  result := false;
  if (trim(stepnr) <> '') then begin
    t_nrs := TStringList.Create();
    if (ExtractStrings(['.'], [' '], PChar(stepnr), t_nrs) > 0) then begin
      result := TryStrToInt(t_nrs[0], casenr);
      if result then casenr := abs(casenr);
    end;
    FreeAndNil(t_nrs);
  end;
end;

function TStepGroup.GetStepNumbers(): string;
begin
  result := t_steps.DelimitedText;
end;

procedure TStepGroup.RemoveStepByIndex(const idx: integer);
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then t_steps.Delete(idx);
end;

constructor TStepGroup.Create();
begin
  inherited Create();
  i_curstep := -1;
  t_steps := TStringList.Create();
  t_steps.Delimiter := Char(';');
end;

destructor TStepGroup.Destroy();
begin
  Clear();
  t_steps.Free();
  inherited Destroy();
end;

function TStepGroup.GetSteps(): integer;
begin
  result := t_steps.Count;
end;

function  TStepGroup.GetFirstStep(): TTestStep;
begin
  if (t_steps.Count > 0) then i_curstep := 0
  else i_curstep := -1;
  result := GetCurStep();
end;

function  TStepGroup.GetPrevStep(): TTestStep;
begin
  dec(i_curstep);
  if (i_curstep < 0) then i_curstep := -1;
  result := GetCurStep()
end;

function  TStepGroup.GetCurStep(): TTestStep;
begin
  if ((i_curstep >= 0) and (i_curstep < t_steps.Count)) then result := TTestStep(t_steps.Objects[i_curstep])
  else result := nil;
end;

function  TStepGroup.GetNextStep(): TTestStep;
begin
  inc(i_curstep);
  if (i_curstep >= t_steps.Count) then i_curstep := t_steps.Count;
  result := GetCurStep()
end;

function  TStepGroup.GetLastStep(): TTestStep;
begin
  i_curstep := t_steps.Count - 1;
  result := GetCurStep();
end;

function  TStepGroup.GetRelativeStep(const relidx: integer): TTestStep;
begin
  i_curstep := i_curstep + relidx;
  if (i_curstep < 0) then i_curstep := -1
  else if (i_curstep >= t_steps.Count) then i_curstep := t_steps.Count;
  result := GetCurStep();
end;

function TStepGroup.AddStep(const step: TTestStep): boolean;
var s_stepnr: string;
begin
  result := false;
  if assigned(step) then begin
    s_stepnr := step.GetFieldValue(SF_NR);
    if (t_steps.IndexOf(s_stepnr) < 0) then
      result := (t_steps.AddObject(s_stepnr, step) >= 0);
  end;
end;

function  TStepGroup.StepByIndex(const idx: integer): TTestStep;
begin
  if (idx < 0) then i_curstep := -1
  else if (idx >= t_steps.Count) then i_curstep := t_steps.Count
  else i_curstep := idx;
  result := GetCurStep();
end;

function  TStepGroup.StepByNr(const nr: string): TTestStep;
var i_idx: integer;
begin
  i_idx := t_steps.IndexOf(nr);
  result := StepByIndex(i_idx);
end;

function  TStepGroup.IndexOfStep(const stepnr: string): integer;
begin
  result := t_steps.IndexOf(stepnr);
end;

function  TStepGroup.StepNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then result := t_steps.Strings[idx]
  else result := '';
end;

procedure TStepGroup.RemoveStep(const stepnr: string);
var i_idx: integer;
begin
  i_idx := t_steps.IndexOf(stepnr);
  RemoveStepByIndex(i_idx);
end;

procedure TStepGroup.Clear();
var i_idx: integer;
begin
  i_idx := t_steps.Count - 1;
  while (i_idx >= 0) do begin
    RemoveStepByIndex(i_idx);
    i_idx := t_steps.Count - 1;
  end;
  i_curstep := -1;
end;

constructor TTestCase.Create();
begin
  inherited Create();
  i_casenr := -1;
  s_title := '';
end;

destructor TTestCase.Destroy();
begin
  inherited Destroy();
end;

function TTestCase.AddStep(const step: TTestStep): boolean;
var s_stepnr: string; i_cnr: integer;
begin
  result := false;
  if assigned(step) then begin
    s_stepnr := step.GetFieldValue(SF_NR);
    if TStepContainer.CalcCaseNr(s_stepnr, i_cnr) then begin
      //add first step in this test case
      if (t_steps.Count <= 0) then begin
        result := (t_steps.AddObject(s_stepnr, step) >= 0);
        i_casenr := i_cnr;
        if s_title = '' then s_title := step.GetFieldValue(SF_T)
      end else begin
        //only the step which has same case number is allowed
        if (i_cnr = i_casenr) then
          result := (t_steps.AddObject(step.GetFieldValue(SF_NR), step) >= 0);
      end;
    end;
  end;
end;

constructor TStepLoop.Create();
begin
  inherited Create();
  i_casenr := -1;
  b_closed := false;
end;

destructor TStepLoop.Destroy();
begin
  //todo:
  inherited Destroy();
end;

function TStepLoop.AddStep(const step: TTestStep): boolean;
var s_func, s_stepnr: string; i_cnr: integer;
begin
  result := false;
  if assigned(step) then begin
    s_func := step.GetFieldValue(SF_FCT);
    s_stepnr := step.GetFieldValue(SF_NR);
    if TStepContainer.CalcCaseNr(s_stepnr, i_cnr) then begin
      if (t_steps.Count <= 0) then begin    //add only test step with function LoopBegin
        if SameText(s_func, CSTR_LOOP_BEGIN) then begin
          result := (t_steps.AddObject(s_stepnr, step) >= 0);
          i_casenr := i_cnr;
        end;
      end else if (not b_closed) then begin //add only test steps between function LoopBegin and LoopBegin
        if (i_cnr = i_casenr) then begin //a loop is allowed in one test case
          result := (t_steps.AddObject(step.GetFieldValue(SF_NR), step) >= 0);
          b_closed := SameText(s_func, CSTR_LOOP_END); //set loop end
        end;
      end;
    end;
  end;
end;

function TCaseGroup.GetCaseNumbers(): string;
begin
  result := t_cases.DelimitedText;
end;

procedure TCaseGroup.RemoveCaseByIndex(const idx: integer);
var t_case: TTestCase;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then begin
    t_case := TTestCase(t_cases.Objects[idx]);
    if (b_freecase and assigned(t_case)) then t_case.Free();
    t_cases.Delete(idx);
  end;
end;

constructor TCaseGroup.Create();
begin
  inherited Create();
  i_curcase := -1;
  b_freecase := false;
  t_cases := TStringList.Create();
  t_cases.Delimiter := Char(';');
end;

destructor TCaseGroup.Destroy();
begin
  Clear();
  t_cases.Free();
  inherited Destroy();
end;

function TCaseGroup.GetCases(): integer;
begin
  result := t_cases.Count;
end;

function TCaseGroup.GetFirstCase(): TTestCase;
begin
  if (t_cases.Count > 0) then i_curcase := 0
  else i_curcase := -1;
  result := GetCurCase();
end;

function TCaseGroup.GetPrevCase(): TTestCase;
begin
  dec(i_curcase);
  if (i_curcase < 0) then i_curcase := -1;
  result := GetCurCase()
end;

function TCaseGroup.GetCurCase(): TTestCase;
begin
  if ((i_curcase >= 0) and (i_curcase < t_cases.Count)) then result := TTestCase(t_cases.Objects[i_curcase])
  else result := nil;
end;

function TCaseGroup.GetNextCase(): TTestCase;
begin
  inc(i_curcase);
  if (i_curcase >= t_cases.Count) then i_curcase := t_cases.Count;
  result := GetCurCase()
end;

function TCaseGroup.GetLastCase(): TTestCase;
begin
  i_curcase := t_cases.Count - 1;
  result := GetCurCase();
end;

function TCaseGroup.GetRelativeCase(const relidx: integer): TTestCase;
begin
  i_curcase := i_curcase + relidx;
  if (i_curcase < 0) then i_curcase := -1
  else if (i_curcase >= t_cases.Count) then i_curcase := t_cases.Count;
  result := GetCurCase();
end;

function TCaseGroup.AddCase(const tcase: TTestCase): boolean;
var s_casenr: string;
begin
  result := false;
  if assigned(tcase) then begin
    s_casenr := IntToStr(tcase.CaseNr);
    if (t_cases.IndexOf(s_casenr) < 0) then
      result := (t_cases.AddObject(s_casenr, tcase) >= 0);
  end;
end;

function  TCaseGroup.CaseByIndex(const idx: integer): TTestCase;
begin
  if (idx < 0) then i_curcase := -1
  else if (idx >= t_cases.Count) then i_curcase := t_cases.Count
  else i_curcase := idx;
  result := GetCurCase();
end;

function  TCaseGroup.CaseByNr(const casenr: string): TTestCase;
var i_caseidx, i_casenr: integer;
begin
  if TryStrToInt(casenr, i_casenr) then i_caseidx := t_cases.IndexOf(IntToStr(i_casenr))
  else i_caseidx := t_cases.IndexOf(casenr);
  result := CaseByIndex(i_caseidx);
end;

function  TCaseGroup.CaseByNr(const nr: integer): TTestCase;
var s_casenr: string;
begin
  s_casenr := IntToStr(nr);
  result := CaseByNr(s_casenr);
end;

function  TCaseGroup.IndexOfCase(const casenr: string): integer;
begin
  result := t_cases.IndexOf(casenr);
end;

function  TCaseGroup.CaseNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then result := t_cases.Strings[idx]
  else result := '';
end;

procedure TCaseGroup.RemoveCase(const casenr: string);
var i_idx: integer;
begin
  i_idx := t_cases.IndexOf(casenr);
  RemoveCaseByIndex(i_idx);
end;

procedure TCaseGroup.Clear();
var i_idx: integer;
begin
  i_idx := t_cases.Count - 1;
  while (i_idx >= 0) do begin
    RemoveCaseByIndex(i_idx);
    i_idx := t_cases.Count - 1;
  end;
  i_curcase := -1;
end;

constructor TLoopStack.Create();
begin
  //todo:
end;

destructor TLoopStack.Destroy();
begin
  //todo:
end;

procedure TLoopStack.PushLoop(const tloop: TStepLoop);
begin
  //todo:
end;

function TLoopStack.PopLoop(): TStepLoop;
begin
  result := nil;
  //todo:
end;

function TLoopStack.GetCurLoop(): TStepLoop;
begin
  result := nil;
  //todo
end;

constructor TTestSequence.Create();
begin
  inherited Create();
  t_casegrp := TCaseGroup.Create();
  t_loopstack := TLoopStack.Create();
end;

destructor TTestSequence.Destroy();
begin
  Clear();
  t_casegrp.Free();
  t_loopstack.Free();
  inherited Destroy();
end;

function TTestSequence.AddStep(const tstep: TTestStep): boolean;
begin
  result := inherited AddStep(tstep);
  if result then UpdateCaseByStep(tstep);
end;

function TTestSequence.AddCase(const tcase: TTestCase): boolean;
var i: integer;
begin
  result := false;
  if assigned(tcase) then begin
    result := t_casegrp.AddCase(tcase);
    if result then begin
      for i := 0 to tcase.StepCount - 1 do
        inherited AddStep(tcase.StepByIndex(i));
    end;
  end;
end;

procedure TTestSequence.Clear();
begin
  inherited Clear();
  t_casegrp.Clear();
  i_lastcnr := -1;
  //todo: clear loop
end;

procedure  TTestSequence.UpdateCaseByStep(const tstep: TTestStep);
var t_tcase: TTestCase; i_casenr: integer; s_stepnr, s_casenr: string;
begin
  if assigned(tstep) then begin
    s_stepnr := tstep.GetFieldValue(SF_NR);
    if CalcCaseNr(s_stepnr, i_casenr) then begin
      s_casenr := IntToStr(i_casenr);
      if (i_casenr = i_lastcnr) then begin //update indexto of last case if the case number is same as last one
        t_tcase := t_casegrp.CaseByNr(i_casenr);
        if assigned(t_tcase) then t_tcase.AddStep(tstep);
      end else begin  //create a new case if the case number differs from last one
        t_tcase := TTestCase.Create();
        t_tcase.CaseNr := i_casenr;
        if (t_casegrp.AddCase(t_tcase)) then t_tcase.AddStep(tstep)
        else FreeAndNil(t_tcase);
        i_lastcnr := i_casenr;
      end;
    end;
  end;
end;

// =============================================================================
//    Description  : analyze the given string casenrs and return back a list of
//                   valid test case index in t_casenrs.
//    Parameter    : casenrs, a string to represent test cases, e.g.:
//                   1. 'all' - all test case in the container
//                   2. '11-201' - test cases from 11 to 201
//                   3. '110, 220, 360' - test cases of 110, 220 and 360
//                   4. '28-400, 520, 650' - combination of 2. and 3.
//    Return       : a list of test case index in t_casenrs, which is a subset of test cases
//                   in the container, subset(incl) - subset(excl)
//    First author : 2016-05-10 /bsu/
//                   WARNING: because of using set in this function the count of
//                   test cases is not allowed beyond CINT_CASES_MAX + 1 (256)
//    History      :
// =============================================================================
function  TStepContainer.CaseIndexSet(const casenrs: string): TIndexSet;
var i, i_maxidx: integer; t_cstrs, t_cnrs: TStrings;
    i_cnr, i_cnr1, i_cnr2, i_idx: integer; s_cnr: string;
begin
  result := [];
  if ((t_casegrp.CaseCount - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
  else i_maxidx := t_casegrp.CaseCount - 1;

  if SameText(casenrs, 'all') then begin
    for i := 0 to i_maxidx do Include(result, i);
  end else  begin
    if (casenrs <>'') then begin
      t_cstrs := TStringList.Create();
      if (ExtractStrings([','], [' '], PChar(casenrs), t_cstrs) > 0) then begin
        t_cnrs := TStringList.Create();
        for i := 0 to t_cstrs.Count - 1 do begin
          t_cnrs.Clear();
          if (ExtractStrings(['-'], [' '], PChar(t_cstrs[i]), t_cnrs) > 1) then begin
            if (TryStrToInt(t_cnrs[0], i_cnr1) and TryStrToInt(t_cnrs[t_cnrs.Count-1], i_cnr2)) then begin
              for i_cnr := i_cnr1 to i_cnr2 do begin
                s_cnr := IntToStr(i_cnr);
                i_idx := t_casegrp.IndexOfCase(s_cnr);
                if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
              end;
            end;
          end else begin
            if TryStrToInt(t_cstrs[i], i_cnr) then begin
              s_cnr := IntToStr(i_cnr);
              i_idx := t_casegrp.IndexOfCase(s_cnr);
              if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
            end;
          end;
        end;
        FreeAndNil(t_cnrs);
      end;
      FreeAndNil(t_cstrs);
    end;
  end;
end;

procedure TStepContainer.UpdateLoop(const stepnr, funcname: string);
begin
  //todo:
end;

procedure TStepContainer.RemoveStepByIndex(const idx: integer);
var t_step: TTestStep;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then begin
    t_step := TTestStep(t_steps.Objects[idx]);
    if assigned(t_step) then t_step.Free();
    t_steps.Delete(idx);
  end;
end;

// =============================================================================
//    Description  : analyze the given strings incl and excl and update t_sequence
//    Parameter    : incl, a string to represent inclusive test cases
//                   excl, a string to represent exclusive test cases
//                   see function IndexSet
//    Return       : --
//    First author : 2016-05-18 /bsu/
//                   WARNING: because of using set in this function the count of
//                   test cases is not allowed beyond CINT_CASES_MAX + 1 (256).
//                   The cases whose indexes (from 0 to 255) is greater than
//                   CINT_CASES_MAX (255), are not considered.
//    History      :
// =============================================================================
procedure TStepContainer.UpdateSequence(const incl: string; const excl: string);
var set_incl, set_excl, set_result: TIndexSet; i, i_maxidx: integer; t_case: TTestCase;
begin
  if ((incl <> s_inclusive) or (excl <> s_exclusive)) then begin
    t_sequence.Clear();
    set_incl := CaseIndexSet(incl); s_inclusive := incl;
    set_excl := CaseIndexSet(excl); s_exclusive := excl;
    set_result := set_incl - set_excl;

    //only 256 test cases is allowed in moment
    if ((t_casegrp.CaseCount - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
    else i_maxidx := t_casegrp.CaseCount - 1;
    for i := 0 to i_maxidx do begin
      if (i in set_result) then begin
        t_case := t_casegrp.CaseByIndex(i);
        t_sequence.AddCase(t_case);
      end;
    end;
  end;
end;

constructor TStepContainer.Create();
begin
  inherited Create();
  t_casegrp.FreeByRemove := true;
  t_sequence := TTestSequence.Create();
end;

destructor TStepContainer.Destroy();
begin
  Clear();
  t_sequence.Free();
  inherited Destroy();
end;

function TStepContainer.LoadStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep;
begin
  t_step := TTestStep.Create();
  t_step.LoadFields(fields);
  result := AddStep(t_step);
  if result then begin
    UpdateLoop(fields[SF_NR], fields[SF_FCT]);
  end else  FreeAndNil(t_step);
end;

function TStepContainer.GetTestSequence(const incl: string; const excl: string): TTestSequence;
begin
  UpdateSequence(incl, excl);
  result := t_sequence;
end;

procedure TStepContainer.Clear();
begin
  inherited Clear();
  t_sequence.Clear();
  s_inclusive := '';
  s_exclusive := ''
end;


// =============================================================================
//    Description  : save field values of test steps into a file
//    Parameter    : sfile, file name to save
//    Return       : --
//    First author : 2016-05-09 /bsu/
//    History      :
// =============================================================================
procedure TStepContainer.SaveFile(const sfile: string);
var i: integer; s_line: string; j:EStepField; t_stepvals: TStringList; t_step: TTestStep;
begin
  t_stepvals := TStringList.Create();
  for i := 0 to t_steps.Count - 1 do begin
    t_step := TTestStep(t_steps.Objects[i]);
    s_line := t_step.GetFieldValue(SF_NR);
    for j := SF_T to High(EStepField) do s_line := s_line + ';' + #9 + t_step.GetFieldValue(j);
    t_stepvals.Add(s_line);
  end;
  t_stepvals.SaveToFile(sfile);
  t_stepvals.Free();
end;

procedure TStepContainer.Assign(const source: TStepContainer);
var t_step: TTestStep; i: integer;
begin
  if assigned(source) then begin
    Clear();
    for i := 0 to source.StepCount - 1 do begin
      t_step := TTestStep.Create();
      t_step.Assign(source.StepByIndex(i));
      self.AddStep(t_step);
    end;
  end;
end;

end.
