// =============================================================================
// Module name  : $RCSfile: StepDescriptor.pas,v $
// Description  : This unit defines test step (TTestStep) and its corresponding .
//                elements, e.g, TStepResult, TStepContainer, TStepField a.s.o.
//                script is composed of variable-value pairs and test steps.
// looks like:
//                |--test step--------------------------------------------------------------------------------------------------|
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                |[Field Name]:[Field Value]|----|test step--------------------------------------------------------------------------------------------------|
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
// Author       : 2016-12-11 /bsu/
// History      :
//==============================================================================

unit StepDescriptor;

interface
uses Classes, Contnrs;

type
  //enumation for all fields in a test step
  EStepField = (
                SF_NR,      //e.g. Nr: 10.00;
                SF_T,       //e.g. T:'beginning of test case 1';
                SF_INIT,    //e.g. R_on: '101'; or Init:'';
                SF_FCT,     //e.g. Fkt: U_Mess_DC;
                SF_M,       //e.g. M:'';
                SF_PAR,     //e.g. Par:'';
                SF_FINAL,   //e.g. R_off: '101'; or Final:'';
                SF_TOL,     //e.g. Tol:(A:''; Min:0; Max:0)
                SF_TOL_A,   //e.g. A:'';
                SF_TOL_MIN, //e.g. Min:0;
                SF_TOL_MAX  //e.g. Max:0
                );
  //enumation for state of test step by execution
  EStepState = (
                SS_UNKNOWN, //state, before execution or unknown error
                SS_INIT,    //state in initializing
                SS_FCT,     //state in running call-function
                SS_TOL,     //state in validation of the value
                SS_FINAL,   //state in finalization
                SS_OK       //state for ok if the step is succesfully executed completely
                );

  //enumation: indicates whether pseudo-strings exist in field
  EFieldEvalLevel = (
                FEL_SPSEUDO,//static Pseudo-string exists in input
                FEL_DPSEUDO,//dynamic Pseudo-string exists in input
                FEL_FINAL   //final string (all pseudo-strings are replaced)
                );

  FieldStringArray = array[EStepField] of string;
  TStepResult = class;
  TTestCase = class;
  TStepGroup = class;
  TTestSequence = class;
  TIndexSet = set of byte;

  // a class representing a field in a step, e.g., Nr, T, R_on, Fkt, Par, R_off, Tol(A, Min, Max)
  TStepField = class
  protected
    s_input:  string;   //to save original input string
    e_fel:    EFieldEvalLevel;
    t_evals:  TStrings; //to save string values, which are evaluated, if a pseudo-sting exists
  protected
    procedure SetInputStr(const val: string);
    procedure SetEvalStrings(const vals: TStrings);

  public
    constructor Create();
    destructor Destroy(); override;

    property  InputString: string read s_input write SetInputStr;
    property  EvalLevel: EFieldEvalLevel read e_fel write e_fel;
    property  EvalStrings: TStrings read t_evals write SetEvalStrings;

    procedure Assign(const source: TStepField);
  end;
  StepFieldArray = array[EStepField] of TStepField;

  //a class representing a test step
  TTestStep = class
  protected
    a_fields:   StepFieldArray;
    t_result:   TStepResult;
    t_fvalues:  TStrings;     //to save the list of a field value, which is split. See SplitFieldValues
  protected
    procedure FreeFields();
    function  IsMinusStepNr(): boolean;
    procedure AssignResult(const source: TStepResult);

  public
    constructor Create();
    destructor Destroy(); override;

    property  StepResult: TStepResult read t_result write AssignResult;
    property  IsMinusStep: boolean read IsMinusStepNr;
    
    procedure InputFields(const fields: FieldStringArray);
    class function  GetFieldName(const esf: EStepField): string;
    function  GetField(const esf: EStepField): TStepField;
    function  GetFieldValue(const esf: EStepField): string;
    procedure SetFieldValue(const esf: EStepField; const sval: string);
    function  SplitFieldValues(const esf: EStepField; const separator: char): TStrings;
    procedure Assign(const source: TTestStep);
  end;

  //a class representing the result of a test step
  TStepResult = class
  protected
    b_state:  EStepState;
    v_result: Variant;
  protected

  public
    constructor Create();
    destructor Destroy(); override;

    property Value: Variant read v_result write v_result;
    property State: EStepState read b_state write b_state;

    function GetString(var val: string): boolean;
    function GetInteger(var val: integer): boolean;
    function GetInt64(var val: int64): boolean;
    function GetReal(var val: double): boolean;
    function GetBoolean(var val: boolean): boolean;
    function GetHexStr(var val: string; const len: integer): boolean;
    procedure Clear();
    procedure Assign(const source: TStepResult);
  end;

  //a class composed of all test steps
  TStepContainer = class
  class function CalcCaseNr(const stepnr: string; var casenr: integer): boolean;
  protected
    t_steps:      TStrings;     //a list of test steps (step number and object pairs)
    i_curstep:    integer;      //index of step, to indicate current step, which is now selected, -1 means no selection
    t_cases:      TStrings;     //a list of test cases (case number and object pairs)
    i_lastcnr:    integer;      //to save last case number during reading test script
    t_sequence:   TTestSequence;//to save current test sequence, in which a case list and a step list are inclusive

    s_inclusive:  string;       //to save expression of inclusive cases, see function TestSequence
    s_exclusive:  string;       //to save expression of exclusive cases, see function TestSequence

  protected
    procedure UpdateCase(const stepnr, title: string);
    procedure UpdateLoop(const stepnr, funcname: string);
    function  GetSteps(): integer;
    function  GetCases(): integer;
    function  GetPrevStep(): TTestStep;
    function  GetCurStep(): TTestStep;
    function  GetNextStep(): TTestStep;
    function  GetCurCase(): TTestCase;

  public
    constructor Create();
    destructor  Destroy(); override;

    property  CountStep: integer read GetSteps;
    property  CountCase: integer read GetCases;
    property  PreviousStep: TTestStep read GetPrevStep;
    property  CurrentStep: TTestStep read GetCurStep;
    property  NextStep: TTestStep read GetNextStep;
    property  CurrentCase: TTestCase read GetCurCase;
    property  TestSequence: TTestSequence read t_sequence;

    function  AddStep(const fields: FieldStringArray): boolean;
    function  StepByIndex(const idx: integer): TTestStep;
    function  StepByNr(const nr: string): TTestStep; 
    function  CaseByIndex(const caseidx: integer): TTestCase;
    function  CaseByNr(const casenr: string): TTestCase; overload;
    function  CaseByNr(const nr: integer): TTestCase; overload;
    function  IndexOfStep(const stepnr: string): integer;
    function  IndexOfCase(const casenr: string): integer;
    function  StepNrOf(const idx: integer): string;
    function  CaseNrOf(const idx: integer): string;
    function  CaseIndexSet(const casenrs: string): TIndexSet;
    
    procedure UpdateSequence(const incl: string; const excl: string = '');
    procedure Clear();
    procedure SaveFile(const sfile: string);
    procedure Assign(const source: TStepContainer);
  end;

  //a class representing a step group, which is composed of test steps in the container
  TStepGroup = class
  protected
    t_stepnrs:    TStrings;       //a list of test steps (step number and reference pairs)
    i_curstepidx: integer;        //indicate current index of t_stepnrs
    t_container:  TStepContainer; //reference to step container

  protected
    function  GetSteps(): integer;
    function  GetFirstTestStep(): TTestStep;
    function  GetPrevTestStep(): TTestStep;
    function  GetCurTestStep(): TTestStep; virtual;
    function  GetNextTestStep(): TTestStep;
    function  GetLastTestStep(): TTestStep;
    procedure Clear(); virtual;

  public
    constructor Create(const container: TStepContainer);
    destructor Destroy(); override;

    property  Count: integer read GetSteps;
    property  StepNrList: TStrings read t_stepnrs;
    property  FirstTestStep: TTestStep read GetFirstTestStep;
    property  PreviousTestStep: TTestStep read GetPrevTestStep;
    property  CurrentTestStep: TTestStep read GetCurTestStep;
    property  NextTestStep: TTestStep read GetNextTestStep;
    property  LastTestStep: integer read GetLastTestStep;

    function  MoveStepIndex(const rela: integer): integer;
    procedure UpdateCurStepIndex();
  end;

  //a class, represents a loop in a test case, which starts with script function
  //'LoopBegin' and ends with 'LoopEnd'
  //warning: step loops may be nested, but they are only allowed  in one test case.
  //         a loop is not allowed between two test cases, that means, the beginning
  //         and its corresponding end muss exist in one case.
  TStepLoop = class(TStepGroup)
  protected
    i_casenr:   integer; //indicates test case, in which the loop exists
    b_closed:  boolean;  //indicates if the loop is closed
  protected
    function GetBeginStep(): TTestStep;
    function GetEndStep(): TTestStep;
    procedure SetBeginStep(const step: TTestStep);
    procedure SetEndStep(const step: TTestStep);

  public
    constructor Create(const container: TStepContainer);
    destructor Destroy(); override;

    property BeginStep: TTestStep read GetBeginStep write SetBeginStep;
    property EndStep: TTestStep read GetEndStep write SetEndStep;
    property LoopClosed : boolean read b_closed;
  end;

  //a class representing a set of test steps in the container (continue from an index to another),
  //e.g. a test case, which have the same main number in field Nr (xxx of xxx.yyy)
  TTestCase = class(TStepGroup)
  protected
    i_casenr:     integer; //number of the group, e.g. test case number
    s_title:      string;  //title of this group
  protected
    function GetIndexFrom(): integer;
    function GetIndexTo(): integer;
    procedure SetIndexFrom(const idx: integer);
    procedure SetIndexTo(const idx: integer);

  public
    constructor Create(const container: TStepContainer);
    destructor Destroy(); override;

    property IndexFrom: integer read GetIndexFrom write SetIndexFrom;
    property IndexTo: integer read GetIndexTo write SetIndexTo;
    property CaseNr: integer read i_casenr write i_casenr;
    property CaseTitle: string read s_title write s_title;
  end;

  //a class representing a test sequence, which is composed of test groups in the container
  TTestSequence = class(TStepGroup)
  protected
    a_cases:      array of integer; //to save indexes of test cases, which are contained in t_container
    i_curcaseidx: integer;          //to indicate current element in a_cases;
    t_casenrs:    TStrings;         //to save numbers of test cases in this test sequence

  protected
    function  GetCases(): integer;
    procedure UpdateCaseSteps(const caseidx: byte);
    procedure Clear(); override;
    function  CurTestStep(): integer; override;
    function  FirstIndexOfCase(): integer;
    function  PrevIndexOfCase(): integer;
    function  CurIndexOfCase(): integer;
    function  NextIndexOfCase(): integer;
    function  LastIndexOfCase(): integer;

  public
    constructor Create(const container: TStepContainer);
    destructor Destroy(); override;

    property  CountCase: integer read GetCases;
    property  CaseNrList: TStrings read t_casenrs;
    property  FirstCaseIndex: integer read FirstIndexOfCase;
    property  PreviousCaseIndex: integer read PrevIndexOfCase;
    property  CurrentCaseIndex: integer read CurIndexOfCase;
    property  NextCaseIndex: integer read NextIndexOfCase;
    property  LastCaseIndex: integer read LastIndexOfCase;

    procedure Update(const csset: TIndexSet);
  end;

const
  CINT_CASES_MAX : integer = 255;
  CSTR_FIELD_NAMES :  FieldStringArray = (
                        'NR',
                        'T',
                        'R_ON',
                        'FKT',
                        'M',
                        'PAR',
                        'R_OFF',
                        'TOL',
                        'A',
                        'MIN',
                        'MAX'
                      );

implementation
uses SysUtils, StrUtils, Variants;

procedure TStepField.SetInputStr(const val: string);
begin
  s_input := val;
end;

procedure TStepField.SetEvalStrings(const vals: TStrings);
begin
  t_evals.Clear();
  t_evals.AddStrings(vals);
end;

constructor TStepField.Create();
begin
  inherited Create();
  e_fel := FEL_SPSEUDO;
  t_evals := TStringList.Create();
end;

destructor TStepField.Destroy();
begin
  t_evals.Free();
  inherited Destroy();
end;

procedure TStepField.Assign(const source: TStepField);
begin
  if assigned(source) then begin
    s_input := source.InputString;
    e_fel := source.EvalLevel;
    SetEvalStrings(source.EvalStrings);
  end;
end;

procedure  TTestStep.FreeFields();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do
    if assigned(a_fields[i]) then FreeAndNil(a_fields[i]);
end;

function  TTestStep.IsMinusStepNr(): boolean;
var f_stepnr: single; s_stepnr: string;
begin
  result := false;
  s_stepnr := ReplaceStr(GetFieldValue(SF_NR), '.', DecimalSeparator);
  if TryStrToFloat(s_stepnr, f_stepnr) then result := (f_stepnr < 0.0);
end;

constructor TTestStep.Create();
begin
  inherited Create();
  t_fvalues := TStringList.Create();
end;

destructor TTestStep.Destroy();
begin
  if assigned(t_result) then t_result.Free();
  FreeFields();
  t_fvalues.Free();
  inherited Destroy();
end;

procedure TTestStep.InputFields(const fields: FieldStringArray);
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do begin
    if fields[i] <> '' then begin
      a_fields[i] := TStepField.Create();
      a_fields[i].InputString := fields[i];
    end;
  end;
end;

function  TTestStep.GetField(const esf: EStepField): TStepField;
begin
  result := a_fields[esf];
end;

class function  TTestStep.GetFieldName(const esf: EStepField): string;
begin
  result := CSTR_FIELD_NAMES[esf];
end;

function  TTestStep.GetFieldValue(const esf: EStepField): string;
begin
  result := '';
  if assigned(a_fields[esf]) then result := a_fields[esf].InputString;
end;

procedure TTestStep.SetFieldValue(const esf: EStepField; const sval: string);
begin
  if assigned(a_fields[esf]) then a_fields[esf].InputString := sval
  else if (sval <> '') then begin
    a_fields[esf] := TStepField.Create();
    a_fields[esf].InputString := sval;
  end;
end;

function  TTestStep.SplitFieldValues(const esf: EStepField; const separator: char): TStrings;
var s_value: string;
begin
  t_fvalues.Clear();
  s_value := GetFieldValue(esf);
  if (s_value <> '') then ExtractStrings([separator], [' ', Char(9)], PChar(s_value), t_fvalues);
  result := t_fvalues;
end;

procedure TTestStep.Assign(const source: TTestStep);
var i: EStepField;
begin
  if assigned(source) then begin
    for i := Low(EStepField) to High(EStepField) do SetFieldValue(i, source.GetFieldValue(i));
    AssignResult(source.StepResult);
  end;
end;

procedure TTestStep.AssignResult(const source: TStepResult);
begin
  if assigned(source) then begin
    if not assigned(t_result) then t_result := TStepResult.Create();
    t_result.Assign(source);
  end else if assigned(t_result) then FreeAndNil(t_result);
end;

constructor TStepResult.Create();
begin
  inherited Create();
  b_state := SS_UNKNOWN;
end;

destructor TStepResult.Destroy();
begin
  inherited Destroy();
end;

function TStepResult.GetString(var val: string): boolean;
begin
  result := ((not VarIsNull(v_result)) and (not VarIsEmpty(v_result)));
  if not result then val := v_result;
end;

function TStepResult.GetInteger(var val: integer): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetInt64(var val: int64): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetReal(var val: double): boolean;
var s_real: string;
begin
  if (VarType(v_result) = varString) then begin  //if the local decimal separator is not '.'
    s_real := v_result;
    s_real := ReplaceStr(s_real, '.', DecimalSeparator);
    result := TryStrToFloat(s_real, val);
  end else begin
    try
      val := v_result;
      result := true;
    except
      result := false;
    end;
  end;
end;

function TStepResult.GetBoolean(var val: boolean): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetHexStr(var val: string; const len: integer): boolean;
var i_val64: int64;
begin
  result := GetInt64(i_val64);
  if result then val := IntToHex(i_val64, len);
end;

procedure TStepResult.Clear();
begin
  b_state := SS_UNKNOWN;
  VarClear(v_result);
end;

procedure TStepResult.Assign(const source: TStepResult);
begin
  if assigned(source) then begin
    b_state := source.State;
    VarCopy(v_result, source.Value);
  end;
end;

class function TStepContainer.CalcCaseNr(const stepnr: string; var casenr: integer): boolean;
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

procedure  TStepContainer.UpdateCase(const stepnr, title:string);
var t_tcase: TTestCase; i_casenr, i_curstepidx: integer;
begin
  if CalcCaseNr(stepnr, i_casenr) then begin
    i_curstepidx := t_steps.Count - 1;
    if (i_casenr = i_lastcnr) then begin //update indexto of last case if the case number is same as last one
      if (t_cases.Count > 0) then begin
        t_tcase := TTestCase(t_cases.Objects[t_cases.Count - 1]);
        if assigned(t_tcase) then t_tcase.IndexTo := i_curstepidx;
      end;
    end else begin  //create a new case if the case number differs from last one
      t_tcase := TTestCase.Create(self);
      if (t_cases.AddObject(IntToStr(i_casenr), t_tcase) >= 0) then begin
        t_tcase.CaseNr := i_casenr;
        t_tcase.CaseTitle := title;
        t_tcase.IndexFrom := i_curstepidx;
        t_tcase.IndexTo := i_curstepidx;
      end else FreeAndNil(t_tcase);
      i_lastcnr := i_casenr;
    end;
  end;
end;

procedure TStepContainer.UpdateLoop(const stepnr, funcname: string);
begin
  //todo:
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
  if ((t_cases.Count - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
  else i_maxidx := t_cases.Count - 1;

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
                i_idx := t_cases.IndexOf(s_cnr);
                if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
              end;
            end;
          end else begin
            if TryStrToInt(t_cstrs[i], i_cnr) then begin
              s_cnr := IntToStr(i_cnr);
              i_idx := t_cases.IndexOf(s_cnr);
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

function  TStepContainer.GetSteps(): integer;
begin
  result := t_steps.Count;
end;

function  TStepContainer.GetCases(): integer;
begin
  result := t_cases.Count;
end;

function  TStepContainer.GetPrevStep(): TTestStep;
begin
  dec(i_curstep);
  result := GetCurStep();
end;

function  TStepContainer.GetCurStep(): TTestStep;
begin
  result := Nil;
  if ((i_curstep < 0) or (i_curstep >= t_steps.Count)) then i_curstep := -1
  else result := TTestStep(t_steps.Objects[i_curstep])
end;

function  TStepContainer.GetNextStep(): TTestStep;
begin
  inc(i_curstep);
  result := GetCurStep();
end;

function  TStepContainer.GetCurCase(): TTestCase;
var i_casenr: integer; s_stepnr: string; t_step: TTestStep;
begin
  result := Nil;
  t_step := GetCurStep();
  if assigned(t_step) then begin
    s_stepnr := t_step.GetFieldValue(SF_NR);
    if CalcCaseNr(s_stepnr, i_casenr) then result := CaseByNr(i_casenr);
  end;
end;

constructor TStepContainer.Create();
begin
  inherited Create();
  i_curstep := -1;
  t_steps := TStringList.Create();
  t_cases := TStringList.Create();
  t_cases.Delimiter := Char(';');
  t_sequence := TTestSequence.Create(self);
end;

destructor TStepContainer.Destroy();
begin
  Clear();
  t_steps.Free();
  t_cases.Free();
  t_sequence.Free();
  inherited Destroy();
end;

function TStepContainer.AddStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep;
begin
  t_step := TTestStep.Create();
  t_step.InputFields(fields);
  result := (t_steps.AddObject(fields[SF_NR], t_step) >= 0);
  if result then begin
    UpdateCase(fields[SF_NR], fields[SF_T]);
    UpdateLoop(fields[SF_NR], fields[SF_FCT]);
  end else  FreeAndNil(t_step);
end;

function  TStepContainer.StepByIndex(const idx: integer): TTestStep;
begin
  result := nil;
  if ((idx >= 0) and (idx < t_steps.Count)) then begin
    i_curstep := idx;
    result := TTestStep(t_steps.Objects[i_curstep]);
  end;
end;

function  TStepContainer.StepByNr(const nr: string): TTestStep;
var i_idx: integer;
begin
  i_idx := t_steps.IndexOf(nr);
  result := StepByIndex(i_idx);
end;

function  TStepContainer.CaseByIndex(const caseidx: integer): TTestCase;
begin
  result := nil;
  if ((caseidx >= 0) and (caseidx < t_cases.Count)) then begin
    result := TTestCase(t_cases.Objects[caseidx]);
    if assigned(result) then StepByIndex(result.IndexFrom);
  end;
end;

function  TStepContainer.CaseByNr(const casenr: string): TTestCase;
var i_caseidx, i_casenr: integer;
begin
  if TryStrToInt(casenr, i_casenr) then  i_caseidx := t_cases.IndexOf(IntToStr(i_casenr))
  else i_caseidx := -1;
  result := CaseByIndex(i_caseidx);
end;

function  TStepContainer.CaseByNr(const nr: integer): TTestCase;
var s_casenr: string;
begin
  s_casenr := IntToStr(nr);
  result := CaseByNr(s_casenr);
end;

function  TStepContainer.IndexOfStep(const stepnr: string): integer;
begin
  result := t_steps.IndexOf(stepnr);
end;

function  TStepContainer.IndexOfCase(const casenr: string): integer;
begin
  result := t_cases.IndexOf(casenr);
end;

function  TStepContainer.StepNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then result := t_steps.Strings[idx]
  else result := '';
end;

function  TStepContainer.CaseNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then result := t_cases.Strings[idx]
  else result := '';
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
var set_incl, set_excl, set_result: TIndexSet;
begin
  if ((incl <> s_inclusive) or (excl <> s_exclusive)) then begin
    t_sequence.Clear();
    set_incl := CaseIndexSet(incl); s_inclusive := incl;
    set_excl := CaseIndexSet(excl); s_exclusive := excl;
    set_result := set_incl - set_excl;
    t_sequence.Update(set_result);
  end;
end;

procedure TStepContainer.Clear();
var i: integer;
begin
  i_curstep := -1;
  i_lastcnr := -1;
  for i := 0 to t_steps.Count - 1 do
    if assigned(t_steps.Objects[i]) then t_steps.Objects[i].Free();
  t_steps.Clear();
  for i := 0 to t_steps.Count - 1 do
    if assigned(t_cases.Objects[i]) then t_cases.Objects[i].Free();
  t_cases.Clear();
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
    for i := 0 to source.CountStep - 1 do begin
      t_step := TTestStep.Create();
      t_step.Assign(source.StepByIndex(i));
      t_steps.AddObject(t_step.GetFieldValue(SF_NR), t_step);
    end;
  end;
end;

function TStepGroup.GetSteps(): integer;
begin
  result := t_stepnrs.Count;
end;

function  TStepGroup.GetFirstTestStep(): TTestStep;
begin
  i_curstepidx := 0;
  result := GetCurTestStep();
end;

function  TStepGroup.GetPrevTestStep(): TTestStep;
begin
  dec(i_curstepidx);
  result := GetCurTestStep()
end;

function  TStepGroup.GetCurTestStep(): TTestStep;
begin
  if ((i_curstepidx >= 0) and (i_curstepidx < t_stepnrs.Count)) then result := TTestStep(t_stepnrs.Objects(i_curstepidx))
  else result := nil;
end;

function  TStepGroup.GetNextTestStep(): TTestStep;
begin
  inc(i_curstepidx);
  result := GetCurTestStep()
end;

function  TStepGroup.GetLastTestStep(): TTestStep;
begin
  i_curstepidx := t_stepnrs.Count - 1;
  result := GetCurTestStep();
end;

procedure TStepGroup.Clear();
begin
  t_stepnrs.Clear();
end;

constructor TStepGroup.Create(const container: TStepContainer);
begin
  inherited Create();
  i_curstepidx := -1;
  t_stepnrs := TStringList.Create();
  t_container := container;
end;

destructor TStepGroup.Destroy();
begin
  Clear();
  t_stepnrs.Free();
  inherited Destroy();
end;

function  TStepGroup.MoveStepIndex(const rela: integer): integer;
begin
  i_curstepidx := i_curstepidx + rela;
  if ((i_curstepidx < 0) or (i_curstepidx >= t_stepnrs.Count)) then i_curstepidx := -1;
  result := i_curstepidx;
end;

procedure TStepGroup.UpdateCurStepIndex();
var i_index: integer; t_step: TTestStep;
begin
  if assigned(t_container) then begin
    t_step := t_container.CurrentStep;
    if assigned(t_step) then begin
      i_index := t_stepnrs.IndexOf(t_step.GetFieldValue(SF_NR));
      if i_index >= 0 then i_curstepidx := i_index;
    end;
  end;
end;

function TStepLoop.GetBeginStep(): TTestStep;
begin
  if (t_stepnrs.Count > 0) then result := TTestStep(t_stepnrs.Objects[0])
  else result := nil;
end;

function TStepLoop.GetEndStep(): integer;
var i_len: integer;
begin
  i_len := t_stepnrs.Count;
  if ((i_len > 0) and b_closed) then result := TTestStep(t_stepnrs.Objects[i_len - 1])
  else result := nil;
end;

procedure TStepLoop.SetBeginStep(const step: TTestStep);
var s_stepnr: string;
begin
  if assigned(step) then begin
    Clear();
    s_stepnr := step.GetFieldValue(SF_NR);
    t_stepnrs.AddObject(s_stepnr, step);
    TStepContainer.CalcCaseNr(s_stepnr, i_casenr);
  end;
end;

procedure TStepLoop.SetEndStep(const step: TTestStep);
var i_cnr: integer; t_step: TTestStep;
begin
  t_step := BeginStep;
  if (assigned(t_step) and assigned(step) and assigned(t_container) and (t_step <> step)) then begin
    t_step := t_container.StepByNr(t_step.GetFieldValue(SF_NR));
    while assigned(t_step) and (t_step <> step) do begin
      t_step := t_container.NextStep;
      if assigned(t_step) then t_stepnrs.AddObject(t_step.GetFieldValue(SF_NR), t_step);
    end;
    if (t_step = step) then begin
      if TStepContainer.CalcCaseNr(t_step.GetFieldValue(SF_NR), i_cnr) then
        b_closed := ((i_cnr = i_casenr) and (i_cnr >= 0));
    end;
  end;
end;

constructor TStepLoop.Create(const container: TStepContainer);
begin
  inherited Create(container);
  i_casenr := -1;
  b_closed := false;
end;

destructor TStepLoop.Destroy();
begin
  //todo:
  inherited Destroy();
end;

function TTestCase.GetIndexFrom(): integer;
begin
  if (assigned(t_container) and (t_stepnrs.Count > 0)) then result := t_container.IndexOfStep(t_stepnrs.Strings[0])
  else result := -1;
end;

function TTestCase.GetIndexTo(): integer;
var i_len: integer;
begin
  i_len := t_stepnrs.Count;
  if (assigned(t_container) and (i_len > 0)) then result := t_container.IndexOfStep(t_stepnrs.Strings[i_len - 1])
  else result := -1;
end;

procedure TTestCase.SetIndexFrom(const idx: integer);
var t_step: TTestStep;
begin
  if assigned(t_container) then begin
    t_step := t_container.StepByIndex(idx);
    if assigned(t_step) then begin
      Clear();
      t_stepnrs.AddObject(t_step.GetFieldValue(SF_NR), t_step);
    end;
  end;
end;

procedure TTestCase.SetIndexTo(const idx: integer);
var i, i_idx, i_idxfirst, i_idxlast: integer; t_step: TTestStep;
begin
  i_idxlast := GetIndexTo(); i_idxfirst := GetIndexFrom();
  if ((t_stepnrs.Count > 0) and assigned(t_container) and (idx > i_idxfirst)) then begin
    if (idx > i_idxlast) then begin
      for i := i_idxlast + 1 to idx do begin
        t_step := t_container.StepByIndex(i);
        if assigned(t_step) then t_stepnrs.AddObject(t_step.GetFieldValue(SF_NR), t_step);
      end;
    end else if (idx < i_idxlast) then begin

    end;

  end;
  i_cntold := Length(a_steps); 
  if ((idx <> i_idxlast) and (i_idxlast >= 0) )then begin
    i_cntnew := i_cntold + (idx - i_idxlast);
    if ((i_cntnew > 0) and assigned(t_container)) then begin
      SetLength(a_steps, i_cntnew);
      for i := i_cntold to i_cntnew - 1 do begin
        Inc(i_idxlast);
        a_steps[i] := i_idxlast;
        t_step := t_container.StepByIndex(a_steps[i]);
        if assigned(t_step) then t_stepnrs.Add(t_step.GetFieldValue(SF_NR));
      end;
    end;
  end;
end;

constructor TTestCase.Create(const container: TStepContainer);
begin
  inherited Create(container);
  i_casenr := -1;
  s_title := '';
end;

destructor TTestCase.Destroy();
begin
  inherited Destroy();
end;

function TTestSequence.GetCases(): integer;
begin
  result := length(a_cases);
end;

procedure TTestSequence.UpdateCaseSteps(const caseidx: byte);
var i, i_curcase, i_steps, i_curstep: integer; 
    t_case: TTestCase; t_step: TTestStep;
begin
  if assigned(t_container) then begin
    i_curcase := length(a_cases); 
    SetLength(a_cases, i_curcase + 1);
    a_cases[i_curcase] := caseidx;
    t_case := t_container.CaseByIndex(caseidx);
    if assigned(t_case) then begin
      t_casenrs.Add(IntToStr(t_case.CaseNr));
      i_steps := t_case.Count;
      i_curstep := length(a_steps);
      if (i_steps > 0) then begin
        SetLength(a_steps, i_curstep + i_steps);
        for i := t_case.IndexFrom to t_case.IndexTo do begin
          a_steps[i_curstep] := i;
          inc(i_curstep);
          t_step := t_container.StepByIndex(i);
          if assigned(t_step) then t_stepnrs.Add(t_step.GetFieldValue(SF_NR));
        end;
      end;
    end;
  end;
end;

constructor TTestSequence.Create(const container: TStepContainer);
begin
  inherited Create(container);
  i_curcaseidx := -1;
  t_casenrs := TStringList.Create();
  t_casenrs.Delimiter := Char(';');
end;

destructor TTestSequence.Destroy();
begin
  Clear();
  t_casenrs.Free();
  inherited Destroy();
end;

procedure TTestSequence.Update(const csset: TIndexSet);
var i, i_maxidx: integer; i_idx: byte;
begin
  if assigned(t_container) then begin
    if ((t_casenrs.Count - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
    else i_maxidx := t_container.CountCase - 1;
    
    Clear();
    for i := 0 to i_maxidx do begin
      i_idx := byte(i);
      if (i_idx in csset) then UpdateCaseSteps(i_idx);
    end;
  end;
end;

procedure TTestSequence.Clear();
begin
  inherited Clear();
  SetLength(a_cases, 0);
  t_casenrs.Clear();
end;

function  TTestSequence.CurTestStep(): integer;
var t_step: TTestStep; i_casenr: integer;
begin
  result := inherited GetCurTestStep();
  if assigned(t_container) then begin
     t_step := t_container.StepByIndex(result);
     if assigned(t_step) then begin
        if t_container.CalcCaseNr(t_step.GetFieldValue(SF_NR), i_casenr) then
          i_curcaseidx := t_casenrs.IndexOf(IntToStr(i_casenr));
     end;
  end;
end;

function  TTestSequence.FirstIndexOfCase(): integer;
begin
  i_curcaseidx := 0;
  result := CurIndexOfCase();
end;

function  TTestSequence.PrevIndexOfCase(): integer;
begin
  dec(i_curcaseidx);
  result := CurIndexOfCase()
end;

function  TTestSequence.CurIndexOfCase(): integer;
begin
  result := -1;
  if ((i_curcaseidx < 0) or (i_curcaseidx >= length(a_cases))) then i_curcaseidx := -1
  else result := a_cases[i_curcaseidx];
end;

function  TTestSequence.NextIndexOfCase(): integer;
begin
  Inc(i_curcaseidx);
  result := CurIndexOfCase()
end;

function  TTestSequence.LastIndexOfCase(): integer;
begin
  i_curcaseidx := length(a_cases) - 1;
  result := CurIndexOfCase();
end;

end.
