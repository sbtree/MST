unit StepContainer;

interface
uses Classes, Contnrs, IniFiles, StepDescriptor;

type
  TStepGroup = class;
  TIndexSet = set of byte;
  TStepContainer = class
  protected
    t_steps:    TObjectList;  //to save test steps
    t_stepnrs:  TStrings;     //auxiliary variable, to save the step number matching index of t_steps;
    i_curstep:  integer;      //index of step, to indicate current step, which is now selected, -1 means no selection
    t_cases:    TObjectList;  //to save test cases
    t_casenrs:  TStrings;     //auxiliary variable, to save the number of test case for easily searching, mathching index of t_cases
    i_lastcnr:  integer;      //to save last case number
  protected
    procedure UpdateTestCase(const stepnr, title: string);
    function  IndexSet(const casenrs: string): TIndexSet;
    function  GetTestSteps(): integer;
    function  GetTestCases(): integer;

  public
    constructor Create();
    destructor  Destroy(); override;

    property  CountStep: integer read GetTestSteps;
    property  CountCase: integer read GetTestCases;

    function  AddTestStep(const fields: FieldStringArray): boolean;
    function  TestStepByIndex(const idx: integer): TTestStep;
    function  TestStepByNr(const nr: string): TTestStep;
    function  PreviousStep(): TTestStep;
    function  CurrentStep(): TTestStep;
    function  NextStep(): TTestStep;
    function  TestCaseByIndex(const caseidx: integer): TStepGroup;
    function  TestCaseByNr(const casenr: string): TStepGroup;
    function  TestSequence(const incl: string; const excl: string = ''): string;
    procedure Clear();
    procedure SaveFile(const sfile: string);
    procedure Assign(const source: TStepContainer);
  end;

  TStepGroup = class
  protected
    i_indexfrom:  integer; //index of the first step of this group in the container
    i_indexto:    integer; //index of the last step of this group in the container
    i_groupnr:    integer; //number of the group, e.g. test case number
    s_title:      string;  //title of this group
    t_container:  TStepContainer; //step container
  public
    constructor Create();
    destructor Destroy(); override;

    property StepContainer: TStepContainer read t_container write t_container;
    property IndexFrom: integer read i_indexfrom write i_indexfrom;
    property IndexTo: integer read i_indexto write i_indexto;
    property GroupNr: integer read i_groupnr write i_groupnr;
    property GroupTitle: string read s_title write s_title;
  end;

const
  CINT_CASES_MAX : integer = 255;

implementation
uses SysUtils, StrUtils;

procedure  TStepContainer.UpdateTestCase(const stepnr, title:string);
var t_tcase: TStepGroup; s_casenr: string; i_casenr, i_curstepidx: integer;
    t_nrs: TStrings;
begin
  if (trim(stepnr) <> '') then begin
    t_nrs := TStringList.Create();
    if (ExtractStrings(['.'], [' '], PChar(stepnr), t_nrs) > 0) then begin
      s_casenr := t_nrs[0];
      if TryStrToInt(s_casenr, i_casenr) then begin
        i_curstepidx := t_steps.Count - 1;
        i_casenr := abs(i_casenr);
        if (i_casenr = i_lastcnr) then begin //update indexto of last case if the case number is same as last one
          if (t_cases.Count > 0) then begin
            t_tcase := TStepGroup(t_cases.Last);
            if assigned(t_tcase) then t_tcase.IndexTo := i_curstepidx;
          end;
        end else begin  //create a new case if the case number differs from last one
          t_tcase := TStepGroup.Create();
          t_tcase.StepContainer := self;
          if (t_cases.Add(t_tcase) >= 0) then begin
            t_tcase.GroupNr := i_casenr;
            t_tcase.GroupTitle := title;
            t_tcase.IndexFrom := i_curstepidx;
            t_tcase.IndexTo := i_curstepidx;
            t_casenrs.Add(IntToStr(i_casenr));
          end else FreeAndNil(t_tcase);
          i_lastcnr := i_casenr;
        end;
      end;
    end;
    FreeAndNil(t_nrs);
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
function  TStepContainer.IndexSet(const casenrs: string): TIndexSet;
var i, i_maxidx: integer; t_cstrs, t_cnrs: TStrings;
    i_cnr, i_cnr1, i_cnr2, i_idx: integer; s_cnr: string;
begin
  result := [];
  if ((t_casenrs.Count - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
  else i_maxidx := t_casenrs.Count - 1;

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
                i_idx := t_casenrs.IndexOf(s_cnr);
                if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
              end;
            end;
          end else begin
            if TryStrToInt(t_cstrs[i], i_cnr) then begin
              s_cnr := IntToStr(i_cnr);
              i_idx := t_casenrs.IndexOf(s_cnr);
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

function  TStepContainer.GetTestSteps(): integer;
begin
  result := t_steps.Count;
end;

function  TStepContainer.GetTestCases(): integer;
begin
  result := t_cases.Count;
end;

constructor TStepContainer.Create();
begin
  inherited Create();
  i_curstep := -1;
  t_steps := TObjectList.Create();
  t_stepnrs := TStringList.Create();
  t_cases := TObjectList.Create();
  t_casenrs := TStringList.Create();
end;

destructor TStepContainer.Destroy();
begin
  Clear();
  t_steps.Free();
  t_stepnrs.Free();
  t_cases.Free();
  t_casenrs.Free();
  inherited Destroy();
end;

function TStepContainer.AddTestStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep;
begin
  t_step := TTestStep.Create();
  t_step.InputFields(fields);
  if (t_steps.Add(t_step) >= 0) then begin
    result := true;
    t_stepnrs.Add(fields[SF_NR]);  //save step number into the list for searching
    UpdateTestCase(fields[SF_NR], fields[SF_T]);
  end else begin
    result := false;
    FreeAndNil(t_step);
  end;
end;

function  TStepContainer.TestStepByIndex(const idx: integer): TTestStep;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then i_curstep := idx
  else i_curstep := -1;
  result := CurrentStep();
end;

function  TStepContainer.TestStepByNr(const nr: string): TTestStep;
var i_idx: integer;
begin
  i_idx := t_stepnrs.IndexOf(nr);
  result := TestStepByIndex(i_idx);
end;

function  TStepContainer.PreviousStep(): TTestStep;
begin
  if (i_curstep >= 0) then dec(i_curstep);
  result := CurrentStep();
end;

function  TStepContainer.CurrentStep(): TTestStep;
begin
  if ((i_curstep >= 0) and (i_curstep < t_steps.Count)) then result := TTestStep(t_steps.Items[i_curstep])
  else result := Nil;
end;

function  TStepContainer.NextStep(): TTestStep;
begin
  if (i_curstep < t_steps.Count) then inc(i_curstep);
  result := CurrentStep();
end;

function  TStepContainer.TestCaseByIndex(const caseidx: integer): TStepGroup;
begin
  result := nil;
  if ((caseidx >= 0) and (caseidx < t_cases.Count)) then result := TStepGroup(t_cases.Items[caseidx]);
end;

function  TStepContainer.TestCaseByNr(const casenr: string): TStepGroup;
var i_caseidx, i_casenr: integer;
begin
  if TryStrToInt(casenr, i_casenr) then  i_caseidx := t_casenrs.IndexOf(IntToStr(i_casenr))
  else i_caseidx := -1;
  result := TestCaseByIndex(i_caseidx);
end;

// =============================================================================
//    Description  : analyze the given strings incl and excl and return back a
//                   list of valid case numbers
//    Parameter    : incl, a string to represent inclusive test cases
//                   excl, a string to represent exclusive test cases
//                   see function IndexSet
//    Return       : a list of test case numbers, which is a subset of test cases
//                   in the container, subset(incl) - subset(excl)
//    First author : 2016-05-09 /bsu/
//                   WARNING: because of using set in this function the count of
//                   test cases is not allowed beyond CINT_CASES_MAX + 1 (256).
//                   The cases whose indexes (from 0 to 255) is greater than
//                   CINT_CASES_MAX (255), are not considered.
//    History      :
// =============================================================================
function  TStepContainer.TestSequence(const incl: string; const excl: string): string;
var set_incl, set_excl, set_result: TIndexSet; i, i_maxidx: integer; i_idx: byte;
begin
  set_incl := IndexSet(incl);
  set_excl := IndexSet(excl);
  set_result := set_incl - set_excl;
  if ((t_casenrs.Count - 1) > CINT_CASES_MAX) then i_maxidx := CINT_CASES_MAX
  else i_maxidx := t_casenrs.Count - 1;
  for i := 0 to i_maxidx do begin
    i_idx := byte(i);
    if (i_idx in set_result) then result := result + t_casenrs[i] + ',';
  end;
  if EndsText(',',result) then result := LeftStr(result, length(result) - 1);
end;

procedure TStepContainer.Clear();
begin
  i_curstep := -1;
  i_lastcnr := -1;
  t_steps.Clear();
  t_stepnrs.Clear();
  t_cases.Clear();
  t_casenrs.Clear();
end;


// =============================================================================
//    Description  : save field values of test steps into a file
//    Parameter    : sfile, file name to save
//    Return       : --
//    First author : 2016-05-09 /bsu/
//    History      :
// =============================================================================
procedure TStepContainer.SaveFile(const sfile: string);
var i: integer; s_line: string; j:EStepField;
    t_stepvals: TStringList; t_step: TTestStep; t_field: TStepField;
begin
  t_stepvals := TStringList.Create();
  for i := 0 to t_steps.Count - 1 do begin
    t_step := TTestStep(t_steps.Items[i]);
    t_field := t_step.GetField(SF_NR);
    if assigned(t_field) then s_line := t_field.InputString;
    for j := SF_T to High(EStepField) do begin
      t_field := t_step.GetField(j);
      if assigned(t_field) then s_line := s_line + ';' + #9 + t_field.InputString;
    end;
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
      t_step.Assign(source.TestStepByIndex(i));
      t_steps.Add(t_step);
      t_stepnrs.Add(t_step.GetFieldValue(SF_NR));
    end;
  end;
end;

constructor TStepGroup.Create();
begin
  inherited Create();
  i_indexfrom := -1;
  i_indexto := -1;
end;

destructor TStepGroup.Destroy();
begin
  inherited Destroy();
end;

end.
