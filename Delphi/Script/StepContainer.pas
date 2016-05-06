unit StepContainer;

interface
uses Classes, Contnrs, IniFiles, StepDescriptor, CaseChecker;

type
  TStepContainer = class
  protected
    t_steps:  TObjectList;  //to save test steps
    t_stepnrs:TStrings;     //auxiliary variable, to save the step number matching index of t_steps;
    i_curstep:integer;      //index of step, to indicate current step, which is now selected, -1 means no selection
    t_cases:  TObjectList;  //to save test cases
//    t_nicases:THashedStringList; //to save pairs of case-nr and case index for searching
//    t_nicids: THashedStringList; //to save pairs of case-nr and case identifer for searching
  protected
  public
    constructor Create();
    destructor  Destroy(); override;

    function  CreateStep(const fields: FieldStringArray): boolean;
    function  GetStepByIndex(const idx: integer): TTestStep;
    function  GetStepByNr(const nr: string): TTestStep;
    function  PreviousStep(): TTestStep;
    function  CurrentStep(): TTestStep;
    function  NextStep(): TTestStep;
    function  StepCount(): integer;
    procedure Clear();
    procedure Assign(const source: TStepContainer);
//   function TestCase(const casenr: integer): TTestCase;
//   function TestSequence(const startcase, endcase: integer): TTestSequence;
  end;

implementation
uses SysUtils, StrUtils;

constructor TStepContainer.Create();
begin
  inherited Create();
  t_steps := TObjectList.Create();
  t_stepnrs := TStringList.Create();
  i_curstep := -1;
  t_cases := TObjectList.Create();
end;

destructor TStepContainer.Destroy();
begin
  Clear();
  t_cases.Free();
  t_stepnrs.Free();
  t_steps.Free();
  inherited Destroy();
end;

function TStepContainer.CreateStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep; i_index: integer;
begin
  t_step := TTestStep.Create();
  t_step.InputFields(fields);
  i_index := t_steps.Add(t_step);
  result := (i_index >= 0);
  if result then begin
    t_stepnrs.Add(fields[SF_NR]);
  end else FreeAndNil(t_step);
end;

function  TStepContainer.GetStepByIndex(const idx: integer): TTestStep;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then i_curstep := idx;
  result := CurrentStep();
end;

function  TStepContainer.GetStepByNr(const nr: string): TTestStep;
var i_idx: integer;
begin
  i_idx := t_stepnrs.IndexOf(nr);
  result := GetStepByIndex(i_idx);
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

function  TStepContainer.StepCount(): integer;
begin
  result := t_steps.Count;
end;

procedure TStepContainer.Clear();
begin
  t_steps.Clear();
  t_stepnrs.Clear();
  i_curstep := -1;
  t_cases.Clear();
  //t_nicases.Clear();
  //t_nicids.Clear();
end;

procedure TStepContainer.Assign(const source: TStepContainer);
var t_step: TTestStep; i: integer;
begin
  if assigned(source) then begin
    Clear();
    for i := 0 to source.StepCount() - 1 do begin
      t_step := TTestStep.Create();
      t_step.Assign(source.GetStepByIndex(i));
      t_steps.Add(t_step);
      t_stepnrs.Add(t_step.StepFields[SF_NR].InputString);
    end;
  end;
end;

end.
