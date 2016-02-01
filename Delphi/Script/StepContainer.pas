unit StepContainer;

interface
uses Classes, Contnrs, IniFiles, TestStep, TestCase;

type
  TCaseDescriptor = class
  
  end;

  TStepContainer = class
  protected
    t_steps:  TObjectList;  //to save test steps
    t_cases:  TObjectList;   //to save test cases
//    t_nisteps:THashedStringList; //to save pairs of step-nr and step index for searching
//    t_nicases:THashedStringList; //to save pairs of case-nr and case index for searching
//    t_nicids: THashedStringList; //to save pairs of case-nr and case identifer for searching
  protected

  public
    constructor Create();
    destructor  Destroy(); override;

    function  CreateStep(const fields: FieldStringArray): boolean;
    function  GetStep(const idx: integer): TTestStep;
    function  StepCount(): integer;
    procedure ClearSteps();
//   function TestStep(const casenr, stepnr: integer): TTestStep;
//   function TestCase(const casenr: integer): TTestStep;
//   function TestSequence(const startcase, endcase: integer): TTestSequence;
  end;
implementation
uses SysUtils;

constructor TStepContainer.Create();
begin
  inherited Create();
  t_steps := TObjectList.Create();
end;

destructor TStepContainer.Destroy();
begin
  ClearSteps();
  t_steps.Free();
  inherited Destroy();
end;

function TStepContainer.CreateStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep; i_index: integer; s_keyval: string;
begin
  t_step := TTestStep.Create();
  t_step.InputFields(fields);
  i_index := t_steps.Add(t_step);
  result := (i_index >= 0);
  if result then begin
    s_keyval := format('%s=%d', [fields[SF_NR], i_index]);
    //t_nisteps.Add()
  end else t_step.Free();
end;

function  TStepContainer.GetStep(const idx: integer): TTestStep;
begin
  result := nil;
  if ((idx >= 0) and (idx < t_steps.Count)) then result := TTestStep(t_steps.Items[idx]);
end;

function  TStepContainer.StepCount(): integer;
begin
  result := t_steps.Count;
end;

procedure TStepContainer.ClearSteps();
begin
  t_steps.Clear();
  t_cases.Clear();
  //t_nisteps.Clear();
  //t_nicases.Clear();
  //t_nicids.Clear();
end;

end.
