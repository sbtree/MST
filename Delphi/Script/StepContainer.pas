unit StepContainer;

interface
uses Classes, Contnrs, TestStep, TestCase;

type
  TStepContainer = class
  protected
    t_steps: TObjectList; //
    t_cases: TObjectList; //
  protected

  public
    constructor Create();
    destructor  Destroy(); override;

    function  CreateStep(const fields: FieldStringArray): boolean;
    function  GetStep(const idx: integer): TTestStep;
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
var t_step: TTestStep;
begin
  t_step := TTestStep.Create();
  t_step.InputFields(fields);
  result := (t_steps.Add(t_step) >= 0);
  if (not result) then t_step.Free();
end;

function  TStepContainer.GetStep(const idx: integer): TTestStep;
begin
  result := nil;
  if ((idx >= 0) and (idx < t_steps.Count)) then result := TTestStep(t_steps.Items[idx]);
end;

procedure TStepContainer.ClearSteps();
begin
  t_steps.Clear();
end;

end.
