unit StepContainer;

interface
uses Classes, Contnrs, ScriptTerm;

type
  TStepContainer = class
  protected
    t_steps: TObjectList; //
  protected

  public
    constructor Create();
    destructor  Destroy(); override;

    function  CreateStep(const fields: FieldStringArray): boolean;
    procedure ClearSteps();
//   function LoadSteps(const ssteps: TStringList): integer;
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

procedure TStepContainer.ClearSteps();
begin
  t_steps.Clear();
end;

end.
