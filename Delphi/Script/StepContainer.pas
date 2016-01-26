unit StepContainer;

interface
uses Classes, Contnrs, ScriptTerm;

type
  TStepContainer = class
  protected
    t_steps: TObjectList; //
  protected
    function PutStep(const sstep: string): boolean;
  public
    function LoadSteps(const ssteps: TStringList): integer;
//   function TestStep(const casenr, stepnr: integer): TTestStep;
//   function TestCase(const casenr: integer): TTestStep;
//   function TestSequence(const startcase, endcase: integer): TTestSequence;
  end;
implementation

function TStepContainer.PutStep(const sstep: string): boolean;
begin
  result := false;
end;

function TStepContainer.LoadSteps(const ssteps: TStringList): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to ssteps.Count - 1 do begin
    if PutStep(ssteps[i]) then Inc(result)
    else begin
      //todo: handle error
    end;
  end;
end;

end.
