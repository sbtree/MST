unit TestCase;

interface

type
  TTestCase = class
  protected
    i_casenr: integer;
    a_stepidx: array of integer;

    //list of step
  public
    //constructor Create();
    //destructor  Destroy(); override;
  end;

implementation
uses StepContainer;

end.
