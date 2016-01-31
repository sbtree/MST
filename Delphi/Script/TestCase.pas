unit TestCase;

interface

type
  TTestCase = class
  protected
    i_casenr:   integer;
    i_firstidx:integer;
    i_lastidx: integer;

    //list of step
  public
    //constructor Create();
    //destructor  Destroy(); override;
  end;

implementation
uses StepContainer;

end.
