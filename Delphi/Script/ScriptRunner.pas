unit ScriptRunner;

interface

uses Classes, ScriptReader, ScriptTerm;

type
  TScriptRunner = class

  public
    function RunStep(const tstep: TTestStep): boolean;
    function RunCase(const tcase: TTestCase): boolean;
    function RunRoutine(const troutine: TTestRoutine): boolean;

  end;

implementation

end.
