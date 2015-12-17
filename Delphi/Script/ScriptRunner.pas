unit ScriptRunner;

interface

uses Classes, ScriptReader, ScriptTerm, TextMessage;

type
// =============================================================================
//    Description  : class of script runner, executes a script
//                   Thread-Safety should be considered of this class if an 
//                   instance of this calls runs in a work thread
//    First author : 2015-12-17 /bsu/
//    History      :
// =============================================================================

  TScriptRunner = class(TTextMessager)

  public
    constructor Create();
    destructor  Destroy(); override;

    function RunStep(const tstep: TTestStep): boolean;
    function RunCase(const tcase: TTestCase): boolean;
    function RunRoutine(const troutine: TTestRoutine): boolean;

  end;

implementation

function TScriptRunner.RunStep(const tstep: TTestStep): boolean;
begin
  //todo: 1. initialize this step and execute statements of 'init' or 'r_on'
  //todo: 2. read information of 'm' and control the execution of this step
  //todo: 3. call script function to execute 'fkt' with 'par'
  //todo: 4. evaluate the resualt of calling function with 'tol'
  //todo: 5. save result of this step
  //todo: 6. finalize this step 'final' or 'r_off'
end;

function TScriptRunner.RunCase(const tcase: TTestCase): boolean;
begin
  //todo: 1. call RunStep in a loop till the last step in this case
  //todo: 2. decide to break or not if an error exists
end;

function TScriptRunner.RunRoutine(const troutine: TTestRoutine): boolean;
begin
  //todo: 1. call RunCase in a loop till the last case in this routine
  //todo: 2. decide to break or not if an error exists
end;

end.
