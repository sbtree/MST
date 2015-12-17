unit FunctionCaller;

interface
uses TextMessage;
type
  //define prototype of script functions
  ScriptFunction = function(const par: string): boolean of object;

  //define options for calling script function
  ECallOption = (
                CO_NORMAL,
                CO_DIAGNOSE,
                CO_SYNTAX,
                CO_SIMULATE
                );

  //define class of function caller, which calls script functions
  TFunctionCaller = class(TTextMessager)
  protected
    t_method: TMethod;
  public
    constructor Create;
    function CallFunction(const func, par: string): boolean;

  //all script functions have to be written as publisched function
  published
    function TestFunc(const par: string): boolean;
  end;

implementation

constructor TFunctionCaller.Create;
begin
  inherited Create();
  t_method.Data := Pointer(self);
end;

function TFunctionCaller.CallFunction(const func, par: string): boolean;
begin
  result := false;
  if (self.MethodAddress(func) <> Nil) then begin
    t_method.Code := self.MethodAddress(func);
    result := ScriptFunction(t_method)(par);
  end else AddMessage('The called function "' + func + '" is not available.', ML_ERROR);
end;

function TFunctionCaller.TestFunc(const par: string): boolean;
begin
  result := false;
end;
end.
