unit FunctionCaller;

interface
type
  ScriptFunction = function(const par: string): boolean of object;
  TFunctionCaller = class
  protected
    function TestFunc(const par: string): boolean;
  public
    function CallFunction(const func, par: string): boolean;
  end;

implementation
function TFunctionCaller.CallFunction(const func, par: string): boolean;
var pfunc: ScriptFunction;
begin
  result := false;
  if self.MethodAddress(func) <>Nil then begin
    TMethod(pfunc).Code := self.MethodAddress(func);
    result := pfunc(par);
  end;
end;

end.
