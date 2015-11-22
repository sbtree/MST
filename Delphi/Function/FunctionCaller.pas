unit FunctionCaller;

interface
uses TextMessage;
type
  ScriptFunction = function(const par: string): boolean of object;
  TFunctionCaller = class(TTextMessager)
  protected
    t_method: TMethod;
  public
    constructor Create;
    function CallFunction(const func, par: string): boolean;

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
  end else AddMessage('function ''' + func + ''' is not found', ML_ERROR);
end;

function TFunctionCaller.TestFunc(const par: string): boolean;
begin
  result := false;
end;
end.
