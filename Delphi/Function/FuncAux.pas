unit FuncAux;

interface
uses Classes, FuncBase, QExp;
type
  TEvalExprBase = class(TFunctionBase)
  protected
    t_expr: TQExprParser;
  public
    function LoadParameter(const par: string): boolean; override;
    function Execute(): boolean; override;
  end;

  EvalExprStr = class(TEvalExprBase)
  public
    function LoadParameter(const par: string): boolean; override;
  end;

  EvalExprInt = class(TEvalExprBase)
  end;

  EvalExprReal = class(TEvalExprBase)
  end;

  EvalExprBool = class(TEvalExprBase)
  end;
var
  g_expr: TQExprParser;

implementation
function TEvalExprBase.LoadParameter(const par: string): boolean;
begin
  //todo:
  //1. replace pseudo strings with their actual values, e.g. @LastInt, @LastReal, @VarInt, @VarReal
  //2. call g_expr.parse in block of try ... catch ...
  //3. set returned value
  result := true;
end;

function TEvalExprBase.Execute(): boolean;
begin
  //todo:
  //1. call g_expr.calc in block of try ... catch ...
  //2. set result string and returned value
  result := true;
end;

function EvalExprStr.LoadParameter(const par: string): boolean;
begin
  //todo:
  //1. replace pseudo strings with their actual values in double quotation, e.g. '"0010:00030002"' for @LastStr
  //2. call g_expr.parse in block of try ... catch ...
  //3. set returned value
  result := true
end;

initialization
  g_expr := TQExprParser.Create();
finalization
  g_expr.Free();
end.
