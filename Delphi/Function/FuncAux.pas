unit FuncAux;

interface
uses Classes, FuncBase, QExp;
type
  TEvalExprBase = class(TFunctionBase)
  protected
    t_expr: TQExprParser;
  public
    function LoadParameter(const par: string): boolean; override;
    function DoTask(): boolean; override;
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

  IntToHexStr = class(TFunctionBase)
  public
    function LoadParameter(const par: string): boolean; override;
    function DoTask(): boolean; override;
  end;
var
  g_exprparser: TQExprParser;

implementation
uses TextMessage;

function TEvalExprBase.LoadParameter(const par: string): boolean;
begin
  //todo:
  //1. replace pseudo strings with their actual values, e.g. @LastInt, @LastReal, @VarInt, @VarReal
  //2. call g_exprparser.parse in block of try ... except ...
  //3. set returned value
  try

    result := true;
  except
    AddMessage('The given expression can not be parsed.', ML_ERROR);
    result := false;
  end;
end;

function TEvalExprBase.DoTask(): boolean;
begin
  //todo:
  //1. call g_exprparser.calc in block of try ... except ...
  //2. set result string and returned value
  result := true;
end;

function EvalExprStr.LoadParameter(const par: string): boolean;
begin
  //todo:
  //1. replace pseudo strings with their actual values in double quotation, e.g. '"0010:00030002"' for @LastStr
  //2. call g_exprparser.parse in block of try ... except ...
  //3. set returned value
  result := true
end;

function IntToHexStr.LoadParameter(const par: string): boolean;
begin
  //todo:
  result := true;
end;
function IntToHexStr.DoTask(): boolean;
begin
  //todo:
  result := true;
end;


initialization
  g_exprparser := TQExprParser.Create();
  Classes.RegisterClass(EvalExprStr);
  Classes.RegisterClass(EvalExprInt);
  Classes.RegisterClass(EvalExprReal);
  Classes.RegisterClass(EvalExprBool);
  Classes.RegisterClass(IntToHexStr);

finalization
  g_exprparser.Free();
  Classes.UnregisterClass(EvalExprStr);
  Classes.UnregisterClass(EvalExprInt);
  Classes.UnregisterClass(EvalExprReal);
  Classes.UnregisterClass(EvalExprBool);
  Classes.UnregisterClass(IntToHexStr);
end.
