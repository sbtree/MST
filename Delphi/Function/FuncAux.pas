// =============================================================================
// Module name  : $RCSfile: FuncAux.pas,v $
// description  : This unit implements subclasses of TFuncitonBase, which can be
//                called as auxiliary functions in a script.
//                Auxiliary Script Function
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit FuncAux;

interface
uses Classes, FuncBase, ParseExpr;
type
  SubStr = class(TFunctionBase)
  end;

  StrToInt = class(TFunctionBase)
  end;

  IntToStr = class(TFunctionBase)
  end;

  IntToHex = class(TFunctionBase)
  end;

  HexToInt = class(TFunctionBase)
  end;

  BitsMaskOne = class(TFunctionBase)
  end;

  BitsMaskZero = class(TFunctionBase)
  end;

  TEvalExprBase = class(TFunctionBase)
  protected
    t_exprparser: TExpressionParser;
  public
    constructor Create(); override;
    destructor Destroy; override;

    function LoadParameter(const par: string): boolean; override;
    function DoTask(): boolean; override;
  end;

  EvalExprStr = class(TEvalExprBase)
  public
    function LoadParameter(const par: string): boolean; override;
  end;

  EvalExprInt = class(TEvalExprBase)
  end;

  EvalExprFloat = class(TEvalExprBase)
  end;

  EvalExprBool = class(TEvalExprBase)
  end;

implementation
uses Math, SysUtils, TextMessage, ParseClass;

constructor TEvalExprBase.Create();
begin
  inherited Create();
  t_exprparser := TExpressionParser.Create();
  t_exprparser.ClearExpressions;
end;
destructor TEvalExprBase.Destroy;
begin
  t_exprparser.Free();
end;

function TEvalExprBase.LoadParameter(const par: string): boolean;
begin
  result := false;
  try
    //todo: replace pseudo strings with their actual values, e.g. @LastInt, @LastReal, @VarInt, @VarReal
    result := (t_exprparser.Evaluate(par) <> NaN);
  except
    on E: EParserException do t_msgrimpl.AddMessage(E.Message, ML_ERROR);
    else t_msgrimpl.AddMessage(format('The given expression(%s) can not be parsed.', [par]), ML_ERROR);
  end;
end;

function TEvalExprBase.DoTask(): boolean;
begin
  result := false;
  try
    result := (t_exprparser.EvaluateCurrent() <> NaN);
  except
    on E: EParserException do t_msgrimpl.AddMessage(E.Message, ML_ERROR);
    else t_msgrimpl.AddMessage(format('Failed to evaluate the expression (%s).', [t_exprparser.Expression[0]]), ML_ERROR);
  end;
end;

function EvalExprStr.LoadParameter(const par: string): boolean;
begin
  //todo:
  //1. replace pseudo strings with their actual values in double quotation, e.g. '"0010:00030002"' for @LastStr
  //2. call g_exprparser.parse in block of try ... except ...
  //3. set returned value
  result := true
end;



initialization
  Classes.RegisterClass(SubStr);
  Classes.RegisterClass(IntToStr);
  Classes.RegisterClass(StrToInt);
  Classes.RegisterClass(IntToHex);
  Classes.RegisterClass(HexToInt);
  Classes.RegisterClass(BitsMaskOne);
  Classes.RegisterClass(BitsMaskZero);
  Classes.RegisterClass(EvalExprStr);
  Classes.RegisterClass(EvalExprInt);
  Classes.RegisterClass(EvalExprFloat);
  Classes.RegisterClass(EvalExprBool);

finalization
  Classes.UnregisterClass(SubStr);
  Classes.UnregisterClass(IntToStr);
  Classes.UnregisterClass(StrToInt);
  Classes.UnregisterClass(IntToHex);
  Classes.UnregisterClass(HexToInt);
  Classes.UnregisterClass(BitsMaskOne);
  Classes.UnregisterClass(BitsMaskZero);
  Classes.UnregisterClass(EvalExprStr);
  Classes.UnregisterClass(EvalExprInt);
  Classes.UnregisterClass(EvalExprFloat);
  Classes.UnregisterClass(EvalExprBool);
end.
