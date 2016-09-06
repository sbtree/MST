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
  IExprParserImpl = interface
    function EvalAsFloat(const expr: string; var val: double): boolean;
    function EvalAsInteger(const expr: string; var val: integer): boolean;
    function EvalAsBoolean(const expr: string; var val: boolean): boolean;
    function EvalAsString(const expr: string; var val: string): boolean;
    function EvalAsHex(const expr: string; var val: string; const len: byte): boolean;
    function GetParserErrorMsg(): string;
    property ParserErrorMsg: string read GetParserErrorMsg;
  end;

  TExprParserImpl = class(TInterfacedObject, IExprParserImpl)
  protected
    t_eparser:TExpressionParser;
    s_expr:   string;
    s_lastmsg:string;
  public
    constructor Create();
    destructor Destroy; override;

    function EvalAsFloat(const expr: string; var val: double): boolean;
    function EvalAsInteger(const expr: string; var val: integer): boolean;
    function EvalAsBoolean(const expr: string; var val: boolean): boolean;
    function EvalAsString(const expr: string; var val: string): boolean;
    function EvalAsHex(const expr: string; var val: string; const len: byte): boolean;
    function GetParserErrorMsg(): string;
    property ParserErrorMsg: string read GetParserErrorMsg;
  end;

  CatStr = class(TFunctionBase)
  protected
    s_expr:   string;
  end;

  SubStr = class(TFunctionBase)
  protected
    s_expr:   string;
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

  TEvalExprBase = class(TFunctionBase, IExprParserImpl)
  protected
    s_expr:   string;
    t_parserimpl: TExprParserImpl;
  protected
    function Evaluate(const expr: string): boolean; virtual; abstract;
  public
    constructor Create(); override;
    destructor Destroy; override;

    property ParserService: TExprParserImpl read t_parserimpl implements IExprParserImpl;
    //function LoadParameter(const par: string): boolean; override;
    function LoadParameters(const pars: TStrings): boolean; override;
    //function DoTask(): boolean; override;
  end;

  EvalExprFloat = class(TEvalExprBase)
  protected
    d_curval: double;
  protected
    function Evaluate(const expr: string): boolean; override;
  public
    function DoTask(): boolean; override;
  end;

  EvalExprStr = class(TEvalExprBase)
  protected
    s_curval: string;
  protected
    function Evaluate(const expr: string): boolean; override;
  public
    function DoTask(): boolean; override;
  end;

  EvalExprInt = class(TEvalExprBase)
  protected
    i_curval: integer;
  protected
    function Evaluate(const expr: string): boolean; override;
  public
    function DoTask(): boolean; override;
  end;

  EvalExprBool = class(TEvalExprBase)
  protected
    b_curval: boolean;
  protected
    function Evaluate(const expr: string): boolean; override;
  public
    function DoTask(): boolean; override;
  end;

implementation
uses Math, SysUtils, TextMessage, ParseClass;

const
  CSTR_FAILED_EVAL = 'Failed to evaluate the expression(%s).';

constructor TExprParserImpl.Create();
begin
  inherited Create();
  t_eparser := TExpressionParser.Create();
  t_eparser.ClearExpressions();
end;

destructor TExprParserImpl.Destroy;
begin
  t_eparser.Free();
  inherited Destroy();
end;

function TExprParserImpl.EvalAsFloat(const expr: string; var val: double): boolean;
begin
  result := false;
  try
    val := t_eparser.Evaluate(expr);
    result := true;
  except
    on E: EParserException do s_lastmsg := E.Message;
    else s_lastmsg := format(CSTR_FAILED_EVAL, [expr]);
  end;
end;

function TExprParserImpl.EvalAsInteger(const expr: string; var val: integer): boolean;
var d_val: double;
begin
  result := EvalAsFloat(expr, d_val);
  val := Round(d_val);
end;

function TExprParserImpl.EvalAsBoolean(const expr: string; var val: boolean): boolean;
var i_expr: integer;
begin
  result := false;
  try
    i_expr := t_eparser.AddExpression(expr);
    if (i_expr >= 0) then begin
      val := t_eparser.AsBoolean[i_expr];
      result := true;
    end;
  except
    on E: EParserException do s_lastmsg := E.Message;
    else s_lastmsg := format(CSTR_FAILED_EVAL, [expr]);
  end;
end;

function TExprParserImpl.EvalAsString(const expr: string; var val: string): boolean;
var i_expr: integer;
begin
  result := false;
  try
    i_expr := t_eparser.AddExpression(expr);
    if (i_expr >= 0) then begin
      val := t_eparser.AsString[i_expr];
      result := true;
    end;
  except
    on E: EParserException do s_lastmsg := E.Message;
    else s_lastmsg := format(CSTR_FAILED_EVAL, [expr]);
  end;
end;

function TExprParserImpl.EvalAsHex(const expr: string; var val: string; const len: byte): boolean;
var i_val: integer;
begin
  result := EvalAsInteger(expr, i_val);
  if result then val := SysUtils.IntToHex(i_val, len);
end;

function TExprParserImpl.GetParserErrorMsg(): string;
begin
  result := s_lastmsg;
end;

constructor TEvalExprBase.Create();
begin
  inherited Create();
  t_parserimpl := TExprParserImpl.Create();
end;

destructor TEvalExprBase.Destroy;
begin
  t_parserimpl.Free();
  inherited Destroy();
end;

{function TEvalExprBase.LoadParameter(const par: string): boolean;
begin
  //todo: replace pseudo strings with their actual values, e.g. @LastInt, @LastReal, @VarInt, @VarReal
  s_expr := par;
  result := Evaluate(s_expr);
end;}

function TEvalExprBase.LoadParameters(const pars: TStrings): boolean;
begin
  result := false;
  if assigned(pars) then begin
    if pars.Count > 0 then begin
      //todo: replace pseudo strings with their actual values, e.g. @LastInt, @LastReal, @VarInt, @VarReal
      s_expr := pars[0];
      result := Evaluate(s_expr);
    end;
  end;
end;

function EvalExprFloat.Evaluate(const expr: string): boolean;
begin
  result := t_parserimpl.EvalAsFloat(expr, d_curval);
  if (not result) then t_msgrimpl.AddMessage(t_parserimpl.ParserErrorMsg, ML_ERROR);
end;

function EvalExprFloat.DoTask(): boolean;
begin
  result := Evaluate(s_expr);
  //todo: reuslt
  if result then t_msgrimpl.AddMessage(format('Successful to evaluate floating point expression: %s = %.5f.', [s_expr, d_curval]));
end;

function EvalExprStr.Evaluate(const expr: string): boolean;
begin
  result := t_parserimpl.EvalAsString(expr, s_curval);
  if (not result) then t_msgrimpl.AddMessage(t_parserimpl.ParserErrorMsg, ML_ERROR);
end;

function EvalExprStr.DoTask(): boolean;
begin
  result := Evaluate(s_expr);
  if result then t_msgrimpl.AddMessage(format('Successful to evaluate string expression: %s = ''%s''.', [s_expr, s_curval]));
end;

function EvalExprInt.Evaluate(const expr: string): boolean;
begin
  result := t_parserimpl.EvalAsInteger(expr, i_curval);
  if (not result) then t_msgrimpl.AddMessage(t_parserimpl.ParserErrorMsg, ML_ERROR);
end;

function EvalExprInt.DoTask(): boolean;
begin
  result := Evaluate(s_expr);
  if result then t_msgrimpl.AddMessage(format('Successful to evaluate integer expression: %s = %d.', [s_expr, i_curval]));
end;

function EvalExprBool.Evaluate(const expr: string): boolean;
begin
  result := t_parserimpl.EvalAsBoolean(expr, b_curval);
  if (not result) then t_msgrimpl.AddMessage(t_parserimpl.ParserErrorMsg, ML_ERROR);
end;

function EvalExprBool.DoTask(): boolean;
begin
  result := Evaluate(s_expr);
  if result then t_msgrimpl.AddMessage(format('Successful to evaluate boolean expression: %s = %s.', [s_expr, BoolToStr(b_curval, true)]));
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
