// =============================================================================
// Module name  : $RCSfile: FuncCaller.pas,v $
// description  : This unit implements the interface to call script functions.
//
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit FuncCaller;

interface
uses Classes,TextMessage, FuncBase, GenType;
type
  //define prototype of script functions
  //ScriptFunction = function(const par: string): boolean of object;

  //define class of function caller, which calls script functions
  TFunctionCaller = class(TInterfacedObject, ITextMessengerImpl)
  protected
    t_func:       TFunctionBase;
    e_exemode:    EExecMode;
    s_result:     string;
    t_msgrimpl:   TTextMessengerImpl;
  protected
    //procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure SetExecutionMode(const em: EExecMode);
    function  FindFunction(const func: string): TFunctionBase; virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;

    property  ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property  ResultString: string read s_result write s_result;
    function  CallFunction(const func, par: string): boolean;
  end;

implementation
uses SysUtils;

procedure TFunctionCaller.SetExecutionMode(const em: EExecMode);
begin
  e_exemode := em;
  if assigned(t_msgrimpl.Messenger) then begin
    case e_exemode of
      EM_NORMAL, EM_SIMULATE: t_msgrimpl.Messenger.MessageThreshold := ML_ERROR;
      EM_DIAGNOSE: t_msgrimpl.Messenger.MessageThreshold := ML_INFO;
    end;
  end;
end;

function TFunctionCaller.FindFunction(const func: string) : TFunctionBase;
var
  t_class : TFunctionClass;
begin
  result := nil;
  if func <> '' then begin
    t_class := TFunctionClass(GetClass(func));
    if (t_class <> nil) then result := t_class.Create();
  end;
end;

constructor TFunctionCaller.Create;
begin
  inherited Create();
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
  ExecutionMode := EM_NORMAL;
end;

destructor TFunctionCaller.Destroy();
begin
  t_msgrimpl.Free();
  inherited Destroy();
end;

function TFunctionCaller.CallFunction(const func, par: string): boolean;
begin
  result := false;
  if (SameText(func, 'nil') or (func = '')) then t_msgrimpl.AddMessage('No function is called.')
  else begin
    t_func := FindFunction(func);
    if assigned(t_func) then
    begin
      ITextMessengerImpl(t_func).Messenger := t_msgrimpl.Messenger;
      t_func.ExecutionMode := e_exemode;
      result := t_func.LoadParameter(par);
      if (result) then begin
        result := t_func.DoTask();
        s_result := t_func.ResultString;
      end else t_msgrimpl.AddMessage('The called function "' + func + '" is not executed because of an error in its parameter.', ML_ERROR);
      FreeAndNil(t_func);
    end else t_msgrimpl.AddMessage('The called function "' + func + '" is not available.', ML_ERROR);
  end;
end;

end.
