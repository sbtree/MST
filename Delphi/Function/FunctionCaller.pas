unit FunctionCaller;

interface
uses Classes,TextMessage, FuncBase, GenType;
type
  //define prototype of script functions
  //ScriptFunction = function(const par: string): boolean of object;

  //define class of function caller, which calls script functions
  TFunctionCaller = class
  protected
    t_func:       TFunctionBase;
    e_exemode:    EExecMode;
    s_result:     string;
    t_messenger:  TTextMessenger;
  protected
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure SetExecutionMode(const em: EExecMode);
    function  FindFunction(const func: string): TFunctionBase; virtual;
  public
    constructor Create();
    destructor Destroy(); override;
    
    property  ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property  Messenger: TTextMessenger read t_messenger write t_messenger;
    property  ResultString: string read s_result write s_result;
    function  CallFunction(const func, par: string): boolean;
  end;

implementation
uses SysUtils;

procedure TFunctionCaller.AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
begin
  //todo:
end;

procedure TFunctionCaller.SetExecutionMode(const em: EExecMode);
begin
  e_exemode := em;
  if assigned(t_messenger) then begin
    case e_exemode of
      EM_NORMAL, EM_SIMULATE: t_messenger.MessageThreshold := ML_ERROR;
      EM_DIAGNOSE: t_messenger.MessageThreshold := ML_INFO;
    end;
  end;
end;

function TFunctionCaller.FindFunction(const func: string) : TFunctionBase;
var
  t_class : TFunctionClass;
begin
  result := Nil;
  t_class := TFunctionClass(GetClass(func));
  if (t_class <> nil) then result := t_class.Create();
end;

constructor TFunctionCaller.Create;
begin
  inherited Create();
  ExecutionMode := EM_NORMAL;
end;

destructor TFunctionCaller.Destroy();
begin
end;

function TFunctionCaller.CallFunction(const func, par: string): boolean;
begin
  result := false;
  if (func = '') then AddMessage('The called function "' + func + '" is not available.', ML_ERROR)
  else if SameText(func, 'nil') then AddMessage('Nothing is done for calling ' + func)
  else begin
    t_func := FindFunction(func);
    if assigned(t_func) then
    begin
      t_func.Messenger := t_messenger;
      t_func.ExecutionMode := e_exemode;
      result := t_func.LoadParameter(par);
      if (result) then begin
        result := t_func.DoTask();
        s_result := t_func.ResultString;
      end else AddMessage('The called function "' + func + '" is not executed because of an error in its parameter.', ML_ERROR);
      FreeAndNil(t_func);
    end else AddMessage('The called function "' + func + '" is not available.', ML_ERROR);
  end;
end;

end.
