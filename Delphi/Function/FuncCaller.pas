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
uses Classes, TextMessage, FuncBase, GenType;

type
  //define class of function caller, which calls script functions
  TFunctionCaller = class(TInterfacedObject, ITextMessengerImpl)
  protected
    t_aliases:  TStrings;
    t_func:     TFunctionBase;
    e_exemode:  EExecMode;
    s_result:   string;
    t_msgrimpl: TTextMessengerImpl;
  protected
    procedure InitAliases();
    procedure SetExecutionMode(const em: EExecMode);
    function  FindFunctionByAlias(const alias: string): TFunctionClass;
    function  FindFunctionByName(const func: string): TFunctionClass;
  public
    constructor Create();
    destructor Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;

    property  ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    property  ResultString: string read s_result write s_result;
    //function FindFunction(const fnname: string): boolean;
    function  CreateFunction(const func: string): TFunctionBase; virtual;
    function  CallFunction(const func, par: string): boolean;
  end;

implementation
uses SysUtils;

procedure TFunctionCaller.InitAliases();
begin
  t_aliases.Add('FsmDownloadFW=FlashOverFDT');
  t_aliases.Add('Frage_JaNein=YesNoQuery');
  t_aliases.Add('XYZ=ExecConsoleCmd');
  //todo: extention
end;

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

function TFunctionCaller.FindFunctionByAlias(const alias: string) : TFunctionClass;
var i_ftidx: integer;
begin
  i_ftidx := t_aliases.IndexOfName(alias);
  if (i_ftidx < 0) then result := nil
  else result := FindFunctionByName(t_aliases.Values[alias]);
end;

function  TFunctionCaller.FindFunctionByName(const func: string): TFunctionClass;
begin
  if (SameText(func, 'nil') or (func = '')) then result := nil
  else result := TFunctionClass(GetClass(func));
end;

constructor TFunctionCaller.Create;
begin
  inherited Create();
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
  ExecutionMode := EM_NORMAL;
  t_aliases := TStringList.Create();
  InitAliases();
end;

destructor TFunctionCaller.Destroy();
begin
  t_aliases.Free();
  t_msgrimpl.Free();
  inherited Destroy();
end;

function TFunctionCaller.CreateFunction(const func: string) : TFunctionBase;
var t_class : TFunctionClass;
begin
  result := nil;
  t_class := FindFunctionByName(func);
  if (not assigned(t_class)) then t_class := FindFunctionByAlias(func);
  if assigned(t_class) then result := t_class.Create()
  else if ((not SameText(func, 'nil')) and (func <> '')) then
    t_msgrimpl.AddMessage(format('The script function %s is not found.', [func]), ML_ERROR);
end;

function TFunctionCaller.CallFunction(const func, par: string): boolean;
begin
  result := false;
  t_func := CreateFunction(func);
  if assigned(t_func) then begin
    ITextMessengerImpl(t_func).Messenger := t_msgrimpl.Messenger;
    t_func.ExecutionMode := e_exemode;
    result := t_func.LoadParameter(par);
    if (result) then begin
      result := t_func.DoTask();
      s_result := t_func.ResultString;
    end else t_msgrimpl.AddMessage('The called function "' + func + '" is not executed because of an error in its parameter.', ML_ERROR);
    FreeAndNil(t_func);
  end;
end;

end.
