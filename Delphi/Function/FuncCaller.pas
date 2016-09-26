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
uses Classes, TextMessage, FuncBase, GenType, StepData, StepGroup, Multimeter, FuncDmm;

type
  //define class of function caller, which calls script functions
  TFunctionCaller = class(TInterfacedObject, ITextMessengerImpl)
  protected
    t_aliases:  TStrings;
    t_func:     TFunctionBase;
    t_msgrimpl: TTextMessengerImpl;
    t_curgroup: TStepGroup;
    t_dmm:      TMultimeter;
    t_stepres:  TStepResult;
  protected
    procedure InitAliases();
    function  FindFunctionByAlias(const alias: string): TFunctionClass;
    function  FindFunctionByName(const func: string): TFunctionClass;
  public
    constructor Create();
    destructor Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;

    //properties
    property CurStepResult: TStepResult write t_stepres;
    property CurStepGroup: TStepGroup read t_curgroup write t_curgroup;
    property DevMultimeter: TMultimeter read t_dmm write t_dmm;

    //property  ExecutionMode: EExecMode read e_exemode write SetExecutionMode;
    //property  ResultString: string read s_result write s_result;
    //function FindFunction(const fnname: string): boolean;
    function  CreateFunction(const func: string): TFunctionBase; virtual;
    function  RunFunction(const func: TFunctionBase; const par: string): boolean; virtual;
    function  CallFunction(const func, par: string): boolean;
  end;

implementation
uses SysUtils, FuncSys;

procedure TFunctionCaller.InitAliases();
begin
  t_aliases.Add('FsmDownloadFW=FlashOverFDT');
  t_aliases.Add('Frage_JaNein=YesNoQuery');
  t_aliases.Add('F_Messung=MeasureF');
  t_aliases.Add('R_Messung=MeasureR');
  t_aliases.Add('U_Mess_AC=MeasureACV');
  t_aliases.Add('U_Mess_DC=MeasureDCV');
  t_aliases.Add('I_Mess_DC=MeasureDCI');
  //todo: extention
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
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());

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
  if assigned(t_class) then begin
    result := t_class.Create();
    if assigned(result) then begin
      if (result is TConditionControl) then
        IStepControlImpl(TConditionControl(result)).CurStepGroup := t_curgroup
      else if (result is TDmmFunction) then
        TDmmFunction(result).DevMultimeter := t_dmm;

      ITextMessengerImpl(result).Messenger := t_msgrimpl.Messenger;
    end;
  end else if ((not SameText(func, 'nil')) and (func <> '')) then
    t_msgrimpl.AddMessage(format('The called function ''%s'' is not found.', [func]), ML_ERROR);
end;

function  TFunctionCaller.RunFunction(const func: TFunctionBase; const par: string): boolean;
begin
  result := false;
  if assigned(func) then begin
    result := func.LoadParameter(par);
    if (result) then begin
      result := func.DoTask();
      if (result and assigned(t_stepres)) then
        t_stepres.Value := func.ResultValue;
    end else t_msgrimpl.AddMessage(format('Failed to load parameter(''%s'') by calling %s.', [par, func.ClassName()]), ML_ERROR);
  end;
end;

function TFunctionCaller.CallFunction(const func, par: string): boolean;
begin
  if (SameText(func, 'nil') or (func = '')) then begin
    result := true;
    t_msgrimpl.AddMessage(format('No function is called with the name ''%s''.', [func]));
  end else begin
    result := false;
    t_func := CreateFunction(func);
    if assigned(t_func) then begin
      result := RunFunction(t_func, par);
      FreeAndNil(t_func);
    end;
  end;
end;

end.
