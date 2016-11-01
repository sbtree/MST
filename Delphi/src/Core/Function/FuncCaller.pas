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
uses Classes, TextMessage, FuncBase, GenType, StepData, StepGroup;

type
  //define class of function caller, which calls script functions
  TFunctionCaller = class(TInterfacedObject, ITextMessengerImpl, IFunctionActors)
  private
    t_curfcls:  TFunctionClass; //indicates current class instance for class of TFunctionBase ()
    t_aliases:  TStrings;       //to save alias of the function name in the format 'alias=FunctionClass'
  protected
    t_func:     TFunctionBase;
    t_msgrimpl: TTextMessengerImpl;
    t_stepres:  TStepResult;
    t_fctactors:TFunctionActors;
  protected
    procedure InitAliases();
    function  FindFunctionByAlias(const alias: string): TFunctionClass;
    function  FindFunctionByName(const func: string): TFunctionClass;

  public
    constructor Create();
    destructor Destroy(); override;

    //delegate interface ITextMessengerImpl and IFunctionActors
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property ActorService: TFunctionActors read t_fctactors implements IFunctionActors;

    //properties
    property CurStepResult: TStepResult read t_stepres write t_stepres;

    function FindFunction(const fnname: string): boolean;
    function CreateFunction(const func: string): TFunctionBase; virtual;
    function RunFunction(const func: TFunctionBase; const par: string): boolean; virtual;
    function CallFunction(const func, par: string): boolean;
  end;

implementation
uses SysUtils;

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
var i_fctidx: integer;
begin
  i_fctidx := t_aliases.IndexOfName(alias);
  if (i_fctidx < 0) then result := nil
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

  t_fctactors := TFunctionActors.Create();
end;

destructor TFunctionCaller.Destroy();
begin
  t_fctactors.Free();
  t_aliases.Free();
  t_msgrimpl.Free();
  inherited Destroy();
end;

function TFunctionCaller.FindFunction(const fnname: string): boolean;
begin
  t_curfcls := FindFunctionByName(fnname);
  if (not assigned(t_curfcls)) then t_curfcls := FindFunctionByAlias(fnname);
  result := assigned(t_curfcls);
end;

function TFunctionCaller.CreateFunction(const func: string) : TFunctionBase;
begin
  result := nil;
  if FindFunction(func) then begin
    result := t_curfcls.Create();
    if assigned(result) then begin
      ITextMessengerImpl(result).Messenger := t_msgrimpl.Messenger;
      result.FunctionActors := t_fctactors;
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
