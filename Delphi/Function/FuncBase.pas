// =============================================================================
// Module name  : $RCSfile: FuncBase.pas,v $
// description  : This unit implements basis methodes and properties of a class
//                for script function. A script function is a function, which is
//                called in a script of a test sequence. It is defined as a class
//                in the source code and it muss be a subclass of this base class.
//                NOTE: all subclasses have to be registered into Delphi-classes
//                system, so that they are automatically recognized by function
//                caller (see unit FuncCaller). Every script function will have
//                the same name as your subclass. 
//                In order to register your subclass, Classes.RegisterClass(YourSubclass)
//                has to be called in section initialization of its unit and
//                unregistered in finalization. See the end of this unit in comment
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit FuncBase;

interface
uses Classes, TextMessage, GenType, DeviceBase, ToolBase;

type
  TFunctionBase = class(TPersistent, ITextMessengerImpl)
  protected
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface ITextMessengerImpl
    e_exemode:  EExecMode;      //execution mode, which can be changed through property ExecutionMode
    b_aborted:  boolean;        //indicates if current execution should be aborted
    t_pars:     TStrings;       //to save parameters
    s_result:   string;         //a string to save result
  protected
     function GetParamSeparator(): Char;
     procedure SetParamSeparator(separator: Char);
     procedure SetAborted(const aborted: boolean);

  public
    constructor Create(); virtual;
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property ResultString: string read s_result write s_result;
    property ExecutionMode: EExecMode read e_exemode write e_exemode;
    property Aborted: boolean read b_aborted write SetAborted;
    property ParamSeparator: Char read GetParamSeparator write SetParamSeparator; //separator of the parameters. The default value is a space

    function LoadParameter(const par: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; virtual;
    function DoTask(): boolean; virtual;
  end;
  TFunctionClass = class of TFunctionBase;

implementation
uses SysUtils, StrUtils;

constructor TFunctionBase.Create;
begin
	inherited Create();
  e_exemode := EM_NORMAL;
  b_aborted := false;
  t_pars := TStringList.Create();
  t_pars.Delimiter := ' ';
  t_pars.StrictDelimiter := true;
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
end;

destructor TFunctionBase.Destroy;
begin
	inherited Destroy;
  t_pars.Free();
  t_msgrimpl.Free();
end;

function TFunctionBase.GetParamSeparator(): Char;
begin
  result := t_pars.Delimiter;
end;

procedure TFunctionBase.SetParamSeparator(separator: Char);
begin
  t_pars.Delimiter := separator;
end;

procedure TFunctionBase.SetAborted(const aborted: boolean);
begin
  b_aborted := aborted;
end;

function TFunctionBase.LoadParameter(const par: string): boolean;
var s_expr, s_rest: string; i_counter, i_pos: integer;
begin
  t_pars.Clear();
  if par <> '' then begin
    //expression with brackets have to be treated as the first element in the parameter string
    s_expr := '';
    i_pos := Pos('(', par);
    if (i_pos > 0) then i_counter := 1
    else i_counter := 0;

    while ((i_pos <= length(par)) and (i_counter > 0)) do begin
      inc(i_pos);
      if par[i_pos] = '(' then inc(i_counter)
      else if par[i_pos] = ')' then Dec(i_counter);
    end;

    if (i_counter = 0) then begin
      s_expr := LeftStr(par, i_pos);
      s_rest := trim(RightStr(par, length(par) - i_pos));
    end else s_rest := par;

    t_pars.DelimitedText := s_rest;
    if (s_expr <> '') then
      t_pars.Insert(0, s_expr);
  end;
  result := LoadParameters(t_pars);
end;

function TFunctionBase.LoadParameters(const pars: TStrings): boolean;
var i: integer;
begin
  //todo: in subclass should check
  // 1. if the count of the parmeters is valid
  // 2. if each parmeter is valid
  result := true; //set return value is true if no parameter is required
  for i := 0 to pars.Count - 1 do begin
    //todo: check if the parameter is valid
    //result := valid(pars[i]);
    if (not result) then break;
  end;
end;

function TFunctionBase.DoTask(): boolean;
begin
  //AddMessage('"Execute" is not specified and its basic function is called.');
  b_aborted := false;
  result := true;
end;

//initialization
//  Classes.RegisterClass(YourSubclass);

//finalization
//  Classes.UnregisterClass(YourSubclass);

end.
