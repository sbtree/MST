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
uses Classes, TextMessage;

type
  //define actors, which realize the function
  EFunctionActorType = (
                    FA_TSEQ,  //test sequece
                    FA_SWT,   //software tool
                    FA_DMM,   //digital multimeter
                    FA_DSO,   //degital storage oscilloscope
                    FA_MXP,   //metronix product, which is the unit under the test (uut)
                    FA_TMM    //thermometer
                    );

  IFunctionActors = interface
    procedure SetActorObject(const efa: EFunctionActorType; const actorobj: TObject);
    function  GetActorObject(const efa: EFunctionActorType): TObject;

    property ActorObject[const efa: EFunctionActorType]: TObject read GetActorObject write SetActorObject;
  end;

  TFunctionActors = class(TInterfacedObject, IFunctionActors)
  protected
    ao_actors:   array[EFunctionActorType] of TObject;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetActorObject(const efa: EFunctionActorType; const actorobj: TObject);
    function  GetActorObject(const efa: EFunctionActorType): TObject;
    property ActorObject[const efa: EFunctionActorType]: TObject read GetActorObject write SetActorObject;
  end;

  TFunctionBase = class(TPersistent, ITextMessengerImpl)
  protected
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface ITextMessengerImpl
    b_aborted:  boolean;            //indicates if current execution should be aborted
    t_pars:     TStrings;           //to save parameters
    v_resval:   Variant;            //value of the result
    t_fntactors:TFunctionActors;    //function actors, should be given after creating
  protected
     function GetParamSeparator(): Char;
     procedure SetParamSeparator(separator: Char);
     procedure SetAborted(const aborted: boolean);
     procedure SetFunctionActors(const fntactors: TFunctionActors); virtual;

  public
    constructor Create(); virtual;
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property ResultValue: Variant read v_resval;
    //property ResultString: string read s_result write s_result;
    //property ExecutionMode: EExecMode read e_exemode write e_exemode;
    property Aborted: boolean read b_aborted write SetAborted;
    property ParamSeparator: Char read GetParamSeparator write SetParamSeparator; //separator of the parameters. The default value is a space
    property FunctionActors: TFunctionActors read t_fntactors write SetFunctionActors;

    function LoadParameter(const par: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; virtual;
    function DoTask(): boolean; virtual;
  end;
  TFunctionClass = class of TFunctionBase;

implementation
uses SysUtils, StrUtils;

constructor TFunctionActors.Create();
var i: EFunctionActorType;
begin
  inherited Create();
  for i := Low(EFunctionActorType) to High(EFunctionActorType) do ao_actors[i] := nil;
end;

destructor TFunctionActors.Destroy();
begin
  inherited Destroy();
end;

procedure TFunctionActors.SetActorObject(const efa: EFunctionActorType; const actorobj: TObject);
begin
  ao_actors[efa] := actorobj;
end;

function  TFunctionActors.GetActorObject(const efa: EFunctionActorType): TObject;
begin
  result := ao_actors[efa];
end;

constructor TFunctionBase.Create;
begin
	inherited Create();
  //e_exemode := EM_NORMAL;
  b_aborted := false;
  t_pars := TStringList.Create();
  t_pars.Delimiter := ' ';
  t_pars.StrictDelimiter := true;
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
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

procedure TFunctionBase.SetFunctionActors(const fntactors: TFunctionActors);
begin
  t_fntactors := fntactors;
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
