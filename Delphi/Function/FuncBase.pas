// =============================================================================
// Module name  : $RCSfile: FunctionBase.pas,v $
// description  : This unit implements basis methodes and properties of a class
//                for script function. A script function is a function, which is
//                called in a script of a test sequence. It is defined as a class
//                in the source code and it muss be a subclass of this base class.
//                NOTE: all subclasses have to be registered into Delphi-classes
//                system, so that they are automatically recognized by function
//                caller (see unit FunctionCaller). Every script function will have
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
uses Classes, TextMessage, GenType;

type
  TFunctionBase = class(TPersistent)
  protected
    t_messenger:TTextMessenger; //a pointer, which can be asssigned through property Messager
    e_exemode:  EExecMode;      //execution mode, which can be changed through property ExecutionMode
    b_aborted:  boolean;        //indicates if current execution should be aborted
    //s_par:      string;         //saves parameter string of this function object
    t_pars:     TStrings;       //tos save parameters
    s_result:   string;         //a string to save result
  protected
     procedure SetAborted(const aborted: boolean);
     procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
  public
    constructor Create();
    destructor Destroy; override;

    property ResultString: string read s_result write s_result;
    property ExecutionMode: EExecMode read e_exemode write e_exemode;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property Aborted: boolean read b_aborted write SetAborted;

    function LoadParameter(const par: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; virtual;
    function Execute(): boolean; virtual;
  end;
  TFunctionClass = class of TFunctionBase;

implementation
uses SysUtils;

constructor TFunctionBase.Create;
begin
	inherited Create;
  e_exemode := EM_NORMAL;
  b_aborted := false;
  t_pars := TStringList.Create();
end;

destructor TFunctionBase.Destroy;
begin
	inherited Destroy;
  t_pars.Free();
end;

procedure TFunctionBase.SetAborted(const aborted: boolean);
begin
  b_aborted := aborted;
end;

procedure TFunctionBase.AddMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then t_messenger.AddMessage(text, '', level);
end;

function TFunctionBase.LoadParameter(const par: string): boolean;
begin
  //AddMessage('"LoadParameter" is not specified and its basic function is called.');
  t_pars.Clear();
  result := (ExtractStrings([' '], [' ', #9], PChar(par), t_pars) > 0);
end;

function TFunctionBase.LoadParameters(const pars: TStrings): boolean;
var i: integer;
begin
  //AddMessage('"LoadParameter" is not specified and its basic function is called.');
  t_pars.Clear();
  for i := 0 to pars.Count - 1 do t_pars.Insert(i, pars.Strings[i]);
  result := true;
end;

function TFunctionBase.Execute(): boolean;
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
