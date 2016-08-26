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
uses Classes, TextMessage, GenType;

type
  TFunctionBase = class(TPersistent, ITextMessengerImpl)
  protected
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface ITextMessengerImpl
    e_exemode:  EExecMode;      //execution mode, which can be changed through property ExecutionMode
    b_aborted:  boolean;        //indicates if current execution should be aborted
    t_pars:     TStrings;       //tos save parameters
    s_result:   string;         //a string to save result
    ch_separator: char;         //separator of the parameters. The default value is a space
  protected
     procedure SetAborted(const aborted: boolean);

  public
    constructor Create();
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property ResultString: string read s_result write s_result;
    property ExecutionMode: EExecMode read e_exemode write e_exemode;
    property Aborted: boolean read b_aborted write SetAborted;
    property ParamSeparator: Char read ch_separator write ch_separator;

    function LoadParameter(const par: string): boolean; virtual;
    function LoadParameters(const pars: TStrings): boolean; virtual;
    function DoTask(): boolean; virtual;
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
  ch_separator := ' '; //default separator
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

function TFunctionBase.LoadParameter(const par: string): boolean;
begin
  ExtractStrings([ch_separator], [' ', #9], PChar(par), t_pars);
  result := LoadParameters(t_pars);
end;

function TFunctionBase.LoadParameters(const pars: TStrings): boolean;
var i: integer;
begin
  result := false;
  //todo: check if all parmeters are valid
  //result := true; //set return value is true if no parameter is required
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
