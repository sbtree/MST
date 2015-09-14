unit FuncFlashRunner;

interface
uses Classes, FunctionBase, FlashRunner;

type
  FR_Set_DM = class(TFunctionBase)
  protected
    i_addr: integer;
    i_data: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Parameterize(const str: string): boolean; override;
    //function Initilize(const str: string): boolean; override;
    function Execute(const str: string): boolean; override;
    function Validate(const str: string): boolean; override;
    function Finalize(const str: string): boolean; override;
    function GetLastError(var msg: string): integer; override;
  end;

implementation
uses SysUtils;

const
  FE_NO_ERROR = 0;
  //error parameter
  FE_PARA_COUNT = $1001;
  FE_ADDR_INVALID = $1002;
  FE_ADDR_OVERFLOW = $1003;
  FE_DATA_INVALID = $1004;
  //error in execution
  FE_STATE_EXEC = $1010;
  FE_DEVICE_OBJ = $1011;
  FE_DEVICE_ERR = $1012;

constructor FR_Set_DM.Create;
begin
	inherited Create;
end;

destructor FR_Set_DM.Destroy;
begin
	inherited Destroy;
end;

function FR_Set_DM.Parameterize(const str: string): boolean;
var t_pars: TStringList; i_len: integer;
begin
  result := false;
  if (e_fstate in C_FUNC_STATES[FE_PARAMETERIZE]) then begin
    t_pars := TStringList.Create;
    i_len := ExtractStrings([' '], [' ', Char(9)], PChar(str),t_pars);
    if i_len = 2 then begin
      result := TryStrToInt(t_pars[0], i_addr);
      if result then begin
        result := (result and (i_addr >= 0) and (i_addr < 504)); //504 = 512 - sizeof(int)*2: word address
        if result then begin
          result := (result and (TryStrToInt(t_pars[1], i_data)));
          if result then i_lasterr := FE_NO_ERROR
          else i_lasterr := FE_DATA_INVALID;
        end else i_lasterr := FE_ADDR_OVERFLOW;
      end else i_lasterr := FE_ADDR_INVALID;
    end else i_lasterr := FE_PARA_COUNT;
    t_pars.Clear;
    FreeAndNil(t_pars);
  end;
  PostEvent(FE_PARAMETERIZE, result);
end;

{function FR_Set_DM.Initilize(const str: string): boolean;
begin
  result := (e_fstate in C_STATES[FE_INITIALIZE]);
  PostEvent(FE_INITIALIZE, result);
end;}

function FR_Set_DM.Execute(const str: string): boolean;
begin
  result := false;
  if (e_fstate in C_FUNC_STATES[FE_START]) then begin
    PostEvent(FE_START, true);
    if assigned(t_flashrunner) then begin
      result := t_flashrunner.SetDynamicMem(i_addr, i_data);
      if result then i_lasterr := FE_NO_ERROR
      else i_lasterr := FE_DEVICE_ERR;
    end else i_lasterr := FE_DEVICE_OBJ;
    PostEvent(FE_FINISH, result);
  end else i_lasterr := FE_STATE_EXEC;
end;

function FR_Set_DM.Validate(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_VALIDATE]);
  PostEvent(FE_VALIDATE, result);
end;

function FR_Set_DM.Finalize(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_FINALIZE]);
  PostEvent(FE_FINALIZE, result);
end;

function FR_Set_DM.GetLastError(var msg: string): integer;
begin
  result := i_lasterr;
  case i_lasterr of
    FE_NO_ERROR: msg := 'No error is found.';
    FE_PARA_COUNT: msg := 'The count of parameters is invalid.';
    FE_ADDR_INVALID: msg := 'The given address in the parameters is invalid.';
    FE_ADDR_OVERFLOW: msg := 'The given address in the parameters is out of the allowed range [0..503].';
    FE_DATA_INVALID: msg := 'The given data in the parameters is invalid.';
    FE_STATE_EXEC: msg := 'The function state is invalide to execute.';
    FE_DEVICE_OBJ: msg := 'The object of FlashRunner is not instanced.';
    FE_DEVICE_ERR: msg := 'An error exist in calling the device of FlashRunner.';
    else msg := 'The error is unknown.'
  end;
end;

initialization
  Classes.RegisterClass(FR_Set_DM);

finalization
  Classes.UnregisterClass(FR_Set_DM);
end.
