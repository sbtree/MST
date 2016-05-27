unit StepResult;

interface
type
  EStepState = (
                SS_UNKNOWN, //state, before execution or unknown error
                SS_INIT,    //state in initializing
                SS_FCT,     //state in running call-function
                SS_TOL,     //state in validation of the value
                SS_FINAL,   //state in finalization
                SS_OK       //state for ok if the step is succesfully executed completely
                );

  TStepResult = class
  protected
    b_state:  EStepState;
    v_result: Variant;
  protected

  public
    constructor Create();
    destructor Destroy(); override;

    property Value: Variant read v_result write v_result;
    property State: EStepState read b_state write b_state;

    function GetString(var val: string): boolean;
    function GetInteger(var val: integer): boolean;
    function GetInt64(var val: int64): boolean;
    function GetReal(var val: double): boolean;
    function GetBoolean(var val: boolean): boolean;
    function GetHexStr(var val: string; const len: integer): boolean;
    procedure Clear();
    procedure Assign(const source: TStepResult);
  end;

implementation
uses SysUtils, Variants, StrUtils;

constructor TStepResult.Create();
begin
  inherited Create();
  b_state := SS_UNKNOWN;
end;

destructor TStepResult.Destroy();
begin
  inherited Destroy();
end;

function TStepResult.GetString(var val: string): boolean;
begin
  result := ((not VarIsNull(v_result)) and (not VarIsEmpty(v_result)));
  if not result then val := v_result;
end;

function TStepResult.GetInteger(var val: integer): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetInt64(var val: int64): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetReal(var val: double): boolean;
var s_real: string;
begin
  if (VarType(v_result) = varString) then begin  //if the local decimal separator is not '.'
    s_real := v_result;
    s_real := ReplaceStr(s_real, '.', DecimalSeparator);
    result := TryStrToFloat(s_real, val);
  end else begin
    try
      val := v_result;
      result := true;
    except
      result := false;
    end;
  end;
end;

function TStepResult.GetBoolean(var val: boolean): boolean;
begin
  try
    val := v_result;
    result := true;
  except
    result := false;
  end;
end;

function TStepResult.GetHexStr(var val: string; const len: integer): boolean;
var i_val64: int64;
begin
  result := GetInt64(i_val64);
  if result then val := IntToHex(i_val64, len);
end;

procedure TStepResult.Clear();
begin
  b_state := SS_UNKNOWN;
  VarClear(v_result);
end;

procedure TStepResult.Assign(const source: TStepResult);
begin
  if assigned(source) then begin
    b_state := source.State;
    VarCopy(v_result, source.Value);
  end;
end;

end.
