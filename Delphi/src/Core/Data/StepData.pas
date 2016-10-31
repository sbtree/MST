// =============================================================================
// Module name  : $RCSfile: StepData.pas,v $
// Description  : This unit defines test step (TTestStep) and its corresponding
//                elements, e.g, TStepResult, TStepField etw.
//                script is composed of variable-value pairs and test steps.
// looks like:
//                |--test step--------------------------------------------------------------------------------------------------|
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                |[Field Name]:[Field Value]|-------------------------------------------------------------------------------------------|
//                (Nr:10.10; T:'  measurement of dc voltage'; R_on:'101'; Fkt:U_Mess_DC; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:1);),
//                ...
//                (Nr:10.99; T:'end of test case 1'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'101'; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:50.00; T:'beginning of test case n'; R_on:'202'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:50.99; T:'end of test case n'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'202'; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:990.00; T:'end of test script'; R_on:''; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);).
// Compiler     : Delphi 2007
// Author       : 2015-12-11 /bsu/
// History      :
//==============================================================================
unit StepData;

interface
uses Classes;

type
  //enumation for all fields in a test step
  EStepField = (
                SF_NR,      //e.g. Nr: 10.00;
                SF_T,       //e.g. T:'beginning of test case 1';
                SF_INIT,    //e.g. R_on: '101'; or Init:'';
                SF_FCT,     //e.g. Fkt: U_Mess_DC;
                SF_M,       //e.g. M:'';
                SF_PAR,     //e.g. Par:'';
                SF_FINAL,   //e.g. R_off: '101'; or Final:'';
                SF_TOL,     //e.g. Tol:(A:''; Min:0; Max:0)
                SF_TOL_A,   //e.g. A:'';
                SF_TOL_MIN, //e.g. Min:0;
                SF_TOL_MAX  //e.g. Max:0
                );
  //enumation for state of test step by execution
  EStepState = (
                SS_UNKNOWN, //state, before execution or unknown error
                SS_INIT,    //state in initializing
                SS_FCT,     //state in running call-function
                SS_TOL,     //state in validation of the value
                SS_FINAL,   //state in finalization
                SS_OK       //state for ok if the step is succesfully executed completely
                );

  //enumation: indicates whether pseudo-strings exist in field
  EFieldEvalLevel = (
                FEL_SPSEUDO,//static Pseudo-string exists in input
                FEL_DPSEUDO,//dynamic Pseudo-string exists in input
                FEL_FINAL   //final string (all pseudo-strings are replaced)
                );

  FieldStringArray = array[EStepField] of string;

  //a class representing the result of a test step
  TStepResult = class
  protected
    b_state:  EStepState;
    v_result: Variant;
  protected
    procedure SetVariant(const varval: Variant);
  public
    constructor Create();
    destructor Destroy(); override;

    property Value: Variant read v_result write SetVariant;
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

  // a class representing a field in a step, e.g., Nr, T, R_on, Fkt, Par, R_off, Tol(A, Min, Max)
  TStepField = class
  protected
    s_input:  string;   //to save original input string
    e_fel:    EFieldEvalLevel;
    t_evals:  TStrings; //to save string values, which are evaluated, if a pseudo-sting exists
  protected
    procedure SetInputStr(const val: string);
    procedure SetEvalStrings(const vals: TStrings);

  public
    constructor Create();
    destructor Destroy(); override;

    property  InputString: string read s_input write SetInputStr;
    property  EvalLevel: EFieldEvalLevel read e_fel write e_fel;
    property  EvalStrings: TStrings read t_evals write SetEvalStrings;

    procedure Assign(const source: TStepField);
  end;
  StepFieldArray = array[EStepField] of TStepField;

  //a class representing a test step
  TTestStep = class
  class function  GetFieldName(const esf: EStepField): string;
  protected
    a_fields:   StepFieldArray;
    t_result:   TStepResult;
    t_fvalues:  TStrings;     //interanl variable to save the list of a field value, which is split. 
  protected
    procedure FreeFields();
    function  IsMinusStepNr(): boolean;
    procedure AssignResult(const source: TStepResult);

  public
    constructor Create();
    destructor Destroy(); override;

    property  StepResult: TStepResult read t_result write AssignResult;
    property  IsMinusStep: boolean read IsMinusStepNr;

    procedure LoadFields(const fields: FieldStringArray);
    function  GetField(const esf: EStepField): TStepField;
    function  GetFieldValue(const esf: EStepField): string;
    procedure SetFieldValue(const esf: EStepField; const sval: string);
    function  SplitFieldValues(const esf: EStepField; const separator: char): TStrings;
    procedure Assign(const source: TTestStep);
  end;
  
const
  CSTR_FIELD_NAMES :  FieldStringArray = (
                        'NR',
                        'T',
                        'R_ON',
                        'FKT',
                        'M',
                        'PAR',
                        'R_OFF',
                        'TOL',
                        'A',
                        'MIN',
                        'MAX'
                      );


implementation
uses SysUtils, StrUtils, Variants, GenUtils;

procedure TStepField.SetInputStr(const val: string);
begin
  s_input := val;
end;

procedure TStepField.SetEvalStrings(const vals: TStrings);
begin
  t_evals.Clear();
  t_evals.AddStrings(vals);
end;

constructor TStepField.Create();
begin
  inherited Create();
  e_fel := FEL_SPSEUDO;
  t_evals := TStringList.Create();
end;

destructor TStepField.Destroy();
begin
  t_evals.Free();
  inherited Destroy();
end;

procedure TStepField.Assign(const source: TStepField);
begin
  if assigned(source) then begin
    s_input := source.InputString;
    e_fel := source.EvalLevel;
    SetEvalStrings(source.EvalStrings);
  end;
end;

class function  TTestStep.GetFieldName(const esf: EStepField): string;
begin
  result := CSTR_FIELD_NAMES[esf];
end;

procedure  TTestStep.FreeFields();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do
    if assigned(a_fields[i]) then FreeAndNil(a_fields[i]);
end;

function  TTestStep.IsMinusStepNr(): boolean;
var f_stepnr: single; s_stepnr: string;
begin
  result := false;
  s_stepnr := TGenUtils.ReplaceDecimalSeparator(GetFieldValue(SF_NR)); //ReplaceStr(GetFieldValue(SF_NR), '.', DecimalSeparator);
  if TryStrToFloat(s_stepnr, f_stepnr) then result := (f_stepnr < 0.0);
end;

constructor TTestStep.Create();
begin
  inherited Create();
  t_fvalues := TStringList.Create();
end;

destructor TTestStep.Destroy();
begin
  if assigned(t_result) then t_result.Free();
  FreeFields();
  t_fvalues.Free();
  inherited Destroy();
end;

procedure TTestStep.LoadFields(const fields: FieldStringArray);
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do begin
    if fields[i] <> '' then begin
      a_fields[i] := TStepField.Create();
      a_fields[i].InputString := fields[i];
    end;
  end;
end;

function  TTestStep.GetField(const esf: EStepField): TStepField;
begin
  result := a_fields[esf];
end;

function  TTestStep.GetFieldValue(const esf: EStepField): string;
begin
  result := '';
  if assigned(a_fields[esf]) then
    result := a_fields[esf].InputString;
end;

procedure TTestStep.SetFieldValue(const esf: EStepField; const sval: string);
begin
  if assigned(a_fields[esf]) then a_fields[esf].InputString := sval
  else if (sval <> '') then begin
    a_fields[esf] := TStepField.Create();
    a_fields[esf].InputString := sval;
  end;
end;

function  TTestStep.SplitFieldValues(const esf: EStepField; const separator: char): TStrings;
var s_value: string;
begin
  t_fvalues.Clear();
  s_value := GetFieldValue(esf);
  if (s_value <> '') then ExtractStrings([separator], [' ', Char(9)], PChar(s_value), t_fvalues);
  result := t_fvalues;
end;

procedure TTestStep.Assign(const source: TTestStep);
var i: EStepField;
begin
  if assigned(source) then begin
    for i := Low(EStepField) to High(EStepField) do SetFieldValue(i, source.GetFieldValue(i));
    AssignResult(source.StepResult);
  end;
end;

procedure TTestStep.AssignResult(const source: TStepResult);
begin
  if assigned(source) then begin
    if not assigned(t_result) then t_result := TStepResult.Create();
    t_result.Assign(source);
  end else if assigned(t_result) then FreeAndNil(t_result);
end;

procedure TStepResult.SetVariant(const varval: Variant);
begin
  VarCopy(v_result, varval);
end;

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
    s_real := TGenUtils.ReplaceDecimalSeparator(s_real); //ReplaceStr(s_real, '.', DecimalSeparator);
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
