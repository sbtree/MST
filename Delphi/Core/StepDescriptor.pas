// =============================================================================
// Module name  : $RCSfile: ScriptTerm.pas,v $
// Description  : This unit defines all terminologies in a test script. A test
//                script is composed of variable-value pairs and test steps.
// looks like:
//                |--test step--------------------------------------------------------------------------------------------------|
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                |[Field Name]:[Field Value]|----|test step--------------------------------------------------------------------------------------------------|
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

unit StepDescriptor;

interface
uses Classes, StepResult;

type

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

  FieldStringArray = array[EStepField] of string;

  // a base class for terminology in a step: Nr, T, R_on, Fkt, Par, R_off, Tol(A, Min, Max)
  TStepField = class
  protected
    s_input:  string;   //to save original input string
    //t_parts:  TStrings; //to save string values, which are split from s_input by method Split
    //c_separ:  char;     //char as separator for splitting s_input
    //s_evalst: string;   //to save string, which is evaluated using the statical information in the configuration
    //s_evaldy: string;   //to save string, which is evaluated using the dynamical inforation in the run
  protected
    procedure SetInputStr(const val: string);
    procedure EvalInputStr();

  public
    constructor Create();
    destructor Destroy(); override;

    property  InputString: string read s_input write SetInputStr;

    procedure Assign(const source: TStepField);
    //function  SplitStrings(const separator: char): TStrings;
  end;

  StepFieldArray = array[EStepField] of TStepField;

  TTestStep = class
  protected
    a_fields:   StepFieldArray;
    t_result:   TStepResult;
    t_fvalues:  TStrings;     //to save the list of a field value, which is split. See SplitFieldValues
  protected
    procedure FreeFields();
    function  IsMinusStepNr(): boolean;
    procedure AssignResult(const source: TStepResult);

  public
    constructor Create();
    destructor Destroy(); override;

    property  StepResult: TStepResult read t_result write AssignResult;
    property  IsMinusStep: boolean read IsMinusStepNr;
    
    procedure InputFields(const fields: FieldStringArray);
    function  GetField(const esf: EStepField): TStepField;
    function  GetFieldName(const esf: EStepField): string;
    function  GetFieldValue(const esf: EStepField): string;
    procedure SetFieldValue(const esf: EStepField; const sval: string);
    function  SplitFieldValues(const esf: EStepField; const separator: char): TStrings;
    procedure Assign(const source: TTestStep);
  end;

implementation
uses SysUtils, StrUtils;

procedure TStepField.SetInputStr(const val: string);
begin
  s_input := val;
  EvalInputStr();
end;

procedure TStepField.EvalInputStr();
begin
  //todo:
end;

constructor TStepField.Create();
begin
  inherited Create();
end;

destructor TStepField.Destroy();
begin
  inherited Destroy();
end;

procedure TStepField.Assign(const source: TStepField);
begin
  if assigned(source) then begin
    s_input := source.InputString;
  end;
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
  s_stepnr := ReplaceStr(GetFieldValue(SF_NR), '.', DecimalSeparator);
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

procedure TTestStep.InputFields(const fields: FieldStringArray);
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

function  TTestStep.GetFieldName(const esf: EStepField): string;
begin
  //todo: names in StepChecker
  result := '';
end;

function  TTestStep.GetFieldValue(const esf: EStepField): string;
begin
  result := '';
  if assigned(a_fields[esf]) then result := a_fields[esf].InputString;
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
{function  TTestStep.FieldString(const field: EStepField): string;
begin
  result := '';
  if assigned(a_fields[field]) then result := a_fields[field].InputString;
end;}

end.