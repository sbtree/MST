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

unit TestStep;

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
    s_input: string;
    s_eval:  string;
  protected
  public
    property  InputString: string read s_input write s_input;
    property  EvalString: string read s_eval write s_eval;
    procedure Assign(const source: TStepField);
  end;

  StepFieldArray = array[EStepField] of TStepField;
  TTestStep = class
  protected
    a_fields:   StepFieldArray;
    t_result:   TStepResult;
  protected
    procedure   FreeFields();

  public
    constructor Create();
    destructor Destroy(); override;

    property  StepFields: StepFieldArray read a_fields write a_fields;
    property  StepResult: TStepResult read t_result write t_result;
    procedure InputFields(const fields: FieldStringArray);
    procedure Assign(const source: TTestStep);
    procedure AssignResult(const source: TStepResult);
    //function  FieldString(const field: EStepField): string;
  end;
  
const
  CSTR_FIELD_KEYS_V01 :  FieldStringArray = (
                        'NR', 'T', 'R_ON', 'FKT', 'M', 'PAR', 'R_OFF',
                        'TOL', 'A', 'MIN', 'MAX'
                      );

  CSTR_FIELD_KEYS_V02  :  FieldStringArray = (
                        'NR', 'T', 'INIT', 'FCT', 'M', 'PAR', 'FINAL',
                        'VALID', 'A', 'MIN', 'MAX'
                      );

  CSTR_FIELD_KEYS_V03  :  FieldStringArray = (
                        'NR', 'T', 'INIT', 'FCT', 'M', 'PAR', 'FINAL',
                        'TOL', 'A', 'MIN', 'MAX'
                      );

implementation
uses SysUtils, StrUtils;

procedure TStepField.Assign(const source: TStepField);
begin
  if assigned(source) then begin
    s_input := source.InputString;
    s_eval := source.EvalString;
  end;
end;

procedure  TTestStep.FreeFields();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do
    if assigned(a_fields[i]) then FreeAndNil(a_fields[i]);
end;

constructor TTestStep.Create();
begin
  inherited Create();
end;

destructor TTestStep.Destroy();
begin
  if assigned(t_result) then t_result.Free();
  FreeFields();
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

procedure TTestStep.Assign(const source: TTestStep);
var i: EStepField;
begin
  if assigned(source) then begin
    for i := Low(EStepField) to High(EStepField) do begin
      if assigned(source.StepFields[i]) then begin
        a_fields[i] := TStepField.Create();
        a_fields[i].Assign(source.StepFields[i]);
      end;
    end;
    AssignResult(source.StepResult);
  end;
end;

procedure TTestStep.AssignResult(const source: TStepResult);
begin
  if assigned(source) then begin
    if not assigned(t_result) then t_result := TStepResult.Create();
    t_result.Assigne(source);
  end else if assigned(t_result) then FreeAndNil(t_result);
end;
{function  TTestStep.FieldString(const field: EStepField): string;
begin
  result := '';
  if assigned(a_fields[field]) then result := a_fields[field].InputString;
end;}

end.
