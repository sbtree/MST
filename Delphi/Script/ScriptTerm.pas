// =============================================================================
// Module name  : $RCSfile: ScriptTerm.pas,v $
// Description  : This unit defines all terminologies in a test script. A test
//                script looks like:
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
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

unit ScriptTerm;

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
    property InputString: string read s_input write s_input;
    property EvalString: string read s_eval write s_eval;
  end;

  TTestStep = class
  protected
    a_fields:   array[EStepField] of TStepField;
    t_result:   TStepResult;
  protected
    procedure   FreeFields();

  public
    constructor Create();
    destructor Destroy(); override;

    property StepResult: TStepResult read t_result write t_result;
    procedure InputFields(const fields: FieldStringArray);
    //function ResolveTerm(var text: string; const term: EStepTerm): TStepTerm;
  end;

  TTestCase = class
  protected
    //list of step
  end;

  TTestSequence = class
  protected
    //list of case
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

procedure  TTestStep.FreeFields();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do
    if assigned(a_fields[i]) then FreeAndNil(a_fields[i]);
end;

constructor TTestStep.Create();
begin
  inherited Create();
  t_result := TStepResult.Create();
end;

destructor TTestStep.Destroy();
begin
  t_result.Free();
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

end.
