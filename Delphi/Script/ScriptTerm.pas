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

  EStepTerm = (
                ST_NR,      //e.g. Nr: 10.00;
                ST_T,       //e.g. T:'beginning of test case 1';
                ST_INIT,    //e.g. R_on: '101'; or Init:'';
                ST_FCT,     //e.g. Fkt: U_Mess_DC;
                ST_M,       //e.g. M:'';
                ST_PAR,     //e.g. Par:'';
                ST_FINAL,   //e.g. R_off: '101'; or Final:'';
                ST_TOL,     //e.g. Tol:(A:''; Min:0; Max:0);
                ST_TOL_A,   //e.g. A:'';
                ST_TOL_MIN, //e.g. Min:0;
                ST_TOL_MAX  //e.g. Max:0
                );

  // a base class for terminology in a step: Nr, T, R_on, Fkt, Par, R_off, Tol(A, Min, Max)
  TStepTerm = class
  protected
    s_input: string;
    s_eval:  string;
  protected
  public
    property InputString: string read s_input write s_input;
    property EvalString: string read s_eval;
  end;

  TTestStep = class
  type AStepTerms = array[EStepTerm] of TStepTerm;
  protected
    a_terms:    AStepTerms;
    t_result:   TStepResult;

  protected
  public
    property StepResult: TStepResult read t_result write t_result;
    property StepTerms: AStepTerms read a_terms write a_terms;

    //function ResolveTerm(var text: string; const term: EStepTerm): TStepTerm;
  end;

  TTestCase = class
  protected
    //list of step
  end;

  TTestRoutine = class
  protected
    //list of case
  end;

implementation
const
  CSTR_TERMS_VER01 :  array[EStepTerm] of string = (
                        'NR', 'T', 'R_ON', 'FKT', 'M', 'PAR', 'R_OFF',
                        'TOL', 'A', 'MIN', 'MAX'
                      );
                      
  CSTR_TERMS_VER02 :  array[EStepTerm] of string = (
                        'NR', 'T', 'INIT', 'FCT', 'M', 'PAR', 'FINAL',
                        'VALID', 'A', 'MIN', 'MAX'
                      );
end.
