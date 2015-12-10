unit ScriptTerm;

interface
uses Classes;
type

  TTestStep = class
  type
    TStepText = class

    end;

    TStepInit = class

    end;

    TStepM = class

    end;

    TStepFunc = class

    end;

    TStepPar = class

    end;

    TStepFinal = class

    end;

    TStepTol = class

    end;

    TStepResult = class

    end;
  protected
    
  end;

  TTestCase = class

  end;

  TTestRoutine = class

  end;
const
  CSTR_TEXT:  string = 'T';
  CSTR_R_ON:  string = 'R_ON';
  CSTR_R_OFF: string = 'R_OFF';
  CSTR_INIT:  string = 'INIT';
  CSTR_M:     string = 'M';
  CSTR_FUNC:  string = 'FKT';
  CSTR_PAR:   string = 'PAR';
  CSTR_TOL:   string = 'TOL';
  CSTR_FINAL: string = 'FINAL';

  CSTR_TERM_DEF:  string = ':';
  CSTR_STEP_BEGIN:string = '(';
  CSTR_STEP_END:  string = '),';
implementation

end.
