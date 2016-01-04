unit ScriptReader;

interface
uses Classes, Contnrs, ScriptTerm, TextMessage, IniFiles;

type
  EParseState = (
                PS_IDLE,        // start state or state when a step is parsed and next step does not start yet
                PS_VARIABLE,    // VAR=VALUE
                PS_STEP,        // text starts with ( for a step
                PS_TERM,        // e.g. T:xxxx
                PS_QUOTATION,   // text starts with ' or "
                PS_LNCOMMENT,   // text starts with //
                PS_BLKCOMMENT,  // text starts with (* or {
                PS_TERMGROUP    // text starts with ( for a term
                );
  PParseState = ^EParseState;

  ParseStateSet = set of EParseState;


  TScriptReader = class(TTextMessager)
  protected
    e_curstate: EParseState; //current state of the reader
    pe_state:   PParseState; //a pointer to EParseState
    t_states:   TStack;      //stack of states
    s_srcfile:  string;      //file name
    s_curtext:  string;      //string, which is not yet parsed
    t_srclines: TStringList; //liens from a source.
    t_varvals:  THashedStringList;
    t_steps:    TObjectList; //list of test steps
    b_allowdef: boolean;     //indicates if a defination is allowed till now
    i_curindex: integer;

  protected
    function  ParseText(const text: string): boolean;
    function  FindTerm(const pattern: string): TStepTerm;
    procedure PushState(const state: EParseState);
    procedure PopState();

  public
    constructor Create();
    destructor Destroy(); override;

    function ReadFromFile(const srcfile: string): boolean;
    function ReadFromText(const srctext: string): boolean;
    function ReadFromList(const srclist: TStringList): boolean;
  end;

const
  CSTR_NR:      string = 'NR';
  CSTR_TEXT:    string = 'T';
  CSTR_R_ON:    string = 'R_ON';
  CSTR_INIT:    string = 'INIT';
  CSTR_FKT:     string = 'FKT';
  CSTR_FCT:     string = 'FCT';
  CSTR_M:       string = 'M';
  CSTR_PAR:     string = 'PAR';
  CSTR_R_OFF:   string = 'R_OFF';
  CSTR_TOL:     string = 'TOL';
  CSTR_TOL_A:   string = 'A';
  CSTR_TOL_MIN: string = 'MIN';
  CSTR_TOL_MAX: string = 'MAX';
  CSTR_FINAL:   string = 'FINAL';
  CSTR_COMMENT_LINE:  string = '//';
  CSTR_COMMENT_BEGIN: string = '(*';
  CSTR_COMMENT_END:   string = '*)';

  CCHR_COLON:         char = ':';
  CCHR_SEMICOLON:     char = ';';
  CCHR_COMMA:         char = ',';
  CCHR_SQUOTATION:    char = '''';
  CCHR_DQUOTATION:    char = '"';
  CCHR_BRACKET_OPEN:  char = '(';
  CCHR_BRACKET_CLOSE: char = ')';
  CCHR_BRACE_OPEN:    char = '{';
  CCHR_BRACE_CLOSE:   char = '}';

implementation

// =============================================================================
//    Description  : push a EParseState into state stack
//    Parameter    : state, a EParseState which is pushed into state stack
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.PushState(const state: EParseState);
begin
  new(pe_state);
  pe_state^ := e_curstate;
  t_states.Push(pe_state);
  e_curstate := state;
end;

// =============================================================================
//    Description  : pop a CheckerState from the state stack
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.PopState();
begin
  pe_state := t_states.Pop();
  e_curstate := pe_state^;
  dispose(pe_state);
end;

// =============================================================================
//    Description  : constructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
constructor TScriptReader.Create();
begin
	inherited Create;
  e_curstate := PS_IDLE;
  t_states := TStack.Create();
end;

// =============================================================================
//    Description  : destructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
destructor TScriptReader.Destroy();
begin
	inherited Destroy;
  while(t_states.Count > 0) do PopState();
  t_states.Free();
end;


function TScriptReader.ParseText(const text: string): boolean;
begin
  //definition VAR=VALUE
  //test step (Nr: 10.00; T:''; Fkt:abcd; M:'';),
end;

end.
