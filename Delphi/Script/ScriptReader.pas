unit ScriptReader;

interface
uses Classes, ScriptTerm, TextMessage;

type
  EParseState = (
                PS_IDLE,        // start state or state when a step is parsed and next step does not start yet
                PS_DEFINITION,  // VAR = VALUE
                PS_STEP,        // text starts with (
                PS_TERM,        // e.g. T:xxxx
                PS_QUOTATION,   // text starts with ' or "
                PS_COMMENT,     // text starts with (* or {
                PS_GROUP        // text starts with ( for a term
                );
  TScriptReader = class(TTextMessager)
  protected
    e_curstate: EParseState;
    s_srcfile:  string;
    t_lines:    TStringList;

  protected
    function ParseText(const text: string): boolean;

  public
    function ReadFromFile(const srcfile: string): boolean;
    function ReadFromText(const srctext: string): boolean; overload;
    function ReadFromText(const srclist: TStringList): boolean; overload;

  end;

const
  CSTR_TEXT:    string = 'T';
  CSTR_R_ON:    string = 'R_ON';
  CSTR_INIT:    string = 'INIT';
  CSTR_M:       string = 'M';
  CSTR_FUNC:    string = 'FKT';
  CSTR_PAR:     string = 'PAR';
  CSTR_R_OFF:   string = 'R_OFF';
  CSTR_TOL:     string = 'TOL';
  CSTR_TOL_A:   string = 'A';
  CSTR_TOL_MAX: string = 'MAX';
  CSTR_TOL_MIN: string = 'MIN';
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

function TScriptReader.ParseText(const text: string): boolean;
begin

end;

end.
