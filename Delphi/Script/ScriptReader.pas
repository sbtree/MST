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
  private
    pe_state:   PParseState; //a pointer to EParseState, auxiliary class remember
  protected
    e_curstate: EParseState; //current state of the reader
    t_states:   TStack;      //stack of states
    s_srcfile:  string;      //file name
    t_srclines: TStringList; //liens from a source.
    //t_varvals:  THashedStringList;
    t_purelines:TStringList; //liens without any useless char.
    t_steps:    TObjectList; //list of test steps
    b_allowvar: boolean;     //indicates if a variable is allowed with the format 'var=value' till now
    s_curtext:  string;      //string, which is not yet parsed
    i_curpos:   integer;
    s_validstep:string;      //string, which is useful for a step
    i_curline:  integer;

  protected
    procedure PushState(const state: EParseState);
    procedure PopState();

  public
    constructor Create();
    destructor Destroy(); override;

    function ReadFromLine(const srctext: string; const bstop: boolean = false): boolean; virtual;
    function ReadFromList(const srclist: TStringList): boolean; virtual;
    function ReadFromFile(const srcfile: string): boolean; virtual;
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
uses SysUtils, StrUtils;

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
  i_curpos := 1;
  b_allowvar := true;
  t_states := TStack.Create();
  t_purelines := TStringList.Create;
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
  t_purelines.Free;
end;

function TScriptReader.ReadFromLine(const srctext: string; const bstop: boolean): boolean;
const  CSET_BLANK_CHARS: set of char = [' ', $9, $A, $B, $C, $D];
var i, i_curpos: integer;
begin
  if e_curstate = PS_IDLE then begin
    if s_validstep <> '' then t_purelines.Add(s_validstep);
    s_validstep := '';
    s_curtext := MidStr(s_curtext, i_curpos + 1);
    i_curpos := 1;
  end;
  //todo: 
  i_curpos := length(s_curtext) + 1;
  s_curtext := s_curtext + srctext;
  for i := i_curpos to length(s_curtext) do begin
    case e_curstate of
      PS_IDLE: begin //only comment, variable and step are possible
        if (s_curtext[i] in CSET_BLANK_CHARS) then

      end;
      PS_VARIABLE: ;
      PS_STEP: begin //comment and terminology are allowed
        b_allowvar := false; //variables are allowed only before the test step
      end;
      PS_TERM: ;
      PS_QUOTATION: ;
      PS_LNCOMMENT: ;
      PS_BLKCOMMENT: ;
      PS_TERMGROUP: ;
    end;
  end;
end;

function TScriptReader.ReadFromList(const srclist: TStringList): boolean;
begin

end;

function TScriptReader.ReadFromFile(const srcfile: string): boolean;
begin

end;

end.
