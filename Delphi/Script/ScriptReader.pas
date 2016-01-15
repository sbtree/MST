unit ScriptReader;

interface
uses Classes, Contnrs, ScriptTerm, TextMessage, IniFiles;

type
  EParseState = (
                PS_IDLE,        // start state or state when a step is parsed and next step does not start yet
                PS_VARNAME,     // VAR=VALUE
                PS_VARVALUE,    // VAR=VALUE
                PS_STEP,        // text starts with ( for a step
                PS_TERMNAME,    // e.g. T of T:xxxx
                PS_TERMVALUE,   // e.g. xxxx of T:xxxx
                PS_QUOTATION,   // text starts with ' or "
                PS_LINECOMMENT,    // text starts with //
                PS_BRACKETCOMMENT, // comment text starts with (*
                PS_BRACECOMMENT,   // comment text starts with {
                PS_TERMGROUP,   // text starts with ( for a term
                PS_PENDING      // it cannot be decided yet
                );
  PParseState = ^EParseState;

  ParseStateSet = set of EParseState;


  TScriptReader = class(TTextMessager)
  private
    pe_state:   PParseState; //a pointer to EParseState, auxiliary class remember
  protected
    e_curstate: EParseState; //current state of the reader
    t_states:   TStack;      //stack of states
    c_lastread: char;        //save last char, which is inputted
    c_expected: char;        //save a char, which is expected 
    s_curtoken: string;      //save token string, which is found till now
    i_rowindex: integer;     //index of row, which is being parsed
    i_colindex: integer;     //index of column, which is being parsed
    s_srcfile:  string;      //file name
    t_fstemp:   TDateTime;   //save time stemp of last changing for s_srcfile
    t_steplines:TStringList; //list of test steps without any useless char
    s_curtext:  string;      //useful chars of the step, which is just being parsed
    t_tokens:   TStack;      //save tokens

    t_steps:    TObjectList; //list of test steps
    b_allowvar: boolean;     //indicates if a variable is allowed with the format 'var=value' till now

  protected
    procedure PushState(const state: EParseState);
    procedure PopState();
    function  ReadNextChar(const ch: char): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    function ReadFromText(const srctext: string; const blast: boolean = false): boolean; virtual;
    function ReadFromList(const srclist: TStringList): boolean; virtual;
    function ReadFromFile(const srcfile: string; const bforce: boolean = false): boolean; virtual;
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
  CCHR_SLASH:         char = '/';
  CCHR_STAR:          char = '*';

  CSET_BLANK_CHARS: set of char = [' ', Char($9), Char($A), Char($B), Char($C), Char($D)];
  CSET_FIRST_CHARS: set of char = ['_', 'A'..'Z', 'a'..'z'];
  CSET_DIGIT_CHARS: set of char = ['0'..'9'];

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
//    Description  : input a new char
//    Parameter    : --
//    Return       : --
//    First author : 2016-01-12 /bsu/
//    History      :
// =============================================================================
function TScriptReader.ReadNextChar(const ch: char): boolean;
begin
  result := true;
  case e_curstate of
    PS_IDLE:
    begin
      if (not (ch in CSET_BLANK_CHARS)) then begin
        if (ch = CCHR_BRACE_OPEN) then PushState(PS_BRACECOMMENT)
        else if (ch in [CCHR_BRACKET_OPEN, CCHR_SLASH]) then  PushState(PS_PENDING)
        else if (ch = CCHR_BRACE_OPEN) then PushState(PS_BRACECOMMENT)
        else if (ch in CSET_FIRST_CHARS) then begin
          if b_allowvar then begin
            s_curtoken := s_curtoken + ch;
            PushState(PS_VARNAME);
          end else AddMessage('A variable can only defined at beginning of the script.');
        end else AddMessage(format('The text can not be parsed: ln%d, col%d', [i_rowindex, i_colindex]));;
      end;
    end;
    PS_VARNAME:
    begin

    end;
    PS_VARVALUE: ;
    PS_STEP: ;
    PS_TERMNAME: ;
    PS_TERMVALUE: ;
    PS_QUOTATION: ;
    PS_LINECOMMENT: ;
    PS_BRACKETCOMMENT: ;
    PS_BRACECOMMENT: ;
    PS_TERMGROUP: ;
    PS_PENDING: 
    begin
      PopState();
      case c_lastread of
      '(':
      begin
        if (ch = CCHR_STAR) then PushState(PS_BRACKETCOMMENT)
        else s_curtoken := s_curtoken + ch;
      end;
      '/':
      begin
        if (ch = CCHR_SLASH) then PushState(PS_LINECOMMENT)
        else if (e_curstate in [PS_VARVALUE, PS_TERMVALUE] )then s_curtoken := s_curtoken + ch
        else AddMessage(format('The text can not be parsed: ln%d, col%d', [i_rowindex, i_colindex]));
      end;
      '*':;
      end;
    end;
  end;

  if (e_curstate = PS_IDLE) then begin //a complete step is parsed
  end;
  c_lastread := ch;
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
  t_fstemp := -1;
  b_allowvar := true;
  t_states := TStack.Create();
  t_steplines := TStringList.Create;
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
  t_steplines.Free;
end;

function TScriptReader.ReadFromText(const srctext: string; const blast: boolean): boolean;
var i: integer;
begin
  result := false; i_colindex := 0;
  for i := 1 to length(srctext) do begin
    Inc(i_rowindex);
    result := ReadNextChar(srctext[i]);
    if (not result) then break
    else if (e_curstate = PS_IDLE) then begin  //a complete step is archived
      if (s_curtext <> '') then begin
        t_steplines.Add(s_curtext);
        s_curtext := '';
      end;
    end;
  end;

  if blast then begin
    s_curtext := trim(s_curtext);
    result := result and (s_curtext = '') and (e_curstate = PS_IDLE) and (t_states.Count = 0);
  end;
end;

function TScriptReader.ReadFromList(const srclist: TStringList): boolean;
var i: integer;
begin
  result := false; i_rowindex := 0;
  t_steplines.Clear();
  if srclist.Count > 0 then  begin
    for i := 0 to srclist.Count - 2 do begin
      Inc(i_rowindex);
      result := ReadFromText(srclist[i]);
      if (not result) then break;
    end;
    if result then begin
      Inc(i_rowindex);
      result := ReadFromText(srclist[srclist.Count - 1], true);
    end;

    if result then result := (t_steplines.Count > 0);
  end;
end;

function TScriptReader.ReadFromFile(const srcfile: string; const bforce: boolean ): boolean;
var t_lines: TStringList; b_update: boolean; t_fdatetime: TDateTime;
begin
  result := FileExists(srcfile);
  if result then begin
    FileAge(srcfile, t_fdatetime);
    if SameText(s_srcfile, srcfile) then b_update := (t_fdatetime <> t_fstemp)
    else b_update := true;

    if (bforce or b_update) then begin
      t_lines := TStringList.Create();
      t_lines.LoadFromFile(srcfile);
      result := ReadFromList(t_lines);
      s_srcfile := srcfile;
      t_fstemp := t_fdatetime;
      t_lines.Free();
    end;
  end;
end;

end.
