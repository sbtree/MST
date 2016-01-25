unit ScriptReader;

interface
uses Classes, Contnrs, ScriptTerm, StepChecker, TextMessage;

type
  EParseState = (
                PS_IDLE,          // start state or state when a step is parsed and next step does not start yet
                PS_VARNAME,       // VAR=VALUE
                PS_VARVAL,        // VAR=VALUE
                PS_STEP,          // text starts with ( for a step
                PS_FIELDKEY,      // e.g. T of T:xxxx
                PS_FIELDVAL,      // e.g. xxxx of T:xxxx
                PS_SQUOTATION,    // text starts with single quote mark '
                PS_DQUOTATION,    // text starts with single quote mark "
                PS_LINECOMMENT,   // text starts with //
                PS_BRACKETCOMMENT,// comment text starts with (*
                PS_BRACECOMMENT,  // comment text starts with {
                PS_FIELDGROUP     // text starts with ( for a term
                );
  PParseState = ^EParseState;

  //ParseStateSet = set of EParseState;


  TScriptReader = class(TTextMessager)
  private
    pe_state:   PParseState;  //a pointer to EParseState, auxiliary class remember
  protected
    e_curstate: EParseState;  //current state of the reader
    t_states:   TStack;       //stack of states
    s_curtoken: string;       //save token string, which is found till now
    i_rowindex: integer;      //index of row, which is being parsed
    i_colindex: integer;      //index of column, which is being parsed
    s_srcfile:  string;       //file name
    t_fstemp:   TDateTime;    //save time stemp of last changing for s_srcfile
    t_steplines:TStringList;  //list of test steps without any useless char
    s_curtext:  string;       //to save a step text or a line of 'var=value', which is parsed
    b_allowvar: boolean;      //indicates if a variable is allowed with the format 'var=value' till now
    t_fkeys:    TFieldKeyChecker;   // a help object for parsing

  protected
    procedure PushState(const state: EParseState);
    procedure PopState();
    function  CheckFieldKey(const key: string): boolean;
    function  ReadChar(const curch, nextch: char): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    function ReadFromText(const srctext: string; const blast: boolean = false): boolean; virtual;
    function ReadFromList(const srclist: TStringList): boolean; virtual;
    function ReadFromFile(const srcfile: string; const bforce: boolean = false): boolean; virtual;
    function SaveSteps(const destfile: string): boolean;
  end;

const
  CSTR_COMMENT_LINE:  string = '//';
  CSTR_COMMENT_BEGIN: string = '(*';
  CSTR_COMMENT_END:   string = '*)';

  CCHR_EQUAL         = '=';
  CCHR_COLON         = ':';
  CCHR_SEMICOLON     = ';';
  CCHR_COMMA         = ',';
  CCHR_SQUOTATION    = '''';
  CCHR_DQUOTATION    = '"';
  CCHR_BRACKET_OPEN  = '(';
  CCHR_BRACKET_CLOSE = ')';
  CCHR_BRACE_OPEN    = '{';
  CCHR_BRACE_CLOSE   = '}';
  CCHR_SLASH         = '/';
  CCHR_STAR          = '*';
  CCHR_SPACE         = ' ';
  CCHR_TAB           = Char(9);
  CCHR_LN            = Char(10);
  CCHR_CR            = Char(13);

  CSET_BLANK_CHARS: set of char = [CCHR_TAB, CCHR_SPACE];
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

function  TScriptReader.CheckFieldKey(const key: string): boolean;
var i_index: EStepField;
begin
  result := t_fkeys.FindKey(key, i_index);
  if result then begin
    if (t_fkeys.IsKeyUsed(i_index)) then result := false
    else t_fkeys.SetKeyUsed(i_index, true);
  end;
end;

// =============================================================================
//    Description  : input a new char and next char
//    Parameter    : curch,
//                   nextch,
//    Return       : --
//    First author : 2016-01-12 /bsu/
//    History      :
// =============================================================================
function  TScriptReader.ReadChar(const curch, nextch: char): boolean;
begin
//[PS_IDLE, PS_VARNAME, PS_VARVAL, PS_STEP, PS_FIELDKEY, PS_FIELDVAL, PS_SQUOTATION, PS_DQUOTATION, PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT, PS_FIELDGROUP]
  result := true;
  case curch of
  CCHR_EQUAL: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_VARNAME) then begin
        PopState();
        s_curtext := s_curtext + trim(s_curtoken) + curch;
        s_curtoken := '';
        PushState(PS_VARVAL);
      end else begin  //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_COLON: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_FIELDKEY) then begin
        if CheckFieldKey(trim(s_curtoken)) then begin
          PopState();
          s_curtext := s_curtext + trim(s_curtoken) + curch;
          s_curtoken := '';
          PushState(PS_FIELDVAL);
        end else begin
          result := false;
          //todo: handle error
        end;
      end else begin  //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDVAL, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_SEMICOLON: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_FIELDVAL) then begin
        PopState();
        s_curtext := s_curtext + trim(s_curtoken) + curch;
        s_curtoken := '';
      end else begin //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_COMMA: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT, PS_IDLE])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else begin  //[PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_SQUOTATION: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then begin
        s_curtoken := s_curtoken + curch;
        if (e_curstate = PS_SQUOTATION) then PopState()
        else if (e_curstate in [PS_VARVAL, PS_FIELDVAL]) then PushState(PS_SQUOTATION);
      end else begin //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_DQUOTATION: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then begin
        s_curtoken := s_curtoken + curch;
        if (e_curstate = PS_DQUOTATION) then PopState()
        else if (e_curstate in [PS_VARVAL, PS_FIELDVAL]) then PushState(PS_DQUOTATION);
      end else begin //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_BRACKET_OPEN: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION]) then s_curtoken := s_curtoken + curch
      else if (nextch = CCHR_STAR) then begin
        PushState(PS_BRACKETCOMMENT);
        Inc(i_colindex);
      end else begin  
        if (e_curstate = PS_IDLE) then begin
          if (trim(s_curtoken) = '') then begin
            s_curtext := s_curtext + curch;
            PushState(PS_STEP);
            b_allowvar := false;
          end else begin
            result := false;
            //todo: handle error
          end; 
        end else if (e_curstate = PS_FIELDVAL) then begin
          if (trim(s_curtoken) = '') then begin
            PushState(PS_FIELDGROUP);
            s_curtext := s_curtext + curch;
          end else s_curtoken := s_curtoken + curch;
        end else if (e_curstate = PS_VARVAL) then s_curtoken := s_curtoken + curch
        else begin  //[PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
          result := false;
          //todo: handle error
        end;
      end;
    end;
  end;
  CCHR_BRACKET_CLOSE: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate in [PS_FIELDVAL, PS_FIELDGROUP, PS_STEP]) then begin
        PopState();
        s_curtext := s_curtext + trim(s_curtoken) + curch;
        s_curtoken := '';
        if (e_curstate = PS_IDLE) then begin//a step is over
          t_steplines.Add(s_curtext);
          s_curtext := '';
          t_fkeys.ResetUnused();
        end else if (e_curstate in [PS_STEP, PS_FIELDGROUP, PS_FIELDVAL]) then begin
          if (e_curstate = PS_FIELDGROUP) then PopState();
        end else begin 
          result := false;
          //todo: handle error
        end;
      end else begin   //[PS_IDLE, PS_VARNAME, PS_FIELDKEY]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_BRACE_OPEN: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION]) then s_curtoken := s_curtoken + curch
      else PushState(PS_BRACECOMMENT); //[PS_IDLE, PS_STEP, PS_VARNAME, PS_VARVAL, PS_FIELDKEY, PS_FIELDVAL, PS_FIELDGROUP]
    end;
  end;
  CCHR_BRACE_CLOSE: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_BRACECOMMENT) then PopState()
      else begin  //[PS_IDLE, PS_STEP, PS_VARNAME, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end; 
    end;
  end;
  CCHR_SLASH: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION]) then s_curtoken := s_curtoken + curch
      else if (nextch = CCHR_SLASH) then begin
        s_curtext := s_curtext + trim(s_curtoken);
        s_curtoken := '';
        PushState(PS_LINECOMMENT);
        Inc(i_colindex);
      end else begin
        if (e_curstate in [PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
        else begin //[PS_IDLE, PS_STEP, PS_VARNAME, PS_FIELDKEY, PS_FIELDGROUP]
          result := false;
          //todo: handle error
        end; 
      end;
    end;
  end;
  CCHR_STAR:begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_BRACKETCOMMENT) then begin
        if (nextch = CCHR_BRACKET_CLOSE) then begin
          PopState();
          Inc(i_colindex);
        end;
      end else begin  //[PS_IDLE, PS_STEP, PS_VARNAME, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_LN, CCHR_CR: begin
    if (not (e_curstate in [PS_IDLE, PS_STEP, PS_FIELDGROUP, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate = PS_LINECOMMENT) then PopState()
      else if (e_curstate = PS_VARVAL) then begin
        s_curtext := s_curtext + trim(s_curtoken);
        s_curtoken := '';
        PopState();
        if (e_curstate = PS_IDLE) then begin//a step is over
          if (s_curtext <> '') then t_steplines.Add(s_curtext);
          s_curtext := '';
          t_fkeys.ResetUnused();
        end;
      end else begin  //[PS_VARNAME, , PS_SQUOTATION, PS_DQUOTATION, PS_FIELDKEY, PS_FIELDVAL]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  else begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACECOMMENT, PS_BRACKETCOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARNAME, PS_VARVAL, PS_FIELDKEY, PS_FIELDVAL]) then
        s_curtoken := s_curtoken + curch
      else begin
        if (curch in CSET_FIRST_CHARS) then begin
          if (e_curstate = PS_IDLE) then begin
            if b_allowvar then begin
              s_curtoken := s_curtoken + curch;
              PushState(PS_VARNAME);
            end else begin
              result := false;
              //todo: handle error
            end;
          end else begin //[PS_STEP, PS_FIELDGROUP]
            s_curtoken := s_curtoken + curch;
            PushState(PS_FIELDKEY); 
          end;
        end else begin
        end;
      end;
    end;
  end;
  end;
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
  t_steplines := TStringList.Create();
  t_fkeys := TFieldKeyChecker.Create();
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
  t_steplines.Free();
  t_fkeys.Free();
end;

function TScriptReader.ReadFromText(const srctext: string; const blast: boolean): boolean;
var i_len: integer; s_text: string;
begin
  if (srctext = '') then result := true
  else result := false;
   
  i_colindex := 1; i_len := length(srctext);
  s_text := srctext + CCHR_CR;
  while i_colindex <= i_len do begin
    result := ReadChar(s_text[i_colindex], s_text[i_colindex + 1]);
    if (not result) then break
    else begin
      if (e_curstate = PS_LINECOMMENT) then i_colindex := i_len;
      Inc(i_colindex);
    end;
  end;
  if result then result := ReadChar(CCHR_CR, CCHR_LN);
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
      result := ReadFromText(srclist[i], false);
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
      b_allowvar := true;
      t_lines := TStringList.Create();
      t_lines.LoadFromFile(srcfile);
      result := ReadFromList(t_lines);
      s_srcfile := srcfile;
      t_fstemp := t_fdatetime;
      t_lines.Free();
    end;
  end;
end;

function TScriptReader.SaveSteps(const destfile: string): boolean;
begin
  t_steplines.SaveToFile(destfile);
  result := true;
end;
end.
