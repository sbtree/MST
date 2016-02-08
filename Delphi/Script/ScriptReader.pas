// =============================================================================
// Module name  : $RCSfile: ScriptReader.pas,v $
// Description  : This unit defines class for reading in a script of test cases.
//                A script is composed some variable definitions and test cases.
//                A test case is composed of some test steps. A test step is
//                composed of some fields. A field has a name and a value.
//                A variable definition has a variable name and a variable value,
//                which are joined through "=" and it looks lie a line  in an
//                ini-file, e.g.:
//                  VERSION=1.4.100000.1.1
//                NOTE: A variable defination must be in one line
//                A field has a field name and a field value, which are separated
//                through ":" and ended through ";", e.g.:
//                  Nr:10.00;
//                A field group is allowed as a field value to be presented in a
//                bracket pair("(" and ")"), e.g.:
//                  Tol:(A:'ABC'; Min:0; Max:123);
//                NOTE: The ";" can be removed after last field in the field group.
//                More than one values should be quoted using quotation marks of
//                ''' or '"' as one value, e.g.:
//                  Par: 'BATCH_0001 BOOTLOADER_0001 FIRMWARE_0001 TIMEOUT_120000';
//                NOTE: one or more blanks (space or tab) can be used to separate
//                the values.
//                A pseudo-string is allowed in a field value with a prifix "#"
//                (statically replaced through settings from the ini-file of a 
//                product variant) or "@" (dynamically replaced in running of the
//                test step), e.g.:
//                  Par:'#TYP_CODE'; and Tol:(A:'@FWVerMain'; Min:0; Max:256);
//                A quotation (a pair of single quotation mark or double
//                quotation mark) can be used in a field value, in order intuitive
//                to present a value, e.g.:
//                  T:"beginning of test case 1";
//                A test step has to be written in a pair of a bracket pair ("("
//                and ")"), e.g.:
//                  (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);)
//                An optional ',' (for ending of a test step) or '.' (for ending 
//                of a test case) kann be given at the end of a test step, e.g:
//                  (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                    ...,
//                  (Nr:10.99; T:'end of test case 1'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'101'; Tol:(A:''; Min:0;Max:0);).
//                One test step may be written in more than one lines, but the
//                line feed is not allowed in a field name, a field value and a
//                quotation, e.g.:
//                  (Nr:10.00;  T:'beginning of test case 1';
//                              R_on:'101'; Fkt:nil; M:''; Par:''; R_off:'';
//                              Tol:(A:''; Min:0;Max:0);)
//                One or more blanks(space and tab) are allowed bewteen fiels in the test step
//                A comment can be written in three patterns:
//                  //comment (line comment start with '//' till the ending of the line)
//                  {comment block} (block comment in brace-pair)
//                  (*comment block*) (block comment in a pair of '(*' and '*)')
//                For example:
//                  (Nr:10.00;  T:'beginning of test case 1'; //start a test case
//                              R_on:'101'; Fkt:nil; {nothing to do} M:''; Par:''; R_off:'';
//                              (*this is a test
//                              line*)
//                              Tol:(A:''; Min:0;Max:0);)
//                NOTE: A comment is allowed not only in a test step, but also
//                in a variable definition. A block comment may be written in
//                more than one liens.
//
//                A complete script of test cases will look like:
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                (Nr:10.10; T:'  measurement of dc voltage'; R_on:'101'; Fkt:U_Mess_DC; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:1);),
//                ...
//                (Nr:10.99; T:'end of test case 1'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'101'; Tol:(A:''; Min:0;Max:0);).
//                ...
//                (Nr:50.00; T:'beginning of test case n'; R_on:'202'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:50.99; T:'end of test case n'; R_on:''; Fkt:nil; M:''; Par:''; R_off:'202'; Tol:(A:''; Min:0;Max:0);),
//                ...
//                (Nr:990.00; T:'end of test script'; R_on:''; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);).
// Compiler     : Delphi 2007
// Author       : 2016-02-03 /bsu/
// History      :
//==============================================================================
unit ScriptReader;

interface
uses Classes, Contnrs, TestStep, StepChecker, StepContainer, TextMessage;

type
  EParseState = (
                PS_IDLE,          // start state or state between ending of a test step and beginning of next test step
                PS_VARNAME,       // in the string before '=' of the pattern 'VAR=VALUE'
                PS_VARVAL,        // in the string after '=' of the pattern 'VAR=VALUE'
                PS_STEP,          // text starts with open bracket '(' for a step
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

  TScriptReader = class(TTextMessager)
  type
    StateEntry = record
      e_state:EParseState;//save state
      i_row:  integer;    //save row of the beginning of the state
      i_col:  integer;    //save column of the beginning of the state
    end;
    PStateEntry = ^StateEntry;

  private
    p_sentry:   PStateEntry;  //a pointer to StateEntry, auxiliary class remember
  protected
    e_curstate: EParseState;  //current state of the reader, identical as the state in t_sentry
    t_sentry:   StateEntry;   //current state entry, inclusive current state
    t_states:   TStack;       //stack of states
    s_curtoken: string;       //save token string, which is found till now
    i_rowindex: integer;      //index of row, which is being parsed
    i_colindex: integer;      //index of column, which is being parsed
    s_srcfile:  string;       //file name
    t_fstemp:   TDateTime;    //save time stemp of last changing for s_srcfile
    s_curtext:  string;       //to save current step text or line of 'var=value', which is parsed
    b_allowvar: boolean;      //indicates if a variable is allowed with the format 'var=value' till now
    t_fkeys:    TFieldKeyChecker; // a help object for parsing
    e_lastfield:EStepField;   //to save the index of last field, which is found in reading the script

    t_tsteps:   TStringList;  //list of test steps without any useless char
    t_container:TStepContainer;   //a container to save steps
    a_fieldvals:FieldStringArray; //an array to save field values of current test step

  protected
    procedure PushState(const state: EParseState);
    procedure PopState();
    procedure ClearStates();
    procedure ResetFieldValues();
    function  CheckFunctionName(const fct: string): boolean;
    function  CheckFieldKey(const key: string): boolean;
    function  CheckFieldValue(const val: string): boolean;
    function  ReadChar(const curch, nextch: char): boolean;

  public
    property StepContainer: TStepContainer read t_container;

    constructor Create();
    destructor Destroy(); override;

    procedure Clear();
    function  ReadFromText(const srctext: string; const blast: boolean = false): boolean; virtual;
    function  ReadFromList(const srclist: TStringList): boolean; virtual;
    function  ReadFromFile(const srcfile: string; const bforce: boolean = false): boolean; virtual;
    function  SaveToFile(const destfile: string): boolean;
  end;

const
  CCHR_EQUAL         = '=';
  CCHR_COLON         = ':';
  CCHR_SEMICOLON     = ';';
  CCHR_COMMA         = ',';
  CCHR_POINT         = '.';
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
uses SysUtils, StrUtils, FunctionBase;

// =============================================================================
//    Description  : push a EParseState into state stack
//    Parameter    : state, a EParseState which is pushed into state stack
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.PushState(const state: EParseState);
begin
  new(p_sentry);
  p_sentry^.e_state := t_sentry.e_state;
  p_sentry^.i_row := t_sentry.i_row;
  p_sentry^.i_col := t_sentry.i_col;
  t_states.Push(p_sentry);
  e_curstate := state;
  t_sentry.e_state := e_curstate;
  t_sentry.i_row := i_rowindex;
  t_sentry.i_col := i_colindex;
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
  p_sentry := t_states.Pop();
  t_sentry.e_state := p_sentry^.e_state;
  t_sentry.i_row := p_sentry^.i_row;
  t_sentry.i_col := p_sentry^.i_col;
  e_curstate := t_sentry.e_state;
  dispose(p_sentry);
end;

procedure TScriptReader.ClearStates();
begin
  while (t_states.Count > 0) do begin
    p_sentry := t_states.Pop();
    dispose(p_sentry);
  end;
  t_sentry.e_state := PS_IDLE;
  t_sentry.i_row := 0;
  t_sentry.i_col := 0;
  e_curstate := t_sentry.e_state;
  i_rowindex := t_sentry.i_row;
  i_colindex := t_sentry.i_col;
end;

procedure TScriptReader.ResetFieldValues();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do a_fieldvals[i] := '';
end;

function TScriptReader.CheckFunctionName(const fct: string) : boolean;
var t_class : TFunctionClass;
begin
  t_class := TFunctionClass(GetClass(fct));
  result := true;//(t_class <> nil);  //test
end;

function  TScriptReader.CheckFieldKey(const key: string): boolean;
begin
  result := t_fkeys.FindKey(key, e_lastfield);
  if result then begin
    if (t_fkeys.IsKeyUsed(e_lastfield)) then result := false
    else t_fkeys.SetKeyUsed(e_lastfield, true);
  end;
end;

function  TScriptReader.CheckFieldValue(const val: string): boolean;
begin
  result := true;
  case e_lastfield of
    SF_NR: result := true; //todo
    SF_FCT: result := CheckFunctionName(val);
    SF_M: result := true; //todo
    SF_T,
    SF_INIT,
    SF_PAR,
    SF_FINAL,
    SF_TOL,
    SF_TOL_A,
    SF_TOL_MIN,
    SF_TOL_MAX: result := true;
  end;
  if result then a_fieldvals[e_lastfield] := val;
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
        if CheckFieldValue(trim(s_curtoken)) then begin
          PopState();
          a_fieldvals[e_lastfield] := trim(s_curtoken);
          s_curtext := s_curtext + a_fieldvals[e_lastfield] + curch;
          s_curtoken := '';
        end else begin
          result := false;
          //todo: handle error
        end;
      end else if (e_curstate = PS_FIELDGROUP) then begin
        if trim(s_curtoken) = '' then begin
          s_curtext := s_curtext + curch;
          PopState(); PopState();
        end else begin
          result := false;
          //todo: handle error
        end;
      end else begin //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDKEY]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_COMMA: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_IDLE) then begin //ending of a test step
        //todo:
      end else begin  //[PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  CCHR_POINT: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_IDLE) then begin //ending of a test case
        //todo:
      end else begin  //[PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
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
      else if e_curstate = PS_FIELDVAL then begin
        if CheckFieldValue(trim(s_curtoken)) then begin
          PopState();
          a_fieldvals[e_lastfield] := trim(s_curtoken);
          s_curtext := s_curtext + a_fieldvals[e_lastfield] + curch;
          s_curtoken := '';
          //if (e_curstate = PS_FIELDGROUP) then PopState();
        end else begin
          result := false;
          //todo: handle error
        end;
      end else if (e_curstate in [PS_FIELDGROUP, PS_STEP]) then begin
        PopState();
        s_curtext := s_curtext + trim(s_curtoken) + curch;
        s_curtoken := '';
        if (e_curstate = PS_IDLE) then begin//a step is over
          t_tsteps.Add(s_curtext);
          t_container.CreateStep(a_fieldvals);
          t_sentry.i_row := i_rowindex; //update row and column for each idle state
          t_sentry.i_col := i_colindex;
          s_curtext := '';
          t_fkeys.ResetUnused();
          ResetFieldValues();
        end else if (e_curstate <> PS_FIELDVAL) then begin
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
          if (s_curtext <> '') then t_tsteps.Add(s_curtext);
          s_curtext := '';
        end;
      end else begin  //[PS_VARNAME, PS_SQUOTATION, PS_DQUOTATION, PS_FIELDKEY, PS_FIELDVAL]
        result := false;
        //todo: handle error
      end;
    end;
  end;
  else begin //other chars
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACECOMMENT, PS_BRACKETCOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARNAME, PS_VARVAL, PS_FIELDKEY, PS_FIELDVAL]) then
        s_curtoken := s_curtoken + curch
      else begin //[PS_IDLE, PS_STEP, PS_FIELDGROUP]
        if (curch in CSET_FIRST_CHARS) then begin
          if (e_curstate = PS_IDLE) then begin
            //todo: what about the help id, e.g. HELP_000100??
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
        end else if (not (curch in CSET_BLANK_CHARS)) then begin
          result := false;
          //todo: unexpected char in current state
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
  t_sentry.e_state := e_curstate;
  t_sentry.i_row := 0;
  t_sentry.i_col := 0;
  t_fstemp := -1;
  b_allowvar := true;
  t_states := TStack.Create();
  t_tsteps := TStringList.Create();
  t_fkeys := TFieldKeyChecker.Create();
  t_container := TStepContainer.Create();
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
  t_tsteps.Free();
  t_fkeys.Free();
  t_container.Free();
end;

procedure TScriptReader.Clear();
begin
  e_curstate := PS_IDLE;
  ClearStates();
  t_fstemp := -1;
  s_srcfile := '';
  s_curtext := '';
  s_curtoken := '';
  b_allowvar := true;
  t_tsteps.Clear();
  t_container.Clear();
end;

function TScriptReader.ReadFromText(const srctext: string; const blast: boolean): boolean;
var i_len, i_lensrc: integer; s_text: string;
begin
  if (srctext = '') then result := true
  else result := false;

  i_colindex := 1; i_lensrc := length(srctext);
  s_text := srctext + CCHR_CR + CCHR_LN; //append CCHR_CR and CCHR_LN, in order easily to solve ending of a line
  i_len := i_lensrc + 2; //length of all char (inclusive CCHR_CR and CCHR_LN)
  while i_colindex < i_len do begin
    result := ReadChar(s_text[i_colindex], s_text[i_colindex + 1]);
    if (not result) then break
    else begin
      if (e_curstate = PS_LINECOMMENT) then i_colindex := i_lensrc; //jump to the end of this text (exclusive CCHR_CR and CCHR_LN), if line comment is found
      Inc(i_colindex);
    end;
  end;
  if blast then begin
    s_curtext := trim(s_curtext);
    result := (result and (s_curtext = '') and (e_curstate = PS_IDLE) and (t_states.Count = 0));
  end;
end;

function TScriptReader.ReadFromList(const srclist: TStringList): boolean;
var i: integer;
begin
  result := false;
  Clear();
  if srclist.Count > 0 then  begin
    for i := 0 to srclist.Count - 2 do begin //read in (n-1) lines, firstly
      Inc(i_rowindex);
      result := ReadFromText(srclist[i], false);
      if (not result) then break;
    end;
    if result then begin //read in the last line
      Inc(i_rowindex);
      result := ReadFromText(srclist[srclist.Count - 1], true);
    end;
    if result then result := (t_tsteps.Count > 0);
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

function TScriptReader.SaveToFile(const destfile: string): boolean;
var i: integer; s_line: string; j:EStepField;
    t_steps: TStringList; t_step: TTestStep; t_field: TStepField;
begin
  t_steps := TStringList.Create();
  for i := 0 to t_container.StepCount() - 1 do begin
    t_step := t_container.GetStep(i);
    if assigned(t_step) then begin
      t_field := t_step.StepFields[SF_NR];
      if assigned(t_field) then s_line := t_field.InputString;
      for j := SF_T to High(EStepField) do begin
        t_field := t_step.StepFields[j];
        if assigned(t_field) then s_line := s_line + ';' + #9 + t_field.InputString;
      end;
      t_steps.Add(s_line);
    end;
  end;
  t_steps.SaveToFile(destfile);
  t_steps.Free();
  //t_tsteps.SaveToFile(destfile);
  result := true;
end;

end.
