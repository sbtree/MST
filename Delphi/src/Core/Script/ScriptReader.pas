// =============================================================================
// Module name  : $RCSfile: ScriptReader.pas,v $
// Description  : This unit defines a class for reading a test script.
//                A test script is composed some variable definitions and some
//                test cases.
//                A variable definition has a variable name and a variable value,
//                which are joined through "=" and it looks like a line in an
//                ini-file, e.g.:
//                  VERSION=1.4.100000.1.1
//                NOTE: A variable defination must be written in one line.
//                A test case is a set of test steps. A test step includes some
//                predefined fields and every field must have a name and a value
//                which are separated through ":" and it ends with ";", e.g.:
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
uses  Classes, Contnrs, StepData, StepGroup, StepChecker, TextMessage,
      TypInfo, StringPairs;

type
  EParseState = (
                PS_IDLE,          // start state or state between ending of a test step and beginning of next test step
                PS_VARNAME,       // in the string before '=' of the pattern 'VAR=VALUE'
                PS_VARVAL,        // in the string after '=' of the pattern 'VAR=VALUE'
                PS_STEP,          // text starts with open bracket '(' for a step
                PS_FIELDNAME,     // e.g. T of T:xxxx
                PS_FIELDVAL,      // e.g. xxxx of T:xxxx
                PS_SQUOTATION,    // text starts with single quote mark '
                PS_DQUOTATION,    // text starts with single quote mark "
                PS_LINECOMMENT,   // text starts with //
                PS_BRACKETCOMMENT,// comment text starts with (*
                PS_BRACECOMMENT,  // comment text starts with {
                PS_FIELDGROUP     // text starts with ( for a term
                );
  PParseState = ^EParseState;

  TScriptReader = class(TInterfacedObject, ITextMessengerImpl)
  type
    StateEntry = record
      e_state:EParseState;//save state
      i_row:  integer;    //save row of the beginning of the state
      i_col:  integer;    //save column of the beginning of the state
      e_field:EStepField; //save step field of a group
    end;
    PStateEntry = ^StateEntry;

  private
    p_sentry:   PStateEntry;  //a pointer to StateEntry, auxiliary class remember
    p_ptinfo:   PTypeInfo;    //a pointer to type info of EParseState
  protected
    e_curstate: EParseState;  //current state of the reader, identical as the state in t_sentry
    t_sentry:   StateEntry;   //current state entry, inclusive current state
    t_states:   TStack;       //stack of states
    s_curtoken: string;       //save token string, which is found till now
    i_rowindex: integer;      //index of row, which is being parsed
    i_colindex: integer;      //index of column, which is being parsed
    t_srcfiles: TStrings;     //to save file names , which are loaded
    a_fstemps:  array of TDateTime; //to save time stemps of the loaded files
    s_curtext:  string;       //to save current step text or line of 'var=value', which is parsed
    b_allowvar: boolean;      //indicates if a variable is allowed with the format 'var=value' till now
    t_fnchecker:TFieldNameChecker;  // a help object for parsing
    t_fvchecker:TFieldValueChecker; // a help object for parsing
    e_lastfield:EStepField;   //to save the index of last field, which is found in reading the script

    t_tsteps:   TStringList;  //list of test steps without any useless char
    t_variables:TStringPairs;      //to save variables, name=value pair
    t_container:TStepContainer;     //a container to save steps
    a_fieldvals:FieldStringArray;   //an array to save field values of current test step
    s_curkey:   string;       //to save current key word, e.g. field name, variable name
    t_msgrimpl: TTextMessengerImpl;   //for delegation of interface ITextMessengerImpl

  protected
    procedure PushState(const state: EParseState);
    procedure PopState();
    procedure ClearStates();
    procedure ResetFieldValues();
    function  CheckFieldName(const name: string): boolean;
    function  CheckFieldValue(const val: string): boolean;
    function  CheckVariable(const name, value: string): boolean;
    function  CheckTestStep(): boolean;
    function  ReadChar(const curch, nextch: char): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    
    property VarContainer: TStringPairs read t_variables write t_variables;
    property StepContainer: TStepContainer read t_container write t_container;
    property FieldNameChecker: TFieldNameChecker read t_fnchecker;
    property FieldValueChecker: TFieldValueChecker read t_fvchecker;

    procedure Clear();
    function  ReadFromText(const srctext: string; const blast: boolean = false): boolean; virtual;
    function  ReadFromList(const srclist: TStringList; const bappend: boolean = false): boolean; virtual;
    function  ReadFromFile(const srcfile: string; const bforce: boolean = false; const bappend: boolean = false): boolean; virtual;
    function  SaveToFile(const destfile: string = ''): boolean;
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
  CCHR_TAB           = char(9);
  CCHR_LN            = char(10);
  CCHR_CR            = char(13);

  CSET_BLANK_CHARS: string = CCHR_TAB + CCHR_SPACE;
  CSET_FIRST_CHARS: string = '_ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  CSET_DIGIT_CHARS: string = '0123456789';

implementation
uses SysUtils, StrUtils, FuncBase, GenUtils;

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
  p_sentry^.e_field := t_sentry.e_field;
  t_states.Push(p_sentry);
  e_curstate := state;
  t_sentry.e_state := e_curstate;
  t_sentry.i_row := i_rowindex;
  t_sentry.i_col := i_colindex;
  t_sentry.e_field := e_lastfield;
end;

// =============================================================================
//    Description  : pop a EParseState from the state stack
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
  t_sentry.e_field := p_sentry^.e_field;
  e_curstate := t_sentry.e_state;
  dispose(p_sentry);
end;

// =============================================================================
//    Description  : clear the state stack and set current state with PS_IDLE
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.ClearStates();
begin
  while (t_states.Count > 0) do begin
    p_sentry := t_states.Pop();
    dispose(p_sentry);
  end;
  t_sentry.e_state := PS_IDLE;
  t_sentry.i_row := 1;
  t_sentry.i_col := 1;
  e_curstate := t_sentry.e_state;
  i_rowindex := t_sentry.i_row;
  i_colindex := t_sentry.i_col;
end;

// =============================================================================
//    Description  : set a_fieldvals with empty string
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.ResetFieldValues();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do a_fieldvals[i] := '';
end;

// =============================================================================
//    Description  : check if the field name is valid or if it is used more than
//                   once in one step.
//    Parameter    : name, field name
//    Return       : true, if the field name is valid.
//                   false, otherwise
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function  TScriptReader.CheckFieldName(const name: string): boolean;
begin
  result := t_fnchecker.IsNameValid(name, e_lastfield);
  if result then begin
    if (t_fnchecker.IsNameUsed(e_lastfield)) then begin
      t_msgrimpl.AddMessage(format('Dopplicated field name (%s).', [t_fnchecker.FieldName(e_lastfield)]),ML_ERROR);
      result := false
    end else  t_fnchecker.SetNameUsed(e_lastfield, true);
  end else t_msgrimpl.AddMessage(format('Invalid field name (%s).', [name]),ML_ERROR);
end;

// =============================================================================
//    Description  : check field value if it is grammatically valid
//    Parameter    : val, value of the corresponding field
//    Return       : true, if the value is valid
//                   false, otherwise
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function  TScriptReader.CheckFieldValue(const val: string): boolean;
begin
  result := t_fvchecker.CheckField(e_lastfield, val);
  if result then
    a_fieldvals[e_lastfield] := TGenUtils.ClearQuotationMarks(val)
  else
    t_msgrimpl.AddMessage(format('Invalid value (%s) for the field (%s).', [val, t_fnchecker.FieldName(e_lastfield)]), ML_ERROR);
end;

// =============================================================================
//    Description  : check if variable definition is valid, e.g. the name may not
//                   be dupplicated
//    Parameter    : name, name of the variable
//                   value, value of the variable in string
//    Return       : true, if the variable definition is valid
//                   false, otherwise
//    First author : 2016-05-11 /bsu/
//    History      :
// =============================================================================
function  TScriptReader.CheckVariable(const name, value: string): boolean;
begin
  result := false;
  if assigned(t_variables) then begin
    result := t_variables.AddPair(name, value);
    if (not result) then
      t_msgrimpl.AddMessage(format('Failed to input variable (name=%s), dupplicated?',[name]), ML_ERROR);
  end else
    t_msgrimpl.AddMessage('No container is given for variables', ML_ERROR);
end;

// =============================================================================
//    Description  : check if variable definition is valid, e.g. the name may not
//                   be dupplicated
//    Parameter    : name, name of the variable
//                   value, value of the variable in string
//    Return       : true, if the variable definition is valid
//                   false, otherwise
//    First author : 2016-05-11 /bsu/
//    History      :
// =============================================================================
function  TScriptReader.CheckTestStep(): boolean;
begin
  result := false;
  if assigned(t_container) then begin
    result := t_container.LoadStep(a_fieldvals);
    if result then begin
      t_sentry.i_row := i_rowindex; //update row and column for each idle state
      t_sentry.i_col := i_colindex;
      s_curtext := '';
      t_fnchecker.ResetUnused();
      ResetFieldValues();
    end else
      t_msgrimpl.AddMessage(format('Failed to input a test step (Nr=%s).',[a_fieldvals[SF_NR]]), ML_ERROR);
  end else
    t_msgrimpl.AddMessage('No container is given for test steps.', ML_ERROR);
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
const CSTR_UNEXCEPTED: string = 'Unexcepted char';
begin
//[PS_IDLE, PS_VARNAME, PS_VARVAL, PS_STEP, PS_FIELDKEY, PS_FIELDVAL, PS_SQUOTATION, PS_DQUOTATION, PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT, PS_FIELDGROUP]
  result := true;
  case curch of
  CCHR_EQUAL: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL, PS_FIELDVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_VARNAME) then begin
        PopState();
        s_curkey := trim(s_curtoken);
        s_curtext := s_curtext + s_curkey + curch;
        s_curtoken := '';
        PushState(PS_VARVAL);
      end else begin  //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDGROUP]
        result := false;
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
      end;
    end;
  end;
  CCHR_COLON: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_FIELDNAME) then begin
        s_curkey := trim(s_curtoken);
        if CheckFieldName(s_curkey) then begin
          PopState();
          s_curtext := s_curtext + s_curkey + curch;
          s_curtoken := '';
          PushState(PS_FIELDVAL);
        end else result := false; // error is already handled in CheckFieldName
      end else begin  //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDVAL, PS_FIELDGROUP]
        result := false;
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
      end;
    end;
  end;
  CCHR_SEMICOLON: begin
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARVAL]) then s_curtoken := s_curtoken + curch
      else if (e_curstate = PS_FIELDVAL) then begin
        if CheckFieldValue(trim(s_curtoken)) then begin
          PopState();
          s_curtext := s_curtext + a_fieldvals[e_lastfield] + curch;
          s_curtoken := '';
          if (e_curstate = PS_FIELDGROUP) then e_lastfield := t_sentry.e_field;
        end else  result := false; // error is already handled in CheckFieldValue
      end else if (e_curstate = PS_FIELDGROUP) then begin
        if trim(s_curtoken) = '' then begin
          s_curtext := s_curtext + curch;
          PopState(); PopState();
        end else begin
          result := false;
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
        end;
      end else begin //[PS_IDLE, PS_VARNAME, PS_STEP, PS_FIELDKEY]
        result := false;
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
            //b_allowvar := false;
          end else begin
            result := false;
            t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
          end;
        end else if (e_curstate = PS_FIELDVAL) then begin
          if (trim(s_curtoken) = '') then begin
            PushState(PS_FIELDGROUP);
            s_curtext := s_curtext + curch;
          end else s_curtoken := s_curtoken + curch;
        end else if (e_curstate = PS_VARVAL) then s_curtoken := s_curtoken + curch
        else begin  //[PS_VARNAME, PS_STEP, PS_FIELDKEY, PS_FIELDGROUP]
          result := false;
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
          s_curtext := s_curtext + a_fieldvals[e_lastfield] + curch;
          s_curtoken := '';
          if (e_curstate = PS_FIELDGROUP) then e_lastfield := t_sentry.e_field;
        end else begin
          result := false;
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
        end;
      end else if (e_curstate in [PS_FIELDGROUP, PS_STEP]) then begin
        PopState();
        s_curtext := s_curtext + trim(s_curtoken) + curch;
        s_curtoken := '';
        if (e_curstate = PS_IDLE) then begin//a step is over
          t_tsteps.Add(s_curtext);
          result := CheckTestStep();
        end else if (e_curstate <> PS_FIELDVAL) then begin
          result := false;
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
        end;
      end else begin   //[PS_IDLE, PS_VARNAME, PS_FIELDKEY]
        result := false;
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
      end;
    end;
  end;
  CCHR_LN, CCHR_CR: begin
    if (not (e_curstate in [PS_IDLE, PS_STEP, PS_FIELDGROUP, PS_BRACKETCOMMENT, PS_BRACECOMMENT])) then begin
      if (e_curstate = PS_LINECOMMENT) then PopState()
      else if (e_curstate = PS_VARVAL) then begin
        result := CheckVariable(s_curkey, s_curtoken);
        if result then begin
          s_curtext := s_curtext + trim(s_curtoken);
          s_curtoken := '';
          PopState();
          if (e_curstate = PS_IDLE) then begin//a step is over
            if (s_curtext <> '') then t_tsteps.Add(s_curtext);
            s_curtext := '';
          end;
        end;
      end else if (StartsText('HELP_', s_curtoken) and (e_curstate = PS_VARNAME)) then begin //ignore HELP-index directly after variable
        s_curtoken := '';
        PopState();
      end else begin  //[PS_VARNAME, PS_SQUOTATION, PS_DQUOTATION, PS_FIELDKEY, PS_FIELDVAL]
        result := false;
        t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
      end;
    end;
  end;
  else begin //other chars
    if (not (e_curstate in [PS_LINECOMMENT, PS_BRACECOMMENT, PS_BRACKETCOMMENT])) then begin
      if (e_curstate in [PS_SQUOTATION, PS_DQUOTATION, PS_VARNAME, PS_VARVAL, PS_FIELDNAME, PS_FIELDVAL]) then
        s_curtoken := s_curtoken + curch
      else begin //[PS_IDLE, PS_STEP, PS_FIELDGROUP]
        if ContainsText(CSET_FIRST_CHARS, curch) then begin
          if (e_curstate = PS_IDLE) then begin
            if (b_allowvar or StartsText(s_curtoken + curch + nextch, 'HELP_')) then begin //todo: what about the help id, e.g. HELP_000100??
              s_curtoken := s_curtoken + curch;
              PushState(PS_VARNAME);
            end else begin
              result := false;
              t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
            end;
          end else begin //[PS_STEP, PS_FIELDGROUP]
            s_curtoken := s_curtoken + curch;
            PushState(PS_FIELDNAME);
          end;
        end else if (not ContainsText(CSET_BLANK_CHARS, curch)) then begin
          result := false;
          t_msgrimpl.AddMessage(format('%s (%s).', [CSTR_UNEXCEPTED, curch]), ML_ERROR);
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
  p_ptinfo := TypeInfo(EParseState);
  t_sentry.e_state := e_curstate;
  t_sentry.i_row := 0;
  t_sentry.i_col := 0;
  t_srcfiles := TStringList.Create();
  b_allowvar := true;
  t_states := TStack.Create();
  t_tsteps := TStringList.Create();
  t_fnchecker := TFieldNameChecker.Create();
  t_fvchecker := TFieldValueChecker.Create();
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
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
  t_msgrimpl.Free();
  ClearStates();
  t_srcfiles.Free();
  SetLength(a_fstemps, 0);
  t_states.Free();
  t_tsteps.Free();
  t_fnchecker.Free();
  t_fvchecker.Free();
	inherited Destroy;
end;

// =============================================================================
//    Description  : clear all information and set all variables into initial state
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TScriptReader.Clear();
var i_vars, i_steps, i_cases: integer; b_info: boolean;
begin
  i_vars := 0; i_steps := 0; i_cases := 0;
  b_info := (t_tsteps.Count > 0);
  e_curstate := PS_IDLE;
  ClearStates();
  t_srcfiles.Clear();
  SetLength(a_fstemps, 0);
  s_curtext := '';
  s_curtoken := '';
  b_allowvar := true;
  t_fnchecker.ResetUnused();
  t_fvchecker.ResetChecker();
  t_tsteps.Clear();
  if assigned(t_variables) then begin
    i_vars := t_variables.Count;
    t_variables.Clear();
  end;
  if assigned(t_container) then begin
    i_steps := t_container.StepCount;
    i_cases := t_container.CaseGroup.CaseCount;
    t_container.Clear();
  end;
  if b_info then t_msgrimpl.AddMessage(format('All existing variables (%d), test steps (%d) in %d case(s) are unloaded.', [i_vars, i_steps, i_cases]));
end;

// =============================================================================
//    Description  : read test step from a line text
//    Parameter    : srctext, text of test script
//                   blast, indicate if this text is the last line
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
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
    if (not result) then t_msgrimpl.AddMessage(format('Script end: text=%s, state=%s', [s_curtext, GetEnumName(p_ptinfo, integer(e_curstate))]), ML_ERROR);
  end;
end;

// =============================================================================
//    Description  : read test steps from a string list
//    Parameter    : srclist, string list of test script
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptReader.ReadFromList(const srclist: TStringList; const bappend: boolean): boolean;
var i, i_vars, i_steps, i_cases: integer;
begin
  result := false;
  if bappend then begin
    if (t_tsteps.Count > 0) then t_msgrimpl.AddMessage('All variables and test steps from this script will be appended.');
  end else Clear();

  if srclist.Count > 0 then  begin
    i_rowindex := 1;
    for i := 0 to srclist.Count - 2 do begin //read in (n-1) lines, firstly
      result := ReadFromText(srclist[i], false);
      if (not result) then break;
      Inc(i_rowindex);
    end;
    if result then begin //read in the last line
      Inc(i_rowindex);
      result := ReadFromText(srclist[srclist.Count - 1], true);
    end;
    if result then begin
      t_container.UpdateCaseGroup();
      if assigned(t_variables) then i_vars := t_variables.Count
      else i_vars := 0;
      if assigned(t_container) then begin
        i_steps := t_container.StepCount;
        i_cases := t_container.CaseGroup.CaseCount;
      end else begin
        i_steps := 0;
        i_cases := 0;
      end;
      if (t_tsteps.Count > 0) then begin
        t_msgrimpl.AddMessage(format('%d variable(s) and %d step(s) in %d case(s) are now loaded.', [i_vars, i_steps, i_cases]), ML_EVER);
        if ((i_cases - 1) > CINT_CASE_INDEX_MAX) then t_msgrimpl.AddMessage(format('The count (%d) of test cases is over the limit (%d).', [i_cases, CINT_CASE_INDEX_MAX + 1]), ML_WARNING);
      end else t_msgrimpl.AddMessage('No valid line is loaded from the test script.', ML_WARNING);
    end;
  end else t_msgrimpl.AddMessage('Empty script.', ML_WARNING);
end;

// =============================================================================
//    Description  : read test steps from a script file. If the script is loaded
//                   currently and the file is not changed, the reading is depended
//                   on the second parameter
//    Parameter    : srcfile, file name of test script
//                   bforce, indicates if the loading is forced to be done
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptReader.ReadFromFile(const srcfile: string; const bforce: boolean; const bappend: boolean): boolean;
var t_lines: TStringList; b_update, b_append: boolean; t_fdatetime: TDateTime; i_idx: integer;
begin
  result := (FileExists(srcfile) and assigned(VarContainer) and assigned(StepContainer));
  if result then begin
    FileAge(srcfile, t_fdatetime);
    b_append := bappend;
    i_idx := t_srcfiles.IndexOf(srcfile);
    if (i_idx >= 0) then begin
      b_update := (t_fdatetime <> a_fstemps[i_idx]);
      b_append := false;
    end else b_update := true;

    if (bforce or b_update) then begin
      b_allowvar := true;
      t_lines := TStringList.Create();
      t_lines.LoadFromFile(srcfile);
      result := ReadFromList(t_lines, b_append);
      if result then begin
        t_srcfiles.Add(srcfile);
        SetLength(a_fstemps, t_srcfiles.Count);
        a_fstemps[t_srcfiles.Count - 1] := t_fdatetime;
      end else t_msgrimpl.AddMessage(format('Failed to read this file (%s)', [srcfile]), ML_ERROR);
      t_lines.Free();
    end else t_msgrimpl.AddMessage('This file is already loaded.', ML_INFO);
  end else begin
    if (not assigned(VarContainer)) then t_msgrimpl.AddMessage('The variable container is not given', ML_WARNING)
    else if (not assigned(StepContainer)) then t_msgrimpl.AddMessage('The step container is not given', ML_WARNING)
    else t_msgrimpl.AddMessage(format('This file is NOT found (%s).', [srcfile]), ML_WARNING);
  end;
end;

// =============================================================================
//    Description  : save clear script into a file
//    Parameter    : destfile, file name to save
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptReader.SaveToFile(const destfile: string): boolean;
var s_fname, s_ext: string;
begin
  result := false;
  s_fname := destfile;
  if ((destfile = '') and (t_srcfiles.Count > 0)) then begin
    if (length(t_srcfiles[0]) > 4) then begin
      s_ext := ExtractFileExt(t_srcfiles[0]);
      s_fname := LeftStr(t_srcfiles[0], length(t_srcfiles[0]) - length(s_ext)) + '_clear' + s_ext;
    end;
  end;
  if s_fname <> '' then begin
    t_tsteps.SaveToFile(s_fname);
    result := (t_tsteps.Count > 0);
  end;
  if result then begin
    if t_tsteps.Count> 1 then t_msgrimpl.AddMessage(format('%d lines are saved in "%s"', [t_tsteps.Count, s_fname]), ML_INFO)
    else t_msgrimpl.AddMessage(format('Only %d line is saved in "%s"', [t_tsteps.Count, s_fname]), ML_INFO);
  end else t_msgrimpl.AddMessage('No line is saved.', ML_WARNING);
end;

end.
