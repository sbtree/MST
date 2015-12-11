//==============================================================================
//==============================================================================
//
//     Module name       : $RCSfile: TestScript.pas,v $
//     Short description : Object for test script
//     Project           : MATS
//     Compiler          : Delphi 2007
//     First author      : 2014-09-05 /bsu/
//     Actual version    : $Revision   1.0 $
//     Copyright         : (c) Metronix GmbH, 2014
//------------------------------------------------------------------------------
//     History
//------------------------------------------------------------------------------
//     Descriptions      : A test must be written in test script, which defines
//                         a syntax of a test command. A test starts with a routine,
//                         in which any test cases and test commands can be called.
//                         A test case is composed of a set of test commands. It
//                         can be called by a test routine. A test command has to
//                         be written in one line. A test script has to be written
//                         in an ascii-text file and it looks like:
//
//                         // =============================================================
//                         // externe Liste für den ARS23xx
//                         // aktuelle Version: $Revision: 1.71 $
//                         //
//                         // =============================================================
//                         Language=EN                                                                                                                                                                                     
//                         FileVersion=1.0                                                                                                                                                                                 
//                         ScriptVersion=2.0                                                                                                                                                                               
//                         Include=.\TestCase100.txt;.\TestCase200.txt                                                                                                                                                     
//                                                                                                                                                                                                                         
//                         (Nr:[XXX.000]; T:'define a named test case'; INIT:'#Par[=default]';           FCT:TestCase; PAR:'MyTestCase'; M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[XXX.010]; T:'call a test instruction';  INIT:'RON:101,102 ROFF:119,120'; FCT:cmd;      PAR:'a b @Par';   M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)                                     
//                         (Nr:[XXX.990]; T:'end the named test case';  INIT:'';                         FCT:Nil;      PAR:'';           M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)                                     
//                                                                                                                                                                                                                         
//                         (Nr:[ZZZ.000]; T:'define a unnamed test case'; INIT:'';                         FCT:Nil;    PAR:'';           M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)                                     
//                         (Nr:[ZZZ.010]; T:'call a test instruction';    INIT:'RON:101,102 ROFF:119,120'; FCT:cmd;    PAR:'a b c';      M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)                                     
//                         (Nr:[ZZZ.990]; T:'end the unnamed test case';  INIT:'';                         FCT:Nil;    PAR:'';           M:''; FINAL:''; TOL: (A:''; MIN: 0; MAX: 0);)                                     
//                                                                                                                                                                                                                         
//                         (Nr:[YYY.000]; T:'Entry of a test routine'; INIT:'';                         FCT:Entry;        PAR:'MyTestRoutine';              M:'';    FINAL:'';                TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.010]; T:'call a named test case';  INIT:'RON:101,102 ROFF:111,122'; FCT:CallCase;     PAR:'MyTestCase';                 M:'';    FINAL:'[RESTORE:R,S]';   TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.020]; T:'Push a value into stack'; INIT:'';                         FCT:Push;         PAR:'123';                        M:'';    FINAL:'';                TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.030]; T:'call a unnamed test case';INIT:'RON:$SpecialRelay';        FCT:CallCase;     PAR:'TestCase_ZZZ';               M:'';    FINAL:'[RESTORE:R]';     TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.040]; T:'call a test command';     INIT:'RON:101,102 ROFF:111,122'; FCT:abc;          PAR:'@Pop';                       M:'V';   FINAL:'[RESTORE:R]';     TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.050]; T:'pop a value from stack';  INIT:'';                         FCT:Pop;          PAR:'#PopVar';                    M:'';    FINAL:'';                TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.060]; T:'close relays for 5000ms'; INIT:'';                         FCT:CloseRelay;   PAR:'101,102 5000';               M:'';    FINAL:'';                TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.070]; T:'push cmd result to stack';INIT:'RON:101,102 ROFF:111,122'; FCT:cmd;          PAR:'$INI_VAR';                   M:'mA';  FINAL:'RESTORE:R @Push]';TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.080]; T:'call case if true';       INIT:'RON:201 ROFF:202';         FCT:CallIfTrue;   PAR:'$INI_VAR=123 MyTestCase a';  M:'';    FINAL:'RESTORE:R @Push]';TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.080]; T:'call case if true';       INIT:'RON:201 ROFF:202';         FCT:CallIfTrue;   PAR:'@PopVar=123 MyTestCase abc'; M:'';    FINAL:'RESTORE:R @Push]';TOL: (A:''; MIN: 0; MAX: 0);)
//                         (Nr:[YYY.999]; T:'End of the routine';      INIT:'';                         FCT:Nil;          PAR:'';                           M:'';    FINAL:'[RESTORE:R,S]';   TOL: (A:''; MIN: 0; MAX: 0);)
//
//==============================================================================
//==============================================================================
unit TestScript;
interface
uses SysUtils, IniFiles, Contnrs, Classes;
type
  TScriptFile=class;

  TNameValueSet=class(THashedStringList)

  end;

  //define type of a line
  LineType = (  ltInvalid,
                ltComment,
                ltVariable,
                ltCommand );

  TSyntaxChecker=class
  type CheckerState = ( csNewState,   //empty stack, no expected word
                        csPredefinition,  //a predefinition like var = value (in ini-format)
                        csTestCommand,   //a command begins, but it is not finished to be parsed
                        csQuotation,     //a quetaion begins with ''' or '"'
                        csLineComment,  //a line comment begins with '//'
                        csBlockComment,   //a block comment starts with '(*' or '{' and end with '*)' or '}'
                        csUnknown  //an unknown state, which can be decided by next char
                       );
  protected
    t_states:   TStack;
    t_tslist:   TStringList;
    str_buf:    string;
    str_expects:string;
    e_curstate: CheckerState;
    e_pstate:   ^CheckerState;
    ch_last, ch_expect: Char;
  protected
    procedure PushState(const state: CheckerState);
    procedure PopState();
    procedure PushExpectedChar(const ch: char);
    procedure PopExpectedChar();
    procedure GetNextChar(const ch: char);
    procedure AcceptChar(const ch: char; unchar: boolean = true);

  public
    constructor Create();
    destructor Destroy; override;

    function SetOutList(var strlist: TStringList): boolean;
    function CheckLine(const line: string): boolean;
    function IsCompleted(): boolean;
  end;

  TScriptBlockBase=class
  protected
    t_vars: TNameValueSet;
    t_owner:TScriptFile;
  public
    constructor Create(owner: TScriptFile);
  end;

  TCaseBlock=class(TScriptBlockBase)

  end;

  TEntryBlock=class(TScriptBlockBase)

  end;

  TScriptFile=class
  protected
    str_filepath: string;
    t_vars:       TNameValueSet;
    t_tslist:     TStringList;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadScript(const filePath: string): boolean;
    function ReloadScript(): boolean;
    function SaveCleanScript(const filePath: string): boolean;

  end;

implementation
uses
  RegExpr, StrUtils;
const
  C_TSI_CMNT:  string = '//';
  C_TSI_BEGIN: string = '(';
  C_TSI_NR:    string = 'Nr';
  C_TSI_T:     string = 'T';
  C_TSI_INIT:  string = 'INIT';
  C_TSI_FCT:   string = 'FCT';
  C_TSI_PAR:   string = 'PAR';
  C_TSI_M:     string = 'M';
  C_TSI_FINAL: string = 'FINAL';
  C_TSI_TOL:   string = 'TOL';
  C_TSI_A:     string = 'A';
  C_TSI_MIN:   string = 'MIN';
  C_TSI_MAX:   string = 'MAX';
  C_TSI_END:   string = ')';

  //C_TSI_KEYS: array[0..10] of string = (C_TSI_NR, C_TSI_T, C_TSI_INIT, C_TSI_FCT, C_TSI_PAR, C_TSI_M, C_TSI_FINAL, C_TSI_TOL, C_TSI_MIN, C_TSI_MAX );

  C_REGEX_CMNT: string = '^//.*';
  C_REGEX_VAR:  string = '^([a-zA-Z][a-zA-Z0-9_]*)=(.*)';
  C_REGEX_CMD:  string = '^[\x20|\t]*\x28' +
                         '[\x20|\t]*Nr:[\x20|\t]*(\d{3,3}\.\d{2,3});' + //match[1] = step number
                         '[\x20|\t]*T:[\x20|\t]*\x27(.*)\x27;' + //match[2] = text of description
                         '[\x20|\t]*INIT:[\x20|\t]*\x27(.*)\x27;' + //match[3] = string of initialization
                         '[\x20|\t]*FCT:[\x20|\t]*(.*);' + //match[4] = name of command
                         '[\x20|\t]*PAR:[\x20|\t]*\x27(.*)\x27;' + //match[5] = string of parameter of command
                         '[\x20|\t]*M:[\x20|\t]*\x27(.*)\x27;' +  //match[6] = string of unit
                         '[\x20|\t]*FINAL:[\x20|\t]*\x27(.*)\x27;' + //match[7] = string of finalization
                         '[\x20|\t]*TOL:[\x20|\t]*\x28' +
                            '[\x20|\t]*A:[\x20|\t]*\x27(.*)\x27;' +  //match[8] = string of tolerance
                            '[\x20|\t]*MIN:[\x20|\t]*(\d*[.|,]{0,1}\d*);' + //match[9] = minimal value
                            '[\x20|\t]*MAX:[\x20|\t]*(\d*[.|,]{0,1}\d*)' + //match[10] = maximal value
                            '[\x20|\t]*\x29;' +
                         '[\x20|\t]*\x29';

const C_UNCHARS : set of char = [char(9),char(32)];
// =============================================================================
//    Description  : Constructor
//    Parameter    : owner, a parent object in type TScritptFile
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
constructor TScriptBlockBase.Create(owner: TScriptFile);
begin
	inherited Create;
  t_owner := owner;
end;

// =============================================================================
//    Description  : push a CheckerState into state stack
//    Parameter    : state, a CheckerState which is pushed into state stack
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.PushState(const state: CheckerState);
begin
  new(e_pstate);
  e_pstate^ := e_curstate;
  t_states.Push(e_pstate);
  e_curstate := state;
end;

// =============================================================================
//    Description  : pop a CheckerState from the state stack
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.PopState();
begin
  e_pstate := t_states.Pop();
  e_curstate := e_pstate^;
  dispose(e_pstate);
end;

// =============================================================================
//    Description  : push a expected char into char list
//    Parameter    : ch, a char which is pushed into the list
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.PushExpectedChar(const ch: char);
begin
  str_expects := str_expects + ch_expect;
  ch_expect := ch;
end;

// =============================================================================
//    Description  : pop a last char of the char list and remove it from the list
//    Parameter    : ch, a char which is pushed into the list
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.PopExpectedChar();
begin
  ch_expect := str_expects[length(str_expects)];
  str_expects := LeftStr(str_expects, length(str_expects) - 1);
end;

// =============================================================================
//    Description  : accept a char and append it to the buffer
//    Parameter    : ch, a char which is appended to the buffer
//                   filt, a blank char (' ' and tab) will be appended, if false
//                         otherwise the blank char will not be appended.
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.AcceptChar(const ch: char; unchar: boolean);
begin
  if not (unchar and (ch in C_UNCHARS) ) then str_buf := str_buf + string(ch);
end;

// =============================================================================
//    Description  : get next char. It is a kern function of analyse of syntax
//    Parameter    : ch, next char will be accepted
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
procedure TSyntaxChecker.GetNextChar(const ch: char);
begin
    case e_curstate of
    csNewState, csPredefinition, csTestCommand:
    begin
      if (ch = '{') then   //comment block started with '{'
      begin
        PushState(csBlockComment);
        PushExpectedChar('}');
      end
      else if (ch = '/') then
      begin
        AcceptChar(ch);
        PushState(csUnknown);
      end
      else if (ch = '(') then //command sentance started with '(Nr:' or a block comment beginning with '(*'
      begin
        AcceptChar(ch);
        PushState(csUnknown);
        PushExpectedChar(')');
      end
      else
      begin
        if (e_curstate = csNewState) then //special for csNewState
        begin
          if ((ch_last = ')') and ((ch = ',') or (ch = '.'))) then //a complete command sentance is over
          begin
            if ((t_states.Count() <= 0) and (length(str_expects) <= 0)) then
            begin
              if assigned(t_tslist) then t_tslist.Add(str_buf);
              str_buf := '';
            end;
          end
          else if (ch in ['A'..'Z', 'a'..'z']) then //predefinition starting with alphabet
          begin
            PushState(csPredefinition);
            AcceptChar(ch);
          end
          else AcceptChar(ch);
        end
        else if (e_curstate = csPredefinition) then AcceptChar(ch, false) //special for csPredefinition
        else  //special for csTestCommand
        begin
          AcceptChar(ch);
          if (ch = ch_expect) then
          begin
            PopExpectedChar();
            PopState();
          end
          else if (ch = ':') then
          begin
            PushState(csTestCommand);
            PushExpectedChar(';');
          end
          else if ((ch = '''') or (ch = '"')) then
          begin
            PushState(csQuotation);
            PushExpectedChar(ch);
          end;
          //else //todo: other case
        end;
      end;
    end;
    csLineComment: ;// PopState(); // this case is solved in caller-function
    csBlockComment:
    begin
      if (ch = ch_expect) then
      begin
        if ((ch_last = '*') and (ch = ')') or (ch = '}')) then
        begin
          PopExpectedChar();
          PopState();
        end;
      end
    end;
    csQuotation:
    begin
      AcceptChar(ch, false);
      if (ch = ch_expect) then
      begin
        PopExpectedChar();
        PopState();
      end
    end;
    csUnknown:
    begin
      if (ch_last = '(') then
      begin
        PopState();
        if (ch = ch_expect) then
        begin
          AcceptChar(ch);
          PopExpectedChar();
        end
        else if (ch = '*') then //a comment block beginning with '(*'
        begin
          PushState(csBlockComment);
          str_buf := LeftStr(str_buf, length(str_buf) - 1);
        end
        else
        begin
          if (e_curstate = csNewState) then
          begin
            PushState(csTestCommand);
            AcceptChar(ch);
          end
          else
          begin
            PushState(e_curstate);
            if (e_curstate = csPredefinition) then AcceptChar(ch, false)
            else AcceptChar(ch);
          end;
        end;
      end
      else if (ch_last = '/') then
      begin
        PopState();
        if (ch = '/') then //a comment line beginning with '//'
        begin
          PushState(csLineComment);
          str_buf := LeftStr(str_buf, length(str_buf) - 1);
        end
        else
        begin
          if (e_curstate = csPredefinition) then AcceptChar(ch, false)
          else AcceptChar(ch);
        end;
      end;
    end;
    end;
    ch_last := ch;
end;

// =============================================================================
//    Description  : constructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
constructor TSyntaxChecker.Create();
begin
	inherited Create;
  e_curstate := csNewState;
  t_states := TStack.Create();
  str_expects := '';
  str_buf := '';
  ch_expect := char(0);
end;

// =============================================================================
//    Description  : destructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
destructor TSyntaxChecker.Destroy();
begin
	inherited Destroy;
  while(t_states.Count > 0) do PopState();
  t_states.Free();
end;

// =============================================================================
//    Description  : set a string list, in which all valid lines are exported
//    Parameter    : strlist, a string list for export
//    Return       : true if strlist is assigned, otherwise false
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TSyntaxChecker.SetOutList(var strlist: TStringList): boolean;
begin
  result := false;
  if assigned(strlist) then
  begin
    t_tslist := strlist;
    t_tslist.Clear();
    result := true;
  end;
end;

// =============================================================================
//    Description  : check a line
//    Parameter    : line, a line of string from file
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TSyntaxChecker.CheckLine(const line: string): boolean;
var str_curline: string; i: integer;
begin
  result := true;
  str_curline := trim(line);
  for i := 1 to length(str_curline) do
  begin
    GetNextChar(char(str_curline[i]));
    if e_curstate = csLineComment then
    begin
      PopState();
      break;
    end
  end;

  if (e_curstate = csPredefinition) then //predefinition is allowed only in one line
  begin
    PopState();
    if ((t_states.Count() <= 0) and (length(str_expects) <= 0)) then
    begin
      if assigned(t_tslist) then t_tslist.Add(trim(str_buf));
      str_buf := '';
    end
    else result := false;
  end
  else if ((e_curstate = csQuotation) and (str_buf <> '')) then result := false;  //a quotation is allowed in one line
end;

// =============================================================================
//    Description  : check, ob all lines are successfully analyzed
//    Parameter    : --
//    Return       : true if all lines are successfully analysed, otherwise false
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TSyntaxChecker.IsCompleted(): boolean;
begin
  result := ((e_curstate = csNewState) and (length(str_buf) = 0));
end;

// =============================================================================
//    Description  : constructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
constructor TScriptFile.Create;
begin
	inherited Create;
  t_tslist := TStringList.Create();
end;

// =============================================================================
//    Description  : destructor
//    Parameter    : --
//    Return       : --
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
destructor TScriptFile.Destroy;
begin
  t_tslist.Clear();
  t_tslist.Free();
	inherited Destroy;
end;

// =============================================================================
//    Description  : load a script file
//    Parameter    : filePath, a full path of a script file
//    Return       : true if the file has no syntax error, otherwise false
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptFile.LoadScript(const filePath: string): boolean;
var strlines: TStringList; t_checker: TSyntaxChecker; i: integer;
begin
  t_checker := TSyntaxChecker.Create();
  t_checker.SetOutList(t_tslist);
  strlines := TStringList.Create();
  strlines.LoadFromFile(filePath);
  for i := 0 to strlines.Count - 1 do
  begin
    if (not t_checker.CheckLine(strlines.Strings[i])) then break;
  end;
  result := t_checker.IsCompleted();
  if (not result) then t_tslist.Add('Error: ' + t_checker.str_buf);  
  strlines.Clear();
  strlines.Free();
  t_checker.Free();
end;

// =============================================================================
//    Description  : reload last script file which is loaded
//    Parameter    : --
//    Return       : true if the file has no syntax error, otherwise false
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptFile.ReloadScript(): boolean;
begin
  result := LoadScript(str_filepath);
end;

// =============================================================================
//    Description  : save all lines into a file, which are already analyzed till now
//    Parameter    : filePath, a full path of a script file to save
//    Return       : true if valid lines is greater than zero
//    First author : 2014-08-27 /bsu/
//    History      :
// =============================================================================
function TScriptFile.SaveCleanScript(const filePath: string): boolean;
begin
  result := t_tslist.Count > 0;
  if result then t_tslist.SaveToFile(filePath);
end;

end.
