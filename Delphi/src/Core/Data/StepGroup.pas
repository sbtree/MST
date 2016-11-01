// =============================================================================
// Module name  : $RCSfile: StepGroup.pas,v $
// Description  : This unit defines different groups of test steps, in order to
//                to manage all steps, which are read from a script.
//                script is composed of variable-value pairs and test steps.
// looks like:
//                |--test step--------------------------------------------------------------------------------------------------|
//                (Nr:10.00; T:'beginning of test case 1'; R_on:'101'; Fkt:nil; M:''; Par:''; R_off:''; Tol:(A:''; Min:0;Max:0);),
//                |[Field Name]:[Field Value]|-------------------------------------------------------------------------------------------|
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

unit StepGroup;

interface
uses Classes, Contnrs, StepData, TextMessage;

type
  TIndexSet = set of byte;

  IStepNavigator = interface
    function GetSteps(): integer;
    function GetFirstStep(): TTestStep;
    function GetPrevStep(): TTestStep;
    function GetCurStep(): TTestStep;
    function GetNextStep(): TTestStep;
    function GetLastStep(): TTestStep;
    function GetRelativeStep(const relidx: integer): TTestStep;
    function MoveStepIndex(const relidx: integer): boolean;
    function GotoStepIndex(const idx: integer): boolean;
    function GotoStepNr(var stepnr: string): boolean;

    property CurrentStep: TTestStep read GetCurStep;
    property PreviousStep: TTestStep read GetPrevStep;
    property NextStep: TTestStep read GetNextStep;
    property FirstStep: TTestStep read GetFirstStep;
    property LastStep: TTestStep read GetLastStep;
    property StepCount: integer read GetSteps;
  end;

  //a class representing a step group, which is composed of test steps in the container
  TStepGroup = class(TInterfacedObject, IStepNavigator, ITextMessengerImpl)
  class function CalcMainNr(const stepnr: string; var casenr: integer): boolean;
  protected
    t_steps:    TStrings;       //a list of test steps (step number and object pairs)
    i_curstep:  integer;        //indicate current index of t_steps
    i_gotoidx:  integer;        //indicate index for goto-query
    b_freestep: boolean;        //indicates if free step object by removing
    t_msgrimpl:TTextMessengerImpl;

  protected
    function GetStepNumbers(): string;
    function IndexOfStepNr(var stepnr: string): integer;
    procedure RemoveStepByIndex(const idx: integer);
    procedure SetFreeByRemove(const bfree: boolean); virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    //basic methods of the interface
    function GetSteps(): integer;
    function GetFirstStep(): TTestStep;
    function GetPrevStep(): TTestStep;
    function GetCurStep(): TTestStep; virtual;
    function GetNextStep(): TTestStep;
    function GetLastStep(): TTestStep;
    function GetRelativeStep(const relidx: integer): TTestStep;
    function MoveStepIndex(const relidx: integer): boolean;
    function GotoStepIndex(const idx: integer): boolean;
    function GotoStepNr(var stepnr: string): boolean;
    function SetGotoQuery(const idx: integer): boolean;

    //properties
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property CurrentStep: TTestStep read GetCurStep;
    property PreviousStep: TTestStep read GetPrevStep;
    property NextStep: TTestStep read GetNextStep;
    property FirstStep: TTestStep read GetFirstStep;
    property LastStep: TTestStep read GetLastStep;
    property StepCount: integer read GetSteps;
    property StepList: TStrings read t_steps;
    property StepNumbers: string read GetStepNumbers;
    property FreeByRemove: boolean read b_freestep write SetFreeByRemove;

    //additional methods
    function AddStep(const step: TTestStep): boolean; virtual;
    function StepByIndex(const idx: integer): TTestStep;
    function StepByNr(var stepnr: string): TTestStep;
    function IndexOfStep(var stepnr: string): integer;
    function StepNrOf(const idx: integer): string;
    function CurStepNr(): string;
    function CurStepIndex(): integer;
    procedure RemoveStep(var stepnr: string);
    procedure Clear(); virtual;
  end;

  //a class representing a set of test steps in the container (continue from an index to another),
  //e.g. a test case, which have the same main number in field Nr (xxx of xxx.yyy)
  TTestCase = class(TStepGroup)
  protected
    i_casenr: integer;    //number of the group, e.g. test case number
    s_title:  string;     //title of this group

  public
    constructor Create();
    destructor Destroy(); override;

    //properties
    property CaseNr: integer read i_casenr write i_casenr;
    property CaseTitle: string read s_title write s_title;

    //reimplemented methods
    function AddStep(const step: TTestStep): boolean; override;
  end;

  ICaseNavigator = interface
    function GetCases(): integer;
    function GetFirstCase(): TTestCase;
    function GetPrevCase(): TTestCase;
    function GetCurCase(): TTestCase;
    function GetNextCase(): TTestCase;
    function GetLastCase(): TTestCase;
    function GetRelativeCase(const relidx: integer): TTestCase;
    function GotoCaseIndex(const caseidx: integer): boolean;
    function GotoCaseNr(const casenr: string): boolean;

    property CurrentCase: TTestCase read GetCurCase;
    property PreviousCase: TTestCase read GetPrevCase;
    property NextCase: TTestCase read GetNextCase;
    property FirstCase: TTestCase read GetFirstCase;
    property LastCase: TTestCase read GetLastCase;
    property CaseCount: integer read GetCases;
  end;

  TCaseGroup = class(TInterfacedObject, ICaseNavigator)
  protected
    t_cases:    TStrings; //a list of test cases (case number and object pairs)
    i_curcase:  integer;  //index, indicates current case in t_cases
    b_freecase: boolean;  //indicates if free object by removing

  protected
    function GetCaseNumbers(): string;
    procedure RemoveCaseByIndex(const idx: integer); virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    //basic methods of the interface
    function GetCases(): integer;
    function GetFirstCase(): TTestCase;
    function GetPrevCase(): TTestCase;
    function GetCurCase(): TTestCase;
    function GetNextCase(): TTestCase;
    function GetLastCase(): TTestCase;
    function GetRelativeCase(const relidx: integer): TTestCase;
    function GotoCaseIndex(const caseidx: integer): boolean;
    function GotoCaseNr(const casenr: string): boolean; overload;
    function GotoCaseNr(const casenr: integer): boolean; overload;

    //properties
    property CurrentCase: TTestCase read GetCurCase;
    property PreviousCase: TTestCase read GetPrevCase;
    property NextCase: TTestCase read GetNextCase;
    property FirstCase: TTestCase read GetFirstCase;
    property LastCase: TTestCase read GetLastCase;
    property CaseCount: integer read GetCases;
    property CaseList: TStrings read t_cases;
    property CaseNumbers: string read GetCaseNumbers;
    property FreeByRemove: boolean read b_freecase write b_freecase;

    //additional methods
    function AddCase(const tcase: TTestCase): boolean;
    function CaseByIndex(const idx: integer): TTestCase;
    function CaseByNr(const casenr: string): TTestCase; overload;
    function CaseByNr(const nr: integer): TTestCase; overload;
    function IndexOfCase(const casenr: string): integer;
    function CaseNrOf(const idx: integer): string;
    procedure UpdateCaseIndex(const casenr: integer);
    procedure RemoveCase(const casenr: string);
    procedure Clear(); virtual;
  end;

  //a class representing a test sequence, which is composed of test groups in the container
  TTestSequence = class(TStepGroup, ICaseNavigator)
  protected
    t_casegrp:  TCaseGroup; //an object of TCaseGroup, to save test cases
    i_lastcnr:  integer;    //to save last case number during reading test script

  protected
    procedure UpdateCaseIndex(const tstep: TTestStep);
    procedure UpdateCaseByStep(const tstep: TTestStep);
    procedure UpdateStepsByCase(const tcase: TTestCase);
    procedure SetFreeByRemove(const bfree: boolean); override;

  public
    constructor Create();
    destructor Destroy(); override;

    //reimpleemnted methods (interface)
    function GetCurStep(): TTestStep; override;

    //properties
    property CaseGroup: TCaseGroup read t_casegrp implements ICaseNavigator;

    //additional methods
    function UpdateCaseGroup(): integer; overload;
    function UpdateCaseGroup(const caseset: TIndexSet; const tseq: TTestSequence): integer; overload;
    procedure Clear(); override;
  end;

  //a class composed of all test steps
  TStepContainer = class(TTestSequence)
  protected
    function CaseIndexSet(const casenrs: string): TIndexSet;

  public
    constructor Create();
    destructor  Destroy(); override;

    function LoadStep(const fields: FieldStringArray): boolean;
    function CreateSequence(const incl: string = 'all'; const excl: string = ''): TTestSequence;
    function UpdateSequence(var tseq: TTestSequence; const incl: string = 'all'; const excl: string = ''): boolean;
    procedure SaveFile(const sfile: string);
  end;

const
  CINT_CASE_INDEX_MAX : integer = 255;

implementation
uses SysUtils, StrUtils, Variants;

class function TStepGroup.CalcMainNr(const stepnr: string; var casenr: integer): boolean;
var t_nrs: TStrings;
begin
  result := false;
  if (trim(stepnr) <> '') then begin
    t_nrs := TStringList.Create();
    if (ExtractStrings(['.'], [' '], PChar(stepnr), t_nrs) > 0) then begin
      result := TryStrToInt(t_nrs[0], casenr);
      if result then casenr := abs(casenr);
    end;
    FreeAndNil(t_nrs);
  end;
end;

function TStepGroup.GetStepNumbers(): string;
begin
  result := t_steps.DelimitedText;
end;

// =============================================================================
//    Description  : return index of the test step with number stepnr
//                   stepnr will be replaced if an equivalent number is found
//                   NOTE: equivalent number means any zeros are alllowed at end
//                   of the sub step number till the third digital after point, e.g.
//                   'xx'='xx.'='xx.0'='xx.00'='xx.000' or 'xx.y'='xx.y0'='x.y00'
//    Parameter    : stepnr, input and output parameter, the number of test step
//                   which is expected. It will be replaced if its equivalent
//                   number is found in the step list.
//    Return       : integer, index of the step with the given number if it's found.
//                   -1 is returned if neither the step number nor its equivalent
//                   number is found.
//    First author : 2016-09-08 /bsu/
//    History      :
// =============================================================================
function TStepGroup.IndexOfStepNr(var stepnr: string): integer;
var i, i_len, i_pos: integer; s_snr: string;
begin
  s_snr := stepnr;
  result := t_steps.IndexOf(s_snr);
  if (result < 0) then begin
    i_pos := Pos('.', s_snr);
    if (i_pos > 0) then begin
      i_len := length(s_snr);
      while (i_len > 0) do begin
        if (s_snr[i_len] = '0') then begin //remove last '0' in sub number
          Dec(i_len);
          s_snr := LeftStr(s_snr, i_len);
          result := t_steps.IndexOf(s_snr);
          if (result >= 0) then break;
        end else break;
      end;
      if (result < 0) then begin
        s_snr := stepnr;
        i_pos := length(s_snr) - i_pos + 1;
      end;
    end else begin
      s_snr := s_snr + '.';
      i_pos := 1;
    end;

    if (result < 0) then begin
      for i := i_pos to 3 do begin  //append '0' and search it in the list 
        s_snr := s_snr + '0';
        result := t_steps.IndexOf(s_snr);
        if (result >= 0) then break;
      end;
    end;
    if (result >= 0) then t_msgrimpl.AddMessage(format('An equivalent step number(%s) is found for the given step number(%s)', [s_snr, stepnr]));
  end;
  if (result < 0) then begin
    t_msgrimpl.AddMessage(format('No step is found for the given step number(%s)', [stepnr]), ML_ERROR);
  end else stepnr := s_snr; //return the equivalent step number
end;

procedure TStepGroup.RemoveStepByIndex(const idx: integer);
var t_step: TTestStep;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then begin
    t_step := StepByIndex(idx);
    if (assigned(t_step) and b_freestep) then t_step.Free();
    t_steps.Delete(idx);
    if (i_curstep > t_steps.Count) then i_curstep := t_steps.Count;
  end;
end;

procedure TStepGroup.SetFreeByRemove(const bfree: boolean);
begin
  b_freestep := bfree;
end;

constructor TStepGroup.Create();
begin
  inherited Create();
  i_curstep := -1;
  i_gotoidx := -1;
  b_freestep := false;
  t_steps := TStringList.Create();
  t_steps.Delimiter := Char(';');
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

destructor TStepGroup.Destroy();
begin
  Clear();
  t_msgrimpl.Free();
  t_steps.Free();
  inherited Destroy();
end;

function TStepGroup.GetSteps(): integer;
begin
  result := t_steps.Count;
end;

function  TStepGroup.GetFirstStep(): TTestStep;
begin
  if (t_steps.Count > 0) then i_curstep := 0
  else i_curstep := -1;
  result := GetCurStep();
end;

function  TStepGroup.GetPrevStep(): TTestStep;
begin
  dec(i_curstep);
  if (i_curstep < 0) then i_curstep := -1;
  result := GetCurStep()
end;

function  TStepGroup.GetCurStep(): TTestStep;
begin
  result := StepByIndex(i_curstep);
end;

function  TStepGroup.GetNextStep(): TTestStep;
begin
  if i_gotoidx >= 0 then begin //if goto-query is set
    i_curstep := i_gotoidx;
    i_gotoidx := -1;
  end else begin               //go to next step normally
    inc(i_curstep);
    if (i_curstep >= t_steps.Count) then i_curstep := t_steps.Count;
  end;
  result := GetCurStep()
end;

function  TStepGroup.GetLastStep(): TTestStep;
begin
  i_curstep := t_steps.Count - 1;
  result := GetCurStep();
end;

function  TStepGroup.GetRelativeStep(const relidx: integer): TTestStep;
begin
  MoveStepIndex(relidx);
  result := GetCurStep();
end;

function TStepGroup.MoveStepIndex(const relidx: integer): boolean;
begin
  result := GotoStepIndex(i_curstep + relidx);
end;

function TStepGroup.GotoStepIndex(const idx: integer): boolean;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then begin
    i_curstep := idx;
    result := true;
  end else result := false;
end;

function TStepGroup.GotoStepNr(var stepnr: string): boolean;
var i_idx: integer;
begin
  i_idx := IndexOfStepNr(stepnr);
  result := GotoStepIndex(i_idx);
  if result then t_msgrimpl.AddMessage(format('Successful to go to step %s', [stepnr]))
  else t_msgrimpl.AddMessage(format('Failed to go to step %s', [stepnr]), ML_ERROR)
end;

//set index of goto-query (i_gotoidx), the execution will be done through NextStep
//GotoStepIndex = SetGotoQuery + NextStep.
//This function is useful for the interface IStepControlImpl
function TStepGroup.SetGotoQuery(const idx: integer): boolean;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then begin
    i_gotoidx := idx;
    result := true;
  end else result := false;
end;

function TStepGroup.AddStep(const step: TTestStep): boolean;
var s_stepnr: string;
begin
  result := false;
  if assigned(step) then begin
    s_stepnr := step.GetFieldValue(SF_NR);
    if (t_steps.IndexOf(s_stepnr) < 0) then
      result := (t_steps.AddObject(s_stepnr, step) >= 0);
  end;
end;

function  TStepGroup.StepByIndex(const idx: integer): TTestStep;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then result := TTestStep(t_steps.Objects[idx])
  else result := nil;
end;

function  TStepGroup.StepByNr(var stepnr: string): TTestStep;
var i_idx: integer;
begin
  i_idx := IndexOfStepNr(stepnr);
  result := StepByIndex(i_idx);
end;

function  TStepGroup.IndexOfStep(var stepnr: string): integer;
begin
  result := IndexOfStepNr(stepnr);
end;

function  TStepGroup.StepNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_steps.Count)) then result := t_steps.Strings[idx]
  else result := '';
end;

function TStepGroup.CurStepNr(): string;
begin
  result := StepNrOf(i_curstep);
end;

function TStepGroup.CurStepIndex(): integer;
begin
  result := i_curstep;
end;

procedure TStepGroup.RemoveStep(var stepnr: string);
var i_idx: integer;
begin
  i_idx := IndexOfStepNr(stepnr);
  RemoveStepByIndex(i_idx);
end;

procedure TStepGroup.Clear();
var i_idx: integer;
begin
  i_curstep := -1;
  i_idx := t_steps.Count - 1;
  while (i_idx >= 0) do begin
    RemoveStepByIndex(i_idx);
    i_idx := t_steps.Count - 1;
  end;
end;

constructor TTestCase.Create();
begin
  inherited Create();
  i_casenr := -1;
  s_title := '';
end;

destructor TTestCase.Destroy();
begin
  inherited Destroy();
end;

function TTestCase.AddStep(const step: TTestStep): boolean;
var s_stepnr: string; i_cnr: integer;
begin
  result := false;
  if assigned(step) then begin
    s_stepnr := step.GetFieldValue(SF_NR);
    if TStepGroup.CalcMainNr(s_stepnr, i_cnr) then begin
      //add first step in this test case
      if (t_steps.Count <= 0) then begin
        result := (t_steps.AddObject(s_stepnr, step) >= 0);
        i_casenr := i_cnr;
        if s_title = '' then s_title := step.GetFieldValue(SF_T)
      end else begin
        //only the step which has same case number is allowed
        if (i_cnr = i_casenr) then
          result := (t_steps.AddObject(step.GetFieldValue(SF_NR), step) >= 0);
      end;
    end;
  end;
end;

function TCaseGroup.GetCaseNumbers(): string;
begin
  result := t_cases.DelimitedText;
end;

procedure TCaseGroup.RemoveCaseByIndex(const idx: integer);
var t_case: TTestCase;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then begin
    t_case := TTestCase(t_cases.Objects[idx]);
    if (b_freecase and assigned(t_case)) then t_case.Free();
    t_cases.Delete(idx);
  end;
end;

constructor TCaseGroup.Create();
begin
  inherited Create();
  i_curcase := -1;
  b_freecase := false;
  t_cases := TStringList.Create();
  t_cases.Delimiter := Char(';');
end;

destructor TCaseGroup.Destroy();
begin
  Clear();
  t_cases.Free();
  inherited Destroy();
end;

function TCaseGroup.GetCases(): integer;
begin
  result := t_cases.Count;
end;

function TCaseGroup.GetFirstCase(): TTestCase;
begin
  if (t_cases.Count > 0) then i_curcase := 0
  else i_curcase := -1;
  result := GetCurCase();
end;

function TCaseGroup.GetPrevCase(): TTestCase;
begin
  dec(i_curcase);
  if (i_curcase < 0) then i_curcase := -1;
  result := GetCurCase()
end;

function TCaseGroup.GetCurCase(): TTestCase;
begin
  result := CaseByIndex(i_curcase);
end;

function TCaseGroup.GetNextCase(): TTestCase;
begin
  inc(i_curcase);
  if (i_curcase >= t_cases.Count) then i_curcase := t_cases.Count;
  result := GetCurCase()
end;

function TCaseGroup.GetLastCase(): TTestCase;
begin
  i_curcase := t_cases.Count - 1;
  result := GetCurCase();
end;

function TCaseGroup.GetRelativeCase(const relidx: integer): TTestCase;
begin
  i_curcase := i_curcase + relidx;
  if (i_curcase < 0) then i_curcase := -1
  else if (i_curcase >= t_cases.Count) then i_curcase := t_cases.Count;
  result := GetCurCase();
end;

function TCaseGroup.GotoCaseIndex(const caseidx: integer): boolean;
begin
  result := ((caseidx >= 0) and (caseidx < t_cases.Count));
  if result then i_curcase := caseidx;
end;

function TCaseGroup.GotoCaseNr(const casenr: string): boolean;
var i_caseidx, i_casenr: integer;
begin
  if TryStrToInt(casenr, i_casenr) then i_caseidx := t_cases.IndexOf(IntToStr(i_casenr))
  else i_caseidx := t_cases.IndexOf(casenr);
  result := GotoCaseIndex(i_caseidx);
end;

function TCaseGroup.GotoCaseNr(const casenr: integer): boolean;
var s_cnr: string; i_idx: integer;
begin
  s_cnr := IntToStr(casenr);
  i_idx := t_cases.IndexOf(s_cnr);
  result := (i_idx >= 0);
  if result then i_curcase := i_idx;
end;

function TCaseGroup.AddCase(const tcase: TTestCase): boolean;
var s_casenr: string;
begin
  result := false;
  if assigned(tcase) then begin
    s_casenr := IntToStr(tcase.CaseNr);
    if (t_cases.IndexOf(s_casenr) < 0) then
      result := (t_cases.AddObject(s_casenr, tcase) >= 0);
  end;
end;

function  TCaseGroup.CaseByIndex(const idx: integer): TTestCase;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then result := TTestCase(t_cases.Objects[idx])
  else result := nil;
end;

function  TCaseGroup.CaseByNr(const casenr: string): TTestCase;
var i_caseidx, i_casenr: integer;
begin
  if TryStrToInt(casenr, i_casenr) then i_caseidx := t_cases.IndexOf(IntToStr(i_casenr)) //remove prefix zeros, e.g. '010'=>'10'
  else i_caseidx := t_cases.IndexOf(casenr);
  result := CaseByIndex(i_caseidx);
end;

function  TCaseGroup.CaseByNr(const nr: integer): TTestCase;
var s_casenr: string;
begin
  s_casenr := IntToStr(nr);
  result := CaseByNr(s_casenr);
end;

function  TCaseGroup.IndexOfCase(const casenr: string): integer;
begin
  result := t_cases.IndexOf(casenr);
end;

function  TCaseGroup.CaseNrOf(const idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_cases.Count)) then result := t_cases.Strings[idx]
  else result := '';
end;

procedure TCaseGroup.UpdateCaseIndex(const casenr: integer);
var s_cnr: string; i_idx: integer;
begin
  s_cnr := IntToStr(casenr);
  i_idx := t_cases.IndexOf(s_cnr);
  if (i_idx >= 0) then i_curcase := i_idx;
end;

procedure TCaseGroup.RemoveCase(const casenr: string);
var i_idx: integer;
begin
  i_idx := t_cases.IndexOf(casenr);
  RemoveCaseByIndex(i_idx);
end;

procedure TCaseGroup.Clear();
var i_idx: integer;
begin
  i_idx := t_cases.Count - 1;
  while (i_idx >= 0) do begin
    RemoveCaseByIndex(i_idx);
    i_idx := t_cases.Count - 1;
  end;
  i_curcase := -1;
end;

constructor TTestSequence.Create();
begin
  inherited Create();
  t_casegrp := TCaseGroup.Create();
end;

destructor TTestSequence.Destroy();
begin
  Clear();
  t_casegrp.Free();
  inherited Destroy();
end;

function TTestSequence.GetCurStep(): TTestStep;
begin
  result := inherited GetCurStep();
  UpdateCaseIndex(result);
end;

function TTestSequence.UpdateCaseGroup(): integer;
var i: integer;
begin
  t_casegrp.Clear();
  for i := 0 to StepCount - 1 do
    UpdateCaseByStep(StepByIndex(i));
  result := t_casegrp.CaseCount;
end;

function TTestSequence.UpdateCaseGroup(const caseset: TIndexSet; const tseq: TTestSequence): integer;
var i: integer;
begin
  if (assigned(tseq) and (tseq <> self)) then begin
    Clear();
    FreeByRemove := false;
    if (tseq.CaseGroup.CaseCount <= 0) then tseq.UpdateCaseGroup();
    for i := 0 to tseq.CaseGroup.CaseCount - 1 do
      if (i in caseset) then UpdateStepsByCase(tseq.CaseGroup.CaseByIndex(i));
  end;
  result := t_casegrp.CaseCount;
end;

procedure TTestSequence.Clear();
begin
  inherited Clear();
  t_casegrp.Clear();
  i_lastcnr := -1;
end;

procedure TTestSequence.UpdateCaseIndex(const tstep: TTestStep);
var s_stepnr: string; i_cnr: integer;
begin
  if assigned(tstep) then begin
    s_stepnr := tstep.GetFieldValue(SF_NR);
    if TStepGroup.CalcMainNr(s_stepnr, i_cnr) then
      t_casegrp.GotoCaseNr(i_cnr);
  end;
end;

procedure  TTestSequence.UpdateCaseByStep(const tstep: TTestStep);
var t_tcase: TTestCase; i_casenr: integer; s_stepnr, s_casenr: string;
begin
  if assigned(tstep) then begin
    s_stepnr := tstep.GetFieldValue(SF_NR);
    if TStepGroup.CalcMainNr(s_stepnr, i_casenr) then begin
      s_casenr := IntToStr(i_casenr);
      if (i_casenr = i_lastcnr) then begin //update indexto of last case if the case number is same as last one
        t_tcase := t_casegrp.CaseByNr(i_casenr);
        if assigned(t_tcase) then t_tcase.AddStep(tstep);
      end else begin  //create a new case if the case number differs from last one
        t_tcase := TTestCase.Create();
        t_tcase.CaseNr := i_casenr;
        if (t_casegrp.AddCase(t_tcase)) then t_tcase.AddStep(tstep)
        else FreeAndNil(t_tcase);
        i_lastcnr := i_casenr;
      end;
    end;
  end;
end;

procedure TTestSequence.UpdateStepsByCase(const tcase: TTestCase);
var i: integer;
begin
  if assigned(tcase) then begin
    if t_casegrp.AddCase(tcase) then begin
      for i := 0 to tcase.StepCount - 1 do
        AddStep(tcase.StepByIndex(i));
    end;
  end;
end;

procedure TTestSequence.SetFreeByRemove(const bfree: boolean);
begin
  inherited SetFreeByRemove(bfree);
  t_casegrp.FreeByRemove := bfree;
end;
// =============================================================================
//    Description  : analyze the given string casenrs and return back a list of
//                   valid test case index in t_casenrs.
//    Parameter    : casenrs, a string to represent test cases, e.g.:
//                   1. 'all' - all test case in the container
//                   2. '11-201' - test cases from 11 to 201
//                   3. '110, 220, 360' - test cases of 110, 220 and 360
//                   4. '28-400, 520, 650' - combination of 2. and 3.
//    Return       : a list of test case index in t_casenrs, which is a subset of test cases
//                   in the container, subset(incl) - subset(excl)
//    First author : 2016-05-10 /bsu/
//                   WARNING: because of using set in this function the count of
//                   test cases is not allowed beyond CINT_CASES_MAX + 1 (256)
//    History      :
// =============================================================================
function  TStepContainer.CaseIndexSet(const casenrs: string): TIndexSet;
var i, i_maxidx: integer; t_cstrs, t_cnrs: TStrings;
    i_cnr, i_cnr1, i_cnr2, i_idx: integer; s_cnr: string;
begin
  result := [];
  if ((t_casegrp.CaseCount - 1) > CINT_CASE_INDEX_MAX) then i_maxidx := CINT_CASE_INDEX_MAX
  else i_maxidx := t_casegrp.CaseCount - 1;

  if SameText(casenrs, 'all') then begin
    for i := 0 to i_maxidx do Include(result, i);
  end else  begin
    if (casenrs <>'') then begin
      t_cstrs := TStringList.Create();
      if (ExtractStrings([','], [' '], PChar(casenrs), t_cstrs) > 0) then begin
        t_cnrs := TStringList.Create();
        for i := 0 to t_cstrs.Count - 1 do begin
          t_cnrs.Clear();
          if (ExtractStrings(['-'], [' '], PChar(t_cstrs[i]), t_cnrs) > 1) then begin
            if (TryStrToInt(t_cnrs[0], i_cnr1) and TryStrToInt(t_cnrs[t_cnrs.Count-1], i_cnr2)) then begin
              for i_cnr := i_cnr1 to i_cnr2 do begin
                s_cnr := IntToStr(i_cnr);
                i_idx := t_casegrp.IndexOfCase(s_cnr);
                if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
              end;
            end;
          end else begin
            if TryStrToInt(t_cstrs[i], i_cnr) then begin
              s_cnr := IntToStr(i_cnr);
              i_idx := t_casegrp.IndexOfCase(s_cnr);
              if ((i_idx >= 0) and (i_idx <= i_maxidx)) then Include(result, i_idx);
            end;
          end;
        end;
        FreeAndNil(t_cnrs);
      end;
      FreeAndNil(t_cstrs);
    end;
  end;
end;

constructor TStepContainer.Create();
begin
  inherited Create();
  FreeByRemove := true;
end;

destructor TStepContainer.Destroy();
begin
  Clear();
  inherited Destroy();
end;

function TStepContainer.LoadStep(const fields: FieldStringArray): boolean;
var t_step: TTestStep;
begin
  t_step := TTestStep.Create();
  t_step.LoadFields(fields);
  result := AddStep(t_step);
  if (not result) then FreeAndNil(t_step);
end;

function TStepContainer.CreateSequence(const incl: string = 'all'; const excl: string = ''): TTestSequence;
begin
  result := TTestSequence.Create();
  UpdateSequence(result, incl, excl);
end;

// =============================================================================
//    Description  : analyze the given strings incl and excl and update t_sequence
//    Parameter    : tseq, a test sequence to return
//                   incl, a string to represent inclusive test cases
//                   excl, a string to represent exclusive test cases
//                   see function IndexSet
//    Return       : --
//    First author : 2016-05-18 /bsu/
//                   WARNING: because of using set in this function the count of
//                   test cases is not allowed beyond CINT_CASES_MAX + 1 (256).
//                   The cases whose indexes (from 0 to 255) is greater than
//                   CINT_CASES_MAX (255), are not considered.
//    History      :
// =============================================================================
function TStepContainer.UpdateSequence(var tseq: TTestSequence; const incl: string; const excl: string): boolean;
var set_incl, set_excl, set_result: TIndexSet;
begin
  result := false;
  if assigned(tseq) then begin
    set_incl := CaseIndexSet(incl);
    set_excl := CaseIndexSet(excl);
    set_result := set_incl - set_excl;
    result := (tseq.UpdateCaseGroup(set_result, self) > 0);
  end;
end;

// =============================================================================
//    Description  : save field values of test steps into a file
//    Parameter    : sfile, file name to save
//    Return       : --
//    First author : 2016-05-09 /bsu/
//    History      :
// =============================================================================
procedure TStepContainer.SaveFile(const sfile: string);
var i: integer; s_line: string; j:EStepField; t_stepvals: TStringList; t_step: TTestStep;
begin
  t_stepvals := TStringList.Create();
  for i := 0 to t_steps.Count - 1 do begin
    t_step := TTestStep(t_steps.Objects[i]);
    s_line := t_step.GetFieldValue(SF_NR);
    for j := SF_T to High(EStepField) do s_line := s_line + ';' + #9 + t_step.GetFieldValue(j);
    t_stepvals.Add(s_line);
  end;
  t_stepvals.SaveToFile(sfile);
  t_stepvals.Free();
end;

end.
