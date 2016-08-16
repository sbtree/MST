unit StepChecker;

interface
uses Classes, FuncBase, StepData, StepGroup;

type
//----------------------------------------------------------------------------//
// class name:  TFieldKeys
// description: A help class to check the name of a step field.
//              1. a field must have a valid name
//              2. the name is forbidden to appear in a step more than once
//----------------------------------------------------------------------------//
  TFieldNameChecker = class
  protected
    a_keys: FieldStringArray;
    b_used: array[EStepField] of boolean;
  protected
    function  CheckNames(const names: FieldStringArray): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetUnused();
    function  SetFieldNames(const names: FieldStringArray): boolean;
    function  IsNameValid(const field: string; var idx: EStepField): boolean;
    function  FieldName(const index: EstepField): string;
    procedure SetNameUsed(const index: EstepField; const used: boolean);
    function  IsNameUsed(const index: EstepField): boolean;
  end;

//----------------------------------------------------------------------------//
// class name:  TFieldKeys
// description: A help class to check the values of step fields.
//              1. check only grammatical error in the value of step fields
//----------------------------------------------------------------------------//
  TFieldValueChecker = class
  protected
    f_lastnr:   single;
  protected
    function CheckNr(const str: string): boolean;
    function CheckT(const str: string): boolean;
    function CheckInit(const str: string): boolean;
    function CheckM(const str: string): boolean;
    function CheckFct(const str: string): boolean;
    function CheckPar(const str: string): boolean;
    function CheckFinal(const str: string): boolean;
    function CheckTol(const str: string): boolean;
    function CheckA(const str: string): boolean;
    function CheckMin(const str: string): boolean;
    function CheckMax(const str: string): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetChecker();
    function  CheckField(const field: EStepField; const val: string): boolean;
  end;

implementation
uses SysUtils, StrUtils;
{const

  CSTR_FIELD_NAMES_V02  :  FieldStringArray = (
                        'NR',
                        'T',
                        'INIT',
                        'FCT',
                        'M',
                        'PAR',
                        'FINAL',
                        'VALID',
                        'A',
                        'MIN',
                        'MAX'
                      );

  CSTR_FIELD_NAMES_V03  :  FieldStringArray = (
                        'NR',
                        'T',
                        'INIT',
                        'FCT',
                        'M',
                        'PAR',
                        'FINAL',
                        'TOL',
                        'A',
                        'MIN',
                        'MAX'
                      );
}
//reduplicated key is not allowed
function  TFieldNameChecker.CheckNames(const names: FieldStringArray): boolean;
var i, j: integer;
begin
  result := true;
  for i := Ord(Low(EStepField)) to Ord(High(EStepField)) - 1 do begin
    for j := i + 1 to Ord(High(EStepField)) do begin
      if SameText(names[EStepField(i)], names[EStepField(j)]) then begin
        result := false;
        break;
      end;
    end;
  end;
end;

constructor TFieldNameChecker.Create();
begin
  inherited Create();
  SetFieldNames(CSTR_FIELD_NAMES);
  ResetUnused();
end;

destructor TFieldNameChecker.Destroy();
begin
  inherited Destroy();
end;

procedure TFieldNameChecker.ResetUnused();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do b_used[i] := false;
end;

function TFieldNameChecker.SetFieldNames(const names: FieldStringArray): boolean;
var i: EStepField;
begin
  result := CheckNames(names);
  if result then for i := Low(EStepField) to High(EStepField) do a_keys[i] := names[i];
end;

function TFieldNameChecker.IsNameValid(const field: string; var idx: EStepField): boolean;
var i_index: integer;
begin
  i_index := IndexText(field, a_keys);
  result := (i_index >= Ord(Low(EStepField)));
  if (result) then idx := EStepField(i_index);
end;

function TFieldNameChecker.FieldName(const index: EstepField): string;
begin
  result := a_keys[index];
end;

procedure TFieldNameChecker.SetNameUsed(const index: EstepField; const used: boolean);
begin
  b_used[index] := used;
end;

function TFieldNameChecker.IsNameUsed(const index: EstepField): boolean;
begin
  result := b_used[index];
end;

function TFieldValueChecker.CheckNr(const str: string): boolean;
var s_snr: string; f_stepnr: single;
begin
  result := false;
  s_snr := ReplaceStr(str, '.', DecimalSeparator); //replace '.' with the system decimal separator
  if TryStrToFloat(s_snr, f_stepnr) then begin
    f_stepnr := abs(f_stepnr);
    if (f_stepnr > f_lastnr) then begin
      result := true;
      f_lastnr := f_stepnr;
    end;
  end;
end;

function TFieldValueChecker.CheckT(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckInit(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckM(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckFct(const str: string): boolean;
var t_class : TFunctionClass;
begin
  t_class := TFunctionClass(GetClass(str));
  result := true;//(t_class <> nil);  //todo:
  //todo: check special function, e.g. LoopBegin, LoopEnd
end;

function TFieldValueChecker.CheckPar(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckFinal(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckTol(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckA(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckMin(const str: string): boolean;
begin
  //todo:
  result := true;
end;

function TFieldValueChecker.CheckMax(const str: string): boolean;
begin
  //todo:
  result := true;
end;

constructor TFieldValueChecker.Create();
begin
  inherited Create();
  ResetChecker();
end;

destructor TFieldValueChecker.Destroy();
begin
  inherited Destroy();
end;

procedure TFieldValueChecker.ResetChecker();
begin
  f_lastnr := 0.0;
end;

function TFieldValueChecker.CheckField(const field: EStepField; const val: string): boolean;
begin
  result := false;
  case field of
    SF_NR: result := CheckNr(val);
    SF_T: result := CheckT(val);
    SF_INIT: result := CheckInit(val);
    SF_FCT: result := CheckFct(val);
    SF_M: result := CheckM(val);
    SF_PAR: result := CheckPar(val);
    SF_FINAL: result := CheckFinal(val);
    SF_TOL: result := CheckTol(val);
    SF_TOL_A: result := CheckA(val);
    SF_TOL_MIN: result := CheckMin(val);
    SF_TOL_MAX: result := CheckMax(val);
  end;
end;


end.
