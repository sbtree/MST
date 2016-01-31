unit StepChecker;

interface
uses TestStep;

type
//----------------------------------------------------------------------------//
// class name:  TFieldKeys
// description: A help class to check the name of a step field.
//              1. a field must have a valid name
//              2. the name is forbidden to appear in a step more than once
//----------------------------------------------------------------------------//
  TFieldKeyChecker = class
  protected
    a_keys: FieldStringArray;
    b_used: array[EStepField] of boolean;
  protected
    function  CheckKeys(const keys: FieldStringArray): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetUnused();
    function  SetFieldKeys(const keys: FieldStringArray): boolean;
    function  FindKey(const field: string; var idx: EStepField): boolean;
    function  FieldKey(const index: EstepField): string;
    procedure SetKeyUsed(const index: EstepField; const used: boolean);
    function  IsKeyUsed(const index: EstepField): boolean;
  end;

//----------------------------------------------------------------------------//
// class name:  TStepNrChecker
// description: A help class to check the step number:
//              1. if it is valid
//              2. the name is forbidden to appear in a step more than once
//----------------------------------------------------------------------------//
  TStepNrChecker = class
  protected
    i_casenr: integer;
    i_stepnr: integer;
  public

    //function CheckNr(const nr: string): boolean;
  end;

  {TStepTChecker = class
  end;

  TStepInitChecker = class
  end;

  TStepParChecker = class
  end;

  TStepFctChecker = class
  end;}

  TStepMChecker = class
  end;


implementation
uses SysUtils, StrUtils;

//reduplicated key is not allowed
function  TFieldKeyChecker.CheckKeys(const keys: FieldStringArray): boolean;
var i, j: integer;
begin
  result := true;
  for i := Ord(Low(EStepField)) to Ord(High(EStepField)) - 1 do begin
    for j := i + 1 to Ord(High(EStepField)) do begin
      if SameText(keys[EStepField(i)], keys[EStepField(j)]) then begin
        result := false;
        break;
      end;
    end;
  end;
end;

constructor TFieldKeyChecker.Create();
begin
  inherited Create();
  SetFieldKeys(CSTR_FIELD_KEYS_V03);
  ResetUnused();
end;

destructor TFieldKeyChecker.Destroy();
begin
  inherited Destroy();
end;

procedure TFieldKeyChecker.ResetUnused();
var i: EStepField;
begin
  for i := Low(EStepField) to High(EStepField) do b_used[i] := false;
end;

function TFieldKeyChecker.SetFieldKeys(const keys: FieldStringArray): boolean;
var i: EStepField;
begin
  result := CheckKeys(keys);
  if result then for i := Low(EStepField) to High(EStepField) do a_keys[i] := keys[i];
end;

function TFieldKeyChecker.FindKey(const field: string; var idx: EStepField): boolean;
var i_index: integer;
begin
  i_index := IndexText(field, a_keys);
  result := (i_index >= Ord(Low(EStepField)));
  if (result) then idx := EStepField(i_index);
end;

function TFieldKeyChecker.FieldKey(const index: EstepField): string;
begin
  result := a_keys[index];
end;

procedure TFieldKeyChecker.SetKeyUsed(const index: EstepField; const used: boolean);
begin
  b_used[index] := used;
end;

function TFieldKeyChecker.IsKeyUsed(const index: EstepField): boolean;
begin
  result := b_used[index];
end;

end.
