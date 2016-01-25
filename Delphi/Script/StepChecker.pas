unit StepChecker;

interface
uses ScriptTerm;

type
//----------------------------------------------------------------------------//
// class name:  TFieldKeys
// description: A help class to check the name of a step field.
//              1. a field must have a valid name
//              2. the name is forbidden to appear in a step more than once
//----------------------------------------------------------------------------//
  TFieldKeyChecker = class
  protected
    a_keys: FieldKeyArray;
    b_used: array[EStepField] of boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetUnused();
    function  SetFieldKeys(const keys: FieldKeyArray): boolean;
    function  FindKey(const field: string; var idx: EStepField): boolean;
    function  FieldKey(const index: EstepField): string;
    procedure SetKeyUsed(const index: EstepField; const used: boolean);
    function  IsKeyUsed(const index: EstepField): boolean;
  end;

  TStepNrChecker = class

  end;

  TStepTChecker = class

  end;

  TStepInitChecker = class

  end;

  TStepFctChecker = class

  end;

  TStepMChecker = class

  end;

  TStepParChecker = class

  end;

implementation
uses StrUtils;

constructor TFieldKeyChecker.Create();
var i: EStepField;
begin
  inherited Create();
  for i := Low(EStepField) to High(EStepField) do a_keys[i] := CSTR_FIELD_KEYS_V03[i];
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

function TFieldKeyChecker.SetFieldKeys(const keys: FieldKeyArray): boolean;
var i: EStepField;
begin
  result := true;
  for i := Low(EStepField) to High(EStepField) do begin
    if (IndexText(keys[i], a_keys) < Ord(Low(EStepField))) then a_keys[i] := keys[i]
    else begin
      result := false;
      break;
    end;
  end;
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
