unit NamedStrings;

interface
uses Classes;

Type
  TNamedStrings = class
  protected
    t_namevals: TStrings;
  protected
    function GetCount(): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    property  Count: integer read GetCount;

    function  AddNamedString(const name, value: string; const bcover: boolean = false): boolean;
    function  AddNamedStrings(const list: TStrings; const bcover: boolean = false): boolean;
    function  FoundName(const name: string): boolean;
    function  GetNamedString(const name: string; var value: string): boolean;
    procedure Clear();
  end;

implementation

function TNamedStrings.GetCount(): integer;
begin
  result := t_namevals.Count;
end;

constructor TNamedStrings.Create();
begin
  t_namevals := TStringList.Create();
end;
destructor TNamedStrings.Destroy();
begin
  t_namevals.Free();
end;

function TNamedStrings.AddNamedString(const name, value: string; const bcover: boolean): boolean;
var i_index: integer;
begin
  result := false;
  if (name <> '') then begin
    i_index := t_namevals.IndexOfName(name);
    if (i_index >= 0) then begin
      if bcover then begin
        t_namevals.ValueFromIndex[i_index] := value;
        result := true;
      end;
    end else begin
      i_index := t_namevals.Add(name + t_namevals.NameValueSeparator + value);
      result := (i_index >= 0);
    end;
  end;
end;

function TNamedStrings.AddNamedStrings(const list: TStrings; const bcover: boolean): boolean;
var i: integer; s_name, s_value: string;
begin
  result := false;
  for i := 0 to list.Count - 1 do begin
    s_name := list.Names[i];
    s_value := list.ValueFromIndex[i];
    result := AddNamedString(s_name, s_value, bcover);
    if (not result) then break;
  end;
end;

function TNamedStrings.FoundName(const name: string): boolean;
begin
  result := false;
  if (name <> '') then result := (t_namevals.IndexOfName(name) >= 0);
end;

function TNamedStrings.GetNamedString(const name: string; var value: string): boolean;
var i_index: integer;
begin
  result := false;
    if name <> '' then begin
    i_index := t_namevals.IndexOfName(name);
    value := '';
    result := (i_index >= 0);
    if result then value := t_namevals.ValueFromIndex[i_index];
  end;
end;

procedure TNamedStrings.Clear();
begin
  t_namevals.Clear();
end;

end.
