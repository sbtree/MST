unit PairStrings;

interface
uses Classes;

Type

  TPairStrings = class
  protected
    t_namevals: TStrings;
  protected
    function GetCount(): integer;
    function Rename(const sname: string): string;

  public
    constructor Create();
    destructor Destroy(); override;

    property  Count: integer read GetCount;
    property  Pairs: TStrings read t_namevals;

    function  FindPair(const sname: string): boolean;
    function  AddPair(const sname, sval: string; bcover: boolean = true): boolean; overload;
    function  AddPair(const nameval: string; bcover: boolean = true): boolean; overload;
    function  AddPairs(const names, vals: TStrings; bcover: boolean = true): integer; overload;
    function  AddPairs(const namevals: TStrings; bcover: boolean = true): integer; overload;
    function  GetPairValue(const sname: string; var sval: string): boolean;
    function  SetPairValue(const sname, sval: string): boolean;
    function  GetPairNames(var names: TStrings): integer;
    function  GetPairValues(var values: TStrings): integer;
    procedure RemovePair(const sname: string);
    procedure Clear();
  end;

implementation
uses SysUtils, StrUtils;

function TPairStrings.GetCount(): integer;
begin
  result := t_namevals.Count;
end;

//if sname is given in format name[n], it will be renamed in format name_n
function TPairStrings.Rename(const sname: string): string;
var i_pos: integer;
begin
  result := trim(sname);
  i_pos := Pos('[', result);
  if (i_pos > 1) then result[i_pos] := '_';
  if (EndsText(']', result)) then begin
    result := LeftStr(result, length(result) - 1);
    result := trim(result);
  end;
end;

constructor TPairStrings.Create();
begin
  t_namevals := TStringList.Create();
end;
destructor TPairStrings.Destroy();
begin
  t_namevals.Free();
end;

function TPairStrings.FindPair(const sname: string): boolean;
var s_name: string;
begin
  s_name := Rename(sname);
  result := (t_namevals.IndexOfName(s_name) >= 0);
end;

function TPairStrings.AddPair(const sname, sval: string; bcover: boolean): boolean;
var s_name: string;
begin
  s_name := Rename(sname);
  if ((bcover) or (t_namevals.IndexOfName(s_name) < 0)) then result := SetPairValue(s_name, sval)
  else result := false;
end;

function  TPairStrings.AddPair(const nameval: string; bcover: boolean): boolean;
var s_pair, s_name, s_value: string; i_pos: integer;
begin
  s_pair := trim(nameval);
  s_name := ''; s_value := '';
  i_pos := Pos(t_namevals.NameValueSeparator, s_pair);
  if (i_pos > 1) then begin //name is not empty
    s_name := LeftStr(s_pair, i_pos - 1);
    s_value := RightStr(s_pair, length(s_pair) - i_pos);
  end else if (i_pos = 0) then s_name := s_pair; //separator is not found, only name is given
  result := AddPair(s_name, s_value, bcover);
end;

function TPairStrings.AddPairs(const names, vals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  if (names.Count = vals.Count) then begin
    for i := 0 to names.Count - 1 do begin
      if AddPair(names[i], vals[i], bcover) then inc(result);
    end;
  end;
end;

function  TPairStrings.AddPairs(const namevals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to namevals.Count - 1 do begin
    if AddPair(namevals[i], bcover) then inc(result);
  end;
end;

function  TPairStrings.GetPairValue(const sname: string; var sval: string): boolean;
var i_index: integer; s_name: string;
begin
  result := false;
  sval := ''; s_name := Rename(sname);
  if s_name <> '' then begin
    i_index := t_namevals.IndexOfName(s_name);
    result := (i_index >= 0);
    sval := t_namevals.ValueFromIndex[i_index];
  end;
end;

function  TPairStrings.SetPairValue(const sname, sval: string): boolean;
var i_index: integer; s_pair, s_name: string;
begin
  result := false; s_name := Rename(sname);
  if s_name <> '' then begin
    i_index := t_namevals.IndexOfName(s_name);
    s_pair := s_name + t_namevals.NameValueSeparator + sval;
    if (i_index >= 0) then begin
      t_namevals.Delete(i_index);
      t_namevals.Insert(i_index, s_pair);
    end else i_index := t_namevals.Add(s_pair);
    result := (i_index >= 0);
  end;
end;

function  TPairStrings.GetPairNames(var names: TStrings): integer;
var i: integer;
begin
  names.Clear();
  for i := 0 to t_namevals.Count - 1 do names.Append(t_namevals.Names[i]);
  result := names.Count;
end;

function  TPairStrings.GetPairValues(var values: TStrings): integer;
var i: integer;
begin
  values.Clear();
  for i := 0 to t_namevals.Count - 1 do values.Append(t_namevals.ValueFromIndex[i]);
  result := values.Count;
end;

procedure TPairStrings.RemovePair(const sname: string);
var i_index: integer;
begin
  i_index := t_namevals.IndexOfName(sname);
  if (i_index >= 0) then t_namevals.Delete(i_index);
end;

procedure TPairStrings.Clear();
begin
  t_namevals.Clear();
end;

end.
