unit PairStrings;

interface
uses Classes;

Type

  TPairStrings = class
  protected
    t_namevals: TStrings;
  protected
    function GetCount(): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    property  Count: integer read GetCount;

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
uses SysUtils;

function TPairStrings.GetCount(): integer;
begin
  result := t_namevals.Count;
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
begin
  result := (t_namevals.IndexOfName(sname) >= 0);
end;

function TPairStrings.AddPair(const sname, sval: string; bcover: boolean): boolean;
begin
  if ((bcover) or (t_namevals.IndexOfName(sname) < 0)) then result := SetPairValue(sname, sval)
  else result := false;
end;

function  TPairStrings.AddPair(const nameval: string; bcover: boolean): boolean;
var t_pair: TStrings;
begin
  result := false;
  t_pair := TStringList.Create();
  if (ExtractStrings([t_namevals.NameValueSeparator], [' '], PChar(nameval), t_pair) = 2) then
    result := AddPair(t_pair[0], t_pair[1]);
  FreeAndNil(t_pair);
end;

function TPairStrings.AddPairs(const names, vals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  if (names.Count = vals.Count) then begin
    for i := 0 to names.Count - 1 do begin
      if (not AddPair(names[i], vals[i], bcover)) then break;
      inc(result);
    end;
  end;
end;

function  TPairStrings.AddPairs(const namevals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to namevals.Count - 1 do begin
    if (not AddPair(namevals[i])) then break;
    inc(result);
  end;
end;

function  TPairStrings.GetPairValue(const sname: string; var sval: string): boolean;
var i_index: integer;
begin
  result := false; sval := '';
  if sname <> '' then begin
    i_index := t_namevals.IndexOfName(sname);
    result := (i_index >= 0);
    sval := t_namevals.ValueFromIndex[i_index];
  end;
end;

function  TPairStrings.SetPairValue(const sname, sval: string): boolean;
var i_index: integer; s_pair: string;
begin
  i_index := t_namevals.IndexOfName(sname);
  s_pair := sname + t_namevals.NameValueSeparator + sval;
  if (i_index >= 0) then begin
    t_namevals.Delete(i_index);
    t_namevals.Insert(i_index, s_pair);
  end else i_index := t_namevals.Add(s_pair);
  result := (i_index >= 0);
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
