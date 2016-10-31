// =============================================================================
// Module name  : $RCSfile: StringPairs.pas,v $
// description  : This unit implements a class, in which name-value pairs are managed.
//                It is very useful for the configuration from ini-file
// Compiler     : Delphi 2007
// Author       : 2015-11-20 /bsu/
// History      :
//==============================================================================
unit StringPairs;

interface
uses Classes;

Type

  TStringPairs = class
  protected
    t_namevals: TStrings;
  protected
    function GetCount(): integer;
    function Rename(const sname: string): string;
    function GetPairName(idx: integer): string;
    function CleanValue(const val: string): string;

  public
    constructor Create();
    destructor Destroy(); override;

    property  Count: integer read GetCount;
    property  Pairs: TStrings read t_namevals;
    property  PairName[idx: integer]: string read GetPairName;

    function  FindPair(const sname: string): boolean;
    function  AddPair(const sname, sval: string; bcover: boolean = true): boolean; overload;
    function  AddPair(const nameval: string; bcover: boolean = true): boolean; overload;
    function  AddPairs(const names, vals: TStrings; bcover: boolean = true): integer; overload;
    function  AddPairs(const namevals: TStrings; bcover: boolean = true): integer; overload;
    function  GetPairValue(const sname: string; var sval: string): boolean; overload;
    function  GetPairValue(const idx: integer; var sval: string): boolean; overload;
    function  SetPairValue(const sname, sval: string): boolean;
    function  GetPairNames(var names: TStrings): integer;
    function  GetPairValues(var values: TStrings): integer;
    function  HasSameValues(const ref: TStringPairs; const varnames: TStrings): boolean;
    function  CopyValuesFrom(const source: TStringPairs; const varnames: TStrings): integer;
    procedure RemovePair(const idx: integer); overload;
    procedure RemovePair(const sname: string); overload;
    procedure Clear(); virtual;
  end;

implementation
uses SysUtils, StrUtils;

function TStringPairs.GetCount(): integer;
begin
  result := t_namevals.Count;
end;

//if sname is given in format name[n], it will be renamed in format name_n
function TStringPairs.Rename(const sname: string): string;
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

function TStringPairs.GetPairName(idx: integer): string;
begin
  result := '';
  if ((idx >= 0) and (idx < t_namevals.Count)) then result := t_namevals.Names[idx];
end;


function TStringPairs.CleanValue(const val: string): string;
var t_strs: TStrings; i: integer;
begin
  //clean combined value, which is composed of more through strings and separated through '|'
  //e.g. 'string1  |string2  |  string3'.
  //Every string of the combined value will be cleaned using trim() in this function
  if (Pos('|', val) > 0) then begin
    t_strs := TStringList.Create();
    ExtractStrings(['|'], [], PChar(val), t_strs);
    result := trim(t_strs[0]);
    for i := 1 to t_strs.Count - 1 do result := result + '|' + trim(t_strs[i]);
    t_strs.Free();
  end else result := trim(val); //clean single value
end;

constructor TStringPairs.Create();
begin
  inherited Create();
  t_namevals := TStringList.Create();
end;

destructor TStringPairs.Destroy();
begin
  t_namevals.Free();
  inherited Destroy();
end;

function TStringPairs.FindPair(const sname: string): boolean;
var s_name: string;
begin
  s_name := Rename(sname);
  result := (t_namevals.IndexOfName(s_name) >= 0);
end;

function TStringPairs.AddPair(const sname, sval: string; bcover: boolean): boolean;
var s_name: string;
begin
  s_name := Rename(sname);
  if ((bcover) or (t_namevals.IndexOfName(s_name) < 0)) then result := SetPairValue(s_name, sval)
  else result := false;
end;

function  TStringPairs.AddPair(const nameval: string; bcover: boolean): boolean;
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

function TStringPairs.AddPairs(const names, vals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  if (names.Count = vals.Count) then begin
    for i := 0 to names.Count - 1 do begin
      if AddPair(names[i], vals[i], bcover) then inc(result);
    end;
  end;
end;

function  TStringPairs.AddPairs(const namevals: TStrings; bcover: boolean): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to namevals.Count - 1 do begin
    if AddPair(namevals[i], bcover) then inc(result);
  end;
end;

function  TStringPairs.GetPairValue(const sname: string; var sval: string): boolean;
var i_index: integer; s_name: string;
begin
  result := false;
  sval := ''; s_name := Rename(sname);
  if s_name <> '' then begin
    i_index := t_namevals.IndexOfName(s_name);
    result := GetPairValue(i_index, sval);
  end;
end;

function  TStringPairs.GetPairValue(const idx: integer; var sval: string): boolean;
begin
  sval := '';
  result := ((idx >= 0) and (idx < t_namevals.Count));
  if result then sval := t_namevals.ValueFromIndex[idx];
end;

function  TStringPairs.SetPairValue(const sname, sval: string): boolean;
var i_index: integer; s_pair, s_name, s_value: string;
begin
  result := false;
  s_name := Rename(sname);
  s_value := CleanValue(sval);
  if s_name <> '' then begin
    i_index := t_namevals.IndexOfName(s_name);
    s_pair := s_name + t_namevals.NameValueSeparator + s_value;
    if (i_index >= 0) then begin
      t_namevals.Delete(i_index);
      t_namevals.Insert(i_index, s_pair);
    end else i_index := t_namevals.Add(s_pair);
    result := (i_index >= 0);
  end;
end;

function  TStringPairs.GetPairNames(var names: TStrings): integer;
var i: integer;
begin
  names.Clear();
  for i := 0 to t_namevals.Count - 1 do names.Append(t_namevals.Names[i]);
  result := names.Count;
end;

function  TStringPairs.GetPairValues(var values: TStrings): integer;
var i: integer;
begin
  values.Clear();
  for i := 0 to t_namevals.Count - 1 do values.Append(t_namevals.ValueFromIndex[i]);
  result := values.Count;
end;

function  TStringPairs.HasSameValues(const ref: TStringPairs; const varnames: TStrings): boolean;
var i: integer; s_name, s_val, s_valref: string;
begin
  result := true;
  for i := 0 to varnames.Count - 1 do begin
    s_name := varnames[i];
    if GetPairValue(s_name, s_val) = ref.GetPairValue(s_name, s_valref) then result := SameText(s_val, s_valref)
    else result := false;
    if (not result) then break;
  end;
end;

function  TStringPairs.CopyValuesFrom(const source: TStringPairs; const varnames: TStrings): integer;
var i: integer; s_name, s_val: string;
begin
  result := 0;
  for i := 0 to varnames.Count - 1 do begin
    s_name := varnames[i];
    if source.GetPairValue(s_name, s_val) then begin
      SetPairValue(s_name, s_val);
      Inc(result);
    end;
  end;
end;

procedure TStringPairs.RemovePair(const idx: integer);
begin
  if ((idx >= 0) and (idx < t_namevals.Count)) then t_namevals.Delete(idx);
end;

procedure TStringPairs.RemovePair(const sname: string);
var i_index: integer;
begin
  i_index := t_namevals.IndexOfName(sname);
  RemovePair(i_index);
end;

procedure TStringPairs.Clear();
begin
  t_namevals.Clear();
end;

end.
