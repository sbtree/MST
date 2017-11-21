// =============================================================================
// Module name  : $RCSfile: ConfigBase.pas,v $
// Description  : This unit defines base class for configurations of measurement
//                device, product and so an.
// Compiler     : Delphi 2007
// Author       : 2015-12-11 /bsu/
// History      :
//==============================================================================
unit ConfigBase;

interface
uses Classes, System.Generics.Collections,StringPairs;

type
  EConfigFormat = (
                CF_INI,
                CF_XML
                );

  TStringDictionary = class(TDictionary<string, string>)
  protected
    c_fldseparator: char;
  public
    constructor Create();

    procedure UpdateBy(const keyvals: TStrings; const bcover: boolean = true); overload;
    procedure UpdateBy(const config: TStringDictionary; const bcover: boolean = true); overload;
    procedure ReduceBy(const config: TStringDictionary);
    function  FieldValue(const fldname: string; const fldindex: integer = 0): string;

    property  FieldSeparator: char read c_fldseparator write c_fldseparator default '|';
  end;

  TConfigDictionary= class;
  TBranchConfig = class(TStringDictionary)
  protected
    s_ownname:  string;
    s_ownid:    string;
    t_parent:   TBranchConfig;
    t_children: TConfigDictionary;
  end;

  TConfigDictionary= class(TObjectDictionary<string, TBranchConfig>)
  end;


  IConfigBase = interface
    function UpdateFromFile(const fname: string; const bforce: boolean): boolean;
    function SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
    function GetConfig(const secname: string): TStringDictionary;
    procedure Clear();
  end;

  TConfigBase = class(TInterfacedObject, IConfigBase)
  protected
    s_fname:  string;
    t_fstemp: TDateTime;
    t_confs:  TStrings;
  protected
    function ReadFromIni(const fname: string): boolean; virtual;
    function ReadFromXml(const fname: string): boolean; virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateFromFile(const fname: string; const bforce: boolean = false): boolean; virtual;
    function SaveToFile(const destfile: string = ''; const cf: EConfigFormat = CF_INI): boolean; virtual;
    function GetConfig(const secname: string): TStringDictionary; virtual;
    procedure Clear(); virtual;
  end;

implementation
uses SysUtils, StrUtils, IniFiles, System.Generics.Defaults;

function ExtractKeyValue(const keyval: string; var skey, sval: string; const sepa: char): boolean;
var i_pos: integer;
begin
  i_pos := Pos(sepa, keyval);
  if (i_pos > 1) then begin //name is not empty
    skey := trim(LeftStr(keyval, i_pos - 1));
    sval := trim(RightStr(keyval, length(keyval) - i_pos));
  end else begin //separator is not found, only name is given
    skey := trim(keyval);
    sval := '';
  end;
  result := (length(skey) > 0);
end;

constructor TStringDictionary.Create();
begin
  inherited Create(TOrdinalIStringComparer.Create);
end;

//update from a string list (list of strings with format 'key=value')
procedure TStringDictionary.UpdateBy(const keyvals: TStrings; const bcover: boolean);
var s_key, s_val, s_keyval: string;
begin
  if assigned(keyvals) then begin
    for s_keyval in keyvals do begin
      if ExtractKeyValue(s_keyval, s_key, s_val, keyvals.NameValueSeparator) then begin
        if bcover then
          AddOrSetValue(s_key, s_val)
        else if not ContainsKey(s_key) then
          Add(s_key, s_val);
      end;
    end;
  end;
end;

//update from another TProductSetting
procedure TStringDictionary.UpdateBy(const config: TStringDictionary; const bcover: boolean);
var t_pair: TPair<string, string>;
begin
  if assigned(config) then begin
    for t_pair in config do begin
      if bcover then
        AddOrSetValue(t_pair.Key, t_pair.Value)
      else if (not ContainsKey(t_pair.Key)) then
        Add(t_pair.Key, t_pair.Value);
    end
  end;
end;

procedure TStringDictionary.ReduceBy(const config: TStringDictionary);
var s_val: string; s_pair: TPair<string, string>;
begin
  if assigned(config) then begin
    for s_pair in config do begin
      if self.TryGetValue(s_pair.Key, s_val) then begin
        if SameText(s_pair.Value, s_val) then
          self.Remove(s_pair.Value);
      end;
    end;
  end;
end;

function TStringDictionary.FieldValue(const fldname: string; const fldindex: integer): string;
var t_fields: TStringList;
begin
  result := '';
  if self.ContainsKey(fldname) then begin
    t_fields := TStringList.Create;
    if (ExtractStrings([FieldSeparator], [char(9), ' '], PChar(self.Items[fldname]), t_fields) >= fldindex) then result := t_fields[fldindex];
    t_fields.Free;
  end;
end;

function TConfigBase.ReadFromIni(const fname: string): boolean;
var t_ini: TIniFile; t_secnames, t_secvals: TStrings; t_conf: TStringDictionary;
    s_key: string;
begin
  t_ini := TIniFile.Create(fname);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();
  t_ini.ReadSections(t_secnames);
  result := (t_secnames.Count > 0);
  for s_key in t_secnames do begin
    t_conf := GetConfig(s_key);
    if (not assigned(t_conf)) then t_conf := TStringDictionary.Create();
    t_secvals.Clear();
    t_ini.ReadSectionValues(s_key, t_secvals);
    t_conf.UpdateBy(t_secvals, true);
    t_confs.AddObject(s_key, t_conf);
  end;
  t_secvals.Free();
  t_secnames.Free();
  t_ini.Free();
end;

function TConfigBase.ReadFromXml(const fname: string): boolean;
begin
  result := false;
  //todo:
end;

constructor TConfigBase.Create();
begin
  inherited Create();
  t_confs := TStringList.Create();
end;

destructor TConfigBase.Destroy();
begin
  Clear();
  t_confs.Free();
  inherited Destroy();
end;

function TConfigBase.UpdateFromFile(const fname: string; const bforce: boolean): boolean;
var t_dtime: TDateTime;
begin
  result := FileExists(fname);
  if result then begin
    FileAge(fname, t_dtime);
    if (bforce or (t_dtime <> t_fstemp) or (fname <> s_fname)) then begin
      Clear();
      if EndsText('.ini', fname) then result := ReadFromIni(fname)
      else if EndsText('.xml', fname) then result := ReadFromXml(fname)
      else result := false;
      if result then begin
        t_fstemp := t_dtime;
        s_fname := fname;
      end;
    end;
  end;
end;

function TConfigBase.SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
begin
  result := false;
  //todo:
end;

function TConfigBase.GetConfig(const secname: string): TStringDictionary;
var i_idx: integer;
begin
  result := nil;
  i_idx := t_confs.IndexOfName(secname);
  if (i_idx >= 0) then result := TStringDictionary(t_confs.Objects[i_idx]);
end;

procedure TConfigBase.Clear();
begin
  while (t_confs.Count > 0) do
    if assigned(t_confs.Objects[0]) then begin
      t_confs.Objects[0].Free();
      t_confs.Delete(0);
    end;
end;

end.
