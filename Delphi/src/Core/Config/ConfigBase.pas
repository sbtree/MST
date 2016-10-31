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
uses Classes, StringPairs;

type
  EConfigFormat = (
                CF_INI,
                CF_XML
                );
  IConfigBase = interface
    function UpdateFromFile(const fname: string; const bforce: boolean): boolean;
    function SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
    function GetConfig(const secname: string): TStringPairs;
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
    function GetConfig(const secname: string): TStringPairs; virtual;
    procedure Clear(); virtual;
  end;

implementation
uses SysUtils, StrUtils, IniFiles;

function TConfigBase.ReadFromIni(const fname: string): boolean;
var i: integer; t_ini: TIniFile; t_secnames, t_secvals: TStrings; t_conf: TStringPairs;
begin
  t_ini := TIniFile.Create(fname);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();
  t_ini.ReadSections(t_secnames);
  result := (t_secnames.Count > 0);
  for i := 0 to t_secnames.Count - 1 do begin
    t_conf := GetConfig(t_secnames[i]);
    if (not assigned(t_conf)) then t_conf := TStringPairs.Create();
    t_secvals.Clear();
    t_ini.ReadSectionValues(t_secnames[i], t_secvals);
    t_conf.AddPairs(t_secvals, true);
    t_confs.AddObject(t_secnames[i], t_conf);
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

function TConfigBase.GetConfig(const secname: string): TStringPairs;
var i_idx: integer;
begin
  result := nil;
  i_idx := t_confs.IndexOfName(secname);
  if (i_idx >= 0) then result := TStringPairs(t_confs.Objects[i_idx]);
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
