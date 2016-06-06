unit ProductConfig;

interface
uses Classes, PairStrings;
const
  CSTR_ROOT = 'ROOT';

type
  EConfigFormat = (
                CF_INI,
                CF_XML
                );

  TConfigItem = class
  protected
    t_parent:     TConfigItem;
    s_confname:   string;
    t_config:     TPairStrings;
    t_children:   TStringList;
  protected
    function FindConfigItem(const sname: string; var citem: TConfigItem): boolean;
    function GetChildren(): integer;
    function GetChild(idx: integer): TConfigItem;
    function WriteToIni(var slines: TStrings): boolean;
    //function WriteToXML(var slines: TStrings): boolean; //todo

  public
    constructor Create(const confname: string; const parent: TConfigItem = nil);
    destructor Destroy(); override;

    property ConfigName: string read s_confname write s_confname;
    property Config: TPairStrings read t_config write t_config;
    property ChildCount: integer read GetChildren;
    property Children[idx: integer]: TConfigItem read GetChild;

    function AddChild(const sname: string): TConfigItem;
    function HasChildren(): boolean;
    function IsLeaf(): boolean;
    function GetOwnConfig(const secname: string; var conf: TPairStrings): boolean;
    function WriteConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
    procedure GetCompleteConfig(var conf: TPairStrings);
    procedure Clear();
  end;

  TConfigReader = class
  protected
    t_croot:    TConfigItem;
    t_cfgfiles: TStrings;           //to save file names , which are loaded
    a_fstemps:  array of TDateTime; //to save time stemps of the loaded files
    t_names:    TStrings;           //to save all names, in order to check dopplicated name

  protected
    function AddConfigItem(const myname: string; const parentname: string = CSTR_ROOT): boolean;
    function ReadFromIni(const sfile: string): boolean;
    function WriteToIni(var slines: TStrings; const bRoot: boolean = false): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    function ReadFromFile(const srcfile: string; const bforce: boolean = false; const bappend: boolean = false): boolean; virtual;
    function SaveToFile(const destfile: string; const cf: EConfigFormat = CF_INI): boolean;
    procedure Clear();
  end;

implementation
uses SysUtils, IniFiles, StrUtils;
const
  CSTR_ARTICLE: string = 'ARTICLE_NR_';
  CSTR_VARIANT: string = 'VARIANTE_';
  CSTR_PARENT:  string = 'PSETTINGS';

function TConfigItem.FindConfigItem(const sname: string; var citem: TConfigItem): boolean;
var i_idx: integer;
begin
  result := false;
  if SameText(sname, s_confname) then begin
    citem := self;
    result := true;
  end else begin
    i_idx := t_children.IndexOf(sname);
    if (i_idx >= 0) then begin
      result := true;
      citem := TConfigItem(t_children.Objects[i_idx]);
    end;
  end;
end;

function TConfigItem.GetChildren(): integer;
begin
  result := t_children.Count;
end;

function TConfigItem.GetChild(idx: integer): TConfigItem;
begin
  result := TConfigItem(t_children.Objects[idx]);
end;

function TConfigItem.WriteToIni(var slines: TStrings): boolean;
var i: integer; t_citem: TConfigItem;
begin
  slines.Add('[' + s_confname + ']');
  if assigned(t_parent) then slines.Add(CSTR_PARENT + t_parent.ConfigName);
  slines.AddStrings(t_config.Pairs);
  result := true;

  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigItem(t_children.Objects[i]);
    result := t_citem.WriteToIni(slines);
  end;
end;

constructor TConfigItem.Create(const confname: string; const parent: TConfigItem);
begin
  inherited Create();
  s_confname := confname;
  t_parent := parent;
  t_children := TStringList.Create();
  t_children.CaseSensitive := false;
  t_children.Duplicates := dupError;
  t_config := TPairStrings.Create();
end;

destructor TConfigItem.Destroy();
begin
  Clear();
  t_children.Free();
  t_config.Free();
  inherited Destroy();
end;

function TConfigItem.AddChild(const sname: string): TConfigItem;
var t_citem: TConfigItem;
begin
  result := nil;
  t_citem := TConfigItem.Create(sname, self);
  try
    if (t_children.AddObject(sname, t_citem) >= 0) then result := t_citem;
  except
    t_citem.Free();
  end;
end;

function TConfigItem.HasChildren(): boolean;
begin
  result := (t_children.Count > 0);
end;

function TConfigItem.IsLeaf(): boolean;
begin
  result := (not HasChildren());
end;

//Breadth-First Search (BFS) in the tree
function TConfigItem.GetOwnConfig(const secname: string; var conf: TPairStrings): boolean;
var i: integer; t_citem: TConfigItem;
begin
  result := false; conf.Clear();
  if FindConfigItem(secname, t_citem) then conf.AddPairs(t_citem.Config.Pairs)
  else begin
    for i := 0 to t_children.Count -1 do begin
      t_citem :=  TConfigItem(t_children.Objects[i]);
      if t_citem.FindConfigItem(secname, t_citem) then begin
        conf.AddPairs(t_citem.Config.Pairs);
        break;
      end;
    end;
  end;
end;

function TConfigItem.WriteConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
begin
  result := false;
  case cf of
    CF_INI: result := WriteToIni(slines);
    CF_XML: ; //it is not yet implemented
  end;
end;

procedure TConfigItem.GetCompleteConfig(var conf: TPairStrings);
begin
  if assigned(t_parent) then t_parent.GetCompleteConfig(conf);
  conf.AddPairs(t_config.Pairs);
end;

procedure TConfigItem.Clear();
var i: integer;
begin
  t_config.Clear();
  for i := 0 to t_children.Count - 1 do t_children.Objects[i].Free();
  t_children.Clear();
end;

function TConfigReader.AddConfigItem(const myname, parentname: string): boolean;
var t_ciparent, t_citem: TConfigItem;
begin
  result := (t_names.IndexOf(myname) < 0);
  if result then begin
    if (not t_croot.FindConfigItem(parentname, t_ciparent)) then t_ciparent := t_croot;
    t_citem := t_ciparent.AddChild(myname);
    result := assigned(t_citem);
    if result then t_names.AddObject(myname, t_citem); //for linearly searching
  end;
end;

function TConfigReader.ReadFromIni(const sfile: string): boolean;
var t_fdatetime: TDateTime; t_inifile: TIniFile; i_idx: integer; b_overwrite: boolean;
    t_secnames, t_secvals: TStrings; s_name, s_parent: string; i: integer;
begin
  result := FileExists(sfile);
  if result then begin
    FileAge(sfile, t_fdatetime);
    i_idx := t_cfgfiles.IndexOf(sfile);
    if (i_idx >= 0) then b_overwrite := (t_fdatetime <> a_fstemps[i_idx])
    else b_overwrite := false;

    t_inifile := TIniFile.Create(sfile);
    t_secnames := TStringList.Create();
    t_secvals := TStringList.Create();

    t_inifile.ReadSections(t_secnames);
    for i := 0 to t_secnames.Count - 1 do begin
      if (StartsText(CSTR_VARIANT, t_secnames[i]) or StartsText(CSTR_ARTICLE, t_secnames[i])) then begin
      
      end;

    end;


    t_secvals.Free();
    t_secnames.Free();
    t_inifile.Free();
    //todo:
  end;
  {  if (bforce or b_update) then begin

      t_inifile.ReadSections();
      if result then begin
        t_cfgfiles.Add(sfile);
        SetLength(a_fstemps, t_cfgfiles.Count);
        a_fstemps[t_cfgfiles.Count - 1] := t_fdatetime;
      end else AddMessage(format('Failed to read this file (%s)', [sfile]), ML_ERROR);
      
    end else AddMessage('This file is already loaded.', ML_INFO);
  end else AddMessage(format('This file is NOT found (%s).', [sfile]), ML_WARNING); }
end;

function TConfigReader.WriteToIni(var slines: TStrings; const bRoot: boolean): boolean;
var i: integer;
begin
  result := false;
  if bRoot then  result := t_croot.WriteToIni(slines)
  else begin
    for i := 0 to t_croot.ChildCount - 1 do begin
      result := t_croot.Children[i].WriteToIni(slines);
      if (not result) then break;
    end;
  end;
end;

constructor TConfigReader.Create();
begin
  t_croot := TConfigItem.Create(CSTR_ROOT);
  t_cfgfiles := TStringList.Create();
  t_names := TStringList.Create()
end;

destructor TConfigReader.Destroy();
begin
  t_croot.Free();
  t_cfgfiles.Free();
  t_names.Free();
end;

function TConfigReader.ReadFromFile(const srcfile: string; const bforce: boolean; const bappend: boolean): boolean;
var e_cf: EConfigFormat;
begin
  result := false;
  e_cf := CF_INI;
  case e_cf of
    CF_INI: result := ReadFromIni(srcfile);
    CF_XML: ; //todo:
  end;
  
end;

function TConfigReader.SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
var t_lines: TStrings;
begin
  result := false;
  t_lines := TStringList.Create();
  case cf of
    CF_INI: result := t_croot.WriteToIni(t_lines);
    CF_XML: ; //todo:
  end;
  t_lines.Free();
end;

procedure TConfigReader.Clear();
begin
  t_croot.Clear();
  t_cfgfiles.Clear;
  t_names.Clear();
  SetLength(a_fstemps, 0);
end;

end.
