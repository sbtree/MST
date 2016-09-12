unit ProductConfig;

interface
uses Classes, IniFiles, StringPairs, ComCtrls;
type
  EConfigFormat = (
                CF_UNKNOWN,
                CF_INI,
                CF_XML
                );

  TConfigSection = class
  protected
    t_parent:   TConfigSection;
    s_confname: string;
    t_config:   TStringPairs;
    t_children: TStringList;
    b_visible:  boolean;
  protected
    function FindConfig(const sname: string): TConfigSection;
    function GetChildren(): integer;
    function GetChild(idx: integer): TConfigSection; overload;
    function GetChild(const sname: string; var idx: integer): TConfigSection; overload;
    function WriteToIni(var slines: TStrings): boolean;
    function WriteToXML(const destfile): boolean;
    function IsVisible(): boolean;
    function GetConfigId(): string;
    procedure SetConfigId(cid: string);

  public
    constructor Create(const confname: string; const parent: TConfigSection = nil);
    destructor Destroy(); override;

    property ConfigName: string read s_confname write s_confname;
    property ConfigId: string read GetConfigId write SetConfigId;
    property ConfigVars: TStringPairs read t_config;
    property Visible: boolean read IsVisible write b_visible;
    property Parent: TConfigSection read t_parent write t_parent;
    property ChildCount: integer read GetChildren;
    property Children[idx: integer]: TConfigSection read GetChild;

    function CreateChild(const sname: string): TConfigSection;
    function AddChild(citem: TConfigSection): boolean;
    function TakeChild(const sname: string): TConfigSection;
    function MoveChildTo(const sname: string; dest: TConfigSection): TConfigSection;
    function HasChildren(): boolean;
    function GetOwnConfig(const secname: string; var conf: TStringPairs): boolean;
    function GetFullConfig(var conf: TStringPairs): boolean;
    function GetValue(const varname: string; var varval: string): boolean;
    function GetValueFromParent(const varname: string; var varval: string): boolean;
    function SaveConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
    function UpdateConfigFrom(const conf: TConfigSection): boolean;
    function BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode): boolean;
    procedure TakeToRemove(var conflist: TStrings);
    procedure Clear();
    procedure CleanConfig();
    procedure CompleteOwnConfig();
    procedure ResetParent(parent: TConfigSection);
    procedure SetConfigVars(const vals: TStrings);
    procedure Filter(const varname, filtertext: string);
    procedure Unfilter();
  end;

  TProdConfigurator = class
  protected
    t_croot:    TConfigSection;     //root of the config items
    t_cursec:   TConfigSection;     //to point config item, which is selected
    t_cfgfiles: TStrings;           //to save file names , which are loaded
    a_fstemps:  array of TDateTime; //to save time stemps of the loaded files
    t_names:    TStringList;        //to arrange in order with names
    t_ids:      TStringList;        //to arrange in order with id_string
    t_curconf:  TStringPairs;       //to save a settings for the current selection
    s_filtervar:string;             //to save a variable name in the config to filter
    s_filtertxt:string;             //to save current filter text

  protected
    function AddConfig(const confname: string; const vals: TStrings): TConfigSection;
    function ReadFromIni(const sfile: string; const bcover: boolean = false): boolean;
    function BuildTreeFromIni(const ini: TIniFile): boolean;
    function WriteToIni(var slines: TStrings; const bRoot: boolean = true): boolean;
    function ReadFromXML(const sfile: string; const bcover: boolean = false): boolean;
    function WriteToXML(const sfile: string; const bRoot: boolean = true): boolean;
    function GetConfig(const name: string): TConfigSection; overload;
    function GetConfig(const idx: integer): TConfigSection; overload;
    function GetOwnConfigVars(const name: string): TStringPairs; overload;
    function GetOwnConfigVars(const citem: TConfigSection): TStringPairs; overload;
    function GetFullConfigVars(const name: string): TStringPairs; overload;
    function GetFullConfigVars(const citem: TConfigSection): TStringPairs; overload;
    function GetOwnCurConfigVars(): TStringPairs;
    function GetFullCurConfigVars(): TStringPairs;
    function GetCount(): integer;

  public
    constructor Create();
    destructor Destroy(); override;

    property ConfigRoot: TConfigSection read t_croot;
    property Config[const name: string]: TConfigSection read GetConfig;
    property OwnConfig[const name: string]: TStringPairs read GetOwnConfigVars;
    property FullConfig[const name: string]: TStringPairs read GetFullConfigVars;
    property CurConfig: TConfigSection read t_cursec;
    property OwnCurConfigVars: TStringPairs read GetOwnCurConfigVars;
    property FullCurConfigVars: TStringPairs read GetFullCurConfigVars;
    property Count: integer read GetCount;
    property FilterVarName: string read s_filtervar write s_filtervar;
    property FilterText: string read s_filtertxt;

    function ReadFromFile(const cfgfile: string; const bforce: boolean = false; const bappend: boolean = false): boolean; virtual;
    function SaveToFile(const destfile: string = ''; const cf: EConfigFormat = CF_INI): boolean;
    function CreateConfig(const confname: string; const parent: string = ''): boolean;
    function PromoteConfig(const confname, confref: string; const varnames: TStrings): boolean;
    function UpdateConfig(const confname, srcname: string): boolean;
    function MoveConfig(const confname, destname: string; const bfull: boolean = true): boolean;
    function RemoveConfig(const confname: string): boolean;
    procedure UpdateDefault(const confref: string);
    procedure UpdateTreeView(var trv: TTreeView; const bclear: boolean = true);
    procedure UpdateListView(var lsv: TListView; const bfull: boolean = false; const bsorted: boolean = false; const bclear: boolean = true);
    procedure Select(const sname: string);
    procedure Filter(const ftext: string);
    procedure CleanCurConfig();
    procedure CleanAllConfig();
    //procedure Optimize(); //todo
    procedure Clear();
  end;

implementation
uses SysUtils, StrUtils, Dialogs;
const
  CSTR_ARTICLE: string = 'ARTICLE_';
  CSTR_VARIANT: string = 'VARIANTE_';
  CSTR_FAMILY:  string = 'FAMILY_';
  CSTR_ROOT_NAME:       string = 'DEFAULT_SETTINGS';
  CSTR_VARIANT_PARENT:  string = 'FAMILY';
  CSTR_ARTICLE_PARENT:  string = 'VARIANT';
  CSTR_ID_STRING:       string = 'ID_STRING';

//Find config section in the whole branch with Breadth-First Search (BFS)
function TConfigSection.FindConfig(const sname: string): TConfigSection;
var i, i_idx: integer; t_child: TConfigSection;
begin
  result := nil;
  if SameText(sname, s_confname) then result := self
  else if t_children.Find(sname, i_idx) then result := TConfigSection(t_children.Objects[i_idx])
  else begin
    for i := 0 to t_children.Count - 1 do begin
      t_child := TConfigSection(t_children.Objects[i]);
      if assigned(t_child) then begin
        result := t_child.FindConfig(sname);
        if assigned(result) then break;
      end;
    end;
  end;
end;

//get count of its own children
function TConfigSection.GetChildren(): integer;
begin
  result := t_children.Count;
end;

//get child only in its own children, not in the grandchildren
function TConfigSection.GetChild(idx: integer): TConfigSection;
begin
  result := nil;
  if ((idx >= 0) and (idx < t_children.Count)) then result := TConfigSection(t_children.Objects[idx]);
end;

//get child only in its own children, not in the grandchildren
function TConfigSection.GetChild(const sname: string; var idx: integer): TConfigSection;
begin
  idx := -1;
  if t_children.Find(sname, idx) then result := TConfigSection(t_children.Objects[idx])
  else result := nil;
end;

//write current config and its child into a string list with ini-format
function TConfigSection.WriteToIni(var slines: TStrings): boolean;
var i: integer; t_citem: TConfigSection;
begin
  slines.Add('');
  slines.Add('[' + s_confname + ']');
  if assigned(t_parent) then begin
    if StartsText(CSTR_ARTICLE, s_confname) then slines.Add(CSTR_ARTICLE_PARENT + '=' + t_parent.ConfigName)
    else if (not SameText(t_parent.s_confname, CSTR_ROOT_NAME)) then slines.Add(CSTR_VARIANT_PARENT + '=' + t_parent.ConfigName)
  end;
  slines.AddStrings(t_config.Pairs);
  result := true;

  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigSection(t_children.Objects[i]);
    result := t_citem.WriteToIni(slines);
  end;
end;

function TConfigSection.WriteToXML(const destfile): boolean;
begin
  result := false;
  //todo:
end;

//constructor of the class
constructor TConfigSection.Create(const confname: string; const parent: TConfigSection);
begin
  inherited Create();
  s_confname := confname;
  t_parent := parent;
  t_children := TStringList.Create();
  t_children.CaseSensitive := false;
  t_children.Sorted := true;
  t_config := TStringPairs.Create();
  b_visible := true;
end;

//destructor of the class
destructor TConfigSection.Destroy();
begin
  Clear();
  t_children.Free();
  t_config.Free();
  inherited Destroy();
end;

//create a child with the given name if it does not exist in the child list
function TConfigSection.CreateChild(const sname: string): TConfigSection;
var i_idx: integer;
begin
  if t_children.Find(sname, i_idx) then result := nil
  else begin
    result := TConfigSection.Create(sname, self);
    t_children.AddObject(sname, result);
  end;
end;

//add a config as a child of current config if it does not in current config
function TConfigSection.AddChild(citem: TConfigSection): boolean;
var i_idx: integer;
begin
  if t_children.Find(citem.ConfigName, i_idx) then result := false
  else begin
    t_children.AddObject(citem.ConfigName, citem);
    citem.Parent := self;
    result := true;
  end;
end;

//take out a child from the child list by name
function TConfigSection.TakeChild(const sname: string): TConfigSection;
var i_idx: integer;
begin
  result := GetChild(sname, i_idx);
  if assigned(result) then t_children.Delete(i_idx);
end;

//move a child to the destination
function TConfigSection.MoveChildTo(const sname: string; dest: TConfigSection): TConfigSection;
begin
  result := TakeChild(sname);
  if (assigned(result) and assigned(dest)) then dest.AddChild(result);
end;

//indicate if current config has children
function TConfigSection.HasChildren(): boolean;
begin
  result := (t_children.Count > 0);
end;

//get own settings (exclusive settings of its parent) and save them in conf
function TConfigSection.GetOwnConfig(const secname: string; var conf: TStringPairs): boolean;
var t_citem: TConfigSection;
begin
  result := false; conf.Clear();
  t_citem := FindConfig(secname);
  if assigned(t_citem) then begin
    conf.AddPairs(t_citem.ConfigVars.Pairs);
    result := true;
  end;
end;

//get full settings (inclusive settings of its parent) and save them in conf
function TConfigSection.GetFullConfig(var conf: TStringPairs): boolean;
begin
  result := true;
  if assigned(t_parent) then result := t_parent.GetFullConfig(conf);
  conf.AddPairs(t_config.Pairs, true);
end;

//get value of a variable name
function TConfigSection.GetValue(const varname: string; var varval: string): boolean;
begin
  result := ConfigVars.GetPairValue(varname, varval);
end;

//recursive function, get first value of a variable which is firstly found in its parent or grandparents
function TConfigSection.GetValueFromParent(const varname: string; var varval: string): boolean;
begin
  result := false;
  if Assigned(t_parent) then begin
    result := t_parent.GetValue(varname, varval);
    if (not result) then result :=t_parent.GetValueFromParent(varname, varval);
  end;
end;

//save config in a string list with given format
function TConfigSection.SaveConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
begin
  result := false;
  case cf of
    CF_INI: result := WriteToIni(slines);
    CF_XML: ; //it is not yet implemented
  end;
end;

//update settings of current config from the given config
function TConfigSection.UpdateConfigFrom(const conf: TConfigSection): boolean;
var t_vars: TStringPairs;
begin
  result := false;
  if assigned(conf) then begin
    t_vars := TStringPairs.Create();
    if conf.GetFullConfig(t_vars) then begin
      //id_string has to be removed for updating because the config section has its own id
      t_vars.RemovePair(t_vars.Pairs.IndexOfName(CSTR_ID_STRING)); 
      result := (t_config.AddPairs(t_vars.Pairs, true) > 0);
      if result then CleanConfig();
    end;
    t_vars.Free();
  end;
end;

//build a tree node with its children
function TConfigSection.BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode): boolean;
var i: integer; t_curnode: TTreeNode; t_citem: TConfigSection;
begin
  result := true;
  for i := 0 to ChildCount - 1 do begin
    t_citem := TConfigSection(t_children.Objects[i]);
    if t_citem.Visible then begin
      if assigned(trvNode) then t_curnode := trvnodes.AddChild(trvNode, t_citem.ConfigName)
      else t_curnode := trvnodes.Add(nil, t_citem.ConfigName);
      result := t_citem.BuildTreeNode(trvnodes, t_curnode);
    end;
  end;
end;

//indicate if the config is visible
function TConfigSection.IsVisible(): boolean;
var i: integer; t_citem: TConfigSection;
begin
  result := b_visible;
  if not b_visible then begin
    for i := 0 to t_children.Count - 1 do begin
      t_citem := TConfigSection(t_children.Objects[i]);
      if t_citem.Visible then begin
        result := true;
        break;
      end;
    end;
  end;
end;

//get identifer of current config
function TConfigSection.GetConfigId(): string;
begin
  t_config.GetPairValue(CSTR_ID_STRING, result);
end;

//set identifer of current config
procedure TConfigSection.SetConfigId(cid: string);
begin
  t_config.SetPairValue(CSTR_ID_STRING, cid);
end;

//move the config section from the children list of its parent list and
//save it in to the given list variable
procedure TConfigSection.TakeToRemove(var conflist: TStrings);
var t_conf: TConfigSection; i: integer;
begin
  if assigned(t_parent) then t_parent.TakeChild(s_confname);
  for i := t_children.Count - 1 downto 0 do begin
    t_conf := GetChild(i);
    if assigned(t_conf) then t_conf.TakeToRemove(conflist);
  end;
  conflist.AddObject(s_confname, self);
end;

//clear setting of current config and its child
procedure TConfigSection.Clear();
var i: integer;
begin
  t_config.Clear();
  for i := 0 to t_children.Count - 1 do t_children.Objects[i].Free();
  t_children.Clear();
end;

//remove items of own string pairs, which have same value as its parent
procedure TConfigSection.CleanConfig();
var i: integer; t_child: TConfigSection; s_myval, s_parentval: string;
begin
  //clean settings in its own config
  for i := t_config.Count - 1 downto 0  do begin
    t_config.GetPairValue(i, s_myval);
    if GetValueFromParent(t_config.PairName[i], s_parentval) then begin
      if SameText(s_myval, s_parentval) then t_config.RemovePair(i);
    end;
  end;

  //clean settings in the config of its children
  for i := 0 to t_children.Count - 1 do begin
    t_child := TConfigSection(t_children.Objects[i]);
    t_child.CleanConfig();
  end;
end;

//complete own string pairs with the string pairs of its parent, but not overwrite
procedure TConfigSection.CompleteOwnConfig();
var t_spairs: TStringPairs;
begin
  t_spairs := TStringPairs.Create();
  if assigned(t_parent) then begin
    if t_parent.GetFullConfig(t_spairs) then t_config.AddPairs(t_spairs.Pairs, false);
  end;
  t_spairs.Free();
end;

procedure TConfigSection.ResetParent(parent: TConfigSection);
begin
  if ((t_parent <> parent) and assigned(parent)) then begin
    if assigned(t_parent) then t_parent.MoveChildTo(s_confname, parent);
  end;
end;

//set settings with the given string list of name-value-pair 
procedure TConfigSection.SetConfigVars(const vals: TStrings);
begin
  Clear();
  t_config.AddPairs(vals, true);
end;

//filter configs which include filtertext in the setting with varname
procedure TConfigSection.Filter(const varname, filtertext: string);
var s_value: string; i: integer; t_citem: TConfigSection;
begin
  //set if it is visible by itself
  if (filtertext = '') then b_visible := true
  else begin
    t_config.GetPairValue(varname, s_value);
    b_visible := ContainsText(s_value, filtertext);
  end;

  //set if its children are visible
  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigSection(t_children.Objects[i]);
    if t_citem.ConfigVars.FindPair(varname) then t_citem.Filter(varname, filtertext) //if it has its own value of this variable
    else t_citem.Visible := b_visible; //otherwise its visibility is same as its parent
  end;
end;

//cancel filtering
procedure TConfigSection.Unfilter();
var i: integer; t_citem: TConfigSection;
begin
  b_visible := true;
  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigSection(t_children.Objects[i]);
    t_citem.Unfilter();;
  end;
end;

//add a config section into root firstly. They will arranged by BuildTreeFromXXX() later
function TProdConfigurator.AddConfig(const confname: string; const vals: TStrings): TConfigSection;
var i_idx: integer; s_id: string;
begin
  result := GetConfig(confname);
  if (result = nil) then begin
    result := t_croot.CreateChild(confname);
    if assigned(result) then begin
      t_names.AddObject(confname, result);
      if assigned(vals) then result.SetConfigVars(vals);

      //check config id
      s_id := result.ConfigId;
      if (s_id = '') then begin
        s_id := confname;
        result.ConfigId := s_id;
      end;
      if (not t_ids.Find(s_id, i_idx)) then t_ids.AddObject(s_id, result)
      else s_id := ''; //todo: dupplicated id, warning??
    end;
  end else result := nil;
end;

//read configs from a ini-file. Only the sections, which have names started with CSTR_FAMILY, CSTR_VARIANT and CSTR_ARTICLE
function TProdConfigurator.ReadFromIni(const sfile: string; const bcover: boolean): boolean;
var t_inifile: TIniFile; t_secnames, t_secvals: TStrings; s_name: string; i: integer;
begin
  result := false;
  t_inifile := TIniFile.Create(sfile);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();

  t_inifile.ReadSections(t_secnames);
  t_inifile.ReadSectionValues(CSTR_ROOT_NAME, t_secvals);
  t_croot.SetConfigVars(t_secvals);
  for i := 0 to t_secnames.Count - 1 do begin
    s_name := t_secnames[i];
    if (StartsText(CSTR_VARIANT, s_name) or
        StartsText(CSTR_ARTICLE, s_name) or
        StartsText(CSTR_FAMILY, s_name)) then
    begin
      t_secvals.Clear();
      t_inifile.ReadSectionValues(s_name, t_secvals);
      if StartsText(CSTR_ARTICLE, s_name) then t_secvals.Values[CSTR_ARTICLE_PARENT] := '' //remove line of setting for article parent
      else t_secvals.Values[CSTR_VARIANT_PARENT] := ''; //remove line of setting for variant parent
      result := (AddConfig(s_name, t_secvals) <> nil);
      if not result then break;
    end;
  end;
  if result then result := BuildTreeFromIni(t_inifile);

  t_secvals.Free();
  t_secnames.Free();
  t_inifile.Free();
end;

//build tree structure from a file with ini-format
function TProdConfigurator.BuildTreeFromIni(const ini: TIniFile): boolean;
var i: integer; t_citem, t_parent: TConfigSection; t_secvals: TStrings; s_name, s_parent: string;
begin
  t_secvals := TStringList.Create();
  for i := 0 to t_names.Count - 1 do begin
    s_name := t_names[i];
    t_citem := TConfigSection(t_names.Objects[i]);
    t_secvals.Clear();
    ini.ReadSectionValues(s_name, t_secvals);
    if StartsText(CSTR_ARTICLE, s_name) then s_parent := t_secvals.Values[CSTR_ARTICLE_PARENT]
    else s_parent := t_secvals.Values[CSTR_VARIANT_PARENT];

    t_parent := GetConfig(s_parent);
    if assigned(t_parent) then t_citem.ResetParent(t_parent);
  end;
  t_secvals.Free();
  result := true;
end;

//write all configs into a string list with ini-format
function TProdConfigurator.WriteToIni(var slines: TStrings; const bRoot: boolean): boolean;
var i: integer;
begin
  result := false;
  slines.Add(';======================================================================================');
  slines.Add('; This file is created by Metronix program automatically.');
  slines.Add('; (c) Metronix 2016');
  slines.Add('; ' + DateTimeToStr(Now()));
  slines.Add(';======================================================================================');
  slines.Add('[revision]');
  slines.Add('VERSION=4.1.0.1.1');
  slines.Add('FILE_VISION=$Revision: 1.1$');
  if bRoot then  result := t_croot.WriteToIni(slines)
  else begin
    for i := 0 to t_croot.ChildCount - 1 do begin
      result := t_croot.Children[i].WriteToIni(slines);
      if (not result) then break;
    end;
  end;
end;

//read configs from a xml-file
function TProdConfigurator.ReadFromXML(const sfile: string; const bcover: boolean): boolean;
begin
  result := false;
  //todo:
end;

//write configs into an xml-files
function TProdConfigurator.WriteToXML(const sfile: string; const bRoot: boolean): boolean;
begin
  result := false;
  //todo:
end;

//get reference of a config by name
function TProdConfigurator.GetConfig(const name: string): TConfigSection;
var i_idx: integer;
begin
  if t_names.Find(name, i_idx) then result := GetConfig(i_idx)
  else result := nil;
end;

//get reference of a config by index
function TProdConfigurator.GetConfig(const idx: integer): TConfigSection;
begin
  if (idx >=0) and (idx < t_names.Count) then result := TConfigSection(t_names.Objects[idx])
  else result := nil;
end;

//get own settings of a config by name (exclusive the settings of its parent)
function TProdConfigurator.GetOwnConfigVars(const name: string): TStringPairs;
begin
  result := GetOwnConfigVars(GetConfig(name));
end;

//get own settings of a config by instance (exclusive the settings of its parent)
function TProdConfigurator.GetOwnConfigVars(const citem: TConfigSection): TStringPairs;
begin
  t_curconf.Clear();
  if assigned(citem) then t_curconf.AddPairs(citem.ConfigVars.Pairs);
  result := t_curconf;
end;

//get full settings of a config by name(inclusive the settings of its parent)
function TProdConfigurator.GetFullConfigVars(const name: string): TStringPairs;
begin
  result := GetFullConfigVars(GetConfig(name));
end;

//get full settings of a config by instance (inclusive the settings of its parent)
function TProdConfigurator.GetFullConfigVars(const citem: TConfigSection): TStringPairs;
begin
  t_curconf.Clear();
  if assigned(citem) then citem.GetFullConfig(t_curconf);
  result := t_curconf;
end;

//get own settings of current config (exclusive the settings of its parent)
function TProdConfigurator.GetOwnCurConfigVars(): TStringPairs;
begin
  result := GetOwnConfigVars(t_cursec);
end;

//get full settings of current config (inclusive the settings of its parent)
function TProdConfigurator.GetFullCurConfigVars(): TStringPairs;
begin
  result := GetFullConfigVars(t_cursec);
end;

//get count of the configs
function TProdConfigurator.GetCount(): integer;
begin
  result := t_names.Count;
end;

constructor TProdConfigurator.Create();
begin
  t_croot := TConfigSection.Create(CSTR_ROOT_NAME);
  t_cfgfiles := TStringList.Create();
  t_names := TStringList.Create();
  t_names.CaseSensitive := false;
  t_names.Sorted := true;
  t_ids := TStringList.Create();
  t_ids.CaseSensitive := false;
  t_ids.Sorted := true;
  t_curconf := TStringPairs.Create();
  t_cursec := nil;
  s_filtervar := CSTR_ID_STRING;
end;

destructor TProdConfigurator.Destroy();
begin
  t_croot.Free();
  t_cfgfiles.Free();
  t_names.Free();
  t_ids.Free();
  t_curconf.Free();
end;

//read configs from a file
function TProdConfigurator.ReadFromFile(const cfgfile: string; const bforce: boolean; const bappend: boolean): boolean;
var e_cf: EConfigFormat; i_idx: integer; t_fdatetime: TDateTime; b_cover: boolean; s_filepath: string;
begin
  s_filepath := ExpandFileName(cfgfile);
  result := FileExists(s_filepath);
  if result then begin
    FileAge(s_filepath, t_fdatetime);
    i_idx := t_cfgfiles.IndexOf(s_filepath);
    if (i_idx >= 0) then b_cover := (t_fdatetime <> a_fstemps[i_idx])
    else b_cover := false;

    if EndsText('.ini', s_filepath) then e_cf := CF_INI
    else if EndsText('.xml', s_filepath) then e_cf := CF_XML
    else e_cf := CF_UNKNOWN;

    case e_cf of
      CF_INI: result := ReadFromIni(s_filepath, b_cover);
      CF_XML: result := ReadFromXML(s_filepath, b_cover);
      else result := false;
    end;

    if result then begin
      t_cfgfiles.Add(cfgfile);
      SetLength(a_fstemps, t_cfgfiles.Count);
      a_fstemps[t_cfgfiles.Count - 1] := t_fdatetime;
    end;
  end;  
end;

//show and update all configs in a tree view
procedure TProdConfigurator.UpdateTreeView(var trv: TTreeView; const bclear: boolean);
begin
  trv.Enabled := false;
  if bclear then begin
    trv.ClearSelection();
    trv.Items.Clear();
    t_cursec := nil;
  end;
  t_croot.BuildTreeNode(trv.Items, nil);
  trv.Enabled := true;
end;

//show and update the settings of current config in a list view
procedure TProdConfigurator.UpdateListView(var lsv: TListView; const bfull: boolean; const bsorted: boolean; const bclear: boolean);
var t_litem: TListItem; t_varnames: TStrings; s_value: string; i: integer;
begin
  lsv.Enabled := false;
  if bclear then lsv.Items.Clear();
  if bfull then GetFullCurConfigVars()
  else GetOwnCurConfigVars();

  t_varnames := TStringList.Create();
  TStringList(t_varnames).CaseSensitive := false;
  TStringList(t_varnames).Sorted := bsorted;
  t_curconf.GetPairNames(t_varnames);
  for i := 0 to t_varnames.Count - 1 do begin
    t_curconf.GetPairValue(t_varnames[i], s_value);
    t_litem := lsv.Items.Add();
    t_litem.Caption := t_varnames[i];
    t_litem.SubItems.Add(s_value);
  end;
  t_varnames.Free();
  lsv.Enabled := true;
end;

//save all configs into a file
function TProdConfigurator.SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
var s_fname, s_ext: string; t_lines: TStrings;
begin
  s_fname := destfile;
  if (destfile = '') then begin
    if (length(t_cfgfiles[0]) > 4) then begin
      s_ext := ExtractFileExt(t_cfgfiles[0]);
      s_fname := LeftStr(t_cfgfiles[0], length(t_cfgfiles[0]) - length(s_ext)) + '_new' + s_ext;
    end;
  end else ;

  case cf of
    CF_INI: begin
      t_lines := TStringList.Create();
      result := WriteToIni(t_lines);
      t_lines.SaveToFile(s_fname);
      t_lines.Free();
    end;
    CF_XML: result := WriteToXML(s_fname);
    else result := false;
  end;
end;

//create a config as child of the given config
function TProdConfigurator.CreateConfig(const confname: string; const parent: string): boolean;
var t_parent, t_conf: TConfigSection;
begin
  result := false;
  t_parent := GetConfig(parent);
  if assigned(t_parent) then begin
    t_conf := AddConfig(confname, nil);
    if assigned(t_conf) then begin
      t_conf.ResetParent(t_parent);
      result := true;
    end;
  end;
end;

//create a new config with given name as parent of the given config.
//the given config and its siblings become children of this new config
//if they has the same values of the given variable names
function TProdConfigurator.PromoteConfig(const confname, confref: string; const varnames: TStrings): boolean;
var t_conf, t_confref, t_parent, t_child: TConfigSection; i: integer; t_spairs: TStringPairs;
begin
  result := false;
  if (not SameText(confname, confref)) then begin
    t_conf := AddConfig(confname, nil);
    t_confref := GetConfig(confref);
    if (assigned(t_conf) and assigned(t_confref)) then begin
      t_conf.UpdateConfigFrom(t_confref);
      t_parent := t_confref.Parent;
      t_confref.ResetParent(t_conf);
      if assigned(t_parent) then begin
        t_spairs := TStringPairs.Create();
        t_spairs.CopyValuesFrom(t_confref.ConfigVars, varnames);
        for i := t_parent.ChildCount - 1 downto 0 do begin
          t_child := t_parent.GetChild(i);
          if (assigned(t_child) and (t_conf <> t_child)) then begin
            if t_child.ConfigVars.HasSameValues(t_spairs, varnames) then begin
              t_child.ResetParent(t_conf);
              t_child.CleanConfig();
              Inc(result);
            end;
          end;
        end;
        t_conf.ResetParent(t_parent);
        t_spairs.Free();
      end;
      t_confref.CleanConfig();
    end;
  end;
end;

//the settings of the given config will be upgraded with those of the source config
function TProdConfigurator.UpdateConfig(const confname, srcname: string): boolean;
var t_src, t_dest: TConfigSection;
begin
  result := false;
  if (not SameText(confname, srcname)) then begin
    t_dest := GetConfig(confname);
    t_src := GetConfig(srcname);
    if (assigned(t_src) and assigned(t_dest)) then  result := t_dest.UpdateConfigFrom(t_src);
  end;
end;

//change the parent of the given config to the destination.
//The settings of the old parent will be brought with if bfull is true
//the settings are cleaned with the new parent after the change
function TProdConfigurator.MoveConfig(const confname, destname: string; const bfull: boolean ): boolean;
var t_conf, t_confdest: TConfigSection;
begin
  result := false;
  if (not SameText(confname, destname)) then begin
    t_conf := GetConfig(confname);
    if bfull then t_conf.CompleteOwnConfig();
    if destname = '' then t_confdest := t_croot
    else t_confdest := GetConfig(destname);
    if (assigned(t_conf) and assigned(t_confdest)) then begin
      t_conf.ResetParent(t_confdest);
      t_conf.CleanConfig();
      result := true;
    end;
  end;
end;

//remvoe a config section but root is not allowed to remove
function TProdConfigurator.RemoveConfig(const confname: string): boolean;
var t_conf: TConfigSection; i, i_idx: integer; t_conflist: TStrings; s_id: string;
begin
  result := false;
  t_conf := GetConfig(confname);
  if assigned(t_conf) then begin
    t_conflist := TStringList.Create();
    t_conf.TakeToRemove(t_conflist);
    for i := t_conflist.Count - 1 downto 0 do begin
      if t_names.Find(t_conflist.Strings[i], i_idx) then t_names.Delete(i_idx);
      t_conf := TConfigSection(t_conflist.Objects[i]);
      if assigned(t_conf) then begin
        s_id := t_conf.ConfigId;
        if t_ids.Find(s_id, i_idx) then t_ids.Delete(i_idx);
      end;
      t_conflist.Objects[i].Free();
    end;
    t_conflist.Clear();
    FreeAndNil(t_conflist);
  end;
end;

//update default settings from the given config section
procedure TProdConfigurator.UpdateDefault(const confref: string);
begin
  t_croot.UpdateConfigFrom(GetConfig(confref));
end;

//set current config by name
procedure TProdConfigurator.Select(const sname: string);
begin
  t_cursec := GetConfig(sname);
end;

//filter configs with ftext.
procedure TProdConfigurator.Filter(const ftext: string);
var i: integer; t_citem: TConfigSection;
begin
  if (ftext = '') then t_croot.Unfilter()
  else begin
    for i := 0 to t_croot.ChildCount - 1 do begin
      t_citem := t_croot.GetChild(i);
      if assigned(t_citem) then t_citem.Filter(s_filtervar, ftext);
    end;
  end;
  s_filtertxt := ftext;
end;

//clean the settings of current config
procedure TProdConfigurator.CleanCurConfig();
begin
  if assigned(t_cursec) then t_cursec.CleanConfig();
end;

//clean settings of all configs
procedure TProdConfigurator.CleanAllConfig();
begin
  t_croot.CleanConfig();
end;

//clear all information in the class
procedure TProdConfigurator.Clear();
begin
  t_croot.Clear();
  t_cfgfiles.Clear;
  t_names.Clear();
  t_ids.Clear();
  SetLength(a_fstemps, 0);
  t_cursec := nil;
  t_curconf.Clear();
end;

end.
