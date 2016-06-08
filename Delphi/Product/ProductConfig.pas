unit ProductConfig;

interface
uses Classes, IniFiles, PairStrings, GenUtils, ComCtrls;
type
  EConfigFormat = (
                CF_UNKNOWN,
                CF_INI,
                CF_XML
                );

  TConfigItem = class
  protected
    t_parent:   TConfigItem;
    s_confname: string;
    t_config:   TPairStrings;
    t_children: TStringList;
    b_visible:  boolean;
  protected
    function FindConfigItem(const sname: string): TConfigItem;
    function GetChildren(): integer;
    function GetChild(idx: integer): TConfigItem; overload;
    function GetChild(const sname: string; var idx: integer): TConfigItem; overload;
    function WriteToIni(var slines: TStrings): boolean;
    function WriteToXML(const destfile): boolean;

  public
    constructor Create(const confname: string; const parent: TConfigItem = nil);
    destructor Destroy(); override;

    property ConfigName: string read s_confname write s_confname;
    property Config: TPairStrings read t_config;
    property Parent: TConfigItem read t_parent write t_parent;
    property ChildCount: integer read GetChildren;
    property Children[idx: integer]: TConfigItem read GetChild;

    function AddChild(const sname: string): TConfigItem; overload;
    function AddChild(citem: TConfigItem): TConfigItem; overload;
    function MoveChildTo(const sname: string; citem: TConfigItem): TConfigItem;
    function HasChildren(): boolean;
    function GetOwnConfig(const secname: string; var conf: TPairStrings): boolean;
    function GetFullConfig(var conf: TPairStrings): boolean;
    function SaveConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
    function BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode): boolean;
    function IsVisible(): boolean;
    procedure Clear();
    procedure ResetParent(parent: TConfigItem);
    procedure UpdateConfig(const vals: TStrings);
    procedure Filter(const varname, filtertext: string);
    procedure Unfilter();
  end;

  TProdConfigurator = class
  protected
    t_croot:    TConfigItem;        //root of the config items
    t_curitem:  TConfigItem;        //to point config item, which is selected
    t_cfgfiles: TStrings;           //to save file names , which are loaded
    a_fstemps:  array of TDateTime; //to save time stemps of the loaded files
    t_names:    TStringList;        //to arrange in order with names
    t_ids:      TStringList;        //to arrange in order with id_string
    t_curconf:  TPairStrings;       //to save a settings for the current selection
    s_filtervar:string;             //to save a variable name in the config to filter

  protected
    function AddConfigItem(const confname: string; const vals: TStrings): boolean;
    function ReadFromIni(const sfile: string; const bcover: boolean = false): boolean;
    function BuildTreeFromIni(const ini: TIniFile): boolean;
    function WriteToIni(var slines: TStrings; const bRoot: boolean = false): boolean;
    function ReadFromXML(const sfile: string; const bcover: boolean = false): boolean;
    function WriteToXML(const sfile: string; const bRoot: boolean = false): boolean;
    function GetConfigItem(const name: string): TConfigItem; overload;
    function GetConfigItem(const idx: integer): TConfigItem; overload;
    function GetOwnConfig(const name: string): TPairStrings; overload;
    function GetOwnConfig(const citem: TConfigItem): TPairStrings; overload;
    function GetFullConfig(const name: string): TPairStrings; overload;
    function GetFullConfig(const citem: TConfigItem): TPairStrings; overload;
    function GetOwnCurConfig(): TPairStrings;
    function GetFullCurConfig(): TPairStrings;
    function GetCount(): integer;

  public
    constructor Create();
    destructor Destroy(); override;

    property ConfigRoot: TConfigItem read t_croot;
    property ConfigItem[const name: string]: TConfigItem read GetConfigItem;
    property OwnConfig[const name: string]: TPairStrings read GetOwnConfig;
    property FullConfig[const name: string]: TPairStrings read GetFullConfig;
    property CurConfigItem: TConfigItem read t_curitem;
    property OwnCurConfig: TPairStrings read GetOwnCurConfig;
    property FullCurConfig: TPairStrings read GetFullCurConfig;
    property Count: integer read GetCount;
    property FilterVar: string read s_filtervar write s_filtervar;

    function ReadFromFile(const cfgfile: string; const bforce: boolean = false; const bappend: boolean = false): boolean; virtual;
    function SaveToFile(const destfile: string; const cf: EConfigFormat = CF_INI): boolean;
    procedure UpdateTreeView(var trv: TTreeView; const bclear: boolean = true);
    procedure UpdateListView(var lsv: TListView; const bfull: boolean = false; const bsorted: boolean = false; const bclear: boolean = true);
    procedure Select(const sname: string);
    procedure Filter(const ftext: string);
    procedure Clear();
  end;

implementation
uses SysUtils, StrUtils, Dialogs;
const
  CSTR_ARTICLE: string = 'ARTICLE_';
  CSTR_VARIANT: string = 'VARIANTE_';
  CSTR_FAMILY:  string = 'FAMILY_';
  CSTR_VARIANT_PARENT:  string = 'FAMILY';
  CSTR_ARTICLE_PARENT:  string = 'VARIANT';
  CSTR_ID_STRING:       string = 'ID_STRING';

function TConfigItem.FindConfigItem(const sname: string): TConfigItem;
var i, i_idx: integer; t_child: TConfigItem;
begin
  result := nil;
  if SameText(sname, s_confname) then result := self
  else if t_children.Find(sname, i_idx) then result := TConfigItem(t_children.Objects[i_idx])
  else begin
    for i := 0 to t_children.Count - 1 do begin
      t_child := TConfigItem(t_children.Objects[i]);
      if assigned(t_child) then begin
        result := t_child.FindConfigItem(sname);
        if assigned(result) then break;
      end;
    end;
  end;
end;

function TConfigItem.GetChildren(): integer;
begin
  result := t_children.Count;
end;

function TConfigItem.GetChild(idx: integer): TConfigItem;
begin
  result := nil;
  if ((idx >= 0) and (idx < t_children.Count)) then result := TConfigItem(t_children.Objects[idx]);
end;

function TConfigItem.GetChild(const sname: string; var idx: integer): TConfigItem;
begin
  idx := -1;
  if t_children.Find(sname, idx) then result := TConfigItem(t_children.Objects[idx])
  else result := nil;
end;

function TConfigItem.WriteToIni(var slines: TStrings): boolean;
var i: integer; t_citem: TConfigItem;
begin
  slines.Add('[' + s_confname + ']');
  if assigned(t_parent) then begin
    if StartsText(CSTR_ARTICLE, s_confname) then slines.Add(CSTR_ARTICLE_PARENT + '=' + t_parent.ConfigName)
    else slines.Add(CSTR_VARIANT_PARENT + '=' + t_parent.ConfigName)
  end;
  slines.AddStrings(t_config.Pairs);
  result := true;

  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigItem(t_children.Objects[i]);
    result := t_citem.WriteToIni(slines);
  end;
end;

function TConfigItem.WriteToXML(const destfile): boolean;
begin
  result := false;
  //todo:
end;

constructor TConfigItem.Create(const confname: string; const parent: TConfigItem);
begin
  inherited Create();
  s_confname := confname;
  t_parent := parent;
  t_children := TStringList.Create();
  t_children.CaseSensitive := false;
  t_children.Sorted := true;
  t_config := TPairStrings.Create();
  b_visible := true;
end;

destructor TConfigItem.Destroy();
begin
  Clear();
  t_children.Free();
  t_config.Free();
  inherited Destroy();
end;

function TConfigItem.AddChild(const sname: string): TConfigItem;
var i_idx: integer;
begin
  if t_children.Find(sname, i_idx) then begin
    result := TConfigItem(t_children.Objects[i_idx]);
    //todo: dupplicated???
  end else begin
    result := TConfigItem.Create(sname, self);
    t_children.AddObject(sname, result);
  end;
end;

function TConfigItem.AddChild(citem: TConfigItem): TConfigItem;
var i_idx: integer;
begin
  result := citem;
  if t_children.Find(citem.ConfigName, i_idx) then begin
    //todo: dupplicated???
    result := TConfigItem(t_children.Objects[i_idx]);
    citem.Free();
  end else begin
    t_children.AddObject(citem.ConfigName, citem);
    citem.Parent := self;
  end;
end;

function TConfigItem.MoveChildTo(const sname: string; citem: TConfigItem): TConfigItem;
var i_idx: integer;
begin
  result := GetChild(sname, i_idx);
  if (assigned(result) and assigned(citem)) then begin
    t_children.Delete(i_idx);
    citem.AddChild(result);
  end;
end;

function TConfigItem.HasChildren(): boolean;
begin
  result := (t_children.Count > 0);
end;

//Breadth-First Search (BFS) in the tree
function TConfigItem.GetOwnConfig(const secname: string; var conf: TPairStrings): boolean;
var t_citem: TConfigItem;
begin
  result := false; conf.Clear();
  t_citem := FindConfigItem(secname);
  if assigned(t_citem) then begin
    conf.AddPairs(t_citem.Config.Pairs);
    result := true;
  end;
end;

function TConfigItem.GetFullConfig(var conf: TPairStrings): boolean;
begin
  result := true;
  if assigned(t_parent) then result := t_parent.GetFullConfig(conf);
  conf.AddPairs(t_config.Pairs, true);
end;

function TConfigItem.SaveConfig(var slines: TStrings; const cf: EConfigFormat): boolean;
begin
  result := false;
  case cf of
    CF_INI: result := WriteToIni(slines);
    CF_XML: ; //it is not yet implemented
  end;
end;

function TConfigItem.BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode): boolean;
var i: integer; t_curnode: TTreeNode; t_citem: TConfigItem;
begin
  result := true;
  for i := 0 to ChildCount - 1 do begin
    t_citem := TConfigItem(t_children.Objects[i]);
    if t_citem.IsVisible() then begin
      if assigned(trvNode) then t_curnode := trvnodes.AddChild(trvNode, t_citem.ConfigName)
      else t_curnode := trvnodes.Add(nil, t_citem.ConfigName);
      result := t_citem.BuildTreeNode(trvnodes, t_curnode);
    end;
  end;
end;

function TConfigItem.IsVisible(): boolean;
var i: integer; t_citem: TConfigItem;
begin
  result := b_visible;
  if not b_visible then begin
    for i := 0 to t_children.Count - 1 do begin
      t_citem := TConfigItem(t_children.Objects[i]);
      if t_citem.IsVisible() then begin
        result := true;
        break;
      end;
    end;
  end;
end;

procedure TConfigItem.Clear();
var i: integer;
begin
  t_config.Clear();
  for i := 0 to t_children.Count - 1 do t_children.Objects[i].Free();
  t_children.Clear();
end;

procedure TConfigItem.ResetParent(parent: TConfigItem);
begin
  if ((t_parent <> parent) and assigned(parent)) then begin
    if assigned(t_parent) then t_parent.MoveChildTo(s_confname, parent);
  end;
end;

procedure TConfigItem.UpdateConfig(const vals: TStrings);
begin
  t_config.Clear();
  t_config.AddPairs(vals);
end;

procedure TConfigItem.Filter(const varname, filtertext: string);
var s_value: string; i: integer; t_citem: TConfigItem;
begin
  if (filtertext = '') then b_visible := true
  else begin
    t_config.GetPairValue(varname, s_value);
    b_visible := ContainsText(s_value, filtertext);
  end;
  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigItem(t_children.Objects[i]);
    t_citem.Filter(varname, filtertext);
  end;
end;

procedure TConfigItem.Unfilter();
var i: integer; t_citem: TConfigItem;
begin
  b_visible := true;
  for i := 0 to t_children.Count - 1 do begin
    t_citem := TConfigItem(t_children.Objects[i]);
    t_citem.Unfilter();;
  end;
end;

//add a config section into root firstly. They will arranged by BuildConfigTree() later
function TProdConfigurator.AddConfigItem(const confname: string; const vals: TStrings): boolean;
var t_citem: TConfigItem; i_idx: integer; s_id: string;
begin
  result := (not t_names.Find(confname, i_idx));
  if result then begin
    t_citem := t_croot.AddChild(confname);
    t_names.AddObject(confname, t_citem);
    t_citem.UpdateConfig(vals);
    s_id := vals.Values[CSTR_ID_STRING];
    if (not t_ids.Find(s_id, i_idx)) then t_ids.AddObject(s_id, t_citem)
    else ; //todo: dupplicated id, warning??
  end;
end;

function TProdConfigurator.ReadFromIni(const sfile: string; const bcover: boolean): boolean;
var t_inifile: TIniFile; t_secnames, t_secvals: TStrings; s_name: string; i: integer;
begin
  result := false;
  t_inifile := TIniFile.Create(sfile);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();

  t_inifile.ReadSections(t_secnames);
  for i := 0 to t_secnames.Count - 1 do begin
    if (StartsText(CSTR_VARIANT, t_secnames[i]) or
        StartsText(CSTR_ARTICLE, t_secnames[i]) or
        StartsText(CSTR_FAMILY, t_secnames[i])) then
    begin
      s_name := t_secnames[i];
      t_secvals.Clear();
      t_inifile.ReadSectionValues(s_name, t_secvals);
      if StartsText(CSTR_ARTICLE, t_secnames[i]) then t_secvals.Values[CSTR_ARTICLE_PARENT] := '' //remove line of setting for article parent
      else t_secvals.Values[CSTR_VARIANT_PARENT] := ''; //remove line of setting for variant parent
      result := AddConfigItem(s_name, t_secvals);
      if not result then break;
    end;
  end;
  if result then result := BuildTreeFromIni(t_inifile);

  t_secvals.Free();
  t_secnames.Free();
  t_inifile.Free();
end;

function TProdConfigurator.BuildTreeFromIni(const ini: TIniFile): boolean;
var i: integer; t_citem, t_parent: TConfigItem; t_secvals: TStrings; s_name, s_parent: string;
begin
  t_secvals := TStringList.Create();
  for i := 0 to t_names.Count - 1 do begin
    s_name := t_names[i];
    t_citem := TConfigItem(t_names.Objects[i]);
    t_secvals.Clear();
    ini.ReadSectionValues(s_name, t_secvals);
    if StartsText(CSTR_ARTICLE, s_name) then s_parent := t_secvals.Values[CSTR_ARTICLE_PARENT]
    else s_parent := t_secvals.Values[CSTR_VARIANT_PARENT];

    t_parent := GetConfigItem(s_parent);
    if assigned(t_parent) then t_citem.ResetParent(t_parent);
  end;
  t_secvals.Free();
  result := true;
end;

function TProdConfigurator.WriteToIni(var slines: TStrings; const bRoot: boolean): boolean;
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

function TProdConfigurator.ReadFromXML(const sfile: string; const bcover: boolean): boolean;
begin
  result := false;
  //todo:
end;

function TProdConfigurator.WriteToXML(const sfile: string; const bRoot: boolean): boolean;
begin
  result := false;
  //todo:
end;

function TProdConfigurator.GetConfigItem(const name: string): TConfigItem;
var i_idx: integer;
begin
  if t_names.Find(name, i_idx) then result := GetConfigItem(i_idx)
  else result := nil;
end;

function TProdConfigurator.GetConfigItem(const idx: integer): TConfigItem;
begin
  result := nil;
  if (idx >=0) and (idx < t_names.Count) then result := TConfigItem(t_names.Objects[idx]);
end;

function TProdConfigurator.GetOwnConfig(const name: string): TPairStrings;
begin
  result := GetOwnConfig(GetConfigItem(name));
end;

function TProdConfigurator.GetOwnConfig(const citem: TConfigItem): TPairStrings;
begin
  t_curconf.Clear();
  if assigned(citem) then t_curconf.AddPairs(citem.Config.Pairs);
  result := t_curconf;
end;

function TProdConfigurator.GetFullConfig(const name: string): TPairStrings;
begin
  result := GetFullConfig(GetConfigItem(name));
end;

function TProdConfigurator.GetFullConfig(const citem: TConfigItem): TPairStrings;
begin
  t_curconf.Clear();
  if assigned(citem) then citem.GetFullConfig(t_curconf);
  result := t_curconf;
end;

function TProdConfigurator.GetOwnCurConfig(): TPairStrings;
begin
  result := GetOwnConfig(t_curitem);
end;

function TProdConfigurator.GetFullCurConfig(): TPairStrings;
begin
  result := GetFullConfig(t_curitem);
end;

function TProdConfigurator.GetCount(): integer;
begin
  result := t_names.Count;
end;

constructor TProdConfigurator.Create();
begin
  t_croot := TConfigItem.Create('ROOT');
  t_cfgfiles := TStringList.Create();
  t_names := TStringList.Create();
  t_names.CaseSensitive := false;
  t_names.Sorted := true;
  t_ids := TStringList.Create();
  t_ids.CaseSensitive := false;
  t_ids.Sorted := true;
  t_curconf := TPairStrings.Create();
  t_curitem := nil;
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
      t_cfgfiles.Add(s_filepath);
      SetLength(a_fstemps, t_cfgfiles.Count);
      a_fstemps[t_cfgfiles.Count - 1] := t_fdatetime;
    end;
  end;  
end;

procedure TProdConfigurator.UpdateTreeView(var trv: TTreeView; const bclear: boolean);
begin
  trv.Enabled := false;
  if bclear then begin
    trv.ClearSelection();
    trv.Items.Clear();
  end;
  t_croot.BuildTreeNode(trv.Items, nil);
  trv.Enabled := true;
end;

procedure TProdConfigurator.UpdateListView(var lsv: TListView; const bfull: boolean; const bsorted: boolean; const bclear: boolean);
var t_litem: TListItem; t_names: TStrings; s_value: string; i: integer;
begin
  lsv.Enabled := false;
  if bclear then lsv.Items.Clear();
  if bfull then GetFullCurConfig()
  else GetOwnCurConfig();

  t_names := TStringList.Create();
  TStringList(t_names).CaseSensitive := false;
  TStringList(t_names).Sorted := bsorted;
  t_curconf.GetPairNames(t_names);
  for i := 0 to t_names.Count - 1 do begin
    t_curconf.GetPairValue(t_names[i], s_value);
    t_litem := lsv.Items.Add();
    t_litem.Caption := t_names[i];
    t_litem.SubItems.Add(s_value);
  end;
  t_names.Free();
  lsv.Enabled := true;
end;

function TProdConfigurator.SaveToFile(const destfile: string; const cf: EConfigFormat): boolean;
var t_lines: TStrings;
begin
  t_lines := TStringList.Create();
  case cf of
    CF_INI: result := t_croot.WriteToIni(t_lines);
    CF_XML: result := t_croot.WriteToXML(destfile);
    else result := false;
  end;
  t_lines.Free();
end;

procedure TProdConfigurator.Select(const sname: string);
begin
  t_curitem := GetConfigItem(sname);
end;

procedure TProdConfigurator.Filter(const ftext: string);
begin
  if (ftext = '') then t_croot.Unfilter()
  else t_croot.Filter(s_filtervar, ftext);
end;

procedure TProdConfigurator.Clear();
begin
  t_croot.Clear();
  t_cfgfiles.Clear;
  t_names.Clear();
  t_ids.Clear();
  SetLength(a_fstemps, 0);
  t_curitem := nil;
  t_curconf.Clear();
end;

end.
