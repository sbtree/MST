unit TestConfig;

interface
uses Classes,ComCtrls,System.Generics.Collections, System.Generics.Defaults, ConfigBase;

function ExtractKeyValue(const keyval: string; var skey, sval: string; const sepa: char = '='): boolean;
function TestModeByStr(const stest: string; const default: integer = 4): integer;

type
  TStringDictionary = class(TDictionary<string, string>)
  protected
    c_fldseparator: char;
  public
    constructor Create();

    procedure UpdateBy(const keyvals: TStrings; const bcover: boolean = true); overload;
    procedure UpdateBy(const config: TStringDictionary; const bcover: boolean = true); overload;
    procedure ReduceBy(const config: TStringDictionary);
    function  FieldValue(const fldname: string; const fldindex: integer): string;

    property  FieldSeparator: char read c_fldseparator write c_fldseparator default '|';
  end;

  TProductConfig = class;
  TProductDictionary = class(TObjectDictionary<string, TProductConfig>)
  protected

  public
    constructor Create();
    //procedure ValueNotify(const TValue: TProductConfig; Action: TCollectionNotification); override;
  end;

  TProductConfig= class(TStringDictionary)
  protected
    s_prodname: string;
    b_visible:  boolean;
    t_parent:   TProductConfig;
    t_children: TProductDictionary;
  protected
    procedure SetProdName(const sname: string);
    procedure SetParent(parent: TProductConfig);
    procedure ReduceOwnSetting();
    procedure WriteToIni(var slines: TStrings);
    function  IsVisible(): boolean;
    function  GetChildrenCount(): integer;
    function  GetProductId(): string;
    procedure SetProductId(cid: string);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure GetParentSettings(var tsetting: TStringDictionary);
    procedure GetFullSettings(var tsetting: TStringDictionary);
    procedure GetOwnSettings(var tsetting: TStringDictionary);
    procedure BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode);

    function  DepthLevel(): integer;
    function  FindProduct(const sname: string): TProductConfig;
    function  HasChild(const sname: string): boolean;
    function  CreateChild(const sname: string): TProductConfig;
    function  GetChild(const sname: string): TProductConfig;
    function  AddChild(const child: TProductConfig): boolean;
    function  TakeChild(const sname: string): TProductConfig;
    function  UpdateChildName(const oldname, newname: string): boolean;
    function  GetRootProduct(): TProductConfig;

    property  ProductName: string read s_prodname write SetProdName;
    property  ProductParent: TProductConfig read t_parent write SetParent;
    property  ProductID: string read GetProductId write SetProductId;
    property  ChildrenCount: integer read GetChildrenCount;
    property  Visible: boolean read IsVisible write b_visible;
  end;

  TProductConfigurator = class(TConfigBase)
  protected
    t_prodroot: TProductConfig;
    t_prodcur:  TProductConfig;
    t_idnamemap:TStringDictionary;
    t_products: TProductDictionary;
  protected
    function  FindRootSettings(const secnames: TStrings; var sroot: string): boolean;
    function  ReadFromIni(const sfile: string): boolean; override;
    function  BuildProductFamily(): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetProductByName(const sname: string);
    procedure SetProductByID(const prodid: string);
    procedure UpdateTreeView(var trv: TTreeView; const bclear: boolean = true);
    procedure UpdateListView(var lsv: TListView; const bfull: boolean = false; const bsorted: boolean = false);
    //read all settings from a configuration file
    //write all settings into a configuration file
    //create a new product/family
    //update settings for a product/family
    //remove a a product/family
    //change parent of a product/family
    //show tree of the products/families in a GUI-component
    //property CurrentProduct: TProductConfig read t_curproduct write SetCurProduct;
  end;

var
  ProductDictionary: TProductDictionary;

implementation

uses SysUtils, StrUtils, IniFiles;
const
  CSTR_ARTICLE: string = 'ARTICLE_';
  CSTR_VARIANT: string = 'VARIANTE_';
  CSTR_FAMILY:  string = 'FAMILY_';
  CSTR_ROOT:    string = 'ROOT_';
  CSTR_GENERAL: string = 'GENERAL';
  CSTR_VARIANT_PARENT:  string = 'FAMILY';
  CSTR_ARTICLE_PARENT:  string = 'VARIANT';
  CSTR_ID_STRING:       string = 'ID_STRING';

//test mode: 0=HV_Test, 1=Rundlauftest, 2=PE_Test, 3=Dauertest, 4=Leistungstest
  CSTR_TEST_MODE: array[0..4] of string = ('HV', 'RLT', 'PE', 'DT','LT');

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

function TestModeByStr(const stest: string; const default: integer): integer;
begin
  result := default;
  if not TryStrToInt(stest, result) then begin //try directly to get the number from stest
    result := IndexText(stest, CSTR_TEST_MODE);  //map to a number if stest is given by text
    if result < 0 then result := default; //return default number if stest is unknown
  end;
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

//procedure TProductFamily.ValueNotify(const TValue: TProductConfig; Action: TCollectionNotification);
//begin
//  inherited ValueNotify(TValue, Action);
//  case Action of
//    cnAdded: TValue.SetParent(self);
//    cnRemoved: TValue.Free;
//    cnExtracted: TValue.SetParent(t_prodroot);
//  end;
//end;

constructor TProductDictionary.Create();
begin
  inherited Create(TOrdinalIStringComparer.Create);
end;

procedure TProductConfig.SetProdName(const sname: string);
begin
  if assigned(t_parent) then t_parent.UpdateChildName(s_prodname, sname);
  s_prodname := sname;
end;

procedure TProductConfig.SetParent(parent: TProductConfig);
begin
  if assigned(t_parent) then t_parent.TakeChild(s_prodname);
  t_parent := parent;
end;

procedure TProductConfig.ReduceOwnSetting();
var t_parentsetting: TStringDictionary;
begin
  t_parentsetting := TStringDictionary.Create();
  GetParentSettings(t_parentsetting);
  ReduceBy(t_parentsetting);
  t_parentsetting.Free();
end;

//write current config and its child into a string list with ini-format
procedure TProductConfig.WriteToIni(var slines: TStrings);
var t_config: TProductConfig; s_key: string;
begin
  slines.Add('');
  slines.Add('[' + s_prodname + ']');
  if assigned(t_parent) then begin
    if StartsText(CSTR_ARTICLE, s_prodname) then slines.Add(CSTR_ARTICLE_PARENT + '=' + t_parent.ProductName)
    else if (not StartsText(CSTR_ROOT, t_parent.ProductName)) then slines.Add(CSTR_VARIANT_PARENT + '=' + t_parent.ProductName)
  end;
  for s_key in self.Keys do slines.Add(s_key + '=' + self.Items[s_key]);
  for t_config in t_children.Values do t_config.WriteToIni(slines);
end;

//indicate if the config is visible
function TProductConfig.IsVisible(): boolean;
var t_config: TProductConfig;
begin
  result := b_visible;
  if not b_visible then begin
    for t_config in t_children.Values do begin
      result := t_config.Visible;
      if result then break;
    end;
  end;
end;

function TProductConfig.GetChildrenCount(): integer;
begin
  result := t_children.Count;
end;

//get identifer of current config
function TProductConfig.GetProductId(): string;
begin
  result := self.Items[CSTR_ID_STRING];
end;

//set identifer of current config
procedure TProductConfig.SetProductId(cid: string);
begin
  self.AddOrSetValue(CSTR_ID_STRING, cid);
end;

constructor TProductConfig.Create();
begin
  inherited Create;
  b_visible := true;
  t_children := TProductDictionary.Create;
  t_parent := nil;
end;

destructor TProductConfig.Destroy();
begin
  Clear;
  t_children.Clear;
  t_children.Free;
end;

procedure TProductConfig.GetFullSettings(var tsetting: TStringDictionary);
begin
  GetParentSettings(tsetting);
  tsetting.UpdateBy(self);
end;

procedure TProductConfig.GetOwnSettings(var tsetting: TStringDictionary);
begin
  tsetting.UpdateBy(self);
end;

//build a tree node with its children
procedure TProductConfig.BuildTreeNode(trvnodes: TTreeNodes; trvNode: TTreeNode);
var t_curnode: TTreeNode; t_config: TProductConfig;
begin
  if assigned(trvNode) then t_curnode := trvnodes.AddChild(trvNode, s_prodname)
  else t_curnode := trvnodes.Add(nil, s_prodname);
  for t_config in t_children.Values do t_config.BuildTreeNode(trvnodes, t_curnode);
end;

procedure TProductConfig.GetParentSettings(var tsetting: TStringDictionary);
begin
  if assigned(t_parent) then begin
    t_parent.GetParentSettings(tsetting);
    tsetting.UpdateBy(t_parent);
  end;
end;

//return the depth level of this tree object. Root-Object has depth level 0
//its children have 1, grandchildren 2, and so an
function TProductConfig.DepthLevel(): integer;
begin
  if assigned(t_parent) then result := t_parent.DepthLevel() + 1
  else result := 0;
end;

//find product configuration in this whole branch
//return the product configuration if found, otherwise nil
function TProductConfig.FindProduct(const sname: string): TProductConfig;
var t_product: TProductConfig;
begin
  result := nil;
  if SameText(s_prodname, sname) then
    result := self
  else begin
    for t_product in t_children.Values do begin
      result := t_product.FindProduct(sname);
      if assigned(result) then break;
    end;
  end;
end;

//return true if the the product configuration is found in children, otherwise false
function TProductConfig.HasChild(const sname: string): boolean;
begin
  result := t_children.ContainsKey(sname);
end;

function TProductConfig.CreateChild(const sname: string): TProductConfig;
begin
  if HasChild(sname) then result := nil
  else begin
    result := TProductConfig.Create;
    result.ProductName := sname;
    result.ProductParent := self;
    t_children.add(sname, result);
  end;
end;

//return the product configuration with the given name in children if is found, other create a new one
function TProductConfig.GetChild(const sname: string): TProductConfig;
begin
  if HasChild(sname) then result := t_children.Items[sname]
  else result := nil;
end;

function TProductConfig.AddChild(const child: TProductConfig): boolean;
begin
  result := false;
  if assigned(child) then begin
    if not self.HasChild(child.ProductName) then begin
      t_children.Add(child.ProductName, child);
      result := true;
    end;
  end;
end;

//take the child with the given name from children
function TProductConfig.TakeChild(const sname: string): TProductConfig;
var t_pair: TPair<string, TProductConfig>;
begin
  t_pair := t_children.ExtractPair(sname);
  result := t_pair.Value;
end;

function TProductConfig.UpdateChildName(const oldname, newname: string): boolean;
var t_config: TProductConfig;
begin
  result := SameText(oldname, newname);
  if (t_children.ContainsKey(oldname) and (not t_children.ContainsKey(newname))) then begin
    t_config := TakeChild(oldname);
    t_children.Add(newname, t_config);
    result := true;
  end else; //todo: error message
end;

function TProductConfig.GetRootProduct(): TProductConfig;
begin
  if assigned(t_parent) then
    result := t_parent.GetRootProduct
  else
    result := self;
end;

procedure TProductConfigurator.SetProductByName(const sname: string);
begin
  t_prodcur := t_prodroot.FindProduct(sname);
end;

procedure TProductConfigurator.SetProductByID(const prodid: string);
begin
  t_prodcur := t_prodroot.FindProduct(t_idnamemap.Items[prodid]);
end;

//find the first section whose name starts with CSTR_ROOT
function TProductConfigurator.FindRootSettings(const secnames: TStrings; var sroot: string): boolean;
var s_name: string; i_idx: integer;
begin
  result := false;
  for s_name in secnames do begin
    if StartsText(CSTR_ROOT, s_name) then begin
      sroot := s_name;
      result := true;
      break;
    end;
  end;
  if not result then begin
    i_idx := secnames.IndexOf(CSTR_GENERAL);
    if (i_idx >= 0) then begin
      result := true;
      sroot := secnames[i_idx];
    end;
  end;
end;

//read configs from a ini-file. Only the sections, which have names started
//with CSTR_ROOT(first one), CSTR_FAMILY, CSTR_VARIANT and CSTR_ARTICLE.
function TProductConfigurator.ReadFromIni(const sfile: string): boolean;
var t_inifile: TIniFile; t_secnames, t_secvals: TStrings; s_name: string; t_config: TProductConfig;
begin
  result := false;
  t_products.Clear;

  t_inifile := TIniFile.Create(sfile);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();

  t_inifile.ReadSections(t_secnames);
  if FindRootSettings(t_secnames, s_name) then begin
    t_inifile.ReadSectionValues(s_name, t_secvals);
    t_prodroot.ProductName := s_name;
    t_prodroot.UpdateBy(t_secvals);
  end;

  for s_name in t_secnames do begin
    if (StartsText(CSTR_VARIANT, s_name) or
        StartsText(CSTR_ARTICLE, s_name) or
        StartsText(CSTR_FAMILY, s_name)) then
    begin
      t_secvals.Clear();
      t_inifile.ReadSectionValues(s_name, t_secvals);
      t_config := t_prodroot.CreateChild(s_name);
      result := assigned(t_config);
      if result then begin
        t_config.UpdateBy(t_secvals);
        t_products.AddOrSetValue(t_config.ProductName, t_config);
        t_idnamemap.AddOrSetValue(t_config.ProductID, t_config.ProductName);
      end else begin
        //todo: error message
        break;
      end;
    end;
  end;
  if result then result := BuildProductFamily();

  t_secvals.Free();
  t_secnames.Free();
  t_inifile.Free();
end;

function  TProductConfigurator.BuildProductFamily(): boolean;
var t_config, t_parent: TProductConfig; s_parent: string;
begin
  for t_config in t_products.Values do begin
    t_parent := nil;
    if StartsText(CSTR_ARTICLE, t_config.ProductName) then begin
      if t_config.ContainsKey(CSTR_ARTICLE_PARENT) then begin
        s_parent := t_config.Items[CSTR_ARTICLE_PARENT];
        if t_products.ContainsKey(s_parent) then t_parent := t_products.Items[s_parent];
      end;
    end else if StartsText(CSTR_VARIANT, t_config.ProductName) then begin
      if t_config.ContainsKey(CSTR_VARIANT_PARENT) then begin
        s_parent := t_config.Items[CSTR_VARIANT_PARENT];
        if t_products.ContainsKey(s_parent) then t_parent := t_products.Items[s_parent];
      end;
    end;
    if assigned(t_parent) then begin
      if t_parent.AddChild(t_config) then t_config.ProductParent := t_parent;
    end else begin
      if t_prodroot.AddChild(t_config) then  t_config.ProductParent := t_prodroot;
    end;
  end;
  result := true;
end;

constructor TProductConfigurator.Create();
begin
  inherited Create;
  t_prodroot := TProductConfig.Create;
  t_prodcur := t_prodroot;
  t_idnamemap := TStringDictionary.Create;
  t_products := TProductDictionary.Create;
end;

destructor TProductConfigurator.Destroy();
begin
  t_prodroot.Free;
  t_idnamemap.Free;
  t_products.Free;
end;

//show and update all configs in a tree view
procedure TProductConfigurator.UpdateTreeView(var trv: TTreeView; const bclear: boolean);
begin
  trv.Enabled := false;
  if bclear then begin
    trv.ClearSelection();
    trv.Items.Clear();
  end;
  t_prodroot.BuildTreeNode(trv.Items, nil);
  //GotoTreeNode(trv);
  trv.Enabled := true;
end;

//show and update the settings of current config in a list view
procedure TProductConfigurator.UpdateListView(var lsv: TListView; const bfull: boolean; const bsorted: boolean);
var t_litem: TListItem; t_settings: TStringDictionary; t_keys: TStringList; s_key: string;
begin
  lsv.Enabled := false;
  lsv.Items.Clear();
  t_settings := TStringDictionary.Create;
  if bfull then t_prodcur.GetFullSettings(t_settings)
  else t_prodcur.GetOwnSettings(t_settings);

  t_keys := TStringList.Create;
  t_keys.CaseSensitive := false;
  t_keys.Sorted := bsorted;
  for s_key in t_settings.Keys do t_keys.Add(s_key);

  for s_key in t_keys do begin
    t_litem := lsv.Items.Add();
    t_litem.Caption := s_key;
    t_litem.SubItems.Add(t_settings.Items[s_key]);
  end;
  t_keys.Free;
  t_settings.Free;
  lsv.Enabled := true;
end;


initialization
  ProductDictionary := TProductDictionary.Create;
finalization
  ProductDictionary.Free;

end.
