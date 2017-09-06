unit TestConfig;

interface
uses Classes,System.Generics.Collections, System.Generics.Defaults;

function ExtractKeyValue(const keyval: string; var skey, sval: string; const sepa: char = '='): boolean;
function TestModeByStr(const stest: string; const default: integer = 4): integer;

type
  //a text comparer without consideration of capital letters
  TTextComparer = class(TOrdinalIStringComparer)
  public
    function Equals(const Left, Right: string): Boolean; reintroduce; overload; override;
  end;

  TStringDictionary = class(TDictionary<string, string>)
  protected
    c_fldseparator: char;
  public
    procedure UpdateBy(const keyvals: TStrings; const bcover: boolean = true); overload;
    procedure UpdateBy(const config: TStringDictionary; const bcover: boolean = true); overload;
    procedure ReduceBy(const config: TStringDictionary);
    function  FieldValue(const fldname: string; const fldindex: integer): string;

    property  FieldSeparator: char read c_fldseparator write c_fldseparator default '|';
  end;

  TProductConfig = class;
  TProductDictionary = class(TObjectDictionary<string, TProductConfig>)
  public
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
    function  GetConfigId(): string;
    procedure SetConfigId(cid: string);
  public
    constructor Create(); overload;
    destructor Destroy(); override;

    procedure GetParentSettings(var tsetting: TStringDictionary);
    procedure GetFullSettings(var tsetting: TStringDictionary);
    procedure GetOwnSettings(var tsetting: TStringDictionary);

    function  DepthLevel(): integer;
    function  FindProduct(const sname: string): TProductConfig;
    function  HasChild(const sname: string): boolean;
    function  CreateOrGetChild(const sname: string): TProductConfig;
    function  TakeChild(const sname: string): TProductConfig;
    function  ChangeChildName(const oldname, newname: string): boolean;

    property  ProductName: string read s_prodname write SetProdName;
    property  ProductParent: TProductConfig read t_parent write SetParent;
    property  ChildrenCount: integer read GetChildrenCount;
    property  Visible: boolean read IsVisible write b_visible;
  end;

  TTestConfigurator = class
  protected
    t_prodroot: TProductConfig;
    t_curproduct: TProductConfig;
  protected
    procedure SetCurProduct(const prod: TProductConfig);
    function FindRootSetting(const secnames: TStrings; var sroot: string): boolean;
    function ReadFromIni(const sfile: string): boolean;
  public
    constructor Create();
    destructor Destroy(); override;
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
  t_prodroot: TProductConfig;


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
  if not TryStrToInt(stest, result) then begin
    result := IndexText(stest, CSTR_TEST_MODE);
    if result < 0 then result := default;
  end;
end;

function TTextComparer.Equals(const Left, Right: string): Boolean;
begin
  result := SameText(Left, Right);
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
      else if not ContainsKey(t_pair.Key) then
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

procedure TProductConfig.SetProdName(const sname: string);
begin
  if not SameText(s_prodname, sname) then begin
    if assigned(t_parent) then begin
      if t_parent.HasChild(sname) then //todo: dupplication, error message
      else s_prodname := sname;
    end else s_prodname := sname;
  end;
end;

procedure TProductConfig.SetParent(parent: TProductConfig);
begin
  if (parent <> t_parent) then begin
    if assigned(t_parent) then  t_parent.TakeChild(s_prodname);
    if assigned(parent) then begin
      if parent.HasChild(s_prodname) then //todo: dupplication, error message
      else t_parent := parent;
    end else
      t_parent := parent;
  end;
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
var i: integer; t_config: TProductConfig; s_key: string;
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
var i: integer; t_config: TProductConfig;
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
function TProductConfig.GetConfigId(): string;
begin
  result := self.Items[CSTR_ID_STRING];
end;

//set identifer of current config
procedure TProductConfig.SetConfigId(cid: string);
begin
  self.AddOrSetValue(CSTR_ID_STRING, cid);
end;

constructor TProductConfig.Create();
begin
  inherited Create(TTextComparer.Create());
  b_visible := true;
  t_children := TProductDictionary.Create(TTextComparer.Create());
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

//return the product configuration with the given name in children if is found, other create a new one
function TProductConfig.CreateOrGetChild(const sname: string): TProductConfig;
begin
  if t_children.ContainsKey(sname) then
    result := t_children.Items[sname]
  else begin
    result := TProductConfig.Create;
    result.ProductName := sname;
    result.ProductParent := self;
  end;
end;

//take the child wthi the given name from children
function TProductConfig.TakeChild(const sname: string): TProductConfig;
var t_pair: TPair<string, TProductConfig>;
begin
  t_pair := t_children.ExtractPair(sname);
  result := t_pair.Value;
end;

function TProductConfig.ChangeChildName(const oldname, newname: string): boolean;
var t_pair: TPair<string, TProductConfig>;
begin
  result := false;
  if (t_children.ContainsKey(oldname) and (not t_children.ContainsKey(newname))) then begin
    t_pair := t_children.ExtractPair(oldname);
    t_pair.Value.ProductName := newname;
    t_children.Add(newname, t_pair.Value);
    result := true;
  end;
end;

//find the first section whose name starts with CSTR_ROOT
function TTestConfigurator.FindRootSetting(const secnames: TStrings; var sroot: string): boolean;
var i: integer;
begin
  result := false;
  for i := 0 to secnames.Count do begin
    if StartsText(CSTR_ROOT, secnames[i]) or SameText(CSTR_GENERAL, secnames[i]) then begin
      result := true;
      sroot := secnames[i];
      break;
    end;
  end;
end;

//read configs from a ini-file. Only the sections, which have names started
//with CSTR_ROOT(first one), CSTR_FAMILY, CSTR_VARIANT and CSTR_ARTICLE.
function TTestConfigurator.ReadFromIni(const sfile: string): boolean;
var t_inifile: TIniFile; t_secnames, t_secvals: TStrings; s_name: string; i: integer;
begin
  result := false;
  t_inifile := TIniFile.Create(sfile);
  t_secnames := TStringList.Create();
  t_secvals := TStringList.Create();

  t_inifile.ReadSections(t_secnames);
  if FindRootSetting(t_secnames, s_name) then begin
    t_inifile.ReadSectionValues(s_name, t_secvals);
    t_prodroot.ProductName := s_name;
    t_prodroot.UpdateBy(t_secvals);
  end;

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

      //t_prodroot.Add(s_name, );
      //result := (AddConfig(s_name, t_secvals) <> nil);
      if not result then break;
    end;
  end;
  //if result then result := BuildTreeFromIni(t_inifile);

  t_secvals.Free();
  t_secnames.Free();
  t_inifile.Free();
end;

constructor TTestConfigurator.Create();
begin
  inherited Create;
  t_prodroot := TProductConfig.Create;
  t_curproduct := nil;
end;

destructor TTestConfigurator.Destroy();
begin
  t_prodroot.Free;
end;

initialization
  t_prodroot := TProductConfig.Create;
finalization
  t_prodroot.Free;

end.
