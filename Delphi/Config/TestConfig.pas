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

  TProductSetting = class(TDictionary<string, string>)
  public
    procedure UpdateBy(const keyvals: TStrings; const bcover: boolean = true); overload;
    procedure UpdateBy(const config: TProductSetting; const bcover: boolean = true); overload;
    procedure ReduceBy(const config: TProductSetting);
  end;

  TProductFamily= class(TObjectDictionary<string, TProductFamily>)
  protected
    s_name: string;
    t_ownsetting: TProductSetting;
    t_parent: TProductFamily;
  protected
    procedure SetName(const sname: string);
    procedure SetParent(parent: TProductFamily);
    procedure GetParentsSetting(var tsetting: TProductSetting);
    procedure ReduceOwnSetting();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ValueNotify(const TValue: TProductFamily; Action: TCollectionNotification); override;
    procedure GetFullSetting(var tsetting: TProductSetting);
    function FindProduct(const sname: string): TProductFamily;

    property OwnSetting: TProductSetting read t_ownsetting;
    property ProductName: string read s_name write SetName;
    property ProductParent: TProductFamily read t_parent write SetParent;
  end;

  TTestConfigurator = class
  protected
    t_prodroot: TProductFamily;
    t_curproduct: TProductFamily;
  protected
    procedure SetCurProduct(const prod: TProductFamily);
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
    property CurrentProduct: TProductFamily read t_curproduct write SetCurProduct;
  end;

var
  t_prodroot: TProductFamily;


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
procedure TProductSetting.UpdateBy(const keyvals: TStrings; const bcover: boolean);
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
procedure TProductSetting.UpdateBy(const config: TProductSetting; const bcover: boolean);
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

procedure TProductSetting.ReduceBy(const config: TProductSetting);
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

procedure TProductFamily.SetName(const sname: string);
var t_pair: TPair<string, TProductFamily>;
begin
  if not SameText(s_name, sname) then begin
    if assigned(t_parent) then begin
      t_pair := t_parent.ExtractPair(s_name);
      t_parent.AddOrSetValue(sname, self);
    end;
    s_name := sname;
  end;
end;

procedure TProductFamily.SetParent(parent: TProductFamily);
var t_pair: TPair<string, TProductFamily>;
begin
  if (parent <> t_parent) then begin
    if assigned(t_parent) then t_pair := t_parent.ExtractPair(s_name);

    t_parent := parent;
    if assigned(t_parent) then begin
      if not t_parent.ContainsKey(s_name) then t_parent.Add(s_name, self)
      else ;//todo: dupplication?
    end;
  end;
end;

procedure TProductFamily.GetParentsSetting(var tsetting: TProductSetting);
begin
  if assigned(t_parent) then begin
    t_parent.GetParentsSetting(tsetting);
    tsetting.UpdateBy(t_parent.OwnSetting);
  end;
end;

constructor TProductFamily.Create();
begin
  inherited Create(TTextComparer.Create());
  t_ownsetting := TProductSetting.Create(TTextComparer.Create());
  t_parent := nil;
end;

procedure TProductFamily.ReduceOwnSetting();
var t_parentsetting: TProductSetting;
begin
  t_parentsetting := TProductSetting.Create();
  GetParentsSetting(t_parentsetting);
  t_ownsetting.ReduceBy(t_parentsetting);
  t_parentsetting.Free();
end;

destructor TProductFamily.Destroy();
begin
  SetParent(nil);
  t_ownsetting.Free();
end;

procedure TProductFamily.ValueNotify(const TValue: TProductFamily; Action: TCollectionNotification);
begin
  inherited ValueNotify(TValue, Action);
  case Action of
    cnAdded: TValue.SetParent(self);
    cnRemoved: TValue.Free;
    cnExtracted: TValue.SetParent(t_prodroot);
  end;
end;

procedure TProductFamily.GetFullSetting(var tsetting: TProductSetting);
begin
  GetParentsSetting(tsetting);
  tsetting.UpdateBy(t_ownsetting);
end;

function TProductFamily.FindProduct(const sname: string): TProductFamily;
var t_product: TProductFamily;
begin
  result := nil;
  if SameText(s_name, sname) then
    result := self
  else if ContainsKey(sname) then
    result := Items[sname]
  else begin
    for t_product in Values do begin
      result := t_product.FindProduct(sname);
      if assigned(result) then break;
    end;
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
    t_prodroot.OwnSetting.UpdateBy(t_secvals);
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
  t_prodroot := TProductFamily.Create;
  t_curproduct := nil;
end;

destructor TTestConfigurator.Destroy();
begin
  t_prodroot.Free;
end;

initialization
  t_prodroot := TProductFamily.Create;
finalization
  t_prodroot.Free;

end.
