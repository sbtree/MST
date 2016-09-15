unit FormProdTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ProductConfig;

type
  TfrmProdTester = class(TForm)
    trvProduct: TTreeView;
    txtConfigFile: TEdit;
    btnOpenIni: TButton;
    btnLoad: TButton;
    chkShowAll: TCheckBox;
    chkFilter: TCheckBox;
    txtFilter: TEdit;
    lsvConfig: TListView;
    chkSorted: TCheckBox;
    cmbFilterName: TComboBox;
    btnCollapse: TButton;
    btnExpand: TButton;
    btnClean: TButton;
    btnCleanAll: TButton;
    btnSave: TButton;
    txtCurConfig: TEdit;
    txtRefConfig: TEdit;
    btnMoveText: TButton;
    btnUpdate: TButton;
    btnMove: TButton;
    btnNew: TButton;
    btnRemove: TButton;
    txtVarNames: TEdit;
    btnPromote: TButton;
    btnClearVarNames: TButton;
    btnDefault: TButton;
    procedure btnOpenIniClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure chkShowAllClick(Sender: TObject);
    procedure trvProductChange(Sender: TObject; Node: TTreeNode);
    procedure chkFilterClick(Sender: TObject);
    procedure txtFilterExit(Sender: TObject);
    procedure txtFilterKeyPress(Sender: TObject; var Key: Char);
    procedure chkSortedClick(Sender: TObject);
    procedure cmbFilterNameChange(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure btnCleanAllClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnMoveTextClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lsvConfigSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnClearVarNamesClick(Sender: TObject);
    procedure btnPromoteClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure lsvConfigCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private-Deklarationen }
    t_pconfig: TProdConfig;
    procedure UpdateConfig();
    procedure UpdateFilter(const bforce: boolean = false);
  public
    { Public-Deklarationen }
  end;

var
  frmProdTester: TfrmProdTester;

implementation
{$R *.dfm}
uses StringPairs, StrUtils;

procedure TfrmProdTester.btnCleanAllClick(Sender: TObject);
begin
  t_pconfig.CleanAllConfig();
  UpdateConfig();
end;

procedure TfrmProdTester.btnCleanClick(Sender: TObject);
begin
  t_pconfig.CleanCurConfig();
  UpdateConfig();
end;

procedure TfrmProdTester.btnClearVarNamesClick(Sender: TObject);
begin
  txtVarNames.Text := '';
end;

procedure TfrmProdTester.btnCollapseClick(Sender: TObject);
begin
  trvProduct.FullCollapse();
end;

procedure TfrmProdTester.btnDefaultClick(Sender: TObject);
begin
  t_pconfig.UpdateDefault(trim(txtCurConfig.Text));
  t_pconfig.GotoTreeNode(trvProduct);
  //t_pconfig.UpdateTreeView(trvProduct);
  t_pconfig.UpdateListView(lsvConfig, chkShowAll.Checked, chkSorted.Checked);
end;

procedure TfrmProdTester.btnExpandClick(Sender: TObject);
begin
  trvProduct.FullExpand();
end;

procedure TfrmProdTester.btnLoadClick(Sender: TObject);
begin
  if t_pconfig.UpdateFromFile(txtConfigFile.Text) then begin
    UpdateFilter(true);
    t_pconfig.UpdateListView(lsvConfig, chkShowAll.Checked, chkSorted.Checked);
  end;
end;

procedure TfrmProdTester.btnOpenIniClick(Sender: TObject);
var
  t_dialog : TOpenDialog;    // Save dialog variable
begin
  t_dialog := TOpenDialog.Create(self); // Create the save dialog object - assign to our save dialog variable
  t_dialog.Title := 'Open a Config File'; // Give the dialog a title
  t_dialog.InitialDir := GetCurrentDir;// Set up the starting directory to be the current one
  t_dialog.Filter := 'Config File|*.ini|All files|*.*'; // Allow only .txt and .doc file types to be saved
  t_dialog.DefaultExt := 'txt';// Set the default extension
  t_dialog.FilterIndex := 1; // Select text files as the starting filter type
  if t_dialog.Execute then txtConfigFile.Text:=t_dialog.FileName; // Display the open file dialog
  t_dialog.Free; // Free up the dialog
end;

procedure TfrmProdTester.btnPromoteClick(Sender: TObject);
var t_varnames: TStrings;
begin
  t_varnames := TStringList.Create();
  ExtractStrings([';'], [' '], PChar(trim(txtVarNames.Text)), t_varnames);
  t_pconfig.PromoteConfig(trim(txtCurConfig.Text), trim(txtRefConfig.Text), t_varnames);
  t_pconfig.UpdateTreeView(trvProduct);
  t_varnames.Free();
end;

procedure TfrmProdTester.btnRemoveClick(Sender: TObject);
begin
  t_pconfig.RemoveConfig(trim(txtCurConfig.Text));
  t_pconfig.UpdateTreeView(trvProduct);;
end;

procedure TfrmProdTester.btnSaveClick(Sender: TObject);
begin
  t_pconfig.SaveToFile();
end;

procedure TfrmProdTester.btnUpdateClick(Sender: TObject);
begin
  t_pconfig.UpdateConfig(trim(txtCurConfig.Text), trim(txtRefConfig.Text));
  UpdateConfig();
end;

procedure TfrmProdTester.btnMoveClick(Sender: TObject);
begin
  t_pconfig.MoveConfigTo(trim(txtCurConfig.Text), trim(txtRefConfig.Text));
  t_pconfig.UpdateTreeView(trvProduct);;
end;

procedure TfrmProdTester.btnMoveTextClick(Sender: TObject);
begin
  txtRefConfig.Text := txtCurConfig.Text;
  txtCurConfig.Text := '';
end;

procedure TfrmProdTester.btnNewClick(Sender: TObject);
begin
  t_pconfig.CreateConfig(trim(txtCurConfig.Text), trim(txtRefConfig.Text));
  t_pconfig.UpdateTreeView(trvProduct);;
end;

procedure TfrmProdTester.chkFilterClick(Sender: TObject);
begin
  txtFilter.Enabled := chkFilter.Checked;
  cmbFilterName.Enabled := chkFilter.Checked;
  if chkFilter.Checked then begin
    txtFilter.SetFocus();
    txtFilter.SelectAll();
  end;
  UpdateFilter();
end;

procedure TfrmProdTester.chkShowAllClick(Sender: TObject);
begin
  UpdateConfig();
end;

procedure TfrmProdTester.chkSortedClick(Sender: TObject);
begin
  UpdateConfig();
end;

procedure TfrmProdTester.cmbFilterNameChange(Sender: TObject);
var s_filtervar: string;
begin
  s_filtervar := trim(cmbFilterName.Text);
  if (s_filtervar <> '') then t_pconfig.FilterVarName := s_filtervar;
end;

procedure TfrmProdTester.FormCreate(Sender: TObject);
begin
  t_pconfig := TProdConfig.Create();
end;

procedure TfrmProdTester.FormDestroy(Sender: TObject);
begin
  t_pconfig.Free();
end;

procedure TfrmProdTester.lsvConfigCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var t_varnames: TStrings; t_confsec: TTreeSection;
begin
  t_varnames := TStringList.Create();
  t_confsec := t_pconfig.CurConfig;
  if assigned(t_confsec) then t_confsec.GetPairNames(t_varnames);
  if (t_varnames.Count > 0) then begin
    if (t_varnames.IndexOf(Item.Caption) >= 0) then Sender.Canvas.Font.Color := clBlack
    else Sender.Canvas.Font.Color := clDkGray;
  end;
  t_varnames.Free();
end;

procedure TfrmProdTester.lsvConfigSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var s_varnames: string;
begin
  s_varnames := trim(txtVarNames.Text);
  if (not ContainsText(s_varnames, Item.Caption)) then begin
    txtVarNames.Text :=   s_varnames + Item.Caption + ';'
  end;
end;

procedure TfrmProdTester.trvProductChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateConfig();
end;

procedure TfrmProdTester.txtFilterExit(Sender: TObject);
begin
  UpdateFilter();
end;

procedure TfrmProdTester.txtFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then UpdateFilter();
end;

procedure TfrmProdTester.UpdateConfig();
begin
  if assigned(trvProduct.Selected) then begin
    txtCurConfig.Text := trvProduct.Selected.Text;
    t_pconfig.Select(trvProduct.Selected.Text);
    t_pconfig.GotoTreeNode(trvProduct);
    t_pconfig.UpdateListView(lsvConfig, chkShowAll.Checked, chkSorted.Checked);
  end;
end;

procedure TfrmProdTester.UpdateFilter(const bforce: boolean);
var s_ftext: string;
begin
  if chkFilter.Checked then s_ftext := trim(txtFilter.Text)
  else s_ftext := '';
  if ((not SameText(s_ftext, t_pconfig.FilterText)) or bforce) then begin
    t_pconfig.Filter(s_ftext);
    t_pconfig.UpdateTreeView(trvProduct);
  end;
end;

end.
