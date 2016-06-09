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
  private
    { Private-Deklarationen }
    t_confreader: TProdConfigurator;
    procedure UpdateConfig();
    procedure UpdateFilter();
  public
    { Public-Deklarationen }
  end;

var
  frmProdTester: TfrmProdTester;

implementation
{$R *.dfm}
uses PairStrings;

procedure TfrmProdTester.btnCleanAllClick(Sender: TObject);
begin
  t_confreader.CleanAllConfig();
  UpdateConfig();
end;

procedure TfrmProdTester.btnCleanClick(Sender: TObject);
begin
  t_confreader.CleanCurConfig();
  UpdateConfig();
end;

procedure TfrmProdTester.btnCollapseClick(Sender: TObject);
begin
  trvProduct.FullCollapse();
end;

procedure TfrmProdTester.btnExpandClick(Sender: TObject);
begin
  trvProduct.FullExpand();
end;

procedure TfrmProdTester.btnLoadClick(Sender: TObject);
begin
  t_confreader.Clear();
  trvProduct.Items.Clear();
  if t_confreader.ReadFromFile(txtConfigFile.Text) then UpdateFilter();
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
  if (s_filtervar <> '') then t_confreader.FilterVar := s_filtervar;
end;

procedure TfrmProdTester.FormCreate(Sender: TObject);
begin
  t_confreader := TProdConfigurator.Create();
end;

procedure TfrmProdTester.FormDestroy(Sender: TObject);
begin
  t_confreader.Free();
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
var s_select: string;
begin
  if assigned(trvProduct.Selected) then s_select := trvProduct.Selected.Text
  else s_select := '';
  t_confreader.Select(s_select);
  t_confreader.UpdateListView(lsvConfig, chkShowAll.Checked, chkSorted.Checked);
end;

procedure TfrmProdTester.UpdateFilter();
var s_ftext: string;
begin
  if chkFilter.Checked then s_ftext := trim(txtFilter.Text)
  else s_ftext := '';
  t_confreader.Filter(s_ftext);
  t_confreader.UpdateTreeView(trvProduct);
end;

end.
