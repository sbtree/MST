unit FormProdTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ProductConfig;

type
  TfrmProdTester = class(TForm)
    trvProduct: TTreeView;
    txtScriptFile: TEdit;
    btnOpenIni: TButton;
    btnLoad: TButton;
    lstVar: TListBox;
    chkShowAll: TCheckBox;
    procedure btnOpenIniClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure chkShowAllClick(Sender: TObject);
    procedure trvProductChange(Sender: TObject; Node: TTreeNode);
  private
    { Private-Deklarationen }
    t_confreader: TConfigReader;
    procedure UpdateConfig();
  public
    { Public-Deklarationen }
  end;

var
  frmProdTester: TfrmProdTester;

implementation
{$R *.dfm}
uses PairStrings;

procedure TfrmProdTester.btnLoadClick(Sender: TObject);
begin
  t_confreader.Clear();
  trvProduct.Items.Clear();
  if t_confreader.ReadFromFile(txtScriptFile.Text) then
    t_confreader.BuildTreeView(trvProduct);
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
  if t_dialog.Execute then txtScriptFile.Text:=t_dialog.FileName; // Display the open file dialog
  t_dialog.Free; // Free up the dialog
end;

procedure TfrmProdTester.chkShowAllClick(Sender: TObject);
begin
  UpdateConfig();
end;

procedure TfrmProdTester.FormCreate(Sender: TObject);
begin
  t_confreader := TConfigReader.Create();
end;

procedure TfrmProdTester.FormDestroy(Sender: TObject);
begin
  t_confreader.Free();
end;

procedure TfrmProdTester.trvProductChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateConfig();
end;

procedure TfrmProdTester.UpdateConfig();
begin
  lstVar.Clear();
  if chkShowAll.Checked then lstVar.Items.AddStrings(t_confreader.ConfigAll[trvProduct.Selected.Text].Pairs)
  else lstVar.Items.AddStrings(t_confreader.ConfigOwn[trvProduct.Selected.Text].Pairs);
end;

end.
