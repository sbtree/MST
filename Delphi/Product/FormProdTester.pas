unit FormProdTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ProductConfig;

type
  TfrmProdTester = class(TForm)
    btnTest: TButton;
    trvProduct: TTreeView;
    txtScriptFile: TEdit;
    btnOpenIni: TButton;
    btnLoad: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure btnOpenIniClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_confreader: TConfigReader;
  public
    { Public-Deklarationen }
  end;

var
  frmProdTester: TfrmProdTester;

implementation
{$R *.dfm}

procedure TfrmProdTester.btnLoadClick(Sender: TObject);
begin
  t_confreader.Clear();
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

procedure TfrmProdTester.btnTestClick(Sender: TObject);
var pch: PChar; str: string; i: integer; t_slist: TStringList;
begin
  t_slist := TStringList.Create();
  t_slist.CaseSensitive := false;
  t_slist.Sorted := true;
  t_slist.AddObject('aaa', TStringList.Create());
  t_slist.AddObject('fff', TStringList.Create());
  t_slist.AddObject('eee', TStringList.Create());
  t_slist.AddObject('bbb', TStringList.Create());
  t_slist.AddObject('ddd', TStringList.Create());
  t_slist.AddObject('ccc', TStringList.Create());
  ShowMessage('before Delete: t_slist = ' + t_slist.CommaText);
  if t_slist.Find('eee', i) then t_slist.Delete(i);
  ShowMessage('after Delete: t_slist = ' + t_slist.CommaText);
{
  if True then

  t_slist.Free();
  str := 'ABCDE';
  pch := PChar(str);
  for i := 0 to length(str) -1 do begin
    pch := 'A';
    inc(pch);
  end;
  pch := PChar(str);
  ShowMessage(pch);
  ShowMessage(str);

  str[2]:= 'A';
  str[3]:= 'A';
  str[4]:= 'A';
  str[5]:= 'A';
  ShowMessage(pch);
  ShowMessage(str);  }
end;

procedure TfrmProdTester.FormCreate(Sender: TObject);
begin
  t_confreader := TConfigReader.Create();
end;

procedure TfrmProdTester.FormDestroy(Sender: TObject);
begin
  t_confreader.Free();
end;

end.
