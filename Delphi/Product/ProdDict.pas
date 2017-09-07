unit ProdDict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, TestConfig;

type
  TfrmProdDict = class(TForm)
    trvProduct: TTreeView;
    lsvConfig: TListView;
    btnLoad: TButton;
    btnOpenIni: TButton;
    txtConfigFile: TEdit;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    t_prodconf: TProductConfigurator;
  public
    { Public-Deklarationen }
  end;

var
  frmProdDict: TfrmProdDict;

implementation

{$R *.dfm}

procedure TfrmProdDict.btnLoadClick(Sender: TObject);
begin
  t_prodconf.UpdateFromFile(txtConfigFile.Text);
  t_prodconf.UpdateTreeView(trvProduct);
end;

procedure TfrmProdDict.FormCreate(Sender: TObject);
begin
  t_prodconf := TProductConfigurator.Create;
end;

procedure TfrmProdDict.FormDestroy(Sender: TObject);
begin
  t_prodconf.Free;
end;

end.
