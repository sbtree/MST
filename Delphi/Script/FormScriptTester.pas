unit FormScriptTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScriptReader;

type
  TfrmScriptTester = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    t_sreader : TScriptReader;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmScriptTester: TfrmScriptTester;

implementation

{$R *.dfm}

procedure TfrmScriptTester.btnTestClick(Sender: TObject);
begin
  t_sreader.ReadFromFile('PST_BT_FT_PS_liste.TXT');
  t_sreader.SaveToFile('PST_BT_FT_PS_liste_new.txt');
end;

procedure TfrmScriptTester.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
end;

procedure TfrmScriptTester.FormDestroy(Sender: TObject);
begin
  t_sreader.Free();
end;

end.
