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
uses StepContainer;

procedure TfrmScriptTester.btnTestClick(Sender: TObject);
var t_localreader: TScriptReader;
begin
  t_sreader.ReadFromFile('PST_BT_FT_PS_liste.TXT');
  t_localreader := TScriptReader.Create();
  t_localreader.StepContainer.Assign(t_sreader.StepContainer);
  t_sreader.StepContainer.Clear();
  t_localreader.SaveToFile('PST_BT_FT_PS_liste_new.txt');
  //t_sreader.SaveToFile('PST_BT_FT_PS_liste_new.txt');
  t_localreader.Free();
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
