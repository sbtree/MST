unit FormScriptTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmScriptTester = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmScriptTester: TfrmScriptTester;

implementation

{$R *.dfm}

uses ScriptReader;

procedure TfrmScriptTester.btnTestClick(Sender: TObject);
var t_sreader: TScriptReader;
begin
  t_sreader := TScriptReader.Create();
  t_sreader.ReadFromFile('test_script.TXT', true);
  t_sreader.SaveSteps('test_script_new.txt');
  t_sreader.Free();
end;

end.
