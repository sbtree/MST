unit FormFunctionTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FunctionCaller, GenType;

type
  TFormFTMain = class(TForm)
    btnTest: TButton;
    memInfo: TMemo;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormFTMain: TFormFTMain;

implementation

{$R *.dfm}

procedure TFormFTMain.btnTestClick(Sender: TObject);
//var t_fr_set_dm: FR_Set_DM;
var t_fcaller: TFunctionCaller;
begin
  t_fcaller := TFunctionCaller.Create;
  t_fcaller.ExecutionMode := EM_DIAGNOSE;
  t_fcaller.Messages := memInfo.Lines;
  t_fcaller.CallFunction('ExecConsoleCmd', 'N:\SW_INBE\DIS-2\Tools\flash_over_jtag.exe');
  t_fcaller.CallFunction('Nil', 'abc');
  t_fcaller.CallFunction('abcd', 'abc');
  t_fcaller.CallFunction('EvaluateStr', '@abc');
  t_fcaller.Free;
{  t_fr_set_dm := FR_Set_DM.Create;

  t_fr_set_dm.Parameterize('0 12345');
  t_fr_set_dm.Initilize('');
  t_fr_set_dm.Execute('');
  FreeAndNil(t_fr_set_dm);  }
end;

end.
