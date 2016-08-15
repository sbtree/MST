unit FormFunctionTester;

interface

uses
  Windows, TextMessage, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FuncCaller, GenType;

type
  TFormFTMain = class(TForm)
    btnTest: TButton;
    memInfo: TMemo;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    t_messenger: TTextMessenger;
  public
    { Public-Deklarationen }
  end;

var
  FormFTMain: TFormFTMain;

implementation
uses StrUtils;
{$R *.dfm}

procedure TFormFTMain.btnTestClick(Sender: TObject);
//var t_fr_set_dm: FR_Set_DM;
var t_fcaller: TFunctionCaller; s_test, s_show: string; i: integer;
begin
  s_test := 'Restarting' + Char(0) + 'Motorola';
  for i := 0 to length(s_test) do
    if s_test[i] = Char(0) then s_show :=  s_show + Char(13)
    else s_show :=  s_show + s_test[i];

  ShowMessage(format('length = %d, %s', [length(s_show), s_show]));
  t_fcaller := TFunctionCaller.Create;
  t_fcaller.ExecutionMode := EM_DIAGNOSE;
  t_fcaller.Messenger := t_messenger;
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

procedure TFormFTMain.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
end;

procedure TFormFTMain.FormDestroy(Sender: TObject);
begin
  t_messenger.Free();
end;

end.
