unit FormFunctionTester;

interface

uses
  Windows, TextMessage, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FuncCaller, GenType, ScriptReader, StringPairs, StepGroup;

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
    t_sreader:  TScriptReader;
    t_vars:     TStringPairs;
    t_container:TStepContainer;
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
{  s_test := 'Restarting' + Char(0) + 'Motorola';
  for i := 0 to length(s_test) do
    if s_test[i] = Char(0) then s_show :=  s_show + Char(13)
    else s_show :=  s_show + s_test[i];

  ShowMessage(format('length = %d, %s', [length(s_show), s_show])); }
  t_sreader.ReadFromFile('..\Script\PST_BT_FT_PS_liste.TXT');
  t_container.FirstStep;
  t_fcaller := TFunctionCaller.Create;
  t_fcaller.CurStepGroup := t_container;
  t_fcaller.ExecutionMode := EM_DIAGNOSE;
  ITextMessengerImpl(t_fcaller).Messenger := t_messenger;
  t_fcaller.CallFunction('EvalExprFloat', 'sqr(sin(1,0)*cos(2,0))');
  t_fcaller.CallFunction('JumpIfFalse', 'abc PS_11.02');
  t_fcaller.CallFunction('JumpIfTrue', 'abc PS_200');
  t_fcaller.CallFunction('RepeatIfFalse', 'abc PS_11.00');
  t_container.LastStep;
  t_fcaller.CallFunction('RepeatIfTrue', 'abc PS_200');
  //t_fcaller.CallFunction('XYZ', 'N:\SW_INBE\DIS-2\Tools\flash_over_jtag.exe');
  t_fcaller.CallFunction('Nil', 'abc');
  t_fcaller.CallFunction('', 'abc');
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
  t_sreader := TScriptReader.Create();
  ITextMessengerImpl(t_sreader).Messenger := t_messenger;
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_container := TStepContainer.Create();
  t_vars := TStringPairs.Create();
  t_messenger.Messages := memInfo.Lines;
  ITextMessengerImpl(t_sreader).Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;
end;

procedure TFormFTMain.FormDestroy(Sender: TObject);
begin
  t_vars.Free();
  t_container.Free();
  t_sreader.Free();
  t_messenger.Free();
end;

end.
