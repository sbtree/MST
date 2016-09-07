unit FormFunctionTester;

interface

uses
  Windows, TextMessage, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FuncCaller, GenType, ScriptReader, StringPairs, StepGroup;

type
  TFormFTMain = class(TForm)
    btnTest: TButton;
    memInfo: TMemo;
    txtScriptFile: TEdit;
    btnOpenScript: TButton;
    txtFuncName: TEdit;
    txtFuncPara: TEdit;
    lblFuncName: TLabel;
    lblFuncPara: TLabel;
    btnReadScript: TButton;
    btnGoTo: TButton;
    txtStepNr: TEdit;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
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

procedure TFormFTMain.btnOpenScriptClick(Sender: TObject);
var
  t_dialog : TOpenDialog;    // Save dialog variable
begin
  t_dialog := TOpenDialog.Create(self); // Create the save dialog object - assign to our save dialog variable
  t_dialog.Title := 'Open a Test Script'; // Give the dialog a title
  t_dialog.InitialDir := GetCurrentDir;// Set up the starting directory to be the current one
  t_dialog.Filter := 'Text file|*.txt|All files|*.*'; // Allow only .txt and .doc file types to be saved
  t_dialog.DefaultExt := 'txt';// Set the default extension
  t_dialog.FilterIndex := 1; // Select text files as the starting filter type
  if t_dialog.Execute then txtScriptFile.Text:=t_dialog.FileName; // Display the open file dialog
  t_dialog.Free; // Free up the dialog
end;

procedure TFormFTMain.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text));
  t_container.FirstStep;
end;

procedure TFormFTMain.btnTestClick(Sender: TObject);
var t_fcaller: TFunctionCaller;
begin
  //t_container.FirstStep;
  t_fcaller := TFunctionCaller.Create;
  t_fcaller.CurStepGroup := t_container;
  t_fcaller.ExecutionMode := EM_DIAGNOSE;
  ITextMessengerImpl(t_fcaller).Messenger := t_messenger;
  t_fcaller.CallFunction(trim(txtFuncName.Text), trim(txtFuncPara.Text));
  //t_fcaller.CallFunction('EvalExprFloat', 'sqr(sin(1,0)*cos(2,0))');
  //t_fcaller.CallFunction('EvalExprInt', '9,5*2,5');
  //t_fcaller.CallFunction('EvalExprStr', '1,5*2,5');
  //t_fcaller.CallFunction('JumpIfFalse', '(a:=1)<0 PS_11.02');
  //t_fcaller.CallFunction('JumpIfTrue', '(a:=1)>0 PS_200');
  //t_fcaller.CallFunction('RepeatIfFalse', '''abc''=''abc'' PS_11.00');
  //t_container.LastStep;
  //t_fcaller.CallFunction('RepeatIfTrue', '(abc:=1)=1 PS_200');
  //t_fcaller.CallFunction('XYZ', 'N:\SW_INBE\DIS-2\Tools\flash_over_jtag.exe');
  //t_fcaller.CallFunction('Nil', 'abc');
  //t_fcaller.CallFunction('', 'abc');
  //t_fcaller.CallFunction('abcd', 'abc');
  t_fcaller.Free;
end;

procedure TFormFTMain.btnGoToClick(Sender: TObject);
begin
  t_container.GotoStepNr(trim(txtStepNr.Text));
end;

procedure TFormFTMain.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
  ITextMessengerImpl(t_sreader).Messenger := t_messenger;
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_container := TStepContainer.Create();
  ITextMessengerImpl(t_container).Messenger := t_messenger;
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
