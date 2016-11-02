unit RunnerMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ScriptReader, TestRunner, TextMessage, StepData, StepGroup, StringPairs;

type
  TFrmTestRunner = class(TForm)
    lblInclusive: TLabel;
    lblExclusive: TLabel;
    txtScriptFile: TEdit;
    btnOpenScript: TButton;
    btnReadScript: TButton;
    btnSaveScript: TButton;
    memInfo: TMemo;
    txtInclusive: TEdit;
    btnSequence: TButton;
    txtExclusive: TEdit;
    btnClear: TButton;
    chkForce: TCheckBox;
    chkAppend: TCheckBox;
    btnRunSequence: TButton;
    btnRepeatSequence: TButton;
    GroupBox1: TGroupBox;
    btnRunFirstStep: TButton;
    btnRepeatStep: TButton;
    btnPreviousStep: TButton;
    btnNextStep: TButton;
    GroupBox2: TGroupBox;
    btnRepeatCase: TButton;
    btnRunPrevCase: TButton;
    btnRunNextCase: TButton;
    btnRunFirstCase: TButton;
    chkJump: TCheckBox;
    chkDiagnose: TCheckBox;
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSequenceClick(Sender: TObject);
    procedure btnRunSequenceClick(Sender: TObject);
    procedure btnRepeatSequenceClick(Sender: TObject);
    procedure btnRunFirstStepClick(Sender: TObject);
    procedure btnPreviousStepClick(Sender: TObject);
    procedure btnRepeatStepClick(Sender: TObject);
    procedure btnNextStepClick(Sender: TObject);
    procedure btnRunFirstCaseClick(Sender: TObject);
    procedure btnRunPrevCaseClick(Sender: TObject);
    procedure btnRepeatCaseClick(Sender: TObject);
    procedure btnRunNextCaseClick(Sender: TObject);
    procedure chkDiagnoseClick(Sender: TObject);
    procedure btnSaveScriptClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_sreader:  TScriptReader;
    t_messenger:TTextMessenger;
    t_container:TStepContainer;
    t_vars:     TStringPairs;
    t_runner:   TTestRunner;
  public
    { Public-Deklarationen }
  end;

var
  FrmTestRunner: TFrmTestRunner;

implementation
uses GenType;
{$R *.dfm}

procedure TFrmTestRunner.btnClearClick(Sender: TObject);
begin
  t_messenger.Clear(true);
  t_sreader.Clear();
end;

procedure TFrmTestRunner.btnNextStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_runner.TestSequence.NextStep);
end;

procedure TFrmTestRunner.btnOpenScriptClick(Sender: TObject);
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
  t_dialog.Free(); // Free up the dialog
end;

procedure TFrmTestRunner.btnPreviousStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_runner.TestSequence.PreviousStep);
end;

procedure TFrmTestRunner.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text), chkForce.Checked, chkAppend.Checked);
  t_runner.StepContainer := t_sreader.StepContainer;
end;

procedure TFrmTestRunner.btnRepeatCaseClick(Sender: TObject);
begin
  t_runner.RunGroup(t_runner.TestSequence.CaseGroup.CurrentCase);
end;

procedure TFrmTestRunner.btnRepeatSequenceClick(Sender: TObject);
begin
  t_runner.RunSequence();
end;

procedure TFrmTestRunner.btnRepeatStepClick(Sender: TObject);
begin
  t_runner.RepeatStep();
end;

procedure TFrmTestRunner.btnRunFirstCaseClick(Sender: TObject);
begin
  t_runner.RunGroup(t_runner.TestSequence.CaseGroup.FirstCase);
end;

procedure TFrmTestRunner.btnRunFirstStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_runner.TestSequence.FirstStep);
end;

procedure TFrmTestRunner.btnRunNextCaseClick(Sender: TObject);
begin
  t_runner.RunGroup(t_runner.TestSequence.CaseGroup.NextCase)
end;

procedure TFrmTestRunner.btnRunPrevCaseClick(Sender: TObject);
begin
  t_runner.RunGroup(t_runner.TestSequence.CaseGroup.PreviousCase);
end;

procedure TFrmTestRunner.btnRunSequenceClick(Sender: TObject);
begin
  if t_runner.UpdateSequence(trim(txtInclusive.Text), trim(txtExclusive.Text)) then
    t_runner.RunSequence();
end;

procedure TFrmTestRunner.btnSaveScriptClick(Sender: TObject);
begin
  t_sreader.SaveToFile();
end;

procedure TFrmTestRunner.btnSequenceClick(Sender: TObject);
var s_casenrs: string;
begin
  t_runner.UpdateSequence(trim(txtInclusive.Text), trim(txtExclusive.Text));
  s_casenrs := t_runner.TestSequence.CaseGroup.CaseNumbers;

  if s_casenrs <> '' then t_messenger.AddMessage('Sequence Cases: ' + s_casenrs)
  else t_messenger.AddMessage('Test Sequence: no case')
end;

procedure TFrmTestRunner.chkDiagnoseClick(Sender: TObject);
begin
  if chkDiagnose.Checked then t_runner.ExecutionMode := EM_DIAGNOSE
  else t_runner.ExecutionMode := EM_NORMAL;
end;

procedure TFrmTestRunner.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
  t_messenger.MessageThreshold := ML_INFO;

  t_vars := TStringPairs.Create();
  t_container := TStepContainer.Create();
  ITextMessengerImpl(t_container).Messenger := t_messenger;

  t_sreader := TScriptReader.Create();
  ITextMessengerImpl(t_sreader).Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;

  t_runner := TTestRunner.Create();
  ITextMessengerImpl(t_runner).Messenger := t_messenger;
end;

procedure TFrmTestRunner.FormDestroy(Sender: TObject);
begin
  t_vars.Free();
  t_container.Free();
  t_sreader.Free();
  t_messenger.Free();
  t_runner.Free();
end;

end.
