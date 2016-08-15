unit FormRunnerTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ScriptReader, TextMessage, StepDescriptor, StringPairs, TestRunner;

type
  TForm2 = class(TForm)
    lblInclusive: TLabel;
    lblExclusive: TLabel;
    btnRunStepByNr: TButton;
    txtStepNr: TEdit;
    txtScriptFile: TEdit;
    btnOpenScript: TButton;
    btnReadScript: TButton;
    btnSaveScript: TButton;
    memInfo: TMemo;
    txtCase: TEdit;
    btnRunCase: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnRunStepByNrClick(Sender: TObject);
    procedure btnRunCaseClick(Sender: TObject);
    procedure btnSequenceClick(Sender: TObject);
    procedure btnPreviousStepClick(Sender: TObject);
    procedure btnNextStepClick(Sender: TObject);
    procedure btnRunFirstStepClick(Sender: TObject);
    procedure btnRunSequenceClick(Sender: TObject);
    procedure btnRepeatSequenceClick(Sender: TObject);
    procedure btnRepeatStepClick(Sender: TObject);
    procedure btnRepeatCaseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnRunFirstCaseClick(Sender: TObject);
    procedure btnRunPrevCaseClick(Sender: TObject);
    procedure btnRunNextCaseClick(Sender: TObject);
    procedure chkJumpClick(Sender: TObject);
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
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnClearClick(Sender: TObject);
begin
  t_sreader.Clear();
end;

procedure TForm2.btnNextStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_container.TestSequence.NextStepIndex);
end;

procedure TForm2.btnOpenScriptClick(Sender: TObject);
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

procedure TForm2.btnPreviousStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_container.TestSequence.PreviousStepIndex);
end;

procedure TForm2.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text), chkForce.Checked, chkAppend.Checked);
end;

procedure TForm2.btnRunCaseClick(Sender: TObject);
begin
  t_runner.RunCase(trim(txtCase.Text));
end;

procedure TForm2.btnRunFirstCaseClick(Sender: TObject);
begin
  t_runner.RunCase(t_container.TestSequence.FirstCaseIndex);
end;

procedure TForm2.btnRunFirstStepClick(Sender: TObject);
begin
  t_runner.RunStep(t_container.TestSequence.FirstStepIndex);
end;

procedure TForm2.btnRunSequenceClick(Sender: TObject);
begin
  if t_runner.SetSequence(trim(txtInclusive.Text), trim(txtExclusive.Text)) then t_runner.RunSequence();
end;

procedure TForm2.btnRunStepByNrClick(Sender: TObject);
begin
  t_runner.RunStep(trim(txtStepNr.Text));
end;

procedure TForm2.btnSequenceClick(Sender: TObject);
var s_casenrs: string;
begin
  t_container.UpdateSequence(trim(txtInclusive.Text), trim(txtExclusive.Text));
  s_casenrs := t_container.TestSequence.CaseNrList.DelimitedText;

  if s_casenrs <> '' then t_messenger.AddMessage('Sequence Cases: ' + s_casenrs)
  else t_messenger.AddMessage('Test Sequence: no case')
end;

procedure TForm2.chkJumpClick(Sender: TObject);
begin
  t_runner.JumpMinusStep := chkJump.Checked;
end;

procedure TForm2.btnRunNextCaseClick(Sender: TObject);
begin
  t_runner.RunCase(t_container.TestSequence.NextCaseIndex)
end;

procedure TForm2.btnRunPrevCaseClick(Sender: TObject);
begin
  t_runner.RunCase(t_container.TestSequence.PreviousCaseIndex)
end;

procedure TForm2.btnRepeatCaseClick(Sender: TObject);
begin
  t_runner.RunCase(t_container.TestSequence.CurrentCaseIndex);
end;

procedure TForm2.btnRepeatSequenceClick(Sender: TObject);
begin
  t_runner.RunSequence();
end;

procedure TForm2.btnRepeatStepClick(Sender: TObject);
begin
  t_runner.RepeatStep();
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_container := TStepContainer.Create();
  t_vars := TStringPairs.Create();
  t_messenger.Messages := memInfo.Lines;
  t_sreader.Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;

  t_runner := TTestRunner.Create();
  t_runner.Messenger := t_messenger;
  t_runner.StepContainer := t_container;
end;

end.
