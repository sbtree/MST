unit FormRunnerTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ScriptReader, TextMessage, StepDescriptor, PairStrings, TestRunner;

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
    btnPrevious: TButton;
    btnNext: TButton;
    txtCase: TEdit;
    btnRunCase: TButton;
    txtInclusive: TEdit;
    btnSequence: TButton;
    txtExclusive: TEdit;
    btnClear: TButton;
    chkForce: TCheckBox;
    chkAppend: TCheckBox;
    btnRunFirst: TButton;
    btnRunSequence: TButton;
    btnRepeatSequence: TButton;
    btnRepeatStep: TButton;
    btnRepeatCase: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnRunStepByNrClick(Sender: TObject);
    procedure btnRunCaseClick(Sender: TObject);
    procedure btnSequenceClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnRunFirstClick(Sender: TObject);
    procedure btnRunSequenceClick(Sender: TObject);
    procedure btnRepeatSequenceClick(Sender: TObject);
    procedure btnRepeatStepClick(Sender: TObject);
    procedure btnRepeatCaseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_sreader:  TScriptReader;
    t_messenger:TTextMessenger;
    t_container:TStepContainer;
    t_vars:     TPairStrings;
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

procedure TForm2.btnNextClick(Sender: TObject);
begin
  t_runner.RunStep(t_container.TestSequence.NextStepIndex);
end;

procedure TForm2.btnPreviousClick(Sender: TObject);
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

procedure TForm2.btnRunFirstClick(Sender: TObject);
begin
  t_runner.RunStep(t_container.TestSequence.FirstStepIndex);
end;

procedure TForm2.btnRunSequenceClick(Sender: TObject);
begin
  t_runner.RunSequence(trim(txtInclusive.Text), trim(txtExclusive.Text));
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

procedure TForm2.btnRepeatCaseClick(Sender: TObject);
begin
  t_runner.RepeatCase();
end;

procedure TForm2.btnRepeatSequenceClick(Sender: TObject);
begin
  t_runner.RepeatSequence();
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
  t_vars := TPairStrings.Create();
  t_messenger.Messages := memInfo.Lines;
  t_sreader.Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;

  t_runner := TTestRunner.Create();
  t_runner.Messenger := t_messenger;
  t_runner.StepContainer := t_container;
end;

end.
