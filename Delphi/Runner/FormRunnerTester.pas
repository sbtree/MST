unit FormRunnerTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ScriptReader, TextMessage, StepContainer, NamedStrings, TestRunner, StepDescriptor;

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
    txtField: TEdit;
    btnGetField: TButton;
    btnPrevious: TButton;
    btnNext: TButton;
    txtCase: TEdit;
    btnRunCase: TButton;
    txtInclusive: TEdit;
    btnSequence: TButton;
    txtExclusive: TEdit;
    txtVariable: TEdit;
    btnVariable: TButton;
    btnClear: TButton;
    chkForce: TCheckBox;
    chkAppend: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnRunStepByNrClick(Sender: TObject);
    procedure btnGetFieldClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_tstep:    TTestStep;
    t_sreader:  TScriptReader;
    t_messenger:TTextMessenger;
    t_container:TStepContainer;
    t_vars:     TNamedStrings;
    t_runner:   TTestRunner;
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnGetFieldClick(Sender: TObject);
var e_field: EStepField; s_fval: string;
begin
  t_tstep := t_container.CurrentStep();
  if assigned(t_tstep) then begin
    if t_sreader.FieldNameChecker.FindName(trim(txtField.Text), e_field) then begin
      s_fval := t_tstep.GetFieldValue(e_field);
      t_messenger.AddMessage('Field value: ' + trim(txtField.Text) + '=' + s_fval);
    end else
      t_messenger.AddMessage('Field is NOT found');
  end else
    t_messenger.AddMessage('Please select a test step, firstly.');
end;

procedure TForm2.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text), chkForce.Checked, chkAppend.Checked);
end;

procedure TForm2.btnRunStepByNrClick(Sender: TObject);
begin
  t_runner.RunStep(trim(txtStepNr.Text));
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_container := TStepContainer.Create();
  t_vars := TNamedStrings.Create();
  t_messenger.Messages := memInfo.Lines;
  t_sreader.Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;

  t_runner := TTestRunner.Create();
  t_runner.Messenger := t_messenger;
  t_runner.StepContainer := t_container;
end;

end.
