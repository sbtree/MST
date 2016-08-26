unit FormScriptTester;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScriptReader, StepData, StepGroup, TextMessage, StringPairs;

type
  TfrmScriptTester = class(TForm)
    btnGetStepByNr: TButton;
    txtStepNr: TEdit;
    btnGetStepByIndex: TButton;
    txtStepIndex: TEdit;
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
    btnGetCase: TButton;
    txtInclusive: TEdit;
    btnSequenceCases: TButton;
    txtExclusive: TEdit;
    lblInclusive: TLabel;
    lblExclusive: TLabel;
    txtVariable: TEdit;
    btnVariable: TButton;
    btnClear: TButton;
    chkForce: TCheckBox;
    chkAppend: TCheckBox;
    btnSequenceSteps: TButton;
    btnCurrentCase: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetStepByNrClick(Sender: TObject);
    procedure btnGetStepByIndexClick(Sender: TObject);
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnSaveScriptClick(Sender: TObject);
    procedure btnGetFieldClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnGetCaseClick(Sender: TObject);
    procedure btnSequenceCasesClick(Sender: TObject);
    procedure btnVariableClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSequenceStepsClick(Sender: TObject);
    procedure btnCurrentCaseClick(Sender: TObject);
  private
    t_sreader:  TScriptReader;
    t_tstep:    TTestStep;
    t_messenger:TTextMessenger;
    t_container:TStepContainer;
    t_vars:     TStringPairs;
  public
    { Public-Deklarationen }
  end;

var
  frmScriptTester: TfrmScriptTester;

implementation
{$R *.dfm}

procedure TfrmScriptTester.btnClearClick(Sender: TObject);
begin
  t_sreader.Clear();
end;

procedure TfrmScriptTester.btnCurrentCaseClick(Sender: TObject);
var t_tcase: TTestCase;
begin
  t_tcase := t_container.CaseGroup.CurrentCase;
  if assigned(t_tcase) then
    t_messenger.AddMessage(format('Test Case: Nr=%d, T=%s, count=%d, step from=%s, to=%s', [t_tcase.CaseNr, t_tcase.CaseTitle, t_tcase.StepCount, t_tcase.StepNrOf(0), t_tcase.StepNrOf(t_tcase.StepCount - 1)]))
  else
    t_messenger.AddMessage(format('Test Case (Nr=%s) is NOT found.', [trim(txtCase.Text)]));
end;

procedure TfrmScriptTester.btnGetCaseClick(Sender: TObject);
var t_tcase: TTestCase;
begin
  if (t_container.CaseGroup.CaseCount > 0) then begin
    t_tcase := t_container.CaseGroup.CaseByNr(trim(txtCase.Text));
    if assigned(t_tcase) then
      t_messenger.AddMessage(format('Test Case: Nr=%d, T=%s, count=%d, step from=%s, to=%s', [t_tcase.CaseNr, t_tcase.CaseTitle, t_tcase.StepCount, t_tcase.StepNrOf(0), t_tcase.StepNrOf(t_tcase.StepCount - 1)]))
    else
      t_messenger.AddMessage(format('Test Case (Nr=%s) is NOT found.', [trim(txtCase.Text)]));
  end else t_messenger.AddMessage('No case is loaded', '', ML_WARNING);
end;

procedure TfrmScriptTester.btnGetFieldClick(Sender: TObject);
var e_field: EStepField; s_fval: string;
begin
  t_tstep := t_container.CurrentStep;
  if assigned(t_tstep) then begin
    if t_sreader.FieldNameChecker.IsNameValid(trim(txtField.Text), e_field) then begin
      s_fval := t_tstep.GetFieldValue(e_field);
      t_messenger.AddMessage('Field value: ' + t_sreader.FieldNameChecker.FieldName(e_field) + '=' + s_fval);
    end else
      t_messenger.AddMessage(format('The field(%s) is NOT found.', [trim(txtField.Text)]));
  end else
    t_messenger.AddMessage('Please select a test step, firstly.');
end;

procedure TfrmScriptTester.btnGetStepByIndexClick(Sender: TObject);
var i_idx: integer;
begin
  if (t_container.StepCount > 0) then begin
    i_idx := StrToInt(trim(txtStepIndex.Text));
    t_tstep := t_container.StepByIndex(i_idx);
    if assigned(t_tstep) then
      t_messenger.AddMessage(format('Current step (Index=%d): Nr=%s', [i_idx,t_container.StepNrOf(i_idx)]))
    else
      t_messenger.AddMessage(format('Index=%d is NOT found.', [i_idx]));
  end else
    t_messenger.AddMessage('No step is loaded.');
end;

procedure TfrmScriptTester.btnGetStepByNrClick(Sender: TObject);
var s_nr: string;
begin
  if (t_container.StepCount > 0) then begin
    s_nr := trim(txtStepNr.Text);
    t_tstep := t_container.StepByNr(s_nr);
    if assigned(t_tstep) then
      t_messenger.AddMessage('Current step: Nr=' + s_nr + ', T=' + t_tstep.GetFieldValue(SF_T))
    else
      t_messenger.AddMessage('Nr=' + s_nr + ' is NOT found');
  end else
    t_messenger.AddMessage('No step is loaded.');
end;

procedure TfrmScriptTester.btnNextClick(Sender: TObject);
begin
  t_tstep := t_container.NextStep;
  if assigned(t_tstep) then
    t_messenger.AddMessage('Next step: Nr=' + t_tstep.GetFieldValue(SF_NR))
  else
    t_messenger.AddMessage('Next step is NOT valid.');
end;

procedure TfrmScriptTester.btnOpenScriptClick(Sender: TObject);
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

procedure TfrmScriptTester.btnPreviousClick(Sender: TObject);
begin
  t_tstep := t_container.PreviousStep;
  if assigned(t_tstep) then
    t_messenger.AddMessage('Previous step: Nr=' + t_tstep.GetFieldValue(SF_NR))
  else
    t_messenger.AddMessage('Previous step is NOT found');
end;

procedure TfrmScriptTester.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text), chkForce.Checked, chkAppend.Checked);
end;

procedure TfrmScriptTester.btnSaveScriptClick(Sender: TObject);
begin
  t_sreader.SaveToFile();
end;

procedure TfrmScriptTester.btnSequenceCasesClick(Sender: TObject);
var s_casenrs: string; t_seq: TTestSequence;
begin
  t_seq := TTestSequence.Create();
  t_container.UpdateSequence(t_seq, trim(txtInclusive.Text), trim(txtExclusive.Text));
  s_casenrs := t_seq.CaseGroup.CaseNumbers;
  FreeAndNil(t_seq);

  if s_casenrs <> '' then t_messenger.AddMessage('Sequence Cases: ' + s_casenrs)
  else t_messenger.AddMessage('Test Sequence: no case')
end;

procedure TfrmScriptTester.btnSequenceStepsClick(Sender: TObject);
var s_stepnrs: string;t_seq: TTestSequence;
begin
  t_seq := TTestSequence.Create();
  t_container.UpdateSequence(t_seq, trim(txtInclusive.Text), trim(txtExclusive.Text));
  s_stepnrs := t_seq.StepNumbers;
  FreeAndNil(t_seq);

  if s_stepnrs <> '' then t_messenger.AddMessage('Sequence Steps: ' + s_stepnrs)
  else t_messenger.AddMessage('Test Sequence: no step')
end;

procedure TfrmScriptTester.btnVariableClick(Sender: TObject);
var s_varname, s_varval: string;
begin
  s_varname := trim(txtVariable.Text);
  if (t_vars.GetPairValue(s_varname, s_varval)) then
    t_messenger.AddMessage(format('Variable: name=%s, value=%s', [s_varname, s_varval]))
  else
    t_messenger.AddMessage(format('Variable NOT found: name=%s', [s_varname]))
end;

procedure TfrmScriptTester.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_container := TStepContainer.Create();
  t_vars := TStringPairs.Create();
  t_messenger.Messages := memInfo.Lines;
  ITextMessengerImpl(t_sreader).Messenger := t_messenger;
  t_sreader.StepContainer := t_container;
  t_sreader.VarContainer := t_vars;
end;

procedure TfrmScriptTester.FormDestroy(Sender: TObject);
begin
  t_sreader.Free();
  t_messenger.Free();
  t_container.Free();
  t_vars.Free();
end;

end.
