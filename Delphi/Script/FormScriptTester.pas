unit FormScriptTester;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScriptReader, StepDescriptor, TextMessage;

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
  private
    t_sreader : TScriptReader;
    t_tstep   : TTestStep;
    t_messenger: TTextMessenger;
  public
    { Public-Deklarationen }
  end;

var
  frmScriptTester: TfrmScriptTester;

implementation

{$R *.dfm}
uses StepContainer;

procedure TfrmScriptTester.btnGetFieldClick(Sender: TObject);
var i_idx: integer; e_field: EStepField; s_fval: string;
begin
  t_tstep := t_sreader.StepContainer.CurrentStep();
  if assigned(t_tstep) then begin
    i_idx := IndexText(trim(txtField.Text), CSTR_FIELD_NAMES_V01);
    if ((i_idx >= Ord(Low(EStepField))) and (i_idx <= Ord(High(EStepField)))) then begin
      e_field := EStepField(i_idx);
      s_fval := t_tstep.StepFields[e_field].InputString;
      t_messenger.AddMessage('Field value: ' + CSTR_FIELD_NAMES_V01[e_field] + '=' + s_fval);
    end else
      t_messenger.AddMessage('Field is NOT found');
  end else
    t_messenger.AddMessage('Please select a test step, firstly.');
end;

procedure TfrmScriptTester.btnGetStepByIndexClick(Sender: TObject);
var i_idx: integer;
begin
  if (t_sreader.StepContainer.StepCount > 0) then begin
    i_idx := StrToInt(trim(txtStepIndex.Text));
    t_tstep := t_sreader.StepContainer.GetStepByIndex(i_idx);
    if assigned(t_tstep) then
      t_messenger.AddMessage(format('Current step (Index=%d): Nr=%s', [i_idx, t_tstep.StepFields[SF_NR].InputString]))
    else
      t_messenger.AddMessage(format('Index=%d is NOT found.', [i_idx]));
  end else
    t_messenger.AddMessage('No step is loaded.');
end;

procedure TfrmScriptTester.btnGetStepByNrClick(Sender: TObject);
var s_nr: string;
begin
  if (t_sreader.StepContainer.StepCount > 0) then begin
    s_nr := trim(txtStepNr.Text);
    t_tstep := t_sreader.StepContainer.GetStepByNr(s_nr);
    if assigned(t_tstep) then
      t_messenger.AddMessage('Current step: Nr=' + s_nr + ', T=' + t_tstep.StepFields[SF_T].InputString)
    else
      t_messenger.AddMessage('Nr=' + s_nr + ' is NOT found');
  end else
    t_messenger.AddMessage('No step is loaded.');
end;

procedure TfrmScriptTester.btnNextClick(Sender: TObject);
begin
  t_tstep := t_sreader.StepContainer.NextStep();
  if assigned(t_tstep) then
    t_messenger.AddMessage('Next step: Nr=' + t_tstep.StepFields[SF_NR].InputString)
  else
    t_messenger.AddMessage('Next step is NOT valid.');
end;

procedure TfrmScriptTester.btnOpenScriptClick(Sender: TObject);
var
  t_dialog : TOpenDialog;    // Save dialog variable
begin
  t_dialog := TSaveDialog.Create(self); // Create the save dialog object - assign to our save dialog variable
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
  t_tstep := t_sreader.StepContainer.PreviousStep();
  if assigned(t_tstep) then
    t_messenger.AddMessage('Previous step: Nr=' + t_tstep.StepFields[SF_NR].InputString)
  else
    t_messenger.AddMessage('Previous is NOT found');
end;

procedure TfrmScriptTester.btnReadScriptClick(Sender: TObject);
begin
  if (t_sreader.ReadFromFile(trim(txtScriptFile.Text), true)) then
    t_messenger.AddMessage(format('The test Script is loaded successfully: %d steps', [t_sreader.StepContainer.StepCount]))
  else
    t_messenger.AddMessage('Failed to read file!');
end;

procedure TfrmScriptTester.btnSaveScriptClick(Sender: TObject);
var s_fsave, s_ext: string;
begin
  s_fsave := trim(txtScriptFile.Text);
  s_ext := RightStr(s_fsave, 4);
  s_fsave := LeftStr(s_fsave, length(s_fsave) - 4) + '_new' + s_ext;
  if t_sreader.SaveToFile(s_fsave) then t_messenger.AddMessage('Script is save in "' + s_fsave + '".' )
  else t_messenger.AddMessage('Failed to save the script into file.' )
end;

procedure TfrmScriptTester.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
end;

procedure TfrmScriptTester.FormDestroy(Sender: TObject);
begin
  t_sreader.Free();
  t_messenger.Free();
end;

end.
