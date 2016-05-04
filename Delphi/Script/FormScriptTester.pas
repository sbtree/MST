unit FormScriptTester;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScriptReader;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetStepByNrClick(Sender: TObject);
    procedure btnGetStepByIndexClick(Sender: TObject);
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnReadScriptClick(Sender: TObject);
    procedure btnSaveScriptClick(Sender: TObject);
  private
    t_sreader : TScriptReader;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmScriptTester: TfrmScriptTester;

implementation

{$R *.dfm}
uses StepContainer, StepDescriptor;

procedure TfrmScriptTester.btnGetStepByIndexClick(Sender: TObject);
var t_tstep: TTestStep; i_idx: integer;
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text));
  i_idx := StrToInt(trim(txtStepIndex.Text));
  t_tstep := t_sreader.StepContainer.GetStepByIndex(i_idx);
  if (t_tstep <> Nil) then ShowMessage('Nr=' + t_tstep.StepFields[SF_NR].InputString + ', T=' + t_tstep.StepFields[SF_T].InputString)
  else ShowMessage('Index=' + txtStepIndex.Text + ' is NOT found');
end;

procedure TfrmScriptTester.btnGetStepByNrClick(Sender: TObject);
var t_tstep: TTestStep; s_nr: string;
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text));
  s_nr := trim(txtStepNr.Text);
  t_tstep := t_sreader.StepContainer.GetStepByNr(s_nr);
  if (t_tstep <> Nil) then ShowMessage('Nr=' + s_nr + ', T=' + t_tstep.StepFields[SF_T].InputString)
  else ShowMessage('Nr=' + s_nr + ' is NOT found');
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

procedure TfrmScriptTester.btnReadScriptClick(Sender: TObject);
begin
  t_sreader.ReadFromFile(trim(txtScriptFile.Text), true);
end;

procedure TfrmScriptTester.btnSaveScriptClick(Sender: TObject);
var s_fsave, s_ext: string;
begin
  s_fsave := trim(txtScriptFile.Text);
  s_ext := RightStr(s_fsave, 4);
  s_fsave := LeftStr(s_fsave, length(s_fsave) - 4) + '_new' + s_ext;
  if t_sreader.SaveToFile(s_fsave) then ShowMessage('Script is save in "' + s_fsave + '".' );
end;

procedure TfrmScriptTester.FormCreate(Sender: TObject);
begin
  t_sreader := TScriptReader.Create();
end;

procedure TfrmScriptTester.FormDestroy(Sender: TObject);
begin
  t_sreader.Free();
end;

end.
