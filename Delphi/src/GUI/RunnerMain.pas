unit RunnerMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFrmTestRunner = class(TForm)
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FrmTestRunner: TFrmTestRunner;

implementation

{$R *.dfm}

end.
