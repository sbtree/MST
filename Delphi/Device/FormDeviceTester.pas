unit FormDeviceTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FlashRunner, IniFiles, StdCtrls;

type
  TfrmDeviceTester = class(TForm)
    btnFR: TButton;
    procedure btnFRClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceTester: TfrmDeviceTester;

implementation

{$R *.dfm}

procedure TfrmDeviceTester.btnFRClick(Sender: TObject);
var fIni: TMemIniFile; fr: TFlashRunner; bOk: boolean; sTmp: string;
begin
  fIni := TMemIniFile.Create('FlashRunner.ini');
  fr := TFlashRunner.Create(self);
  bOk := fr.ConfigDevice(fIni);
  bOk := fr.Connect();
  bOk := fr.SetDynamicMem(0,12345);
  FreeAndNil(fr);
  FreeAndNil(fIni);
end;

end.
