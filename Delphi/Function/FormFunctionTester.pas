unit FormFunctionTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FuncFlashRunner, StdCtrls;

type
  TFormFTMain = class(TForm)
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormFTMain: TFormFTMain;

implementation

{$R *.dfm}

procedure TFormFTMain.btnTestClick(Sender: TObject);
var t_fr_set_dm: FR_Set_DM;
begin
  t_fr_set_dm := FR_Set_DM.Create;

  t_fr_set_dm.Parameterize('0 12345');
  t_fr_set_dm.Initilize('');
  t_fr_set_dm.Execute('');
  FreeAndNil(t_fr_set_dm);
end;

end.
