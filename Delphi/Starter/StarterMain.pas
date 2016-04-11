unit StarterMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids;

type
  TFrmStarterMain = class(TForm)
    sgdTestStations: TStringGrid;
    btnStart: TButton;
    btnCancel: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FrmStarterMain: TFrmStarterMain;

implementation

{$R *.dfm}

uses ShellApi;

procedure TFrmStarterMain.btnStartClick(Sender: TObject);
var s_exe, s_par: string; h_inst: cardinal;
begin
  s_exe := trim(sgdTestStations.Rows[sgdTestStations.Row][1]);
  s_par := '-c"' + trim(sgdTestStations.Rows[sgdTestStations.Row][2]) + '"';
  h_inst := ShellExecute(GetDesktopWindow(), 'open', PChar(s_exe), PChar(s_par), nil, SW_SHOW);
  if h_inst > 32 then begin //successful
    FrmStarterMain.Close();
  end else begin
    //show error message
  end;
end;

procedure TFrmStarterMain.FormCreate(Sender: TObject);
begin
  sgdTestStations.Cells[0,0] := 'Prüfplatz';
  sgdTestStations.Cells[1,0] := 'Exe';
  sgdTestStations.Cells[2,0] := 'Ini';

  //test
  sgdTestStations.Cells[0,1] := 'DIS-2';
  sgdTestStations.Cells[1,1] := 'PP_ATT_UNI.exe';
  sgdTestStations.Cells[2,1] := 'N:\SW_INBE\DIS-2_SW3.2\ini.ini';
end;

end.
