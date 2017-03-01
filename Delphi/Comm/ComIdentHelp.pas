unit ComIdentHelp;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TLoopbackDlg = class(TForm)
    btnOK: TButton;
    bvlMain: TBevel;
    imgLoopback: TImage;
    mmoLoopback: TMemo;
    lblName: TLabel;
    btnCancel: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  LoopbackDlg: TLoopbackDlg;

implementation

{$R *.dfm}

end.
