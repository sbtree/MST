unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage, ConnBase, RelayControl, Multimeter;

type
  TfrmDeviceManager = class(TForm)
    btnReset: TButton;
    btnMeasure: TButton;
    memInfo: TMemo;
    cmbMeasure: TComboBox;
    txtRelays: TEdit;
    btnClose: TButton;
    Label1: TLabel;
    btnOpen: TButton;
    btnOpenAll: TButton;
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenAllClick(Sender: TObject);
  private
    t_messenger : TTextMessenger;
    t_multimeter: TMultimeterKeithley;
    t_relay: TRelayControl;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation

{$R *.dfm}
uses DeviceBase, RS232;

procedure TfrmDeviceManager.btnResetClick(Sender: TObject);
begin
  t_multimeter.InitDevice(nil);
end;

procedure TfrmDeviceManager.btnMeasureClick(Sender: TObject);
var f_val: double;
begin
  case cmbMeasure.ItemIndex of
  0: t_multimeter.MeasureR(f_val);
  1: t_multimeter.MeasureDCV(f_val);
  2: t_multimeter.MeasureACV(f_val);
  3: t_multimeter.MeasureDCI(f_val);
  4: t_multimeter.MeasureACI(f_val);
  5: t_multimeter.MeasureF(f_val);
  6: t_multimeter.MeasureP(f_val);
  7: t_multimeter.MeasureT(f_val);
  else;
  end;
end;

procedure TfrmDeviceManager.btnOpenAllClick(Sender: TObject);
begin
  t_relay.OpenAllRelays();
end;

procedure TfrmDeviceManager.btnOpenClick(Sender: TObject);
var s_relays: string;
begin
  s_relays := trim(txtRelays.Text);
  t_relay := t_multimeter.RelayControl;
  t_relay.OpenRelays(s_relays);
end;

procedure TfrmDeviceManager.btnCloseClick(Sender: TObject);
var s_relays: string;
begin
  s_relays := trim(txtRelays.Text);
  t_relay := t_multimeter.RelayControl;
  t_relay.CloseRelays(s_relays);
end;

procedure TfrmDeviceManager.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
  t_messenger.MessageThreshold := ML_INFO;

  t_multimeter := TMultimeterKeithley.Create(self);
  ITextMessengerImpl(t_multimeter).Messenger := t_messenger;
  t_multimeter.InitDevice(nil);
end;

procedure TfrmDeviceManager.FormDestroy(Sender: TObject);
begin
  t_multimeter.Free();
  t_messenger.Free();
end;

end.
