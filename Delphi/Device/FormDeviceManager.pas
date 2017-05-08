unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage, ConnBase, RelayControl,
  Multimeter, RS232;

type
  TfrmDeviceManager = class(TForm)
    btnInit: TButton;
    btnMeasure: TButton;
    cmbMeasure: TComboBox;
    txtRelays: TEdit;
    btnClose: TButton;
    Label1: TLabel;
    btnOpen: TButton;
    btnOpenAll: TButton;
    pgcMain: TPageControl;
    pgR100: TTabSheet;
    pgR200: TTabSheet;
    pgR300: TTabSheet;
    pgR400: TTabSheet;
    pgR500: TTabSheet;
    pgInfo: TTabSheet;
    memInfo: TMemo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    btnConnect: TButton;
    procedure btnInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenAllClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    t_messenger : TTextMessenger;
    t_multimeter: TMultimeterKeithley;
    t_relay: TRelayControl;
    t_ser: TSerialAdapter;
  protected
    procedure CreateCheckBox(const rcard: integer);
    procedure EnableRelayCards();
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation
{$R *.dfm}
uses DeviceBase, RS232DlgDyn;

procedure TfrmDeviceManager.btnConnectClick(Sender: TObject);
begin
  SerialDialog.SerialObj := t_ser.SerialObj;
  SerialDialog.ShowModal();
end;

procedure TfrmDeviceManager.btnInitClick(Sender: TObject);
begin
  t_multimeter.InitDevice();
  EnableRelayCards();
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

  t_ser := TSerialAdapter.Create(self);
  t_ser.Config('port:5|baudrate:9600');
  t_multimeter := TMultimeterKeithley.Create(self);
  t_multimeter.DevConnect := t_ser;
  ITextMessengerImpl(t_multimeter).Messenger := t_messenger;
end;

procedure TfrmDeviceManager.FormDestroy(Sender: TObject);
begin
  t_multimeter.Free();
  t_messenger.Free();
  t_ser.Free();
end;

procedure TfrmDeviceManager.CreateCheckBox(const rcard: integer);
begin

end;

procedure TfrmDeviceManager.EnableRelayCards();
begin
  case t_multimeter.RelayCards of
    1: begin
      pgR100.Enabled := true;
    end;
    2: begin
      pgR100.Enabled := true;
      pgR200.Enabled := true;
    end;
    3: begin
      pgR100.Enabled := true;
      pgR200.Enabled := true;
      pgR300.Enabled := true;
    end;
    4: begin
      pgR100.Enabled := true;
      pgR200.Enabled := true;
      pgR300.Enabled := true;
      pgR400.Enabled := true;
    end;
    5: begin
      pgR100.Enabled := true;
      pgR200.Enabled := true;
      pgR300.Enabled := true;
      pgR400.Enabled := true;
      pgR500.Enabled := true;
    end;
  else
    pgcMain.ActivePageIndex := 5;
  end;
end;

end.
