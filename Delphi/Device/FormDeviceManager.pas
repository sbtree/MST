unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage, ConnBase, RelayControl;

type
  TfrmDeviceManager = class(TForm)
    btnFR: TButton;
    btnCreateTree: TButton;
    btnTest: TButton;
    memInfo: TMemo;
    procedure btnFRClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    t_messenger : TTextMessenger;
    t_conn: TConnBase;
    t_relay: TRelayControl;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation

{$R *.dfm}
uses DeviceBase, Multimeter, RS232;

procedure TfrmDeviceManager.btnFRClick(Sender: TObject);
var dmm: TMultimeter;
begin
  dmm := TMultimeter.Create(self);
  ITextMessengerImpl(dmm).Messenger := t_messenger;
  dmm.Free();
  {fIni := TMemIniFile.Create('FlashRunner.ini');
  fr := TFlashRunner.Create(self);
  
  bOk := fr.ConfigDevice(fIni);
  bOk := fr.Connect();
  bOk := fr.SetDynamicMem(0,12345);
  bOk := fr.RunScript('bt_24v.frs', 30000);
  FreeAndNil(fr);
  FreeAndNil(fIni);}
end;

procedure TfrmDeviceManager.btnTestClick(Sender: TObject);
var s_relays: string;
begin
  s_relays := '101,102,103';
  t_relay.CloseRelays(s_relays);
  t_relay.OpenAllRelays;

end;

procedure TfrmDeviceManager.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
  t_messenger.MessageThreshold := ML_INFO;

  t_conn := TMtxRs232.Create(self);
  ITextMessengerImpl(t_conn).Messenger := t_messenger;
  t_conn.Config('Port:5|baudrate:9600');
  t_conn.Connect();


  t_relay := TRelayKeithley.Create();
  ITextMessengerImpl(t_relay).Messenger := t_messenger;
  t_relay.CurConnect := t_conn;
end;

procedure TfrmDeviceManager.FormDestroy(Sender: TObject);
begin
  t_relay.Free();
  t_conn.Disconnect();
  t_conn.Free();
  t_messenger.Free();
end;

end.
