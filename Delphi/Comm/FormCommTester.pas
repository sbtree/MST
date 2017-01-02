unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConnBase, TextMessage, {PCAN,} ExtCtrls;

type
  TfrmCommTester = class(TForm)
    btnRS232: TButton;
    btnCan: TButton;
    memLog: TMemo;
    lblCount: TLabel;
    tmrUpdate: TTimer;
    btnUSB: TButton;
    btnSend: TButton;
    btnRecv: TButton;
    cmbConf: TComboBox;
    cmbSending: TComboBox;
    chkCr: TCheckBox;
    chkLF: TCheckBox;
    procedure btnRS232Click(Sender: TObject);
    procedure btnCanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure btnUSBClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnRecvClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_conn: TCommBase;
    //t_conn2: TCommBase;
    //t_canthread: TPCanReadThread;
    t_messenger: TTextMessenger;
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}
uses RS232, MtxUSB;

procedure TfrmCommTester.btnCanClick(Sender: TObject);
//var s_conf: string;
begin
  {if assigned(t_conn) then FreeAndNil(t_conn);
  t_conn := TPCanLight.Create(self);
  ITextMessengerImpl(t_conn).Messenger := t_messenger;
  t_conn.Timeout := 10000;
  s_conf := trim(cmbConf.Text); //'HWT:USB1CH|PCANDLL:PCAN_USB.dll|baudrate:1M|CANVER:EXT';
  t_conn.Config(s_conf);
  t_conn.Connect();

  if assigned(t_conn2) then FreeAndNil(t_conn2);
  t_conn2 := TPCanLight.Create(self);
  ITextMessengerImpl(t_conn2).Messenger := t_messenger;
  t_conn2.Timeout := 10000;
  s_conf := 'HWT:USB2CH|PCANDLL:PCAN_2USB.dll|baudrate:1M|CANVER:STD';
  t_conn2.Config(s_conf);
  t_conn2.Connect();}
end;

procedure TfrmCommTester.btnRecvClick(Sender: TObject);
var s_recv: string;
begin
  if t_conn.Connected then begin
    t_conn.RecvStr(s_recv);
    //t_conn.RecvStrTimeout(s_recv, 10000);
  end;
end;

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
var s_conf: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TMtxRS232.Create(self);
  ITextMessengerImpl(t_conn).Messenger := t_messenger;
  t_conn.Timeout := 10000;
  s_conf := trim(cmbConf.Text); //'Port:8|baudrate:9600'; //'PARITY', 'DATABITS', 'STOPBITS', 'FLOWCONTROL'
  t_conn.Config(s_conf);
  t_conn.Connect();
end;

procedure TfrmCommTester.btnSendClick(Sender: TObject);
var s_sending: string; //s_ansi: AnsiString;
begin
  //format of CAN-Message: '60A:40800020';
  //format of OROW: 'OR:0008' or 'OR:1:0008'
  s_sending := trim(cmbSending.Text);
  if chkCR.Checked then s_sending := s_sending + Char(#13);
  if chkLF.Checked then s_sending := s_sending + Char(#10);

  if assigned(t_conn) then t_conn.SendStr(s_sending);
  {if assigned(t_conn) then begin
    s_ansi := AnsiString(s_sending);
    t_conn.SendPacket(PByteArray(@s_ansi[1]), length(s_ansi));
  end;}
end;

procedure TfrmCommTester.btnUSBClick(Sender: TObject);
var s_conf: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TMtxUSB.Create(self);
  ITextMessengerImpl(t_conn).Messenger := t_messenger;
  t_conn.Timeout := 10000;
  s_conf := trim(cmbConf.Text); //'VID:$1B97|PID:$2|PSN:1234';
  t_conn.Config(s_conf);
  t_conn.Connect();
end;

procedure TfrmCommTester.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memLog.Lines;
  //t_conn := TPCanLight.Create(self);
  //t_conn.Messenger := t_messenger;
end;

procedure TfrmCommTester.FormDestroy(Sender: TObject);
begin
  if assigned(t_conn) then FreeAndNil(t_conn);
  //if assigned(t_canthread) then FreeAndNil(t_canthread);
  FreeAndNil(t_messenger);
end;

procedure TfrmCommTester.tmrUpdateTimer(Sender: TObject);
//var t_pcan: TPCanLight;
begin
  {t_pcan := TPCanLight(t_conn);
  if assigned(t_pcan) then begin
    lblCount.Caption := format('Tx(%d); Rx(%d)', [t_pcan.CountSending, t_pcan.CountReceive]);
    //UpdateMemoText();
  end;}
end;

end.
