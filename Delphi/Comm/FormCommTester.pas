unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConnBase, TextMessage, CAN, ExtCtrls;

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
    t_conn: TConnBase;
    t_canthread: TPCanReadThread;
    t_messenger: TTextMessenger;
  private
    procedure UpdateMemoText();
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}
uses RS232, USB;

procedure TfrmCommTester.btnCanClick(Sender: TObject);
var s_conf: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TPCanLight.Create(self);
  t_conn.Messenger := t_messenger;
  s_conf := trim(cmbConf.Text); //'HWT:USB1CH|PCANDLL:PCAN_USB.dll|baudrate:1M|CANVER:EXT';
  t_conn.Config(s_conf);
  t_conn.Connect();
end;

procedure TfrmCommTester.btnRecvClick(Sender: TObject);
var s_recv: string;
begin
  t_conn.RecvStr(s_recv, true);
end;

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
var s_conf: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TMtxRS232.Create(self);
  t_conn.Messenger := t_messenger;
  s_conf := trim(cmbConf.Text); //'Port:8|baudrate:9600'; //'PARITY', 'DATABITS', 'STOPBITS', 'FLOWCONTROL'
  t_conn.Config(s_conf);
  t_conn.Connect();
end;

procedure TfrmCommTester.btnSendClick(Sender: TObject);
begin
  //format of CAN-Message: '60A:40800020';
  //format of OROW: 'OR:0008'
  t_conn.SendStr(trim(cmbSending.Text) + Char(#13));
end;

procedure TfrmCommTester.btnUSBClick(Sender: TObject);
var s_conf: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TMtxUSB.Create(self);
  t_conn.Messenger := t_messenger;
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
  if assigned(t_canthread) then FreeAndNil(t_canthread);
  FreeAndNil(t_messenger);
end;

procedure TfrmCommTester.tmrUpdateTimer(Sender: TObject);
var t_pcan: TPCanLight;
begin
  t_pcan := TPCanLight(t_conn);
  if assigned(t_pcan) then begin
    lblCount.Caption := format('Tx(%d); Rx(%d)', [t_pcan.CountSending, t_pcan.CountReceive]);
    //UpdateMemoText();
  end;
end;

procedure TfrmCommTester.UpdateMemoText();
var i: integer;
begin
  for i := memLog.Lines.Count to t_messenger.CountLines - 1 do memLog.Lines.Append(t_messenger.LineText[i]);
end;

end.
