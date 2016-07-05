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
    txtSending: TEdit;
    btnSend: TButton;
    btnRecv: TButton;
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
var s_conf, s_send{, s_recv}: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TPCanLight.Create(self);
  t_conn.Messenger := t_messenger;
  s_conf := 'HWT:USB1CH|PCANDLL:PCAN_USB.dll|baudrate:1M|CANVER:EXT';
  if t_conn.Config(s_conf) then ShowMessage('PCAN is configured' + ' [' + s_conf + ']')
  else ShowMessage('PCAN is NOT configured' + ' [' + s_conf + ']');

  if t_conn.Connect then begin
    //t_canthread := TPCanReadThread.Create(TPCanLight(t_conn));
    s_send := '60A:40800020';
    t_conn.Timeout := 3000;
    {if t_conn.SendStr(s_send) then begin
      t_conn.RecvStr(s_recv);
      ShowMessage('PCAN receives string:' + ' [' + s_recv + ']'); 
    end;}
  end else ShowMessage('PCAN is NOT connected' + ' [' + s_conf + ']');
  //FreeAndNil(t_conn);
end;

procedure TfrmCommTester.btnRecvClick(Sender: TObject);
var s_recv: string;
begin
  t_conn.RecvStr(s_recv, true);
end;

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
var s_conf: string; s_send, s_recv: string;
begin
  if assigned(t_conn) then FreeAndNil(t_conn);

  t_conn := TMtxRS232.Create(self);
  t_conn.Messenger := t_messenger;
  //t_rs232.Config('Port:1|baudrate:115200');
  s_conf := 'Port:5|baudrate:9600';
  if t_conn.Config(s_conf) then ShowMessage('RS232 is configured' + ' [' + s_conf + ']')
  else ShowMessage('RS232 is NOT configured' + ' [' + s_conf + ']');

  if t_conn.Connect then begin
    s_send := 'FORM:ELEM?' + Char(#13);
    t_conn.Timeout := 3000;
    if t_conn.SendStr(s_send) then begin
      t_conn.RecvStr(s_recv);
      ShowMessage('RS232 receives string:' + ' [' + s_recv + ']');
    end;
  end else ShowMessage('RS232 is NOT connected' + ' [' + s_conf + ']');

  //FreeAndNil(t_conn);
end;

procedure TfrmCommTester.btnSendClick(Sender: TObject);
begin
  t_conn.SendStr(trim(txtSending.Text));
end;

procedure TfrmCommTester.btnUSBClick(Sender: TObject);
var t_usb: TMtxUSB;
begin
  t_usb := TMtxUSB.Create(self);
  //t_usb.Timeout := 30000;
  t_usb.Connect();
  t_conn := t_usb;
  t_conn.Messenger := t_messenger;
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
    lblCount.Caption := format('Messages: Tx(%d); Rx(%d)', [t_pcan.CountSending, t_pcan.CountReceive]);
    //UpdateMemoText();
  end;
end;

procedure TfrmCommTester.UpdateMemoText();
var i: integer;
begin
  for i := memLog.Lines.Count to t_messenger.CountLines - 1 do memLog.Lines.Append(t_messenger.LineText[i]);
end;

end.
