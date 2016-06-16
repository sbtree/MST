unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConnBase, TextMessage;

type
  TfrmCommTester = class(TForm)
    btnRS232: TButton;
    btnCan: TButton;
    memLog: TMemo;
    lblCount: TLabel;
    btnCount: TButton;
    procedure btnRS232Click(Sender: TObject);
    procedure btnCanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_conn: TConnBase;
    t_messenger: TTextMessenger;
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}
uses RS232, PCAN;

procedure TfrmCommTester.btnCanClick(Sender: TObject);
var s_conf: string; s_send, s_recv: string;
begin
  //t_conn := TPCanLight.Create(self);
  s_conf := 'HWT:USB1CH|PCANDLL:PCAN_USB.dll|baudrate:1M|CANVER:EXT';
  if t_conn.Config(s_conf) then ShowMessage('PCAN is configured' + ' [' + s_conf + ']')
  else ShowMessage('PCAN is NOT configured' + ' [' + s_conf + ']');

  if t_conn.Connect then begin
    s_send := '60A:40800020';
    t_conn.Timeout := 3000;
    {if t_conn.SendStr(s_send) then begin
      t_conn.RecvStr(s_recv);
      ShowMessage('PCAN receives string:' + ' [' + s_recv + ']'); 
    end;}
  end else ShowMessage('PCAN is NOT connected' + ' [' + s_conf + ']');
  //FreeAndNil(t_conn);
end;

procedure TfrmCommTester.btnCountClick(Sender: TObject);
var t_pcan: TPCanLight;
begin
  t_pcan := TPCanLight(t_conn);
  if assigned(t_pcan) then lblCount.Caption := format('Sending: %d; Receiving: %d', [t_pcan.CountSending, t_pcan.CountReceive]);
end;

procedure TfrmCommTester.btnRS232Click(Sender: TObject);
var s_conf: string; s_send, s_recv: string;
begin
  t_conn := TConnRS232.Create(self);
  //t_rs232.Config('Port:1|baudrate:115200');
  s_conf := 'Port:2|baudrate:9600';
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

  FreeAndNil(t_conn);
{   }
end;

procedure TfrmCommTester.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memLog.Lines;
  t_conn := TPCanLight.Create(self);
  t_conn.Messenger := t_messenger;
end;

procedure TfrmCommTester.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_conn);
  FreeAndNil(t_messenger);
end;

end.
