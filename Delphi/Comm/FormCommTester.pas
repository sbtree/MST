unit FormCommTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommBase, TextMessage, ExtCtrls, ConnManager;

type
  TfrmCommTester = class(TForm)
    memLog: TMemo;
    tmrUpdate: TTimer;
    btnSend: TButton;
    btnRecv: TButton;
    cmbConf: TComboBox;
    cmbSending: TComboBox;
    chkCr: TCheckBox;
    chkLF: TCheckBox;
    btnConnect: TButton;
    btnDisconnect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnRecvClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
  private
    { Private-Deklarationen }
    t_messenger: TTextMessenger;
    t_conmgr: TConnManager;
    t_conn: TConnBase;
  protected
    procedure CreateNameType(const idx: integer; var sname: string; var etype: EConnectType);
  public
    { Public-Deklarationen }
  end;

var
  frmCommTester: TfrmCommTester;

implementation

{$R *.dfm}
uses RS232, MtxUSB, PCAN;

procedure TfrmCommTester.CreateNameType(const idx: integer; var sname: string; var etype: EConnectType);
begin
  case idx of
    0: begin
      sname := 'RS232_01';
      etype := CT_RS232;
    end;
    1: begin
      sname := 'PCAN_01';
      etype := CT_PCAN;
    end;
    2: begin
      sname := 'PCAN_02';
      etype := CT_PCAN;
    end;
    3: begin
      sname := 'MTXUSB_01';
      etype := CT_MTXUSB;
    end;
    else begin
      sname := 'UNKNOWN';
      etype := CT_UNKNOWN;
    end;
  end;
end;

procedure TfrmCommTester.btnConnectClick(Sender: TObject);
var e_ctype: EConnectType; s_cname, s_conf: string;
begin
  CreateNameType(cmbConf.ItemIndex, s_cname, e_ctype);
  t_conn := t_conmgr.CreateConnect(s_cname, e_ctype);
  if assigned(t_conn) then begin
    t_conn.Timeout := 10000;
    s_conf := trim(cmbConf.Text);
    t_conn.Config(s_conf);
    t_conn.Connect();
  end;
end;

procedure TfrmCommTester.btnDisconnectClick(Sender: TObject);
var e_ctype: EConnectType; s_cname: string;
begin
  CreateNameType(cmbConf.ItemIndex, s_cname, e_ctype);
  t_conn := t_conmgr.GetConnect(s_cname);
  if assigned(t_conn) then t_conn.Disconnect();
end;

procedure TfrmCommTester.btnRecvClick(Sender: TObject);
var s_recv: string;
begin
  if t_conn.Connected then begin
    t_conn.RecvStr(s_recv);
    //t_conn.RecvStrTimeout(s_recv, 10000);
  end;
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

procedure TfrmCommTester.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memLog.Lines;
  t_conmgr := TConnManager.Create(self);
  ITextMessengerImpl(t_conmgr).Messenger := t_messenger;
end;

procedure TfrmCommTester.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_conmgr);
  FreeAndNil(t_messenger);
end;

end.
