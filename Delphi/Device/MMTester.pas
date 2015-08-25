unit MMTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Serial3,ExtCtrls, CPort, CPortCtl, RegExpr;

type
  TForm1 = class(TForm)
    memRecv: TMemo;
    btnStart: TButton;
    lblPort: TLabel;
    lblBaudrate: TLabel;
    cmbBaudrate: TComboBox;
    cmbPort: TComboBox;
    txtSend: TEdit;
    chbRecv: TCheckBox;
    btnClear: TButton;
    btnExport: TButton;
    lblLoop: TLabel;
    txtLoop: TEdit;
    lstSending: TListBox;
    t_comport: TComPort;
    chbVerify: TCheckBox;
    txtVerify: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure cmbPortChange(Sender: TObject);
    procedure cmbBaudrateChange(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure lstSendingDblClick(Sender: TObject);
    procedure chbRecvClick(Sender: TObject);
    procedure chbVerifyClick(Sender: TObject);
    procedure txtVerifyEnter(Sender: TObject);
  protected
    procedure Transmit();
    function SendStr(const str: string): integer;
    function RecvStr(var str:string): integer;
  private
    { Private-Deklarationen }
    t_ser: TSerial;
    i_timeout: cardinal;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
const C_DELAY_MSEC: Cardinal = 10;

procedure Delay(const msec: cardinal);
var i_timeout: cardinal;
begin
  i_timeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() >= i_timeout);
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Str: String;
begin
  t_comport.ReadStr(Str, Count);
  i_timeout := GetTickCount() - i_timeout;
  memRecv.Lines.Add(format('%0.3f:<%s',[i_timeout/1000, Str]));
  i_timeout := GetTickCount();
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  memRecv.Lines.Clear();
end;

procedure TForm1.Transmit();
var i_tstart, i_tend: cardinal; s_buffer: string; i: integer; t_cmds:TStringList;
    t_regex: TRegExpr; s_tmp: string;
begin
  s_buffer := trim(txtSend.Text);
  s_buffer := StringReplace(s_buffer, '<CR>', Char(13),[rfReplaceAll, rfIgnoreCase]) + Char(13);
  t_cmds := TStringList.create;
  ExtractStrings([Char(13)], [], PChar(s_buffer), t_cmds);
  for i := 0 to t_cmds.Count - 1 do
  begin
    i_tstart := GetTickCount();
    SendStr(t_cmds[i] + Char(13));
    i_tend := GetTickCount();
    memRecv.Lines.Add(format('%0.3f:>%s',[(i_tend - i_tstart)/1000, t_cmds[i]]));
  end;
  {i_timeout := GetTickCount();}
  if chbRecv.Checked then
  begin
    s_buffer := '';
    i_tstart := GetTickCount();
    RecvStr(s_buffer);
    i_tend := GetTickCount();
    if (chbVerify.Checked and (trim(txtVerify.Text)<>'')) then
    begin
      t_regex := TRegExpr.Create;
      t_regex.Expression := trim(txtVerify.Text);
      s_tmp := StringReplace(s_buffer, char(13), '', [rfReplaceAll, rfIgnoreCase]);
      if (t_regex.Exec(s_tmp)) then s_buffer := s_buffer + '='
      else s_buffer := s_buffer + '?';
      FreeAndNil(t_regex);
    end;
    memRecv.Lines.Add(format('%0.3f:<%s',[(i_tend - i_tstart)/1000, s_buffer]));
  end;
  FreeAndNil(t_cmds);
end;

procedure TForm1.txtVerifyEnter(Sender: TObject);
begin
  txtVerify.SelectAll;
end;

procedure TForm1.btnExportClick(Sender: TObject);
var
  saveDialog : TSaveDialog;    // Save dialog variable
begin
  // Create the save dialog object - assign to our save dialog variable
  saveDialog := TSaveDialog.Create(self);

  // Give the dialog a title
  saveDialog.Title := 'Export Result';

  // Set up the starting directory to be the current one
  saveDialog.InitialDir := GetCurrentDir;

  // Allow only .txt and .doc file types to be saved
  saveDialog.Filter := 'Text file|*.txt|All files|*.*';

  // Set the default extension
  saveDialog.DefaultExt := 'txt';

  // Select text files as the starting filter type
  saveDialog.FilterIndex := 1;

  // Display the open file dialog
  if saveDialog.Execute then memRecv.Lines.SaveToFile(saveDialog.FileName);

  // Free up the dialog
  saveDialog.Free;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var i, n: integer; s_text: String;
begin
  if not t_ser.Active then  t_ser.Active := true;
  if t_ser.Active then
  {if not t_comport.Connected then t_comport.open();
  if t_comport.Connected then}
  begin
    s_text := trim(txtSend.Text);
    if lstSending.Items.IndexOf(s_text) < 0 then lstSending.Items.Add(s_text);

    If not TryStrToInt(txtLoop.Text, n) then n := 1;
    if n<=0 then n:= 1;
    for i := 1 to n do Transmit();
  end;

end;

procedure TForm1.chbRecvClick(Sender: TObject);
begin
  chbVerify.Enabled := chbRecv.Checked;
  txtVerify.Enabled := chbVerify.Checked;
end;

procedure TForm1.chbVerifyClick(Sender: TObject);
begin
  txtVerify.Enabled := chbVerify.Checked;
  if chbVerify.Checked then txtVerify.SetFocus;
end;

procedure TForm1.cmbBaudrateChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Baudrate := StrToInt(cmbBaudrate.Text);
  t_ser.Active := b_active;
end;

procedure TForm1.cmbPortChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Port := StrToInt(cmbPort.Text);
  t_ser.Active := b_active;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  t_ser :=TSerial.Create(self);
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
  t_ser.Name := 'Multimeter';
  t_ser.Baudrate := StrToInt(cmbBaudrate.Items[cmbBaudrate.ItemIndex]);
  t_ser.Port := StrToInt(cmbPort.Items[cmbPort.ItemIndex]);
  i_timeout := 5000;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_ser);
end;

procedure TForm1.lstSendingDblClick(Sender: TObject);
begin
  txtSend.Text := lstSending.Items[lstSending.ItemIndex]
end;

function TForm1.SendStr(const str: string): integer;
var ch: char; timeout: cardinal;
begin
  timeout := GetTickCount() + i_timeout;
  repeat if (t_ser.ReadChar(ch) <> 1) then Delay(C_DELAY_MSEC);
  until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));

  t_ser.WriteString(str);
  repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
  until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= timeout));
  result := length(str);
  {result := t_comport.WriteStr(str);}
end;

function TForm1.RecvStr(var str:string): integer;
var ch: char; timeout: cardinal;
begin
  timeout := GetTickCount() + i_timeout;
  repeat if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
  until ((t_ser.RxWaiting > 0) or (GetTickCount() >= timeout));

  str := '';
  repeat
  begin
    ch := chr(0);
    if (t_ser.ReadChar(ch) = 1) then str := str + ch
    else Delay(C_DELAY_MSEC);
  end;
  until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));
  result := length(str);
  {result := t_comport.ReadStr(str,result);}
end;

end.
