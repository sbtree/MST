unit BootTesterMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Serial3,ExtCtrls, {CPort, CPortCtl,} RegExpr;

type
  EBootState = (
                BS_UNKNOWN,
                BS_ORIGIN,
                BS_MTXSRV,
                BS_MTXAPP
                );

  TFrmBootTester = class(TForm)
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
    //t_comport: TComPort;
    chbVerify: TCheckBox;
    txtVerify: TEdit;
    btnReset: TButton;
    cmbTo: TComboBox;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    //procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure cmbPortChange(Sender: TObject);
    procedure cmbBaudrateChange(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure lstSendingDblClick(Sender: TObject);
    procedure chbRecvClick(Sender: TObject);
    procedure chbVerifyClick(Sender: TObject);
    procedure txtVerifyEnter(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  protected
    procedure Transmit();
    function SendStr(const str: string): integer;
    function RecvStr(var str:string): integer;
    function BootState(): EBootState;
    function ExpectStr(const str: string; var sRecv: string; const msecs: cardinal): boolean;

  private
    { Private-Deklarationen }
    t_ser: TSerial;
    c_timeout: cardinal;
  public
    { Public-Deklarationen }
  end;

var
  FrmBootTester: TFrmBootTester;

implementation

{$R *.dfm}
const
  C_DELAY_MSEC: Cardinal = 50;
  CSTR_FORMAT_TO : string = '%0.3f--------- ---------- ---------- ---------->';
  CSTR_FORMAT_FROM : string = '%0.3f<--------- ---------- ---------- ----------';

procedure Delay(const msec: cardinal);
var i_timeout: cardinal;
begin
  i_timeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() >= i_timeout);
end;

{procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Str: String;
begin
  t_comport.ReadStr(Str, Count);
  i_timeout := GetTickCount() - i_timeout;
  memRecv.Lines.Add(format('%0.3f:<%s',[i_timeout/1000, Str]));
  i_timeout := GetTickCount();
end;}

procedure TFrmBootTester.btnClearClick(Sender: TObject);
begin
  memRecv.Lines.Clear();
end;

procedure TFrmBootTester.btnCloseClick(Sender: TObject);
var a: array[-1..5] of char;
begin
  t_ser.Active := false;
end;

procedure TFrmBootTester.Transmit();
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

procedure TFrmBootTester.txtVerifyEnter(Sender: TObject);
begin
  txtVerify.SelectAll;
end;

procedure TFrmBootTester.btnExportClick(Sender: TObject);
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
function TFrmBootTester.BootState(): EBootState;
const
  CSTR_BOOT: string = 'BOOT?';
  CSTR_SERVICE: string = 'SERVICE';
  CSTR_APPLICATION: string = 'APPLICATION';
  CSTR_ERROR: string = 'ERROR!';
var sSend, sRecv, sTemp: string; cTimeout, tStart, tEnd: cardinal; ch: char; bRepeat: boolean;
begin
  result := BS_UNKNOWN;
  if t_ser.Active then begin
    repeat
      cTimeout := GetTickCount() + c_timeout;
      if (t_ser.ReadString(sTemp) > 0) then memRecv.Lines.Add(sTemp);

      tStart := GetTickCount();
      sSend := CSTR_BOOT + Char(13);
      t_ser.WriteString(sSend);
      while ((t_ser.TxWaiting > 0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
      tEnd := GetTickCount();
      memRecv.Lines.Add(format(CSTR_FORMAT_TO,[(tEnd - tStart)/1000.0]));
      memRecv.Lines.Add(sSend);

      tStart := GetTickCount(); sRecv := '';
      while ((t_ser.RxWaiting <= 0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
      repeat
        ch := chr(0);
        if (t_ser.ReadChar(ch) = 1) then if (not (ch = char(0))) then sRecv := sRecv + ch;
        if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
      until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= cTimeout));
      tEnd := GetTickCount();
      sTemp := UpperCase(trim(sRecv));
      memRecv.Lines.Add(format(CSTR_FORMAT_FROM,[(tEnd - tStart)/1000.0]));
      if (sRecv = '') then memRecv.Lines.Add('[empty]')
      else memRecv.Lines.Add(sRecv);

      bRepeat := false;
      if (Pos(CSTR_APPLICATION, sTemp) > 0) then result := BS_MTXAPP
      else if (Pos(CSTR_SERVICE, sTemp) > 0) then result := BS_MTXSRV
      else if (Pos(CSTR_ERROR, sTemp) > 0) then bRepeat := true
      else if (sTemp = '') then result := BS_UNKNOWN
      else result := BS_ORIGIN;
    until ((not bRepeat) or (GetTickCount() >= cTimeout));
  end;
end;

function TFrmBootTester.ExpectStr(const str: string; var sRecv: string; const msecs: cardinal): boolean;
var cTimeout: cardinal; ch: char; sTemp, sIn: string;
begin
  result := false;
  if t_ser.Active then begin
    cTimeout := GetTickCount() + msecs;
    sIn := UpperCase(str);
    while ((t_ser.RxWaiting <= 0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
    repeat
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then if (not (ch = char(0))) then sRecv := sRecv + ch;
      sTemp := UpperCase(sRecv);
      if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
    until ((Pos(sIn, sTemp) > 0) or (GetTickCount() >= cTimeout));
  end;

end;

procedure TFrmBootTester.btnResetClick(Sender: TObject);
const
  CSTR_WAITING : string = 'WAITING...';
  CSTR_CHECKSUM : string = 'CHECKSUM';
  CSTR_STARTING : string = 'STARTING APPLICATION...';
  CSTR_RESET: string = 'RESET!';
  CSTR_RUN: string = 'RUN!';
  CSTR_SERVICE: string = 'service';
  CSTR_BOOT: string = 'BOOT?';
  C_TIMEOUT:cardinal = 30000;
  CSTR_POWER_ONOFF: string = 'Please reset the unit using power off/on...';
var ch: char; sRecv, sSend, sTemp, sReset: string;
    bWait, bApp, bService, bReset, bResetSW: boolean;
    cTimeout, tStart, tEnd: cardinal;
    eBoot: EBootState;
begin
  if not t_ser.Active then  t_ser.Active := true;
  if t_ser.Active then begin
    cTimeout := GetTickCount() + C_TIMEOUT;
    if (t_ser.ReadString(sRecv) > 0) then memRecv.Lines.Add(sRecv);;

    //check boot mode
    eBoot := BootState();
    bReset := true; bResetSW := true;
    case eBoot of
      BS_UNKNOWN: begin
        bResetSW := false;
        memRecv.Lines.Add(CSTR_POWER_ONOFF);
      end;
      BS_ORIGIN: begin
        bReset := false;
        memRecv.Lines.Add('The soft reset is not supported on this unit.');
      end;
      BS_MTXSRV: begin
        if (self.cmbTo.ItemIndex = 1) then bReset := false;
        sReset := CSTR_RUN + Char(13);
      end;
      BS_MTXAPP: begin
        if (self.cmbTo.ItemIndex = 0) then bReset := false;
        sReset := CSTR_RESET + Char(13)
      end;
    end;

    if bReset then begin
      //send reset command
      if bResetSW then begin
        tStart := GetTickCount();
        t_ser.WriteString(sReset);
        while ((t_ser.TxWaiting > 0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
        tEnd := GetTickCount();
        memRecv.Lines.Add(format(CSTR_FORMAT_TO,[(tEnd - tStart)/1000.0]));
        memRecv.Lines.Add(sReset);
      end;

      //wait for the anwser from the unit
      tStart := GetTickCount(); sRecv := ''; bWait := false; bApp := false; bService := false;
      while ((t_ser.RxWaiting <= 0) and (GetTickCount() < cTimeout)) do begin
        if not bResetSW then begin
          tEnd := Round((cTimeout - GetTickCount()) / 1000);
          sTemp := format('%s %ds', [CSTR_POWER_ONOFF, tEnd]);
          memRecv.Lines[memRecv.Lines.count - 1] := sTemp;
        end;
        Delay(C_DELAY_MSEC);
      end;
      {repeat
        ch := chr(0);
        if (t_ser.ReadChar(ch) = 1) then  if (not (ch = char(0))) then sRecv := sRecv + ch;
        sTemp := UpperCase(sRecv);
        if (Pos(CSTR_WAITING, sTemp) > 0) then begin
          Delay(C_DELAY_MSEC);
          bWait := true;
          break;
        end;
        if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
      until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= cTimeout));}
      bWait := ExpectStr(CSTR_WAITING, sRecv, cTimeout - GetTickCount());
      if (t_ser.ReadString(sTemp) > 0) then sRecv := sRecv + Stemp;
      tEnd := GetTickCount();
      memRecv.Lines.Add(format(CSTR_FORMAT_FROM,[(tEnd - tStart)/1000.0]));
      memRecv.Lines.Add(sRecv);

      if (self.cmbTo.ItemIndex = 1) then begin //service mode
        Delay(100);
        if (t_ser.ReadString(sTemp) > 0) then memRecv.Lines.Add(sTemp);

        tStart := GetTickCount(); sRecv := '';
        sSend := CSTR_SERVICE + Char(13);
        t_ser.WriteString(sSend);
        while ((t_ser.TxWaiting > 0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
        tEnd := GetTickCount();
        memRecv.Lines.Add(format(CSTR_FORMAT_TO,[(tEnd - tStart)/1000.0]));
        memRecv.Lines.Add(sSend);

        tStart := GetTickCount(); sRecv := '';
        while ((t_ser.RxWaiting <=0) and (GetTickCount() < cTimeout)) do Delay(C_DELAY_MSEC);
        repeat
          ch := chr(0);
          if (t_ser.ReadChar(ch) = 1) then if (not (ch = char(0))) then sRecv := sRecv + ch;
          if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
        until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= cTimeout));
        tEnd := GetTickCount();
        memRecv.Lines.Add(format(CSTR_FORMAT_FROM,[(tEnd - tStart)/1000.0]));
        memRecv.Lines.Add(sRecv);
      end else begin//application mode
        sTemp := UpperCase(sRecv);
        if (Pos(CSTR_CHECKSUM, sTemp) > 0) then Delay(1000);
      end;

      if (t_ser.ReadString(sTemp) > 0) then memRecv.Lines.Add(sTemp);
    end;
  end;
end;

procedure TFrmBootTester.btnStartClick(Sender: TObject);
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

procedure TFrmBootTester.chbRecvClick(Sender: TObject);
begin
  chbVerify.Enabled := chbRecv.Checked;
  txtVerify.Enabled := chbVerify.Checked;
end;

procedure TFrmBootTester.chbVerifyClick(Sender: TObject);
begin
  txtVerify.Enabled := chbVerify.Checked;
  if chbVerify.Checked then txtVerify.SetFocus;
end;

procedure TFrmBootTester.cmbBaudrateChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Baudrate := StrToInt(cmbBaudrate.Text);
  t_ser.Active := b_active;
end;

procedure TFrmBootTester.cmbPortChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Port := StrToInt(cmbPort.Text);
  t_ser.Active := b_active;
end;

procedure TFrmBootTester.FormCreate(Sender: TObject);
begin
  t_ser :=TSerial.Create(self);
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
  //t_ser.Name := 'Multimeter';
  t_ser.Baudrate := StrToInt(cmbBaudrate.Items[cmbBaudrate.ItemIndex]);
  t_ser.Port := StrToInt(cmbPort.Items[cmbPort.ItemIndex]);
  c_timeout := 5000;
end;

procedure TFrmBootTester.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_ser);
end;

procedure TFrmBootTester.lstSendingDblClick(Sender: TObject);
begin
  txtSend.Text := lstSending.Items[lstSending.ItemIndex]
end;

function TFrmBootTester.SendStr(const str: string): integer;
var ch: char; timeout: cardinal; sRecv: string;
begin
  timeout := GetTickCount() + c_timeout;
  if (t_ser.ReadString(sRecv) > 0) then memRecv.Lines.Add(sRecv);;

  t_ser.WriteString(str);
  repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
  until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= timeout));
  result := length(str);
  {result := t_comport.WriteStr(str);}
end;

function TFrmBootTester.RecvStr(var str:string): integer;
var ch: char; timeout: cardinal;
begin
  timeout := GetTickCount() + c_timeout;
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
