unit BootTesterMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Serial3,ExtCtrls, {CPort, CPortCtl,} RegExpr, ComCtrls;

type
  EBootState = (
                BS_UNKNOWN,
                BS_MTLBL_ONLY, //only Motorola S-Record Loader exists on the board (virginal board)
                BS_MTLBL_UPD,  //Motorola S-Record Loader + Metronix BL_Updater
                BS_MTLBL_APP,  //Motorola S-Record Loader + Metronix FW
                BS_MTXBL_ONLY, //only Metronix Boot Loader exists on the board
                BS_MTXBL_UPD,  //Metronix Boot Loader + Metronix BL_Updater
                BS_MTXBL_APP   //Metronix Boot Loader + Metronix FW
                );

  TFrmBootTester = class(TForm)
    memRecv: TMemo;
    btnClear: TButton;
    btnExport: TButton;
    grpRS232: TGroupBox;
    lblPort: TLabel;
    cmbPort: TComboBox;
    lblBaudrate: TLabel;
    cmbBaudrate: TComboBox;
    btnClose: TButton;
    chkXonXoff: TCheckBox;
    grpSendCommand: TGroupBox;
    lblLoop: TLabel;
    btnSend: TButton;
    txtSend: TEdit;
    chbRecv: TCheckBox;
    txtLoop: TEdit;
    lstSending: TListBox;
    chbVerify: TCheckBox;
    txtVerify: TEdit;
    grpService: TGroupBox;
    txtFile: TEdit;
    btnFile: TButton;
    pgbSendFile: TProgressBar;
    lblSendFile: TLabel;
    btnStateQue: TButton;
    btnDownload: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    //procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure cmbPortChange(Sender: TObject);
    procedure cmbBaudrateChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure lstSendingDblClick(Sender: TObject);
    procedure chbRecvClick(Sender: TObject);
    procedure chbVerifyClick(Sender: TObject);
    procedure txtVerifyEnter(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure chkXonXoffClick(Sender: TObject);
    procedure btnStateQueClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
  protected
    procedure Transmit();
    function SendStr(const str: string; const bprint: boolean = true): boolean;
    function RecvStr(var str:string; const bwait: boolean = true; const bprint: boolean = true): integer;
    function RecvStrTimeout(var str:string; const tend: cardinal): integer;
    function RecvStrInterval(var str:string; const tend: cardinal; const interv: cardinal = 3000): integer;
    function RecvStrExpected(const exstr: TStringList; tend: cardinal; const bcase: boolean = false): integer;
    function WaitForReading(const tend: cardinal): boolean;
    function SwitchOn(const cmd: string; const tend: cardinal): boolean;
    function HasInvalidAscii(const str: string): boolean;
    function GetBootMessageMTL(var msg: string): integer;
    function GetSwitchOnMessage(var blmsg, fwmsg:string; const elapse: cardinal): integer;
    function GetBootState(const blmsg, fwmsg: string): EBootState;
    function BootToService(const bs: EBootState; const initfw: string = ''): boolean;
    function ExpectStr(const str: string; var sRecv: string; const msecs: cardinal): boolean;

  private
    { Private-Deklarationen }
    t_ser: TSerial;
    c_timeout: cardinal;
    e_bootstate: EBootState;
  public
    { Public-Deklarationen }
  end;

var
  FrmBootTester: TFrmBootTester;

implementation

{$R *.dfm}
uses StrUtils, TypInfo;
const
  C_DELAY_MSEC: Cardinal = 50;
  CSTR_FORMAT_TO : string = '%0.3f--------- ---------- ---------- ---------->';
  CSTR_FORMAT_FROM : string = '%0.3f<--------- ---------- ---------- ----------';
  CSTR_POWER_ONOFF: string = 'Please reset the unit using power off/on...';

  CSTR_BOOTQUE: string = 'BOOT?';
  CSTR_SERVICE: string = 'SERVICE';
  CSTR_APPLICATION: string = 'APPLICATION';
  CSTR_ERROR: string = 'ERROR!';
  CSTR_UNKNOWNCMD: string = 'UNKNOWN COMMAND';
  CSTR_CHECKSUM: string = 'CHECKSUM';
  CSTR_DONE: string = 'DONE.';
  CSTR_WAITING: string = 'WAITING...';
  CSTR_MOTOROLA: string = 'MOTOROLA INC. S-RECORD LOADER';
  CSTR_B115200: string = 'B115200';
  CINT_B115200: integer = 115200;
  C_REBOOT_TIME: cardinal = 10000;  //10 seconds for reboot
  C_MANUAL_RESTART: cardinal = 20000;

  C_MTXSET: set of EBootState = [BS_MTXBL_ONLY, BS_MTXBL_UPD, BS_MTXBL_APP];

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
  memRecv.Lines.Add(format('prog[%0.3f]:<%s',[i_timeout/1000, Str]));
  i_timeout := GetTickCount();
end;}

procedure TFrmBootTester.btnClearClick(Sender: TObject);
begin
  memRecv.Lines.Clear();
end;

procedure TFrmBootTester.btnCloseClick(Sender: TObject);
begin
  t_ser.Active := false;
end;

procedure TFrmBootTester.btnDownloadClick(Sender: TObject);
const C_DOWNLOAD_INTERVAL: cardinal = 6000;
var s_file, s_line, s_recv: string; i, i_trial: integer; t_lines: TStringList; b_break, b_ok: boolean;
    i_baud: integer; e_flowctrl: eFlowControl; c_start, c_end: cardinal;
begin
  if not t_ser.Active then begin
    memRecv.Lines.Add(format('prog: serial interface is deaktived: port=%d; baud=%d', [t_ser.Port, t_ser.Baudrate]));
    exit;
  end;

  i_baud := t_ser.Baudrate;
  e_flowctrl := t_ser.FlowMode;
  s_file := trim(txtFile.Text);
  if FileExists(s_file) then begin
    if BootToService(e_bootstate) then begin
      lblSendFile.Caption := 'loading...';
      t_lines := TStringList.Create;
      t_lines.LoadFromFile(s_file);
      pgbSendFile.Max := t_lines.Count;
      pgbSendFile.Position := 0;
      lblSendFile.Caption := '0%';
      c_start := GetTickCount();
      if (e_bootstate in C_MTXSET) then begin
        for i := 0 to t_lines.Count - 1 do begin
          i_trial := 0; b_break := false; b_ok := false;
          s_line := t_lines[i] + Char(13);
          repeat
            //SendStr(s_line, false);
            t_ser.WriteString(s_line);
            s_recv := '';
            if WaitForReading(GetTickCount() + C_DOWNLOAD_INTERVAL) then begin
              t_ser.ReadString(s_recv);
              //RecvStr(s_recv, false, false);
              if SameText(s_recv, '*') then b_ok := true
              else if SameText(s_recv, '#') then begin //repeat sending the same line
                Inc(i_trial);
                b_break := (i_trial > 20);
              end else b_break := true; //'@' or other char
            end else b_break := true;  //received no data
          until (b_break or b_ok);

          pgbSendFile.Position := i + 1;
          lblSendFile.Caption := format('%d', [Round((pgbSendFile.Position / pgbSendFile.Max) * 100.0)]) + '%';
          if b_break then begin
            self.memRecv.Lines.Add(format('prog: failed to send file [trials(%d) of sending:%s; answer:%s;]', [i_trial, s_line, s_recv]));
            break;
          end;
        end;
      end else begin
        for i := 0 to t_lines.Count - 1 do begin
          t_ser.WriteString(t_lines[i] + Char(13));
          while t_ser.TxWaiting > 0 do Delay(C_DELAY_MSEC);
          pgbSendFile.Position := i + 1;
          lblSendFile.Caption := format('%d', [Round((pgbSendFile.Position / pgbSendFile.Max) * 100.0)]) + '%';
          Application.ProcessMessages();
        end;
      end;
      c_end := GetTickCount();
      self.memRecv.Lines.Add(format('prog[%0.3f]: downloaded %d lines of totle %d from file "%s"', [(c_end - c_start)/1000.0, pgbSendFile.Position, pgbSendFile.Max, s_file]));
      t_lines.Clear;
      FreeAndNil(t_lines);
      lblSendFile.Caption := 'over';
    end;
  end;
  t_ser.Baudrate := i_baud;
  t_ser.FlowMode := e_flowctrl;
  Delay(C_DELAY_MSEC);
end;

procedure TFrmBootTester.Transmit();
var s_buffer: string; i: integer; t_cmds:TStringList;
    t_regex: TRegExpr; s_tmp: string;
begin
  s_buffer := trim(txtSend.Text);
  s_buffer := StringReplace(s_buffer, '<CR>', Char(13),[rfReplaceAll, rfIgnoreCase]) + Char(13);
  t_cmds := TStringList.create;
  ExtractStrings([Char(13)], [], PChar(s_buffer), t_cmds);
  for i := 0 to t_cmds.Count - 1 do SendStr(t_cmds[i] + Char(13));

  {i_timeout := GetTickCount();}
  if chbRecv.Checked then
  begin
    s_buffer := '';
    RecvStrInterval(s_buffer, GetTickCount() + c_timeout, 100);
    if (chbVerify.Checked and (trim(txtVerify.Text)<>'')) then
    begin
      t_regex := TRegExpr.Create;
      t_regex.Expression := trim(txtVerify.Text);
      s_tmp := StringReplace(s_buffer, char(13), '', [rfReplaceAll, rfIgnoreCase]);
      if (t_regex.Exec(s_tmp)) then s_buffer := s_buffer + '='
      else s_buffer := s_buffer + '?';
      FreeAndNil(t_regex);
    end;
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
procedure TFrmBootTester.btnFileClick(Sender: TObject);
var t_fopen: TOpenDialog;
begin
  t_fopen := TOpenDialog.Create(self);
  t_fopen.Filter := 'S-Record Files (*.s, *.mot)|*.s;*.mot';
  if t_fopen.Execute then txtFile.Text := trim(t_fopen.FileName);
  FreeAndNil(t_fopen);
end;

function TFrmBootTester.WaitForReading(const tend: cardinal): boolean;
var b_timeout: boolean;
begin
  b_timeout := false;
  while ((t_ser.RxWaiting <= 0) and (not b_timeout)) do begin
    //Delay(C_DELAY_MSEC);
    Application.ProcessMessages();
    b_timeout := (GetTickCount() >= tend);
  end;
  result := (t_ser.RxWaiting > 0);
end;

function TFrmBootTester.SwitchOn(const cmd: string; const tend: cardinal): boolean;
var c_time: cardinal;  b_timeout: boolean;
begin
  result := false;
  if t_ser.Active then begin
    if ((cmd = '-1') or (cmd = '')) then begin //manually reset
      c_time := GetTickCount() + C_MANUAL_RESTART; //set timeout
      //wait for the anwser from the unit
      memRecv.Lines.Add(format('prog: %s %ds', [CSTR_POWER_ONOFF, Round((c_time - GetTickCount()) / 1000)]));
      repeat
        memRecv.Lines[memRecv.Lines.count - 1] := format('prog: %s %ds', [CSTR_POWER_ONOFF, Round((c_time - GetTickCount()) / 1000)]);
        Delay(C_DELAY_MSEC);
        b_timeout := (GetTickCount() >= c_time);
      until (b_timeout or (t_ser.RxWaiting > 0));
      result := ((not b_timeout) and (t_ser.RxWaiting > 0));
    end else begin
      //todo: reset by relay (automatically hard reset)
      //todo: reset through command (soft rest)
      // wait for that the first char arrives in C_RESET_TIMEOUT milliseconds after resetting
      result := WaitForReading(tend);
    end;
  end;
end;

function TFrmBootTester.HasInvalidAscii(const str: string): boolean;
var i: integer;
begin
  result := false;
  for i := 1 to Length(str) do begin
    if (char(str[i]) > Char($7F)) then begin
      result := true;
      break;
    end;
  end;
end;

function TFrmBootTester.GetBootMessageMTL(var msg: string): integer;
const CSTR_TEST_CMD: string= 'abcdefg';
var i_baud: integer; c_endtime: cardinal;
begin
  result := 0;
  if t_ser.Active then begin
    i_baud := t_ser.Baudrate;
    t_ser.Baudrate := CINT_B115200;
    Delay(C_DELAY_MSEC);
    SendStr(CSTR_TEST_CMD + Char(13));
    c_endtime := GetTickCount() + 1000;
    result := RecvStrInterval(msg, c_endtime, 100);
    t_ser.Baudrate := i_baud;
    Delay(C_DELAY_MSEC);
  end;
end;


function TFrmBootTester.GetSwitchOnMessage(var blmsg, fwmsg:string; const elapse: cardinal): integer;
const C_BL_FW_INTERVAL: cardinal = 5000; C_ANSWER_INTERVAL: cardinal = 100;
var c_endtime: cardinal; b_reset: boolean; s_recv: string;
begin
  result := 0; blmsg := ''; fwmsg := '';
  if t_ser.Active then begin
    c_endtime := GetTickCount() + elapse;
    //clear buffers of the serial interface
    RecvStr(s_recv, false);

    //restart the unit manually
    b_reset := SwitchOn('', c_endtime);
    //receive the message
    if b_reset then begin
      result := RecvStrInterval(blmsg, c_endtime, C_ANSWER_INTERVAL);
      if HasInvalidAscii(blmsg) then result := GetBootMessageMTL(blmsg);
      c_endtime := GetTickCount() + C_BL_FW_INTERVAL;
      if WaitForReading(c_endtime) then result := result + RecvStrInterval(fwmsg, c_endtime, 1000);
    end;
  end;
end;

function TFrmBootTester.GetBootState(const blmsg, fwmsg: string): EBootState;
const
  C_MTLBL = $00010000;
  C_MTXBL = $00020000;
  C_MTXUPD = $00000001;
  C_MTXAPP = $00000002;
var sSend, sRecv, sTemp, s_inblmsg, s_infwmsg: string; cTimeout: cardinal; bRepeat: boolean;
    t_lines: TStringList; lw_blfw: longword;
begin
  result := BS_UNKNOWN; lw_blfw := 0;
  s_inblmsg := UpperCase(trim(blmsg));
  s_infwmsg := UpperCase(trim(fwmsg));
  if (Pos(CSTR_MOTOROLA, s_inblmsg) > 0) then lw_blfw := (lw_blfw or C_MTLBL);
  if (Pos(CSTR_WAITING, s_inblmsg) > 0) then lw_blfw := (lw_blfw or C_MTXBL);

  if (fwmsg <> '') then begin
    t_lines := TStringList.Create;
    ExtractStrings([Char(10)], [Char(13)], PChar(s_infwmsg), t_lines);
    if ((Pos(CSTR_DONE, t_lines[t_lines.Count - 1]) > 0) or (Pos(CSTR_CHECKSUM, t_lines[t_lines.Count - 1]) > 0))  then lw_blfw := (lw_blfw or C_MTXUPD)
    else if t_ser.Active then begin
      repeat
        cTimeout := GetTickCount() + c_timeout;
        RecvStr(sTemp, false);

        sSend := CSTR_BOOTQUE + Char(13);
        SendStr(sSend);

        WaitForReading(cTimeout);
        sRecv := ''; RecvStrInterval(sRecv, cTimeout, 100);
        sTemp := UpperCase(trim(sRecv));

        bRepeat := false;
        if (Pos(CSTR_APPLICATION, sTemp) > 0) then lw_blfw := (lw_blfw or C_MTXAPP)
        else if (Pos(CSTR_SERVICE, sTemp) > 0) then lw_blfw := (lw_blfw or C_MTXBL)
        else if ((Pos(CSTR_ERROR, sTemp) > 0) or (Pos(CSTR_UNKNOWNCMD, sTemp) > 0)) then bRepeat := true;
      until ((not bRepeat) or (GetTickCount() >= cTimeout));
    end;
    t_lines.Clear;
    FreeAndNil(t_lines);
  end;
  case lw_blfw of
    $00010000: result := BS_MTLBL_ONLY;
    $00010001: result := BS_MTLBL_UPD;
    $00010002: result := BS_MTLBL_APP;
    $00020000: result := BS_MTXBL_ONLY;
    $00020001: result := BS_MTXBL_UPD;
    $00020002: result := BS_MTXBL_APP;
  end;
end;

function TFrmBootTester.BootToService(const bs: EBootState; const initfw: string): boolean;
var s_recv: string; c_endtime, c_start, c_end: cardinal; t_exstrs: TStringList;
begin
  result := false;
  t_exstrs := TStringList.Create;
  c_start := GetTickCount();
  if t_ser.Active then begin
    case bs of
      BS_UNKNOWN: self.memRecv.Lines.Add('prog: found unknown boot loader on this unit');
      BS_MTLBL_ONLY:begin
        t_ser.Active := false;
        t_ser.Baudrate := 115200;
        t_ser.FlowMode := fcXON_XOF;
        t_ser.Active := true;
        result := t_ser.Active;
      end;
      BS_MTLBL_UPD, BS_MTLBL_APP: begin
        t_ser.Active := false;
        t_ser.Baudrate := 115200;
        t_ser.FlowMode := fcXON_XOF;
        t_ser.Active := true;
        Delay(C_DELAY_MSEC);
        c_endtime := GetTickCount() + C_REBOOT_TIME;
        if SwitchOn('', c_endtime) then begin
          t_exstrs.Clear;
          t_exstrs.Add(CSTR_MOTOROLA);
          result := (RecvStrExpected(t_exstrs, c_endtime) >= 0);
        end;
      end;
      BS_MTXBL_ONLY, BS_MTXBL_UPD, BS_MTXBL_APP: begin
        c_endtime := GetTickCount() + C_MANUAL_RESTART;
        t_exstrs := TStringList.Create;
        t_exstrs.Add(CSTR_WAITING);
        if SwitchOn('', c_endtime) then begin
          if (RecvStrExpected(t_exstrs, c_endtime) >= 0) then begin
            SendStr(CSTR_SERVICE + Char(13));
            //WaitForReading(c_endtime);
            t_exstrs.Clear;
            t_exstrs.Add(CSTR_SERVICE);
            c_endtime := GetTickCount() + 1000;
            SendStr(CSTR_BOOTQUE + Char(13));
            result := (RecvStrExpected(t_exstrs, c_endtime) >= 0);
            if result then begin
              SendStr(CSTR_B115200 + Char(13));
              c_endtime := GetTickCount() + 1000;
              if WaitForReading(c_endtime) then begin
                RecvStr(s_recv, false);
                t_ser.Baudrate := 115200;
              end;
            end;
          end;
        end;
        t_exstrs.Clear;
        FreeAndNil(t_exstrs);
      end;
    end;
  end;
  c_end := GetTickCount();
  self.memRecv.Lines.Add(format('prog[%0.3f]: boot to service [%s]; port=%d; baudrate=%d; flow control=%s',[(c_end - c_start)/1000.0, BoolToStr(result), t_ser.Port, t_ser.Baudrate, GetEnumName(TypeInfo(eFlowControl), Ord(t_ser.FlowMode))]));
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

procedure TFrmBootTester.btnSendClick(Sender: TObject);
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

procedure TFrmBootTester.btnStateQueClick(Sender: TObject);
var s_blmsg, s_fwmsg: string; t_start, t_end: cardinal;
begin
  if not t_ser.Active then  t_ser.Active := true;
  if t_ser.Active then begin
    t_start := GetTickCount();
    GetSwitchOnMessage(s_blmsg, s_fwmsg, C_MANUAL_RESTART);
    e_bootstate := GetBootState(s_blmsg, s_fwmsg);
    t_end := GetTickCount();
    memRecv.Lines.Add(format('prog[%0.3f]: boot state is %s',[(t_end - t_start)/1000.0, GetEnumName(TypeInfo(EBootState), Ord(e_bootstate))]));
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



procedure TFrmBootTester.chkXonXoffClick(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  if chkXonXoff.Checked then t_ser.FlowMode := fcXON_XOF
  else t_ser.FlowMode := fcNone;
  t_ser.Active := b_active;
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
  c_timeout := 3000;
  e_bootstate := BS_UNKNOWN;
end;

procedure TFrmBootTester.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_ser);
end;

procedure TFrmBootTester.lstSendingDblClick(Sender: TObject);
begin
  txtSend.Text := lstSending.Items[lstSending.ItemIndex]
end;

function TFrmBootTester.SendStr(const str: string; const bprint: boolean): boolean;
var timeout, c_start, c_end: cardinal; s_recv: string;
begin
  result := true; RecvStr(s_recv, false);

  c_start := GetTickCount();
  //timeout := c_start + c_timeout;
  t_ser.WriteString(str);
  //while ((t_ser.TxWaiting > 0) and (GetTickCount() < timeout)) do Delay(C_DELAY_MSEC);
  //result := length(str);
  {result := t_comport.WriteStr(str);}
  c_end := GetTickCount();
  if bprint then self.memRecv.Lines.Add(format('prog[%0.3f]:>%s', [(c_end - c_start) /1000.0, str]));
end;

function TFrmBootTester.RecvStr(var str:string; const bwait: boolean; const bprint: boolean): integer;
var c_time, c_start, c_end: cardinal;
begin
  str := ''; c_start := GetTickCount();
  c_time := c_start + c_timeout;
  if bwait then WaitForReading(c_time);
  result := t_ser.ReadString(str);
  if ((str<>'') and bprint) then begin
    c_end := GetTickCount();
    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, str]));
  end;
end;

function TFrmBootTester.RecvStrTimeout(var str:string; const tend: cardinal): integer;
var b_timeout: boolean; s_recv: string; i_len: integer; c_start, c_end: cardinal;
begin
  result := 0; str := '';
  c_start := GetTickCount();
  repeat
    s_recv := ''; WaitForReading(tend);
    i_len := t_ser.ReadString(s_recv);
    if (i_len > 0) then begin
      str := str + s_recv;
      result := result + i_len;
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout);
  if str<>'' then begin
    c_end := GetTickCount();
    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, str]));
  end;
end;

function TFrmBootTester.RecvStrInterval(var str:string; const tend: cardinal; const interv: cardinal): integer;
var b_break, b_timeout: boolean; s_recv: string; i_len: integer; c_endtime, c_start, c_end: cardinal;
begin
  result := 0; str := '';
  c_start := GetTickCount();
  repeat
    s_recv := '';
    c_endtime := GetTickCount() + interv;
    if (tend < c_endtime) then c_endtime := tend;
    b_break := (not WaitForReading(c_endtime));
    i_len := t_ser.ReadString(s_recv);
    if ( i_len> 0) then begin
      str := str + s_recv;
      result := result + i_len;
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_break);
  if str<>'' then begin
    c_end := GetTickCount();
    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, str]));
  end;
end;

function TFrmBootTester.RecvStrExpected(const exstr: TStringList; tend: cardinal; const bcase: boolean): integer;
var s_recv, s_input, s_temp: string; b_found: boolean; i, i_len: integer; b_timeout: boolean; c_start, c_end: cardinal;
begin
  result := -1;
  s_recv := '';  b_found := false;
  c_start := GetTickCount();
  repeat
    s_temp := ''; WaitForReading(tend);
    i_len := t_ser.ReadString(s_temp);
    if (i_len > 0) then begin
      if bcase then s_recv := s_recv + s_temp
      else  s_recv := s_recv + UpperCase(s_temp);
      for i := 0 to exstr.Count - 1 do begin
        if bcase then s_input := exstr[i]
        else s_input := UpperCase(exstr[i]);
        b_found := (Pos(s_input, s_recv) > 0);
        if b_found then begin
          result := i;
          break;
        end;
      end;
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_found);
  if (s_recv <> '') then begin
    c_end := GetTickCount();
    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, s_recv]));
  end;
end;

end.
