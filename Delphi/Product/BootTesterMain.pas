unit BootTesterMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Serial3,ExtCtrls, {CPort, CPortCtl,} RegExpr, ComCtrls, MtxDownloader,
  NewProgressbar;

type
{
  EBootState = (
                BS_UNKNOWN,
                BS_MTLBL_ONLY, //only Motorola S-Record Loader exists on the board (virginal board)
                BS_MTLBL_UPD,  //Motorola S-Record Loader + Metronix BL_Updater
                BS_MTLBL_APP,  //Motorola S-Record Loader + Metronix FW
                BS_MTXBL_ONLY, //only Metronix Boot Loader exists on the board
                BS_MTXBL_UPD,  //Metronix Boot Loader + Metronix BL_Updater
                BS_MTXBL_APP,  //Metronix Boot Loader + Metronix FW
                BS_XBL_UPD //one Boot Loader (of s-record loader and metronix boot loader) is just updated through BL-Updater
                );

  EDownloadProtocol = (
                DP_MTL, //download protocol of Motorola S-Record loader
                DP_MTX //download protocol of metronix DIS-2 boot loader
                );
  }
  TFrmBootTester = class(TForm)
    memRecv: TMemo;
    btnClear: TButton;
    btnExport: TButton;
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
    btnStateQue: TButton;
    btnDownload: TButton;
    txtBootCmd: TEdit;
    lblBootCmd: TLabel;
    cmbPortProd: TComboBox;
    btnCloseTest: TButton;
    chkXonXoffTest: TCheckBox;
    cmbBaudTest: TComboBox;
    lblBaudTest: TLabel;
    cmbPortTest: TComboBox;
    lblPortTest: TLabel;
    chkXonXoffProd: TCheckBox;
    cmbBaudProd: TComboBox;
    lblPortProd: TLabel;
    lblBaudProd: TLabel;
    btnCloseProd: TButton;
    pgbSendFile: tCustomProgressbar;
    chkBaudFactor: TCheckBox;
    txtBaudFactor: TEdit;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    //procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure cmbPortTestChange(Sender: TObject);
    procedure cmbBaudTestChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure lstSendingDblClick(Sender: TObject);
    procedure chbRecvClick(Sender: TObject);
    procedure chbVerifyClick(Sender: TObject);
    procedure txtVerifyEnter(Sender: TObject);
    procedure btnCloseTestClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure chkXonXoffTestClick(Sender: TObject);
    procedure btnStateQueClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure cmbPortProdChange(Sender: TObject);
    procedure cmbBaudProdChange(Sender: TObject);
    procedure chkXonXoffProdClick(Sender: TObject);
    procedure btnCloseProdClick(Sender: TObject);
    procedure chkBaudFactorClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
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
    function EnterService(const cmd: string): boolean;
    function EnterServiceMode(const bs: EBootState): boolean;
    function ExpectStr(const str: string; var sRecv: string; const msecs: cardinal): boolean;

  private
    { Private-Deklarationen }
    t_ser, t_ctrl: TSerial;
    c_timeout: cardinal;
    e_bootstate: EBootState;
    e_dlprotocol: EDownloadProtocol;
    t_downloader: TDownloader;
  public
    { Public-Deklarationen }
  end;

var
  FrmBootTester: TFrmBootTester;

implementation

{$R *.dfm}
uses StrUtils, TypInfo, GenUtils;

const
  C_DELAY_MSEC: Cardinal = 50;
  CSTR_FORMAT_TO : string = '%0.3f--------- ---------- ---------- ---------->';
  CSTR_FORMAT_FROM : string = '%0.3f<--------- ---------- ---------- ----------';
  CSTR_POWER_ONOFF: string = 'Please reset the unit using power off/on...';

  CSTR_BOOTQUE: string = 'BOOT?';
  CSTR_SERVICE: string = 'service';
  CSTR_APPLICATION: string = 'APPLICATION';
  CSTR_ERROR: string = 'ERROR!';
  CSTR_UNKNOWNCMD: string = 'UNKNOWN COMMAND';
  CSTR_CHECKSUM: string = 'CHECKSUM';
  CSTR_DONE: string = 'DONE.';
  CSTR_WAITING: string = 'WAITING...';
  CSTR_MOTOROLA: string = 'MOTOROLA INC. S-RECORD LOADER';
  CSTR_METRONIX: string = 'BOOTLOADER (C) METRONIX';
  CSTR_BLUPDATER: string = 'BOOTLOADER UPDATER';
  CSTR_BOOTCODE: string = 'BOOTCODE';

  CSTR_B115200: string = 'B115200';
  CINT_B115200: integer = 115200;
  C_REBOOT_TIME: cardinal = 10000;  //10 seconds for reboot
  C_MANUAL_RESTART: cardinal = 30000;  

{procedure Delay(const msec: cardinal);
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
  memRecv.Lines.Add(format('prog[%0.3f]:<%s',[i_timeout/1000, Str]));
  i_timeout := GetTickCount();
end;}

procedure TFrmBootTester.btnClearClick(Sender: TObject);
begin
  memRecv.Lines.Clear();
end;

procedure TFrmBootTester.btnCloseProdClick(Sender: TObject);
begin
  t_ctrl.Active := false;
end;

procedure TFrmBootTester.btnCloseTestClick(Sender: TObject);
begin
  t_ser.Active := false;
end;

procedure TFrmBootTester.btnDownloadClick(Sender: TObject);
const C_DOWNLOAD_INTERVAL: cardinal = 6000;
var s_file: string; b_ok: boolean; r_factor: single;
begin
  r_factor := 1.0;
  if chkBaudFactor.Checked then begin
    if (not TryStrToFloat(trim(txtBaudFactor.Text), r_factor)) then r_factor := 1.0;
  end;
  t_downloader.BaudrateFactor := r_factor;
  s_file := trim(txtFile.Text);
  t_downloader.Download(trim(txtBootCmd.Text), s_file);
end;

procedure TFrmBootTester.Transmit();
var s_buffer: string; i: integer; t_cmds:TStringList;
    t_regex: TRegExpr; s_tmp: string;
begin
  s_buffer := trim(txtSend.Text);
  s_buffer := StringReplace(s_buffer, '<CR>', Char(13),[rfReplaceAll, rfIgnoreCase]) + Char(13);
  t_cmds := TStringList.create;
  ExtractStrings([Char(13)], [' '], PChar(s_buffer), t_cmds);
  for i := 0 to t_cmds.Count - 1 do begin
    if SameText(t_cmds[i], 'Wait') then TGenUtils.Delay(1000)
    else SendStr(t_cmds[i] + Char(13));
  end;

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
var c_time: cardinal;  b_timeout: boolean; s_recv: string;
begin
  result := false;
  if t_ser.Active then begin
    //clear buffers of the serial interface
    if (t_ser.ReadString(s_recv) > 0) then self.memRecv.Lines.Add('prog:<' + s_recv);
    if ((cmd = '-1') or (cmd = '')) then begin //manually reset
      c_time := GetTickCount() + C_MANUAL_RESTART; //set timeout
      //wait for the anwser from the unit
      memRecv.Lines.Add(format('prog: %s %ds', [CSTR_POWER_ONOFF, Round((c_time - GetTickCount()) / 1000)]));
      repeat
        memRecv.Lines[memRecv.Lines.count - 1] := format('prog: %s %ds', [CSTR_POWER_ONOFF, Round((c_time - GetTickCount()) / 1000)]);
        TGenUtils.Delay(C_DELAY_MSEC);
        b_timeout := (GetTickCount() >= c_time);
      until (b_timeout or (t_ser.RxWaiting > 0));
    end else begin
      //todo: reset by relay (automatically hard reset)
      //todo: reset through command (soft rest)
      // wait for that the first char arrives in C_RESET_TIMEOUT milliseconds after resetting
      WaitForReading(tend);
    end;
    result := (t_ser.RxWaiting > 0);
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
    //TGenUtils.Delay(C_DELAY_MSEC);
    SendStr(CSTR_TEST_CMD + Char(13));
    c_endtime := GetTickCount() + 1000;
    result := RecvStrInterval(msg, c_endtime, 100);
    t_ser.Baudrate := i_baud;
    //TGenUtils.Delay(C_DELAY_MSEC);
  end;
end;

function TFrmBootTester.GetSwitchOnMessage(var blmsg, fwmsg:string; const elapse: cardinal): integer;
const C_BL_FW_INTERVAL: cardinal = 6000; C_ANSWER_INTERVAL: cardinal = 100;
var c_endtime: cardinal; b_reset: boolean;
begin
  result := 0; blmsg := ''; fwmsg := '';
  if t_ser.Active then begin
    c_endtime := GetTickCount() + elapse;
    //restart the unit manually
    b_reset := SwitchOn('', c_endtime);
    //receive the message
    if b_reset then begin
      result := RecvStrInterval(blmsg, c_endtime, C_ANSWER_INTERVAL);
      if HasInvalidAscii(blmsg) then result := GetBootMessageMTL(blmsg);
      c_endtime := GetTickCount() + C_BL_FW_INTERVAL;
      if WaitForReading(c_endtime) then result := result + RecvStrInterval(fwmsg, c_endtime, 1500);
    end;
  end;
end;

function TFrmBootTester.EnterService(const cmd: string): boolean;
var s_recv, s_temp: string; c_endtime, c_start, c_end: cardinal; t_exstrs: TStringList; i_trials: integer;
begin
  result := false;
  c_start := GetTickCount();
  c_endtime := c_start + C_MANUAL_RESTART;
  t_exstrs := TStringList.Create;
  t_exstrs.Add(CSTR_WAITING);
  s_recv := ''; i_trials := 0;
  if SwitchOn(cmd, c_endtime) then begin
    c_endtime := GetTickCount() + C_REBOOT_TIME;
    while (GetTickCount() < c_endtime) do begin
      s_temp := ''; RecvStrInterval(s_temp, c_endtime, 100);
      s_recv := s_recv + s_temp;
      if HasInvalidAscii(s_recv) then begin //s-record loader, baudrate=115200
        t_ser.Baudrate := CINT_B115200;
        repeat
          SendStr(CSTR_BOOTQUE + Char(13));
          WaitForReading(c_endtime);
          s_recv := ''; RecvStrInterval(s_recv, c_endtime, 100);
          s_temp := UpperCase(trim(s_recv));
          result := (Pos(CSTR_MOTOROLA, s_temp) > 0);
          if result then begin
            t_ser.FlowMode := fcXON_XOF;
            e_dlprotocol := DP_MOTOROLA;
          end;
        until (result or (i_trials > 5));
        break;
      end else begin
        s_temp := UpperCase(s_recv);
        if (Pos(CSTR_WAITING, s_temp) > 0) then begin
          SendStr(CSTR_SERVICE + Char(13));
          TGenUtils.Delay(C_DELAY_MSEC); //wait till the service mode is reached
          t_exstrs.Clear; t_exstrs.Add(CSTR_SERVICE);
          i_trials := 0;
          repeat
            SendStr(CSTR_BOOTQUE + Char(13));
            result := (RecvStrExpected(t_exstrs, GetTickCount() + 1000) >= 0);
            if result then begin
              SendStr(CSTR_B115200 + Char(13));
              if WaitForReading(GetTickCount() + 1000) then begin
                if (t_ser.ReadString(s_recv) > 0) then begin
                  self.memRecv.Lines.Add('prog:<' + s_recv);
                  t_ser.Baudrate := CINT_B115200;
                end;
              end;
            end else TGenUtils.Delay(C_DELAY_MSEC);
            Inc(i_trials);
          until (result or (i_trials > 5));
          e_dlprotocol := DP_METRONIX1;
          break;
        end;
      end;
    end;
  end;
  c_end := GetTickCount();
  self.memRecv.Lines.Add(format('prog[%0.3f]: enter service [%s]; port=%d; baudrate=%d; flow control=%s',[(c_end - c_start)/1000.0, BoolToStr(result), t_ser.Port, t_ser.Baudrate, GetEnumName(TypeInfo(eFlowControl), Ord(t_ser.FlowMode))]));
end;

function TFrmBootTester.EnterServiceMode(const bs: EBootState): boolean;
var s_recv, s_temp: string; c_endtime, c_start, c_end: cardinal; t_exstrs: TStringList; i_trials: integer;
begin
  result := false;
  t_exstrs := TStringList.Create;
  c_start := GetTickCount();
  if t_ser.Active then begin
    e_dlprotocol := DP_MOTOROLA;
    case bs of
      BS_UNKNOWN: self.memRecv.Lines.Add('prog: found unknown boot loader on this unit');
      BS_MTLBL_ONLY:begin
        t_ser.Baudrate := CINT_B115200;
        t_ser.FlowMode := fcXON_XOF;
        //TGenUtils.Delay(C_DELAY_MSEC);
        result := t_ser.Active;
      end;
      BS_MTLBL_UPD, BS_MTLBL_APP: begin
        t_ser.Baudrate := CINT_B115200;
        t_ser.FlowMode := fcXON_XOF;
        //TGenUtils.Delay(C_DELAY_MSEC);
        c_endtime := GetTickCount() + C_REBOOT_TIME;
        if SwitchOn('', c_endtime) then begin
          t_exstrs.Clear; t_exstrs.Add(CSTR_MOTOROLA);
          result := (RecvStrExpected(t_exstrs, c_endtime) >= 0);
        end;
      end;
      BS_MTXBL_ONLY: begin
        c_endtime := GetTickCount() + C_MANUAL_RESTART;
        e_dlprotocol := DP_METRONIX1;
        repeat
          t_exstrs.Clear; t_exstrs.Add('>');
          SendStr(CSTR_SERVICE + Char(13));
          result := (RecvStrExpected(t_exstrs, GetTickCount() + 1000) >= 0);
          if (not result) then begin
            t_exstrs.Clear; t_exstrs.Add(CSTR_WAITING);
            RecvStrExpected(t_exstrs, c_endtime);
          end;
        until (result or (GetTickCount() >= c_endtime));
        if result then begin
          SendStr(CSTR_B115200 + Char(13));
          if WaitForReading(GetTickCount() + 1000) then begin
            if (t_ser.ReadString(s_recv) > 0) then begin
              self.memRecv.Lines.Add('prog:<' + s_recv);
              t_ser.Baudrate := CINT_B115200;
              //TGenUtils.Delay(C_DELAY_MSEC);
            end;
          end;
        end;
      end;
      BS_MTXBL_UPD, BS_MTXBL_APP: begin
        c_endtime := GetTickCount() + C_MANUAL_RESTART;
        t_exstrs.Clear; t_exstrs.Add(CSTR_WAITING);
        if SwitchOn('', c_endtime) then begin
          if (RecvStrExpected(t_exstrs, c_endtime) >= 0) then begin
            SendStr(CSTR_SERVICE + Char(13));
            TGenUtils.Delay(C_DELAY_MSEC); //wait till the service mode is reached
            t_exstrs.Clear; t_exstrs.Add(CSTR_SERVICE);
            i_trials := 0;
            repeat
              SendStr(CSTR_BOOTQUE + Char(13));
              result := (RecvStrExpected(t_exstrs, GetTickCount() + 1000) >= 0);
              if result then begin
                SendStr(CSTR_B115200 + Char(13));
                if WaitForReading(GetTickCount() + 1000) then begin
                  if (t_ser.ReadString(s_recv) > 0) then begin
                    self.memRecv.Lines.Add('prog:<' + s_recv);
                    t_ser.Baudrate := CINT_B115200;
                    //TGenUtils.Delay(C_DELAY_MSEC);
                  end;
                end;
              end else TGenUtils.Delay(C_DELAY_MSEC);
              Inc(i_trials);
            until (result or (i_trials > 5));
          end;
        end;
        e_dlprotocol := DP_METRONIX1;
      end;
      BS_XBL_UPD: begin
        c_endtime := GetTickCount() + C_MANUAL_RESTART;
        t_exstrs.Clear; t_exstrs.Add(CSTR_WAITING);
        s_recv := '';
        if SwitchOn('', c_endtime) then begin
          c_endtime := GetTickCount() + C_REBOOT_TIME;
          while (GetTickCount() < c_endtime) do begin
            s_temp := ''; RecvStrInterval(s_temp, c_endtime, 100);
            s_recv := s_recv + s_temp;
            if HasInvalidAscii(s_recv) then begin //s-record loader, baudrate=115200
              t_ser.Baudrate := CINT_B115200;
              i_trials := 0;
              repeat
                SendStr(CSTR_BOOTQUE + Char(13));
                WaitForReading(c_endtime);
                s_recv := ''; RecvStrInterval(s_recv, c_endtime, 100);
                s_temp := UpperCase(trim(s_recv));
                result := (Pos(CSTR_MOTOROLA, s_temp) > 0);
                if result then begin
                  t_ser.FlowMode := fcXON_XOF;
                  e_dlprotocol := DP_MOTOROLA;
                end;
                Inc(i_trials);
              until (result or (i_trials > 5));
              break;
            end else begin
              s_temp := UpperCase(s_recv);
              if (Pos(CSTR_WAITING, s_temp) > 0) then begin
                SendStr(CSTR_SERVICE + Char(13));
                TGenUtils.Delay(C_DELAY_MSEC); //wait till the service mode is reached
                t_exstrs.Clear; t_exstrs.Add(CSTR_SERVICE);
                i_trials := 0;
                repeat
                  SendStr(CSTR_BOOTQUE + Char(13));
                  result := (RecvStrExpected(t_exstrs, GetTickCount() + 1000) >= 0);
                  if result then begin
                    SendStr(CSTR_B115200 + Char(13));
                    if WaitForReading(GetTickCount() + 1000) then begin
                      if (t_ser.ReadString(s_recv) > 0) then begin
                        self.memRecv.Lines.Add('prog:<' + s_recv);
                        t_ser.Baudrate := CINT_B115200;
                        //TGenUtils.Delay(C_DELAY_MSEC);
                      end;
                    end;
                  end else TGenUtils.Delay(C_DELAY_MSEC);
                  Inc(i_trials);
                until (result or (i_trials > 5));
                e_dlprotocol := DP_METRONIX1;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  c_end := GetTickCount();
  self.memRecv.Lines.Add(format('prog[%0.3f]: enter service [%s]; port=%d; baudrate=%d; flow control=%s',[(c_end - c_start)/1000.0, BoolToStr(result), t_ser.Port, t_ser.Baudrate, GetEnumName(TypeInfo(eFlowControl), Ord(t_ser.FlowMode))]));
  t_exstrs.Clear;
  FreeAndNil(t_exstrs);
end;

function TFrmBootTester.ExpectStr(const str: string; var sRecv: string; const msecs: cardinal): boolean;
var cTimeout: cardinal; ch: char; sTemp, sIn: string;
begin
  result := false;
  if t_ser.Active then begin
    cTimeout := GetTickCount() + msecs;
    sIn := UpperCase(str);
    while ((t_ser.RxWaiting <= 0) and (GetTickCount() < cTimeout)) do TGenUtils.Delay(C_DELAY_MSEC);
    repeat
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then if (not (ch = char(0))) then sRecv := sRecv + ch;
      sTemp := UpperCase(sRecv);
      if (t_ser.RxWaiting <= 0) then TGenUtils.Delay(C_DELAY_MSEC);
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
var r_factor: single;
begin
  r_factor := 1.0;
  if chkBaudFactor.Checked then begin
    if (not TryStrToFloat(trim(txtBaudFactor.Text), r_factor)) then r_factor := 1.0;
  end;
  t_downloader.BaudrateFactor := r_factor;
  e_bootstate := t_downloader.GetBootState(trim(txtBootCmd.Text));
end;

procedure TFrmBootTester.btnTestClick(Sender: TObject);
var s_send, s_recv, s_tmp: string; b_app: boolean; c_time: cardinal;
begin
  if not t_ser.Active then  t_ser.Active := true;
  if t_ser.Active then
  begin
    s_send := 'INIT!' + Char(13);
    SendStr(s_send);
    b_app := false; s_recv := '';
    s_send := 'BOOT?' + Char(13);
    c_time := GetTickCount() + 20000;
    while (not b_app) do
    begin
      s_tmp := '';
      SendStr(s_send); TGenUtils.Delay(100);
      if (RecvStr(s_tmp) > 0) then s_recv := s_recv + trim(s_tmp);
      b_app := (EndsText('application', s_recv) or (GetTickCount() >= c_time));
    end;

    s_tmp := '';
    if (RecvStr(s_tmp) > 0) then s_recv := s_recv + trim(s_tmp);
    s_send := 'OW:0014:000004E2' + Char(13);
    SendStr(s_send);
    s_recv := '';
    RecvStr(s_recv);
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

procedure TFrmBootTester.chkBaudFactorClick(Sender: TObject);
begin
  txtBaudFactor.Enabled := chkBaudFactor.Checked;
end;

procedure TFrmBootTester.chkXonXoffProdClick(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ctrl.Active;
  t_ctrl.Active := False;
  if chkXonXoffProd.Checked then t_ctrl.FlowMode := fcXON_XOF
  else t_ctrl.FlowMode := fcNone;
  t_ctrl.Active := b_active;
end;

procedure TFrmBootTester.chkXonXoffTestClick(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  if chkXonXoffTest.Checked then t_ser.FlowMode := fcXON_XOF
  else t_ser.FlowMode := fcNone;
  t_ser.Active := b_active;
end;

procedure TFrmBootTester.cmbBaudProdChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ctrl.Active;
  t_ctrl.Active := False;
  t_ctrl.Baudrate := StrToInt(cmbBaudProd.Text);
  t_ctrl.Active := b_active;
end;

procedure TFrmBootTester.cmbBaudTestChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Baudrate := StrToInt(cmbBaudTest.Text);
  t_ser.Active := b_active;
end;

procedure TFrmBootTester.cmbPortProdChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ctrl.Active;
  t_ctrl.Active := False;
  t_ctrl.Port := StrToInt(cmbPortProd.Text);
  t_ctrl.Active := b_active;
end;

procedure TFrmBootTester.cmbPortTestChange(Sender: TObject);
var b_active: boolean;
begin
  b_active := t_ser.Active;
  t_ser.Active := False;
  t_ser.Port := StrToInt(cmbPortTest.Text);
  t_ser.Active := b_active;
end;

procedure TFrmBootTester.FormCreate(Sender: TObject);
begin
  t_ser :=TSerial.Create(self);
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
  t_ser.Port := StrToInt(cmbPortTest.Items[cmbPortTest.ItemIndex]);
  t_ser.Baudrate := StrToInt(cmbBaudTest.Items[cmbBaudTest.ItemIndex]);
  //t_ser.Name := 'Multimeter';

  t_ctrl := TSerial.Create(self);
  t_ctrl.CheckParity := false;
  t_ctrl.DataBits := d8Bit;
  t_ctrl.NotifyErrors := neNone;
  t_ctrl.Port := StrToInt(cmbPortProd.Items[cmbPortProd.ItemIndex]);
  t_ctrl.Baudrate := StrToInt(cmbBaudProd.Items[cmbBaudProd.ItemIndex]);

  c_timeout := 3000;
  e_bootstate := BS_UNKNOWN;
  e_dlprotocol := DP_MOTOROLA;
  t_comdownloader.ComObj := t_ctrl;//t_ser;
  t_downloader := t_comdownloader;
  t_downloader.ProgressBar := pgbSendFile;
  t_downloader.Messager := memRecv.Lines;
end;

procedure TFrmBootTester.FormDestroy(Sender: TObject);
begin
  FreeAndNil(t_ser);
  FreeAndNil(t_ctrl);
  FreeAndNil(t_downloader);
end;

procedure TFrmBootTester.lstSendingDblClick(Sender: TObject);
begin
  txtSend.Text := lstSending.Items[lstSending.ItemIndex]
end;

function TFrmBootTester.SendStr(const str: string; const bprint: boolean): boolean;
var timeout, c_start, c_end: cardinal; s_recv: string;
begin
  //result := false;
  if (t_ser.ReadString(s_recv) > 0) then self.memRecv.Lines.Add('prog:<' + s_recv);

  c_start := GetTickCount();
  timeout := c_start + c_timeout;
  t_ser.WriteString(str);
  while ((t_ser.TxWaiting > 0) and (GetTickCount() < timeout)) do TGenUtils.Delay(C_DELAY_MSEC);
  result := (t_ser.TxWaiting <= 0);
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
  if (bprint) then begin
    c_end := GetTickCount();
    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, IfThen(str='', '[empty]', str)]));
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
  WaitForReading(tend);
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
var s_recv, s_input, s_temp: string; b_found: boolean; i, i_len: integer; b_timeout: boolean; //c_start, c_end: cardinal;
begin
  result := -1;
  s_recv := '';  b_found := false;
//  c_start := GetTickCount();
  repeat
    s_temp := ''; //WaitForReading(tend);
    //i_len := t_ser.ReadString(s_temp);
    i_len := RecvStrInterval(s_temp, tend, 50);
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
//  if (s_recv <> '') then begin
//    c_end := GetTickCount();
//    self.memRecv.Lines.Add(format('prog[%0.3f]:<%s', [(c_end - c_start) /1000.0, s_recv]));
//  end;
end;

end.
