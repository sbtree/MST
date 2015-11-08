unit MtxDownloader;

interface
uses Serial3, Classes;
type
  EBootState = (
                BS_UNKNOWN,
                BS_MTLBL_ONLY,  //only Motorola S-Record Loader exists on the board (virginal board)
                BS_MTLBL_UPD,   //Motorola S-Record Loader + Metronix BL_Updater
                BS_MTLBL_APP,   //Motorola S-Record Loader + Metronix FW
                BS_MTXBL_ONLY,  //only Metronix Boot Loader exists on the board
                BS_MTXBL_UPD,   //Metronix Boot Loader + Metronix BL_Updater
                BS_MTXBL_APP,   //Metronix Boot Loader + Metronix FW
                BS_XBL_UPD      //one Boot Loader (of s-record loader and metronix boot loader) is just updated through BL-Updater
                );
  EDownloadProtocol = (
                DP_MTL, //download protocol of Motorola S-Record loader
                DP_MTX  //download protocol of metronix boot loader
                );

  TDownloader = class
  protected
    e_bootstate: EBootState; //to save actural boot state
    e_dlprotocol: EDownloadProtocol; //to save protocol
    s_blmessage, s_fwmessage: string; //to save switch-on message of boot loader and firmware
    s_lastmsg: string; //to save information in the last action
  protected
    procedure UpdateSwitchOnMessage(const tend: cardinal); virtual;
    procedure UpdateBootState(); virtual;
    function  SwitchOn(const cmd: string; const tend: cardinal): boolean; virtual;
    function  EnterService(): boolean; virtual;
  public
    function Download(const fname: string): boolean; virtual;
  end;

  TComDownloader = class(TDownloader)
  protected
    t_ser: TSerial;
  protected
    function  SendStr(const str: string; const bprint: boolean = true): boolean;
    function  RecvStr(var str:string; const bwait: boolean = true; const bprint: boolean = true): integer;
    function  RecvStrTimeout(var str:string; const tend: cardinal): integer;
    function  RecvStrInterval(var str:string; const tend: cardinal; const interv: cardinal = 3000): integer;
    function  RecvStrExpected(var str:string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
    function  WaitForReading(const tend: cardinal): boolean;

    procedure UpdateSwitchOnMessage(const tend: cardinal); override;
    procedure UpdateBootState(); override;
    function  SwitchOn(const cmd: string; const tend: cardinal): boolean; override;
    function  EnterService(): boolean; override;
  public
    function Download(const fname: string): boolean; override;
  end;

implementation
uses Windows;

const
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
  CSTR_B115200: string = 'B115200';
  CINT_B115200: integer = 115200;
  C_REBOOT_TIME: cardinal = 10000;


function TComDownloader.EnterService(): boolean;
var s_recv, s_temp: string; c_endtime, c_start, c_end: cardinal; t_exstrs: TStringList; i_trials: integer;
begin
  result := false;
  t_exstrs := TStringList.Create;
  c_start := GetTickCount();
  if assigned(t_ser) then begin
    if t_ser.Active then begin
      case e_bootstate of
        BS_UNKNOWN: s_lastmsg := 'prog: failed to enter servec mode due to an unknown boot loader on this unit';
        BS_MTLBL_ONLY:begin
          t_ser.Baudrate := CINT_B115200;
          t_ser.FlowMode := fcXON_XOF;
          result := t_ser.Active;
        end;
        BS_MTLBL_UPD, BS_MTLBL_APP: begin
          t_ser.Baudrate := CINT_B115200;
          t_ser.FlowMode := fcXON_XOF;
          c_endtime := GetTickCount() + C_REBOOT_TIME;
          if SwitchOn('', c_endtime) then begin
            t_exstrs.Clear; t_exstrs.Add(CSTR_MOTOROLA);
            result := (RecvStrExpected(t_exstrs, c_endtime) >= 0);
          end;
        end;
        BS_MTXBL_ONLY: begin
          c_endtime := GetTickCount() + C_REBOOT_TIME;
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
                s_lastmsg := 'prog:<' + s_recv;
                t_ser.Baudrate := CINT_B115200;
              end;
            end;
          end;
        end;
        BS_MTXBL_UPD, BS_MTXBL_APP: begin
          c_endtime := GetTickCount() + C_REBOOT_TIME;
          t_exstrs.Clear; t_exstrs.Add(CSTR_WAITING);
          if SwitchOn('', c_endtime) then begin
            if (RecvStrExpected(t_exstrs, c_endtime) >= 0) then begin
              SendStr(CSTR_SERVICE + Char(13));
              Delay(C_DELAY_MSEC); //wait till the service mode is reached
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
                      //Delay(C_DELAY_MSEC);
                    end;
                  end;
                end else Delay(C_DELAY_MSEC);
                Inc(i_trials);
              until (result or (i_trials > 5));
            end;
          end;
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
                t_ser.FlowMode := fcXON_XOF;
                //Delay(C_DELAY_MSEC);
                result := t_ser.Active;
                break;
              end else begin
                s_temp := UpperCase(s_recv);
                if (Pos(CSTR_WAITING, s_temp) > 0) then begin
                  SendStr(CSTR_SERVICE + Char(13));
                  Delay(C_DELAY_MSEC); //wait till the service mode is reached
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
                          //Delay(C_DELAY_MSEC);
                        end;
                      end;
                    end else Delay(C_DELAY_MSEC);
                    Inc(i_trials);
                  until (result or (i_trials > 5));
                  break;
                end;
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

end.
