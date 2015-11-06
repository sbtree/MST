unit MtxDownloader;

interface
uses Serial3;
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

  TDownloader = class
  protected
    s_blmessage, s_fwmessage: string; //to save switch-on message of boot loader and firmware
    e_bootstate: EBootState; //
  protected
    procedure UpdateSwitchOnMessage(const tend: cardinal); virtual;
    procedure UpdateBootState(const blmsg, fwmsg: string); virtual;
    function  SwitchOn(const cmd: string; const tend: cardinal): boolean; virtual;
    function  EnterService(): boolean; virtual;
  public
    function Download(const fname: string): boolean; virtual;
  end;

  TComDownloader = class(TDownloader)
  protected
    t_ser: TSerial;
    
  protected

  end;

implementation
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

end.
