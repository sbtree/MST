unit FlashWriter;

interface

type
  EBootState = (BS_UNKNOWN, //unknown state, which cannot be recognized
                BS_MTLBL,   //state, in which Motorola S-Record bootloader is running
                BS_MTXBLARS,//state, in which Metronix ARS2000 bootloader is running
                BS_MTXBLDIS,//state, in which Metronix DIS2 bootloader is running
                BS_MTXBLUPD,//state, in which Metronix bootloader updater is running
                BS_MTXAPP   //state, in which Metronix application(firmware) is running
                );

  EFlashProtocol = (FP_MTLSREC, //flash protocol with Motorola s-record bootloader
                    FP_MTXRS232,//Metronix flash protocol over RS232
                    FP_MTXUSB,  //Metronix flash protocol over USB
                    FP_MTXETH,  //Metronix flash protocol over ethernet
                    FP_MTXCAN,  //Metronix flash protocol over CAN-Bus
                    FP_EXTTOOL  //flash protocol througth external tool, e.g. flash_over_jtag, fflash, fdt, codewarrior flash programmer
                    );

  ESRecordFile = (SR_GENERAL,    //general s-record file
                  SR_BL,    //s-record file of bootloader
                  SR_BLUPD, //s-record file of bootloader updater
                  SR_FW     //s-record file of firmware
                  );

  IFlashWriter = interface
    function EnterService(): boolean;
    function StartWriting(): boolean;
    function AddSRecord(const fname: string): boolean;
  end;

  TFlashWriter = class(TInterfacedObject, IFlashWriter)

  end;


implementation

end.
