unit MtxProduct;

interface

type
  EShutOnState = (SS_UNKNOWN,   //no information from starting
                  SS_MTL_BL,   //started with Motorola boot loader
                  SS_MTX_UPD,   //started with Metronix boot loader updater
                  SS_MTX_SRV,   //started in mode of Metronix Service
                  SS_MTX_APP ); //started in mode of Metronix Application

  TProductBase = class
  protected
    e_state: EShutOnState;
    s_message: string;
  public
    property ShutOnState: EShutOnState read e_state;
    property ShutOnMessage: string read s_message;

    function ResetHW(): boolean; virtual; abstract;
    function ResetSW(): boolean; virtual; abstract;
    function ResetTo(const ss: EShutOnState): boolean; virtual; abstract;

    function UpdateBootloader(): boolean; virtual; abstract;
    function UpdateFirmware(): boolean; virtual; abstract;
  end;


implementation

end.
