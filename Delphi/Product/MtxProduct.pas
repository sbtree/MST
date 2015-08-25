unit MtxProduct;

interface

type
  EShutOnState = (SS_UNKNOWN,   //no information from starting
                  SS_MTL_BL,   //started with Motorola boot loader
                  SS_MTX_UPD,   //started with Metronix boot loader updater
                  SS_MTX_SRV,   //started in mode of Metronix Service
                  SS_MTX_APP ); //started in mode of Metronix Application

  TProductBase = interface
    function reset: Integer;
    function getStatus: Integer;
    function updateBootloader: Integer;
    function updateFirmware: Integer;
  end;


implementation

end.
