unit ProductBase;

interface
uses DeviceBase;
type
  IProductBase = interface
    function Reset(const cmd: string): boolean;
    function EnterService(const cmd: string; const expe: string): boolean;
    function FlashBootloader(const sfile: string): boolean;
    function FlashFirmware(const sfile: string): boolean;
    function FlashAdditional(const sfile: string): boolean;
    function FlashComplete(const blfile, fwfile: string; const addfile: string = ''): boolean;
  end;

implementation

end.
