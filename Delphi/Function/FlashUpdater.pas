unit FlashUpdater;

interface
uses RS232;

type
  EUpdaterChannel = (
                DC_RS232,    //download s-record directly over rs232 connection
                DC_USB,      //download s-record directly over usb connection
                DC_CAN,      //download s-record directly over can-bus connection
                DC_FOJ,      //flash over jtag, a software tool (adapter: parallel port -> jtag)
                DC_FDT,      //flash development toolkit, a software tool of Renesas for flashing (rs232 in boot mode)
                DC_FCW       //Freescale CodeWarrior flash, a software tool for flashing (adapter: usb->jtag)
                );

  IFlashUpdater = interface
    function ValidVersion(const query, vali: string): boolean;
    function EnterService(const cmd: string): boolean;
    function UpdateFlash(): boolean;
  End;

  TFlashUpdater = class(TInterfacedObject, IFlashUpdater)
  protected
    s_fwfile:   string;
    s_blfile:   string;
    s_reset:    string;
    s_verquery: string;
    s_vervalid: string;
    c_timeout:  cardinal;
  public
    //constructor and destructor
    constructor Create();
    destructor Destroy; override;

    //properties
    property FirmwareFile: string read s_fwfile write s_fwfile;
    property BootloaderFile: string read s_blfile write s_blfile;
    property ResetCommand: string read s_reset write s_reset;
    property Timeout: cardinal read c_timeout write c_timeout;

    //abstract methods
    function ValidVersion(const ask, vali: string): boolean; virtual; abstract;
    function EnterService(const cmd: string): boolean; virtual; abstract;
    function UpdateFlash(): boolean; virtual; abstract;
  end;

  TFlashUpdaterRS232 = class(TFlashUpdater)
  protected
    t_rs232: TMtxRS232;
  public
    //constructor and destructor
    constructor Create();
    destructor Destroy; override;

    //override methods
    function ValidVersion(const ask, vali: string): boolean; override;
    function EnterService(const cmd: string): boolean; override;
    function UpdateFlash(): boolean; override;
  end;

  TFlashUpdaterUSB = class(TFlashUpdater)

  end;

  TFlashUpdaterCAN = class(TFlashUpdater)

  end;

implementation
constructor TFlashUpdater.Create();
begin
  inherited Create();
  //todo:
end;

destructor TFlashUpdater.Destroy;
begin
  //todo:
  inherited Destroy();
end;

constructor TFlashUpdaterRS232.Create();
begin
  inherited Create();
  //todo:
end;

destructor TFlashUpdaterRS232.Destroy;
begin
  //todo:
  inherited Destroy();
end;

function TFlashUpdaterRS232.ValidVersion(const ask, vali: string): boolean;
begin
  result := false;
  //todo:
end;

function TFlashUpdaterRS232.EnterService(const cmd: string): boolean;
begin
  result := false;
  //todo:
end;

function TFlashUpdaterRS232.UpdateFlash(): boolean; override;
begin
  result := false;
  //todo:
end;

end.
