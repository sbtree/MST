unit CAN;

interface
uses  Classes, ConnBase;

type
  //
  EPCanAPI =  (
              PA_INIT,
              PA_CLOSE,
              PA_STATUS,
              PA_WRITE,
              PA_READ,
              PA_READ_EX,
              PA_VERSION_INFO,
              PA_DLL_VERSION,
              PA_SPECIAL_FUNC,
              PA_RESET_CLIENT,
              PA_MSG_FILTER,
              PA_RESET_FILTER,
              PA_SET_DEVICENR,
              PA_GET_DEVICENR,
              PA_SET_RCVEVENT
              );
  PCanFunctionNames = array[EPCanAPI] of string;

  EDllInterface = (
                DI_CAN,
                DI_CAN2
                );

  // CAN Message
  //
  TPCANMsg = record
    ID: LongWord;               // 11/29 bit identifier
    MSGTYPE: Byte;              // Bits from MSGTYPE_*
    LEN: Byte;                  // Data Length Code of the Msg (0..8)
    DATA: array[0..7] of Byte;  // Data 0 .. 7
  end;

  // Timestamp of a receive/transmit event
  // Total microseconds = micros + 1000 * millis + $FFFFFFFF * 1000 * millis_overflow
  //
  TPCANTimestamp = record
    millis: LongWord;           // Base-value: milliseconds: 0.. 2^32-1
    millis_overflow: Word;      // Roll-arounds of millis
    micros: Word;               // Microseconds: 0..999
  end;

  CAN_INIT = function(wBTR0BTR1: Word; CANMsgType: Integer): longword; stdcall;
  CAN_CLOSE = function(): longword; stdcall;
  CAN_STATUS = CAN_CLOSE;
  CAN_WRITE = function(var MsgBuff: TPCANMsg): longword; stdcall;
  CAN_READ = CAN_WRITE;
  CAN_READEX = function(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp): longword; stdcall;
  CAN_VERSIONINFO = function(lpszTextBuff: PAnsiChar): longword; stdcall;
  CAN_DLLVERSIONINFO = CAN_VERSIONINFO;
  CAN_SPECIALFUNCTION = function(distributorcode: LongWord; codenumber: Integer): longword; stdcall;
  CAN_RESETCLIENT = CAN_CLOSE;
  CAN_MSGFILTER = function(FromID, ToID: LongWord; _Type: Integer): longword; stdcall;
  CAN_RESETFILTER = CAN_CLOSE;
  CAN_SETUSBDEVICENR = function(DevNum: LongWord): longword; stdcall;
  CAN_GETUSBDEVICENR = function(var DevNum: LongWord): longword; stdcall;
  CAN_SETRCVEVENT = function(hEvent: THandle): longword; stdcall;

  TConnPCanUsb = class(TConnBase)
  protected
    h_dll:    THandle;
  protected
    Init:  CAN_INIT;
    Close: CAN_CLOSE;
    Status:CAN_STATUS;
    Write: CAN_WRITE;
    Read:  CAN_READ;
    ReadEx:CAN_READEX;
    VersionInfo:     CAN_VERSIONINFO;
    DllVersionInfo:  CAN_DLLVERSIONINFO;
    SpecialFunction: CAN_SPECIALFUNCTION;
    ResetClient:     CAN_RESETCLIENT;
    MsgFilter:       CAN_MSGFILTER;
    ResetFilter:     CAN_RESETFILTER;
    SetUsbDeviceNr:  CAN_SETUSBDEVICENR;
    GetUsbDeviceNr:  CAN_GETUSBDEVICENR;
    SetRcvEvent:     CAN_SETRCVEVENT;
  protected
    function LoadDll(const fncnames: PCanFunctionNames; const sdll: string): boolean; overload;
    function LoadDll(const dllinterf: EDllInterface; const sdll: string = ''): boolean; overload;
    procedure UnloadDll();
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconf: string): boolean; overload; override;
    function Config(const sconfs: TStrings): boolean; overload; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
    function SendPacket(const a: array of char): boolean; override;
    function RecvPacket(var a: array of char; const tend: cardinal): boolean; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = true): integer; override;
    function RecvStrTimeout(var str: string; const tend: cardinal): integer; override;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = CINT_RECV_INTERVAL): integer; override;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; override;
    function WaitForReading(const tend: cardinal): boolean; override;
  end;
  PConnPCanUsb = ^TConnPCanUsb;

const
  CSTR_PCAN_CONFIG: array[EDllInterface] of string = (
                'PCAN_USB1',  //config name for the first dll file
                'PCAN_USB2'   //config name for the second dll file
                );
  CSTR_DLL_FILENAMES: array[EDllInterface] of string = (
                'PCAN_USB.dll', //default name of the first dll file
                'PCAN_2USB.dll' //default name of the second dll file
                );

  CSTR_CANUSB1_NAMES: PCanFunctionNames = (
                'CAN_Init',
                'CAN_Close',
                'CAN_Status',
                'CAN_Write',
                'CAN_Read',
                'CAN_ReadEx',
                'CAN_VersionInfo',
                'CAN_DLLVersionInfo',
                'CAN_SpecialFunktion',
                'CAN_ResetClient',
                'CAN_MsgFilter',
                'CAN_ResetFilter',
                'SetUSBDeviceNr',
                'GetUSBDeviceNr',
                'CAN_SetRcvEvent'
                );

  CSTR_CANUSB2_NAMES: PCanFunctionNames = (
                'CAN2_Init',
                'CAN2_Close',
                'CAN2_Status',
                'CAN2_Write',
                'CAN2_Read',
                'CAN2_ReadEx',
                'CAN2_VersionInfo',
                'CAN2_DLLVersionInfo',
                'CAN2_SpecialFunktion',
                'CAN2_ResetClient',
                'CAN2_MsgFilter',
                'CAN2_ResetFilter',
                'SetUSB2DeviceNr',
                'GetUSB2DeviceNr',
                'CAN2_SetRcvEvent'
                );

    // Constants definitions - Frame Type
    //
    CAN_INIT_TYPE_EX = $01;  // Extended Frame
    CAN_INIT_TYPE_ST = $00;  // Standard Frame

    // Constants definitions - ID
    //
    CAN_MAX_STANDARD_ID = $7ff;
    CAN_MAX_EXTENDED_ID = $1fffffff;

    // Constants definitions  - CAN message types
    //
    MSGTYPE_STANDARD = $00;  // Standard Data frame (11-bit ID)
    MSGTYPE_RTR      = $01;  // 1, if Remote Request frame
    MSGTYPE_EXTENDED = $02;  // 1, if Extended Data frame (CAN 2.0B, 29-bit ID)
    MSGTYPE_ERROR    = $80;  // 1, if Status information

    // Baud rate codes = BTR0/BTR1 register values for the CAN controller.
    // You can define your own Baudrate with the BTROBTR1 register !!
    // take a look at www.peak-system.com for our software BAUDTOOL to
    // calculate the BTROBTR1 register for every baudrate and sample point.
    //
    CAN_BAUD_1M   =  $0014;       //   1 MBit/s
    CAN_BAUD_500K =  $001C;       // 500 kBit/s
    CAN_BAUD_250K =  $011C;       // 250 kBit/s
    CAN_BAUD_125K =  $031C;       // 125 kBit/s
    CAN_BAUD_100K =  $432F;       // 100 kBit/s
    CAN_BAUD_50K  =  $472F;       //  50 kBit/s
    CAN_BAUD_20K  =  $532F;       //  20 kBit/s
    CAN_BAUD_10K  =  $672F;       //  10 kBit/s
    CAN_BAUD_5K   =  $7F7F;       //   5 kBit/s

    // Error codes (bit code)
    //
    CAN_ERR_OK         = $0000;   // No error
    CAN_ERR_XMTFULL    = $0001;   // Transmit buffer in CAN controller is full
    CAN_ERR_OVERRUN    = $0002;   // CAN controller was read too late
    CAN_ERR_BUSLIGHT   = $0004;   // Bus error: an error counter reached the 'light' limit
    CAN_ERR_BUSHEAVY   = $0008;   // Bus error: an error counter reached the 'heavy' limit  
    CAN_ERR_BUSOFF     = $0010;   // Bus error: the CAN controller is in bus-off state
    CAN_ERR_QRCVEMPTY  = $0020;   // Receive queue is empty
    CAN_ERR_QOVERRUN   = $0040;   // Receive queue was read too late
    CAN_ERR_QXMTFULL   = $0080;   // Transmit queue ist full
    CAN_ERR_REGTEST    = $0100;   // Test of the CAN controller hardware registers failed (no hardware found)
    CAN_ERR_NOVXD      = $0200;   // Driver not loaded
    CAN_ERR_NODRIVER   = $0200;   // Driver not loaded
    CAN_ERRMASK_ILLHANDLE=$1C00;  // Mask for all handle errors
    CAN_ERR_HWINUSE    = $0400;   // Hardware already in use by a Net
    CAN_ERR_NETINUSE   = $0800;   // a Client is already connected to the Net
    CAN_ERR_ILLHW      = $1400;   // Hardware handle is invalid
    CAN_ERR_ILLNET     = $1800;   // Net handle is invalid
    CAN_ERR_ILLCLIENT  = $1C00;   // Client handle is invalid
    CAN_ERR_RESOURCE   = $2000;   // Resource (FIFO, Client, timeout) cannot be created
    CAN_ERR_ILLPARAMTYPE = $4000; // Invalid parameter
    CAN_ERR_ILLPARAMVAL  = $8000; // Invalid parameter value
    CAN_ERR_UNKNOWN    = $10000;  // Unknown error
    CAN_ERR_ANYBUSERR = (CAN_ERR_BUSLIGHT or CAN_ERR_BUSHEAVY or CAN_ERR_BUSOFF);

implementation
uses Windows, SysUtils;

function TConnPCanUsb.LoadDll(const fncnames: PCanFunctionNames; const sdll: string): boolean;
begin
  try
    UnloadDll();
    h_dll := LoadLibrary(PChar(sdll));
    result := (h_dll <> 0);
    if result then begin
      Init := GetProcAddress(h_dll, PChar(fncnames[PA_INIT]));
      Close := GetProcAddress(h_dll, PChar(fncnames[PA_CLOSE]));
      Status := GetProcAddress(h_dll, PChar(fncnames[PA_STATUS]));
      Write := GetProcAddress(h_dll, PChar(fncnames[PA_WRITE]));
      Read := GetProcAddress(h_dll, PChar(fncnames[PA_READ]));
      ReadEx := GetProcAddress(h_dll, PChar(fncnames[PA_READ_EX]));
      VersionInfo := GetProcAddress(h_dll, PChar(fncnames[PA_VERSION_INFO]));
      DllVersionInfo := GetProcAddress(h_dll, PChar(fncnames[PA_DLL_VERSION]));
      SpecialFunction := GetProcAddress(h_dll, PChar(fncnames[PA_SPECIAL_FUNC]));
      ResetClient := GetProcAddress(h_dll, PChar(fncnames[PA_RESET_CLIENT]));
      MsgFilter := GetProcAddress(h_dll, PChar(fncnames[PA_MSG_FILTER]));
      ResetFilter := GetProcAddress(h_dll, PChar(fncnames[PA_RESET_FILTER]));
      SetUsbDeviceNr := GetProcAddress(h_dll, PChar(fncnames[PA_SET_DEVICENR]));
      GetUsbDeviceNr := GetProcAddress(h_dll, PChar(fncnames[PA_GET_DEVICENR]));
      SetRcvEvent := GetProcAddress(h_dll, PChar(fncnames[PA_SET_RCVEVENT]));
    end;
  except
    UnloadDll();
    result := false;
  end;
end;

function TConnPCanUsb.LoadDll(const dllinterf: EDllInterface; const sdll: string): boolean;
var s_dllfile: string;
begin
  if (sdll = '') then s_dllfile := CSTR_DLL_FILENAMES[dllinterf]
  else s_dllfile := trim(sdll);
  if (dllinterf = DI_CAN) then result := LoadDll(CSTR_CANUSB1_NAMES, s_dllfile)
  else result := LoadDll(CSTR_CANUSB2_NAMES, s_dllfile)
end;

procedure TConnPCanUsb.UnloadDll();
begin
  if (h_dll <> 0) then begin
    if FreeLibrary(h_dll) then begin
      h_dll := 0;
      Init := nil;
      Close := nil;
      Status := nil;
      Write := nil;
      Read := nil;
      ReadEx := nil;
      VersionInfo := nil;
      DllVersionInfo := nil;
      SpecialFunction := nil;
      ResetClient := nil;
      MsgFilter := nil;
      ResetFilter := nil;
      SetUsbDeviceNr := nil;
      GetUsbDeviceNr := nil;
      SetRcvEvent := nil;
    end;
  end;
end;

constructor TConnPCanUsb.Create(owner: TComponent);
begin
  inherited Create(owner);
  h_dll := 0;
  Init := nil;
  Close := nil;
  Status := nil;
  Write := nil;
  Read := nil;
  ReadEx := nil;
  VersionInfo := nil;
  DllVersionInfo := nil;
  SpecialFunction := nil;
  ResetClient := nil;
  MsgFilter := nil;
  ResetFilter := nil;
  SetUsbDeviceNr := nil;
  GetUsbDeviceNr := nil;
  SetRcvEvent := nil;
end;

destructor TConnPCanUsb.Destroy;
begin
  UnloadDll();
  inherited Destroy();
end;

function TConnPCanUsb.Config(const sconf: string): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.Config(const sconfs: TStrings): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.Connect(): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.Disconnect: boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.SendPacket(const a: array of char): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.RecvPacket(var a: array of char; const tend: cardinal): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.SendStr(const str: string): boolean;
begin
  result := false;
  //todo:
end;

function TConnPCanUsb.RecvStr(var str: string; const bwait: boolean): integer;
begin
  result := 0;
  //todo:
end;

function TConnPCanUsb.RecvStrTimeout(var str: string; const tend: cardinal): integer;
begin
  result := 0;
  //todo:
end;

function TConnPCanUsb.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
begin
  result := 0;
  //todo:
end;

function TConnPCanUsb.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean): integer;
begin
  result := 0;
  //todo:
end;

function TConnPCanUsb.WaitForReading(const tend: cardinal): boolean; 
begin
  result := false;
  //todo:
end;

end.
