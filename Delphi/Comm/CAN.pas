// =============================================================================
// Module name  : $RCSfile: CAN.pas,v $
// Description  : This unit defines classes for communication over can bus.
//                TConnPCanUsb wraps the interface of pcan-light, but the dll
//                file will be dynamic loaded, so that the dll file is changable
//                in the configuration and the dll-hell problem can be avoided.
// Compiler     : Delphi 2007
// Author       : 2016-06-14 /bsu/
// History      :

unit CAN;

interface
uses  Classes, ConnBase;

type
//======================start of definitions from pcan-light====================
  // CAN Message
  //
  TPCANMsg = packed record
    ID: LongWord;               // 11/29 bit identifier
    MSGTYPE: Byte;              // Bits from MSGTYPE_*
    LEN: Byte;                  // Data Length Code of the Msg (0..8)
    DATA: array[0..7] of Byte;  // Data 0 .. 7
  end;

  // Timestamp of a receive/transmit event
  // Total microseconds = micros + 1000 * millis + $FFFFFFFF * 1000 * millis_overflow
  //
  TPCANTimestamp = packed record
    millis: LongWord;           // Base-value: milliseconds: 0.. 2^32-1
    millis_overflow: Word;      // Roll-arounds of millis
    micros: Word;               // Microseconds: 0..999
  end;

///////////////////////////////////////////////////////////////////////////////
//  CAN_Init()
//  This function does the following:
//      - Activates a Hardware
//      - Performs a Register Test of 82C200/SJA1000
//      - Allocates a Send buffer and a Hardware handle
//      - Programs the configuration of the transmit/receive driver
//      - Sets the Baudrate register
//      - Sets the Controller in RESET condition
//
//  If CANMsgType=0  ---> ID 11Bit
//  If CANMsgType=1  ---> ID 11/29Bit 
//
//  Possible Errors: NOVXD ILLHW REGTEST RESOURCE
//
//function CAN_Init(wBTR0BTR1: Word; CANMsgType: Integer): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_Close()
//  This function terminate and release the configured hardware and all 
//  allocated resources
//
//  Possible Errors: NOVXD
//
//function CAN_Close: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  Init()
//  This function make the following:
//		- Activate a Hardware
//		- Make a Register Test of 82C200/SJA1000
//		- Allocate a Send buffer and a Hardware handle
//		- Programs the configuration of the transmit/receive driver
//		- Set the Baudrate register
//		- Set the Controller in RESET condition	
//		
//  If CANMsgType=0  ---> ID 11Bit
//  If CANMsgType=1  ---> ID 11/29Bit 
//
//  CANHw Type:  (Only new SJA Dongle !!!)
//	HW_ISA             1
//	HW_DONGLE_SJA      5
//	HW_DONGLE_SJA_EPP  6
//	HW_DONGLE_PRO      7
//	HW_DONGLE_PRO_EPP  8
//	HW_ISA_SJA         9
//	HW_PCI		       10
//
//  Possible Errors: NOVXD ILLHW REGTEST RESOURCE
//
//function CAN_Init(wBTR0BTR1: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord; Interupt: Word): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_Status()
//  This function request the current status of the hardware (b.e. BUS-OFF)
//
//  Possible Errors: NOVXD BUSOFF BUSHEAVY OVERRUN
//
//function CAN_Status: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_Write()
//  This function Place a CAN message into the Transmit Queue of the CAN Hardware
//
//  Possible Errors: NOVXD RESOURCE BUSOFF QXMTFULL
//
//function CAN_Write(var MsgBuff: TPCANMsg): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_Read()
//  This function get the next message or the next error from the Receive Queue of 
//  the CAN Hardware.  
//  REMARK:
//      - Check always the type of the received Message (MSGTYPE_STANDARD,MSGTYPE_RTR,
//        MSGTYPE_EXTENDED,MSGTYPE_STATUS)
//      - The function will return ERR_OK always that you receive a CAN message successfully
//        although if the messages is a MSGTYPE_STATUS message.
//      - When a MSGTYPE_STATUS message is returned, the ID and Length information of the message
//        will be treated as undefined values. Actually information of the received message
//        should be interpreted using the first 4 data bytes as follows:
//              Data0  Data1  Data2  Data3  Kind of Error
//              $00    $00    $00    $02    CAN_ERR_OVERRUN     $0002  CAN Controller was read to late
//              $00    $00    $00    $04    CAN_ERR_BUSLIGHT    $0004  Bus Error: An error counter limit reached (96)
//              $00    $00    $00    $08    CAN_ERR_BUSHEAVY    $0008  Bus Error: An error counter limit reached (128)
//              $00    $00    $00    $10    CAN_ERR_BUSOFF      $0010  Bus Error: Can Controller went "Bus-Off"
//      - If a CAN_ERR_BUSOFF status message is received, the CAN Controller must to be
//        initialized again using the Init() function.  Otherwise, will be not possible
//        to send/receive more messages.
//      - The message will be written to 'msgbuff'.
//
//  Possible Errors: NOVXD  QRCVEMPTY
//
//function CAN_Read(var MsgBuff: TPCANMsg): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_ReadEx()
//  This function get the next message or the next error from the Receive Queue of 
//  the CAN Hardware and the time when the message arrived.  
//  REMARK:
//      - Check always the type of the received Message (MSGTYPE_STANDARD,MSGTYPE_RTR,
//        MSGTYPE_EXTENDED,MSGTYPE_STATUS)
//      - The function will return ERR_OK always that you receive a CAN message successfully 
//        although if the messages is a MSGTYPE_STATUS message.  
//      - When a MSGTYPE_STATUS message is returned, the ID and Length information of the message 
//        will be treated as undefined values. Actually information of the received message
//        should be interpreted using the first 4 data bytes as follows:
//              Data0  Data1  Data2  Data3  Kind of Error
//              $00    $00    $00    $02    CAN_ERR_OVERRUN     $0002  CAN Controller was read to late
//              $00    $00    $00    $04    CAN_ERR_BUSLIGHT    $0004  Bus Error: An error counter limit reached (96)
//              $00    $00    $00    $08    CAN_ERR_BUSHEAVY    $0008  Bus Error: An error counter limit reached (128)
//              $00    $00    $00    $10    CAN_ERR_BUSOFF      $0010  Bus Error: Can Controller went "Bus-Off"
//      - If a CAN_ERR_BUSOFF status message is received, the CAN Controller must to be 
//        initialized again using the Init() function.  Otherwise, will be not possible 
//        to send/receive more messages. 
//      - The message will be written to 'msgbuff'.
//      Since Version 2.x the Ext. Version is available - new Parameter:
//      -  Receive timestamp
//
//  Possible Errors: NOVXD  QRCVEMPTY
//
//function CAN_ReadEx(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp): LongWord; stdcall;
        
///////////////////////////////////////////////////////////////////////////////
//  CAN_VersionInfo()
//  This function get the Version and copyright of the hardware as text 
//  (max. 255 characters)
//
//  Possible Errors:  NOVXD
//
//function CAN_VersionInfo(lpszTextBuff: PAnsiChar): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_DLLVersionInfo()
//  This function is used to get the Version and copyright of the DLL as 
//  text (max. 255 characters)
//
//  Possible Errors: -1 for NULL-Pointer parameters :-)
//
//function CAN_DLLVersionInfo(lpszTextBuff: PAnsiChar): LongWord; stdcall;
        
///////////////////////////////////////////////////////////////////////////////
//  CAN_SpecialFunktion()
//  This function is an special function to be used "ONLY" for distributors
//  Return: 1 - the given parameters and the parameters in the hardware agree 
//          0 - otherwise
//
//  Possible Errors:  NOVXD
//
//function CAN_SpecialFunktion(distributorcode: LongWord; codenumber: Integer): LongWord; stdcall;

//////////////////////////////////////////////////////////////////////////////
//  CAN_ResetClient()
//  This function delete the both queues (Transmit,Receive) of the CAN Controller 
//  using a RESET
//
//  Possible Errors: ERR_ILLCLIENT ERR_NOVXD
//
//function CAN_ResetClient: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_MsgFilter(FromID, ToID, int Type)
//  This function set the receive message filter of the CAN Controller.
//  REMARK:
//      - A quick register of all messages is possible using the parameters FromID and ToID = 0
//      - Every call of this function maybe cause an extention of the receive filter of the
//        CAN controller, which one can go briefly to RESET
//      - New in Ver 2.x:
//          * Standard frames will be put it down in the acc_mask/code as Bits 28..13
//          * Hardware driver for 82C200 must to be moved to Bits 10..0 again!
//  WARNING:
//      It is not guaranteed to receive ONLY the registered messages.
//
//  Possible Errors: NOVXD ILLCLIENT ILLNET REGTEST
//
//function CAN_MsgFilter(FromID, ToID: LongWord; _Type: Integer): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_ResetFilter()
//  This function close completely the Message Filter of the Hardware.
//  They will be no more messages received.
//
//  Possible Errors: NOVXD
//
//function CAN_ResetFilter: LongWord; stdcall;

//////////////////////////////////////////////////////////////////////////////
//  SetUSBDeviceNr()
//  This function set an identification number to the USB CAN hardware
//
//  Possible Errors: NOVXD ILLHW ILLPARAMTYPE ILLPARAMVAL REGTEST
//
//function SetUSBDeviceNr(DevNum: LongWord): LongWord; stdcall;

//////////////////////////////////////////////////////////////////////////////
//  GetUSBDeviceNr()
//  This function read the device number of a USB CAN Hardware
//
//  Possible Errors: NOVXD ILLHW ILLPARAMTYPE
//
//function GetUSBDeviceNr(var DevNum: LongWord): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN_SetRcvEvent()
//  This function is used to set the Event for the Event Handler
//
//  Possible Errors: ILLCLIENT ILLPARAMTYPE ILLPARAMVAL NOVXD
//
//function CAN_SetRcvEvent(hEvent: THandle): LongWord; stdcall;

//======================end of definitions from pcan-light======================

  EPCanFunction = (
                  PCF_INIT,
                  PCF_CLOSE,
                  PCF_STATUS,
                  PCF_WRITE,
                  PCF_READ,
                  PCF_READEX,
                  PCF_VERSIONINFO,
                  PCF_DLLVERSIONINFO,
                  PCF_SPECIALFUNCTION,
                  PCF_RESETCLIENT,
                  PCF_MSGFILTER,
                  PCF_RESETFILTER,
                  PCF_SETRCVEVENT,
                  PCF_SETUSBDEVICENR,
                  PCF_GETUSBDEVICENR
                  );
  PCanFunctionNames = array[EPCanFunction] of string;

  EPCanProperty = (
                  PCP_HWT,
                  PCP_DLL,
                  PCP_BAUD,
                  PCP_CANVER,
                  PCP_SUBHW,
                  PCP_IOPORT,
                  PCP_IRQ
                  );

  EPCanHardwareType = (
                      PCH_NONE,
                      PCH_ISA1CH,		// ISA 1 Channel
                      PCH_ISA2CH,		// ISA 2 Channels
											PCH_PCI1CH,		// PCI 1 Channel
											PCH_PCI2CH,		// PCI 2 Channels
											PCH_PCC1CH,		// PCC 1 Channel
											PCH_PCC2CH,		// PCC 2 Channels
											PCH_USB1CH,		// USB 1st Channel
                      PCH_USB2CH,   // USB 2nd Channel
											PCH_DNP,		  // DONGLE PRO
											PCH_DNG		 		// DONGLE
                      );

  EPCanBaudrate = (
                  PCB_1M,
                  PCB_500K,
                  PCB_250K,
                  PCB_125K,
                  PCB_100K,
                  PCB_50K,
                  PCB_20K,
                  PCB_10K,
                  PCB_5K
                  );

  EPCanVersion = (
                  PCV_STD,   //can standard with 11-bit id
                  PCV_EXT    //can 2.0B extend with 29-bit id
                  );

  EPCanNPnPType = (
                  HW_ISA            = 1,
                  HW_DONGLE_SJA     = 5,
                  HW_DONGLE_SJA_EPP = 6,
                  HW_DONGLE_PRO     = 7,
                  HW_DONGLE_PRO_EPP = 8,
                  HW_ISA_SJA        = 9,
                  HW_PCI		        = 10
                  );
  TPCanReadThread = class;
  TPCanLight = class(TConnBase)
  class function FindHardwareType(const shwt: string; var ehwt: EPCanHardwareType): boolean; virtual;
  protected
    e_hwt:    EPCanHardwareType;  //indicate hardware type of the can adapter, see EPCanHardwareType
    e_baud:   EPCanBaudrate;      //indicate baudrate of can bus, see EPCanBaudrate
    e_canver: EPCanVersion;       //indicate version of can bus, see EPCanVersion
    h_dll:    THandle;
    h_rcveve: THandle;
    s_dllfile:string;
    t_rbuf:   TPCANMsg;
    t_wbuf:   TPCANMsg;
    lw_sendcnt, lw_recvcnt: longword;
    lw_devnr:   longword;
    e_subtype:  EPCanNPnPType;
    lw_ioport:  longword;
    w_irq:      word;
    t_thread:  TPCanReadThread;
    a_pcanfnt: array[EPCanFunction] of Pointer;
  protected
    function LoadDll(const sdll: string): boolean; virtual;
    function LoadDefaultDll(const hwt: EPCanHardwareType): boolean; overload;
    function LoadDefaultDll(const shwt: string): boolean; overload;
    function UnloadDll(): boolean; virtual;
    function GetErrorMsg(const errnr: longword): string; virtual;
    function FillSendMsg(const msg: string): boolean; virtual;
    function BuildRecvMsg(var msg: string): boolean; virtual;
    function IsValidFunction(const canfnt: EPCanFunction; var errnr: longword): boolean;
    function SetNPnP(etype: EPCanNPnPType; ioport: longword; irq: word): boolean;
    function SetProperty(const eprop: EPCanProperty; const sval: string): boolean;
    function SetHardwareType(const hwt: string): boolean;
    function SetBaudrate(const sbaud: string): boolean;
    function SetCanVersion(const sver: string): boolean;
    function GetHardwareTypeText(): string;
    function GetBaudrateText(): string;
    function IsReadable(): boolean; override;
    function IsWriteComplete(): boolean; override;
    procedure TryConnect(); override;
    procedure SetMsgVersion(ecanver: EPCanVersion);
    procedure SetDeviceNr(devnr: longword);

    function CanInitPnP(wBR: Word; CANMsgType: Integer): longword; virtual;
    function CanInitNPnP(wBR: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord; Interupt: Word): longword; virtual;
    function CanClose(): longword; virtual;
    function CanStatus(): longword; virtual;
    function CanWrite(var MsgBuff: TPCANMsg): longword; virtual;
    function CanRead(var MsgBuff: TPCANMsg): longword; virtual;
    function CanReadEx(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp): longword; virtual;
    function CanVersionInfo(lpszTextBuff: PAnsiChar): longword; virtual;
    function CanDllVersionInfo(lpszTextBuff: PAnsiChar): longword; virtual;
    function CanSpecialFunction(distributorcode: LongWord; codenumber: Integer): longword; virtual;
    function CanResetClient(): longword; virtual;
    function CanMsgFilter(FromID, ToID: LongWord; _Type: Integer): longword; virtual;
    function CanResetFilter(): longword; virtual;
    function CanSetRcvEvent(hEvent: THandle): longword; virtual;
    function CanSetUsbDeviceNr(DevNum: LongWord): longword;
    function CanGetUsbDeviceNr(var DevNum: LongWord): longword;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;

    //properties of pcan light
    property HardwareType: EPCanHardwareType read e_hwt write e_hwt;
    property HardwareTypeText: string read GetHardwareTypeText;
    property DllFile: string read s_dllfile;
    property Baudrate: EPCanBaudrate read e_baud write e_baud;
    property BaudrateText: string read GetBaudrateText;
    property MsgVersion: EPCanVersion read e_canver write SetMsgVersion;
    property HardwareSubType: EPCanNPnPType read e_subtype write e_subtype;
    property IoPort: longword read lw_ioport write lw_ioport;
    property Irq: word read w_irq write w_irq;
    property UsbDeviceNr: longword read lw_devnr write SetDeviceNr;
    property RecvEvent: THandle read h_rcveve;
    property CountSending: cardinal read lw_sendcnt;
    property CountReceive: cardinal read lw_recvcnt;

    function SetDllFile(const sdll: string): boolean;
    function BuildMessage(const canmsg: TPCANMsg): string;
    function Config(const sconfs: TStrings): boolean; overload; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
    function SendBuf(const buf: PChar; const len: longword): boolean; override;
    function RecvBuf(var buf: PChar; const len: longword): integer; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = false): integer; override;
  end;
  PPCanLight = ^TPCanLight;

  //a thread for receiving data from can-bus
  TPCanReadThread = class(TThread)
  protected
    t_pcan: TPCanLight;
  protected
    procedure Execute(); override;
    procedure ReadCanMessage();
  public
    constructor Create(pcan: TPCanLight);
    destructor Destroy(); override;
  end;

const
  // ===================start of constants from pcan-light======================
  // start of definitions from pcan-light
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
  // End of definitions from pcan-light
  // ===================End of constants from pcan-light========================

  ERR_HARDWARE_TYPE = $FFFD; // hardware typ is not given
  ERR_PCAN_FNT      = $FFFE; // Function pointer of pcan-light is null
  ERR_NO_DLL        = $FFFF; // No dll is loaded

  CSTR_PCAN_PROPERTIES: array[EPCanProperty] of string = (
                        'HWT',      //hardware type
                        'PCANDLL',  //config name for the dll file
                        'BAUDRATE', //config name for the baudrate of can bus
                        'CANVER',   //version of can bus: 11 or 29 bits
                        'SUBHW',    //sub type of None-PnP device
                        'IOPORT',   //IO port
                        'IRQ'       //number of interruption
                        );

  CSTR_PCAN_HWTS: array[EPCanHardwareType] of string = (
                  'NONE',
                  'ISA1CH',		// ISA 1 Channel
                  'ISA2CH',		// ISA 2 Channels
									'PCI1CH',		// PCI 1 Channel
									'PCI2CH',		// PCI 2 Channels
									'PCC1CH',		// PCC 1 Channel
									'PCC2CH',		// PCC 2 Channels
									'USB1CH',		// USB 1st Channel
                  'USB2CH',   // USB 2nd Channel
									'DNP',		  // DONGLE PRO
									'DNG'		 		// DONGLE
                  );

  CSTR_PCAN_DLLFILES: array[EPCanHardwareType] of string = (
                      '',
                      'PCAN_ISA.dll',		// ISA 1 Channel
                      'PCAN_2ISA.dll',	// ISA 2 Channels
											'PCAN_PCI.dll',		// PCI 1 Channel
											'PCAN_2PCI.dll',	// PCI 2 Channels
											'PCAN_PCC.dll',		// PCC 1 Channel
											'PCAN_2PCC.dll',	// PCC 2 Channels
											'PCAN_USB.dll',		// USB 1st Channel
                      'PCAN_2USB.dll',  // USB 2nd Channel
											'PCAN_DNP.dll',		// DONGLE PRO
											'PCAN_DNG.dll'		// DONGLE
                      );

  CINT_PCAN_BAUDRATES: array[EPCanBaudrate] of Word = (
                       CAN_BAUD_1M,
                       CAN_BAUD_500K,
                       CAN_BAUD_250K,
                       CAN_BAUD_125K,
                       CAN_BAUD_100K,
                       CAN_BAUD_50K,
                       CAN_BAUD_20K,
                       CAN_BAUD_10K,
                       CAN_BAUD_5K
                       );

  CSTR_PCAN_BAUDRATES: array[EPCanBaudrate] of string = (
                       '1M',
                       '500K',
                       '250K',
                       '125K',
                       '100K',
                       '50K',
                       '20K',
                       '10K',
                       '5K'
                       );
  CSTR_PCAN_VERSIONS: array[EPCanVersion] of string = (
                      'STD',
                      'EXT'
                      );

  CSTR_PCAN1CH_NAMES: PCanFunctionNames = (
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
                'CAN_SetRcvEvent',
                'SetUSBDeviceNr',
                'GetUSBDeviceNr'
                );

  CSTR_PCAN2CH_NAMES: PCanFunctionNames = (
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
                'CAN2_SetRcvEvent',
                'SetUSB2DeviceNr',
                'GetUSB2DeviceNr'
                );

  C_CANMSG_LENGTH_MAX = 8;

implementation
uses Windows, SysUtils, StrUtils, TextMessage, TypInfo, Forms;
type
  SetPCanProperty = function(ppcan: TPCanLight; const sval: string): boolean;
  CAN_INIT_PNP = function(wBR: Word; CANMsgType: Integer): longword; stdcall;
  CAN_INIT_NPNP = function(wBR: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord; Interupt: Word): LongWord; stdcall;           // init Non Plug And Play
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
  CAN_SETRCVEVENT = function(hEvent: THandle): longword; stdcall;
  CAN_SETUSBDEVICENR = function(DevNum: LongWord): longword; stdcall;
  CAN_GETUSBDEVICENR = function(var DevNum: LongWord): longword; stdcall;

class function TPCanLight.FindHardwareType(const shwt: string; var ehwt: EPCanHardwareType): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := IndexText(shwt, CSTR_PCAN_HWTS);
  if ((i_idx >= Ord(Low(EPCanHardwareType))) and (i_idx <= Ord(High(EPCanHardwareType)))) then begin
    ehwt := EPCanHardwareType(i_idx);
    result := true;
  end;
end;

function TPCanLight.LoadDll(const sdll: string): boolean;
var i: EPCanFunction;
begin
  result := false;
  if e_hwt <> PCH_NONE then begin
    try
      UnloadDll();
      h_dll := LoadLibrary(PChar(sdll));
      result := (h_dll <> 0);
      if result then begin
        case e_hwt of
        // ISA 1 Channel, PCI 1 Channel, PCC 1 Channel, USB 1st Channel, DONGLE PRO, DONGLE
        PCH_ISA1CH, PCH_PCI1CH, PCH_PCC1CH,	PCH_USB1CH,	PCH_DNP, PCH_DNG:
        begin
          for i := Low(EPCanFunction) to High(EPCanFunction) do
            a_pcanfnt[i] := GetProcAddress(h_dll, PChar(CSTR_PCAN1CH_NAMES[i]));
        end;
        //ISA 2 Channels, PCI 2 Channels, PCC 2 Channels, USB 2nd Channel
        PCH_ISA2CH, PCH_PCI2CH, PCH_PCC2CH, PCH_USB2CH:
        begin
          for i := Low(EPCanFunction) to High(EPCanFunction) do
            a_pcanfnt[i] := GetProcAddress(h_dll, PChar(CSTR_PCAN2CH_NAMES[i]));
        end;
        else
        end;
        //check if the primary functions are found
        result := assigned(a_pcanfnt[PCF_INIT]) and
                  assigned(a_pcanfnt[PCF_WRITE]) and
                  assigned(a_pcanfnt[PCF_READ]) and
                  assigned(a_pcanfnt[PCF_READEX]);
        if result then s_dllfile := sdll
        else begin
          UnloadDll();
          AddMessage(format('No expected function is found in the dll "%s"', [sdll]), ML_ERROR);
        end;
      end else AddMessage(format('Failed to load dll: %s', [sdll]), ML_ERROR);
    except
      UnloadDll();
      result := false;
    end;
  end else AddMessage(GetErrorMsg(ERR_HARDWARE_TYPE), ML_ERROR);
end;

function TPCanLight.LoadDefaultDll(const hwt: EPCanHardwareType): boolean;
begin
  result := LoadDll(CSTR_PCAN_DLLFILES[hwt]);
end;

function TPCanLight.LoadDefaultDll(const shwt: string): boolean;
var e_htype: EPCanHardwareType;
begin
  result := FindHardwareType(shwt, e_htype);
  if result then result := LoadDefaultDll(e_htype);
end;

function TPCanLight.UnloadDll(): boolean;
var i: EPCanFunction;
begin
  if (h_dll <> 0) then begin
    Disconnect();
    if FreeLibrary(h_dll) then begin
      s_dllfile := '';
      h_dll := 0;
      for i := Low(EPCanFunction) to High(EPCanFunction) do a_pcanfnt[i] := nil;
    end;
  end;
  result := true;
end;

function TPCanLight.GetErrorMsg(const errnr: longword): string;
begin
  case errnr of
  CAN_ERR_OK:           result := 'No error';
  CAN_ERR_XMTFULL:      result := 'Transmit buffer in CAN controller is full';
  CAN_ERR_OVERRUN:      result := 'CAN controller was read too late';
  CAN_ERR_BUSLIGHT:     result := 'Bus error: an error counter reached the ''light'' limit';
  CAN_ERR_BUSHEAVY:     result := 'Bus error: an error counter reached the ''heavy'' limit';
  CAN_ERR_BUSOFF:       result := 'Bus error: the CAN controller is in bus-off state';
  CAN_ERR_QRCVEMPTY:    result := 'Receive queue is empty';
  CAN_ERR_QOVERRUN:     result := 'Receive queue was read too late';
  CAN_ERR_QXMTFULL:     result := 'Transmit queue ist full';
  CAN_ERR_REGTEST:      result := 'Test of the CAN controller hardware registers failed (no hardware found)';
  CAN_ERR_NOVXD:        result := 'Driver not loaded';
  CAN_ERR_HWINUSE:      result := 'Hardware already in use by a Net';
  CAN_ERR_NETINUSE:     result := 'a Client is already connected to the Net';
  CAN_ERR_ILLHW:        result := 'Hardware handle is invalid';
  CAN_ERR_ILLNET:       result := 'Net handle is invalid';
  CAN_ERR_ILLCLIENT:    result := 'Client handle is invalid';
  CAN_ERR_RESOURCE:     result := 'Resource (FIFO, Client, timeout) cannot be created';
  CAN_ERR_ILLPARAMTYPE: result := 'Invalid parameter';
  CAN_ERR_ILLPARAMVAL:  result := 'Invalid parameter value';
  CAN_ERR_UNKNOWN:      result := 'Unknown error';
  ERR_HARDWARE_TYPE:    result := 'The type of hardware is unknown';
  ERR_PCAN_FNT:         result := 'Function ''%s'' is not found in the dll' + '"' + s_dllfile + '"';
  ERR_NO_DLL:           result := 'No dll is loaded';
  else                  result := format('Error number (%d) is unknown', [errnr]);
  end;
end;

//msg is a formatted string and looks like '58A:2038F612' with hexidecimal format 'can_nodenr:data'
//note: byte order have to convert to big-endien before sending
function TPCanLight.FillSendMsg(const msg: string): boolean;
var s_nodenr, s_data: string; i, i_len, i_pos, i_nodenr, i_data: integer;
begin
  result := false;
  i_pos := Pos(':', msg);
  i_len := Length(msg);
  if (i_pos > 0) then begin
    s_nodenr := trim(LeftStr(msg, i_pos - 1));
    if TryStrToInt('$' + s_nodenr, i_nodenr) then begin
      s_data := trim(RightStr(msg, i_len - i_pos));
      i_len := length(s_data);
      if ((i_len mod 2) <> 0) then s_data := '0' + s_data; //add prefix '0' if a half byte exists in the string
      i_len := Round(i_len / 2);
      if i_len > C_CANMSG_LENGTH_MAX then i_len := C_CANMSG_LENGTH_MAX;
      for i := 0 to i_len - 1 do begin
        result := TryStrToInt('$' + MidStr(s_data, i * 2 + 1, 2), i_data);
        if result then t_wbuf.DATA[i] := byte(i_data)
        else break;
      end;
      if result then begin
        t_wbuf.ID := i_nodenr;
        t_wbuf.LEN := byte(i_len);
      end;
    end;
  end;
end;

function TPCanLight.BuildRecvMsg(var msg: string): boolean;
begin
  msg := BuildMessage(t_rbuf);
  result := true;
end;

function TPCanLight.IsValidFunction(const canfnt: EPCanFunction; var errnr: longword): boolean;
var s_errmsg: string;
begin
  result := ((h_dll <> 0) and (e_hwt <> PCH_NONE) and assigned(a_pcanfnt[canfnt]));
  errnr := CAN_ERR_OK;
  if (not result) then begin
    if (h_dll = 0) then begin
      errnr := ERR_NO_DLL;
      s_errmsg := GetErrorMsg(ERR_NO_DLL)
    end else begin
      case e_hwt of
      PCH_ISA1CH, PCH_PCI1CH, PCH_PCC1CH,	PCH_USB1CH,	PCH_DNP, PCH_DNG: begin
        errnr := ERR_PCAN_FNT;
        s_errmsg := format(GetErrorMsg(errnr), [CSTR_PCAN1CH_NAMES[canfnt]]);
      end;
      PCH_ISA2CH, PCH_PCI2CH, PCH_PCC2CH, PCH_USB2CH: begin
        errnr := ERR_PCAN_FNT;
        s_errmsg := format(GetErrorMsg(errnr), [CSTR_PCAN2CH_NAMES[canfnt]]);
      end else begin
        errnr := ERR_HARDWARE_TYPE;
        s_errmsg := GetErrorMsg(errnr);
      end;
      end;
    end;
    AddMessage(s_errmsg, ML_ERROR);
  end;
end;

function TPCanLight.SetNPnP(etype: EPCanNPnPType; ioport: longword; irq: word): boolean;
begin
  e_subtype := etype;
  lw_ioport := ioport;
  w_irq := irq;
  result := true;
end;

function TPCanLight.SetProperty(const eprop: EPCanProperty; const sval: string): boolean;
begin
  result := false;
  case eprop of
    PCP_HWT:    result := SetHardwareType(sval);
    PCP_DLL:    result := SetDllFile(sval);
    PCP_BAUD:   result := SetBaudrate(sval);
    PCP_CANVER: result := SetCanVersion(sval);
    PCP_SUBHW:  result := true; //todo
    PCP_IOPORT: result := true; //todo
    PCP_IRQ:    result := true; //todo  
  end;
end;

function TPCanLight.SetHardwareType(const hwt: string): boolean;
var e_htype: EPCanHardwareType;
begin
  result := TPCanLight.FindHardwareType(hwt, e_htype);
  if result then e_hwt := e_htype;
end;

function TPCanLight.SetBaudrate(const sbaud: string): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := IndexText(sbaud, CSTR_PCAN_BAUDRATES);
  if ((i_idx >= Ord(Low(EPCanBaudrate))) and (i_idx <= Ord(High(EPCanBaudrate)))) then begin
    e_baud := EPCanBaudrate(i_idx);
    result := true;
  end;
end;

function TPCanLight.SetCanVersion(const sver: string): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := IndexText(sver, CSTR_PCAN_VERSIONS);
  if ((i_idx >= Ord(Low(EPCanVersion))) and (i_idx <= Ord(High(EPCanVersion)))) then begin
    e_canver := EPCanVersion(i_idx);
    result := true;
  end;
end;

function TPCanLight.GetHardwareTypeText(): string;
begin
  result := CSTR_PCAN_HWTS[e_hwt];
end;

function TPCanLight.GetBaudrateText(): string;
begin
  result := CSTR_PCAN_BAUDRATES[e_baud];
end;

function TPCanLight.IsReadable(): boolean;
begin
  if (h_rcveve = 0) then result := false
  else result := (WaitForSingleObject(h_rcveve, 0) = WAIT_OBJECT_0);
end;

function TPCanLight.IsWriteComplete(): boolean;
begin
  result := (CanStatus() = CAN_ERR_OK);
end;

procedure TPCanLight.TryConnect();
var b_ok: boolean; lw_ret: longword;
begin
  if (e_hwt in [PCH_ISA1CH, PCH_ISA2CH, PCH_DNP, PCH_DNG]) then
    b_ok := (CanInitNPnP(CINT_PCAN_BAUDRATES[e_baud], Ord(e_canver), Ord(e_subtype), lw_ioport, w_irq) = CAN_ERR_OK)
  else
    b_ok := (CanInitPnP(CINT_PCAN_BAUDRATES[e_baud], Ord(e_canver)) = CAN_ERR_OK);

  //create event and worker-thread for reading data
  if b_ok then begin
    CanGetUsbDeviceNr(lw_devnr);
    if (h_rcveve = 0) then begin
      h_rcveve:= CreateEvent(nil,false,false,nil);
      b_ok := (h_rcveve <> 0);
      if b_ok then begin
        lw_ret:= CanSetRcvEvent(h_rcveve);
        b_ok := (lw_ret = CAN_ERR_OK);
        if b_ok then begin
          e_state := CS_CONNECTED;
          AddMessage(format('Succeed to connect to can adapter with number (=%x).', [lw_devnr]), ML_INFO);
        end else begin
          CanSetRcvEvent(0);
          CloseHandle(h_rcveve);
          h_rcveve := 0;
          AddMessage('Failed to set event for reading data.', ML_ERROR);
        end;
      end else AddMessage('Failed to create event for reading data.', ML_ERROR);
    end;
    if not assigned(t_thread) then t_thread := TPCanReadThread.Create(self);
  end else AddMessage('Failed to initialize the connection.', ML_ERROR);;
end;

procedure TPCanLight.SetMsgVersion(ecanver: EPCanVersion);
begin
  e_canver := ecanver;
  if (e_canver = PCV_STD) then t_wbuf.MSGTYPE := MSGTYPE_STANDARD
  else t_wbuf.MSGTYPE := MSGTYPE_EXTENDED;
end;

procedure TPCanLight.SetDeviceNr(devnr: longword);
begin
  CanSetUsbDeviceNr(devnr);
end;

function TPCanLight.CanInitPnP(wBR: Word; CANMsgType: Integer): longword;
begin
  //if IsValidFunction(PCF_INIT, result) then
  result := CAN_INIT_PNP(a_pcanfnt[PCF_INIT])(wBR, CANMsgType);
end;

function TPCanLight.CanInitNPnP(wBR: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord; Interupt: Word): longword;
begin
  //if IsValidFunction(PCF_INIT, result) then
  result := CAN_INIT_NPNP(a_pcanfnt[PCF_INIT])(wBR, CANMsgType, CANHwType, IO_Port, Interupt);
end;

function TPCanLight.CanClose(): longword;
begin
  if IsValidFunction(PCF_CLOSE, result) then
    result := CAN_CLOSE(a_pcanfnt[PCF_CLOSE])();
end;

function TPCanLight.CanStatus(): longword;
begin
  if IsValidFunction(PCF_STATUS, result) then
    result := CAN_STATUS(a_pcanfnt[PCF_STATUS])();
end;

function TPCanLight.CanWrite(var MsgBuff: TPCANMsg): longword;
begin
  //if IsValidFunction(PCF_WRITE, result) then begin
    result := CAN_WRITE(a_pcanfnt[PCF_WRITE])(MsgBuff);
    if (result = CAN_ERR_OK) then begin
      AddMessage('Sent: ' + BuildMessage(MsgBuff));
      Inc(lw_recvcnt);
    end else AddMessage('Error: ' + BuildMessage(MsgBuff));
  //end;
end;

function TPCanLight.CanRead(var MsgBuff: TPCANMsg): longword;
begin
  //if IsValidFunction(PCF_READ, result) then begin
    //todo: thread-safe??
    result := CAN_READ(a_pcanfnt[PCF_READ])(MsgBuff);
    if (result = CAN_ERR_BUSOFF) then begin // initilize the controller again if it has this error
      Connect();
      AddMessage('Reconnected by error: ' + BuildMessage(MsgBuff));
    end else if (result = CAN_ERR_OK) then begin
      Inc(lw_recvcnt);
      AddMessage('Received: ' + BuildMessage(MsgBuff));
    end else if (result <> CAN_ERR_QRCVEMPTY) then AddMessage('Error: ' + BuildMessage(MsgBuff));
  //end;
end;

function TPCanLight.CanReadEx(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp): longword;
begin
  //if IsValidFunction(PCF_READEX, result) then begin
    result := CAN_READEX(a_pcanfnt[PCF_READEX])(MsgBuff, RcvTime);
    if (result = CAN_ERR_BUSOFF) then begin // initilize the controller again if it has this error
      self.Connect();
      AddMessage('Reconnected by error: ' + BuildMessage(MsgBuff));
    end else if (result = CAN_ERR_OK) then begin
      Inc(lw_recvcnt);
      AddMessage('Received: ' + BuildMessage(MsgBuff));
    end else if (result <> CAN_ERR_QRCVEMPTY) then AddMessage('Error: ' + BuildMessage(MsgBuff));
  //end;
end;

function TPCanLight.CanVersionInfo(lpszTextBuff: PAnsiChar): longword;
begin
  if IsValidFunction(PCF_VERSIONINFO, result) then
    result := CAN_VERSIONINFO(a_pcanfnt[PCF_VERSIONINFO])(lpszTextBuff);
end;

function TPCanLight.CanDllVersionInfo(lpszTextBuff: PAnsiChar): longword;
begin
  if IsValidFunction(PCF_DLLVERSIONINFO, result) then
    result := CAN_DLLVERSIONINFO(a_pcanfnt[PCF_DLLVERSIONINFO])(lpszTextBuff);
end;

function TPCanLight.CanSpecialFunction(distributorcode: LongWord; codenumber: Integer): longword;
begin
  if IsValidFunction(PCF_SPECIALFUNCTION, result) then
    result := CAN_SPECIALFUNCTION(a_pcanfnt[PCF_SPECIALFUNCTION])(distributorcode, codenumber);
end;

function TPCanLight.CanResetClient(): longword;
begin
  if IsValidFunction(PCF_RESETCLIENT, result) then
    result := CAN_RESETCLIENT(a_pcanfnt[PCF_RESETCLIENT])();
end;

function TPCanLight.CanMsgFilter(FromID, ToID: LongWord; _Type: Integer): longword;
begin
  if IsValidFunction(PCF_MSGFILTER, result) then
    result := CAN_MSGFILTER(a_pcanfnt[PCF_MSGFILTER])(FromID, ToID, _Type);
end;

function TPCanLight.CanResetFilter(): longword;
begin
  if IsValidFunction(PCF_RESETFILTER, result) then
    result := CAN_RESETFILTER(a_pcanfnt[PCF_RESETFILTER])();
end;

function TPCanLight.CanSetRcvEvent(hEvent: THandle): longword;
begin
  if IsValidFunction(PCF_SETRCVEVENT, result) then
    result := CAN_SETRCVEVENT(a_pcanfnt[PCF_SETRCVEVENT])(hEvent);
end;

function TPCanLight.CanSetUsbDeviceNr(DevNum: LongWord): longword;
begin
  if IsValidFunction(PCF_SETUSBDEVICENR, result) then
    result := CAN_SETUSBDEVICENR(a_pcanfnt[PCF_SETUSBDEVICENR])(DevNum);
end;

function TPCanLight.CanGetUsbDeviceNr(var DevNum: LongWord): longword;
begin
  if IsValidFunction(PCF_GETUSBDEVICENR, result) then
    result := CAN_GETUSBDEVICENR(a_pcanfnt[PCF_GETUSBDEVICENR])(DevNum);
end;

constructor TPCanLight.Create(owner: TComponent);
var i: EPCanFunction;
begin
  inherited Create(owner);
  e_type := CT_CAN;
  e_hwt := PCH_NONE;
  e_baud := PCB_1M;
  h_dll := 0;
  s_dllfile := '';
  e_canver := PCV_STD;
  t_wbuf.MSGTYPE := MSGTYPE_STANDARD;
  lw_sendcnt := 0;
  lw_recvcnt := 0;
  for i := Low(EPCanFunction) to High(EPCanFunction) do a_pcanfnt[i] := nil;
  SetNPnP(HW_ISA, 0, 0);
end;

destructor TPCanLight.Destroy();
begin
  UnloadDll();
  inherited Destroy();
end;

function TPCanLight.SetDllFile(const sdll: string): boolean;
begin
  result := LoadDll(sdll);
end;

function TPCanLight.BuildMessage(const canmsg: TPCANMsg): string;
var i: integer; s_data: string;
begin
  s_data := '';
  for i := 0 to canmsg.LEN - 1 do s_data := s_data + format('%.2x', [canmsg.DATA[i]]);
  if (canmsg.MSGTYPE = MSGTYPE_EXTENDED) then result := format('%.8x:%s', [canmsg.ID, s_data])
  else result := format('%.3x:%s', [canmsg.ID, s_data]);
end;

function TPCanLight.Config(const sconfs: TStrings): boolean;
var i: EPCanProperty; s_conf: string;
begin
  result := false;
  if (e_state in [CS_UNKNOWN, CS_CONFIGURED]) then begin
    for i := LOW(EPCanProperty) to HIGH(EPCanProperty) do begin
      s_conf := sconfs.Values[CSTR_PCAN_PROPERTIES[i]];
      result := SetProperty(i, s_conf);
      if not result then break;
    end;
    if result then e_state := CS_CONFIGURED;
  end else AddMessage(format('The current state (%s) is not suitable for configuration.', [GetStateStr()]), ML_WARNING);
end;

function TPCanLight.Connect(): boolean;
begin
  result := false;
  if (e_state in [CS_CONFIGURED, CS_CONNECTED]) then
    result := WaitForConnecting(GetTickCount() + c_timeout)
  else AddMessage(format('The current state (%s) is not suitable to connect.', [GetStateStr()]), ML_WARNING);
end;

function TPCanLight.Disconnect: boolean;
var lw_ret: longword;
begin
  lw_ret := CanClose();
  result := (lw_ret = CAN_ERR_OK);
  lw_devnr := 0;
  if assigned(t_thread) then begin
    t_thread.Terminate;
    t_thread.WaitFor();
    FreeAndNil(t_thread);
  end;
  if (h_rcveve <> 0) then begin
      CloseHandle(h_rcveve);
      CanSetRcvEvent(0);
      h_rcveve := 0;
  end;
end;

function TPCanLight.SendBuf(const buf: PChar; const len: longword): boolean;
var lw_ret: longword;
begin
  result := false;
  if len = sizeof(TPCANMsg) then begin
    Move(buf, t_wbuf, len);
    lw_ret := CanWrite(t_wbuf);
    result := (lw_ret = CAN_ERR_OK);
    if result then result := WaitForWriting(GetTickCount() + c_timeout);
    if (not result) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
  end else AddMessage('The length of the buffer is not suitable for CAN-message.', ML_ERROR);
end;

function TPCanLight.RecvBuf(var buf: PChar; const len: longword): integer;
var lw_ret: longword;
begin
  result := 0;
  if len = sizeof(TPCANMsg) then begin
    if (WaitForReading(GetTickCount() + c_timeout)) then begin
      lw_ret := CanRead(t_rbuf);
      Move(t_rbuf, buf, len);
      result := len;
      if (lw_ret <> CAN_ERR_OK) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
    end;
  end else AddMessage('The length of the buffer is not suitable for CAN-message.', ML_ERROR);
end;

function TPCanLight.SendStr(const str: string): boolean;
var lw_ret: longword;
begin
  result := FillSendMsg(str);
  if result then begin
    lw_ret := CanWrite(t_wbuf);
    result := (lw_ret = CAN_ERR_OK);
    if result then result := WaitForWriting(GetTickCount() + c_timeout);
    if (not result) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
  end;
end;

function TPCanLight.RecvStr(var str: string; const bwait: boolean): integer;
var lw_ret: longword;
begin
  result := 0;
  if bwait then WaitForReading(GetTickCount() + c_timeout);
  lw_ret := CanRead(t_rbuf);
  if (lw_ret = CAN_ERR_OK) then begin
    BuildRecvMsg(str);
    result := t_rbuf.LEN;
  end else begin
    case lw_ret of
      CAN_ERR_QRCVEMPTY, CAN_ERR_QOVERRUN:
        AddMessage(GetErrorMsg(lw_ret), ML_INFO);
      else AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
    end;
  end;
end;

procedure TPCanReadThread.ReadCanMessage();
var t_canmsg: TPCANMsg; t_msgtime: TPCANTimestamp; lw_ret: longword;
begin
  if assigned(t_pcan) then begin
    lw_ret := CAN_ERR_QXMTFULL;
    while ((lw_ret AND CAN_ERR_QRCVEMPTY) <> CAN_ERR_QRCVEMPTY) do begin
      Application.ProcessMessages();
      lw_ret := t_pcan.CanReadEx(t_canmsg, t_msgtime);
    end;
  end;
end;

procedure TPCanReadThread.Execute;
begin
  while Not Terminated do begin
    if (t_pcan.RecvEvent <> 0) then begin
      Application.ProcessMessages();
      if (WaitForSingleObject(t_pcan.RecvEvent, 0) = WAIT_OBJECT_0) then
        Synchronize(ReadCanMessage);
    end;
  end;
end;

constructor TPCanReadThread.Create(pcan: TPCanLight);
begin
  t_pcan := pcan;
  inherited Create(False);
end;

destructor TPCanReadThread.Destroy();
begin
  if Not Terminated then Terminate();
end;

end.
