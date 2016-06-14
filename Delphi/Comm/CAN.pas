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
  //
  EPCanFunction =  (
              PCF_INIT,
              PCF_CLOSE,
              PCF_STATUS,
              PCF_WRITE,
              PCF_READ,
              PCF_READ_EX,
              PCF_VERSION_INFO,
              PCF_DLL_VERSION,
              PCF_SPECIAL_FUNC,
              PCF_RESET_CLIENT,
              PCF_MSG_FILTER,
              PCF_RESET_FILTER,
              PCF_SET_DEVICENR,
              PCF_GET_DEVICENR,
              PCF_SET_RCVEVENT
              );
  PCanFunctionNames = array[EPCanFunction] of string;

  EDllInterface = (
                DI_NONE,
                DI_CAN,
                DI_CAN2
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

  EPCanProperty = (
                PCP_DLL,
                PCP_BAUD
                );

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
    e_baud:     EPCanBaudrate;
    h_dll:      THandle;
    s_dllfile:  string;
    e_dllinterf:EDllInterface;
    t_rbuf, t_wbuf: TPCANMsg;
    lw_devnr:   longword;

  protected
    function LoadDll(const fncnames: PCanFunctionNames; const sdll: string): boolean; overload;
    function LoadDll(const sdll: string): boolean; overload;
    function LoadDefaultDll(const edll: EDllInterface): boolean;
    procedure UnloadDll();
    function GetErrorMsg(const errnr: longword): string;
    function FillSendMsg(const msg: string): boolean;
    function BuildRecvMsg(var msg: string): boolean;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function SetDllFile(const sdll: string): boolean;
    function SetBaudrate(const sbaud: string): boolean;
    function GetBaudrate(var sbaud: string): boolean;
    function GetDllInterface(var interf: string): boolean;

    function Config(const sconf: string): boolean; overload; override;
    function Config(const sconfs: TStrings): boolean; overload; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
    function SendPacket(const a: array of char): boolean; override;
    function RecvPacket(var a: array of char; const tend: cardinal): boolean; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = true): integer; override;
    function WaitForReading(const tend: cardinal): boolean; override;

  protected
    CanFunc: array[EPCanFunction] of Pointer;

    {Init:  CAN_INIT;
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
    SetRcvEvent:     CAN_SETRCVEVENT; }
  end;
  PConnPCanUsb = ^TConnPCanUsb;

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

  CSTR_PCAN_DLLINTERF: array[EDllInterface] of string = (
                'NONE',
                'PCAN_USB',
                'PCAN_2USB'
                );

  CSTR_DLL_FILENAMES: array[EDllInterface] of string = (
                '',
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

  C_PCAN_BAUDRATES: array[EPCanBaudrate] of Word = (
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

  CSTR_PCAN_BAUDNAMES: array[EPCanBaudrate] of string = (
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

  C_CANMSG_LENGTH_MAX = 8;

implementation
uses Windows, SysUtils, StrUtils, RegExpr, TextMessage, TypInfo;
type
  SetPCanProperty = function(ppcan: PConnPCanUsb; const sval: string): boolean;
const  CSTR_PCAN_KEYS: array[EPCanProperty] of string = (
                'PCANDLL',  //config name for the dll file
                'BAUDRATE'   //config name for the baudrate of can bus
                );
var PPCanPropertyCalls: array [EPCanProperty] of SetPCanProperty;

function SetPCanDLL(ppcan: PConnPCanUsb; const sdll: string): boolean;
begin
  result := ppcan^.SetDllFile(trim(sdll));
end;

function SetPCanBaudrate(ppcan: PConnPCanUsb; const sbaud: string): boolean;
begin
  result := ppcan^.SetBaudrate(trim(sbaud));;
end;

function TConnPCanUsb.LoadDll(const fncnames: PCanFunctionNames; const sdll: string): boolean;
var i: EPCanFunction;
begin
  try
    UnloadDll();
    h_dll := LoadLibrary(PChar(sdll));
    result := (h_dll <> 0);
    if result then begin
      for i := Low(EPCanFunction) to High(EPCanFunction) do begin
        CanFunc[i] := GetProcAddress(h_dll, PChar(fncnames[i]));
        result := assigned(CanFunc[i]);
        if (not result) then break;
      end;
      
      {Init := GetProcAddress(h_dll, PChar(fncnames[PCF_INIT]));
      Close := GetProcAddress(h_dll, PChar(fncnames[PCF_CLOSE]));
      Status := GetProcAddress(h_dll, PChar(fncnames[PCF_STATUS]));
      Write := GetProcAddress(h_dll, PChar(fncnames[PCF_WRITE]));
      Read := GetProcAddress(h_dll, PChar(fncnames[PCF_READ]));
      ReadEx := GetProcAddress(h_dll, PChar(fncnames[PCF_READ_EX]));
      VersionInfo := GetProcAddress(h_dll, PChar(fncnames[PCF_VERSION_INFO]));
      DllVersionInfo := GetProcAddress(h_dll, PChar(fncnames[PCF_DLL_VERSION]));
      SpecialFunction := GetProcAddress(h_dll, PChar(fncnames[PCF_SPECIAL_FUNC]));
      ResetClient := GetProcAddress(h_dll, PChar(fncnames[PCF_RESET_CLIENT]));
      MsgFilter := GetProcAddress(h_dll, PChar(fncnames[PCF_MSG_FILTER]));
      ResetFilter := GetProcAddress(h_dll, PChar(fncnames[PCF_RESET_FILTER]));
      SetUsbDeviceNr := GetProcAddress(h_dll, PChar(fncnames[PCF_SET_DEVICENR]));
      GetUsbDeviceNr := GetProcAddress(h_dll, PChar(fncnames[PCF_GET_DEVICENR]));
      SetRcvEvent := GetProcAddress(h_dll, PChar(fncnames[PCF_SET_RCVEVENT]));
      
      result := (assigned(Init) and assigned(Close) and assigned(Status) and
                assigned(Write) and assigned(Read) and assigned(ReadEx) and
                assigned(VersionInfo) and assigned(DllVersionInfo) and assigned(SpecialFunction) and
                assigned(ResetClient) and assigned(MsgFilter) and assigned(ResetFilter) and
                assigned(SetUsbDeviceNr) and assigned(GetUsbDeviceNr) and assigned(SetRcvEvent)
                ); }
      if (not result) then UnloadDll();
    end;
  except
    UnloadDll();
    result := false;
  end;
end;

function TConnPCanUsb.LoadDll(const sdll: string): boolean;
begin
  result := LoadDll(CSTR_CANUSB1_NAMES, sdll);
  if result then begin
    e_dllinterf := DI_CAN;
    s_dllfile := sdll;
  end else begin
    result := LoadDll(CSTR_CANUSB2_NAMES, sdll);
    if result then begin
      e_dllinterf := DI_CAN2;
      s_dllfile := sdll;
    end;
  end;
  if result then AddMessage(format('%s is loaded for [%s], successfully', [sdll, GetEnumName(TypeInfo(EDllInterface), integer(e_dllinterf))]))
  else AddMessage(format('Failed to load: %s', [sdll]), ML_ERROR);
end;

function TConnPCanUsb.LoadDefaultDll(const edll: EDllInterface): boolean;
begin
  result := false;
  case edll of
  DI_CAN: result := LoadDll(CSTR_CANUSB1_NAMES, CSTR_DLL_FILENAMES[edll]);
  DI_CAN2: result := LoadDll(CSTR_CANUSB2_NAMES, CSTR_DLL_FILENAMES[edll]);
  else
  end;
  if result then AddMessage(format('Succeeded to load default dll: %s for [%s]', [CSTR_DLL_FILENAMES[edll], GetEnumName(TypeInfo(EDllInterface), integer(edll))]))
  else AddMessage(format('Failed to load default dll: %s', [CSTR_DLL_FILENAMES[edll]]), ML_ERROR);
end;

procedure TConnPCanUsb.UnloadDll();
var i: EPCanFunction;
begin
  if (h_dll <> 0) then begin
    Disconnect();
    if FreeLibrary(h_dll) then begin
      s_dllfile := '';
      e_dllinterf := DI_NONE;
      h_dll := 0;
      for i := Low(EPCanFunction) to High(EPCanFunction) do CanFunc[i] := nil;
      {Init := nil;
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
      SetRcvEvent := nil;}
    end;
  end;
end;

function TConnPCanUsb.GetErrorMsg(const errnr: longword): string;
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
  else                  result := format('Error number (%d) is unknown', [errnr]);
  end;
end;

//msg is a formatted string and looks like '58A:2038F612' with hexidecimal format 'can_nodenr:data'
//note: byte order have to convert to big-endien before sending
function TConnPCanUsb.FillSendMsg(const msg: string): boolean;
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
        t_wbuf.MSGTYPE := MSGTYPE_STANDARD;
        t_wbuf.LEN := byte(i_len);
      end;
    end;
  end;
end;

function TConnPCanUsb.BuildRecvMsg(var msg: string): boolean;
var i: integer; s_data: string;
begin
  s_data := '';
  for i := 0 to t_rbuf.LEN - 1 do s_data := s_data + format('%.2x', [t_rbuf.DATA[i]]);
  msg := format('%.3x:%s', [t_rbuf.ID, s_data]);
  result := true;
end;

constructor TConnPCanUsb.Create(owner: TComponent);
var i: EPCanFunction;
begin
  inherited Create(owner);
  s_dllfile := '';
  e_dllinterf := DI_NONE;
  e_baud := PCB_500K;
  h_dll := 0;
  for i := Low(EPCanFunction) to High(EPCanFunction) do CanFunc[i] := nil;
  {Init := nil;
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
  SetRcvEvent := nil; }
  t_wbuf.MSGTYPE := MSGTYPE_STANDARD;
  e_type := CT_CAN;
end;

destructor TConnPCanUsb.Destroy;
begin
  UnloadDll();
  inherited Destroy();
end;

function TConnPCanUsb.SetDllFile(const sdll: string): boolean;
begin
  result := LoadDll(sdll);
end;

function TConnPCanUsb.SetBaudrate(const sbaud: string): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := IndexText(sbaud, CSTR_PCAN_BAUDNAMES);
  if ((i_idx >= Ord(Low(EPCanBaudrate))) and (i_idx <= Ord(High(EPCanBaudrate)))) then begin
    e_baud := EPCanBaudrate(i_idx);
    result := true;
  end;
end;

function TConnPCanUsb.GetBaudrate(var sbaud: string): boolean;
begin
  sbaud := CSTR_PCAN_BAUDNAMES[e_baud];
  result := true;
end;

function TConnPCanUsb.GetDllInterface(var interf: string): boolean;
begin
  interf := CSTR_PCAN_DLLINTERF[e_dllinterf];
  result := true;
end;

function TConnPCanUsb.Config(const sconf: string): boolean;
var s_conf: string; i: EPCanProperty; t_regexp: TRegExpr;
begin
  result := false;
  s_conf := UpperCase(sconf);
  t_regexp := TRegExpr.Create;
  for i := LOW(EPCanProperty) to HIGH(EPCanProperty) do begin
    t_regexp.Expression := '(^|\|)[\t\ ]*' + CSTR_PCAN_KEYS[i] + '\b[\t\ ]*:([^\|$]*)';
    if t_regexp.Exec(s_conf) then  begin
      result := (PPCANPropertyCalls[i](@self, t_regexp.Match[2]));
      if not result then break;
    end;
  end;
  FreeAndNil(t_regexp);
  if result then e_state := CS_CONFIGURED;
end;

function TConnPCanUsb.Config(const sconfs: TStrings): boolean;
var t_confs: TStringList; i: EPCanProperty; s_conf: string; i_idx: integer;
begin
  result := false;
  t_confs := TStringList.Create();
  t_confs.Sorted := true;
  t_confs.CaseSensitive := false;
  t_confs.AddStrings(sconfs);
  for i := LOW(EPCanProperty) to HIGH(EPCanProperty) do begin
    if t_confs.Find(CSTR_PCAN_KEYS[i], i_idx) then begin
      s_conf := t_confs.ValueFromIndex[i_idx];
      result := (PPCanPropertyCalls[i](@self, s_conf));
      if not result then break;
    end;
  end;
  FreeAndNil(t_confs);
  if result then e_state := CS_CONFIGURED;
end;

function TConnPCanUsb.Connect(): boolean;
var lw_ret: longword;
begin
  //lw_ret := Init(C_PCAN_BAUDRATES[e_baud], CAN_INIT_TYPE_ST);
  lw_ret := CAN_INIT(CanFunc[PCF_INIT])(C_PCAN_BAUDRATES[e_baud], CAN_INIT_TYPE_ST);
  result := (lw_ret = CAN_ERR_OK);
  //if result then GetUsbDeviceNr(lw_devnr)
  if result then CAN_GETUSBDEVICENR(CanFunc[PCF_GET_DEVICENR])(lw_devnr)
  else AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
end;

function TConnPCanUsb.Disconnect: boolean;
var lw_ret: longword;
begin
  //lw_ret := self.Close();
  lw_ret := CAN_CLOSE(CanFunc[PCF_CLOSE])();
  lw_devnr := 0;
  result := (lw_ret = CAN_ERR_OK);
end;

function TConnPCanUsb.SendPacket(const a: array of char): boolean;
var lw_ret: longword;
begin
  result := false;
  if length(a) >= sizeof(t_wbuf) then begin
    Move(a, t_wbuf, sizeof(t_wbuf));
    //lw_ret := self.Write(t_wbuf);
    lw_ret := CAN_WRITE(CanFunc[PCF_WRITE])(t_wbuf);
    result := (lw_ret = CAN_ERR_OK);
    if (not result) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
  end else AddMessage('The size of the array is not suitable for CAN-message.', ML_ERROR);
end;

function TConnPCanUsb.RecvPacket(var a: array of char; const tend: cardinal): boolean;
var lw_ret: longword;
begin
  result := false;
  if length(a) >= sizeof(t_rbuf) then begin
    //lw_ret := self.Read(t_rbuf);
    lw_ret := CAN_READ(CanFunc[PCF_READ])(t_rbuf);
    Move(t_rbuf, a, sizeof(t_rbuf));
    result := (lw_ret = CAN_ERR_OK);
    if (not result) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
  end else AddMessage('The size of the array is not suitable for CAN-message.', ML_ERROR);
end;

function TConnPCanUsb.SendStr(const str: string): boolean;
var lw_ret: longword;
begin
  result := FillSendMsg(str);
  if result then begin
    //lw_ret := self.Write(t_wbuf);
    lw_ret := CAN_WRITE(CanFunc[PCF_WRITE])(t_wbuf);
    result := (lw_ret = CAN_ERR_OK);
    if (not result) then AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
  end;
end;

function TConnPCanUsb.RecvStr(var str: string; const bwait: boolean): integer;
var lw_ret: longword;
begin
  result := 0;
  if bwait then WaitForReading(GetTickCount() + i_timeout);

  //lw_ret := self.Read(t_rbuf);
  lw_ret := CAN_READ(CanFunc[PCF_READ])(t_rbuf);
  if (lw_ret = CAN_ERR_OK) then begin
    BuildRecvMsg(str);
    result := t_rbuf.LEN;
  end else AddMessage(GetErrorMsg(lw_ret), ML_ERROR);
end;

function TConnPCanUsb.WaitForReading(const tend: cardinal): boolean;
begin
  result := true;
  //todo:
end;

initialization
  PPCanPropertyCalls[PCP_DLL] := SetPCanDll;
  PPCanPropertyCalls[PCP_BAUD]:= SetPCanBaudrate;
end.
