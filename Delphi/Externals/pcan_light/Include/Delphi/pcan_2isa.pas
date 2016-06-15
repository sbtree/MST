///////////////////////////////////////////////////////////////////////////////
//  PCAN-Light
//  PCAN-2ISA.pas
//
//  Version 2.x
//
//  ~~~~~~~~~~
//
//  Definition of the PCAN-Light API. 
//  The driver supports a Hardware and a Software that wants to communicate with CAN-busses 
//
//  ~~~~~~~~~~~~
//
//  PCAN-Light-API
//
//  ~~~~~~~~~~~~
//
//  - CAN2_Init(wBTR0BTR1: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord; Interupt: Word)
//  - CAN2_Close()
//  - CAN2_Status()
//  - CAN2_Write(var MsgBuff: TPCANMsg)
//  - CAN2_Read(var MsgBuff: TPCANMsg)
//  - CAN2_ReadEx(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp)
//  - CAN2_VersionInfo(lpszTextBuff: PAnsiChar)
//  - CAN2_DLLVersionInfo(lpszTextBuff: PAnsiChar)
//  - CAN2_ResetClient()
//  - CAN2_MsgFilter(FromID, ToID: LongWord; _Type: Integer)
//  - CAN2_ResetFilter()
//  - CAN2_SetRcvEvent(hEvent: THandle)
//
//  ------------------------------------------------------------------
//  Author : Hoppe, Wilhelm
//  Modified By: Wolf (15.03.2011)
//
//  Language: Delphi
//  ------------------------------------------------------------------
//
//  Copyright (C) 1999-2011  PEAK-System Technik GmbH, Darmstadt
//
unit pcan_2isa;

interface

const
    // Hardware Type
    //
    HW_ISA          = 1;
    HW_ISA_SJA      = 9;

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

type
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
    
///////////////////////////////////////////////////////////////////////////////
//  CAN2_Init()
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
//  CANHw Type:  (Only new SJA Dongle !!!)
//  HW_ISA             1
//  HW_DONGLE_SJA      5
//  HW_DONGLE_SJA_EPP  6
//  HW_DONGLE_PRO      7
//  HW_DONGLE_PRO_EPP  8
//  HW_ISA_SJA         9
//  HW_PCI             10
//
//  Possible Errors: NOVXD ILLHW REGTEST RESOURCE
//
function CAN2_Init(wBTR0BTR1: Word;
        CANMsgType: Integer;
        CANHwType: Integer;
        IO_Port: LongWord;
        Interupt: Word): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_Close()
//  This function terminate and release the configured hardware and all 
//  allocated resources
//
//  Possible Errors: NOVXD
//
function CAN2_Close: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_Status()
//  This function request the current status of the hardware (b.e. BUS-OFF)
//
//  Possible Errors: NOVXD BUSOFF BUSHEAVY OVERRUN
//
function CAN2_Status: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_Write()
//  This function Place a CAN message into the Transmit Queue of the CAN Hardware
//
//  Possible Errors: NOVXD RESOURCE BUSOFF QXMTFULL
//
function CAN2_Write(var MsgBuff: TPCANMsg): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_Read()
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
function CAN2_Read(var MsgBuff: TPCANMsg): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_ReadEx()
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
function CAN2_ReadEx(
        var MsgBuff: TPCANMsg; 
        var RcvTime: TPCANTimestamp
        ): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_VersionInfo()
//  This function get the Version and copyright of the hardware as text
//  (max. 255 characters)
//
//  Possible Errors:  NOVXD
//
function CAN2_VersionInfo(
        lpszTextBuff: PAnsiChar
        ): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_DLLVersionInfo()
//  This function is used to get the Version and copyright of the DLL as 
//  text (max. 255 characters)
//
//  Possible Errors: -1 for NULL-Pointer parameters :-)
//
function CAN2_DLLVersionInfo(
        lpszTextBuff: PAnsiChar
        ): LongWord; stdcall;
                    
//////////////////////////////////////////////////////////////////////////////
//  CAN2_ResetClient()
//  This function delete the both queues (Transmit,Receive) of the CAN Controller 
//  using a RESET
//
//  Possible Errors: ERR_ILLCLIENT ERR_NOVXD
//
function CAN2_ResetClient: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_MsgFilter(FromID, ToID, int Type)
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
function CAN2_MsgFilter(
        FromID, ToID: LongWord;
        _Type: Integer
        ): LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_ResetFilter()
//  This function close completely the Message Filter of the Hardware.
//  They will be no more messages received.
//
//  Possible Errors: NOVXD
//
function CAN2_ResetFilter: LongWord; stdcall;

///////////////////////////////////////////////////////////////////////////////
//  CAN2_SetRcvEvent()
//  This function is used to set the Event for the Event Handler
//
//  Possible Errors: ILLCLIENT ILLPARAMTYPE ILLPARAMVAL NOVXD
//
function CAN2_SetRcvEvent(hEvent: THandle): LongWord; stdcall;


implementation

uses SysUtils;

const DLL_Name = 'PCAN_2ISA.dll';

function CAN2_Init(wBTR0BTR1: Word; CANMsgType: Integer; CANHwType: Integer; IO_Port: LongWord;Interupt: Word): LongWord; stdcall;
external DLL_Name;

function CAN2_Close: LongWord; stdcall;
external DLL_Name;

function CAN2_Status: LongWord; stdcall;
external DLL_Name;

function CAN2_Write(var MsgBuff: TPCANMsg): LongWord; stdcall;
external DLL_Name;

function CAN2_Read(var MsgBuff: TPCANMsg): LongWord; stdcall;
external DLL_Name;

function CAN2_ReadEx(var MsgBuff: TPCANMsg; var RcvTime: TPCANTimestamp): LongWord; stdcall;
external DLL_NAME;

function CAN2_VersionInfo(lpszTextBuff: PAnsiChar): LongWord; stdcall;
external DLL_Name;

function CAN2_DLLVersionInfo(lpszTextBuff: PAnsiChar): LongWord; stdcall;
external DLL_Name;

function CAN2_ResetClient: LongWord; stdcall;
external DLL_Name;

function CAN2_MsgFilter(FromID, ToID: LongWord; _Type: Integer): LongWord; stdcall;
external DLL_Name;

function CAN2_ResetFilter: LongWord; stdcall;
external DLL_Name;

function CAN2_SetRcvEvent(hEvent: THandle): LongWord; stdcall;
external DLL_Name;

end.
