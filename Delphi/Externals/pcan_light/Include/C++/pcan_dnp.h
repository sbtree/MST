///////////////////////////////////////////////////////////////////////////////
//	PCAN-Light
//  PCAN-DNP.H
//
//  Version 2.x
//
//  ~~~~~~~~~~
//
//  Definition of the PCAN-Light API. 
//	The Driver support a Hardware and a Software who want to communicate with CAN-busses 
//
//  ~~~~~~~~~~~~
//
//  PCAN-Light -API
//
//  ~~~~~~~~~~~~
//
//  - CAN_Init(WORD wBTR0BTR1, int CANMsgType, int CANHwType, DWORD IO_Port, WORD Interupt)
//  - CAN_Close(void)  
//  - CAN_Status(void) 
//  - CAN_Write(TPCANMsg* pMsgBuff) 
//  - CAN_Read(TPCANMsg* pMsgBuff)  
//  - CAN_ReadEx(TPCANMsg* pMsgBuff, TPCANTimestamp*  pRcvTime)
//  - CAN_VersionInfo(LPSTR lpszTextBuff) 
//  - CAN_DLLVersionInfo(LPSTR lpszTextBuff) 
//  - CAN_MsgFilter(DWORD FromID,DWORD ToID, int Type) 
//  - CAN_ResetFilter(void) 
//  - CAN_ResetClient(void) 
//  - CAN_SetRcvEvent(HANDLE hEvent)
//
//  ------------------------------------------------------------------
//  Author : Hoppe, Wilhelm
//  Modified By: Wagner (20.08.2008)
//	Last change: 28.09.2009 Wagner
//
//  Language: ANSI-C
//  ------------------------------------------------------------------
//
//  Copyright (C) 1999-2009  PEAK-System Technik GmbH, Darmstadt
//  more Info at http://www.peak-system.com 
//
#ifndef __PCANDNPH__       
#define __PCANDNPH__

#pragma region Frames, IDs and CAN Message types
// Constants definitions - Frame Type 
//
#define CAN_INIT_TYPE_EX		0x01	//Extended Frame
#define CAN_INIT_TYPE_ST		0x00	//Standart Frame 

// Constants definitions - ID
//
#define CAN_MAX_STANDARD_ID     0x7ff
#define CAN_MAX_EXTENDED_ID     0x1fffffff

// Constants definitions  - CAN message types
//
#define MSGTYPE_STANDARD 0x00   // Standard Data frame (11-bit ID)
#define MSGTYPE_RTR      0x01   // 1, if Remote Request frame
#define MSGTYPE_EXTENDED 0x02   // 1, if Extended Data frame (CAN 2.0B, 29-bit ID)
#define MSGTYPE_STATUS   0x80   // 1, if Status information
#pragma endregion

#pragma region Baurate Codes
// Baud rate codes = BTR0/BTR1 register values for the CAN controller.
// You can define your own Baudrate with the BTROBTR1 register !!
// take a look at www.peak-system.com for our software BAUDTOOL to
// calculate the BTROBTR1 register for every baudrate and sample point.
//
#define CAN_BAUD_1M     0x0014  //   1 MBit/s
#define CAN_BAUD_500K   0x001C  // 500 kBit/s
#define CAN_BAUD_250K   0x011C  // 250 kBit/s
#define CAN_BAUD_125K   0x031C  // 125 kBit/s
#define CAN_BAUD_100K   0x432F  // 100 kBit/s
#define CAN_BAUD_50K    0x472F  //  50 kBit/s
#define CAN_BAUD_20K    0x532F  //  20 kBit/s
#define CAN_BAUD_10K    0x672F  //  10 kBit/s
#define CAN_BAUD_5K     0x7F7F  //   5 kBit/s
#pragma endregion

#pragma region Error Codes
// Error codes (bit code) 
// Represent the PCAN error and status codes 
//
#define CAN_ERR_OK            0x00000  // No error 
#define CAN_ERR_XMTFULL       0x00001  // Transmit buffer in CAN controller is full
#define CAN_ERR_OVERRUN       0x00002  // CAN controller was read too late
#define CAN_ERR_BUSLIGHT      0x00004  // Bus error: an error counter reached the 'light' limit
#define CAN_ERR_BUSHEAVY      0x00008  // Bus error: an error counter reached the 'heavy' limit
#define CAN_ERR_BUSOFF        0x00010  // Bus error: the CAN controller is in bus-off state
#define CAN_ERR_ANYBUSERR     (CAN_ERR_BUSLIGHT | CAN_ERR_BUSHEAVY | CAN_ERR_BUSOFF) // Mask for all bus errors
#define CAN_ERR_QRCVEMPTY     0x00020  // Receive queue is empty
#define CAN_ERR_QOVERRUN      0x00040  // Receive queue was read too late
#define CAN_ERR_QXMTFULL      0x00080  // Transmit queue is full
#define CAN_ERR_REGTEST       0x00100  // Test of the CAN controller hardware registers failed (no hardware found)
#define CAN_ERR_NODRIVER      0x00200  // Driver not loaded
#define CAN_ERR_HWINUSE       0x00400  // Hardware already in use by a Net
#define CAN_ERR_NETINUSE      0x00800  // A Client is already connected to the Net
#define CAN_ERR_ILLHW         0x01400  // Hardware handle is invalid
#define CAN_ERR_ILLNET        0x01800  // Net handle is invalid
#define CAN_ERR_ILLCLIENT     0x01C00  // Client handle is invalid
#define CAN_ERR_ILLHANDLE     (CAN_ERR_ILLHW | CAN_ERR_ILLNET | CAN_ERR_ILLCLIENT)  // Mask for all handle errors
#define CAN_ERR_RESOURCE      0x02000  // Resource (FIFO, Client, timeout) cannot be created
#define CAN_ERR_ILLPARAMTYPE  0x04000  // Invalid parameter
#define CAN_ERR_ILLPARAMVAL   0x08000  // Invalid parameter value
#define CAN_ERR_UNKNOWN       0x10000  // Unknow error
// All further error conditions <> 0 please ask PEAK when required.......internal driver failure ........
#pragma endregion

#pragma region Structures
// CAN Message
//
typedef struct {
    DWORD ID;        // 11/29 bit identifier
    BYTE  MSGTYPE;   // Bits from MSGTYPE_*
    BYTE  LEN;       // Data Length Code of the Msg (0..8)
    BYTE  DATA[8];   // Data 0 .. 7
} TPCANMsg;

// Timestamp of a receive/transmit event
// Total microseconds = micros + 1000 * millis + 0xFFFFFFFF * 1000 * millis_overflow
//
typedef struct 
{
    DWORD millis;          // Base-value: milliseconds: 0.. 2^32-1
    WORD  millis_overflow; // Roll-arounds of millis
    WORD  micros;          // Microseconds: 0..999
} TPCANTimestamp;
#pragma endregion

#ifdef NTVERSION
   #pragma pack(push, 1)    // Byte aligment (MS Visual C++)
#endif

#ifdef NTVERSION
   #pragma pack(pop)        // Default alignment again
#endif

#ifdef __cplusplus
  extern "C" {
#endif

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
DWORD __stdcall CAN_Init(WORD wBTR0BTR1, int CANMsgType, int CANHwType, DWORD IO_Port, WORD Interupt);        

///////////////////////////////////////////////////////////////////////////////
//  CAN_Close()
//  This function terminate and release the configured hardware and all 
//  allocated resources
//
//  Possible Errors: NOVXD
//
DWORD __stdcall CAN_Close(void);

///////////////////////////////////////////////////////////////////////////////
//  CAN_Status()
//  This function request the current status of the hardware (b.e. BUS-OFF)
//
//  Possible Errors: NOVXD BUSOFF BUSHEAVY OVERRUN
//
DWORD __stdcall CAN_Status(void);

///////////////////////////////////////////////////////////////////////////////
//  CAN_Write()
//  This function Place a CAN message into the Transmit Queue of the CAN Hardware
//
//  Possible Errors: NOVXD RESOURCE BUSOFF QXMTFULL
//
DWORD __stdcall CAN_Write(TPCANMsg* pMsgBuff);

///////////////////////////////////////////////////////////////////////////////
//  CAN_Read()
//  This function get the next message or the next error from the Receive Queue of 
//  the CAN Hardware.  
//  REMARK:
//		- Check always the type of the received Message (MSGTYPE_STANDARD,MSGTYPE_RTR,
//		  MSGTYPE_EXTENDED,MSGTYPE_STATUS)
//		- The function will return ERR_OK always that you receive a CAN message successfully 
//		  although if the messages is a MSGTYPE_STATUS message.  
//		- When a MSGTYPE_STATUS mesasge is got, the ID and Length information of the message 
//		  will be treated as indefined values. Actually information of the received message
//		  should be interpreted using the first 4 data bytes as follow:
//			*	Data0	Data1	Data2	Data3	Kind of Error
//				0x00	0x00	0x00	0x02	CAN_ERR_OVERRUN		0x0002	CAN Controller was read to late
//				0x00	0x00	0x00	0x04	CAN_ERR_BUSLIGHT	0x0004  Bus Error: An error counter limit reached (96)
//				0x00	0x00	0x00	0x08	CAN_ERR_BUSHEAVY	0x0008	Bus Error: An error counter limit reached (128)
//				0x00	0x00	0x00	0x10	CAN_ERR_BUSOFF		0x0010	Bus Error: Can Controller went "Bus-Off"
//		- If a CAN_ERR_BUSOFF status message is received, the CAN Controller must to be 
//		  initialized again using the Init() function.  Otherwise, will be not possible 
//		  to send/receive more messages. 
//		- The message will be written to 'msgbuff'.
//
//  Possible Errors: NOVXD  QRCVEMPTY
//
DWORD __stdcall CAN_Read(TPCANMsg* pMsgBuff);      

///////////////////////////////////////////////////////////////////////////////
//  CAN_ReadEx()
//  This function get the next message or the next error from the Receive Queue of 
//  the CAN Hardware and the time when the message arrived.  
//  REMARK:
//		- Check always the type of the received Message (MSGTYPE_STANDARD,MSGTYPE_RTR,
//		  MSGTYPE_EXTENDED,MSGTYPE_STATUS)
//		- The function will return ERR_OK always that you receive a CAN message successfully 
//		  although if the messages is a MSGTYPE_STATUS message.  
//		- When a MSGTYPE_STATUS mesasge is got, the ID and Length information of the message 
//		  will be treated as indefined values. Actually information of the received message
//		  should be interpreted using the first 4 data bytes as follow:
//			*	Data0	Data1	Data2	Data3	Kind of Error
//				0x00	0x00	0x00	0x02	CAN_ERR_OVERRUN		0x0002	CAN Controller was read to late
//				0x00	0x00	0x00	0x04	CAN_ERR_BUSLIGHT	0x0004  Bus Error: An error counter limit reached (96)
//				0x00	0x00	0x00	0x08	CAN_ERR_BUSHEAVY	0x0008	Bus Error: An error counter limit reached (128)
//				0x00	0x00	0x00	0x10	CAN_ERR_BUSOFF		0x0010	Bus Error: Can Controller went "Bus-Off"
//		- If a CAN_ERR_BUSOFF status message is received, the CAN Controller must to be 
//		  initialized again using the Init() function.  Otherwise, will be not possible 
//		  to send/receive more messages. 
//		- The message will be written to 'msgbuff'.
//		Since Version 2.x the Ext. Version is available - new Parameter:
//		-  Receive timestamp
//
//  Possible Errors: NOVXD  QRCVEMPTY
//
DWORD __stdcall CAN_ReadEx(TPCANMsg* pMsgBuff, TPCANTimestamp*  pRcvTime);            

///////////////////////////////////////////////////////////////////////////////
//  CAN_VersionInfo()
//  This function get the Version and copyright of the hardware as text 
//  (max. 255 characters)
//
//  Possible Errors:  NOVXD
//
DWORD __stdcall CAN_VersionInfo(LPSTR lpszTextBuff);

///////////////////////////////////////////////////////////////////////////////
//  CAN_DLLVersionInfo()
//  This function is used to get the Version and copyright of the DLL as 
//  text (max. 255 characters)
//
//  Possible Errors: -1 for NULL-Pointer parameters :-)
//
DWORD __stdcall CAN_DLLVersionInfo(LPSTR lpszTextBuff);

//////////////////////////////////////////////////////////////////////////////
//  CAN_ResetClient()
//  This function delete the both queues (Transmit,Receive) of the CAN Controller 
//  using a RESET
//
//  Possible Errors: ERR_ILLCLIENT ERR_NOVXD
//
DWORD __stdcall CAN_ResetClient(void);

///////////////////////////////////////////////////////////////////////////////
//  CAN_MsgFilter(FromID, ToID, int Type)
//  This function set the receive message filter of the CAN Controller.
//  REMARK:
//		- A quick register of all messages is possible using the parameters FromID and ToID = 0
//		- Every call of this function maybe cause an extention of the receive filter of the 
//		  CAN controller, which one can go briefly to RESET
//		- New in Ver 2.x:
//			* Standard frames will be put it down in the acc_mask/code as Bits 28..13
//			* Hardware driver for 82C200 must to be moved to Bits 10..0 again!
//	WARNING: 
//		It is not guaranteed to receive ONLY the registered messages.
//
//  Possible Errors: NOVXD ILLCLIENT ILLNET REGTEST
//
DWORD __stdcall CAN_MsgFilter(DWORD FromID, DWORD ToID, int Type);

///////////////////////////////////////////////////////////////////////////////
//  CAN_ResetFilter()
//  This function close completely the Message Filter of the Hardware.
//  They will be no more messages received.
//
//  Possible Errors: NOVXD
//
DWORD __stdcall CAN_ResetFilter(void);

///////////////////////////////////////////////////////////////////////////////
//  CAN2_SetRcvEvent()
//  This function is used to set the Event for the Event Handler
//
//  Possible Errors: ILLCLIENT ILLPARAMTYPE ILLPARAMVAL NOVXD
//
DWORD __stdcall CAN_SetRcvEvent(HANDLE hEvent);

#ifdef __cplusplus
}
#endif
#endif 