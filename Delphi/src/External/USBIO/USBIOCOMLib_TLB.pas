unit USBIOCOMLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision: 1.1 $
// File generated on 03.08.2005 16:40:28 from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: W:\_Thesycon\UsbIo\bin\release\USBIOCOM.dll (1)
// IID\LCID: {F95AB400-6BA5-44DD-BF19-AAB5AD44CD65}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Hint: Parameter 'Type' of IUSBIOInterface3.ClassOrVendorInRequest changed to 'Type_'
//   Hint: Parameter 'Type' of IUSBIOInterface3.ClassOrVendorOutRequest changed to 'Type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  USBIOCOMLibMajorVersion = 3;
  USBIOCOMLibMinorVersion = 0;

  LIBID_USBIOCOMLib: TGUID = '{F95AB400-6BA5-44DD-BF19-AAB5AD44CD65}';

  DIID__IUSBIOInterfaceEvents3: TGUID = '{3C83A43F-CFD4-4781-8949-04EDDBD3E191}';
  IID_IUSBIOInterface3: TGUID = '{775938FE-790A-476E-BA6D-20BE61EFE5BC}';
  CLASS_USBIOInterface3: TGUID = '{B949A098-6A08-4EEE-B32D-CA0D19BBF24E}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum _USBIOCOM_ERR_CODES
type
  _USBIOCOM_ERR_CODES = TOleEnum;
const
  USBIO_ERR_SUCCESS = $00000000;
  USBIO_ERR_CRC = $E0000001;
  USBIO_ERR_BTSTUFF = $E0000002;
  USBIO_ERR_DATA_TOGGLE_MISMATCH = $E0000003;
  USBIO_ERR_STALL_PID = $E0000004;
  USBIO_ERR_DEV_NOT_RESPONDING = $E0000005;
  USBIO_ERR_PID_CHECK_FAILURE = $E0000006;
  USBIO_ERR_UNEXPECTED_PID = $E0000007;
  USBIO_ERR_DATA_OVERRUN = $E0000008;
  USBIO_ERR_DATA_UNDERRUN = $E0000009;
  USBIO_ERR_RESERVED1 = $E000000A;
  USBIO_ERR_RESERVED2 = $E000000B;
  USBIO_ERR_BUFFER_OVERRUN = $E000000C;
  USBIO_ERR_BUFFER_UNDERRUN = $E000000D;
  USBIO_ERR_NOT_ACCESSED = $E000000F;
  USBIO_ERR_FIFO = $E0000010;
  USBIO_ERR_XACT_ERROR = $E0000011;
  USBIO_ERR_BABBLE_DETECTED = $E0000012;
  USBIO_ERR_DATA_BUFFER_ERROR = $E0000013;
  USBIO_ERR_ENDPOINT_HALTED = $E0000030;
  USBIO_ERR_NO_MEMORY = $E0000100;
  USBIO_ERR_INVALID_URB_FUNCTION = $E0000200;
  USBIO_ERR_INVALID_PARAMETER = $E0000300;
  USBIO_ERR_ERROR_BUSY = $E0000400;
  USBIO_ERR_REQUEST_FAILED = $E0000500;
  USBIO_ERR_INVALID_PIPE_HANDLE = $E0000600;
  USBIO_ERR_NO_BANDWIDTH = $E0000700;
  USBIO_ERR_INTERNAL_HC_ERROR = $E0000800;
  USBIO_ERR_ERROR_SHORT_TRANSFER = $E0000900;
  USBIO_ERR_BAD_START_FRAME = $E0000A00;
  USBIO_ERR_ISOCH_REQUEST_FAILED = $E0000B00;
  USBIO_ERR_FRAME_CONTROL_OWNED = $E0000C00;
  USBIO_ERR_FRAME_CONTROL_NOT_OWNED = $E0000D00;
  USBIO_ERR_NOT_SUPPORTED = $E0000E00;
  USBIO_ERR_INVALID_CONFIGURATION_DESCRIPTOR = $E0000F00;
  USBIO_ERR_INSUFFICIENT_RESOURCES = $E8001000;
  USBIO_ERR_SET_CONFIG_FAILED = $E0002000;
  USBIO_ERR_USBD_BUFFER_TOO_SMALL = $E0003000;
  USBIO_ERR_USBD_INTERFACE_NOT_FOUND = $E0004000;
  USBIO_ERR_INVALID_PIPE_FLAGS = $E0005000;
  USBIO_ERR_USBD_TIMEOUT = $E0006000;
  USBIO_ERR_DEVICE_GONE = $E0007000;
  USBIO_ERR_STATUS_NOT_MAPPED = $E0008000;
  USBIO_ERR_CANCELED = $E0010000;
  USBIO_ERR_ISO_NOT_ACCESSED_BY_HW = $E0020000;
  USBIO_ERR_ISO_TD_ERROR = $E0030000;
  USBIO_ERR_ISO_NA_LATE_USBPORT = $E0040000;
  USBIO_ERR_ISO_NOT_ACCESSED_LATE = $E0050000;
  USBIO_ERR_FAILED = $E0001000;
  USBIO_ERR_INVALID_INBUFFER = $E0001001;
  USBIO_ERR_INVALID_OUTBUFFER = $E0001002;
  USBIO_ERR_OUT_OF_MEMORY = $E0001003;
  USBIO_ERR_PENDING_REQUESTS = $E0001004;
  USBIO_ERR_ALREADY_CONFIGURED = $E0001005;
  USBIO_ERR_NOT_CONFIGURED = $E0001006;
  USBIO_ERR_OPEN_PIPES = $E0001007;
  USBIO_ERR_ALREADY_BOUND = $E0001008;
  USBIO_ERR_NOT_BOUND = $E0001009;
  USBIO_ERR_DEVICE_NOT_PRESENT = $E000100A;
  USBIO_ERR_CONTROL_NOT_SUPPORTED = $E000100B;
  USBIO_ERR_TIMEOUT = $E000100C;
  USBIO_ERR_INVALID_RECIPIENT = $E000100D;
  USBIO_ERR_INVALID_TYPE = $E000100E;
  USBIO_ERR_INVALID_IOCTL = $E000100F;
  USBIO_ERR_INVALID_DIRECTION = $E0001010;
  USBIO_ERR_TOO_MUCH_ISO_PACKETS = $E0001011;
  USBIO_ERR_POOL_EMPTY = $E0001012;
  USBIO_ERR_PIPE_NOT_FOUND = $E0001013;
  USBIO_ERR_INVALID_ISO_PACKET = $E0001014;
  USBIO_ERR_OUT_OF_ADDRESS_SPACE = $E0001015;
  USBIO_ERR_INTERFACE_NOT_FOUND = $E0001016;
  USBIO_ERR_INVALID_DEVICE_STATE = $E0001017;
  USBIO_ERR_INVALID_PARAM = $E0001018;
  USBIO_ERR_DEMO_EXPIRED = $E0001019;
  USBIO_ERR_INVALID_POWER_STATE = $E000101A;
  USBIO_ERR_POWER_DOWN = $E000101B;
  USBIO_ERR_VERSION_MISMATCH = $E000101C;
  USBIO_ERR_SET_CONFIGURATION_FAILED = $E000101D;
  USBIO_ERR_ADDITIONAL_EVENT_SIGNALLED = $E000101E;
  USBIO_ERR_INVALID_PROCESS = $E000101F;
  USBIO_ERR_DEVICE_ACQUIRED = $E0001020;
  USBIO_ERR_DEVICE_OPENED = $E0001021;
  USBIO_ERR_VID_RESTRICTION = $E0001080;
  USBIO_ERR_ISO_RESTRICTION = $E0001081;
  USBIO_ERR_BULK_RESTRICTION = $E0001082;
  USBIO_ERR_EP0_RESTRICTION = $E0001083;
  USBIO_ERR_PIPE_RESTRICTION = $E0001084;
  USBIO_ERR_PIPE_SIZE_RESTRICTION = $E0001085;
  USBIO_ERR_CONTROL_RESTRICTION = $E0001086;
  USBIO_ERR_INTERRUPT_RESTRICTION = $E0001087;
  USBIO_ERR_DEVICE_NOT_FOUND = $E0001100;
  USBIO_ERR_DEVICE_NOT_OPEN = $E0001102;
  USBIO_ERR_NO_SUCH_DEVICE_INSTANCE = $E0001104;
  USBIO_ERR_INVALID_FUNCTION_PARAM = $E0001105;
  USBIO_ERR_LOAD_SETUP_API_FAILED = $E0001106;
  USBIO_ERR_DEVICE_ALREADY_OPENED = $E0001107;
  USBIO_ERR_NOT_ENUMERATED = $E000A001;
  USBIO_ERR_TOO_MANY_INTERFACES = $E000A002;
  USBIO_ERR_NO_INTERFACE = $E000A003;
  USBIO_ERR_START_THREAD_FAILED = $E000A004;
  USBIO_ERR_NO_DATA = $E000A005;
  USBIO_ERR_BUFFER_TOO_SMALL = $E000A006;
  USBIO_ERR_THREAD_IS_RUNNING = $E000A007;
  USBIO_ERR_INVALID_PIPE_TYPE = $E000A008;
  USBIO_ERR_NO_BUFFER = $E000A009;
  USBIO_ERR_BUFFER_TOO_LARGE = $E000A00A;
  USBIO_ERR_WRITE_NOT_STARTED = $E000A00B;
  USBIO_ERR_READ_NOT_STARTED = $E000A00C;
  USBIO_ERR_INVALID_ISO_BUFFER = $E000A00D;
  USBIO_ERR_STATUS_ARRAY_TOO_SMALL = $E000A00E;
  USBIO_ERR_INVALID_ARRAY = $E000A00F;
  USBIO_ERR_DEVICE_ALREADY_OPEN = $E000A010;
  USBIO_ERR_ALREADY_CALLED = $E000A011;

// Constants for enum _USBIOCOM_INFO_FLAGS
type
  _USBIOCOM_INFO_FLAGS = TOleEnum;
const
  USBIOCOM_INFOFLAG_CHECKED_BUILD = $00000010;
  USBIOCOM_INFOFLAG_DEMO_VERSION = $00000020;
  USBIOCOM_INFOFLAG_LIGHT_VERSION = $00000100;

// Constants for enum _USBIOCOM_DEVICE_OPTION_FLAGS
type
  _USBIOCOM_DEVICE_OPTION_FLAGS = TOleEnum;
const
  USBIOCOM_RESET_DEVICE_ON_CLOSE = $00000001;
  USBIOCOM_UNCONFIGURE_ON_CLOSE = $00000002;
  USBIOCOM_ENABLE_REMOTE_WAKEUP = $00000004;

// Constants for enum _USBIOCOM_PIPE_OPTION_FLAGS
type
  _USBIOCOM_PIPE_OPTION_FLAGS = TOleEnum;
const
  USBIOCOM_SHORT_TRANSFER_OK = $00010000;

// Constants for enum _USBIOCOM_REQUEST_RECIPIENT
type
  _USBIOCOM_REQUEST_RECIPIENT = TOleEnum;
const
  USBIOCOM_RecipientDevice = $00000000;
  USBIOCOM_RecipientInterface = $00000001;
  USBIOCOM_RecipientEndpoint = $00000002;
  USBIOCOM_RecipientOther = $00000003;

// Constants for enum _USBIOCOM_REQUEST_TYPE
type
  _USBIOCOM_REQUEST_TYPE = TOleEnum;
const
  USBIOCOM_RequestTypeClass = $00000001;
  USBIOCOM_RequestTypeVendor = $00000002;

// Constants for enum _USBIOCOM_PIPE_TYPE
type
  _USBIOCOM_PIPE_TYPE = TOleEnum;
const
  USBIOCOM_PipeTypeControl = $00000000;
  USBIOCOM_PipeTypeIsochronous = $00000001;
  USBIOCOM_PipeTypeBulk = $00000002;
  USBIOCOM_PipeTypeInterrupt = $00000003;

// Constants for enum _USBIOCOM_DEVICE_POWER_STATE
type
  _USBIOCOM_DEVICE_POWER_STATE = TOleEnum;
const
  USBIOCOM_DevicePowerStateD0 = $00000000;
  USBIOCOM_DevicePowerStateD1 = $00000001;
  USBIOCOM_DevicePowerStateD2 = $00000002;
  USBIOCOM_DevicePowerStateD3 = $00000003;

// Constants for enum _USBIOCOM_DESCRIPTOR_TYPE
type
  _USBIOCOM_DESCRIPTOR_TYPE = TOleEnum;
const
  USB_DeviceDescriptorType = $00000001;
  USB_ConfigurationDescriptorType = $00000002;
  USB_StringDescriptorType = $00000003;
  USB_InterfaceDescriptorType = $00000004;
  USB_EndpointDescriptorType = $00000005;
  USB_HID_DescriptorType = $00000021;

// Constants for enum _USBIOCOM_QUERY_PIPE_STATISTICS_FLAGS
type
  _USBIOCOM_QUERY_PIPE_STATISTICS_FLAGS = TOleEnum;
const
  USBIOCOM_QPS_FLAG_RESET_BYTES_TRANSFERRED = $00000001;
  USBIOCOM_QPS_FLAG_RESET_REQUESTS_SUCCEEDED = $00000002;
  USBIOCOM_QPS_FLAG_RESET_REQUESTS_FAILED = $00000004;
  USBIOCOM_QPS_FLAG_RESET_ALL_COUNTERS = $00000007;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IUSBIOInterfaceEvents3 = dispinterface;
  IUSBIOInterface3 = interface;
  IUSBIOInterface3Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  USBIOInterface3 = IUSBIOInterface3;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  USBIOCOM_ERR_CODES = _USBIOCOM_ERR_CODES; 
  USBIOCOM_INFO_FLAGS = _USBIOCOM_INFO_FLAGS; 
  USBIOCOM_DEVICE_OPTION_FLAGS = _USBIOCOM_DEVICE_OPTION_FLAGS; 
  USBIOCOM_PIPE_OPTION_FLAGS = _USBIOCOM_PIPE_OPTION_FLAGS; 
  USBIOCOM_REQUEST_RECIPIENT = _USBIOCOM_REQUEST_RECIPIENT; 
  USBIOCOM_REQUEST_TYPE = _USBIOCOM_REQUEST_TYPE; 
  USBIOCOM_PIPE_TYPE = _USBIOCOM_PIPE_TYPE; 
  USBIOCOM_DEVICE_POWER_STATE = _USBIOCOM_DEVICE_POWER_STATE; 
  USBIOCOM_DESCRIPTOR_TYPE = _USBIOCOM_DESCRIPTOR_TYPE; 
  USBIOCOM_QUERY_PIPE_STATISTICS_FLAGS = _USBIOCOM_QUERY_PIPE_STATISTICS_FLAGS; 

// *********************************************************************//
// DispIntf:  _IUSBIOInterfaceEvents3
// Flags:     (4096) Dispatchable
// GUID:      {3C83A43F-CFD4-4781-8949-04EDDBD3E191}
// *********************************************************************//
  _IUSBIOInterfaceEvents3 = dispinterface
    ['{3C83A43F-CFD4-4781-8949-04EDDBD3E191}']
    procedure ReadComplete(const Obj: IDispatch); dispid 1;
    procedure WriteComplete(const Obj: IDispatch); dispid 2;
    procedure WriteStatusAvailable(const Obj: IDispatch); dispid 3;
    procedure PnPAddNotification(const Obj: IDispatch); dispid 4;
    procedure PnPRemoveNotification(const Obj: IDispatch); dispid 5;
  end;

// *********************************************************************//
// Interface: IUSBIOInterface3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {775938FE-790A-476E-BA6D-20BE61EFE5BC}
// *********************************************************************//
  IUSBIOInterface3 = interface(IDispatch)
    ['{775938FE-790A-476E-BA6D-20BE61EFE5BC}']
    procedure EnumerateDevices(const GUIDDriverInterface: WideString; out NumberOfDevices: SYSINT); safecall;
    procedure OpenDevice(DeviceNumber: SYSINT; out Status: SYSINT); safecall;
    procedure CloseDevice; safecall;
    function  Get_DevicePathName: WideString; safecall;
    procedure GetDriverInfo(out APIVersion: SYSINT; out DriverVersion: SYSINT; 
                            out DriverBuildNumber: SYSINT; out Flags: SYSINT; out Status: SYSINT); safecall;
    function  Get_IsCheckedBuild: Integer; safecall;
    function  Get_IsDemoVersion: Integer; safecall;
    function  Get_IsLightVersion: Integer; safecall;
    function  Get_DeviceOptions: SYSINT; safecall;
    procedure Set_DeviceOptions(Options: SYSINT); safecall;
    function  Get_DeviceRequestTimeout: SYSINT; safecall;
    procedure Set_DeviceRequestTimeout(pVal: SYSINT); safecall;
    procedure GetDescriptor(var Descriptor: PSafeArray; var DescSize: SYSINT; Recipient: SYSINT; 
                            DescriptorType: SYSINT; DescriptorIndex: SYSINT; LanguageId: SYSINT; 
                            out Status: SYSINT); safecall;
    procedure GetDeviceDescriptor(var DeviceDescriptor: PSafeArray; var DescSize: SYSINT; 
                                  out Status: SYSINT); safecall;
    procedure GetConfigurationDescriptor(var ConfigDescriptor: PSafeArray; var DescSize: SYSINT; 
                                         Index: Byte; out Status: SYSINT); safecall;
    procedure GetStringDescriptor(var StringDescriptor: PSafeArray; var DescSize: SYSINT; 
                                  Index: Byte; LanguageId: SYSINT; out Status: SYSINT); safecall;
    procedure SetDescriptor(var Descriptor: PSafeArray; Recipient: SYSINT; DescriptorType: SYSINT; 
                            DescriptorIndex: SYSINT; LanguageId: SYSINT; out Status: SYSINT); safecall;
    procedure AddInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT); safecall;
    procedure DeleteInterfaces; safecall;
    procedure SetConfiguration(ConfigurationIndex: SYSINT; out Status: SYSINT); safecall;
    procedure GetConfiguration(out ConfigurationValue: Byte; out Status: SYSINT); safecall;
    procedure UnconfigureDevice(out Status: SYSINT); safecall;
    procedure SetInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT); safecall;
    procedure GetInterface(out AlternateSetting: Byte; InterfaceIndex: SYSINT; out Status: SYSINT); safecall;
    procedure ClassOrVendorInRequest(var Buffer: PSafeArray; var ByteCount: SYSINT; Flags: SYSINT; 
                                     Type_: SYSINT; Recipient: SYSINT; 
                                     RequestTypeReservedBits: SYSINT; Request: SYSINT; 
                                     Value: SYSINT; Index: SYSINT; out Status: SYSINT); safecall;
    procedure ClassOrVendorOutRequest(var Buffer: PSafeArray; Flags: SYSINT; Type_: SYSINT; 
                                      Recipient: SYSINT; RequestTypeReservedBits: SYSINT; 
                                      Request: SYSINT; Value: SYSINT; Index: SYSINT; 
                                      out Status: SYSINT); safecall;
    procedure SetFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                         out Status: SYSINT); safecall;
    procedure ClearFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                           out Status: SYSINT); safecall;
    procedure GetDevicePowerState(out DevicePowerState: SYSINT; out Status: SYSINT); safecall;
    procedure SetDevicePowerState(DevicePowerState: SYSINT; out Status: SYSINT); safecall;
    procedure ResetDevice(out Status: SYSINT); safecall;
    procedure CyclePort(out Status: SYSINT); safecall;
    procedure GetStatus(out StatusValue: SYSINT; Recipient: SYSINT; Index: SYSINT; 
                        out Status: SYSINT); safecall;
    procedure GetCurrentFrameNumber(out FrameNumber: SYSINT; out Status: SYSINT); safecall;
    function  ErrorText(Status: SYSINT): WideString; safecall;
    procedure Bind(EndpointAddress: Byte; out Status: SYSINT); safecall;
    procedure Unbind(out Status: SYSINT); safecall;
    procedure StartReading(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; out Status: SYSINT); safecall;
    procedure ReadData(var Buffer: PSafeArray; out ByteCount: SYSINT; out Status: SYSINT); safecall;
    procedure ReadIsoData(var Buffer: PSafeArray; out ByteCount: SYSINT; 
                          var SubBufferLength_ErrorCode: PSafeArray; out Status: SYSINT); safecall;
    procedure StopReading; safecall;
    procedure StartWriting(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; WriteStatus: Integer; out Status: SYSINT); safecall;
    procedure WriteData(var Buffer: PSafeArray; UserId: SYSINT; out Status: SYSINT); safecall;
    procedure GetWriteStatus(out UserId: SYSINT; out Status: SYSINT); safecall;
    procedure WriteIsoData(var Buffer: PSafeArray; var SubBufferLength: PSafeArray; UserId: SYSINT; 
                           out Status: SYSINT); safecall;
    procedure GetIsoWriteStatus(out UserId: SYSINT; var StatusArray: PSafeArray; 
                                out FrameCount: SYSINT; out Status: SYSINT); safecall;
    procedure StopWriting; safecall;
    procedure ResetPipe(out Status: SYSINT); safecall;
    procedure AbortPipe(out Status: SYSINT); safecall;
    function  Get_ShortTransferOK: Integer; safecall;
    procedure Set_ShortTransferOK(ShortTransfer: Integer); safecall;
    function  Get_EndpointFifoSize: SYSINT; safecall;
    procedure EnablePnPNotification(const Guid: WideString; out Status: SYSINT); safecall;
    procedure DisablePnPNotification(const Guid: WideString; out Status: SYSINT); safecall;
    procedure GetBandwidthInfo(out TotalBandwidth: SYSINT; out ConsumedBandwidth: SYSINT; 
                               out Status: SYSINT); safecall;
    function  Get_IsOperatingAtHighSpeed: Integer; safecall;
    procedure SetupPipeStatistics(AveragingInterval: SYSINT; out Status: SYSINT); safecall;
    procedure QueryPipeStatistics(out ActualAveragingInterval: SYSINT; out AverageRate: SYSINT; 
                                  out BytesTransferred_L: SYSINT; out BytesTransferred_H: SYSINT; 
                                  out RequestsSucceeded: SYSINT; out RequestsFailed: SYSINT; 
                                  Flags: SYSINT; out Status: SYSINT); safecall;
    procedure AcquireDevice(out Status: SYSINT); safecall;
    procedure ReleaseDevice(out Status: SYSINT); safecall;
    function  Get_OpenCount: SYSINT; safecall;
    property DevicePathName: WideString read Get_DevicePathName;
    property IsCheckedBuild: Integer read Get_IsCheckedBuild;
    property IsDemoVersion: Integer read Get_IsDemoVersion;
    property IsLightVersion: Integer read Get_IsLightVersion;
    property DeviceOptions: SYSINT read Get_DeviceOptions write Set_DeviceOptions;
    property DeviceRequestTimeout: SYSINT read Get_DeviceRequestTimeout write Set_DeviceRequestTimeout;
    property ShortTransferOK: Integer read Get_ShortTransferOK write Set_ShortTransferOK;
    property EndpointFifoSize: SYSINT read Get_EndpointFifoSize;
    property IsOperatingAtHighSpeed: Integer read Get_IsOperatingAtHighSpeed;
    property OpenCount: SYSINT read Get_OpenCount;
  end;

// *********************************************************************//
// DispIntf:  IUSBIOInterface3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {775938FE-790A-476E-BA6D-20BE61EFE5BC}
// *********************************************************************//
  IUSBIOInterface3Disp = dispinterface
    ['{775938FE-790A-476E-BA6D-20BE61EFE5BC}']
    procedure EnumerateDevices(const GUIDDriverInterface: WideString; out NumberOfDevices: SYSINT); dispid 1;
    procedure OpenDevice(DeviceNumber: SYSINT; out Status: SYSINT); dispid 2;
    procedure CloseDevice; dispid 3;
    property DevicePathName: WideString readonly dispid 4;
    procedure GetDriverInfo(out APIVersion: SYSINT; out DriverVersion: SYSINT; 
                            out DriverBuildNumber: SYSINT; out Flags: SYSINT; out Status: SYSINT); dispid 5;
    property IsCheckedBuild: Integer readonly dispid 6;
    property IsDemoVersion: Integer readonly dispid 7;
    property IsLightVersion: Integer readonly dispid 8;
    property DeviceOptions: SYSINT dispid 9;
    property DeviceRequestTimeout: SYSINT dispid 10;
    procedure GetDescriptor(var Descriptor: {??PSafeArray} OleVariant; var DescSize: SYSINT; 
                            Recipient: SYSINT; DescriptorType: SYSINT; DescriptorIndex: SYSINT; 
                            LanguageId: SYSINT; out Status: SYSINT); dispid 12;
    procedure GetDeviceDescriptor(var DeviceDescriptor: {??PSafeArray} OleVariant; 
                                  var DescSize: SYSINT; out Status: SYSINT); dispid 13;
    procedure GetConfigurationDescriptor(var ConfigDescriptor: {??PSafeArray} OleVariant; 
                                         var DescSize: SYSINT; Index: Byte; out Status: SYSINT); dispid 14;
    procedure GetStringDescriptor(var StringDescriptor: {??PSafeArray} OleVariant; 
                                  var DescSize: SYSINT; Index: Byte; LanguageId: SYSINT; 
                                  out Status: SYSINT); dispid 15;
    procedure SetDescriptor(var Descriptor: {??PSafeArray} OleVariant; Recipient: SYSINT; 
                            DescriptorType: SYSINT; DescriptorIndex: SYSINT; LanguageId: SYSINT; 
                            out Status: SYSINT); dispid 16;
    procedure AddInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT); dispid 17;
    procedure DeleteInterfaces; dispid 18;
    procedure SetConfiguration(ConfigurationIndex: SYSINT; out Status: SYSINT); dispid 19;
    procedure GetConfiguration(out ConfigurationValue: Byte; out Status: SYSINT); dispid 20;
    procedure UnconfigureDevice(out Status: SYSINT); dispid 21;
    procedure SetInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT); dispid 22;
    procedure GetInterface(out AlternateSetting: Byte; InterfaceIndex: SYSINT; out Status: SYSINT); dispid 23;
    procedure ClassOrVendorInRequest(var Buffer: {??PSafeArray} OleVariant; var ByteCount: SYSINT; 
                                     Flags: SYSINT; Type_: SYSINT; Recipient: SYSINT; 
                                     RequestTypeReservedBits: SYSINT; Request: SYSINT; 
                                     Value: SYSINT; Index: SYSINT; out Status: SYSINT); dispid 24;
    procedure ClassOrVendorOutRequest(var Buffer: {??PSafeArray} OleVariant; Flags: SYSINT; 
                                      Type_: SYSINT; Recipient: SYSINT; 
                                      RequestTypeReservedBits: SYSINT; Request: SYSINT; 
                                      Value: SYSINT; Index: SYSINT; out Status: SYSINT); dispid 25;
    procedure SetFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                         out Status: SYSINT); dispid 26;
    procedure ClearFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                           out Status: SYSINT); dispid 27;
    procedure GetDevicePowerState(out DevicePowerState: SYSINT; out Status: SYSINT); dispid 28;
    procedure SetDevicePowerState(DevicePowerState: SYSINT; out Status: SYSINT); dispid 29;
    procedure ResetDevice(out Status: SYSINT); dispid 30;
    procedure CyclePort(out Status: SYSINT); dispid 31;
    procedure GetStatus(out StatusValue: SYSINT; Recipient: SYSINT; Index: SYSINT; 
                        out Status: SYSINT); dispid 32;
    procedure GetCurrentFrameNumber(out FrameNumber: SYSINT; out Status: SYSINT); dispid 33;
    function  ErrorText(Status: SYSINT): WideString; dispid 34;
    procedure Bind(EndpointAddress: Byte; out Status: SYSINT); dispid 35;
    procedure Unbind(out Status: SYSINT); dispid 36;
    procedure StartReading(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; out Status: SYSINT); dispid 37;
    procedure ReadData(var Buffer: {??PSafeArray} OleVariant; out ByteCount: SYSINT; 
                       out Status: SYSINT); dispid 38;
    procedure ReadIsoData(var Buffer: {??PSafeArray} OleVariant; out ByteCount: SYSINT; 
                          var SubBufferLength_ErrorCode: {??PSafeArray} OleVariant; 
                          out Status: SYSINT); dispid 39;
    procedure StopReading; dispid 40;
    procedure StartWriting(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; WriteStatus: Integer; out Status: SYSINT); dispid 41;
    procedure WriteData(var Buffer: {??PSafeArray} OleVariant; UserId: SYSINT; out Status: SYSINT); dispid 42;
    procedure GetWriteStatus(out UserId: SYSINT; out Status: SYSINT); dispid 43;
    procedure WriteIsoData(var Buffer: {??PSafeArray} OleVariant; 
                           var SubBufferLength: {??PSafeArray} OleVariant; UserId: SYSINT; 
                           out Status: SYSINT); dispid 44;
    procedure GetIsoWriteStatus(out UserId: SYSINT; var StatusArray: {??PSafeArray} OleVariant; 
                                out FrameCount: SYSINT; out Status: SYSINT); dispid 45;
    procedure StopWriting; dispid 46;
    procedure ResetPipe(out Status: SYSINT); dispid 47;
    procedure AbortPipe(out Status: SYSINT); dispid 48;
    property ShortTransferOK: Integer dispid 49;
    property EndpointFifoSize: SYSINT readonly dispid 51;
    procedure EnablePnPNotification(const Guid: WideString; out Status: SYSINT); dispid 52;
    procedure DisablePnPNotification(const Guid: WideString; out Status: SYSINT); dispid 53;
    procedure GetBandwidthInfo(out TotalBandwidth: SYSINT; out ConsumedBandwidth: SYSINT; 
                               out Status: SYSINT); dispid 54;
    property IsOperatingAtHighSpeed: Integer readonly dispid 55;
    procedure SetupPipeStatistics(AveragingInterval: SYSINT; out Status: SYSINT); dispid 56;
    procedure QueryPipeStatistics(out ActualAveragingInterval: SYSINT; out AverageRate: SYSINT; 
                                  out BytesTransferred_L: SYSINT; out BytesTransferred_H: SYSINT; 
                                  out RequestsSucceeded: SYSINT; out RequestsFailed: SYSINT; 
                                  Flags: SYSINT; out Status: SYSINT); dispid 57;
    procedure AcquireDevice(out Status: SYSINT); dispid 58;
    procedure ReleaseDevice(out Status: SYSINT); dispid 59;
    property OpenCount: SYSINT readonly dispid 60;
  end;

// *********************************************************************//
// The Class CoUSBIOInterface3 provides a Create and CreateRemote method to          
// create instances of the default interface IUSBIOInterface3 exposed by              
// the CoClass USBIOInterface3. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUSBIOInterface3 = class
    class function Create: IUSBIOInterface3;
    class function CreateRemote(const MachineName: string): IUSBIOInterface3;
  end;

  TUSBIOInterface3ReadComplete = procedure(Sender: TObject; var Obj: OleVariant) of object;
  TUSBIOInterface3WriteComplete = procedure(Sender: TObject; var Obj: OleVariant) of object;
  TUSBIOInterface3WriteStatusAvailable = procedure(Sender: TObject; var Obj: OleVariant) of object;
  TUSBIOInterface3PnPAddNotification = procedure(Sender: TObject; var Obj: OleVariant) of object;
  TUSBIOInterface3PnPRemoveNotification = procedure(Sender: TObject; var Obj: OleVariant) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUSBIOInterface3
// Help String      : USBIOInterface3 Class
// Default Interface: IUSBIOInterface3
// Def. Intf. DISP? : No
// Event   Interface: _IUSBIOInterfaceEvents3
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUSBIOInterface3Properties= class;
{$ENDIF}
  TUSBIOInterface3 = class(TOleServer)
  private
    FOnReadComplete: TUSBIOInterface3ReadComplete;
    FOnWriteComplete: TUSBIOInterface3WriteComplete;
    FOnWriteStatusAvailable: TUSBIOInterface3WriteStatusAvailable;
    FOnPnPAddNotification: TUSBIOInterface3PnPAddNotification;
    FOnPnPRemoveNotification: TUSBIOInterface3PnPRemoveNotification;
    FIntf:        IUSBIOInterface3;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TUSBIOInterface3Properties;
    function      GetServerProperties: TUSBIOInterface3Properties;
{$ENDIF}
    function      GetDefaultInterface: IUSBIOInterface3;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_DevicePathName: WideString;
    function  Get_IsCheckedBuild: Integer;
    function  Get_IsDemoVersion: Integer;
    function  Get_IsLightVersion: Integer;
    function  Get_DeviceOptions: SYSINT;
    procedure Set_DeviceOptions(Options: SYSINT);
    function  Get_DeviceRequestTimeout: SYSINT;
    procedure Set_DeviceRequestTimeout(pVal: SYSINT);
    function  Get_ShortTransferOK: Integer;
    procedure Set_ShortTransferOK(ShortTransfer: Integer);
    function  Get_EndpointFifoSize: SYSINT;
    function  Get_IsOperatingAtHighSpeed: Integer;
    function  Get_OpenCount: SYSINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUSBIOInterface3);
    procedure Disconnect; override;
    procedure EnumerateDevices(const GUIDDriverInterface: WideString; out NumberOfDevices: SYSINT);
    procedure OpenDevice(DeviceNumber: SYSINT; out Status: SYSINT);
    procedure CloseDevice;
    procedure GetDriverInfo(out APIVersion: SYSINT; out DriverVersion: SYSINT; 
                            out DriverBuildNumber: SYSINT; out Flags: SYSINT; out Status: SYSINT);
    procedure GetDescriptor(var Descriptor: PSafeArray; var DescSize: SYSINT; Recipient: SYSINT; 
                            DescriptorType: SYSINT; DescriptorIndex: SYSINT; LanguageId: SYSINT; 
                            out Status: SYSINT);
    procedure GetDeviceDescriptor(var DeviceDescriptor: PSafeArray; var DescSize: SYSINT; 
                                  out Status: SYSINT);
    procedure GetConfigurationDescriptor(var ConfigDescriptor: PSafeArray; var DescSize: SYSINT; 
                                         Index: Byte; out Status: SYSINT);
    procedure GetStringDescriptor(var StringDescriptor: PSafeArray; var DescSize: SYSINT; 
                                  Index: Byte; LanguageId: SYSINT; out Status: SYSINT);
    procedure SetDescriptor(var Descriptor: PSafeArray; Recipient: SYSINT; DescriptorType: SYSINT; 
                            DescriptorIndex: SYSINT; LanguageId: SYSINT; out Status: SYSINT);
    procedure AddInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT);
    procedure DeleteInterfaces;
    procedure SetConfiguration(ConfigurationIndex: SYSINT; out Status: SYSINT);
    procedure GetConfiguration(out ConfigurationValue: Byte; out Status: SYSINT);
    procedure UnconfigureDevice(out Status: SYSINT);
    procedure SetInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                           MaximumTransferSize: SYSINT; out Status: SYSINT);
    procedure GetInterface(out AlternateSetting: Byte; InterfaceIndex: SYSINT; out Status: SYSINT);
    procedure ClassOrVendorInRequest(var Buffer: PSafeArray; var ByteCount: SYSINT; Flags: SYSINT; 
                                     Type_: SYSINT; Recipient: SYSINT; 
                                     RequestTypeReservedBits: SYSINT; Request: SYSINT; 
                                     Value: SYSINT; Index: SYSINT; out Status: SYSINT);
    procedure ClassOrVendorOutRequest(var Buffer: PSafeArray; Flags: SYSINT; Type_: SYSINT; 
                                      Recipient: SYSINT; RequestTypeReservedBits: SYSINT; 
                                      Request: SYSINT; Value: SYSINT; Index: SYSINT; 
                                      out Status: SYSINT);
    procedure SetFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                         out Status: SYSINT);
    procedure ClearFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                           out Status: SYSINT);
    procedure GetDevicePowerState(out DevicePowerState: SYSINT; out Status: SYSINT);
    procedure SetDevicePowerState(DevicePowerState: SYSINT; out Status: SYSINT);
    procedure ResetDevice(out Status: SYSINT);
    procedure CyclePort(out Status: SYSINT);
    procedure GetStatus(out StatusValue: SYSINT; Recipient: SYSINT; Index: SYSINT; 
                        out Status: SYSINT);
    procedure GetCurrentFrameNumber(out FrameNumber: SYSINT; out Status: SYSINT);
    function  ErrorText(Status: SYSINT): WideString;
    procedure Bind(EndpointAddress: Byte; out Status: SYSINT);
    procedure Unbind(out Status: SYSINT);
    procedure StartReading(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; out Status: SYSINT);
    procedure ReadData(var Buffer: PSafeArray; out ByteCount: SYSINT; out Status: SYSINT);
    procedure ReadIsoData(var Buffer: PSafeArray; out ByteCount: SYSINT; 
                          var SubBufferLength_ErrorCode: PSafeArray; out Status: SYSINT);
    procedure StopReading;
    procedure StartWriting(SizeOfBuffer_IsoFramesInBuffer: SYSINT; NumberOfBuffers: SYSINT; 
                           MaxErrorCount: SYSINT; WriteStatus: Integer; out Status: SYSINT);
    procedure WriteData(var Buffer: PSafeArray; UserId: SYSINT; out Status: SYSINT);
    procedure GetWriteStatus(out UserId: SYSINT; out Status: SYSINT);
    procedure WriteIsoData(var Buffer: PSafeArray; var SubBufferLength: PSafeArray; UserId: SYSINT; 
                           out Status: SYSINT);
    procedure GetIsoWriteStatus(out UserId: SYSINT; var StatusArray: PSafeArray; 
                                out FrameCount: SYSINT; out Status: SYSINT);
    procedure StopWriting;
    procedure ResetPipe(out Status: SYSINT);
    procedure AbortPipe(out Status: SYSINT);
    procedure EnablePnPNotification(const Guid: WideString; out Status: SYSINT);
    procedure DisablePnPNotification(const Guid: WideString; out Status: SYSINT);
    procedure GetBandwidthInfo(out TotalBandwidth: SYSINT; out ConsumedBandwidth: SYSINT; 
                               out Status: SYSINT);
    procedure SetupPipeStatistics(AveragingInterval: SYSINT; out Status: SYSINT);
    procedure QueryPipeStatistics(out ActualAveragingInterval: SYSINT; out AverageRate: SYSINT; 
                                  out BytesTransferred_L: SYSINT; out BytesTransferred_H: SYSINT; 
                                  out RequestsSucceeded: SYSINT; out RequestsFailed: SYSINT; 
                                  Flags: SYSINT; out Status: SYSINT);
    procedure AcquireDevice(out Status: SYSINT);
    procedure ReleaseDevice(out Status: SYSINT);
    property  DefaultInterface: IUSBIOInterface3 read GetDefaultInterface;
    property DevicePathName: WideString read Get_DevicePathName;
    property IsCheckedBuild: Integer read Get_IsCheckedBuild;
    property IsDemoVersion: Integer read Get_IsDemoVersion;
    property IsLightVersion: Integer read Get_IsLightVersion;
    property EndpointFifoSize: SYSINT read Get_EndpointFifoSize;
    property IsOperatingAtHighSpeed: Integer read Get_IsOperatingAtHighSpeed;
    property OpenCount: SYSINT read Get_OpenCount;
    property DeviceOptions: SYSINT read Get_DeviceOptions write Set_DeviceOptions;
    property DeviceRequestTimeout: SYSINT read Get_DeviceRequestTimeout write Set_DeviceRequestTimeout;
    property ShortTransferOK: Integer read Get_ShortTransferOK write Set_ShortTransferOK;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUSBIOInterface3Properties read GetServerProperties;
{$ENDIF}
    property OnReadComplete: TUSBIOInterface3ReadComplete read FOnReadComplete write FOnReadComplete;
    property OnWriteComplete: TUSBIOInterface3WriteComplete read FOnWriteComplete write FOnWriteComplete;
    property OnWriteStatusAvailable: TUSBIOInterface3WriteStatusAvailable read FOnWriteStatusAvailable write FOnWriteStatusAvailable;
    property OnPnPAddNotification: TUSBIOInterface3PnPAddNotification read FOnPnPAddNotification write FOnPnPAddNotification;
    property OnPnPRemoveNotification: TUSBIOInterface3PnPRemoveNotification read FOnPnPRemoveNotification write FOnPnPRemoveNotification;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUSBIOInterface3
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUSBIOInterface3Properties = class(TPersistent)
  private
    FServer:    TUSBIOInterface3;
    function    GetDefaultInterface: IUSBIOInterface3;
    constructor Create(AServer: TUSBIOInterface3);
  protected
    function  Get_DevicePathName: WideString;
    function  Get_IsCheckedBuild: Integer;
    function  Get_IsDemoVersion: Integer;
    function  Get_IsLightVersion: Integer;
    function  Get_DeviceOptions: SYSINT;
    procedure Set_DeviceOptions(Options: SYSINT);
    function  Get_DeviceRequestTimeout: SYSINT;
    procedure Set_DeviceRequestTimeout(pVal: SYSINT);
    function  Get_ShortTransferOK: Integer;
    procedure Set_ShortTransferOK(ShortTransfer: Integer);
    function  Get_EndpointFifoSize: SYSINT;
    function  Get_IsOperatingAtHighSpeed: Integer;
    function  Get_OpenCount: SYSINT;
  public
    property DefaultInterface: IUSBIOInterface3 read GetDefaultInterface;
  published
    property DeviceOptions: SYSINT read Get_DeviceOptions write Set_DeviceOptions;
    property DeviceRequestTimeout: SYSINT read Get_DeviceRequestTimeout write Set_DeviceRequestTimeout;
    property ShortTransferOK: Integer read Get_ShortTransferOK write Set_ShortTransferOK;
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoUSBIOInterface3.Create: IUSBIOInterface3;
begin
  Result := CreateComObject(CLASS_USBIOInterface3) as IUSBIOInterface3;
end;

class function CoUSBIOInterface3.CreateRemote(const MachineName: string): IUSBIOInterface3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_USBIOInterface3) as IUSBIOInterface3;
end;

procedure TUSBIOInterface3.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{B949A098-6A08-4EEE-B32D-CA0D19BBF24E}';
    IntfIID:   '{775938FE-790A-476E-BA6D-20BE61EFE5BC}';
    EventIID:  '{3C83A43F-CFD4-4781-8949-04EDDBD3E191}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUSBIOInterface3.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IUSBIOInterface3;
  end;
end;

procedure TUSBIOInterface3.ConnectTo(svrIntf: IUSBIOInterface3);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TUSBIOInterface3.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TUSBIOInterface3.GetDefaultInterface: IUSBIOInterface3;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TUSBIOInterface3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUSBIOInterface3Properties.Create(Self);
{$ENDIF}
end;

destructor TUSBIOInterface3.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUSBIOInterface3.GetServerProperties: TUSBIOInterface3Properties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TUSBIOInterface3.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnReadComplete) then
            FOnReadComplete(Self, Params[0] {const IDispatch});
   2: if Assigned(FOnWriteComplete) then
            FOnWriteComplete(Self, Params[0] {const IDispatch});
   3: if Assigned(FOnWriteStatusAvailable) then
            FOnWriteStatusAvailable(Self, Params[0] {const IDispatch});
   4: if Assigned(FOnPnPAddNotification) then
            FOnPnPAddNotification(Self, Params[0] {const IDispatch});
   5: if Assigned(FOnPnPRemoveNotification) then
            FOnPnPRemoveNotification(Self, Params[0] {const IDispatch});
  end; {case DispID}
end;

function  TUSBIOInterface3.Get_DevicePathName: WideString;
begin
  Result := DefaultInterface.Get_DevicePathName;
end;

function  TUSBIOInterface3.Get_IsCheckedBuild: Integer;
begin
  Result := DefaultInterface.Get_IsCheckedBuild;
end;

function  TUSBIOInterface3.Get_IsDemoVersion: Integer;
begin
  Result := DefaultInterface.Get_IsDemoVersion;
end;

function  TUSBIOInterface3.Get_IsLightVersion: Integer;
begin
  Result := DefaultInterface.Get_IsLightVersion;
end;

function  TUSBIOInterface3.Get_DeviceOptions: SYSINT;
begin
  Result := DefaultInterface.Get_DeviceOptions;
end;

procedure TUSBIOInterface3.Set_DeviceOptions(Options: SYSINT);
begin
  DefaultInterface.Set_DeviceOptions(Options);
end;

function  TUSBIOInterface3.Get_DeviceRequestTimeout: SYSINT;
begin
  Result := DefaultInterface.Get_DeviceRequestTimeout;
end;

procedure TUSBIOInterface3.Set_DeviceRequestTimeout(pVal: SYSINT);
begin
  DefaultInterface.Set_DeviceRequestTimeout(pVal);
end;

function  TUSBIOInterface3.Get_ShortTransferOK: Integer;
begin
  Result := DefaultInterface.Get_ShortTransferOK;
end;

procedure TUSBIOInterface3.Set_ShortTransferOK(ShortTransfer: Integer);
begin
  DefaultInterface.Set_ShortTransferOK(ShortTransfer);
end;

function  TUSBIOInterface3.Get_EndpointFifoSize: SYSINT;
begin
  Result := DefaultInterface.Get_EndpointFifoSize;
end;

function  TUSBIOInterface3.Get_IsOperatingAtHighSpeed: Integer;
begin
  Result := DefaultInterface.Get_IsOperatingAtHighSpeed;
end;

function  TUSBIOInterface3.Get_OpenCount: SYSINT;
begin
  Result := DefaultInterface.Get_OpenCount;
end;

procedure TUSBIOInterface3.EnumerateDevices(const GUIDDriverInterface: WideString; 
                                            out NumberOfDevices: SYSINT);
begin
  DefaultInterface.EnumerateDevices(GUIDDriverInterface, NumberOfDevices);
end;

procedure TUSBIOInterface3.OpenDevice(DeviceNumber: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.OpenDevice(DeviceNumber, Status);
end;

procedure TUSBIOInterface3.CloseDevice;
begin
  DefaultInterface.CloseDevice;
end;

procedure TUSBIOInterface3.GetDriverInfo(out APIVersion: SYSINT; out DriverVersion: SYSINT; 
                                         out DriverBuildNumber: SYSINT; out Flags: SYSINT; 
                                         out Status: SYSINT);
begin
  DefaultInterface.GetDriverInfo(APIVersion, DriverVersion, DriverBuildNumber, Flags, Status);
end;

procedure TUSBIOInterface3.GetDescriptor(var Descriptor: PSafeArray; var DescSize: SYSINT; 
                                         Recipient: SYSINT; DescriptorType: SYSINT; 
                                         DescriptorIndex: SYSINT; LanguageId: SYSINT; 
                                         out Status: SYSINT);
begin
  DefaultInterface.GetDescriptor(Descriptor, DescSize, Recipient, DescriptorType, DescriptorIndex, 
                                 LanguageId, Status);
end;

procedure TUSBIOInterface3.GetDeviceDescriptor(var DeviceDescriptor: PSafeArray; 
                                               var DescSize: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetDeviceDescriptor(DeviceDescriptor, DescSize, Status);
end;

procedure TUSBIOInterface3.GetConfigurationDescriptor(var ConfigDescriptor: PSafeArray; 
                                                      var DescSize: SYSINT; Index: Byte; 
                                                      out Status: SYSINT);
begin
  DefaultInterface.GetConfigurationDescriptor(ConfigDescriptor, DescSize, Index, Status);
end;

procedure TUSBIOInterface3.GetStringDescriptor(var StringDescriptor: PSafeArray; 
                                               var DescSize: SYSINT; Index: Byte; 
                                               LanguageId: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetStringDescriptor(StringDescriptor, DescSize, Index, LanguageId, Status);
end;

procedure TUSBIOInterface3.SetDescriptor(var Descriptor: PSafeArray; Recipient: SYSINT; 
                                         DescriptorType: SYSINT; DescriptorIndex: SYSINT; 
                                         LanguageId: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.SetDescriptor(Descriptor, Recipient, DescriptorType, DescriptorIndex, 
                                 LanguageId, Status);
end;

procedure TUSBIOInterface3.AddInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                                        MaximumTransferSize: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.AddInterface(InterfaceIndex, AlternateSettingIndex, MaximumTransferSize, Status);
end;

procedure TUSBIOInterface3.DeleteInterfaces;
begin
  DefaultInterface.DeleteInterfaces;
end;

procedure TUSBIOInterface3.SetConfiguration(ConfigurationIndex: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.SetConfiguration(ConfigurationIndex, Status);
end;

procedure TUSBIOInterface3.GetConfiguration(out ConfigurationValue: Byte; out Status: SYSINT);
begin
  DefaultInterface.GetConfiguration(ConfigurationValue, Status);
end;

procedure TUSBIOInterface3.UnconfigureDevice(out Status: SYSINT);
begin
  DefaultInterface.UnconfigureDevice(Status);
end;

procedure TUSBIOInterface3.SetInterface(InterfaceIndex: SYSINT; AlternateSettingIndex: SYSINT; 
                                        MaximumTransferSize: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.SetInterface(InterfaceIndex, AlternateSettingIndex, MaximumTransferSize, Status);
end;

procedure TUSBIOInterface3.GetInterface(out AlternateSetting: Byte; InterfaceIndex: SYSINT; 
                                        out Status: SYSINT);
begin
  DefaultInterface.GetInterface(AlternateSetting, InterfaceIndex, Status);
end;

procedure TUSBIOInterface3.ClassOrVendorInRequest(var Buffer: PSafeArray; var ByteCount: SYSINT; 
                                                  Flags: SYSINT; Type_: SYSINT; Recipient: SYSINT; 
                                                  RequestTypeReservedBits: SYSINT; Request: SYSINT; 
                                                  Value: SYSINT; Index: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.ClassOrVendorInRequest(Buffer, ByteCount, Flags, Type_, Recipient, 
                                          RequestTypeReservedBits, Request, Value, Index, Status);
end;

procedure TUSBIOInterface3.ClassOrVendorOutRequest(var Buffer: PSafeArray; Flags: SYSINT; 
                                                   Type_: SYSINT; Recipient: SYSINT; 
                                                   RequestTypeReservedBits: SYSINT; 
                                                   Request: SYSINT; Value: SYSINT; Index: SYSINT; 
                                                   out Status: SYSINT);
begin
  DefaultInterface.ClassOrVendorOutRequest(Buffer, Flags, Type_, Recipient, 
                                           RequestTypeReservedBits, Request, Value, Index, Status);
end;

procedure TUSBIOInterface3.SetFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                                      out Status: SYSINT);
begin
  DefaultInterface.SetFeature(Recipient, FeatureSelector, Index, Status);
end;

procedure TUSBIOInterface3.ClearFeature(Recipient: SYSINT; FeatureSelector: SYSINT; Index: SYSINT; 
                                        out Status: SYSINT);
begin
  DefaultInterface.ClearFeature(Recipient, FeatureSelector, Index, Status);
end;

procedure TUSBIOInterface3.GetDevicePowerState(out DevicePowerState: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetDevicePowerState(DevicePowerState, Status);
end;

procedure TUSBIOInterface3.SetDevicePowerState(DevicePowerState: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.SetDevicePowerState(DevicePowerState, Status);
end;

procedure TUSBIOInterface3.ResetDevice(out Status: SYSINT);
begin
  DefaultInterface.ResetDevice(Status);
end;

procedure TUSBIOInterface3.CyclePort(out Status: SYSINT);
begin
  DefaultInterface.CyclePort(Status);
end;

procedure TUSBIOInterface3.GetStatus(out StatusValue: SYSINT; Recipient: SYSINT; Index: SYSINT; 
                                     out Status: SYSINT);
begin
  DefaultInterface.GetStatus(StatusValue, Recipient, Index, Status);
end;

procedure TUSBIOInterface3.GetCurrentFrameNumber(out FrameNumber: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetCurrentFrameNumber(FrameNumber, Status);
end;

function  TUSBIOInterface3.ErrorText(Status: SYSINT): WideString;
begin
  Result := DefaultInterface.ErrorText(Status);
end;

procedure TUSBIOInterface3.Bind(EndpointAddress: Byte; out Status: SYSINT);
begin
  DefaultInterface.Bind(EndpointAddress, Status);
end;

procedure TUSBIOInterface3.Unbind(out Status: SYSINT);
begin
  DefaultInterface.Unbind(Status);
end;

procedure TUSBIOInterface3.StartReading(SizeOfBuffer_IsoFramesInBuffer: SYSINT; 
                                        NumberOfBuffers: SYSINT; MaxErrorCount: SYSINT; 
                                        out Status: SYSINT);
begin
  DefaultInterface.StartReading(SizeOfBuffer_IsoFramesInBuffer, NumberOfBuffers, MaxErrorCount, 
                                Status);
end;

procedure TUSBIOInterface3.ReadData(var Buffer: PSafeArray; out ByteCount: SYSINT; 
                                    out Status: SYSINT);
begin
  DefaultInterface.ReadData(Buffer, ByteCount, Status);
end;

procedure TUSBIOInterface3.ReadIsoData(var Buffer: PSafeArray; out ByteCount: SYSINT; 
                                       var SubBufferLength_ErrorCode: PSafeArray; out Status: SYSINT);
begin
  DefaultInterface.ReadIsoData(Buffer, ByteCount, SubBufferLength_ErrorCode, Status);
end;

procedure TUSBIOInterface3.StopReading;
begin
  DefaultInterface.StopReading;
end;

procedure TUSBIOInterface3.StartWriting(SizeOfBuffer_IsoFramesInBuffer: SYSINT; 
                                        NumberOfBuffers: SYSINT; MaxErrorCount: SYSINT; 
                                        WriteStatus: Integer; out Status: SYSINT);
begin
  DefaultInterface.StartWriting(SizeOfBuffer_IsoFramesInBuffer, NumberOfBuffers, MaxErrorCount, 
                                WriteStatus, Status);
end;

procedure TUSBIOInterface3.WriteData(var Buffer: PSafeArray; UserId: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.WriteData(Buffer, UserId, Status);
end;

procedure TUSBIOInterface3.GetWriteStatus(out UserId: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetWriteStatus(UserId, Status);
end;

procedure TUSBIOInterface3.WriteIsoData(var Buffer: PSafeArray; var SubBufferLength: PSafeArray; 
                                        UserId: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.WriteIsoData(Buffer, SubBufferLength, UserId, Status);
end;

procedure TUSBIOInterface3.GetIsoWriteStatus(out UserId: SYSINT; var StatusArray: PSafeArray; 
                                             out FrameCount: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetIsoWriteStatus(UserId, StatusArray, FrameCount, Status);
end;

procedure TUSBIOInterface3.StopWriting;
begin
  DefaultInterface.StopWriting;
end;

procedure TUSBIOInterface3.ResetPipe(out Status: SYSINT);
begin
  DefaultInterface.ResetPipe(Status);
end;

procedure TUSBIOInterface3.AbortPipe(out Status: SYSINT);
begin
  DefaultInterface.AbortPipe(Status);
end;

procedure TUSBIOInterface3.EnablePnPNotification(const Guid: WideString; out Status: SYSINT);
begin
  DefaultInterface.EnablePnPNotification(Guid, Status);
end;

procedure TUSBIOInterface3.DisablePnPNotification(const Guid: WideString; out Status: SYSINT);
begin
  DefaultInterface.DisablePnPNotification(Guid, Status);
end;

procedure TUSBIOInterface3.GetBandwidthInfo(out TotalBandwidth: SYSINT; 
                                            out ConsumedBandwidth: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.GetBandwidthInfo(TotalBandwidth, ConsumedBandwidth, Status);
end;

procedure TUSBIOInterface3.SetupPipeStatistics(AveragingInterval: SYSINT; out Status: SYSINT);
begin
  DefaultInterface.SetupPipeStatistics(AveragingInterval, Status);
end;

procedure TUSBIOInterface3.QueryPipeStatistics(out ActualAveragingInterval: SYSINT; 
                                               out AverageRate: SYSINT; 
                                               out BytesTransferred_L: SYSINT; 
                                               out BytesTransferred_H: SYSINT; 
                                               out RequestsSucceeded: SYSINT; 
                                               out RequestsFailed: SYSINT; Flags: SYSINT; 
                                               out Status: SYSINT);
begin
  DefaultInterface.QueryPipeStatistics(ActualAveragingInterval, AverageRate, BytesTransferred_L, 
                                       BytesTransferred_H, RequestsSucceeded, RequestsFailed, 
                                       Flags, Status);
end;

procedure TUSBIOInterface3.AcquireDevice(out Status: SYSINT);
begin
  DefaultInterface.AcquireDevice(Status);
end;

procedure TUSBIOInterface3.ReleaseDevice(out Status: SYSINT);
begin
  DefaultInterface.ReleaseDevice(Status);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUSBIOInterface3Properties.Create(AServer: TUSBIOInterface3);
begin
  inherited Create;
  FServer := AServer;
end;

function TUSBIOInterface3Properties.GetDefaultInterface: IUSBIOInterface3;
begin
  Result := FServer.DefaultInterface;
end;

function  TUSBIOInterface3Properties.Get_DevicePathName: WideString;
begin
  Result := DefaultInterface.Get_DevicePathName;
end;

function  TUSBIOInterface3Properties.Get_IsCheckedBuild: Integer;
begin
  Result := DefaultInterface.Get_IsCheckedBuild;
end;

function  TUSBIOInterface3Properties.Get_IsDemoVersion: Integer;
begin
  Result := DefaultInterface.Get_IsDemoVersion;
end;

function  TUSBIOInterface3Properties.Get_IsLightVersion: Integer;
begin
  Result := DefaultInterface.Get_IsLightVersion;
end;

function  TUSBIOInterface3Properties.Get_DeviceOptions: SYSINT;
begin
  Result := DefaultInterface.Get_DeviceOptions;
end;

procedure TUSBIOInterface3Properties.Set_DeviceOptions(Options: SYSINT);
begin
  DefaultInterface.Set_DeviceOptions(Options);
end;

function  TUSBIOInterface3Properties.Get_DeviceRequestTimeout: SYSINT;
begin
  Result := DefaultInterface.Get_DeviceRequestTimeout;
end;

procedure TUSBIOInterface3Properties.Set_DeviceRequestTimeout(pVal: SYSINT);
begin
  DefaultInterface.Set_DeviceRequestTimeout(pVal);
end;

function  TUSBIOInterface3Properties.Get_ShortTransferOK: Integer;
begin
  Result := DefaultInterface.Get_ShortTransferOK;
end;

procedure TUSBIOInterface3Properties.Set_ShortTransferOK(ShortTransfer: Integer);
begin
  DefaultInterface.Set_ShortTransferOK(ShortTransfer);
end;

function  TUSBIOInterface3Properties.Get_EndpointFifoSize: SYSINT;
begin
  Result := DefaultInterface.Get_EndpointFifoSize;
end;

function  TUSBIOInterface3Properties.Get_IsOperatingAtHighSpeed: Integer;
begin
  Result := DefaultInterface.Get_IsOperatingAtHighSpeed;
end;

function  TUSBIOInterface3Properties.Get_OpenCount: SYSINT;
begin
  Result := DefaultInterface.Get_OpenCount;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ActiveX',[TUSBIOInterface3]);
end;

end.
