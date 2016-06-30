unit USB;

interface
uses USBIOCOMLib_TLB, USBSPEC, Classes, SysUtils, StrUtils, Windows, ActiveX, ConnBase;

type

  RUsbInterface = packed record
    InterfDescriptor : PUSB_INTERFACE_DESCRIPTOR;         // InterfaceDescriptor
    EndPoints        : array of PUSB_ENDPOINT_DESCRIPTOR; // Array mit den unterstützten Endpunkten
  end;

  RUsbConfiguration = packed record
    DataBuffer        : array of byte;
    ConfigDescriptor  : PUSB_CONFIGURATION_DESCRIPTOR;
    UsbSpeed          : string;
    InterfDescriptors : array of RUsbInterface;
  end; // TUsbConfigDescriptor = packed record

  RUsbDeviceConfig = packed record
    DeviceDescriptor  : USB_DEVICE_DESCRIPTOR;
    Manufacturer      : string;
    ProductType       : string;
    SerialNumber      : string;
    ConfigDescriptors : array of RUsbConfiguration;
  end; // TUsbConfigDescriptor = packed record


  TMtxUsb=class(TConnBase)
  protected
    t_usbio:    TUSBIOInterface3;
    t_usbepin:  TUSBIOInterface3;  //endpoint for reading from usb-device
    t_usbepout: TUSBIOInterface3;  //endpoint for writing to usb-device
    i_status:   integer;
    w_vid:      word;
    w_pid:      word;
    i_prodsn:   integer;
    i_curdev:   integer; //index of the found device
    r_devconf:  RUsbDeviceConfig;
//    r_devdesc:  USB_DEVICE_DESCRIPTOR;
//    r_cfgdesc:  USB_CONFIGURATION_DESCRIPTOR;
//    r_intfdesc: USB_INTERFACE_DESCRIPTOR;
//    r_epindesc: USB_ENDPOINT_DESCRIPTOR;
//    r_epoutdesc:USB_ENDPOINT_DESCRIPTOR;
  protected
    function IsConnected(): boolean; override;
    function FindDevice( const sn: integer; const pid: word; const vid: word): boolean; overload;
    function GetDeviceConfig(): boolean;
    function Init(): boolean;
    function Uninit(): boolean;
    procedure CopyByteData (const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
    procedure ReadComplete(sender: TObject; var obj: OleVariant);
    procedure WriteComplete(sender: TObject; var obj: OleVariant);
    procedure WriteStatusAvailable(sender: TObject; var obj: OleVariant);

    procedure OnAddDevice(Sender: TObject; var Obj: OleVariant);
    procedure OnRemoveDevice(Sender: TObject; var Obj: OleVariant);
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property VendorID: word read w_vid write w_vid;
    property ProductID: word read w_pid write w_pid;
    property ProductSN: integer read i_prodsn write i_prodsn;

    function FindDevice(): boolean; overload;
    function Config(const sconfs: TStrings): boolean; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
    function SendBuf(const buf: PChar; const len: longword): boolean; override;
    function RecvBuf(var buf: PChar; const len: longword): integer; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = false): integer; override;

    function RecvStrTimeout(var str: string; const tend: cardinal): integer; override;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = CINT_RECV_INTERVAL): integer; override;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; override;
    function WaitForReading(const tend: cardinal): boolean; override;
  end;

implementation
uses GenUtils, Forms;

const
  CSTR_MTXUSB_ARS2000_GUID : string = '{6CC88F5A-EA80-4707-845B-D3CF7BDBCA6C}';
  CREC_MTXUSB_ARS2000_GUID : TGUID = (D1:$6CC88F5A; D2:$EA80; D3:$4707; D4:($84, $5B, $D3, $CF, $7B, $DB, $CA, $6C));

procedure TMtxUsb.CopyByteData ( const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
var
  li_inx_dest : longint;
  li_inx_src  : longint;
  byte_tmp    : byte;
begin
  li_inx_src  := li_offset;
  li_inx_dest := 0;
  while ( li_inx_src < (li_offset+li_size) ) do
  begin
    SafeArrayGetElement ( psarr, li_inx_src, byte_tmp );
    pbarr^[li_inx_dest] := byte_tmp;
    inc ( li_inx_src  );
    inc ( li_inx_dest );
  end;
end;

function TMtxUsb.FindDevice( const sn: integer; const pid, vid: word): boolean;
var i, i_count, i_size, i_psnr: integer; s_prodsn: string;
    c_tend: cardinal; pa_descr : PSafeArray ; r_strdesc: USB_STRING_DESCRIPTOR;
begin
  result := false; i_curdev := -1;
  c_tend := GetTickCount() + c_timeout;
  repeat
    t_usbio.EnumerateDevices(CSTR_MTXUSB_ARS2000_GUID,i_count);
    for i := 0 to i_count - 1 do begin
      t_usbio.OpenDevice(i, i_status);
      if ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPEN))then begin
        i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetDeviceDescriptor(pa_descr, i_size, i_status);
        if (i_status = USBIO_ERR_SUCCESS) then begin
          CopyByteData(pa_descr, 0, i_size, PByteArray(@r_devconf.DeviceDescriptor));
          if ((r_devconf.DeviceDescriptor.idVendor = vid) and (r_devconf.DeviceDescriptor.idProduct = pid)) then begin
            if (sn = -1) then result := true
            else begin
              i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
              pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
              t_usbio.GetDeviceDescriptor(  pa_descr, i_size, i_status);
              if (i_status = USBIO_ERR_SUCCESS) then begin
                CopyByteData(pa_descr, 0, i_size, PByteArray(@r_devconf.DeviceDescriptor));
                i_size := SizeOf(USB_STRING_DESCRIPTOR);
                FillChar(r_strdesc.bString, Length(r_strdesc.bString), $00);
                pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
                t_usbio.GetStringDescriptor(pa_descr, i_size, r_devconf.DeviceDescriptor.iSerialNumber, 0, i_status);
                if (i_status = USBIO_ERR_SUCCESS) then begin
                  CopyByteData (pa_descr, 0, i_size, PByteArray(@r_strdesc));
                  r_devconf.SerialNumber := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
                  if TryStrToInt(r_devconf.SerialNumber, i_psnr) then  result := (i_psnr = sn);
                end;

                i_size := SizeOf(USB_STRING_DESCRIPTOR);
                FillChar(r_strdesc.bString, Length(r_strdesc.bString), $00);
                t_usbio.GetStringDescriptor(pa_descr, i_size, r_devconf.DeviceDescriptor.iManufacturer, 0, i_status);
                if (i_status = USBIO_ERR_SUCCESS) then r_devconf.Manufacturer := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));

                i_size := SizeOf(USB_STRING_DESCRIPTOR);
                FillChar(r_strdesc.bString, Length(r_strdesc.bString), $00);
                t_usbio.GetStringDescriptor(pa_descr, i_size, r_devconf.DeviceDescriptor.iProduct, 0, i_status);
                if (i_status = USBIO_ERR_SUCCESS) then r_devconf.ProductType := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
              end;
              SafeArrayDestroy(pa_descr);
            end;
          end;
        end;
      end;
      if result then begin
        i_curdev := i;
        GetDeviceConfig();
        break;
      end else t_usbio.CloseDevice();
    end;
    Application.ProcessMessages();
  until ((GetTickCount() >= c_tend) or b_break or result);
end;

function TMtxUsb.GetDeviceConfig(): boolean;
var i, j, k, i_size, i_offset: integer;
    pa_descr1, pa_descr2: PSafeArray ;
    r_conf:  USB_CONFIGURATION_DESCRIPTOR;
begin
  result := false;
  SetLength(r_devconf.ConfigDescriptors, r_devconf.DeviceDescriptor.bNumConfigurations);
  for i := 0 to r_devconf.DeviceDescriptor.bNumConfigurations - 1 do begin
    i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
    pa_descr1 := SafeArrayCreateVector(VT_UI1, 0, i_size);
    t_usbio.GetConfigurationDescriptor(pa_descr1, i_size, 0, i_status);
    result := (i_status = USBIO_ERR_SUCCESS);
    if result then begin
      CopyByteData(pa_descr1, 0, i_size, PByteArray(@r_conf));
      i_size := r_conf.wTotalLength;
      pa_descr2 := SafeArrayCreateVector(VT_UI1, 0, i_size);
      t_usbio.GetConfigurationDescriptor(pa_descr2, i_size, 0, i_status);
      result := (i_status = USBIO_ERR_SUCCESS);
      if result then begin
        SetLength(r_devconf.ConfigDescriptors[i].DataBuffer, i_size);
        CopyByteData(pa_descr1, 0, i_size, PByteArray(@r_devconf.ConfigDescriptors[i].DataBuffer));
        SetLength(r_devconf.ConfigDescriptors[i].InterfDescriptors, r_conf.bNumInterfaces);
        for j := 0 to r_conf.bNumInterfaces - 1 do begin
        //todo:
        end;
      end;
      SafeArrayDestroy(pa_descr2);
    end;
    SafeArrayDestroy(pa_descr1);
  end;
end;

function TMtxUsb.Init: boolean;
var
  i_count, i_vstatus, i_size: integer;
  conf_descr    : USB_CONFIGURATION_DESCRIPTOR;
  str_descr     : USB_STRING_DESCRIPTOR;
  pa_descr : PSafeArray ;
  str_dev, str_serial: string;
begin
  result := false;
  if (i_curdev >= 0) then begin
    //todo: 1. open device
    t_usbio.OpenDevice(i_curdev, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPENED));

    //todo: 2. set configuration
    i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
    pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
    t_usbio.GetConfigurationDescriptor (  pa_descr, i_size, 0, i_status );
    CopyByteData(pa_descr, 0, i_size, PByteArray(@conf_descr));
    SafeArrayDestroy(pa_descr);

    //todo: 3. add interface

    //todo: 4. bind endpoint for reading from usb-device
    t_usbepin.OpenDevice(i_curdev, i_status);
    result := (i_status = USBIO_ERR_SUCCESS);
    //t_usbepin.Bind();

    //todo: 5. bind endpoint for writing to usb-device
    t_usbepout.OpenDevice(i_curdev, i_status);
  end;
  if FindDevice() then begin
    i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
    pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
    t_usbio.GetConfigurationDescriptor (  pa_descr, i_size, 0, i_status );
    CopyByteData(pa_descr, 0, i_size, PByteArray(@conf_descr));
    SafeArrayDestroy(pa_descr);
  end;

    i_vstatus := USBIOCOM_DevicePowerStateD0;
    t_usbio.GetDevicePowerState(i_vstatus, i_status);
    i_vstatus := USBIOCOM_DevicePowerStateD1;
    t_usbio.GetDevicePowerState(i_vstatus, i_status);
    i_vstatus := USBIOCOM_DevicePowerStateD2;
    t_usbio.GetDevicePowerState(i_vstatus, i_status);
    i_vstatus := USBIOCOM_DevicePowerStateD3;
    t_usbio.GetDevicePowerState(i_vstatus, i_status);

    if (i_status = USBIO_ERR_SUCCESS) then t_usbio.SetConfiguration(0,i_status);

    if (i_status = USBIO_ERR_SUCCESS) then t_usbio.Bind(0,i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPENED));
end;

function TMtxUsb.Uninit(): boolean;
begin

end;

function TMtxUsb.IsConnected(): boolean;
begin
  result := false;
end;

procedure TMtxUsb.ReadComplete(sender: TObject; var obj: OleVariant);
begin
end;

procedure TMtxUsb.WriteComplete(sender: TObject; var obj: OleVariant);
begin
end;

procedure TMtxUsb.WriteStatusAvailable(sender: TObject; var obj: OleVariant);
begin
end;

procedure TMtxUsb.OnAddDevice(Sender: TObject; var Obj: OleVariant);
begin
  if (i_curdev < 0) then begin
    if FindDevice() then begin


    end;
  end;
end;

procedure TMtxUsb.OnRemoveDevice(Sender: TObject; var Obj: OleVariant);
var i_status: integer;
begin
  if (i_curdev >= 0) then begin
    t_usbepin.StopReading();
    t_usbepin.Unbind(i_status);

    t_usbepout.StopWriting();
    t_usbepout.Unbind(i_status);
  end;
end;

constructor TMtxUsb.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_type := CT_USB;
  e_state := CS_UNKNOWN;
  w_vid := $1B97;
  w_pid := $2;
  i_prodsn := -1;

  t_usbio := TUSBIOInterface3.Create(self);
  t_connobj := t_usbio;
  t_usbio.OnPnPAddNotification := OnAddDevice;
  t_usbio.OnPnPRemoveNotification := OnRemoveDevice;
  t_usbio.EnablePnPNotification(CSTR_MTXUSB_ARS2000_GUID, i_status);
  t_usbepin := TUSBIOInterface3.Create(self);
  t_usbepin.OnReadComplete := ReadComplete;
  t_usbepout := TUSBIOInterface3.Create(self);
  t_usbepout.OnWriteComplete := WriteComplete;
end;

destructor TMtxUsb.Destroy;
begin
  t_usbio.OnWriteStatusAvailable := Nil;
  t_usbio.OnWriteComplete := Nil;
  t_usbio.OnReadComplete := Nil;
  FreeAndNil(t_usbio);
  t_connobj := nil;
	inherited Destroy;
end;

function TMtxUsb.FindDevice(): boolean;
begin
  result := FindDevice(i_prodsn, w_pid, w_vid);
end;

function TMtxUsb.Config(const sconfs: TStrings): boolean;
begin
  result := false;
  //todo:
end;

function TMtxUsb.Connect(): boolean;
begin
  result := FindDevice();
  if result then begin
    result := Init();
    t_usbio.Connect();
  end;
end;

function TMtxUsb.Disconnect: boolean; 
begin
  result := false;
  //todo:
end;

function TMtxUsb.SendBuf(const buf: PChar; const len: longword): boolean; 
begin
  result := false;
  //todo:
end;

function TMtxUsb.RecvBuf(var buf: PChar; const len: longword): integer;
begin
  result := 0;
  //todo:
end;

function TMtxUsb.SendStr(const str: string): boolean; 
begin
  result := false;
  //todo:
end;

function TMtxUsb.RecvStr(var str: string; const bwait: boolean = false): integer; 
begin
  result := 0;
  //todo:
end;

function TMtxUsb.RecvStrTimeout(var str: string; const tend: cardinal): integer;
begin
  result := 0;
  //todo:
end;

function TMtxUsb.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
begin
  result := 0;
  //todo:
end;

function TMtxUsb.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean): integer;
begin
  result := 0;
  //todo:
end;

function TMtxUsb.WaitForReading(const tend: cardinal): boolean; 
begin
  result := false;
  //todo:
end;

end.
