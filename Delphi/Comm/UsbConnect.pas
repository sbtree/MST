unit UsbConnect;

interface
uses USBIOCOMLib_TLB, Classes, SysUtils, StrUtils, Windows, Forms, ActiveX, USBSPEC;

type
  TUsbConnect=class(TComponent)
  protected
    t_usbio: TUSBIOInterface3;
    i_status: integer;
  protected
    procedure  CopyByteData (const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
  public
    constructor Create(owner: TComponent);
    destructor Destroy; override;
    function Init: boolean;

    procedure ReadComplete(sender: TObject; var obj: OleVariant);
    procedure WriteComplete(sender: TObject; var obj: OleVariant);
    procedure WriteStatusAvailable(sender: TObject; var obj: OleVariant);
  end;

implementation
const
  C_USB_GUID_ARS2000_STR : string = '{6CC88F5A-EA80-4707-845B-D3CF7BDBCA6C}';
  C_USB_GUID_ARS2000_REC : TGUID = (D1:$6CC88F5A; D2:$EA80; D3:$4707; D4:($84, $5B, $D3, $CF, $7B, $DB, $CA, $6C));

procedure Delay (Delaytime : Dword);	// Delaytime in Millisekunden
var
  ticks : DWord;
begin
  ticks := GetTickCount() + DelayTime;
  repeat Application.ProcessMessages;
  until (GetTickCount() >= Ticks);
end;

procedure TUsbConnect.copyByteData ( const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
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

constructor TUsbConnect.Create(owner: TComponent);
begin
	inherited Create(owner);
  t_usbio := TUSBIOInterface3.Create(self);
  t_usbio.OnReadComplete := ReadComplete;
  t_usbio.OnWriteComplete := WriteComplete;
  t_usbio.OnWriteStatusAvailable := WriteStatusAvailable;
  i_status := integer(USBIO_ERR_SUCCESS);
end;

destructor TUsbConnect.Destroy;
begin
  t_usbio.OnWriteStatusAvailable := Nil;
  t_usbio.OnWriteComplete := Nil;
  t_usbio.OnReadComplete := Nil;
  FreeAndNil(t_usbio);
	inherited Destroy;
end;

function TUsbConnect.Init: boolean;
var
  i_count, i_vstatus, i_size: integer;
  dev_descr     : USB_DEVICE_DESCRIPTOR;
  conf_descr    : USB_CONFIGURATION_DESCRIPTOR;
  str_descr     : USB_STRING_DESCRIPTOR;
  pa_descr : PSafeArray ;
  str_dev, str_serial: string;
begin
  repeat
    t_usbio.EnumerateDevices(C_USB_GUID_ARS2000_STR,i_count);
    if (i_count>=1) then
    begin
      t_usbio.OpenDevice(0, i_status);
      t_usbio.GetStatus(i_vstatus,0, 0,i_status);
      if ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPEN))then
      begin
        i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetConfigurationDescriptor (  pa_descr, i_size, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@conf_descr));
        SafeArrayDestroy(pa_descr);

        i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetDeviceDescriptor(  pa_descr, i_size, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@dev_descr));
        SafeArrayDestroy(pa_descr);

        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetStringDescriptor (  pa_descr, i_size, dev_descr.iProduct, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@str_descr));
        SafeArrayDestroy(pa_descr);
        str_dev := WideCharToString(str_descr.bString);

        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetStringDescriptor (  pa_descr, i_size, dev_descr.iSerialNumber, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@str_descr));
        SafeArrayDestroy(pa_descr);
        str_serial := WideCharToString(str_descr.bString);
      end
      else
      begin
        t_usbio.CyclePort(i_status);
        Delay(500);
        i_count := 0;
      end;
    end;
  until ((i_status = USBIO_ERR_SUCCESS) and (i_count >=1));

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

procedure TUsbConnect.ReadComplete(sender: TObject; var obj: OleVariant);
begin
end;

procedure TUsbConnect.WriteComplete(sender: TObject; var obj: OleVariant);
begin
end;

procedure TUsbConnect.WriteStatusAvailable(sender: TObject; var obj: OleVariant);
begin
end;

end.
