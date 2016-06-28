unit USB;

interface
uses USBIOCOMLib_TLB, USBSPEC, Classes, SysUtils, StrUtils, Windows, ActiveX, ConnBase;

type
  TMtxUsb=class(TConnBase)
  protected
    t_usbio:    TUSBIOInterface3;
    i_status:   integer;
    r_devdesc:  USB_DEVICE_DESCRIPTOR;
    r_cfgdesc:  USB_CONFIGURATION_DESCRIPTOR;
    r_intfdesc: USB_INTERFACE_DESCRIPTOR;
  protected
    function IsConnected(): boolean; override;
    function OpenDevice(const prodid: integer = -1): boolean;
    function Init(): boolean;
    procedure CopyByteData (const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
    procedure ReadComplete(sender: TObject; var obj: OleVariant);
    procedure WriteComplete(sender: TObject; var obj: OleVariant);
    procedure WriteStatusAvailable(sender: TObject; var obj: OleVariant);

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

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

function TMtxUsb.OpenDevice(const prodid: integer): boolean;
var i, i_count, i_vstatus, i_size: integer; c_tend: cardinal;
    pa_descr : PSafeArray ; r_strdesc: USB_STRING_DESCRIPTOR;
    s_prodid: string; i_prodid: integer;
begin
  result := false;

  c_tend := GetTickCount() + c_timeout;
  repeat
    t_usbio.EnumerateDevices(CSTR_MTXUSB_ARS2000_GUID,i_count);
    if (i_count > 0) then begin
      if (prodid = -1) then begin
        t_usbio.OpenDevice(0, i_status);
        result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPEN));
      end else begin
        for i := 0 to i_count - 1 do begin
          t_usbio.OpenDevice(i, i_status);
          if ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPEN))then
          begin
            i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
            pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
            t_usbio.GetDeviceDescriptor(  pa_descr, i_size, i_status);
            if (i_status = USBIO_ERR_SUCCESS) then begin
              CopyByteData(pa_descr, 0, i_size, PByteArray(@r_devdesc));
              i_size := SizeOf(USB_STRING_DESCRIPTOR);
              pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
              t_usbio.GetStringDescriptor(pa_descr, i_size, r_devdesc.iProduct, 0, i_status);
              if (i_status = USBIO_ERR_SUCCESS) then begin
                 CopyByteData (pa_descr, 0, i_size, PByteArray(@r_strdesc));
                 s_prodid := WideCharToString(r_strdesc.bString);
                 if TryStrToInt(s_prodid, i_prodid) then result := (i_prodid = prodid);
              end;
            end;
            SafeArrayDestroy(pa_descr);
            if (not result) then t_usbio.CloseDevice();
          end;
        end;
      end;
    end else result := false;
    Application.ProcessMessages();
  until ((GetTickCount() < c_tend) and (not b_break) and (not result));
end;

function TMtxUsb.Init: boolean;
var
  i_count, i_vstatus, i_size: integer;
  conf_descr    : USB_CONFIGURATION_DESCRIPTOR;
  str_descr     : USB_STRING_DESCRIPTOR;
  pa_descr : PSafeArray ;
  str_dev, str_serial: string;
begin
 
  repeat
    t_usbio.EnumerateDevices(CSTR_MTXUSB_ARS2000_GUID,i_count);
    if (i_count>=1) then
    begin
      t_usbio.OpenDevice(0, i_status);
      t_usbio.GetStatus(i_vstatus,0, 0,i_status);
      if ((i_status = USBIO_ERR_SUCCESS) or (i_status = USBIO_ERR_DEVICE_ALREADY_OPEN))then
      begin
        i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetDeviceDescriptor(  pa_descr, i_size, i_status);
        CopyByteData(pa_descr, 0, i_size, PByteArray(@r_devdesc));
        SafeArrayDestroy(pa_descr);

        i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetConfigurationDescriptor (  pa_descr, i_size, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@conf_descr));
        SafeArrayDestroy(pa_descr);


        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetStringDescriptor (  pa_descr, i_size, r_devdesc.iProduct, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@str_descr));
        SafeArrayDestroy(pa_descr);
        str_dev := WideCharToString(str_descr.bString);

        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        pa_descr := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetStringDescriptor (  pa_descr, i_size, r_devdesc.iSerialNumber, 0, i_status );
        CopyByteData(pa_descr, 0, i_size, PByteArray(@str_descr));
        SafeArrayDestroy(pa_descr);
        str_serial := WideCharToString(str_descr.bString);
      end
      else
      begin
        t_usbio.CyclePort(i_status);
        TGenUtils.Delay(500);
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

constructor TMtxUsb.Create(owner: TComponent);
begin
	inherited Create(owner);
  t_usbio := TUSBIOInterface3.Create(self);
  t_connobj := t_usbio;
  e_type := CT_USB;
  t_usbio.OnReadComplete := ReadComplete;
  t_usbio.OnWriteComplete := WriteComplete;
  t_usbio.OnWriteStatusAvailable := WriteStatusAvailable;
  if (t_usbio.IsDemoVersion <> 0) then MessageBox(0, 'USBIO is demo.', '', MB_ICONWARNING);
  e_state := CS_UNKNOWN;
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

function TMtxUsb.Config(const sconfs: TStrings): boolean;
begin
  result := false;
  //todo:
end;

function TMtxUsb.Connect(): boolean;
begin
  result := Init();
  if result then t_usbio.Connect();
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
