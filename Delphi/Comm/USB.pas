unit USB;

interface
uses USBIOCOMLib_TLB, USBSPEC, Classes, SysUtils, StrUtils, Windows, ActiveX, ConnBase;
const
  C_USB_BUFFER_SIZE = 1024;
  
type
  EMtxUsbProperty = ( MUP_VID,
                      MUP_PID,
                      MUP_PSN
                    );

  RMtxUsbConfigDesc = packed record
    ConfigDescriptor    : USB_CONFIGURATION_DESCRIPTOR;
    InterfaceDescriptor : USB_INTERFACE_DESCRIPTOR;
    EndpointIn          : USB_ENDPOINT_DESCRIPTOR;
    EndpointOut         : USB_ENDPOINT_DESCRIPTOR;
  end;

  RMtxUsbDeviceConfig = packed record
    DeviceDescriptor  : USB_DEVICE_DESCRIPTOR;
    MtxUsbConfigDesc  : RMtxUsbConfigDesc;
    Manufacturer      : string;
    ProductType       : string;
    SerialNumber      : string;
    Configuration     : string;
  end;

  TMtxUsb=class(TConnBase)
  protected
    t_usbio:    TUSBIOInterface3; //intance for general setting of usb (EP0)
    t_usbrx:    TUSBIOInterface3; //endpoint for reading from usb-device (Pipe In)
    t_usbtx:    TUSBIOInterface3; //endpoint for writing to usb-device  (Pipe Out)
    i_status:   integer;
    w_vid:      word;             //vendor id of usb device
    w_pid:      word;             //product id of usb device
    i_psn:      integer;          //serial number of usb device for setting to connect
    i_actpsn:   integer;          //serial number of usb device, which is actually connected
    i_curdev:   integer;          //index of the found device
    r_devconf:  RMtxUsbDeviceConfig;    //record of usb device configuration
  protected
    function DetectDevice(const sn: integer; const pid: word; const vid: word): boolean;
    function GetDeviceConfig(): boolean;
    function GetProductSN(): integer;
    function Init(const psn: integer): boolean;
    function Uninit(): boolean;
    function SetProperty(const eprop: EMtxUsbProperty; const sval: string): boolean;
    function SetDeviceVid(const sval: string): boolean;
    function SetDevicePid(const sval: string): boolean;
    function SetProductSN(const sval: string): boolean;
    function ConnectTo(const psn: integer): boolean;
    function IsReadComplete(): boolean; override;
    function IsWriteComplete(): boolean; override;
    function SendData(const buf: PChar; len: word): boolean; override;
    function RecvData(): boolean; override;
    procedure TryConnect(); override;
    procedure SafeArrayToArray(const psarr: PSafeArray; const parr: PByteArray; const size: Integer);
    procedure ClearDeviceInfo();

    procedure OnReadComplete(sender: TObject; var obj: OleVariant);
    procedure OnWriteComplete(sender: TObject; var obj: OleVariant);
    procedure OnWriteStatusAvailable(sender: TObject; var obj: OleVariant);
    procedure OnAddDevice(Sender: TObject; var Obj: OleVariant);
    procedure OnRemoveDevice(Sender: TObject; var Obj: OleVariant);
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property VendorID: word read w_vid write w_vid;
    property ProductID: word read w_pid write w_pid;
    property ProductSN: integer read GetProductSN write i_psn;

    function FindDevice(const psn: integer = -1; const tout: integer = -1): boolean;
    function Config(const sconfs: TStrings): boolean; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
  end;

implementation
uses GenUtils, Forms, TextMessage, SyncObjs;

const
  CSTR_MTXUSB_ARS2000_GUID: string = '{6CC88F5A-EA80-4707-845B-D3CF7BDBCA6C}';
  CREC_MTXUSB_ARS2000_GUID: TGUID = (D1:$6CC88F5A; D2:$EA80; D3:$4707; D4:($84, $5B, $D3, $CF, $7B, $DB, $CA, $6C));
  C_USB_RX_BUFFER_SIZE = C_USB_BUFFER_SIZE; // Größe des USB-Empfangspuffers
  C_USB_TX_BUFFER_SIZE = C_USB_BUFFER_SIZE; // Größe des USB-Sendepuffers
                                // ACHTUNG: Der Puffer muss größer sein als die
                                // im Servo hinterlegte Fifo-Größe, da das Gerät
                                // an USB 1.1 und USB 2.0 angeschlossen werden kann.
                                // Bei USB 1.1 können nur bis zu 64 Bytes pro
                                // Datenpaket übertragen werden. Bei der Benutzung
                                // der Fifo-Größe kommen die Oszi-Daten in mehreren
                                // Paketen an. Deshalb wird die Puffer-Größe deutlich
                                // höher gesetzt, damit die Fragmentierung von
                                // langen Daten bei USB 1.1 vom Treiber übernommen wird.
                                // Die Sendeseite muss nach dem letzten Datenpaket ein
                                // Nullpaket schicken, damit die Datenübertragung als
                                // abgeschlossen erkannt wird.
  C_USB_MAX_TX_ERRORS  = 5;     // Maximale Anzahl von Fehlern, die beim Senden von Daten
                                // auftreten dürfen. Wird diese Anzahl erreicht,
                                // so wird der interne Worker-Thread angehalten.
                                // (s. TUSBIOInterface3.StartWriting)
  C_USB_CNT_TX_BUFFERS = 16;     // Anzahl Sendepuffer
  C_USB_MAX_RX_ERRORS  = 5;     // Maximale Anzahl von Fehlern, die beim Datenempfang
                               // auftreten dürfen. Wird diese Anzahl erreicht,
                               // so wird der interne Worker-Thread angehalten.
                               // (s. TUSBIOInterface3.StartWriting)
  C_USB_CNT_RX_BUFFERS = 16;    // Anzahl Empfangspuffer
  CSTR_MTXUSB_PROPERTIES: array[EMtxUsbProperty] of string = (
                          'VID',
                          'PID',
                          'PSN'
                          );


procedure TMtxUsb.SafeArrayToArray(const psarr: PSafeArray; const parr: PByteArray; const size: Integer);
var i, i_idx, i_ubound, i_lbound: integer; byte_val: byte;
begin
  if ((SafeArrayGetUBound(psarr, 1, i_ubound) = S_OK) and (SafeArrayGetLBound(psarr, 1, i_lbound) = S_OK)) then begin
    i_idx := 0;
    if (i_ubound >= size) then i_ubound := size;
    for i := i_lbound to i_ubound do begin
      if (SafeArrayGetElement(psarr, i, byte_val) = S_OK) then begin
        parr^[i_idx] := byte_val;
        inc(i_idx);
      end;
    end;
  end;
end;

function TMtxUsb.DetectDevice( const sn: integer; const pid, vid: word): boolean;
var i, i_count, i_size: integer;
    pa_descr1, pa_descr2: PSafeArray; r_strdesc: USB_STRING_DESCRIPTOR;
begin
  result := false;
  ClearDeviceInfo();
  t_usbio.EnumerateDevices(CSTR_MTXUSB_ARS2000_GUID,i_count);
  for i := 0 to i_count - 1 do begin
    t_usbio.OpenDevice(i, i_status);
    if ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_DEVICE_ALREADY_OPEN)))then begin
      //read device descriptor
      i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
      pa_descr1 := SafeArrayCreateVector ( VT_UI1, 0, i_size );
      t_usbio.GetDeviceDescriptor(pa_descr1, i_size, i_status);
      if (i_status = USBIO_ERR_SUCCESS) then begin
        SafeArrayToArray(pa_descr1, PByteArray(@r_devconf.DeviceDescriptor), i_size);
        //find out the device with expected vid and pid
        if ((r_devconf.DeviceDescriptor.idVendor = vid) and (r_devconf.DeviceDescriptor.idProduct = pid)) then begin
          //get serial number
          i_size := SizeOf(USB_STRING_DESCRIPTOR);
          ZeroMemory(@r_strdesc.bString, SizeOf(r_strdesc.bString));
          pa_descr2 := SafeArrayCreateVector ( VT_UI1, 0, i_size );
          t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iSerialNumber, 0, i_status);
          if (i_status = USBIO_ERR_SUCCESS) then begin
            SafeArrayToArray(pa_descr2, PByteArray(@r_strdesc), i_size);
            r_devconf.SerialNumber := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
            if (not TryStrToInt(r_devconf.SerialNumber, i_actpsn)) then i_actpsn := -1;
          end;
          if (sn = -1) then result := true
          else result := (i_actpsn = sn);
          //get information of manufacturer and product type
          if result then begin
            i_size := SizeOf(USB_STRING_DESCRIPTOR);
            ZeroMemory(@r_strdesc.bString, SizeOf(r_strdesc.bString));
            t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iManufacturer, 0, i_status);
            if (i_status = USBIO_ERR_SUCCESS) then begin
              SafeArrayToArray(pa_descr2, PByteArray(@r_strdesc), i_size);
              r_devconf.Manufacturer := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
            end;

            i_size := SizeOf(USB_STRING_DESCRIPTOR);
            ZeroMemory(@r_strdesc.bString, SizeOf(r_strdesc.bString));
            t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iProduct, 0, i_status);
            if (i_status = USBIO_ERR_SUCCESS) then begin
              SafeArrayToArray(pa_descr2, PByteArray(@r_strdesc), i_size);
              r_devconf.ProductType := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
            end;
          end;
          SafeArrayDestroy(pa_descr2);
        end;
      end;
      SafeArrayDestroy(pa_descr1);
    end;
    if result then begin
      i_curdev := i;
      GetDeviceConfig();
      break;
    end else t_usbio.CloseDevice();
  end;
end;

function TMtxUsb.GetDeviceConfig(): boolean;
var i_size: integer; pa_descr1, pa_descr2: PSafeArray ;
    r_conf:  USB_CONFIGURATION_DESCRIPTOR; r_strdesc: USB_STRING_DESCRIPTOR;
begin
  result := (r_devconf.DeviceDescriptor.bNumConfigurations = 1); //Metronix USB-Device has only one configuration
  if result then begin
    //get configuration descriptor firstly
    i_size := SizeOf(USB_CONFIGURATION_DESCRIPTOR);
    pa_descr1 := SafeArrayCreateVector(VT_UI1, 0, i_size);
    t_usbio.GetConfigurationDescriptor(pa_descr1, i_size, 0, i_status);
    result := (i_status = USBIO_ERR_SUCCESS);
    if result then begin
      SafeArrayToArray(pa_descr1, PByteArray(@r_conf), i_size);
      //get total data of the configuration, incl. Interface descriptor und endpoints
      i_size := r_conf.wTotalLength;
      pa_descr2 := SafeArrayCreateVector(VT_UI1, 0, i_size);
      t_usbio.GetConfigurationDescriptor(pa_descr2, i_size, 0, i_status);
      result := (i_status = USBIO_ERR_SUCCESS);
      if result then  SafeArrayToArray(pa_descr2, PByteArray(@r_devconf.MtxUsbConfigDesc), i_size);
      SafeArrayDestroy(pa_descr2);

      if result then begin
        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        ZeroMemory(@r_strdesc.bString, SizeOf(r_strdesc.bString));
        pa_descr2 := SafeArrayCreateVector(VT_UI1, 0, i_size);
        t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.MtxUsbConfigDesc.ConfigDescriptor.iConfiguration, 0, i_status);
        if (i_status = USBIO_ERR_SUCCESS) then begin
          SafeArrayToArray(pa_descr2, PByteArray(@r_strdesc), i_size);
          r_devconf.Configuration := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
        end;
      end;
    end;
    SafeArrayDestroy(pa_descr1);
  end;
end;

function TMtxUsb.GetProductSN(): integer;
begin
  if (i_actpsn = -1) then result := i_psn
  else result := i_actpsn;
end;

function TMtxUsb.Init(const psn: integer): boolean;
begin
  result := FindDevice(psn, 0);
  //1.add interface
  if result then begin
    t_usbio.AddInterface(r_devconf.MtxUsbConfigDesc.InterfaceDescriptor.bInterfaceNumber,
                         r_devconf.MtxUsbConfigDesc.InterfaceDescriptor.bAlternateSetting,
                         (C_USB_RX_BUFFER_SIZE * C_USB_CNT_RX_BUFFERS), i_status);
    result := (i_status = USBIO_ERR_SUCCESS);
  end;

  //2. set configuration
  if result then begin
    t_usbio.SetConfiguration(0, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_ALREADY_CONFIGURED)));
  end;

  //3. open and bind endpoint for reading from usb-device
  if result then begin
    t_usbrx.OpenDevice(i_curdev, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_DEVICE_ALREADY_OPENED)));
  end;
  if result then begin
    t_usbrx.Bind(r_devconf.MtxUsbConfigDesc.EndpointIn.bEndpointAddress, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_ALREADY_BOUND)));
  end;

  //4. open and bind endpoint for writing to usb-device
  if result then begin
    t_usbtx.OpenDevice(i_curdev, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_DEVICE_ALREADY_OPENED)));
  end;
  if result then begin
    t_usbtx.Bind(r_devconf.MtxUsbConfigDesc.EndpointOut.bEndpointAddress, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_ALREADY_BOUND)));
  end;

  //5. start reading and writing worker-thread
  if result then begin
    ZeroMemory(@ba_rbuf, length(ba_rbuf));
    t_usbrx.StartReading(C_USB_RX_BUFFER_SIZE, C_USB_CNT_RX_BUFFERS, C_USB_MAX_RX_ERRORS, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_THREAD_IS_RUNNING)));
    if result then begin
      t_usbrx.ResetPipe(i_status);
      t_usbtx.StartWriting( C_USB_TX_BUFFER_SIZE, C_USB_CNT_TX_BUFFERS, C_USB_MAX_TX_ERRORS, 0, i_status);
      result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_THREAD_IS_RUNNING)));
      if result then t_usbtx.ResetPipe(i_status);
    end;
  end;

  //update the state 
  if (result and (e_state in [CS_CONFIGURED, CS_CONNECTED])) then e_state := CS_CONNECTED;
end;

function TMtxUsb.Uninit(): boolean;
begin
  //5. stop reading and writing worker-thread
  t_usbtx.StopWriting();
  t_usbrx.StopReading();

  //4. & 3. unbind and close reading and writing pipe
  t_usbtx.CloseDevice();
  t_usbrx.CloseDevice();

  //2. unconfig usb device
  t_usbio.UnconfigureDevice(i_status);

  //1. remove interface
  t_usbio.DeleteInterfaces();

  //0. close EP0 and clear device info
  t_usbio.CloseDevice();
  ClearDeviceInfo();
  result := true;

  //set state to 'configured'
  if (e_state <> CS_UNKNOWN) then e_state := CS_CONFIGURED;
end;

function TMtxUsb.SetProperty(const eprop: EMtxUsbProperty; const sval: string): boolean;
begin
  result := false;
  case eprop of
    MUP_VID: result := SetDeviceVid(sval);
    MUP_PID: result := SetDevicePid(sval);
    MUP_PSN: result := SetProductSN(sval);
  end;
end;

function TMtxUsb.SetDeviceVid(const sval: string): boolean;
var i_val: integer;
begin
  if (sval = '') then result := true
  else begin
    result := TryStrToInt(sval, i_val);
    if result then w_vid := word(i_val);
  end;
end;

function TMtxUsb.SetDevicePid(const sval: string): boolean;
var i_val: integer;
begin
  if (sval = '') then result := true
  else begin
    result := TryStrToInt(sval, i_val);
    if result then w_pid := word(i_val);
  end;
end;

function TMtxUsb.SetProductSN(const sval: string): boolean;
var i_val: integer;
begin
  if (sval = '') then result := true
  else begin
    result := TryStrToInt(sval, i_val);
    if result then i_psn := i_val;
  end;
end;

function TMtxUsb.ConnectTo(const psn: integer): boolean;
begin
  if ((psn <> -1) and (psn <> i_actpsn)) then begin
    if IsConnected() then Disconnect();
    i_psn := psn;
  end;
  if (not IsConnected()) then TryConnect();
  result := IsConnected();
end;

function TMtxUsb.IsReadComplete(): boolean;
begin
  Application.ProcessMessages();
  result := (t_rxwait.WaitFor(0) = wrSignaled);
end;

function TMtxUsb.IsWriteComplete(): boolean;
begin
  Application.ProcessMessages();
  result := (t_txwait.WaitFor(0) = wrSignaled);
end;

function TMtxUsb.SendData(const buf: PChar; len: word): boolean;
var pa_send: PSafeArray; i: integer;
begin
  result := false;
  ClearBuffer();
  if (len > 0) then begin
    pa_send := SafeArrayCreateVector ( VT_UI1, 0, len );
    for i := 0 to len - 1 do SafeArrayPutElement(pa_send, i, buf[i]);
    t_usbtx.WriteData(pa_send, 0, i_status);
    result := (i_status = USBIO_ERR_SUCCESS);
    if (not result) then
      t_usbtx.ResetPipe(i_status);
    SafeArrayDestroy(pa_send);
  end;
end;

function TMtxUsb.RecvData(): boolean;
var pa_recv: PSafeArray; i_size: integer;
begin
  i_size := length(ba_rbuf);
  pa_recv := SafeArrayCreateVector ( VT_UI1, 0, C_USB_RX_BUFFER_SIZE );
  i_status := USBIO_ERR_SUCCESS;
  repeat  
    t_usbrx.ReadData(pa_recv, i_size, i_status);
    if (i_status = USBIO_ERR_SUCCESS) then begin
      SafeArrayToArray(pa_recv, PByteArray(@(ba_rbuf[w_rlen])), i_size);
      w_rlen := w_rlen + i_size;
    end else if (i_status <> integer(USBIO_ERR_NO_DATA)) then begin
      t_usbrx.ResetPipe(i_status);
      break;
    end;
  until (i_status = integer(USBIO_ERR_NO_DATA));
  SafeArrayDestroy (pa_recv);
  //It is possible to receive no data, if this function is already automatically caleld because of event ReadComplete
  result := (w_rlen > 0); 
end;

procedure TMtxUsb.TryConnect();
begin
  if (not Init(i_psn)) then
    Uninit();
end;

procedure TMtxUsb.ClearDeviceInfo();
begin
  i_curdev := -1;
  ZeroMemory(@r_devconf.MtxUsbConfigDesc, SizeOf(RMtxUsbConfigDesc));
  ZeroMemory(@r_devconf.DeviceDescriptor, SizeOf(USB_DEVICE_DESCRIPTOR));
  r_devconf.Manufacturer := '';
  r_devconf.ProductType := '';
  r_devconf.SerialNumber := '';
  r_devconf.Configuration := '';
end;

procedure TMtxUsb.OnReadComplete(sender: TObject; var obj: OleVariant);
begin
  t_rxwait.SetEvent();
  ClearBuffer();
  RecvData();
end;

procedure TMtxUsb.OnWriteComplete(sender: TObject; var obj: OleVariant);
begin
  t_txwait.SetEvent();
end;

procedure TMtxUsb.OnWriteStatusAvailable(sender: TObject; var obj: OleVariant);
begin
  t_txwait.SetEvent();
end;

procedure TMtxUsb.OnAddDevice(Sender: TObject; var Obj: OleVariant);
begin
  if (not Connected) then Connect();
end;

procedure TMtxUsb.OnRemoveDevice(Sender: TObject; var Obj: OleVariant);
begin
  if (Connected) then
    if (not FindDevice(ProductSN, 0)) then Disconnect();
end;

constructor TMtxUsb.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_type := CT_USB;
  e_state := CS_UNKNOWN;
  w_vid := $1B97;  //default vendor id of usb device
  w_pid := $2;     //default product id of usb device
  i_psn := -1;  //default serial number of usb device, which means the first device with the given vid and pid
  i_actpsn := -1;   //initialize acture serial number with -1, which means that no serial number is not yet found.
  //create and set instances of usbiointerface for EP0, pipe in and out
  t_usbio := TUSBIOInterface3.Create(self);
  t_connobj := t_usbio;
  t_usbio.OnPnPAddNotification := OnAddDevice;
  t_usbio.OnPnPRemoveNotification := OnRemoveDevice;
  t_usbio.EnablePnPNotification(CSTR_MTXUSB_ARS2000_GUID, i_status);
  t_usbrx := TUSBIOInterface3.Create(self);
  t_usbrx.OnReadComplete := OnReadComplete;
  t_usbtx := TUSBIOInterface3.Create(self);
  t_usbtx.OnWriteComplete := OnWriteComplete;
  t_usbtx.OnWriteStatusAvailable := OnWriteStatusAvailable;
  //clear device info
  ClearDeviceInfo();
  //connect all intances to COM server (USBIO-Driver)
  t_usbio.Connect();
  t_usbrx.Connect();
  t_usbtx.Connect();
end;

destructor TMtxUsb.Destroy;
begin
  if Connected then Disconnect();
  t_usbtx.Disconnect();
  t_usbrx.Disconnect();
  t_usbio.Disconnect();
  t_usbtx.OnWriteStatusAvailable := Nil;
  t_usbtx.OnWriteComplete := Nil;
  t_usbrx.OnReadComplete := Nil;
  t_usbio.DisablePnPNotification(CSTR_MTXUSB_ARS2000_GUID, i_status);
  FreeAndNil(t_usbio);
  t_connobj := nil;
	inherited Destroy;
end;

function TMtxUsb.FindDevice(const psn: integer; const tout: integer): boolean;
var c_time: cardinal;
begin
  if (tout < 0) then c_time := GetTickCount() + c_timeout
  else c_time := GetTickCount() + cardinal(tout);

  repeat
    Application.ProcessMessages();
    result := DetectDevice(psn, w_pid, w_vid);
  until (result or (GetTickCount() >= c_time));
end;

function TMtxUsb.Config(const sconfs: TStrings): boolean;
var i: EMtxUsbProperty; s_conf: string;
begin
  result := false;
  if (e_state in [CS_UNKNOWN, CS_CONFIGURED]) then begin
    for i := LOW(EMtxUsbProperty) to HIGH(EMtxUsbProperty) do begin
      s_conf := sconfs.Values[CSTR_MTXUSB_PROPERTIES[i]];
      result := SetProperty(i, s_conf);
      if not result then break;
    end;
    if result then e_state := CS_CONFIGURED
    else AddMessage(format('Failed to configurate the configuration (%s).', [GetTypeName()]), ML_ERROR);
  end else AddMessage(format('The current state is not suitable to config (state=%d).', [Ord(e_state)]), ML_ERROR);
end;

function TMtxUsb.Connect(): boolean;
begin
  result := false;
  if (e_state in [CS_CONFIGURED, CS_CONNECTED]) then begin
    ConnectTo(i_psn);
    result := WaitForConnecting(GetTickCount() + c_timeout);
    if result then begin
      e_state := CS_CONNECTED;
      AddMessage(format('Successful to make a connection(%s) to device(sn=%d).', [GetTypeName(), ProductSN]));
    end else AddMessage(format('Failed to make a connection(%s) to device(sn=%d)', [GetTypeName(), ProductSN]), ML_ERROR);
  end else AddMessage(format('The current state (%s) is not suitable for making a connection.', [GetStateStr()]), ML_WARNING);
end;

function TMtxUsb.Disconnect: boolean;
begin
  result := Uninit();
  AddMessage('The device is disconnected.');
end;

end.
