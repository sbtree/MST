unit USB;

interface
uses USBIOCOMLib_TLB, USBSPEC, Classes, SysUtils, StrUtils, Windows, ActiveX, ConnBase, SyncObjs;

type
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
    t_usbio:    TUSBIOInterface3;  //intance for general setting of usb
    t_usbrx:    TUSBIOInterface3;  //endpoint for reading from usb-device
    t_usbtx:    TUSBIOInterface3;  //endpoint for writing to usb-device
    i_status:   integer;
    w_vid:      word;
    w_pid:      word;
    i_prodsn:   integer;
    i_curdev:   integer; //index of the found device
    r_devconf:  RMtxUsbDeviceConfig;
    ba_rbuf:    array[0..127] of byte; //buffer for data received from device
    //ba_wbuf:    array[0..127] of byte; //buffer for data sent to device
    i_rlen:     integer;  //actual length of the received data
    t_rxwait:   TEvent;   //wait event for reading
    t_txwait:   TEvent;   //wait event for writing complete
  protected
    function IsConnected(): boolean; override;
    function FindDevice(const sn: integer; const pid: word; const vid: word; const tend: cardinal): boolean; overload;
    function GetDeviceConfig(): boolean;
    function Init(const psn: integer = -1): boolean;
    function Uninit(): boolean;
    function RecvData(): boolean;
    function WaitForTxComlete(const tend: cardinal): boolean;
    procedure CopyByteData (const psarr : PSafeArray; li_offset, li_size : longint; const pbarr : PByteArray );
    procedure InitDevConfig();

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
    property ProductSN: integer read i_prodsn;

    function FindDevice(const sn: integer = -1; const tout: integer = -1): boolean; overload;
    function Config(const sconfs: TStrings): boolean; override;
    function Connect(): boolean; override;
    function ConnectTo(const psnr: integer): boolean;
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
  C_USB_RX_BUFFER_SIZE  = 16384; // Größe des USB-Empfangspuffers
  C_USB_TX_BUFFER_SIZE  = 16384; // Größe des USB-Sendepuffers
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
  C_USB_MAX_TX_ERRORS   =  5;   // Maximale Anzahl von Fehlern, die beim Senden von Daten
                                // auftreten dürfen. Wird diese Anzahl erreicht,
                                // so wird der interne Worker-Thread angehalten.
                                // (s. TUSBIOInterface3.StartWriting)
  C_USB_CNT_TX_BUFFERS  =  5;   // Anzahl Sendepuffer

  C_USB_MAX_RX_ERRORS   =  5;   // Maximale Anzahl von Fehlern, die beim Datenempfang
                                // auftreten dürfen. Wird diese Anzahl erreicht,
                                // so wird der interne Worker-Thread angehalten.
                                // (s. TUSBIOInterface3.StartWriting)
  C_USB_CNT_RX_BUFFERS  = 5;    // Anzahl Empfangspuffer

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

function TMtxUsb.FindDevice( const sn: integer; const pid, vid: word; const tend: cardinal): boolean;
var i, i_count, i_size, i_psnr: integer;
    pa_descr1, pa_descr2: PSafeArray; r_strdesc: USB_STRING_DESCRIPTOR;
begin
  result := false; i_curdev := -1; i_prodsn := -1;
  repeat
    t_usbio.EnumerateDevices(CSTR_MTXUSB_ARS2000_GUID,i_count);
    for i := 0 to i_count - 1 do begin
      t_usbio.OpenDevice(i, i_status);
      if ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_DEVICE_ALREADY_OPEN)))then begin
        i_size := SizeOf(USB_DEVICE_DESCRIPTOR);
        pa_descr1 := SafeArrayCreateVector ( VT_UI1, 0, i_size );
        t_usbio.GetDeviceDescriptor(pa_descr1, i_size, i_status);
        if (i_status = USBIO_ERR_SUCCESS) then begin
          CopyByteData(pa_descr1, 0, i_size, PByteArray(@r_devconf.DeviceDescriptor));
          if ((r_devconf.DeviceDescriptor.idVendor = vid) and (r_devconf.DeviceDescriptor.idProduct = pid)) then begin
            i_size := SizeOf(USB_STRING_DESCRIPTOR);
            FillChar(r_strdesc.bString, SizeOf(r_strdesc.bString), $00);
            pa_descr2 := SafeArrayCreateVector ( VT_UI1, 0, i_size );
            t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iSerialNumber, 0, i_status);
            if (i_status = USBIO_ERR_SUCCESS) then begin
              CopyByteData (pa_descr2, 0, i_size, PByteArray(@r_strdesc));
              r_devconf.SerialNumber := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
            end;
            if TryStrToInt(r_devconf.SerialNumber, i_psnr) then i_prodsn := i_psnr;
            result := ((i_prodsn = sn) or (sn = -1));

            if result then begin
              i_size := SizeOf(USB_STRING_DESCRIPTOR);
              FillChar(r_strdesc.bString, SizeOf(r_strdesc.bString), $00);
              t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iManufacturer, 0, i_status);
              if (i_status = USBIO_ERR_SUCCESS) then begin
                CopyByteData (pa_descr2, 0, i_size, PByteArray(@r_strdesc));
                r_devconf.Manufacturer := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
              end;

              i_size := SizeOf(USB_STRING_DESCRIPTOR);
              FillChar(r_strdesc.bString, SizeOf(r_strdesc.bString), $00);
              t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.DeviceDescriptor.iProduct, 0, i_status);
              if (i_status = USBIO_ERR_SUCCESS) then begin
                CopyByteData (pa_descr2, 0, i_size, PByteArray(@r_strdesc));
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
    Application.ProcessMessages();
  until ((GetTickCount() >= tend) or b_break or result);
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
      CopyByteData(pa_descr1, 0, i_size, PByteArray(@r_conf));
      //get total data of the configuration, incl. Interface descriptor und endpoints
      i_size := r_conf.wTotalLength;
      pa_descr2 := SafeArrayCreateVector(VT_UI1, 0, i_size);
      t_usbio.GetConfigurationDescriptor(pa_descr2, i_size, 0, i_status);
      result := (i_status = USBIO_ERR_SUCCESS);
      if result then  CopyByteData(pa_descr2, 0, i_size, PByteArray(@r_devconf.MtxUsbConfigDesc));
      SafeArrayDestroy(pa_descr2);

      if result then begin
        i_size := SizeOf(USB_STRING_DESCRIPTOR);
        FillChar(r_strdesc.bString, SizeOf(r_strdesc.bString), $00);
        pa_descr2 := SafeArrayCreateVector(VT_UI1, 0, i_size);
        t_usbio.GetStringDescriptor(pa_descr2, i_size, r_devconf.MtxUsbConfigDesc.ConfigDescriptor.iConfiguration, 0, i_status);
        if (i_status = USBIO_ERR_SUCCESS) then begin
          CopyByteData (pa_descr2, 0, i_size, PByteArray(@r_strdesc));
          r_devconf.Configuration := WideCharToString(PWideChar(Copy(r_strdesc.bString, 0, r_strdesc.bLength)));
        end;
      end;
    end;
    SafeArrayDestroy(pa_descr1);
  end;
end;

function TMtxUsb.Init(const psn: integer): boolean;
var i_size: integer;
begin
  result := FindDevice(psn);
  //1.add interface
  if result then begin
    if (r_devconf.MtxUsbConfigDesc.EndpointIn.wMaxPacketSize > r_devconf.MtxUsbConfigDesc.EndpointOut.wMaxPacketSize) then
      i_size := r_devconf.MtxUsbConfigDesc.EndpointIn.wMaxPacketSize
    else
      i_size := r_devconf.MtxUsbConfigDesc.EndpointOut.wMaxPacketSize;
    t_usbio.AddInterface(r_devconf.MtxUsbConfigDesc.InterfaceDescriptor.bInterfaceNumber,
                         r_devconf.MtxUsbConfigDesc.InterfaceDescriptor.bAlternateSetting,
                         i_size, i_status);
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
end;

function TMtxUsb.Uninit(): boolean;
begin
  t_usbtx.CloseDevice();
  t_usbrx.CloseDevice();
  t_usbio.UnconfigureDevice(i_status);
  t_usbio.DeleteInterfaces();
  t_usbio.CloseDevice();
  InitDevConfig();
  result := true;
  e_state := CS_CONFIGURED;
end;

function TMtxUsb.RecvData(): boolean;
var pa_recv: PSafeArray; i_size: integer;
begin
  result := false; i_size := SizeOf(ba_rbuf);
  pa_recv := SafeArrayCreateVector ( VT_UI1, 0, C_USB_RX_BUFFER_SIZE );
  t_usbrx.ReadData(pa_recv, i_size, i_status);
  if (i_status = USBIO_ERR_SUCCESS) then begin
    FillChar(ba_rbuf, i_size, $00);
    i_rlen := i_size;
    CopyByteData(pa_recv, 0, i_size, PByteArray(@ba_rbuf));
  end else t_usbrx.ResetPipe(i_status);
  SafeArrayDestroy (pa_recv);
end;

function TMtxUsb.WaitForTxComlete(const tend: cardinal): boolean;
begin
  repeat
    Application.ProcessMessages();
    result := (t_txwait.WaitFor(0) = wrSignaled);
  until (result or (GetTickCount() >= tend));
end;

function TMtxUsb.IsConnected(): boolean;
begin
  result := (e_state = CS_CONNECTED);
end;

procedure TMtxUsb.InitDevConfig();
begin
  i_curdev := -1;
  FillChar(r_devconf.MtxUsbConfigDesc, SizeOf(RMtxUsbConfigDesc), $00);
  FillChar(r_devconf.DeviceDescriptor, SizeOf(USB_DEVICE_DESCRIPTOR), $00);
  r_devconf.Manufacturer := '';
  r_devconf.ProductType := '';
  r_devconf.SerialNumber := '';
  r_devconf.Configuration := '';
end;

procedure TMtxUsb.OnReadComplete(sender: TObject; var obj: OleVariant);
begin
  t_rxwait.SetEvent();
  RecvData();
end;

procedure TMtxUsb.OnWriteComplete(sender: TObject; var obj: OleVariant);
begin
  //t_txwait.SetEvent();
end;

procedure TMtxUsb.OnWriteStatusAvailable(sender: TObject; var obj: OleVariant);
begin
  t_txwait.SetEvent();
end;

procedure TMtxUsb.OnAddDevice(Sender: TObject; var Obj: OleVariant);
begin
  if (i_curdev < 0) then Connect();
end;

procedure TMtxUsb.OnRemoveDevice(Sender: TObject; var Obj: OleVariant);
begin
  if (i_curdev >= 0) then
    if (not FindDevice(i_prodsn, 2000)) then Disconnect();      
end;

constructor TMtxUsb.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_type := CT_USB;
  e_state := CS_UNKNOWN;
  w_vid := $1B97;  //default vendor id of usb device
  w_pid := $2;     //default product id of usb device
  i_prodsn := -1;
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
  InitDevConfig();
  //connect all intances to COM server (USBIO-Driver)
  t_usbio.Connect();
  t_usbrx.Connect();
  t_usbtx.Connect();
  t_rxwait := TEvent.Create(nil, false, false, 'TMtxUsb.Rx');
  t_txwait := TEvent.Create(nil, false, false, 'TMtxUsb.Tx');
end;

destructor TMtxUsb.Destroy;
begin
  t_usbtx.Disconnect();
  t_usbrx.Disconnect();
  t_usbio.Disconnect();
  t_usbtx.OnWriteStatusAvailable := Nil;
  t_usbtx.OnWriteComplete := Nil;
  t_usbrx.OnReadComplete := Nil;
  t_usbio.DisablePnPNotification(CSTR_MTXUSB_ARS2000_GUID, i_status);
  FreeAndNil(t_usbio);
  t_connobj := nil;
  FreeAndNil(t_rxwait);
  FreeAndNil(t_txwait);
	inherited Destroy;
end;

function TMtxUsb.FindDevice(const sn: integer; const tout: integer): boolean;
var c_time: cardinal;
begin
  if (tout < 0) then c_time := GetTickCount() + c_timeout
  else c_time := GetTickCount() + cardinal(tout);

  result := FindDevice(sn, w_pid, w_vid, c_time);
end;

function TMtxUsb.Config(const sconfs: TStrings): boolean;
begin
  result := false;
  //todo:
  
  if result then e_state := CS_CONFIGURED;
end;

function TMtxUsb.Connect(): boolean;
begin
  result := ConnectTo(-1);
end;

function TMtxUsb.ConnectTo(const psnr: integer): boolean;
begin
  Disconnect();
  result := Init(psnr);
  if result then begin
    t_usbrx.StartReading(C_USB_RX_BUFFER_SIZE, C_USB_CNT_RX_BUFFERS, C_USB_MAX_RX_ERRORS, i_status);
    result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_THREAD_IS_RUNNING)));
    if result then begin
      t_usbrx.ResetPipe(i_status);
      FillChar(ba_rbuf, SizeOf(ba_rbuf), $00);
    end;

    if result then begin
      t_usbtx.StartWriting( C_USB_TX_BUFFER_SIZE, C_USB_CNT_TX_BUFFERS, C_USB_MAX_TX_ERRORS, 1, i_status);
      result := ((i_status = USBIO_ERR_SUCCESS) or (i_status = integer(USBIO_ERR_THREAD_IS_RUNNING)));
      if result then t_usbtx.ResetPipe(i_status);
    end;
  end;
  if result then e_state := CS_CONNECTED
  else Disconnect();
end;

function TMtxUsb.Disconnect: boolean;
begin
  t_usbtx.StopWriting();
  t_usbrx.StopReading();
  result := Uninit();
  t_rxwait.ResetEvent();
  t_txwait.ResetEvent();
  if (e_state = CS_CONNECTED) then  e_state := CS_CONFIGURED;
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
var pa_send: PSafeArray; i, i_size: integer; c_time: cardinal;
begin
  result := false;
  if Connected then begin
    i_size := Length(str) + 1;
    if (i_size > 0) then begin
      c_time := GetTickCount() + c_timeout;
      pa_send := SafeArrayCreateVector ( VT_UI1, 0, i_size );
      for i := 0 to i_size - 1 do SafeArrayPutElement(pa_send, i, str[i]);
      t_usbtx.WriteData(pa_send, 0, i_status);
      result := (i_status = USBIO_ERR_SUCCESS);
      SafeArrayDestroy(pa_send);
      if result then result := WaitForTxComlete(c_time);
      if result then Addmessage(format('Successed to send: %s', [str]))
      else Addmessage(format('Failed to send: %s', [str]));
    end else Addmessage('The given string is empty and nothing is sent.');
  end else Addmessage('No connection and nothing is sent.');
end;

function TMtxUsb.RecvStr(var str: string; const bwait: boolean = false): integer;
var i: integer;
begin
  result := 0; str := '';
  if Connected then begin
    if bwait then WaitForReading(GetTickCount + c_timeout)
    else begin
      Application.ProcessMessages();
      t_rxwait.WaitFor(0);
    end;
    //if RecvData() then begin
      for i := 0 to i_rlen - 1 do if (ba_rbuf[i] > 0) then str := str + Char(ba_rbuf[i]);
      result := i_rlen;
      for i := 0 to sizeof(ba_rbuf) - 1 do ba_rbuf[i] := 0;
      i_rlen := 0;
    //end;

    if (result > 0) then Addmessage(format('Received: %s', [str]))
    else Addmessage('Nothing is received.');
  end else Addmessage('No connection and nothing is received.');;
end;

function TMtxUsb.RecvStrTimeout(var str: string; const tend: cardinal): integer;
var i: integer;
begin
  result := 0; str := '';
  if Connected then begin
    if (WaitForReading(tend)) then begin
      for i := 0 to i_rlen - 1 do if (ba_rbuf[i] > 0) then str := str + Char(ba_rbuf[i]);
      result := i_rlen;
    end;
    for i := 0 to sizeof(ba_rbuf) - 1 do ba_rbuf[i] := 0;
    i_rlen := 0;
  end;
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
  repeat
    Application.ProcessMessages();
    result := (t_rxwait.WaitFor(0) = wrSignaled);
  until (result or (GetTickCount() >= tend));
end;

end.
