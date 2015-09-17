// =============================================================================
// Module name  : $RCSfile: ConnectBase.pas,v $
// description  : This unit implements a basis methodes and properties of a class
//                for connection FlashRunner. The types of connection includes Jtag
//                RS232, USB, CAN-bus and Profil-Bus.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit ConnBase;

interface
uses  Classes, Windows, DataBuffer;
type

  EConnectionType = (
                    CT_JTAG,  //jtag
                    CT_GPIB, //GPIB/ieee 488
                    CT_RS232, //rs232
                    CT_USB,   //usb
                    CT_ETHERNET, //ethernet
                    CT_CAN,   //can-bus
                    CT_PROFIL //profil-bus
                    );

  TConnBase = class(TComponent)
  protected
    e_type : EConnectionType;
    i_lasterr: integer;
  protected

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property ConnectionType : EConnectionType read e_type;
    function Config(const sconf: string): boolean;virtual;abstract;
    function IsConnected(): boolean; virtual; abstract;
    function Connect(): boolean;virtual;abstract;
    function Disconnect: boolean;virtual;abstract;
    function SendData(const sbuf: TCharBuffer; const timeout: cardinal): Integer;virtual;abstract;
    function RecvData(var rbuf: TCharBuffer; const timeout: cardinal): Integer;virtual;abstract;
    function GetLastError(var msg: string): Integer;virtual;abstract;
  end;
  PConnBase = ^TConnBase;

const
  CSTR_CONN_KEYS : array[LOW(EConnectionType)..HIGH(EConnectionType)] of string = (
                    'CONN_JTAG',
                    'CONN_GPIB',
                    'CONN_RS232',
                    'CONN_USB',
                    'CONN_ETHERNET',
                    'CONN_CAN',
                    'CONN_PROFIL'
                    );

implementation

constructor TConnBase.Create(owner: TComponent);
begin
	inherited Create(owner);
end;

destructor TConnBase.Destroy;
begin
	inherited Destroy;
end;

end.
