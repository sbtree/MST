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
uses  Classes, DataBuffer;
type

  EConnectionType = (
                    CT_UNKNOWN, //a undefined type of connction
                    CT_JTAG,  //jtag
                    CT_RS232, //rs232
                    CT_USB,   //usb
                    CT_ETHERNET, //ethernet
                    CT_CAN,   //can-bus
                    CT_PROFIL //profil-bus
                    );

  TConnBase = class
  protected
    e_type : EConnectionType;
    i_lasterr: integer;
  public
    constructor Create;
    destructor Destroy; override;

    property ConnectionType : EConnectionType read e_type;
    function Config(const sconf: string): boolean;virtual;abstract;
    function IsConnected(): boolean; virtual; abstract;
    function Connect(): boolean;virtual;abstract;
    function Disconnect: boolean;virtual;abstract;
    function SendData(const data: TCharBuffer): Integer;virtual;abstract;
    function RecvData(var data: TCharBuffer): Integer;virtual;abstract;
    function GetLastError(var msg: string): Integer;virtual;abstract;
  end;
  PConnBase = ^TConnBase;

//const
//  CSTR_COMM_STATES : array[LOW(ECommunicationState)..HIGH(ECommunicationState)] of string = ('unusable','configured','connected','ready','busy', 'waiting');
implementation

constructor TConnBase.Create;
begin
	inherited Create;
  e_type := CT_UNKNOWN;
end;

destructor TConnBase.Destroy;
begin
	inherited Destroy;
end;

end.
