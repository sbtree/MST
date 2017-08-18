//==============================================================================
// Module name  : $RCSfile: ProductBase.pas,v $
// Description  : This unit defines base classes for variants of Metronix products
// Copyright    : (c) Metronix 2015
// Reversion    : $Revision 1.0$
// Compiler     : Delphi 2007
// Author       : 2015-07-14 /bsu/
// History      :
//==============================================================================
unit ProductBase;

interface
uses Classes, CommBase, DeviceBase, MtxDownloader;
type
  IMtxTestUnit = interface
    //1. Reset
    //2. Boot to service
    //3. Flash (BL & FW)
    //4. EEPROM (product type, module type, serial number)
    //5. write service data (exam-date, serial number)
    //6. query communication object (CO)
    //7. ...
  end;

  TMtxTestUnit = class(TDeviceBase, IMtxTestUnit)

  end;

  IMtxProduct = interface
    function Reset(const cmd: string): boolean;
    {function EnterService(const cmd: string; const expe: string = ''): boolean;

    function FlashBootloader(const sfile: string): boolean;
    function FlashFirmware(const sfile: string): boolean;
    function FlashAdditional(const sfile: string): boolean;
    function FlashComplete(const blfile, fwfile: string; const addfile: string = ''): boolean;

    function ActiveConnect(const connid: string): boolean;

    function WriteParameterSet(const sfile: string): boolean;
    function WriteEeprom(const sfile: string): boolean;
    function WriteHardwareVer(const sver: string): boolean;
    function WriteSerialNr(const snr: string): boolean;
    function WriteBoardNr(const snr: string): boolean;

    function CheckBootloaderVer(const isver, exver: string): boolean;
    function CheckFirmwareVer(const isver, exver: string): boolean; }
  end;

  TMtxProduct = class(TDeviceBase, IMtxProduct)
  protected
    t_connlist: TStrings;     //a list of all available connections
  protected
    {function AddConnect(const cnname: string; const conn: TConnBase): boolean;
    function RemoveConnect(const cnname: string): boolean;
    procedure ClearConnections();  }

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    {function Reset(const cmd: string): boolean; override;
    function EnterService(const cmd: string; const expe: string = ''): boolean; virtual;
    function FlashBootloader(const sfile: string): boolean; virtual;
    function FlashFirmware(const sfile: string): boolean; virtual;
    function FlashAdditional(const sfile: string): boolean; virtual;
    function FlashComplete(const blfile, fwfile: string; const addfile: string): boolean; virtual;
    function ActiveConnect(const connid: string): boolean; virtual;
    function WriteParameterSet(const sfile: string): boolean; virtual;
    function WriteEeprom(const sfile: string): boolean; virtual;
    function WriteHardwareVer(const sver: string): boolean; virtual;
    function WriteSerialNr(const snr: string): boolean; virtual;
    function WriteBoardNr(const snr: string): boolean; virtual;
    function CheckBootloaderVer(const isver, exver: string): boolean; virtual;
    function CheckFirmwareVer(const isver, exver: string): boolean; virtual;}
  end;

implementation

constructor TMtxProduct.Create(owner: TComponent);
begin
  inherited Create(owner);
  t_connlist := TStringList.Create();
end;

destructor TMtxProduct.Destroy;
begin
  //ClearConnections();
  t_connlist.Free();
  inherited Destroy();
end;

end.
