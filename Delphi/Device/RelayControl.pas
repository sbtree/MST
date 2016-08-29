unit RelayControl;

interface
uses Classes, ConnBase;
type
  IRelayControl = interface
    function CloseRelays(const relays: string): boolean;
    function OpenRelays(const relays: string): boolean;
    function SwitchRelaysOnOff(const relays: string; const tdelay: cardinal = 50): boolean;
    function SwitchRelaysOffOn(const relays: string; const tdelay: cardinal = 50): boolean;
    function QueryRelays(var relnrs: string): boolean;
  end;

  TRelayControl = class(TInterfacedObject, IRelayControl)
  protected
    t_curconn: TConnBase;
  public
    constructor Create();
    destructor Destroy; override;

    function CloseRelays(const relays: string): boolean; virtual;
    function OpenRelays(const relays: string): boolean; virtual;
    function SwitchRelaysOnOff(const relays: string; const tdelay: cardinal): boolean; virtual;
    function SwitchRelaysOffOn(const relays: string; const tdelay: cardinal): boolean; virtual;
    function QueryRelays(var relnrs: string): boolean; virtual;

    property CurConnect: TConnBase read t_curconn write t_curconn;
  end;

  TRelayHygrosenUsb = class(TRelayControl)
  public
    constructor Create();
    destructor Destroy; override;

    property CurConnect: TConnBase read t_curconn;
  end;


  TRelayKeithley = class(TRelayControl)
  type Channels = 1..40;

  end;

implementation
uses GenUtils;

constructor TRelayControl.Create();
begin
  inherited Create();
  //todo:
end;

destructor TRelayControl.Destroy;
begin
  //todo:
  inherited Destroy();
end;

function TRelayControl.CloseRelays(const relays: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(relays);
  end;
end;

function TRelayControl.OpenRelays(const relays: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(relays);
  end;
end;

function TRelayControl.SwitchRelaysOnOff(const relays: string; const tdelay: cardinal): boolean;
begin
  result := CloseRelays(relays);
  if result then begin
    TGenUtils.Delay(tdelay);
    result := OpenRelays(relays);
  end;
end;

function TRelayControl.SwitchRelaysOffOn(const relays: string; const tdelay: cardinal): boolean;
begin
  result := OpenRelays(relays);
  if result then begin
    TGenUtils.Delay(tdelay);
    result := CloseRelays(relays);
  end;
end;

function TRelayControl.QueryRelays(var relnrs: string): boolean;
begin
  result := false;
  //todo:
end;

end.
