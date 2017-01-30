// =============================================================================
// Module name  : $RCSfile: ConnManager.pas,v $
// description  : This unit defines a class to manager connections. It works as
//                a factory and a container of connections, which are supported
//                in this program
// Compiler     : Delphi XE7
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit ConnManager;

interface
uses Classes, ConnBase, TextMessage;

type
  TConnManager = class(TComponent, ITextMessengerImpl)
  protected
    t_conns:    TStrings;           //to save all created connections
    i_curidx:   integer;            //index of current active connection in t_conns
    t_msgrimpl: TTextMessengerImpl; //for transfering messages
  protected
    function ConnectByIndex(const idx: integer): TCommBase;
    procedure ClearConns();
  public
    //constructor and destructor
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;

    function CreateConnect(const cname: string; const ctype: EConnectType): TCommBase;
    function RemoveConnect(const cname: string): boolean;
    function CurrentConnect(): TCommBase;
    function ActiveConnect(const cname: string): TCommBase;
    function GetConnect(const cname: string): TCommBase;
  end;

implementation
uses SysUtils, RS232, MtxUSB, PCAN;

function TConnManager.ConnectByIndex(const idx: integer): TCommBase;
begin
  if ((idx >= 0) and (idx < t_conns.Count)) then result := TCommBase(t_conns.Objects[idx])
  else result := nil;
end;

procedure TConnManager.ClearConns();
var i: integer;
begin
  for i := 0 to t_conns.Count - 1 do begin
    if assigned(t_conns.Objects[i]) then t_conns.Objects[i].Free();
  end;
  i_curidx := -1;
  t_conns.Clear();
end;

constructor TConnManager.Create(owner: TComponent);
begin
  inherited Create(owner);
  t_conns := TStringList.Create();
  i_curidx := -1;
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

destructor TConnManager.Destroy;
begin
  ClearConns();
  t_conns.Free();
  t_msgrimpl.Free();
  inherited Destroy();
end;

function TConnManager.CreateConnect(const cname: string; const ctype: EConnectType): TCommBase;
var s_conname: string;
begin
  s_conname := trim(cname);
  RemoveConnect(s_conname); //remove it if a connection exists with this name
  case ctype of
    CT_RS232: result := TRS232.Create(self);
    CT_MTXUSB: result := TMtxUSB.Create(self);
    CT_TEKUSB: result := nil; //todo:
    CT_GPIB: result := nil; //todo:
    CT_ETHERNET: result := nil; //todo:
    CT_JTAG: result := nil; //todo:
    CT_PCAN: result := TPCanLight.Create(self);
    CT_PROFI: result := nil; //todo:
    else result := nil;
  end;

  if assigned(result) then begin
    ITextMessengerImpl(result).Messenger := t_msgrimpl.Messenger;
    t_conns.AddObject(s_conname, result);
    if ((t_conns.Count = 1) and (i_curidx < 0)) then i_curidx := 0;
  end;
end;

function TConnManager.RemoveConnect(const cname: string): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := t_conns.IndexOf(trim(cname));
  if (i_idx >= 0) then begin
    t_conns.Objects[i_idx].Free();
    t_conns.Delete(i_idx);
    if (i_idx = i_curidx) then begin
      if ((t_conns.Count = 1) and (i_curidx < 0)) then i_curidx := 0
      else i_curidx := -1;
    end;
    result := true;
  end;
end;

function TConnManager.CurrentConnect(): TCommBase;
begin
  result := ConnectByIndex(i_curidx);
end;

function TConnManager.ActiveConnect(const cname: string): TCommBase;
begin
  i_curidx := t_conns.IndexOf(trim(cname));
  result := CurrentConnect();
end;

function TConnManager.GetConnect(const cname: string): TCommBase;
var i_idx: integer;
begin
  i_idx := t_conns.IndexOf(trim(cname));
  result := ConnectByIndex(i_idx);
end;

end.
