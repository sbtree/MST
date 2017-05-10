unit RelayControl;

interface
uses Classes, ConnBase, TextMessage;
type
  IRelayControl = interface
    function CloseRelays(const relays: string): boolean;
    function OpenRelays(const relays: string): boolean;
    function OpenAllRelays(): boolean;
    function SwitchRelaysOnOff(const relays: string; const tdelay: cardinal = 50): boolean;
    function SwitchRelaysOffOn(const relays: string; const tdelay: cardinal = 50): boolean;
    function QueryRelays(var relnrs: string): boolean;
    function GetClosedRelays(): string;
    function GetOpenedRelays(): string;
    function GetAvailableRelays(): string;
    function VerifyClosedRelays(const refrelnrs: string): boolean;
  end;

  TRelayControl = class(TInterfacedObject, IRelayControl, ITextMessengerImpl)
  protected
    i_rcount: integer;
    t_curconn: TConnBase;
    t_msgrimpl:TTextMessengerImpl;
  protected
    procedure SetConnection(const conn: TConnBase); virtual;

  public
    constructor Create();
    destructor Destroy; override;

    function CloseRelays(const relays: string): boolean; virtual;
    function OpenRelays(const relays: string): boolean; virtual;
    function OpenAllRelays(): boolean; virtual;
    function SwitchRelaysOnOff(const relays: string; const tdelay: cardinal): boolean; virtual;
    function SwitchRelaysOffOn(const relays: string; const tdelay: cardinal): boolean; virtual;
    function QueryRelays(var relnrs: string): boolean; virtual;
    function GetClosedRelays(): string; virtual;
    function GetOpenedRelays(): string; virtual;
    function GetAvailableRelays(): string; virtual;
    function VerifyClosedRelays(const refrelnrs: string): boolean; virtual;

    property CurConnect: TConnBase read t_curconn write SetConnection;
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    property RelayCount: integer read i_rcount;
  end;

  TRelayHygrosenUsb = class(TRelayControl)
  public
    //constructor Create();
    //destructor Destroy; override;

    property CurConnect: TConnBase read t_curconn;
  end;

  //Every Keithley Pseudocard 7705 has 40 channels and they are nummerized :
  //101, 102, ..., 140 for the first pseudo card
  //201, 202, ..., 240 for the second pseudo card, and so on
  //301-340 for third, 401-440 for forth and 501-540 for fifth card
  //here a channel set (index of channel) is defined for all channels which are being mapped from 0 to 199,
  //together are 200 channels for 5 cards. Die mapping:
  //101, 102, ..., 140, 201, 202, ..., 240, ......, 501, 502, ..., 540
  //0,   1,   ..., 39,  40,  41,  ..., 79,  ......, 160, 161, ..., 199
  //using channel set it's easy to compare two groups of relays, see in VerifyClosedRelays
  TKeithleyChannelIndex = 0..199;
  TKeithleyChannelSet = set of TKeithleyChannelIndex;

  //a class to control Keithley Pseudocard 7705
  TRelayKeithley = class(TRelayControl)
  protected
    t_slots:  TStrings;
    set_all:  TKeithleyChannelSet;
  protected
    procedure UpdateCardInfo();
    procedure SetConnection(const conn: TConnBase); override;
    function GetCardSlots(): integer;
    function GetCardName(idx: integer): string;
    function GetCardNumber(idx: integer): integer;
    function ChannelIndexSet(const chnr: string): TKeithleyChannelSet;
    function ChannelNumbers(const chset: TKeithleyChannelSet): string;
    function ChannelIndexToNumber(const idx: integer): string;
    function ChannelNumberToIndex(const chnr: string): integer;
  public
    constructor Create();
    destructor Destroy; override;

    function CloseRelays(const relays: string): boolean; override;
    function OpenRelays(const relays: string): boolean; override;
    function OpenAllRelays(): boolean; override;
    function QueryRelays(var relnrs: string): boolean; override;
    function GetClosedRelays(): string; override;
    function GetOpenedRelays(): string; override;
    function GetAvailableRelays(): string; override;
    function VerifyClosedRelays(const refrelnrs: string): boolean; override;

    property CardSlots: integer read GetCardSlots;
    property CardName[idx: integer]: string read GetCardName;
  end;

implementation
uses SysUtils, GenUtils, StrUtils;

const
  C_MULTIMETER_OPT        = '*OPT?';                //query which typ of pseudo card and how many cards are installed
  C_MULTIMETER_OPEN_ALL   = 'ROUT:OPEN:ALL';        //open all relays
  C_MULTIMETER_OPEN       = 'ROUT:MULT:OPEN (@%s)'; //n relays to open. %s: relay numbers with separator ','
  C_MULTIMETER_CLOSE      = 'ROUT:MULT:CLOS (@%s)'; //n relays to clase. %s: relay numbers with separator ','
  C_MULTIMETER_CLOSE_ASK  = 'ROUT:CLOS?';           // geschlossene Relais abfragen
  C_PCARD_CHANNEL_MAX     = 40; //maximal channels of Pseudocard 7705
  C_PCARD_SLOT_MAX        = 5;  //maximal slots for Pseudocard 7705

procedure TRelayControl.SetConnection(const conn: TConnBase);
begin
  t_curconn := conn;
end;

constructor TRelayControl.Create();
begin
  inherited Create();
  i_rcount := 0;
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

destructor TRelayControl.Destroy;
begin
  t_msgrimpl.Free();
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

function TRelayControl.OpenAllRelays(): boolean;
begin
  result := false;
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
end;

function TRelayControl.GetClosedRelays(): string;
begin
  result := '';
end;

function TRelayControl.GetOpenedRelays(): string;
begin
  result := '';
end;

function TRelayControl.GetAvailableRelays(): string;
begin
  result := '';
end;

function TRelayControl.VerifyClosedRelays(const refrelnrs: string): boolean;
begin
  result := false;
end;

procedure TRelayKeithley.UpdateCardInfo();
var i, i_start, i_end, i_cards: integer; s_recv: string;
begin
  t_slots.Clear();
  i_cards := 0;
  set_all := [];
  if assigned(t_curconn) then begin
    if t_curconn.SendStr(C_MULTIMETER_OPT + Char(13)) then begin
      t_curconn.ExpectStr(s_recv, AnsiChar(13), false);
      s_recv := trim(s_recv);
      if (ExtractStrings([','], [' '], PChar(s_recv), t_slots) > 0) then begin
        for i := 0 to t_slots.Count - 1 do
          if SameText(t_slots[i], '7705') then begin
            inc(i_cards);
            i_start := C_PCARD_CHANNEL_MAX * i;
            i_end := C_PCARD_CHANNEL_MAX * i + 39;
            set_all := set_all + [i_start..i_end];
          end;
        i_rcount := i_cards * C_PCARD_CHANNEL_MAX;
      end;
      if (i_cards > 0) then
        t_msgrimpl.AddMessage(format('%d available relay cards(%s) are found.', [i_cards, s_recv]))
      else
        t_msgrimpl.AddMessage('No available relay card is found.', ML_WARNING);
    end;
  end;
end;

procedure TRelayKeithley.SetConnection(const conn: TConnBase);
begin
  inherited SetConnection(conn);
  UpdateCardInfo();
end;

function TRelayKeithley.GetCardSlots(): integer;
begin
  result := t_slots.Count;
end;

function TRelayKeithley.GetCardName(idx: integer): string;
begin
  if ((idx >= 0) and (idx < t_slots.Count)) then result := t_slots[idx]
  else result := '';
end;

function TRelayKeithley.GetCardNumber(idx: integer): integer;
begin
  result := 0;
  if ((idx >= 0) and (idx < t_slots.Count)) then begin
    if SameText(t_slots[idx], '7705') then
      result := (idx + 1 ) * 100
    else
      result := 0;
  end;
end;

function TRelayKeithley.ChannelIndexSet(const chnr: string): TKeithleyChannelSet;
var t_chnrs: TStrings; i, i_idx: integer;
begin
  t_chnrs := TStringList.Create();
  result := [];
  if (ExtractStrings([','], [' '], PChar(chnr), t_chnrs) > 0) then begin
    for i := 0 to t_chnrs.Count - 1 do begin
      i_idx := ChannelNumberToIndex(t_chnrs[i]);
      if (i_idx >= Low(TKeithleyChannelIndex)) then Include(result, i_idx);
    end;
  end;
  t_chnrs.Free();
end;

function TRelayKeithley.ChannelNumbers(const chset: TKeithleyChannelSet): string;
var i: integer; s_chnr: string;
begin
  result := '';
  for i := Low(TKeithleyChannelIndex) to High(TKeithleyChannelIndex) do begin
    if (i in chset) then begin
      s_chnr := ChannelIndexToNumber(i);
      if (s_chnr <> '') then result := result + s_chnr + ',';
    end;
  end;
  if EndsText(',', result) then result := LeftStr(result, length(result) - 1);
end;

function TRelayKeithley.ChannelIndexToNumber(const idx: integer): string;
var i_nr: integer;
begin
  if ((idx >= Low(TKeithleyChannelIndex)) and (idx <= High(TKeithleyChannelIndex))) then begin
    i_nr := ((idx  div C_PCARD_CHANNEL_MAX) + 1) * 100 + (idx mod C_PCARD_CHANNEL_MAX) + 1;
    result := IntToStr(i_nr);
  end else result := '';
end;

function TRelayKeithley.ChannelNumberToIndex(const chnr: string): integer;
var i_chnr, i_cardnr: integer;
begin
  result := -1;
  if TryStrToInt(chnr, i_chnr) then begin
    i_cardnr := trunc(i_chnr / 100);
    i_chnr := (i_chnr mod 100);
    if ((i_cardnr > 0) and (i_cardnr <= t_slots.Count) and (i_chnr <= C_PCARD_CHANNEL_MAX)) then
      result := (i_cardnr - 1) * C_PCARD_CHANNEL_MAX + i_chnr - 1;
  end;
end;

constructor TRelayKeithley.Create();
begin
  inherited Create();
  t_slots := TStringList.Create();
end;

destructor TRelayKeithley.Destroy;
begin
  t_slots.Free();
  inherited Destroy();
end;

function TRelayKeithley.CloseRelays(const relays: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(Format(C_MULTIMETER_CLOSE + AnsiChar(13), [relays]));
    if result then result := VerifyClosedRelays(relays);
  end;
end;

function TRelayKeithley.OpenRelays(const relays: string): boolean;
var set_relopen, set_relclose: TKeithleyChannelSet; s_relclose: string;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(format(C_MULTIMETER_OPEN + AnsiChar(13), [relays]));
    if result then begin
      result := QueryRelays(s_relclose);
      if result then begin
        set_relclose := ChannelIndexSet(s_relclose);
        set_relopen := ChannelIndexSet(relays);
        result := ((set_relopen * set_relclose) = []);
      end;
    end;
  end;
end;

function TRelayKeithley.OpenAllRelays(): boolean;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(C_MULTIMETER_OPEN_ALL + Char(13));
    if result then result := VerifyClosedRelays('');
  end;
end;

function TRelayKeithley.QueryRelays(var relnrs: string): boolean;
var s_recv: string;
begin
  result := false;
  if assigned(t_curconn) then begin
    result := t_curconn.SendStr(C_MULTIMETER_CLOSE_ASK + Char(13));
    if result then begin
      if t_curconn.ExpectStr(s_recv, ')', false) then
        s_recv := trim(s_recv);
        if (AnsiStartsText('(@', s_recv) and EndsText(')', s_recv)) then relnrs := AnsiMidStr(s_recv, 3, length(s_recv) - 3)
        else relnrs := s_recv;
    end;
  end;
end;

function TRelayKeithley.GetClosedRelays(): string;
begin
  if not QueryRelays(result) then result := '';
end;

function TRelayKeithley.GetOpenedRelays(): string;
var set_closed, set_opened: TKeithleyChannelSet;
    s_closed: string;
begin
  result := '';
  if (CardSlots > 0) then begin
    s_closed := GetClosedRelays();
    set_closed := ChannelIndexSet(s_closed);
    set_opened := (set_all - set_closed);
    result := ChannelNumbers(set_opened);
  end;
end;

function TRelayKeithley.GetAvailableRelays(): string;
begin
  result := ChannelNumbers(set_all);
end;

function TRelayKeithley.VerifyClosedRelays(const refrelnrs: string): boolean;
var set_refrel, set_isrel: TKeithleyChannelSet; s_isrel: string;
begin
  result := QueryRelays(s_isrel);
  if result then begin
    set_refrel := ChannelIndexSet(refrelnrs);
    set_isrel := ChannelIndexSet(s_isrel);
    result := (set_refrel <= set_isrel) and (set_refrel <> []);
  end;
end;

end.
