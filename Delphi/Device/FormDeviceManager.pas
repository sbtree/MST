unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage, ConnBase, RelayControl,
  Multimeter, RS232;

type
  TfrmDeviceManager = class(TForm)
    btnDisconnect: TButton;
    btnMeasure: TButton;
    cmbMeasure: TComboBox;
    btnClose: TButton;
    btnOpen: TButton;
    btnOpenAll: TButton;
    btnConnect: TButton;
    chkSelection: TCheckBox;
    txtMeasure: TEdit;
    btnUpdate: TButton;
    txtFile: TEdit;
    lblRelaysDesc: TLabel;
    btnFile: TButton;
    btnView: TButton;
    memInfo: TMemo;
    pgcMain: TPageControl;
    tabRelays: TTabSheet;
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenAllClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure RelayCheck(Sender: TObject);
    procedure chkSelectionClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
  private
    t_messenger : TTextMessenger;
    t_multimeter: TMultimeterKeithley;
    t_relay: TRelayControl;
    t_ser: TSerialAdapter;
    i_extend: integer;
  protected
    function  SelectedRelays(): string;
    function  GetCheckBox(const relaynr: string): TCheckBox;
    procedure CreateCheckBoxes();
    procedure ColorCheckBoxes(const relays: string);
    procedure UncolorCheckBoxes(const relays: string);
    procedure UpdateCheckBoxes(const fname: string);
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation
{$R *.dfm}
uses StrUtils, DeviceBase, RS232DlgDyn;

procedure TfrmDeviceManager.RelayCheck(Sender: TObject);
var s_rnum: string; t_chk: TCheckBox;
begin
  t_chk := (Sender as TCheckBox);
  if assigned(t_chk) then begin
    s_rnum := RightStr(t_chk.name, length(t_chk.name) - 4); //'chkR' + number
    if (not chkSelection.Checked) then begin
      if t_chk.Checked then
        t_relay.CloseRelays(s_rnum)
      else
        t_relay.OpenRelays(s_rnum)
    end;

    if t_relay.VerifyClosedRelays(s_rnum) then
      t_chk.Color := clLime
    else begin
      if t_chk.Checked then
        t_chk.Color := clSilver
      else
        t_chk.ParentColor := true;
    end;
  end;
end;

procedure TfrmDeviceManager.btnConnectClick(Sender: TObject);
var s_relays: string;
begin
  SerialDialog.SerialObj := t_ser.SerialObj;
  if SerialDialog.ShowModal() = mrOK then begin
    t_multimeter.InitDevice();
    CreateCheckBoxes();
    s_relays := t_relay.GetOpenedRelays();
    UncolorCheckBoxes(s_relays);
  end;
end;

procedure TfrmDeviceManager.btnFileClick(Sender: TObject);
var t_fopen: TOpenDialog;
begin
  t_fopen := TOpenDialog.Create(self);
  t_fopen.Filter := 'Config Files (*.ini, *.txt)|*.ini;*.txt';
  if t_fopen.Execute then txtFile.Text := trim(t_fopen.FileName);
  FreeAndNil(t_fopen);
end;

procedure TfrmDeviceManager.btnDisconnectClick(Sender: TObject);
begin
  t_ser.Disconnect();
end;

procedure TfrmDeviceManager.btnMeasureClick(Sender: TObject);
var f_val: double; b_ok: boolean;
begin
  case cmbMeasure.ItemIndex of
  0: b_ok := t_multimeter.MeasureR(f_val);
  1: b_ok := t_multimeter.MeasureDCV(f_val);
  2: b_ok := t_multimeter.MeasureACV(f_val);
  3: b_ok := t_multimeter.MeasureDCI(f_val);
  4: b_ok := t_multimeter.MeasureACI(f_val);
  5: b_ok := t_multimeter.MeasureF(f_val);
  6: b_ok := t_multimeter.MeasureP(f_val);
  7: b_ok := t_multimeter.MeasureT(f_val);
  else b_ok := false;
  end;
  if b_ok then txtMeasure.Text := format('%.6f',[f_val])
  else txtMeasure.Text := 'Error';
end;

procedure TfrmDeviceManager.btnOpenAllClick(Sender: TObject);
var s_relays: string;
begin
  t_relay.OpenAllRelays();
  s_relays := t_relay.GetOpenedRelays();
  UncolorCheckBoxes(s_relays);
end;

procedure TfrmDeviceManager.btnOpenClick(Sender: TObject);
var s_relays: string;
begin
  s_relays := SelectedRelays();
  t_relay.OpenRelays(s_relays);
  UncolorCheckBoxes(s_relays);
end;

procedure TfrmDeviceManager.btnUpdateClick(Sender: TObject);
begin
  UpdateCheckBoxes(txtFile.Text);
end;

procedure TfrmDeviceManager.btnViewClick(Sender: TObject);
begin
  memInfo.Visible := (not memInfo.Visible);
  if memInfo.Visible then begin
    self.Height := self.Height + i_extend;
    btnView.Caption := '&View <<';
  end else begin
    self.Height := self.Height - i_extend;
    btnView.Caption := '&View >>';
  end;
end;

procedure TfrmDeviceManager.chkSelectionClick(Sender: TObject);
begin
  btnClose.Enabled := chkSelection.Checked;
  btnOpen.Enabled := chkSelection.Checked;
end;

procedure TfrmDeviceManager.btnCloseClick(Sender: TObject);
var s_relays: string;
begin
  s_relays := SelectedRelays();
  t_relay.CloseRelays(s_relays);
  ColorCheckBoxes(s_relays);
end;

procedure TfrmDeviceManager.FormCreate(Sender: TObject);
begin
  t_messenger := TTextMessenger.Create();
  t_messenger.Messages := memInfo.Lines;
  t_messenger.MessageThreshold := ML_INFO;

  t_ser := TSerialAdapter.Create(self);
  t_ser.MessengerService.Messenger := t_messenger;
  t_ser.Config('port:5|baudrate:9600');

  t_multimeter := TMultimeterKeithley.Create(self);
  t_multimeter.DevConnect := t_ser;
  t_multimeter.MessengerService.Messenger := t_messenger;

  t_relay := t_multimeter.RelayControl;
  i_extend := memInfo.Height + 8;
end;

procedure TfrmDeviceManager.FormDestroy(Sender: TObject);
begin
  t_multimeter.Free();
  //t_ser.Free();
  t_messenger.Free();
end;

function  TfrmDeviceManager.SelectedRelays(): string;
var i, i_len: integer; s_rnum: string; t_chk: TCheckBox;
begin
  result := '';
  for i := 0 to tabRelays.ControlCount - 1 do begin
    t_chk := (tabRelays.Controls[i] as TCheckBox);
    if assigned(t_chk) and chkSelection.Checked then begin
      s_rnum := RightStr(t_chk.Name, length(t_chk.Name) - 4); // name looks like 'chkR101', prefix + number
      if t_chk.Checked then result := result + s_rnum + ',';
    end;
  end;
  i_len := length(result);
  if (i_len > 0) then result := leftStr(result, i_len - 1);
end;

function  TfrmDeviceManager.GetCheckBox(const relaynr: string): TCheckBox;
begin
  result := (tabRelays.FindChildControl('chkR' + trim(relaynr)) as TCheckBox);
end;

procedure TfrmDeviceManager.CreateCheckBoxes();
var i, i_colwidth, i_cards: integer; t_chkR: TCheckBox; s_rnum, s_relays: string; t_relays: TStringList;
const CINT_COLDIST: integer = 10; CINT_ROWDIST: integer = 2;
begin
  s_relays := t_relay.GetAvailableRelays();
  t_relays := TStringList.Create();
  if (ExtractStrings([','], [' '], PChar(s_relays), t_relays) > 0) and (tabRelays.ControlCount <= 0) then begin
    i_cards := (t_relays.Count div 40);
    i_colwidth := (tabRelays.Width - CINT_COLDIST * (i_cards * 2 - 1)) div (2 * i_cards);
    for i := 0 to t_relays.Count - 1 do begin
      t_chkR := TCheckBox.Create(tabRelays);
      tabRelays.InsertControl(t_chkR);
      s_rnum := t_relays[i];
      t_chkR.Name := 'chkR' + s_rnum;
      t_chkR.Caption := 'R' + s_rnum;
      t_chkR.Width := i_colwidth;
      t_chkR.Height := 17;
      t_chkR.Left := (i div 20) * (t_chkR.Width + CINT_COLDIST);
      t_chkR.Top := (i mod 20) * (t_chkR.Height + CINT_ROWDIST);
      t_chkR.OnClick := RelayCheck;
    end;
  end;
  t_relays.Free();
end;

procedure TfrmDeviceManager.ColorCheckBoxes(const relays: string);
var i: integer; t_rnames: TStringList; t_chk: TCheckBox;
begin
  if t_relay.VerifyClosedRelays(relays) then begin
    t_rnames := TStringList.Create();
    if (ExtractStrings([','], [' '], PChar(relays), t_rnames) > 0) then begin
      for i := 0 to t_rnames.Count - 1 do begin
        t_chk := GetCheckBox(t_rnames[i]);
        if assigned(t_chk) then
          t_chk.Color := clLime;
      end;
    end;
    t_rnames.Free();
  end;
end;

procedure TfrmDeviceManager.UncolorCheckBoxes(const relays: string);
var i: integer; t_rnames: TStringList; t_chk: TCheckBox;
begin
  t_rnames := TStringList.Create();
  if (ExtractStrings([','], [' '], PChar(relays), t_rnames) > 0) then begin
    for i := 0 to t_rnames.Count - 1 do begin
      t_chk := GetCheckBox(t_rnames[i]);
      if assigned(t_chk) then begin
        if t_chk.Checked then
          t_chk.Color := clSilver
        else
          t_chk.ParentColor := true;
      end;
    end;
  end;
  t_rnames.Free();
end;

procedure TfrmDeviceManager.UpdateCheckBoxes(const fname: string);
const CSTR_PREFIX: string = 'Relais_0'; CSTR_INISEC: string = 'Relais_names';
var i: integer; s_rnum, s_rkey, s_rdesc: string;
    f_ini: TIniFile; t_chk: TCheckBox;
begin
  f_ini := TIniFile.Create(fname);
  if f_ini.SectionExists(CSTR_INISEC) then begin
    for i := 0 to tabRelays.ControlCount - 1 do begin
      t_chk := (tabRelays.Controls[i] as TCheckBox);
      if assigned(t_chk) then begin
        s_rnum := RightStr(t_chk.Name, length(t_chk.Name) - 4); //'chkR'+number
        s_rkey := CSTR_PREFIX + s_rnum;
        if f_ini.ValueExists(CSTR_INISEC, s_rkey) then begin
          s_rdesc := trim(f_ini.ReadString(CSTR_INISEC, s_rkey, ''));
          if StartsText('...', s_rdesc) then s_rdesc := RightStr(s_rdesc, length(s_rdesc) - 3);
          s_rdesc := s_rnum + ': ' + s_rdesc;
          if (s_rdesc <> '') then begin
            t_chk.Caption := s_rdesc;
            t_chk.Hint := s_rdesc;
            t_chk.ShowHint := true;
          end;
        end;
      end;
    end;
  end;
  f_ini.Free();
end;
end.
