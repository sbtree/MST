unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage, ConnBase, RelayControl,
  Multimeter, RS232;

type
  TfrmDeviceManager = class(TForm)
    btnInit: TButton;
    btnMeasure: TButton;
    cmbMeasure: TComboBox;
    btnClose: TButton;
    btnOpen: TButton;
    btnOpenAll: TButton;
    pgcMain: TPageControl;
    pgInfo: TTabSheet;
    memInfo: TMemo;
    btnConnect: TButton;
    tabRelays: TTabSheet;
    chkSelection: TCheckBox;
    txtMeasure: TEdit;
    btnUpdate: TButton;
    txtFile: TEdit;
    Label1: TLabel;
    btnFile: TButton;
    procedure btnInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenAllClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure RelayCheck(Sender: TObject);
    procedure chkSelectionClick(Sender: TObject);
  private
    t_messenger : TTextMessenger;
    t_multimeter: TMultimeterKeithley;
    t_relay: TRelayControl;
    t_ser: TSerialAdapter;
  protected
    function  SelectedRelays(): string;
    function  GetCheckBox(const relaynr: string): TCheckBox;
    procedure CreateCheckBoxes(const cards: integer);
    procedure ColorCheckBoxes(const relays: string);
    procedure UncolorCheckBoxes(const relays: string);
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
    s_rnum := RightStr(t_chk.Caption, length(t_chk.Caption) - 1);
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
        t_chk.Color := clGray
      else
        t_chk.ParentColor := true;
    end;
  end;
end;

procedure TfrmDeviceManager.btnConnectClick(Sender: TObject);
begin
  SerialDialog.SerialObj := t_ser.SerialObj;
  SerialDialog.ShowModal();
end;

procedure TfrmDeviceManager.btnInitClick(Sender: TObject);
begin
  t_multimeter.InitDevice();
  CreateCheckBoxes(t_multimeter.RelayCards);
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
var s_relays: string; i, j: integer;
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
      s_rnum := RightStr(t_chk.Caption, length(t_chk.Caption) - 1);
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

procedure TfrmDeviceManager.CreateCheckBoxes(const cards: integer);
var i, j, i_lmargin: integer; t_chkR: TCheckBox; s_rnum: string;
begin
  if tabRelays.ControlCount <= 0 then begin
    for j := 1 to cards do begin
      i_lmargin := (j - 1) * 150;
      for i := 0 to 39 do begin
        t_chkR := TCheckBox.Create(tabRelays);
        tabRelays.InsertControl(t_chkR);
        s_rnum := IntToStr(j * 100 + i + 1);
        t_chkR.Name := 'chkR' + s_rnum;
        t_chkR.Caption := 'R' + s_rnum;
        t_chkR.Width := 50;
        t_chkR.Height := 17;
        t_chkR.Left := (i div 20) * (t_chkR.Width + 10) + i_lmargin;
        t_chkR.Top := (i mod 20) * (t_chkR.Height + 2);
        t_chkR.OnClick := RelayCheck;
      end;
    end;
  end;
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
          t_chk.Color := clGray
        else
          t_chk.ParentColor := true;
      end;
    end;
  end;
  t_rnames.Free();
end;

end.
