unit ComIdentMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ComCtrls, Serial3, RS232, TextMessage,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    ltvCom: TListView;
    lstImages: TImageList;
    txtSend: TEdit;
    btnSend: TButton;
    memoLog: TMemo;
    chkCR: TCheckBox;
    chkLN: TCheckBox;
    cmbCom: TComboBox;
    btnStart: TButton;
    btnStop: TButton;
    btnHelp: TButton;
    tmrActive: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ltvComDblClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure txtSendKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateListView();
    procedure cmbComSelect(Sender: TObject);
    procedure cmbComEnter(Sender: TObject);
   procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure tmrActiveTimer(Sender: TObject);
  private
    function FindItemByName(const sname: string): TListItem;
    function IsAllComsActive(): boolean;
    procedure OnSignalChange(Sender : TObject ; Signal : eModemInSignal ; SignalState : Boolean);
    procedure UpdateComList();
    procedure AddOneCom(const sname: string);
    procedure RemoveCom(const sname: string); overload;
    procedure RemoveCom(const idx: integer); overload;
    procedure ClearComList();
    procedure SetComsActive(const bact: boolean);
  private
    t_comlist: TStrings;
    b_active: boolean;
    t_seradapter: TSerialAdapter;
    t_txtmessenger: TTextMessenger;
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation
uses RS232Dlg, ComIdentHelp, GenUtils, StrUtils;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  t_comlist := TStringList.Create();
  b_active := false;

  t_txtmessenger := TTextMessenger.Create();
  t_txtmessenger.Messages := memoLog.Lines;

  t_seradapter := TSerialAdapter.Create(self);
  ITextMessengerImpl(t_seradapter).Messenger := t_txtmessenger;

  UpdateListView();
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  ClearComList();
  FreeAndNil(t_comlist);
  t_seradapter.Free();
  t_txtmessenger.Free();
end;

procedure TForm3.UpdateListView();
var i: integer; t_item: TListItem; s_comname: string;
begin
  UpdateComList();
  //remove all items from the list view which don't exist any more
  for i := ltvCom.Items.Count - 1 downto 0 do begin
    s_comname := ltvCom.Items[i].Caption;
    if (t_comlist.IndexOf(s_comname) < 0) then ltvCom.Items.Delete(i);
  end;

  //update with exiting com
  for i := 0 to t_comlist.Count - 1 do begin
    s_comname := t_comlist.Strings[i];
    t_item := self.FindItemByName(s_comname);
    if not assigned(t_item) then begin
      t_item := ltvCom.Items.Add();
      t_item.Caption := s_comname;
      t_item.SubItems.Add('');
      t_item.SubItemImages[0] := 1;
    end;
  end;
end;

procedure TForm3.ltvComDblClick(Sender: TObject);
var t_ser: TSerial; i_index: integer; t_item: TListItem;
begin
  t_item := ltvCom.Selected;
  if assigned(t_item) then begin
    i_index := t_comlist.IndexOf(ltvCom.Selected.Caption);
    if (i_index >= 0) then begin
      t_ser := TSerial(t_comlist.Objects[i_index]);
      if assigned(t_ser) then begin
        SerialDialog.SerialObj := t_ser;
        SerialDialog.ShowModal();
      end;
    end;
  end;
end;

function TForm3.FindItemByName(const sname: string): TListItem;
begin
  result := ltvCom.FindCaption(0, sname, false, true, false);
end;

function TForm3.IsAllComsActive(): boolean;
var i: integer; t_ser: TSerial;
begin
  result := true;
  for i := 0 to t_comlist.Count -1 do begin
    t_ser := TSerial(t_comlist.Objects[i]);
    if (not t_ser.Active) then begin
      result := false;
      break;
    end;
  end;
end;

procedure TForm3.OnSignalChange(Sender : TObject ; Signal : eModemInSignal ; SignalState : Boolean);
var t_ser: TSerial; t_litem: TListItem;
begin
  t_ser := TSerial(sender);
  if (assigned(t_ser) and (Signal in [msCTS, msDSR, msRLSD])) then begin
    t_litem := FindItemByName(t_ser.Name);
    if (assigned(t_litem) and t_ser.Active) then begin
      if SignalState then
        t_litem.SubItemImages[0] := 3
      else
        t_litem.SubItemImages[0] := 1;
    end;
  end;
end;

procedure TForm3.tmrActiveTimer(Sender: TObject);
begin
  tmrActive.Enabled := false;
  UpdateListView();
  SetComsActive(true);
  tmrActive.Enabled := (not IsAllComsActive());
end;

procedure TForm3.txtSendKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) then btnSendClick(btnSend);
end;

procedure TForm3.UpdateComList();
var i: integer; t_comnames: TStringList;
begin
  t_comnames := TStringList.Create();
  TSerialAdapter.EnumSerialPort(t_comnames);
  //remove items from t_comlist which don't exist any more
  for i := t_comlist.Count - 1 downto 0 do begin
    if (t_comnames.IndexOf(t_comlist[i]) < 0) then RemoveCom(i);
  end;

  //add items into t_comlist which are not included
  for i := 0 to t_comnames.Count - 1 do begin
    AddOneCom(t_comnames[i]);
  end;
  t_comnames.Free();
end;

procedure TForm3.AddOneCom(const sname: string);
var i_port: integer; t_ser: TSerial;
begin
  if (t_comlist.IndexOf(sname) < 0) then begin
    if TryStrToInt(MidStr(sname, 4, length(sname) - 3), i_port) then begin
      t_ser := TSerial.Create(self);
      t_ser.Baudrate := 9600;
      t_ser.CheckParity := false;
      t_ser.DataBits := d8Bit;
      t_ser.NotifyErrors := neNone;
      t_ser.FlowMode := fcNone;
      t_ser.Port := i_port;
      t_ser.OnSignalChange := OnSignalChange;
      t_ser.Name := sname;
      t_comlist.AddObject(sname, t_ser);
    end;
  end;
end;

procedure TForm3.RemoveCom(const sname: string);
var i_index: integer;
begin
  i_index := t_comlist.IndexOf(sname);
  RemoveCom(i_index);
end;

procedure TForm3.RemoveCom(const idx: integer);
var t_ser: TSerial;
begin
  if ((idx >= 0) and (idx < t_comlist.Count))  then begin
    t_ser := TSerial(t_comlist.Objects[idx]);
    if assigned(t_ser) then begin
      t_ser.Active := false;
      t_ser.Free();
    end;
    t_comlist.Delete(idx);
  end;
end;

procedure TForm3.btnHelpClick(Sender: TObject);
begin
  LoopbackDlg.ShowModal();
end;

procedure TForm3.btnSendClick(Sender: TObject);
var s_sending, s_recv: string;
begin
  s_sending := trim(txtSend.Text);
  if chkCR.Checked then s_sending := s_sending + Char(13);
  if chkLN.Checked then s_sending := s_sending + Char(10);
  if (not t_seradapter.Connected) then t_seradapter.Connect();
  if t_seradapter.SendStr(s_sending) then t_seradapter.RecvStrInterval(s_recv, t_seradapter.Timeout, 300);
end;

procedure TForm3.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled := false;
  tmrActiveTimer(Sender);
end;

procedure TForm3.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled := true;
  SetComsActive(false);
  tmrActive.Enabled := false;
end;

procedure TForm3.ClearComList();
begin
  while (t_comlist.Count > 0) do RemoveCom(0);
end;

procedure TForm3.cmbComEnter(Sender: TObject);
begin
  //update combibox
  cmbCom.Items.Clear();
  cmbCom.Items.AddStrings(t_comlist);
end;

procedure TForm3.cmbComSelect(Sender: TObject);
var i_index: integer;
begin
  if (cmbCom.ItemIndex >= 0) then begin
    i_index := t_comlist.IndexOf(cmbCom.Items[cmbCom.ItemIndex]);
    if (i_index >= 0) then t_seradapter.SerialObj := TSerial(t_comlist.Objects[i_index])
    else t_seradapter.SerialObj := nil;
  end else t_seradapter.SerialObj := nil;
end;

procedure TForm3.SetComsActive(const bact: boolean);
var i: integer; t_ser: TSerial; t_item: TListItem;
begin
  if assigned(t_comlist) then begin
    for i := 0 to t_comlist.Count - 1 do begin
      t_ser := TSerial(t_comlist.Objects[i]);
      if assigned(t_ser) then begin
        if (t_ser.Active <> bact) then t_ser.Active := bact;
        t_item := FindItemByName(t_comlist.Strings[i]);
        if assigned(t_item) then begin
          if (bact) then begin
            if t_ser.Active then t_item.SubItemImages[0] := 1
            else t_item.SubItemImages[0] := 4;
          end else
            t_item.SubItemImages[0] := 1;
        end;
      end;
      Application.ProcessMessages();
    end;
  end;
end;

end.
