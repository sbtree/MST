unit ComIdentMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ComCtrls, Serial3,
  Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    ltvCom: TListView;
    lstImages: TImageList;
    tmrUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    function FindItemByPort(const iport: integer): TListItem;
    procedure OnSignalChange(Sender : TObject ; Signal : eModemInSignal ; SignalState : Boolean);
    procedure CreateComList();
    procedure ClearComList();
    procedure ActiveComs();
    procedure DeactiveComs();
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation
uses RS232, StrUtils;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  CreateComList();
end;

function TForm3.FindItemByPort(const iport: integer): TListItem;
var i, i_port: integer; t_ser: TSerial;
begin
  result := nil;
  if assigned(ltvCom) then begin
    for i := 0 to ltvCom.Items.Count - 1 do begin
      t_ser := TSerial(ltvCom.Items[i].Data);
      if assigned(t_ser) then i_port := t_ser.Port
      else i_port := -1;
      //i_port := integer(ltvCom.Items[i].Data);
      if iport = i_port then begin
        result := ltvCom.Items[i];
        break;
      end;
    end;
  end;
end;

procedure TForm3.OnSignalChange(Sender : TObject ; Signal : eModemInSignal ; SignalState : Boolean);
var t_ser: TSerial; t_litem: TListItem;
begin
  t_ser := TSerial(sender);
  if assigned(t_ser) then begin
    t_litem := FindItemByPort(t_ser.Port);
    if assigned(t_litem) and t_ser.Active then begin
      if SignalState then t_litem.SubItemImages[0] := 3
      else t_litem.SubItemImages[0] := 1;
    end;
  end;
end;

procedure TForm3.tmrUpdateTimer(Sender: TObject);
begin
  tmrUpdate.Enabled := false;
  ActiveComs();
  //DeactiveComs();
  tmrUpdate.Enabled := true;
end;

procedure TForm3.CreateComList();
var i, i_port: integer; t_litem: TListItem; t_comnames: TStringList;
    s_curname: string; t_ser: TSerial;
begin
  t_comnames := TStringList.Create();
  TSerialAdapter.EnumSerialPort(t_comnames);
  for i := 0 to t_comnames.Count - 1 do begin
    s_curname := t_comnames[i];
    t_litem := ltvCom.Items.Add();
    t_litem.Caption := s_curname;
    t_litem.SubItems.Add('');
    t_litem.SubItemImages[0] := 1;
    if TryStrToInt(MidStr(s_curname, 4, length(s_curname) - 3), i_port) then begin
      t_ser := TSerial.Create(self);
      t_ser.Port := i_port;
      t_ser.OnSignalChange := OnSignalChange;;
      t_litem.Data := Pointer(t_ser); //Pointer(i_port);
    end;
  end;
end;

procedure TForm3.ClearComList();
var t_ser: TSerial;
begin
  while (ltvCom.Items.Count > 0) do begin
    t_ser := TSerial(ltvCom.Items[0].Data);
    if assigned(t_ser) then FreeAndNil(t_ser);
    ltvCom.Items.Delete(0);
  end;
end;

procedure TForm3.ActiveComs();
var i: integer; t_ser: TSerial;
begin
  if assigned(ltvCom) then begin
    for i := 0 to ltvCom.Items.Count - 1 do begin
      t_ser := TSerial(ltvCom.Items[i].Data);
      if assigned(t_ser) then begin
        t_ser.Active := true;
        if not t_ser.Active then ltvCom.Items[i].SubItemImages[0] := 4;
      end
    end;
  end;
end;

procedure TForm3.DeactiveComs();
var i: integer; t_ser: TSerial;
begin
  if assigned(ltvCom) then begin
    for i := 0 to ltvCom.Items.Count - 1 do begin
      t_ser := TSerial(ltvCom.Items[i].Data);
      if assigned(t_ser) then begin
        //t_ser.OnSignalChange := nil;
        t_ser.Active := false;
      end
    end;
  end;
end;

end.
