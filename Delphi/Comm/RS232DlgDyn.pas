unit RS232DlgDyn;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Serial3;

type
  TSerialDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblBaud: TLabel;
    cmbBaud: TComboBox;
    bvlCom: TBevel;
    rdgParity: TRadioGroup;
    rdgDataBits: TRadioGroup;
    rdgStopBits: TRadioGroup;
    rdgFlowMode: TRadioGroup;
    cmbPort: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    t_ser: TSerial;
  public
    property SerialObj: TSerial read t_ser write t_ser;
  end;

var
  SerialDialog: TSerialDialog;

implementation
uses RS232, StrUtils;

{$R *.dfm}

procedure TSerialDialog.btnOKClick(Sender: TObject);
var i_baud, i_port: integer; s_port: string;
begin
  if assigned(t_ser) then begin
    s_port := trim(cmbPort.Text);
    if TryStrToInt(MidStr(s_port, 4, length(s_port)), i_port) then t_ser.Port := i_port;
    if TryStrToInt(cmbBaud.Text, i_baud) then t_ser.Baudrate := i_baud;
    t_ser.Parity := eParity(rdgParity.ItemIndex);
    t_ser.DataBits := eDataBits(rdgDataBits.ItemIndex);
    t_ser.StopBits := eStopBits(rdgStopBits.ItemIndex);
    t_ser.FlowMode := eFlowControl(rdgFlowMode.ItemIndex);
  end;
end;

procedure TSerialDialog.FormCreate(Sender: TObject);
var t_pnames: TStringList;
begin
  t_pnames := TStringList.Create();
  TSerialAdapter.EnumSerialPort(t_pnames);
  cmbPort.Items.AddStrings(t_pnames);
  t_pnames.Free();
end;

procedure TSerialDialog.FormShow(Sender: TObject);
var i_index: integer; s_pname: string;
begin
  if assigned(t_ser) then begin
    s_pname := 'COM' + IntToStr(t_ser.Port);
    i_index := cmbPort.Items.IndexOf(s_pname);
    if (i_index >= 0) then cmbPort.ItemIndex := i_index
    else cmbBaud.Text := s_pname;

    i_index := cmbBaud.Items.IndexOf(IntToStr(t_ser.Baudrate));
    if (i_index >= 0) then cmbBaud.ItemIndex := i_index
    else cmbBaud.Text := IntToStr(t_ser.Baudrate);
    rdgParity.ItemIndex := Ord(t_ser.Parity);
    rdgDataBits.ItemIndex := Ord(t_ser.DataBits);
    rdgStopBits.ItemIndex := Ord(t_ser.StopBits);
    rdgFlowMode.ItemIndex := Ord(t_ser.FlowMode);
  end;
end;

end.
