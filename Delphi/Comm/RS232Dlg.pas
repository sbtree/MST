unit RS232Dlg;

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
    lblCom: TLabel;
    rdgParity: TRadioGroup;
    rdgDataBits: TRadioGroup;
    rdgStopBits: TRadioGroup;
    rdgFlowMode: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    t_ser: TSerial;
  public
    property SerialObj: TSerial read t_ser write t_ser;
  end;

var
  SerialDialog: TSerialDialog;

implementation

{$R *.dfm}

procedure TSerialDialog.btnOKClick(Sender: TObject);
var i_baud: integer;
begin
  if assigned(t_ser) then begin
    if TryStrToInt(cmbBaud.Text, i_baud) then t_ser.Baudrate := i_baud;
    t_ser.Parity := eParity(rdgParity.ItemIndex);
    t_ser.DataBits := eDataBits(rdgDataBits.ItemIndex);
    t_ser.StopBits := eStopBits(rdgStopBits.ItemIndex);
    t_ser.FlowMode := eFlowControl(rdgFlowMode.ItemIndex);
  end;
end;

procedure TSerialDialog.FormShow(Sender: TObject);
var i_index: integer;
begin
  if assigned(t_ser) then begin
    lblCom.Caption := t_ser.Name;

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
