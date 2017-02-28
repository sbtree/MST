unit RS232Dlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Serial3;

type
  TSerialDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    grpParity: TGroupBox;
    lblBaud: TLabel;
    cmbBaud: TComboBox;
    grpDataBits: TGroupBox;
    rdbD8Bit: TRadioButton;
    rdbD7Bit: TRadioButton;
    grpStopBits: TGroupBox;
    rdbStopBit1: TRadioButton;
    rdbStopBit2: TRadioButton;
    grpFlowMode: TGroupBox;
    rdbFcNone: TRadioButton;
    rdbFcRtsCts: TRadioButton;
    rdbFcXonXoff: TRadioButton;
    rdbFcDtrDsr: TRadioButton;
    rdbNone: TRadioButton;
    rdbOdd: TRadioButton;
    rdbEven: TRadioButton;
    rdbMark: TRadioButton;
    rdbSpace: TRadioButton;
    bvlCom: TBevel;
    lblCom: TLabel;
    procedure FormShow(Sender: TObject);
  private
    t_ser: TSerial;
  public
    property SerialObj: TSerial read t_ser write t_ser;
  end;

var
  SerialDialog: TSerialDialog;

implementation

{$R *.dfm}

procedure TSerialDialog.FormShow(Sender: TObject);
var i_index: integer;
begin
  if assigned(t_ser) then begin
    lblCom.Caption := t_ser.Name;

    i_index := cmbBaud.Items.IndexOf(IntToStr(t_ser.Baudrate));
    if (i_index >= 0) then cmbBaud.ItemIndex := i_index
    else cmbBaud.Text := IntToStr(t_ser.Baudrate);

    case t_ser.Parity of
      paNone: rdbNone.Checked := true;
      PaOdd: rdbOdd.Checked := true;
      paEven: rdbEven.Checked := true;
      paMark: rdbMark.Checked := true;
      paSpace: rdbSpace.Checked := true;
    end;

    case t_ser.DataBits of
      d7bit: rdbD7Bit.Checked := true;
      d8bit: rdbD8Bit.Checked := true;
    end;

    case t_ser.FlowMode of
      fcNone: rdbFcNone.Checked := true;
      fcRTS_CTS: rdbFcRtsCts.Checked := true;
      fcDTR_DSR: rdbFcDtrDsr.Checked := true;
      fcXON_XOF: rdbFcXonXoff.Checked := true;
    end;
  end;
end;

end.
