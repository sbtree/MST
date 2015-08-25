unit FRMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial3, StdCtrls, FlashRunner, IniFiles;

type
  TFRTester = class(TForm)
    btnStart: TButton;
    memDisplay: TMemo;
    txtCmd: TEdit;
    btnSend: TButton;
    btnSetDM: TButton;
    btnRunScript: TButton;
    lstSending: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnRunScriptClick(Sender: TObject);
    procedure btnSetDMClick(Sender: TObject);
    procedure lstSendingDblClick(Sender: TObject);
  private
  { Private-Deklarationen }
   end;

var
  FRTester: TFRTester;

implementation

{$R *.dfm}
const
  C_REAL_OK : Real = 0.0;
  C_REAL_NOK: Real = -1.0;
var FlashRunner: TFlashRunner;

function FR_Run_Script(const par: string): real;
var s_send, s_recv, s_temp: string; i_len, i_err: integer;
begin
  result := C_REAL_NOK;
  s_send := trim(par);
  i_len := length(s_send);
  if i_len > 0 then begin
    s_send := 'RUN ' + s_send + Char(13);
    i_len := FlashRunner.SendStr(s_send);
    if (i_len > 0) then begin
      s_temp := '>' + s_send;
      FRTester.memDisplay.Lines.Append(s_temp);
      i_len := FlashRunner.RecvStr(s_recv);
      if i_len > 0 then begin
        i_err := FlashRunner.GetLastError();
        if i_err = 0 then begin
          result := C_REAL_OK;
          //log message
          s_temp := '<' + s_recv;
          FRTester.memDisplay.Lines.Append(s_temp);
        end else begin
          //error message
          s_temp := format('<Error in Answer (%.4x): %s', [i_err, s_recv]);
          FRTester.memDisplay.Lines.Append(s_temp);
        end;
      end else begin
        s_temp := '<(No Answer: Empty)';
        FRTester.memDisplay.Lines.Append(s_temp);
      end;
    end else begin
      //error message
      s_temp := format('>Failed to Send: %s', [s_send]);
      FRTester.memDisplay.Lines.Append(s_temp);
    end;
  end else begin
    //error message
    s_temp := 'Error in Parameter: Empty';
    FRTester.memDisplay.Lines.Append(s_temp);
  end;
end;

function FR_Set_DM(const par: string): real;
var s_send, s_recv, s_temp: string; i_addr, i_data, i_len, i_err: integer; t_pars: TStringList;
begin
  result := C_REAL_NOK;
  t_pars := TStringList.Create;
  i_len := ExtractStrings([' '], [' ', Char(9)], PChar(par),t_pars);
  if i_len = 2 then begin
    if TryStrToInt(t_pars[0], i_addr) then begin
      if TryStrToInt(t_pars[1], i_data) then begin
        s_send := format('DMSET $%.4x 8 %s', [i_addr, FlashRunner.IntToDMDataStr(i_data)]) + Char(13);
        i_len := FlashRunner.SendStr(s_send);
        if ( i_len > 0 ) then begin
          s_temp := '>' + s_send;
          FRTester.memDisplay.Lines.Append(s_temp);
          i_len := FlashRunner.RecvStr(s_recv);
          if i_len > 0 then begin
            i_err := FlashRunner.GetLastError();
            if i_err = 0 then begin
              result := C_REAL_OK;
              //log message
              s_temp := '<' + s_recv;
              FRTester.memDisplay.Lines.Append(s_temp);
            end else begin
              //error message
              s_temp := format('<Error in Answer (%.4x): %s', [i_err, s_recv]);
              FRTester.memDisplay.Lines.Append(s_temp);
            end;
          end else begin
            s_temp := '<(No Answer: Empty)';
            FRTester.memDisplay.Lines.Append(s_temp);
          end;
        end else begin
          //error message
          s_temp := format('>Failed to Send: %s', [s_send]);
          FRTester.memDisplay.Lines.Append(s_temp);
        end;
      end else begin
        //error message
        s_temp := format('Failed in Parameter:  %s', [t_pars[1]]);
        FRTester.memDisplay.Lines.Append(s_temp);
      end;
    end else begin
      //error message
      s_temp := format('Failed in Parameter:  %s', [t_pars[0]]);
      FRTester.memDisplay.Lines.Append(s_temp);
    end;
  end else begin
    //error message
    s_temp := '<Error in Parameter: 2 Parameter is needed.';
    FRTester.memDisplay.Lines.Append(s_temp);
  end;
  t_pars.Clear;
  FreeAndNil(t_pars);
end;

procedure TFRTester.btnRunScriptClick(Sender: TObject);
var str_temp: string; i_ms: cardinal;
begin
  if not FlashRunner.Connect() then exit;

  str_temp := trim(txtCmd.Text);
  if lstSending.Items.IndexOf(str_temp) < 0 then lstSending.Items.Add(str_temp);

  i_ms := GetTickCount();
  FR_Run_Script(str_temp);
  i_ms := GetTickCount() - i_ms;
  str_temp := format('elapse: %d ms', [i_ms]);
  memDisplay.Lines.Append(str_temp);
end;

procedure TFRTester.btnSendClick(Sender: TObject);
var str_temp: string; i_ms: cardinal;
begin
  if not FlashRunner.Connect() then exit;

  str_temp := trim(txtCmd.Text);
  if lstSending.Items.IndexOf(str_temp) < 0 then lstSending.Items.Add(str_temp);

  i_ms := GetTickCount();
  if (FlashRunner.SendStr(str_temp + Char(13)) > 0) then self.memDisplay.Lines.Append('>' + str_temp);
  if (FlashRunner.RecvStr(str_temp) > 0) then self.memDisplay.Lines.Append('<' + str_temp);
  i_ms := GetTickCount() - i_ms;
  str_temp := format('elapse: %d ms', [i_ms]);
  memDisplay.Lines.Append(str_temp);
end;

procedure TFRTester.btnSetDMClick(Sender: TObject);
var str_temp: string; i_ms: cardinal;
begin
  if not FlashRunner.Connect() then exit;

  str_temp := trim(txtCmd.Text);
  if lstSending.Items.IndexOf(str_temp) < 0 then lstSending.Items.Add(str_temp);

  i_ms := GetTickCount();
  FR_Set_DM(str_temp);
  i_ms := GetTickCount() - i_ms;
  str_temp := format('elapse: %d ms', [i_ms]);
  memDisplay.Lines.Append(str_temp);
end;

procedure TFRTester.btnStartClick(Sender: TObject);
begin
  //btnStart.Caption := '&Start';
  if FlashRunner.Connect() then begin
    if FlashRunner.Coordinate() then begin
      //btnStart.Caption := '&Stop';
    end;
  end;
end;

procedure TFRTester.FormCreate(Sender: TObject);
var t_ini: TMemIniFile;
begin
  t_ini := TMemIniFile.Create('FlashRunner.ini');
  if not assigned(FlashRunner) then FlashRunner := TFlashRunner.Create(self);
  FlashRunner.ConfigDevice(t_ini);
  FreeAndNil(t_ini);
end;

procedure TFRTester.FormDestroy(Sender: TObject);
begin
  if assigned(FlashRunner) then FreeAndNil(FlashRunner);
end;


procedure TFRTester.lstSendingDblClick(Sender: TObject);
begin
  txtCmd.Text := lstSending.Items[lstSending.ItemIndex]
end;

end.
