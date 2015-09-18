unit TestFlashRunner;

interface

uses
  Windows, Serial3, Forms, StrUtils, Classes, ConnBase, SysUtils, DeviceBase, RS232, FlashRunner, 
  TestFramework, IniFiles;

type
  // Testmethoden für Klasse TFlashRunner
  
  TestTFlashRunner = class(TTestCase)
  var ini: TMemIniFile;
  strict private
    FFlashRunner: TFlashRunner;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfigDevice;
    procedure TestSetDynamicMem;
    procedure TestRunScript;
  end;

implementation

procedure TestTFlashRunner.SetUp;
begin
  FFlashRunner := TFlashRunner.Create(nil);
  ini := TMemIniFile.Create('FlashRunner.ini');
end;

procedure TestTFlashRunner.TearDown;
begin
  FFlashRunner.Free;
  FFlashRunner := nil;
end;

procedure TestTFlashRunner.TestConfigDevice;
var
  ReturnValue: Boolean;
begin
  // TODO: Methodenaufrufparameter einrichten
  ReturnValue := FFlashRunner.ConfigDevice(ini);
  // TODO: Methodenergebnisse prüfen
  CheckEquals(true, ReturnValue, 'OK');
end;

procedure TestTFlashRunner.TestSetDynamicMem;
var
  ReturnValue: Boolean;
  num: Integer;
  addr: Word;
begin
  // Methodenaufrufparameter einrichten
  addr := 0; num := 12345;
  ReturnValue := FFlashRunner.ConfigDevice(ini);
  //Methodenergebnisse prüfen
  CheckEquals(true, ReturnValue, 'OK');

  ReturnValue := FFlashRunner.SetDynamicMem(addr, num);
  //Methodenergebnisse prüfen
  CheckEquals(true, ReturnValue, 'OK');
end;

procedure TestTFlashRunner.TestRunScript;
var
  ReturnValue: Boolean;
  msecs: Integer;
  script: string;
begin
  //Methodenaufrufparameter einrichten
  script := 'bt_24v.frs'; msecs := 30000;
  ReturnValue := FFlashRunner.RunScript(script, msecs);

  // Methodenergebnisse prüfen
  CheckEquals(false, ReturnValue, 'OK');
end;

initialization
  RegisterTest(TestTFlashRunner.Suite);
end.
