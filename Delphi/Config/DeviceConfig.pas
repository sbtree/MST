unit DeviceConfig;

interface
uses Classes, SysUtils, ConfigBase, StringPairs;

type
  EDeviceType = (
                DT_MULTIMETER,
                DT_OSCILOSCOPE,
                DT_POWERSUPPLY,
                DT_RELAICONTROL,
                DT_THERMOMETER,
                DT_PCANADAPTOR
                );


  TDeviceConfigurator = class
  protected
    t_devices: TStrings;
  public
    constructor Create();
    destructor Destroy(); override;

    function LoadDeviceSettings(const devname, fname: string): boolean;
    function GetDeviceConfig(const devname: string): TConfigBase;
    function GetDeviceConfigSection(const devname, secname: string): TStringPairs;
    procedure ClearDevices();
  end;

implementation
//uses StrUtils, IniFiles;

constructor TDeviceConfigurator.Create();
begin
  inherited Create();
  t_devices := TStringList.Create();
end;

destructor TDeviceConfigurator.Destroy();
begin
  ClearDevices();
  t_devices.Free();
  inherited Destroy();
end;

function TDeviceConfigurator.LoadDeviceSettings(const devname, fname: string): boolean;
var t_devconf: TConfigBase;
begin
  t_devconf := GetDeviceConfig(devname);
  if (not assigned(t_devconf)) then t_devconf := TConfigBase.Create();
  result := t_devconf.UpdateFromFile(fname);
end;

function TDeviceConfigurator.GetDeviceConfig(const devname: string): TConfigBase;
var i_idx: integer;
begin
  result := nil;
  i_idx := t_devices.IndexOfName(devname);
  if (i_idx >= 0) then result := TConfigBase(t_devices.Objects[i_idx]);
end;

function TDeviceConfigurator.GetDeviceConfigSection(const devname, secname: string): TStringPairs;
var t_devconf: TConfigBase;
begin
  result := nil;
  t_devconf := GetDeviceConfig(devname);
  if assigned(t_devconf) then result := t_devconf.GetConfig(secname);
end;

procedure TDeviceConfigurator.ClearDevices();
begin
  while (t_devices.Count > 0) do
    if assigned(t_devices.Objects[0]) then begin
      t_devices.Objects[0].Free();
      t_devices.Delete(0);
    end;
end;

end.
