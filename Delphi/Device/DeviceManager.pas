unit DeviceManager;

interface
uses Classes, Multimeter, Oscilloscope, PowerSupply, RelayControl, Thermometer, DeviceConfig;
type
  TDeviceManager = class
  protected
    t_dmm: TMultimeter;
    t_dso: TOscilloscope;
    t_pws: TPowerSupply;
    t_rlc: TRelayControl;
    t_tmm: TThermometer;

  end;

implementation

end.
