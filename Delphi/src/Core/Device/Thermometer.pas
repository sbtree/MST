unit Thermometer;

interface
uses Classes, DeviceBase, TextMessage;
type
  IThermometer = interface
    {function TemperatureC(var dval: double): boolean;
    function TemperatureK(var dval: double): boolean;}
  end;

  TThermometer = class(TDeviceBase, IThermometer)

  end;

implementation

end.
