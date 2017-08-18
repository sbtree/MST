//==============================================================================
// Module name  : $RCSfile: UnitsSystem.pas,v $
// Description  : This unit defines types, constants and functions for calculation
//                of units system for Metronix products
// Copyright    : (c) Metronix 2016
// Reversion    : $Revision 1.0$
// Compiler     : Delphi 2007
// Author       : 2016-10-10 /bsu/
// History      :
//==============================================================================
unit UnitsSystem;

interface
type
  ESystemUnit = ( SU_CURR,  //current, 32 bit
                  SU_ACCEL, //acceleration, 32 bit
                  SU_SPEED, //speed, 32 bit
                  SU_POS,   //position, 32 bit
                  SU_TORQUE,//torque, 32 bit
                  SU_VOLT,  //voltage, 32 bit
                  SU_ROTOR_ANGLE,//rotor speed, 32 bit
                  SU_AMPL,  //amplatude, 32 bit
                  SU_TIME,  //time, 32 bit
                  SU_TEMP,  //temperature, 16 bit
                  SU_PERC,  //percent, 16 bit
                  SU_RESIST,//resistance, 32 bit
                  SU_INDUC, //induction, 32 bit
                  SU_POWER  //power(performance)
                );

  function CalcUnitValue(const sval: string; const eunit: ESystemUnit; var dval: double): boolean;

implementation
uses SysUtils;

const
  CSTR_UNITS_EN: array[ESystemUnit] of string = (
              //CURR, ACCEL, SPEED, POS, TORQUE, VOLT, ROTOR_ANGLE, AMPL, TIME, TEMP, PERC, RESIST, INDUC, POWER
              'A', 'rpm/s', 'rpm', 'r', 'Nm/A', 'V', 'r', '', 's', '°C', '', 'Ohm', 'H', 'W');
  CSTR_UNITS_DE: array[ESystemUnit] of string = (
              'A', 'Upm/s', 'Upm', 'U', 'Nm/A', 'Volt', 'U', '', 's', '°C', '', 'Ohm', 'H', 'W');
  C_UNITS_SCALE: array[ESystemUnit] of double = (
              //rciprocal of 2^16, 2^8, 2^12, 2^16, 2^12, 2^16, 2^16, 2^16, 10^7, 2^4, 2^16, 2^8, 2^14, 2^8
              1/65536, 1/256, 1/4096, 1/65536, 1/4096, 1/65536, 1/65536, 1/65536, 10E-7, 1/16, 1/65536, 1/256, 1/16384, 1/256);

function CalcUnitValue(const sval: string; const eunit: ESystemUnit; var dval: double): boolean;
begin
  result := TryStrToFloat(sval, dval);
  if result then dval := dval * (C_UNITS_SCALE[eunit]);
end;

end.
