unit PseudoInstructor;

interface
uses Classes, PairStrings;
type
  TPseudoInstructor=class
  protected
    t_staticpairs: TPairStrings;
  protected

  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateStaticPseudos(const vars: TPairStrings): integer;
    function ReplacePseudos(const instr: string; var outstr: string): integer;
  end;

const
  CCHR_SPECIAL_PSEUDOS : Array of string = (
                'BOOTLOADER_', //file of bootloader 0001-0010
                'BL_UPDATER_',
                'FIRMWARE_',
                'PARA_',
                'TX_FILE_',
                'FRS_',
                'TIMEOUT_',
                'BATCH_'
                );

  CCHR_STATIC_PSEUDOS : array of string = (
                'TxVer',
                'BLVersion',
                'BLVerMain',
                'BLVerApp',
                'BLVerKM',
                'FWVersion',
                'FWVerMain',
                'FWVerApp',
                'FWVerKM',
                'FRSName',

                'ArithInt16',
                'ArithInt32',
                'Datum',
                'Woche',
                'Jahr',
                'Echo',
                'VoltSupply',
                'PlatinenNrWord_',
                'StoSerNr',
                'SerNrDez5',
                'SerNrDez',
                'SerNr',
                'HWHexNr',
                'PCBHexNr',
                'PCBNr',
                'HWNr',
                'BTHWword_',
                'HWVersionHaupt',
                'HWVersionSub',
                'FSMHWVer',
                'HWVersion',
                'HW_4p4',
                'PlatNrDez10',
                'Plat2NrDez10',
                'PlatNr32',
                'Plat2Nr32',
                'PlatinenNr1',
                'Platinen_Nr1_str',
                'PlatinenNr2',
                'Platinen_Nr2_str',
                'PlatinenNr3',
                'Platinen_Nr3_str',
                'Kunde',
                'CUSTOMER_ID',
                'FWVariante',
                'HWHaupt_ASCII',
                'HWSub_ASCII',
                'NormStromU',
                'NormStromV',
                'StromOffsetU',
                'StromOffsetV',
                'I_KorrFaktor',
                'SonderRel_',
                'Msg'
                );

  CSTR_DYNAMIC_PSEUDOS : array of string = (
                'VarStr',
                'VarInt',
                'VarReal',
                'LastStr',
                'LastInt',
                'LastReal'
                );

implementation

function TPseudoInstructor.ReplacePseudos(const instr: string; var outstr: string): integer;
begin
  result := 0;
end;

end.
