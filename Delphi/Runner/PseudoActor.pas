unit PseudoActor;

interface
uses Classes, PairStrings;
type
  TPseudoActor=class
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
                'BOOTLOADER_', //file of bootloader
                'BL_UPDATER_',
                'FIRMWARE_',
                'PARA_',
                'TX_FILE_',
                'FRS_',
                'TIMEOUT_',
                'BATCH_',
                'SonderRel_'
                );

  CCHR_STATIC_PSEUDOS : array of string = (
                //Pseudo-strings in array, which are not changed after selection of a product variant any more
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

                //Pseudo-strings, which are changed only once for each UUT at beginning of the test
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
                'PlatinenNrWord_',
                'StoSerNr',
                'SerNrDez5',
                'SerNrDez',
                'SerNr',
                'PCBHexNr',
                'PCBNr',

                //single Pseudo-strings, which are not changed after selection of a product variant any more
                'HWHexNr',
                'HWNr',
                'BTHWword_',
                'HWVersionHaupt',
                'HWVersionSub',
                'FSMHWVer',
                'HWVersion',
                'HW_4p4',
                'Kunde',
                'CUSTOMER_ID',
                'FWVariante',
                'HWHaupt_ASCII',
                'HWSub_ASCII',
                'Datum',
                'Woche',
                'Jahr',

                //pseudo-strings, which must be calculated or updated in real time
                'ArithInt16',
                'ArithInt32',
                'Echo',
                'VoltSupply',
                'NormStromU',
                'NormStromV',
                'StromOffsetU',
                'StromOffsetV',
                'I_KorrFaktor',
                'Msg'
                );

  CSTR_DYNAMIC_PSEUDOS : array of string = ( //
                'VarStr',
                'VarInt',
                'VarReal',
                'VarBool',
                'LastStr',
                'LastInt',
                'LastReal',
                'LastBool'
                );

implementation

function TPseudoActor.ReplacePseudos(const instr: string; var outstr: string): integer;
begin
  result := 0;
end;

end.
