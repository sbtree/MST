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
                //Pseudo-String without beginning char '@'
                'BOOTLOADER_', //file of boot loader, e.g.: BOOTLOADER_0001
                'BL_UPDATER_', //file of bool loader updater, e.g.: BL_UPDATER_0001
                'FIRMWARE_',   //file of firmware, e.g.: FIRMWARW_0001
                'PARA_',       //file of parameter set, e.g.: PARA_0001
                'TX_FILE_',    //eeprom file, e.g.: TX_FILE_0001
                'FRS_',        //file name of flash runner, e.g.: FRS_0001
                'TIMEOUT_',
                'BATCH_',
                'SonderRel_'
                );

  CCHR_STATIC_PSEUDOS : array of string = (
                //Pseudo-Strings with beginning char '@'
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
                'Echo', //cooper device ?
                'VoltSupply', //???
                'NormStromU', //???
                'NormStromV', //???
                'StromOffsetU',//???
                'StromOffsetV',//???
                'I_KorrFaktor',
                'Msg',

                //Pseudo-strings, which are changed only through a call-function in the script
                'VarStr',
                'VarInt',
                'VarReal',
                'VarBool',

                //Pseudo-strings, which are changed by running each test step
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
