unit commThreadIS1885L;
{ Zero-based indexation of AI, DI, AO, DO. This way it was all tested. }

{$define EXTERNAL_PROCEDURES} // last testing done with this enabled
{$define RAMP_UP}
{$define RAMP_DOWN} // nop while not defined RAMP_UP
{$ifdef DEMO}
  {$define STANDALONE_IO40}
{$endif}

//{$define SERVICE}
//{$define STANDALONE_IO40}
//{$define MIN_MAX}

INTERFACE

uses
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdModBusClient, ModbusTypes,
  Classes, SyncObjs;


const
  SHORT_DEVICE_DESCRIPTION = 'IS 1885L';

  { Useful default values }
  cModbusTcpPortNrDefault   = 502;
  cModbusTcpConnTimeout     = 500;
  cModbusTcpReadTimeout     = 200;
  cActionTimeoutDefault     = 1000;
  cWaitforVoltageTimeout    = 500;
//  cCSAccessTimeoutDefault   = 5000;
  cUpdateIntervalDefault    = 250;
  cUpdateGapDefault         = 10;
  cActionTimeoutDebug       = 10000;
  cUpdateIntervalDebug      = 10000;
  cModbusTcpBaseRegDefault  = 0;
  cPowerSupplyRange         = 30.0;
  cPowerSupplyVoltageMin    = 15.0;
  cPowerSupplyVoltageMax    = 24.0;

  cParTestTimeLimitMin      = 1.0;
  cParTestTimeLimitMax      = 999.9;
  cParTestTimeDefault       = 5.0;

  cParRampTimeLimitMin      = 0.0;
  cParRampTimeLimitMax      = 99.9;
  cParRampTimeDefault       = 0.0;

  cParUstartLimitMin        = 0.0;
  cParUstartLimitMax        = 6000.0;
  cParUstartDefault         = 0.0;

  cParUnomLimitMin          = 100.0;
  cParUnomLimitMax          = 6000.0;
  cParUnomDefault           = 1000.0;
  cUminPercDef              = 0.9;
  cUmaxPercDef              = 1.1;

  cParIminLimitMin          = 0.0;
  cParIminLimitMax          = 1e-2;
  cParIminDefault           = 0.0;

  cParImaxLimitMin          = 1e-8; // 10 nA
  cParImaxLimitMax          = 1e-2; // 10 mA
  cParImaxDefault           = 1e-3; 

  cParRminLimitMin          = 0.0;
//  cParRminLimitMax          = 1e9;  // variable calculation (see T_IS1885.calcRrange() method)
  cParRminDefault           = 1e6;

  cParPIminLimitMin         = 0.0;
  cParPIminLimitMax         = 1e3;
  cParPIminDefault          = 80.0;

  cParPImaxLimitMin         = 0.0;
  cParPImaxLimitMax         = 1e3;
  cParPImaxDefault          = 300.0;   

  cParDARminLimitMin        = 0.0;
  cParDARminLimitMax        = 1e3;
  cParDARminDefault         = 80.0;

  cParDARmaxLimitMin        = 0.0;
  cParDARmaxLimitMax        = 1e3;
  cParDARmaxDefault         = 150.0;  

  cParDetectDelayLimitMin   = 1.0;
  cParDetectDelayLimitMax   = 100.0;
  cParDetectDelayDefault    = 10.0;


  { 'I' = 0b01001001
               XXXXX
    'S' = 0b01010011
               YYYYY

    0x4CC1 = 0b | 01001 100 | 11 000001 |
                | XXXXX YYY | YY ZZZZZZ | (where ZZZZZZ is counter of IS devices :)
  }
  cDefaultIdIO40IS1885L     = $4CC1;

  { Error codes ans status codes }
  OK                        = 0;
  ErrorGeneral              = -1;
  ErrorDisconnected         = -2;
  ErrorNotYetReady          = -3;
  ErrorBusy                 = -4;
  ErrorInvalidAction        = -5;
  ErrorInvalidRegNr         = -6;
  ErrorNotRequested         = -7;
  ErrorTimedOut             = -8;
  ErrorCommunication        = -9;
  ErrorInvalidAddress       = -10;
  ErrorInvalidDINumber      = -11;
  ErrorInvalidDONumber      = -12;
  ErrorInvalidAINumber      = -13;
  ErrorInvalidAONumber      = -14;
  ErrorValueOutOfRange      = -15;
  ErrorShortArray           = -16;
  ErrorTooManyValues        = -17;
  ErrorNoArrayReference     = -18;
  ErrorPowerSupplyLowVolt   = -19;
  ErrorPowerSupplyHighVolt  = -20;
  ErrorInvalidRange         = -21;
  ErrorInvalidTest          = -22;
  ErrorInvalidID            = -23;
  ErrorHG40NotReady         = -24;
  ErrorAlreadyInTest        = -25;
  ErrorInvalidTestParam     = -26;
  ErrorNotYetPreparedTest   = -27;
  ErrorNoOptUsense          = -28;
  ErrorInvalidTestParamRng  = -29;

  StatNoActionYet           = 1;

  { Range of temporary array of words (for writing of multiple register range at once). }
  cTempRegRangeMin          = 0;
  cTempRegRangeMax          = 15;

  { Input register image }
  AddrRng_InImg_AO_Min      = $00;
  Addr_InImg_AO1            = $01;
  Addr_InImg_AO2            = $03;
  AddrRng_InImg_AO_Max      = $03;
  AddrRng_InImg_AI_Min      = $04;
  Addr_InImg_AI1            = $05;
  Addr_InImg_AI2            = $07;
  Addr_InImg_AI3            = $09;
  Addr_InImg_AI4            = $0B;
  AddrRng_InImg_AI_Max      = $0B;
  AddrRng_InImg_HG40I_Min   = $0C;
  AddrRng_InImg_HG40I_Max   = $0D;
  AddrRng_InImg_HG40U_Min   = $0E;
  AddrRng_InImg_HG40U_Max   = $0F;
  AddrRng_InImg_AllAI_Max   = $0F;
  AddrRng_InImg_DI_Min      = $10;
  AddrRng_InImg_DI_Max      = $10;

  AddrRng_InImg_Min         = AddrRng_InImg_AO_Min;
  AddrRng_InImg_Max         = AddrRng_InImg_DI_Max;

  { Output register image }
  OutImg_Offset             = $0800;
  AddrRng_OutImg_AO_Min     = AddrRng_InImg_AO_Min    + OutImg_Offset;
  Addr_OutImg_AO1           = Addr_InImg_AO1          + OutImg_Offset;
  Addr_OutImg_AO2           = Addr_InImg_AO2          + OutImg_Offset;
  AddrRng_OutImg_AO_Max     = AddrRng_InImg_AO_Max    + OutImg_Offset;
  AddrRng_OutImg_AI_Min     = AddrRng_InImg_AI_Min    + OutImg_Offset;
  AddrRng_OutImg_AI_Max     = AddrRng_InImg_AI_Max    + OutImg_Offset;
  AddrRng_OutImg_HG40I_Min  = AddrRng_InImg_HG40I_Min + OutImg_Offset;
  AddrRng_OutImg_HG40I_Max  = AddrRng_InImg_HG40I_Max + OutImg_Offset;
  AddrRng_OutImg_HG40U_Min  = AddrRng_InImg_HG40U_Min + OutImg_Offset;
  AddrRng_OutImg_HG40U_Max  = AddrRng_InImg_HG40U_Max + OutImg_Offset;
  AddrRng_OutImg_DO_Min     = AddrRng_InImg_DI_Min    + OutImg_Offset;
  AddrRng_OutImg_DO_Max     = AddrRng_InImg_DI_Max    + OutImg_Offset;

  AddrRng_OutImg_Min        = AddrRng_OutImg_AO_Min;
  AddrRng_OutImg_Max        = AddrRng_OutImg_DO_Max;

  { Special register addresses }
  AddrSpec_CouplerID_first  = $1000;
  AddrSpec_CouplerID_last   = $1005;
  AddrSpec_IO40ID           = $1006;

  AddrSpec_CouplerDiag      = $100B;
  AddrSpec_CouplerStatus    = $100C;

  AddrSpec_AOBitCount       = $1010;
  AddrSpec_AIBitCount       = $1011;
  AddrSpec_DOBitCount       = $1012;
  AddrSpec_DIBitCount       = $1013;

  AddrSpec_WDogTime         = $1020;
  AddrSpec_WDogTimeout      = $1120;
  
  AddrSpec_Cal_Status_Cont  = $4000;  // Status control register for Calibration storage on HG40, not used for now.
  AddrSpec_Cal_Data         = $4001;  // Start Register for Calibration Data.
  AddrSpec_Cal_Data_Max     = $4041;  // End Register for Calibration Data,64 registers.


  { Digital inputs - used }
  DigIn_Option_USense       = 0;
  DigIn_HG40_Ready          = 9;
  DigIn_HG40_100uA          = 10;
  DigIn_HG40_10mA           = 11;
  DigIn_HG40_SPO            = 12;

  DigIn_min                 = DigIn_Option_USense;
  DigIn_max                 = DigIn_HG40_SPO;

  { Digital outputs - used (new DO layout requested by WL on 2020-12-02 - similar to ST 3810) }    
  DigOut_K1_HV_Normal       = 0;  // 5;
  DigOut_K2_HV_Inverted     = 1;  // 6;
  DigOut_K3_HV_Sense        = 2;  // 7;
  DigOut_ResetAll           = 3;  // 0;
  DigOut_HV_Enable          = 4;  // 3;
  DigOut_K_Discharge        = 5;  // 4;
  DigOut_10mA_Range         = 6;  // 1;
  DigOut_100uA_Range        = 7;  // 2;

  DigOut_min                = 0;
  DigOut_max_used           = 7;  //  only first 8 DOs are used - rest 8 are reserved
  DigOut_max                = 15;

  { Analog inputs - used }
  AlogIn_24V                = 0;
  AlogIn_IHV                = 1;
  AlogIn_UHV                = 2;
  AlogIn_TRMS_Usense        = 3;
  
  AlogIn_min                = AlogIn_24V;
  AlogIn_max                = AlogIn_TRMS_Usense;

  { Analog outputs - used }
  AlogOut_Usoll             = 0;
  
  AlogOut_min               = AlogOut_Usoll;
  AlogOut_max               = AlogOut_Usoll + 1;  // 1 currently unused analog output

  { Register numbers - for beckhoff register communication }
  RegNr_Ustart              = 0;
  RegNr_RampTime            = 1;

  { Window for resistance averaging }
  cAvgResWinSizeMin         = 1;
  cAvgResWinSizeMax         = 10;
  cAvgResDelayMin           = 0;
  cAvgResDelayMax           = 10;


{ Originally hidden (in implementations ection) 3 data types - moved here to enable debugging in service tool. }
{ (TIRange enum and TTestPhase enum and TTestStatus record) }
type
  TIRange = ( ir1uA
            , ir100uA
            , ir10mA
            );


  TTestPhase = ( tpIdle
               , tpWaitVolt 
               , tpRampUp
               , tpTest
               , tpRampDown
               , tpFinishing
               );


  TTestStatus = record
    ReadyToStart:     Boolean;
    Finishing:        Boolean;
    Finished:         Boolean;
    ErrorCode:        Integer;
    ElapsedTime:      LongWord;   //  [ms]
    TotalRemainTime:  Double;     //  [s]
    PhaseRemainTime:  Double;     //  [s]
    TestPhase:        TTestPhase;

    SensingEnabled:   Boolean;

    LimitUsenseMin:   Double; // Word;
    LimitUsenseMax:   Double; // Word;
    LimitUmin:        Double; // LongWord;
    LimitUmax:        Double; // LongWord;
    LimitImin:        Double; // LongWord;
    LimitImax:        Double; // LongWord;
    LimitRmin:        Double;
    RangeRmax:        Double;

    ReadHG40Ready:    Boolean;
    ReadSPO:          Boolean;
    ReadIrange:       TIRange;
    ReadUsense:       Word;
    ReadU:            LongWord;
    ReadI:            LongWord;
    ReadU_BK:         LongWord;
    ReadI_BK:         LongWord;

    ConvUsense:       Double;
    ConvU:            Double;
    ConvI:            Double;
    ConvR:            Double;
    ConvI_BK:         Double;
    ConvU_BK:         Double;
    FlagOL:           Boolean;

    AvgConvU:         Double;
    AvgConvI:         Double;
    AvgConvR:         Double;
    AvgFlagOL:        Boolean;
    AvgConvUCache:    array [0 .. cAvgResWinSizeMax - 1] of Double;
    AvgConvICache:    array [0 .. cAvgResWinSizeMax - 1] of Double;
    AvgConvRCacheSize:Integer;
    AvgConvRIndex:    Integer;

    LastUsense:       Double;
    LastU:            Double;
    LastI:            Double;
    LastR:            Double;
    LastOL:           Boolean;
                      
{$ifdef MIN_MAX}
    YetNoMinMax:      Boolean;
    MinReadIrange:    TIRange;
    MinReadUsense:    Word;
    MinReadU:         LongWord;
    MinReadI:         LongWord;
    MinConvR:         Double;
    MaxReadIrange:    TIRange;
    MaxReadUsense:    Word;
    MaxReadU:         LongWord;
    MaxReadI:         LongWord;
    MaxConvR:         Double;
{$endif}
  end;


  
{ Usual data types required on the interface. }
type
  TRetVal = LongInt;

  
  TInstalledOption = ( optUSense
                     );
  TInstalledOptions = set of TInstalledOption;


  TCalibrationParam = ( BKUNomFact   = 0
                      , BKUNomOff        // set voltage
                      , BKURealFact
                      , BKURealOff       // read analog voltage
                      , BKIRealFact
                      , BKIRealOff       // analog current 10mA
                      , HG40I1uAFact
                      , HG40I1uAOff
                      , HG40I100uAFact
                      , HG40I100uAOff
                      , HG40I10mAFact
                      , HG40I10mAOff
                      , HG40UFact
                      , HG40UOff
                      , UsenseFact
                      , UsenseOff
                      );
  TCalibrationParams = set of TCalibrationParam;

{$ifdef SERVICE}
  TUpdateMeas = procedure (aTotRemTime, RemainTime, aUgen, aUreal, aI_Rreal: Double; aGlobStat: Byte; aErrCode: Integer; aOL, aFinished: Boolean) of object;
  TUpdateAll  = procedure (aRec: Pointer) of object;
{$endif}


  T_IS1885L = class (TObject)
    protected
      fThread:      TThread;
      fCSInputImg:  TCriticalSection;
      fCSTestStat:  TCriticalSection;
      fCommTout:    Word;

      fIO40ID:      Word;
      fCouplerID:   AnsiString;
      f24V:         Double;
      fInstOpts:    TInstalledOptions;

      fInitOk:      Boolean;

{$ifdef SERVICE}
      fOnUpdate:    TUpdateMeas;
      fOnUpdateAll: TUpdateAll;
{$endif}
      fAlreadyRead: Boolean;
      fTotRemTime:  Double;
      fRemainTime:  Double;
      fUgen:        Double;
      fUreal:       Double;
      fIreal:       Double;
      fRreal:       Double;
      fGlobStat:    Byte;
      fErrCode:     Integer;
      fOL:          Boolean;
      fFinished:    Boolean;

      fCalibrationData: array [0 .. 63] of Double;

    public
      constructor create;
      destructor destroy; override;

      function connect (const aIP: LongWord; const aPortNr: Word = cModbusTcpPortNrDefault): TRetVal; overload;
      function disconnect: TRetVal;

      { Reading of single register using modbus function 4. }
      function read_raw (aAddr: Word; var aReadValue: Word): TRetVal;
      { Reading of multiple registers using modbus funtion 4. }
      function readMulti_raw (aAddr: Word; aCount: Byte; var aArray: array of Word): TRetVal;
      { Reading of single register from internal input image cache. }
      function read_cache_raw (aAddr: Word; var aReadValue: Word; aForced: Boolean = false): TRetVal;
      { Reading of analog input from input image cache - returns 16b unsigned integer. }
      function readAI_raw (aAINr: Word; var aReadValue: Word; aForced: Boolean = false): TRetVal;
      { Reading of analog input from input image cache - returns double of given range (default: 0-1). }
      function readAI(aAINr: Word; var aReadValue: Double; aRange: Double = 1.0; aForced: Boolean = false): TRetVal;
      { Reading of digital input from input image cache. }
      function readDI (aDINr: Word; var aReadValue: Boolean; aForced: Boolean = false): TRetVal;
      { Reading of status of single digital output using modbus function 1. }
      function readDO (aDONr: Word; var aReadValue: Boolean): TRetVal;
      { Writing of single register using modbus function 6. }
      function write_raw (aAddr: Word; aWriteValue: Word): TRetVal;
      { Writing of multiple registers using modbus function 16. }
      function writeMulti_raw (aAddr: Word; aNumberOfValues: Byte; const aWriteValues: array of Word): TRetVal;
      { Writing only value zero into multiple registers using modbus function 16. }
      function writeMulti_zero (aAddr: Word; aNumberOfValues: Byte): TRetVal;
      { Writing of analog output in form of 16b unsigned integer - internally calls write_raw. }
      function writeAO_raw (aAONr: Word; aWriteValue: Word): TRetVal;
      { Writing of analog output in form of double with given range (default: 0-1) - internally calls write_raw. }
      function writeAO (aAONr: Word; aWriteValue: Double; aRange: Double = 1.0): TRetVal;
      { Writing of single digital output using modbus function 5. }
      function writeDO (aDONr: Word; aWriteValue: Boolean): TRetVal;
      { Writing of first 16 digital outputs (all supported by IO40) using modbus function 15. }
      function writeDOs (aWriteValues: Word = 0): TRetVal;  // First/all 16 values at once

      { Bekchoff register communication technique }
      function regCommRead (aStatByteAddr: Word; aRegNr: Byte; var aReadValue: Word): TRetVal;
      function regCommWrite (aStatByteAddr: Word; aRegNr: Byte; aWriteValue: Word): TRetVal;

      function devInit (aWatchdogTime: Word = 0): TRetVal;
      function devTestStop: TRetVal;
      function devTestStart_I6: TRetVal;
      function devTestPrep_I6 (aTestTime, aRampTime, aUstart, aUnom, aRmin, aDetectDelay: Double; aRampDown, aInverted, a4wire: Boolean;
          aAvgResWinSize: Integer = cAvgResWinSizeMax; aAvgResDelay: Integer = cAvgResDelayMax): TRetVal;
      function devTestGet_I6 (var aTotalRemTime, aRemTime, aUgen, aUreal, aRreal: Double; var aErrCode: Integer; var aGlobStat: Byte; var aOL, aFinished: Boolean): TRetVal;
      function devTestStart_H6: TRetVal;
      function devTestPrep_H6 (aTestTime, aRampTime, aUstart, aUnom, aImin, aImax: Double; aRampDown, aInverted, a4wire: Boolean): TRetVal;
      function devTestGet_H6 (var aTotalRemTime, aRemTime, aUgen, aUreal, aIreal: Double; var aErrCode: Integer; var aGlobStat: Byte;var aFinished: Boolean): TRetVal;

      { Functions for calibration}
      procedure clearAllCalibData;
      function  readAllCalibData (aContinueOnError: Boolean = true): TRetVal;
      procedure passAllCalibData; 
      function  getCalibValue (ValIndex: TCalibrationParam): Double;
      procedure setCalibValue (ValIndex: TCalibrationParam; Value: Double);
      function  writeCalibValue (ValIndex: TCalibrationParam; Value: Double): TRetVal;
      function  readCalibValue (ValIndex: TCalibrationParam; var Value: Single): TRetVal;

    public
      class function showRetVal (const aCode: TRetVal; const aDescription: Boolean = false): AnsiString;
      class function calcRrange (const aUnom: Double): Double;
      class function showTestPhase (const aTestPhase: TTestPhase): AnsiString;
      class function convIRange (const aIRange: TIRange): Double;
      class function showIRange (const aIRange: TIRange): AnsiString;

    public
      property initOk: Boolean read fInitOk;
      property installedOptions: TInstalledOptions read fInstOpts;
{$ifdef SERVICE}
      property onUpdate: TUpdateMeas write fOnUpdate { write only ! };
      property onUpdateAll: TUpdateAll write fOnUpdateAll { write only ! };
{$endif}

    protected
      function checkPrologue (aConnected: Boolean = true): TRetVal;
      procedure hndlUpdate_I6 (aThread: TThread);
      procedure hndlUpdate_H6 (aThread: TThread);

    protected
      class function AINr2Addr (aAINr: Word): Word;
      class function AONr2Addr (aAONr: Word): Word;
  end;


var
  is1885lObj: T_IS1885L;


IMPLEMENTATION

uses
{$ifndef SERVICE}
  CommLibH,
{$endif}
  MMSystem, Windows, Forms, SysUtils, SynaIP, Math;


const
  cNoAddr: Word = 0;
  
  cInTestUpdateInterval           = 90;

  cDetectDelayMinimum_ms          = 150;  // [ms]
  cDetectDelayMaximum_ms          = 999900;

  cVoltageCheckStart_ms           = 200;  // [ms]

  { *WriteMult: factor for conversion from analog value (Double) to digital value (Word or LongWord) }
  cUrangeWriteMult: Double        = 1.6666666667e-4;
  cIrangeWriteMult_10mA: Double   = 100.0;
  cIrangeWriteMult_100uA: Double  = 10000.0;
  cIrangeWriteMult_1uA: Double    = 1000000.0;
  cScale16bWriteMult: Double      = 32767.0;    // 0x7FFF   (15b)
  cScale24bWriteMult: Double      = 8388607.0;  // 0x7FFFFF (23b)
  { *ReadMult: factor for conversion from digital value (Word or LongWord) to analog value (Double) }
  cUrangeReadMult: Double         = 6000.0;     // 0-6 kV
  cIrangeReadMult_10mA: Double    = 1e-2;       // 0-10 mA 
  cIrangeReadMult_100uA: Double   = 1e-4;       // 0-0.1 mA (0-100 uA)
  cIrangeReadMult_1uA: Double     = 1e-6;       // 0-1 uA
  cScale16bReadMult: Double       = 3.0518509476e-5;
  cScale24bReadMult: Double       = 1.1920930376e-7;

{$ifdef SERVICE}
  { Copy of error codes from unit CommLibH.pas. }
  I6_LOWVOLTAGE                 = -2410;   //< I6 test - low voltage error
  I6_HIGHVOLTAGE                = -2411;   //< I6 test - high voltage error
  I6_LOWRESISTANCE              = -2412;   //< I6 test - low resistance error
  I6_HIGHRESISTANCE             = -2413;   //< I6 test - high resistance error
  I6_SENSE_VOLTAGE              = -2417;   //< I6 test - sense voltage error

  H6_LOWVOLTAGE                 = -2420;   //< H6 test - low voltage error
  H6_HIGHVOLTAGE                = -2421;   //< H6 test - high voltage error
  H6_LOWCURRENT                 = -2422;   //< H6 test - low current error
  H6_HIGHCURRENT                = -2423;   //< H6 test - high current error
  H6_SENSE_VOLTAGE              = -2427;   //< H6 test - sense voltage error

  { Copy of used status byte values from unit CommLibH.pas. }
  STA_TEST_IDLE                 = $00;
  STA_TEST_PREPARE              = $20;
  STA_TEST_RAMP_UP              = $30;
  STA_TEST_RAMP_DOWN            = $50;
  STA_TEST_MEASURING            = $60;
  STA_TEST_FINISHED             = $80;
{$endif} 

  cTestPhase2GlobStat: array [TTestPhase] of Byte =
    ( { tpIdle      } STA_TEST_IDLE
    , { tpWaitVolt  } STA_TEST_PREPARE
    , { tpRampUp    } STA_TEST_RAMP_UP
    , { tpTest      } STA_TEST_MEASURING
    , { tpRampDown  } STA_TEST_RAMP_DOWN
    , { tpFinishing } STA_TEST_FINISHED
    );

  cCalParOffsets: TCalibrationParams = [ BKUNomOff, BKURealOff, BKIRealOff, HG40I1uAOff, HG40I100uAOff
                                       , HG40I10mAOff, HG40UOff, UsenseOff ];
                                       
  cCalParFactors: TCalibrationParams = [ BKUNomFact, BKURealFact, BKIRealFact, HG40I1uAFact, HG40I100uAFact
                                       , HG40I10mAFact, HG40UFact, UsenseFact ];

                                                                                 
type                                                                             
  TAction = ( actForceReadInputImage                                             
            , actWriteSingleDO                                                   
            , actWriteMultiDOs                                                   
            , actReadSingleDO                                                    
            , actReadSingleReg
            , actReadMultiRegs                                                   
            , actWriteSingleReg
            , actWriteMultiRegs                                                  
            , actRegCommWrite
            , actRegCommRead                                                     
            , actNewConnect
            , actDisconnect                                                      
            , actTestStop
            , actTestPrep
            , actTestStart
            );


  TReqOrCurr = ( rocRequested
               , rocCurrent
               );


  TTestKind = ( tkNoTest
              , tkI6_InsulationTest
              , tkH6_HighVoltageTest
              );


  TReqRespRec = record
    fReq:         Boolean;
    fReady:       Boolean;

    fVarAddr:     Word;
    fVarRegNr:    Byte;
    fVarParam:    Word;

    fRetVal:      TRetVal;
    fRetWord:     Word;
  end;


  TCommunicationParameters = record
    IPAddress:  LongWord;
    PortNr:     Word;
    // BaseReg:    Integer;
    // UnitId:     Byte;
  end;


  TTestParams_I6 = record
    TestTime:       Double;
    RampTime:       Double;
    Ustart:         Double;
    Unom:           Double;
    Rmin:           Double;
    Rrange:         Double;
    DetectDelay:    Double;
    RampDown:       Boolean;
    Inverted:       Boolean;
    Usense:         Boolean;
    AvgResWinSize:  Integer;
    AvgResDelay:    Integer;
  end;           
                 

  TTestParams_H6 = record
    TestTime:     Double;
    RampTime:     Double;
    Ustart:       Double;
    Unom:         Double;
    Imin:         Double;
    Imax:         Double;   
    RampDown:     Boolean;
    Inverted:     Boolean;
    Usense:       Boolean;
  end;


  TCommErrKind = ( cekNoError
                 , cekResponseError
                 , cekResponseMismatch
                 );
                 
  TCommErrRec = record
    kind:     TCommErrKind;
    funcCode: Byte;
    errCode:  Byte;
    respBuf:  TModBusResponseBuffer;
  end;


  TThreadStatusUpdateEvent = procedure (aThread: TThread) of object;


  TCalibBasicItem = ( cbiNoCalib
                    , BKUNom
                    , BKUReal
                    , BKIReal
                    , HG40I1uA
                    , HG40I100uA
                    , HG40I10mA
                    , HG40U
                    , Usense
                    );



  TCommThreadIS1885L = class (TThread)
    private
      fCSInputImg:        TCriticalSection;
      fCSTestStat:        TCriticalSection;

      fRequests:          array [TAction] of TReqRespRec;
      fReadyToRelease:    Boolean;
      fModbusClient:      TIdModBusClient;

      fTestRun:           TTestKind;
      fTimeLastRead:      LongWord;
      fTimeTestStart:     LongWord;
      fTimeWaitVoltStart: LongWord;
      fTimeRampUpEnd:     LongWord;
      fTimeRampDnStart:   LongWord;
      fTimeTestEnd:       LongWord;
      fTimeDetectStart:   LongWord;
      fTimeUminCheck:     LongWord;
      fTestStatus:        array [TTestKind] of TTestStatus;

      fConnected:         Boolean;

      fCommParams:        array [TReqOrCurr] of TCommunicationParameters;
      fI6Params:          array [TReqOrCurr] of TTestParams_I6;
      fH6Params:          array [TReqOrCurr] of TTestParams_H6;
      fTestKind:          array [TReqOrCurr] of TTestKind;

      fUpdateInterval:    LongWord;
      fUpdateNext:        LongWord;
      fUpdateGapStart:    LongWord;
      fInputImage:        array [AddrRng_InImg_Min .. AddrRng_InImg_Max] of Word;
      fTempInputImage:    array [AddrRng_InImg_Min .. AddrRng_InImg_Max] of Word;

      fDigOutImage:       array [DigOut_min .. DigOut_max] of Boolean;

      fTempRegLength:     Byte;
      fTempReg:           array [TReqOrCurr] of array [cTempRegRangeMin .. cTempRegRangeMax] of Word;

      fArrayPointer:      PWordArray;

      fLastCommErr:       TCommErrRec;
      fCommErrCounter:    array [TCommErrKind] of LongWord;

      fOnUpdate:          array [TTestKind] of TThreadStatusUpdateEvent;

      fCalibOffset:       array [TCalibBasicItem] of Double;
      fCalibFactor:       array [TCalibBasicItem] of Double;

    public
      constructor create (const aCSInputImg, aCSTestStat: TCriticalSection; const aOnUpdI6, aOnUpdH6: TThreadStatusUpdateEvent; const aUpdateInterval: Word);
      destructor destroy; override;

      procedure execute; override;

    private
      procedure doAction (const aAction: TAction);
      procedure doProcessReading (const aTestKind: TTestKind);
      procedure doUpdateStatus (const aTestKind: TTestKind);

      procedure clearStatusRec (var aRec: TTestStatus);
      procedure hndlConnect (aSender: TObject);
      procedure hndlDisconnect (aSender: TObject);
      procedure hndlRespErr (const aFunctionCode, aErrorCode: Byte; const aResponseBuffer: TModBusResponseBuffer);
      procedure hndlRespMismatch (const aFunctionCode, aErrorCode: Byte; const aResponseBuffer: TModBusResponseBuffer);

      function writeCoil (const aRegNo: Word; const aValue: Boolean): TRetVal;
      function writeCoils (const aRegNo, aBlocks: Word; const aRegisterData: array of Boolean): TRetVal;
      function readCoil (const aRegNo: Word; var aValue: Boolean): TRetVal;
      function writeRegister (const aRegNo: Word; const aValue: Word): TRetVal;
      function writeRegisters (const aRegNo: Word; const aBlocks: Word; const aRegisterData: array of Word): TRetVal;
      function readRegister (const aRegNo: Word; var aValue: Word): TRetVal;
      function readRegisters (const aRegNo, aBlocks: Word; var aRegisterData: array of Word): TRetVal;

      function regCommRead (const aStatOrCtrlByteAddr: Word; const aRegNr: Byte; var aReadValue: Word): TRetVal;
      function regCommWrite (const aStatOrCtrlByteAddr: Word; const aRegNr: Byte; const aWriteValue: Word): TRetVal;

      function rampProg (const aAddr: Word; const aUstart, aRampTime: Double): TRetVal;

    public
      function request (const aAction: TAction; const aAddr: Word = 0; const aParam: Word = 0; const aRegNr: Byte = 0): TRetVal;
      procedure passCommParams (const aIP: LongWord; const aPortNr: Word);
      procedure passWriteMultiRegsZero (aNumberOfRegs: Byte);
      procedure passWriteMultiRegs (aNumberOfRegs: Byte; const aArray: array of Word);
      procedure passI6Params (const aSrc: TTestParams_I6);
      procedure passH6Params (const aSrc: TTestParams_H6);
      procedure fetchMultiRegs (const aCount: Byte; var aArray: array of Word);
      procedure softReset;
      function waitForResponse (const aAction: TAction; out aRetVal: TRetVal; out aRetWord: Word; const aTimeout: Word = cActionTimeoutDefault): TRetVal; overload;
//      function waitForResponse (const aAction: TAction; const aTimeout: Word = cActionTimeoutDefault): TRetVal; overload;
      function ack (const aAction: TAction): TRetVal;
      function safeInputImageRead (const aAddr: Word; out aReadValue: Word): TRetVal;

    public
      property readyToRelease: Boolean read fReadyToRelease;
      property isConnected: Boolean read fConnected;
  end;


{ Helper function to reduce code redundancy (cannot be method due to visibility control). }
{ Parameter aData is used as both input and output parameter. }
function basicThreadAction (aThread: TThread; aAction: TAction; aTout: Word; aAddr: Word; var aData: Word; aRegNr: Byte = 0): TRetVal;
var
  rv: TRetVal;
  w:  Word;

begin        
  result := ErrorGeneral;
  if ( (aThread = nil) or (not (aThread is TCommThreadIS1885L)) ) then
  begin
    exit;
  end;

  try
    { REQUEST }
    result := TCommThreadIS1885L(aThread).request(aAction, aAddr, aData, aRegNr);
    if (result <> OK) then exit;

    { RESPONSE }
    result := TCommThreadIS1885L(aThread).waitForResponse(aAction, rv, w, aTout);
    if (result <> OK) then exit;

    { ACK }
    result := TCommThreadIS1885L(aThread).ack(aAction);
    if (result <> OK) then exit;

  finally
    if (result <> OK) then
    begin { NAK }
      TCommThreadIS1885L(aThread).ack(aAction);
    end
    else
    begin { thread procedure successful }
      aData := w;
      result := rv;
    end;
  end;
end;


function checkAddressRange (aAddr, aMinAddr, aMaxAddr: Word): Boolean;
var
  tmp:  Word;

begin
  if (aMinAddr > aMaxAddr) then
  begin { optional swap of parameters }
    tmp := aMinAddr;
    aMinAddr := aMaxAddr;
    aMaxAddr := tmp;
  end;

  result := ( (aAddr >= aMinAddr) or (aAddr <= aMaxAddr) );
end;



{ T_IS1885L }

constructor T_IS1885L.create;
var
  t:  Word;

begin
//{$warn SYMBOL_PLATFORM off}
//  if (DebugHook <> 0) then
//{$warn SYMBOL_PLATFORM default}
//  begin
//    t := cUpdateIntervalDebug;
//    self.fCommTout := cActionTimeoutDebug;
//  end
//  else
  begin
    t := cUpdateIntervalDefault;
    self.fCommTout := cActionTimeoutDefault;
  end;

  { Internal variable and  }
  self.fCSInputImg := TCriticalSection.Create;
  self.fCSTestStat := TCriticalSection.Create;
  self.fThread := TCommThreadIS1885L.create(self.fCSInputImg, self.fCSTestStat, self.hndlUpdate_I6, self.hndlUpdate_H6, t);

  { Last initialization status holding attributes. }
  self.fIO40ID := 0;
  self.fCouplerID := '';
  self.f24V := 0.0;
  self.fInstOpts := [];
  self.fInitOk := false;

{$ifdef SERVICE}
  self.fOnUpdate := nil;
  self.fOnUpdateAll := nil;
{$endif}
  
  { Intermediate value used by hndlUpdate_XX() callback method and devTestGet_XX() method for passing of measured values. }
  self.fAlreadyRead := false;
  self.fRemainTime := 0.0;
  self.fTotRemTime := 0.0;
  self.fUgen := 0.0;
  self.fUreal := 0.0;
  self.fIreal := 0.0;
  self.fRreal := 0.0;
  self.fGlobStat := STA_TEST_IDLE;
  self.fErrCode := 0;
  self.fOL := false;
  self.fFinished := false;
end;


destructor T_IS1885L.destroy;
begin
  self.fThread.Terminate;
  while (not TCommThreadIS1885L(self.fThread).readyToRelease) do
  begin
    Sleep(1);
  end;
  self.fThread.Free;
  self.fCSInputImg.Free;
  self.fCSTestStat.Free;

  inherited;
end;


function T_IS1885L.connect(const aIP: LongWord; const aPortNr: Word): TRetVal;
const
  cAction: TAction = actNewConnect;

var
  dummy:  Word;

begin
  result := self.checkPrologue(false { no connection test });
  if (result <> OK) then exit;

  TCommThreadIS1885L(self.fThread).passCommParams(aIP, aPortNr);

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, dummy);

  self.fInitOk := false;

  if (result = OK) then // if connection is OK load calibration Data
  begin
    self.clearAllCalibData;
    self.readAllCalibData(true);
    self.passAllCalibData;
  end;
end;


function T_IS1885L.disconnect: TRetVal;
const
  cAction: TAction = actDisconnect;

var
  dummy:  Word;

begin
  result := self.checkPrologue(false { no connection test });
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, dummy);

  self.fInitOk := false;
end;


function T_IS1885L.read_raw(aAddr: Word; var aReadValue: Word): TRetVal;
const
  cAction: TAction = actReadSingleReg;

var
  w:  Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aAddr, w);
  if (result <> OK) then exit;

  aReadValue := w;
end;


function T_IS1885L.readMulti_raw (aAddr: Word; aCount: Byte; var aArray: array of Word): TRetVal;
const
  cAction: TAction = actReadMultiRegs;

var
  w:  Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  if (not Assigned(@aArray)) then
  begin
    result := ErrorNoArrayReference;
    exit;
  end;

  if (aCount > Length(aArray)) then
  begin
    result := ErrorShortArray;
    exit;
  end;

  w := aCount;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aAddr, w);
  if (result <> OK) then exit;

  TCommThreadIS1885L(self.fThread).fetchMultiRegs(aCount, aArray);
end;


function T_IS1885L.read_cache_raw (aAddr: Word; var aReadValue: Word; aForced: Boolean = false): TRetVal;
const
  cAction: TAction = actForceReadInputImage;

var
  rv:   TRetVal;
  w:    Word;

begin
  if (not checkAddressRange(aAddr, AddrRng_InImg_Min, AddrRng_InImg_Max)) then  // no offset into input image
  begin
    result := ErrorInvalidAddress;
    exit;
  end;

  result := self.checkPrologue;
  if (result <> OK) then exit;

  if (aForced) then
  begin  
    try
      result := TCommThreadIS1885L(self.fThread).request(cAction, aAddr);
      if (result = OK) then
      begin
        result := TCommThreadIS1885L(self.fThread).waitForResponse(cAction, rv, w, self.fCommTout);
        if (result <> OK) then exit;
        result := TCommThreadIS1885L(self.fThread).ack(cAction);
        if (result <> OK) then exit;
        result := rv;
        if (result <> OK) then exit;
      end
      else if (result <> ErrorBusy) then
      begin
        exit;
      end;
    finally
      if ( (result <> OK) and (result <> ErrorBusy) ) then
      begin
        TCommThreadIS1885L(self.fThread).ack(cAction);
      end;
    end;
  end;

  result := TCommThreadIS1885L(self.fThread).safeInputImageRead(aAddr, w);
  if (result <> OK) then exit;

  aReadValue := w;
end;


function T_IS1885L.readAI_raw(aAINr: Word; var aReadValue: Word; aForced: Boolean): TRetVal;
var
  adr:  Word;
  w:    Word;

begin         
  if (not checkAddressRange(aAINr, AlogIn_min, AlogIn_max)) then
  begin
    result := ErrorInvalidAINumber;
    exit;
  end;             

  adr := self.AINr2Addr(aAINr);

  result := self.read_cache_raw(adr, w, aForced);
  if (result <> OK) then exit;

  aReadValue := w;
end; 


function T_IS1885L.readAI(aAINr: Word; var aReadValue: Double; aRange: Double; aForced: Boolean): TRetVal;
var
  adr:  Word; 
  w:    Word;

begin
  if (not checkAddressRange(aAINr, AlogIn_min, AlogIn_max)) then
  begin
    result := ErrorInvalidAINumber;
    exit;
  end;

  adr := self.AINr2Addr(aAINr);

  result := self.read_cache_raw(adr, w, aForced);
  if (result <> OK) then exit;

  aReadValue := aRange * w * cScale16bReadMult;
end;


function T_IS1885L.readDI(aDINr: Word; var aReadValue: Boolean; aForced: Boolean): TRetVal;
var
  w:    Word;

begin   
  if (not checkAddressRange(aDINr, DigIn_min, DigIn_max)) then
  begin
    result := ErrorInvalidDINumber;
    exit;
  end;

  result := self.read_cache_raw(AddrRng_InImg_DI_Min, w, aForced);
  if (result <> OK) then exit;

  aReadValue := (((w shl aDINr) and 1) <> 0);
end;


function T_IS1885L.readDO(aDONr: Word; var aReadValue: Boolean): TRetVal;   
const
  cAction: TAction = actReadSingleDO;

var
  w:  Word;

begin
  if (not checkAddressRange(aDONr, DigOut_min, DigOut_max)) then
  begin
    result := ErrorInvalidDONumber;
    exit;
  end;

  result := self.checkPrologue;
  if (result <> OK) then exit;
            
          
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aDONr, w);
  if (result <> OK) then exit;

  aReadValue := (w <> 0);
end;


function T_IS1885L.write_raw(aAddr, aWriteValue: Word): TRetVal;
const
  cAction: TAction = actWriteSingleReg;

var
  dummy: Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aAddr, dummy);
end;      


function T_IS1885L.writeMulti_raw(aAddr: Word; aNumberOfValues: Byte; const aWriteValues: array of Word): TRetVal;
const
  cAction: TAction = actWriteMultiRegs;

var
  dummy: Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  if (Length(aWriteValues) < aNumberOfValues) then
  begin
    result := ErrorShortArray;
    exit;
  end;

  if (cTempRegRangeMax + 1 < aNumberOfValues) then
  begin
    result := ErrorTooManyValues;
    exit;
  end;

  TCommThreadIS1885L(self.fThread).passWriteMultiRegs(aNumberOfValues, aWriteValues);

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aAddr, dummy);
end; 


function T_IS1885L.writeMulti_zero(aAddr: Word; aNumberOfValues: Byte): TRetVal;
const
  cAction: TAction = actWriteMultiRegs;

var
  dummy: Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  TCommThreadIS1885L(self.fThread).passWriteMultiRegsZero(aNumberOfValues);

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aAddr, dummy);
end;


function T_IS1885L.writeAO_raw(aAONr, aWriteValue: Word): TRetVal;
var
  adr:  Word;

begin
  if (not checkAddressRange(aAONr, AlogOut_min, AlogOut_min)) then
  begin
    result := ErrorInvalidAONumber;
    exit;
  end;

  adr := self.AONr2Addr(aAONr);

  result := self.write_raw(adr, aWriteValue);
end;


function T_IS1885L.writeAO(aAONr: Word; aWriteValue, aRange: Double): TRetVal;
var
  x:  Double;
  w:  Word;
  
begin
  if (aRange <= 0.0) then
  begin
    result := ErrorInvalidRange;
    exit;
  end;

  if ( (aWriteValue < 0.0) or (aWriteValue > aRange) ) then
  begin
    result := ErrorValueOutOfRange;
    exit;
  end;

  x := 1 / aRange;
  x := aWriteValue * x * cScale16bWriteMult;
  if (x > cScale16bWriteMult) then x := cScale16bWriteMult;
  w := Trunc(x);

  result := self.writeAO_raw(aAONr, w);
end;


function T_IS1885L.writeDO(aDONr: Word; aWriteValue: Boolean): TRetVal;
const
  cAction: TAction = actWriteSingleDO;

var
  w:  Word;
  
begin
  if (not checkAddressRange(aDONr, DigOut_min, DigOut_max)) then
  begin
    result := ErrorInvalidDONumber;
    exit;
  end;

  w := Abs(Ord(aWriteValue));
    
  result := self.checkPrologue;
  if (result <> OK) then exit;  

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aDONr, w);
end;


function T_IS1885L.writeDOs(aWriteValues: Word): TRetVal;    
const
  cAction: TAction = actWriteMultiDOs;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, aWriteValues);
end; 


function T_IS1885L.regCommRead(aStatByteAddr: Word; aRegNr: Byte; var aReadValue: Word): TRetVal;
const
  cAction: TAction = actRegCommRead;

var
  w:  Word;
  
begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aStatByteAddr, w, aRegNr);
  if (result <> OK) then exit;

  aReadValue := w;
end;


function T_IS1885L.regCommWrite(aStatByteAddr: Word; aRegNr: Byte; aWriteValue: Word): TRetVal;
const
  cAction: TAction = actRegCommWrite;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, aStatByteAddr, aWriteValue, aRegNr);
end;


function T_IS1885L.devInit (aWatchdogTime: Word): TRetVal;
var
  i:  Integer;
  ii: Integer;   
{$ifndef STANDALONE_IO40}
  t:  LongWord;
{$endif}
  b:  Boolean;
  x:  Double;
  a:  array [0 .. 6] of Word;
  w:  Word;

begin
  self.fIO40ID := 0;
  self.fCouplerID := '';
  self.f24V := 0.0;
  self.fInstOpts := [];
  self.fInitOk := false;

  TCommThreadIS1885L(self.fThread).softReset;

  result := self.checkPrologue;
  if (result <> OK) then exit;

  { Wait 10 ms (after connection) }
  sleep(10);

  { Read out ID of IO40 (will be set external, not with DAT) }
  result := self.readMulti_raw(AddrSpec_CouplerID_first, Length(a), a);
  if (result <> OK) then exit;

  SetLength(self.fCouplerID, 12);
  for i := 0 to 5 do
  begin
    ii := (i shl 1) + 1;

    self.fCouplerID[ii    ] := Chr(WordRec(a[i]).Lo);
    self.fCouplerID[ii + 1] := Chr(WordRec(a[i]).Hi);
  end;
  self.fIO40ID := a[6];

  if ( (self.fCouplerID <> 'BK90IO40V.00') or (self.fIO40ID <> cDefaultIdIO40IS1885L) ) then
  begin { Checking both available IDs. }
    result := ErrorInvalidID;
    exit;
  end;         

  { Set desired watchdog time (0 to deactivate) }
  result := self.write_raw($1121, $BECF);
  if (result <> OK) then exit;
  result := self.write_raw($1121, $AFFE);
  if (result <> OK) then exit;
  result := self.write_raw($1120, aWatchdogTime);
  if (result <> OK) then exit;
  result := self.write_raw($1122, $0001);    
  if (result <> OK) then exit;
  result := self.write_raw($1121, $BECF);
  if (result <> OK) then exit;
  result := self.write_raw($1121, $AFFE);
  if (result <> OK) then exit;

  result := self.writeMulti_zero(AddrRng_OutImg_Min, Addr_OutImg_AO2 - AddrRng_OutImg_Min + 1);
  if (result <> OK) then exit;  { resetting all AOs (DIs does not work this way) }
  result := self.writeDOs(0);   { resetting all DOs }
  if (result <> OK) then exit;

  { Set Digital Out ``Reset All'' for HG40 once [resetting all other DOs] }
  result := self.writeDOs(1 shl DigOut_ResetAll);
  if (result <> OK) then exit;
  sleep(50);  // reasonable time to see reaction on the board (instead of stated 1 ms)
  result := self.writeDO(DigOut_ResetAll, false);
  if (result <> OK) then exit;

  { Check HG40 Digital Input ``Ready'' (will be high as soon as the uC is ready) }
{$ifndef STANDALONE_IO40}
  t := timeGetTime + 5000;  // 5 second timeout
  repeat
    result := self.read_raw(AddrRng_InImg_DI_Min, w);
    if (result <> OK) then exit;

    b := (((1 shl DigIn_HG40_Ready) and w) <> 1);
    if (b) then break;
  until (timeGetTime >= t);
  if (not b) then
  begin
    result := ErrorTimedOut;
    exit;
  end;
{$else}
  result := self.read_raw(AddrRng_InImg_DI_Min, w);
  if (result <> OK) then exit;
{$endif}

  { Fill installed options (from last digital intput image). }    
  b := (((1 shl DigIn_Option_USense) and w) <> 0);
  if (b) then self.fInstOpts := self.fInstOpts + [optUSense];

  { Check Analog Input f(24V) }
  result := self.readAI(AlogIn_24V, x, cPowerSupplyRange);
  if (result = OK) then
  begin
{$ifndef STANDALONE_IO40}
    if (x < cPowerSupplyVoltageMin) then result := ErrorPowerSupplyLowVolt
//    else if (x > cPowerSupplyVoltageMax) then result := ErrorPowerSupplyHighVolt;
{$endif}
  end;
  if (result <> OK) then exit;
  self.f24V := x;

  { Activate K Discharge }
  result := self.writeDO(DigOut_K_Discharge, false);
  if (result <> OK) then exit;

  { Pass calibration data from interface object to internal thread. }
  self.passAllCalibData;

  self.fInitOk := (result = OK);
end;


function T_IS1885L.devTestStop: TRetVal;
const
  cAction: TAction = actTestStop;

var
  dummy:  Word;
  
begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  dummy := 0;
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, dummy);
end;


function T_IS1885L.devTestStart_I6: TRetVal;
const
  cAction: TAction = actTestStart;

var
  w:  Word;
  
begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  w := Ord(tkI6_InsulationTest);
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, w);
end;


function T_IS1885L.devTestPrep_I6(aTestTime, aRampTime, aUstart, aUnom, aRmin, aDetectDelay: Double;
    aRampDown, aInverted, a4wire: Boolean;
    aAvgResWinSize, aAvgResDelay: Integer): TRetVal;
const
  cAction: TAction = actTestPrep;

var
  w:    Word;
  p:    TTestParams_I6;
  rng:  Double;
{$ifdef EXTERNAL_PROCEDURES}
  b:    Boolean;
{$ifdef RAMP_UP}
  x:    Double;
  CB:   Word;
//  lw:   Word;
{$endif}
{$endif}

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := ErrorInvalidTestParam;
  if (aTestTime < cParTestTimeLimitMin) then exit;
  if (aTestTime > cParTestTimeLimitMax) then exit;
  if (aRampTime < cParRampTimeLimitMin) then exit;
  if (aRampTime > cParRampTimeLimitMax) then exit;
  if (aUstart < cParUstartLimitMin) then exit;
  if (aUstart > cParUstartLimitMax) then exit;
  if (aUnom < cParUnomLimitMin) then exit;
  if (aUnom > cParUnomLimitMax) then exit;
  if (aRmin < cParRminLimitMin) then exit;
  if (aDetectDelay < cParDetectDelayLimitMin) then exit;
  if (aDetectDelay > cParDetectDelayLimitMax) then exit;
  if (aAvgResWinSize < cAvgResWinSizeMin) then exit;
  if (aAvgResWinSize > cAvgResWinSizeMax) then exit;
  if (aAvgResDelay < cAvgResDelayMin) then exit;
  if (aAvgResDelay > cAvgResDelayMax) then exit;

  result := ErrorInvalidTestParamRng;
  rng := T_IS1885L.calcRrange(aUnom);
  if (aRmin > rng) then exit;

  if ( (a4wire) and (not (optUSense in self.fInstOpts)) ) then
  begin
    result := ErrorNoOptUsense;
    exit;
  end;

{$ifdef EXTERNAL_PROCEDURES}
  b := true;

  { Set Digital Out for K1 - normal test }
  b := b and (self.writeDO(DigOut_K1_HV_Normal, not aInverted) = OK);

  { Opt.: set Digital Out for K2, inverted test instead of K1 }
  b := b and (self.writeDO(DigOut_K2_HV_Inverted, aInverted) = OK);

  { Opt.: set additional Digital Output for K3 USense }
  b := b and (self.writeDO(DigOut_K3_HV_Sense, a4wire) = OK);

  { Opt.: set Ramp Function with CW B7 in R0 and R1 for Addresses 0x0800 and 0x0801 - R10= start value, R1 = time value }
{$ifdef RAMP_UP}
  { Opt.: set Ramp Function with CW B7 in R0 and R1 for Addresses 0x0800 and 0x0801 - R10= start value, R1 = time value }
  CB := Addr_OutImg_AO1 - 1;
  x := (aUstart - self.fCalibrationData[Ord(BKUNomOff)]) * self.fCalibrationData[Ord(BKUNomFact)];
  x := x * cScale16bWriteMult * cUrangeWriteMult;
  b := b and (self.regCommWrite(CB, 0, Trunc(x)) = OK);
  b := b and (self.regCommRead(CB, 0, w) = OK);
//  lw := w;  { TODO: ?! }
  x := aRampTime * 100.0; { 1x s => 10x ms }
  b := b and (self.regCommWrite(CB, 1, Trunc(x)) = OK);
  b := b and (self.regCommRead(CB, 1, w) = OK);
{$endif}

  { Set NO Current range - for IS test, no current range is selected, HG40 will run on automatic current range mode. }

  { Setting of AO and disabling of K Discharge DO is moved to the actTestStart procedure itself. }
  
  if (not b) then
  begin
    result := ErrorCommunication;
    exit;
  end;
{$endif}

  with p do
  begin
    TestTime      := aTestTime;
    RampTime      := aRampTime;
    Ustart        := aUstart;
    Unom          := aUnom;
    Rmin          := aRmin;
    Rrange        := rng;
    DetectDelay   := aDetectDelay;
    RampDown      := aRampDown;
    Inverted      := aInverted;
    Usense        := a4wire and (optUSense in self.fInstOpts);
    AvgResWinSize := aAvgResWinSize;
    AvgResDelay   := aAvgResDelay;
  end;
  TCommThreadIS1885L(self.fThread).passI6Params(p);

  w := Ord(tkI6_InsulationTest);
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, w);
end;  


function T_IS1885L.devTestGet_I6(var aTotalRemTime, aRemTime, aUgen, aUreal, aRreal: Double;
                                 var aErrCode: Integer; var aGlobStat: Byte;
                                 var aOL, aFinished: Boolean): TRetVal;
begin
  self.fCSTestStat.Enter; { lock the data to not update them during gathering operation }

  aTotalRemTime := self.fTotRemTime;
  aRemTime := self.fRemainTime;
  aUgen := self.fUgen;
  aUreal := self.fUreal;
  aRreal := self.fRreal;
  aGlobStat := self.fGlobStat;
  aErrCode := self.fErrCode;
  aOL := self.fOL;
  aFinished := self.fFinished;

  self.fCSTestStat.Leave;
  
  self.fAlreadyRead := true;

  result := OK;
end;


function T_IS1885L.devTestStart_H6: TRetVal;
const
  cAction: TAction = actTestStart;

var
  w:  Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  w := Ord(tkH6_HighVoltageTest);
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, w);
end;


function T_IS1885L.devTestPrep_H6(aTestTime, aRampTime, aUstart, aUnom, aImin, aImax: Double; aRampDown, aInverted, a4wire: Boolean): TRetVal;
const
  cAction: TAction = actTestPrep;

var
  w:  Word;
  p:  TTestParams_H6;    
{$ifdef EXTERNAL_PROCEDURES}
  b:  Boolean;  
{$ifdef RAMP_UP}
  x:  Double;
  CB: Word;  
//  lw: Word;
{$endif}
{$endif}

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  result := ErrorInvalidTestParam;
  if (aTestTime < cParTestTimeLimitMin) then exit;
  if (aTestTime > cParTestTimeLimitMax) then exit;
  if (aRampTime < cParRampTimeLimitMin) then exit;
  if (aRampTime > cParRampTimeLimitMax) then exit;
  if (aUstart < cParUstartLimitMin) then exit;
  if (aUstart > cParUstartLimitMax) then exit;
  if (aUnom < cParUnomLimitMin) then exit;
  if (aUnom > cParUnomLimitMax) then exit;
  if (aImin < cParIminLimitMin) then exit;
  if (aImin > cParIminLimitMax) then exit;
  if (aImax < cParImaxLimitMin) then exit;
  if (aImax > cParImaxLimitMax) then exit;

  if ( (a4wire) and (not (optUSense in self.fInstOpts)) ) then
  begin
    result := ErrorNoOptUsense;
    exit;
  end;

{$ifdef EXTERNAL_PROCEDURES}
  b := true;
  
  { Set Digital Out for K1 - normal test }
  b := b and (self.writeDO(DigOut_K1_HV_Normal, not aInverted) = OK);

  { Opt.: set Digital Out for K2, inverted test instead of K1 }
  b := b and (self.writeDO(DigOut_K2_HV_Inverted, aInverted) = OK);

  { Opt.: set additional Digital Output for K3 USense }
  b := b and (self.writeDO(DigOut_K3_HV_Sense, a4wire) = OK);

{$ifdef RAMP_UP}
  { Opt.: set Ramp Function with CW B7 in R0 and R1 for Addresses 0x0800 and 0x0801 - R10= start value, R1 = time value }
  CB := Addr_OutImg_AO1 - 1;
  x := (aUstart - self.fCalibrationData[Ord(BKUNomOff)]) * self.fCalibrationData[Ord(BKUNomFact)];
  x := x * cScale16bWriteMult * cUrangeWriteMult;
  b := b and (self.regCommWrite(CB, 0, Trunc(x)) = OK);
  b := b and (self.regCommRead(CB, 0, w) = OK);
//  lw := w;  { TODO: ?! }
  x := aRampTime * 100.0; { 1x s => 10x ms }
  b := b and (self.regCommWrite(CB, 1, Trunc(x)) = OK);
  b := b and (self.regCommRead(CB, 1, w) = OK);
{$endif}

  { Set Current range 10 mA }
  b := b and (self.writeDO(DigOut_10mA_Range, true) = OK);

  { Setting of AO and disabling of K Discharge DO is moved to the actTestStart procedure itself. }
  
  if (not b) then
  begin
    result := ErrorCommunication;
    exit;
  end;
{$endif}

  with p do
  begin
    TestTime := aTestTime;
    RampTime := aRampTime;
    Ustart   := aUstart;
    Unom     := aUnom;
    Imin     := aImin;
    Imax     := aImax;
    RampDown := aRampDown;
    Inverted := aInverted;
    Usense   := a4wire and (optUSense in self.fInstOpts);
  end;
  TCommThreadIS1885L(self.fThread).passH6Params(p);

  w := Ord(tkH6_HighVoltageTest);
  result := basicThreadAction(self.fThread, cAction, self.fCommTout, cNoAddr, w);
end;  


function T_IS1885L.devTestGet_H6(var aTotalRemTime, aRemTime, aUgen, aUreal, aIreal: Double;
                                 var aErrCode: Integer; var aGlobStat: Byte;
                                 var aFinished: Boolean): TRetVal;
begin
  self.fCSTestStat.Enter; { lock the data to not update them during gathering operation }

  aTotalRemTime := self.fTotRemTime;
  aRemTime := self.fRemainTime;
  aUgen := self.fUgen;
  aUreal := self.fUreal;
  aIreal := self.fIreal;
  aGlobStat := self.fGlobStat;
  aErrCode := self.fErrCode;
  aFinished := self.fFinished;

  self.fCSTestStat.Leave;
  
  self.fAlreadyRead := true;
  
  result := OK;
end;


class function T_IS1885L.showRetVal(const aCode: TRetVal; const aDescription: Boolean): AnsiString;
begin
  if (aDescription) then
  case aCode of
    OK:                       result := 'OK';
    ErrorGeneral:             result := 'General error!';
    ErrorDisconnected:        result := 'Client is not connected!';
    ErrorNotYetReady:         result := 'Operation result is not yet ready!';
    ErrorBusy:                result := 'Device is busy!';
    ErrorInvalidAction:       result := 'Requested invalid action!';
    ErrorInvalidRegNr:        result := 'Requested invalid register number (range of 6b unsigned value)!';
    ErrorNotRequested:        result := 'Action is not requested!';
    ErrorTimedOut:            result := 'Action timed out!';
    ErrorCommunication:       result := 'Communication error!';
    ErrorInvalidAddress:      result := 'Address is invalid!';
    ErrorInvalidDINumber:     result := 'Digital input number is invalid!';
    ErrorInvalidDONumber:     result := 'Digital output number is invalid!';
    ErrorInvalidAINumber:     result := 'Analog input number is invalid!';
    ErrorInvalidAONumber:     result := 'Analog output number is invalid!';
    ErrorValueOutOfRange:     result := 'Value is out of range!';
    ErrorShortArray:          result := 'Given array is too short (not enough elements)!';
    ErrorTooManyValues:       result := 'Exceeded limit of supported values in array!';
    ErrorNoArrayReference:    result := 'Missing reference for array!';
    ErrorPowerSupplyLowVolt:  result := 'Detected low voltage of power supply!';
    ErrorPowerSupplyHighVolt: result := 'Detected high voltage of power supply!';
    ErrorInvalidRange:        result := 'Given range is invalid (e.g. negative or zero)!';
    ErrorInvalidTest:         result := 'Detected invalid test!';
    ErrorInvalidID:           result := 'Wrong device ID!';
    ErrorHG40NotReady:        result := 'Generator HG40 is not ready for operation!';
    ErrorAlreadyInTest:       result := 'Test is already running!';
    ErrorInvalidTestParam:    result := 'Invalid test parameter!';
    ErrorNotYetPreparedTest:  result := 'Test it not yet prepared! One has to call devTestPrep_XX() before calling devTestStart_XX().';
    ErrorNoOptUsense:         result := 'The device does not have installed Usense module!';
    StatNoActionYet:          result := 'No action has been yet fired.';
    ErrorInvalidTestParamRng: result := 'Invalid test parameter (variable resistance range exceeded)!';
    else                      result := '??';
  end
  else
  case aCode of
    OK:                       result := 'OK';
    ErrorGeneral:             result := 'ErrorGeneral';
    ErrorDisconnected:        result := 'ErrorDisconnected';
    ErrorNotYetReady:         result := 'ErrorNotYetReady';
    ErrorBusy:                result := 'ErrorBusy';
    ErrorInvalidAction:       result := 'ErrorInvalidAction';
    ErrorInvalidRegNr:        result := 'ErrorInvalidRegNr';
    ErrorNotRequested:        result := 'ErrorNotRequested';
    ErrorTimedOut:            result := 'ErrorTimedOut';
    ErrorCommunication:       result := 'ErrorCommunication';
    ErrorInvalidAddress:      result := 'ErrorInvalidAddress';
    ErrorInvalidDINumber:     result := 'ErrorInvalidDINumber';
    ErrorInvalidDONumber:     result := 'ErrorInvalidDONumber';
    ErrorInvalidAINumber:     result := 'ErrorInvalidAINumber';
    ErrorInvalidAONumber:     result := 'ErrorInvalidAONumber';
    ErrorValueOutOfRange:     result := 'ErrorValueOutOfRange';
    ErrorShortArray:          result := 'ErrorShortArray';
    ErrorTooManyValues:       result := 'ErrorTooManyValues';
    ErrorNoArrayReference:    result := 'ErrorNoArrayReference';
    ErrorPowerSupplyLowVolt:  result := 'ErrorPowerSupplyLowVolt';
    ErrorPowerSupplyHighVolt: result := 'ErrorPowerSupplyHighVolt';
    ErrorInvalidRange:        result := 'ErrorInvalidRange';
    ErrorInvalidTest:         result := 'ErrorInvalidTest';
    ErrorInvalidID:           result := 'ErrorInvalidID';
    ErrorHG40NotReady:        result := 'ErrorHG40NotReady';
    ErrorAlreadyInTest:       result := 'ErrorAlreadyInTest';
    ErrorInvalidTestParam:    result := 'ErrorInvalidTestParam';
    ErrorNotYetPreparedTest:  result := 'ErrorNotYetPreparedTest';
    ErrorNoOptUsense:         result := 'ErrorNoOptUsense';
    ErrorInvalidTestParamRng: result := 'ErrorInvalidTestParamRng';
    StatNoActionYet:          result := 'StatNoActionYet';
    else                      result := '??';
  end;
end;


class function T_IS1885L.calcRrange(const aUnom: Double): Double;
begin
  result := aUnom * cIrangeWriteMult_1uA * 1000;
end;          


class function T_IS1885L.showTestPhase(const aTestPhase: TTestPhase): AnsiString;
begin
  case aTestPhase of
    tpIdle:       result := 'idle';
    tpWaitVolt:   result := 'wait for U';
    tpRampUp:     result := 'ramp up';
    tpTest:       result := 'test';
    tpRampDown:   result := 'ramp down';
    tpFinishing:  result := 'finishing';
    else          result := '?? (' + IntToStr(Ord(aTestPhase)) + ')';
  end;
end; 


class function T_IS1885L.convIRange(const aIRange: TIRange): Double;
begin
  case aIRange of
    ir1uA:    result := 1e-6;
    ir100uA:  result := 1e-4;
    ir10mA:   result := 1e-2;
    else      result := 0.0;
  end;
end;


class function T_IS1885L.showIRange(const aIRange: TIRange): AnsiString;
begin
  case aIRange of
    ir1uA:    result := '1 uA';
    ir100uA:  result := '100 uA';
    ir10mA:   result := '10 mA';
    else      result := '?? (' + IntToStr(Ord(aIRange)) + ')';
  end;
end;


function T_IS1885L.checkPrologue(aConnected: Boolean): TRetVal;
begin
  if (self.fThread = nil) then
  begin
    result := ErrorGeneral;
    exit;
  end;

  if ( (aConnected) and (not TCommThreadIS1885L(self.fThread).isConnected) ) then
  begin
    result := ErrorDisconnected;
    exit;
  end;

  result := OK;
end;          


procedure T_IS1885L.hndlUpdate_I6(aThread: TThread);
begin
  with TCommThreadIS1885L(aThread).fTestStatus[tkI6_InsulationTest] do
  begin
    if (Finished) then
    begin
      self.fAlreadyRead := false;
      self.fRemainTime := PhaseRemainTime;
      self.fTotRemTime := TotalRemainTime;
      self.fUgen := LastU;
      if (SensingEnabled) then self.fUreal := LastUsense
      else                     self.fUreal := LastU;
      self.fRreal := LastR;
      self.fGlobStat := STA_TEST_FINISHED;
      self.fErrCode := ErrorCode;
      self.fOL := LastOL;
      self.fFinished := Finished;
    end
    else
    begin
      self.fAlreadyRead := false;
      self.fRemainTime := PhaseRemainTime;
      self.fTotRemTime := TotalRemainTime;
      self.fUgen := ConvU;
      if (SensingEnabled) then self.fUreal := ConvUsense
      else                     self.fUreal := ConvU;
      self.fRreal := AvgConvR;
      self.fGlobStat := cTestPhase2GlobStat[TestPhase];
      self.fErrCode := ErrorCode;
      self.fOL := FlagOL;
      self.fFinished := Finished;
    end;
  end;
  
{$ifdef SERVICE}
  if (Assigned(self.fOnUpdate)) then self.fOnUpdate(fTotRemTime, fRemainTime, fUgen, fUreal, fRreal, fGlobStat, fErrCode, fOL, fFinished);
  if (Assigned(self.fOnUpdateAll)) then self.fOnUpdateAll(@TCommThreadIS1885L(aThread).fTestStatus[tkI6_InsulationTest]);
{$endif}
end;


procedure T_IS1885L.hndlUpdate_H6(aThread: TThread);
begin
  with TCommThreadIS1885L(aThread).fTestStatus[tkH6_HighVoltageTest] do
  begin
    if (Finished) then
    begin
      self.fAlreadyRead := false;
      self.fRemainTime := PhaseRemainTime;
      self.fTotRemTime := TotalRemainTime;
      self.fUgen := LastU;
      if (SensingEnabled) then self.fUreal := LastUsense
      else                     self.fUreal := LastU;
      self.fIreal := LastI;
      self.fGlobStat := STA_TEST_FINISHED;
      self.fErrCode := ErrorCode;
      self.fFinished := Finished;
    end
    else
    begin
      self.fAlreadyRead := false;
      self.fRemainTime := PhaseRemainTime;
      self.fTotRemTime := TotalRemainTime;
      self.fUgen := ConvU;
      if (SensingEnabled) then self.fUreal := ConvUsense
      else                     self.fUreal := ConvU;
      self.fIreal := ConvI;
      self.fGlobStat := cTestPhase2GlobStat[TestPhase];
      self.fErrCode := ErrorCode;
      self.fFinished := Finished;
    end;
  end;
  
{$ifdef SERVICE}
  if (Assigned(self.fOnUpdate)) then self.fOnUpdate(fTotRemTime, fRemainTime, fUgen, fUreal, fIreal, fGlobStat, fErrCode, fOL, fFinished); 
  if (Assigned(self.fOnUpdateAll)) then self.fOnUpdateAll(@TCommThreadIS1885L(aThread).fTestStatus[tkH6_HighVoltageTest]);
{$endif}
end;


class function T_IS1885L.AINr2Addr(aAINr: Word): Word;
begin
  result := AddrRng_InImg_AI_Min + ((aAINr + 1) shl 1) - 1;
end;


class function T_IS1885L.AONr2Addr(aAONr: Word): Word;
begin
  result := AddrRng_OutImg_AO_Min + ((aAONr + 1) shl 1) - 1;
end;



{ TCommThread_IS1885L }

constructor TCommThreadIS1885L.create (const aCSInputImg, aCSTestStat: TCriticalSection; const aOnUpdI6, aOnUpdH6: TThreadStatusUpdateEvent; const aUpdateInterval: Word);
var
  act:  TAction;
  roc:  TReqOrCurr;
  tk:   TTestKind;
  i:    Integer;
  ci:   TCalibBasicItem;

begin
  inherited create(true);
  self.FreeOnTerminate := false;

  self.fReadyToRelease := false;

  self.fCSInputImg := aCSInputImg;
  self.fCSTestStat := aCSTestStat;

  self.fOnUpdate[tkNoTest] := nil;
  self.fOnUpdate[tkI6_InsulationTest] := aOnUpdI6;
  self.fOnUpdate[tkH6_HighVoltageTest] := aOnUpdH6;

  self.fModbusClient := TIdModBusClient.Create(nil);
  with self.fModbusClient do
  begin
    BaseRegister := cModbusTcpBaseRegDefault;  // Important !!!
  end;

  with self.fLastCommErr do
  begin
    kind := cekNoError;
    funcCode := 0;
    errCode  := 0;
    respBuf.Header.TransactionID := 0;
    respBuf.Header.ProtocolID := 0;
    respBuf.Header.RecLength := 0;
    respBuf.Header.UnitID := 0;
    respBuf.FunctionCode := 0;
    for i := Low(respBuf.MBPData) to High(respBuf.MBPData) do respBuf.MBPData[i] := 0;
  end;
  self.fCommErrCounter[cekNoError] := 0;
  self.fCommErrCounter[cekResponseError] := 0;
  self.fCommErrCounter[cekResponseMismatch] := 0;
                 
  self.fTestRun         := tkNoTest;
  self.fTimeLastRead    := 0;
  self.fTimeTestStart   := 0;
  self.fTimeRampUpEnd   := 0;
  self.fTimeRampDnStart := 0; 
  self.fTimeTestEnd     := 0;
  self.fTimeDetectStart := 0;
  self.fTimeUminCheck   := 0;

  for tk := Low(tk) to High(tk) do
  begin
    self.clearStatusRec(fTestStatus[tk]);
  end;

  self.fConnected := false;
  
  for act := Low(act) to High(act) do
  with self.fRequests[act] do
  begin
    fReq := false;
    fReady := false;

    fVarAddr  := 0;
    fVarRegNr := 0;
    fVarParam := 0;

    fRetVal  := StatNoActionYet;
    fRetWord := 0;
  end;

  for roc := Low(roc) to High(roc) do
  begin
    with fCommParams[roc] do
    begin
      IPAddress := 0;
      PortNr := cModbusTcpPortNrDefault;
    end;

    with fI6Params[roc] do
    begin
      TestTime := 0.0;
      RampTime := 0.0;
      Ustart := 0.0;
      Unom := 0.0;
      Rmin := 0.0;
      Rrange := 0.0;
      Inverted := false;
      Usense := false;
    end;

    with fH6Params[roc] do
    begin
      TestTime := 0.0;
      RampTime := 0.0;
      Ustart := 0.0;
      Unom := 0.0;
      Imin := 0.0;
      Imax := 0.0;
      Inverted := false;
      Usense := false;
    end;

    fTestKind[roc] := tkNoTest;
    
    self.fTempRegLength := 0;
    for i := cTempRegRangeMin to cTempRegRangeMax do
    begin
      self.fTempReg[roc][i] := 0;
    end;
  end;

  self.fUpdateInterval := aUpdateInterval;
  if (aUpdateInterval > 0) then self.fUpdateNext := 0 // udpdate as soon as possible
  else                          self.fUpdateNext := High(self.fUpdateNext); // no update at all
  for i := AddrRng_InImg_Min to AddrRng_InImg_Max do
  begin
    self.fInputImage[i] := 0;
    self.fTempInputImage[i] := 0;
  end;

  for i := DigOut_min to DigOut_max do self.fDigOutImage[i] := false;

  for ci := Low(ci) to High(ci) do
  begin
    self.fCalibFactor[ci] := 1.0;
    self.fCalibOffset[ci] := 0.0;
  end;

  self.fArrayPointer := nil;

  self.Resume;  // start the thread
end;


destructor TCommThreadIS1885L.destroy;
begin
  self.fModbusClient.Free;  

  inherited;
end;


procedure TCommThreadIS1885L.execute;
var             
  act:  TAction;
  upd:  Boolean;
  b:    Boolean;
  tst:  TTestKind;
  t:    LongWord;
  rdwn: Boolean;
  x:    Double;
  y:    Double;    
  i:    Integer;
//  HGon: Boolean;

begin
  while (not Terminated) do
  begin
    upd := false; // not yet updated this cycle

    t := timeGetTime;
    if ( (self.fConnected) and (t > self.fUpdateNext) ) then
    begin { automatic update request }
      if (t > self.fUpdateGapStart + cUpdateGapDefault) then  { guard by ``update gap'' }
      begin
        upd := (self.request(actForceReadInputImage) = OK);
      end;
    end;

    for act := Low(act) to High(act) do
    begin // later defined action can be starved...
      if ( (self.fRequests[act].fReq) and (not self.fRequests[act].fReady) ) then { new request }
      begin
        self.doAction(act);

        if (act = actForceReadInputImage) then
        begin { reaction on reading of new input image }
          if (self.fTestRun in [tkI6_InsulationTest, tkH6_HighVoltageTest]) then
          begin { during test run }
            tst := self.fTestRun;
            rdwn := false;

            { Update of status/time variables. }
            self.fTimeLastRead := timeGetTime;
            self.fTestStatus[tst].ElapsedTime := self.fTimeLastRead - self.fTimeTestStart;
            if (self.fTimeLastRead >= self.fTimeTestEnd) then
            with self.fTestStatus[tst] do
            begin               
              TotalRemainTime := 0.0;
              PhaseRemainTime := 0.0;
              Finishing := true;
            end
            else
            with self.fTestStatus[tst] do
            begin
              TotalRemainTime := (self.fTimeTestEnd - self.fTimeLastRead) / 1000.0;
              case TestPhase of
                tpWaitVolt,
                tpRampUp:     PhaseRemainTime := (self.fTimeRampUpEnd - self.fTimeLastRead) / 1000.0;
                tpTest:       PhaseRemainTime := (self.fTimeRampDnStart - self.fTimeLastRead) / 1000.0;
                tpRampDown:   PhaseRemainTime := (self.fTimeTestEnd - self.fTimeLastRead) / 1000.0;
                tpFinishing:  PhaseRemainTime := 0.0;
                tpIdle:       PhaseRemainTime := 0.0;
              end;
              if (PhaseRemainTime >= 1000.0) then PhaseRemainTime := 0.0;

{$ifdef RAMP_DOWN}
              { Set flag to start ramp down. }
              rdwn := ( (TestPhase in [tpRampUp, tpTest]) and (self.fTimeLastRead >= self.fTimeRampDnStart) );
{$endif}
            end;

            { Processing of values from updated input image. }
            self.doProcessReading(tst);

            { Optional assignment of error code (e.g. limit checking). }
{$ifndef STANDALONE_IO40}
{$ifndef DEMO}
            { Found first valid voltage in test time - update of internal time intent variables... }
            with self.fTestStatus[tst] do
            if (TestPhase = tpWaitVolt) then
            begin
              if (ConvU >= LimitUmin) then
              begin
                self.fTestStatus[tst].TestPhase := tpTest;//voltage OK, go test phase
                self.fTimeRampUpEnd := self.fTimeLastRead;

                t := self.fTimeLastRead - self.fTimeWaitVoltStart;  //time diff used for wait for voltage
                
                self.fTimeRampUpEnd := self.fTimeRampUpEnd - t;    //set test times
                self.fTimeRampDnStart := self.fTimeRampDnStart - t;
                self.fTimeTestEnd := self.fTimeTestEnd - t;
                self.fTimeDetectStart := fTimeDetectStart - t;
              end;
            end;

            { Evaluation of limits }
            with self.fTestStatus[tst] do
            if (ErrorCode = 0) then
            case tst of
              tkI6_InsulationTest:
              begin
                if (self.fTestStatus[tst].TestPhase = tpTest) then
                begin
                  if ( (ConvR < LimitRmin) and (self.fTimeLastRead >= self.fTimeDetectStart) ) then
                  begin { low resistance (over-current) }
                    ErrorCode := I6_LOWRESISTANCE;
                  end
                  else if ( (ConvU < LimitUmin) {and (ReadU_minor < LimitUmin) and (self.fTimeLastRead >= self.fTimeUminCheck)} ) then
                  begin { under-voltage }
                    ErrorCode := I6_LOWVOLTAGE;
                  end
                  else if (ConvU > LimitUmax) then
                  begin { over-voltage }
                    ErrorCode := I6_HIGHVOLTAGE;
                  end
                  else if (SensingEnabled) then
                  begin
                    { sense under-voltage }
                    if (ConvUsense < LimitUsenseMin) then
                    begin
                      ErrorCode := I6_SENSE_VOLTAGE;
                    end
                    else if (ConvUsense > LimitUsenseMax) then
                    begin { sense over-voltage }
                      ErrorCode := I6_SENSE_VOLTAGE;
                    end;
                  end;
                end;
              end;

              tkH6_HighVoltageTest:
              begin
                if (self.fTestStatus[tst].TestPhase in [tpWaitVolt, tpRampUp, tpTest]) then
                begin
                  if (ReadSPO) then
                  begin { short circuit detected - too high current }
                    ErrorCode := H6_HIGHCURRENT;
                  end
                  else if (ConvI > LimitImax) then
                  begin { over-current }
                    ErrorCode := H6_HIGHCURRENT;
                  end;
                end;

                if ( (ErrorCode = 0) and (self.fTestStatus[tst].TestPhase = tpTest) ) then
                begin
                  if ( (ConvU < LimitUmin) {and (ReadU_minor < LimitUmin) and (self.fTimeLastRead >= self.fTimeUminCheck)} ) then
                  begin { under-voltage }
                    ErrorCode := H6_LOWVOLTAGE;
                  end
                  else if (ConvI < LimitImin) then
                  begin { under-current }
                    ErrorCode := H6_LOWCURRENT;
                  end
                  else if (ConvU > LimitUmax) then
                  begin { over-voltage }
                    ErrorCode := H6_HIGHVOLTAGE;
                  end
                  else if (SensingEnabled) then
                  begin
                    if (ConvUsense < LimitUsenseMin) then
                    begin { sense under-voltage }
                      ErrorCode := H6_SENSE_VOLTAGE;
                    end
                    else if (ConvUsense > LimitUsenseMax) then
                    begin { sense over-voltage }
                      ErrorCode := H6_SENSE_VOLTAGE;
                    end;
                  end;
                end;
              end;
            end;
{$endif}
{$endif}

            { Optional finishing of the test. }
            b := (self.fTestStatus[tst].ErrorCode <> 0);
            if ( (self.fTestStatus[tst].Finishing) or (b) ) then
            begin
//              i := 10;
//              brk := false;
//              while (i > 0) do // simple few times repeat
//              begin       
//                sleep(10);
//                try
//                  if (self.writeCoil(DigOut_HV_Enable, false) = OK) then
//                  begin // checking DO feedback in a loop
////                    brk := ( (self.readCoil(DigOut_HV_Enable, HGon) = OK) and (not HGon) );
//                    brk := true;
//                  end;
//                except
//                  brk := false;
//                end;
//
//                if (brk) then break;
//
//                sleep(15);
//                Dec(i);
//              end;                                  
//              sleep(10);
//              self.writeCoil(DigOut_HV_Enable, false);
//
//
//              if (not HGon) then
              begin
                sleep(25);
                for i := DigOut_min to DigOut_max_used do self.fDigOutImage[i] := false;
                if (self.writeCoils(0, Length(self.fDigOutImage), self.fDigOutImage) = OK) then
                begin
                  self.fTestStatus[tst].Finishing := false;
                  self.fTestStatus[tst].Finished := true;
                  self.fTestRun := tkNoTest;

                  sleep(25);
                  self.writeRegister(Addr_OutImg_AO1, 0);
                end;
              end;
            end
            else if (rdwn) then
            begin
              case tst of
                tkI6_InsulationTest:
                with self.fI6Params[rocCurrent] do
                begin
                  x := Unom;
                  y := RampTime;
                end;

                tkH6_HighVoltageTest:
                with self.fH6Params[rocCurrent] do
                begin
                  x := Unom;
                  y := RampTime;
                end;

                else
                begin
                  x := 0.0;
                  y := 0.0;
                end;
              end;

              self.rampProg(Addr_OutImg_AO1 - 1, x, y);
              self.writeRegister(Addr_OutImg_AO1, 0);
            end;

            { Extra handling for very high values of DetectDelay parameter (I6 only) - additional check for Rmin limit (finishing) }
            if ( ((self.fTestRun = tkNoTest) or (rdwn)) and (tst = tkI6_InsulationTest) and
                 (self.fTimeLastRead < self.fTimeDetectStart) ) then
            with self.fTestStatus[tst] do
            begin
              if (ErrorCode = 0) then
              begin
                if (ConvR < LimitRmin)  then
                begin
                  self.fTestStatus[tst].ErrorCode := I6_LOWRESISTANCE;
                end;
              end;
            end;

            { To update intermediate values in interface object. }
            self.doUpdateStatus(tst);
          end;
        end;

        break; // process first active request
      end;
    end;

    if (upd) then self.ack(actForceReadInputImage);

    sleep(1);
  end;

  self.fReadyToRelease := true;
end;    


procedure TCommThreadIS1885L.doAction(const aAction: TAction);
var
  retVal: TRetVal;
  b:      Boolean;
  w:      Word;
  dw:     LongWord;
  i:      Integer;
  mask:   Word;
  act:    TAction;
  CB:     Word;
  x:      Double;
  rv:     TRetVal;
  tt:     Double;
  rt:     Double;
  rd:     Boolean;

begin
  self.fRequests[aAction].fReady := false;  // to be sure
  retVal := ErrorGeneral;

  if ( (aAction <> actNewConnect) and (aAction <> actDisconnect) and (not self.fModbusClient.Connected) ) then
  begin // expected connected client, but it is not                                   
    retVal := ErrorDisconnected;
    self.fConnected := false;
    self.fRequests[aAction].fRetWord := 0;
  end
  else
  case aAction of // already checked connection status
    { at first more calls of function code 4 }
    actForceReadInputImage:
    begin
      try
        if (self.fModbusClient.ReadInputRegisters(AddrRng_InImg_Min, Length(self.fTempInputImage), self.fTempInputImage)) then
        begin
          retVal := OK;

          self.fCSInputImg.Enter;
          for i := AddrRng_InImg_Min to AddrRng_InImg_Max do
          begin
            self.fInputImage[i] := self.fTempInputImage[i];
          end;
          self.fCSInputImg.Leave;
        end
        else
        begin
          retVal := ErrorCommunication;
        end;
      except
        retVal := ErrorCommunication;
      end;

      { Determine time interval before new input image update is forcibly readed. }
      if (self.fTestRun <> tkNoTest) then dw := self.fUpdateInterval
      else                                dw := cInTestUpdateInterval;
      
      self.fUpdateNext := timeGetTime + dw;
    end;

    { function code 5 }
    actWriteSingleDO:
    with self.fRequests[aAction] do
    begin
      retVal := self.writeCoil(fVarAddr, fVarParam <> 0);
    end;

    { function code 15 }
    actWriteMultiDOs:     
    with self.fRequests[aAction] do
    begin
      mask := 1;
      for i := DigOut_min to DigOut_max do
      begin { Setting all 16 outputs at once. }
        self.fDigOutImage[i] := ((fVarParam and mask) <> 0);
        mask := mask shl 1;  
      end;

      retVal := self.writeCoils(0, Length(self.fDigOutImage), self.fDigOutImage);
    end;

    { function code 1 }
    actReadSingleDO:
    with self.fRequests[aAction] do
    begin
      retVal := self.readCoil(fVarAddr, b);
      if (retVal = OK) then fRetWord := Abs(Ord(b));
    end;

    { function code 4 }
    actReadSingleReg:
    with self.fRequests[aAction] do
    begin
      retVal := self.readRegister(fVarAddr, w);
      if (retVal = OK) then fRetWord := w;
    end;

    { function code 4 }
    actReadMultiRegs:   
    with self.fRequests[aAction] do
    begin
      retVal := self.readRegisters(fVarAddr, fVarParam, self.fTempReg[rocCurrent]);
    end;

    { function code 6 }
    actWriteSingleReg:
    with self.fRequests[aAction] do
    begin
      retVal := self.writeRegister(fVarAddr, fVarParam);
    end;

    { function code 16 }
    actWriteMultiRegs:   
    with self.fRequests[aAction] do
    begin
      { optional setting of different dynamic array lenght }
//      if (self.fTempRegLength <> Length(self.fTempRegDyn)) then SetLength(self.fTempRegDyn, self.fTempRegLength);
//
      for i := 0 to self.fTempRegLength - 1 do
      begin
        self.fTempReg[rocCurrent][i] := self.fTempReg[rocRequested][i];
      end;

      retVal := self.writeRegisters(fVarAddr, self.fTempRegLength, self.fTempReg[rocCurrent]);
    end;  

    { function codes 6 + 6 }
    actRegCommWrite:
    with self.fRequests[aAction] do
    begin                           
      retVal := self.regCommWrite(fVarAddr, fVarRegNr, fVarParam);
    end;

    { function codes 6 + 4 + 6 }
    actRegCommRead:
    with self.fRequests[aAction] do
    begin
      retVal := self.regCommRead(fVarAddr, fVarRegNr, fRetWord);
    end;

    { --- }
    actNewConnect:
    begin
      try
        if (self.fModbusClient.Connected) then
        begin
          self.fModbusClient.Disconnect;
        end;

        self.fCommParams[rocCurrent] := self.fCommParams[rocRequested];

        with self.fModbusClient do
        begin
          Host := SynaIP.IpToStr(self.fCommParams[rocCurrent].IPAddress);
          Port := self.fCommParams[rocCurrent].PortNr;
          BaseRegister := cModbusTcpBaseRegDefault;
          // UnitID := self.fCommParams[rocCurrent].UnitID;
          OnConnected := self.hndlConnect;
          OnDisconnected := self.hndlDisconnect;
          OnResponseError := self.hndlRespErr;
          OnResponseMismatch := self.hndlRespMismatch;
          ConnectTimeOut := cModbusTcpConnTimeout;
          Connect(cModbusTcpConnTimeout);
          ReadTimeout := cModbusTcpReadTimeout;
        end;

        if (self.fModbusClient.Connected) then
        begin
          retVal := OK;
        end
        else
        begin
          retVal := ErrorCommunication;
        end;
      except
        retVal := ErrorCommunication;
      end
    end;

    { --- }
    actDisconnect:
    begin
      try
        b := true;
        b := b and (self.writeCoil(DigOut_HV_Enable, false) = OK);
        b := b and (self.writeCoil(DigOut_K_Discharge, false) = OK);
        b := b and (self.writeRegister(Addr_OutImg_AO1, 0) = OK);
        if (b) then begin { hint catcher } end;

        self.fModbusClient.Disconnect;

        for act := Low(act) to High(act) do
        if (act <> actDisconnect) then
        with self.fRequests[act] do
        begin
          fReq := false;
          fReady := false;
          fVarAddr := 0;
          fVarRegNr := 0;
          fVarParam := 0;
          fRetVal := OK;
          fRetWord := 0;
        end; 

        retVal := OK;
      except
        retVal := ErrorCommunication;
      end;
    end;
            
    { --- }
    actTestStop:
    begin
      try
        b := true;
        b := b and (self.writeCoil(DigOut_HV_Enable, false) = OK);
        b := b and (self.writeCoil(DigOut_K_Discharge, false) = OK);
        b := b and (self.writeRegister(Addr_OutImg_AO1, 0) = OK);
        if (b) then
        begin
          retVal := OK;
          
          self.fTestRun := tkNoTest;
          self.fTestStatus[tkI6_InsulationTest].ReadyToStart := false;
          self.fTestStatus[tkH6_HighVoltageTest].ReadyToStart := false;
        end
        else
        begin
          retVal := ErrorCommunication;
        end;
      except
        retVal := ErrorCommunication;
      end;
    end;
           
    { --- }
    actTestPrep:
    with self.fRequests[aAction] do
    begin
      if (self.fTestRun <> tkNoTest) then
      begin { test already runs - unable to start new test }
        retVal := ErrorAlreadyInTest;
      end
      else if (fVarAddr <> cNoAddr) then
      begin { request for test start has to have address 0 }
        retVal := ErrorInvalidAddress;
      end
      else if (not (TTestKind(fVarParam) in [tkI6_InsulationTest, tkH6_HighVoltageTest])) then
      begin { requested unsupported test (or even no test) }
        retVal := ErrorInvalidTest;
      end
      else
      begin
        i := fVarParam;
        case TTestKind(i) of
          tkI6_InsulationTest:
          begin
            self.fTestStatus[tkNoTest].ReadyToStart := false;
            self.fTestStatus[tkH6_HighVoltageTest].ReadyToStart := false;

            self.fI6Params[rocCurrent] := self.fI6Params[rocRequested];

            self.clearStatusRec(self.fTestStatus[tkI6_InsulationTest]);
            with self.fTestStatus[tkI6_InsulationTest] do
            begin
              if (self.fI6Params[rocCurrent].Usense) then
              begin
                SensingEnabled := true;
//                LimitUsenseMin := Trunc(cUminPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult});
//                LimitUsenseMax := Trunc(cUmaxPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult});
//                if (LimitUsenseMax > $7FFF) then LimitUsenseMax := $7FFF;
                LimitUsenseMin := cUminPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult;
                LimitUsenseMax := cUmaxPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult;
                if (LimitUsenseMax > cParUstartLimitMax) then LimitUsenseMax := cParUstartLimitMax;
              end
              else
              begin
                SensingEnabled := false;
                LimitUsenseMin := cParUstartLimitMin; // Low(LimitUsenseMin);
                LimitUsenseMax := cParUstartLimitMax; // High(LimitUsenseMax);
              end;

//              LimitUmin := Trunc(cUminPercDef * self.fI6Params[rocCurrent].Unom * cScale24bWriteMult * cUrangeWriteMult);
//              LimitUmax := Trunc(cUmaxPercDef * self.fI6Params[rocCurrent].Unom * cScale24bWriteMult * cUrangeWriteMult);
//              if (LimitUmax > $7FFFFF) then LimitUmax := $7FFFFF;
              LimitUmin := cUminPercDef * self.fI6Params[rocCurrent].Unom;
              LimitUmax := cUmaxPercDef * self.fI6Params[rocCurrent].Unom;
              if (LimitUmax > cParUstartLimitMax) then LimitUmax := cParUstartLimitMax;

              LimitImin := cParIminLimitMin; // Low(LimitImin);  // no direct checking of current value in IS test
              LimitImax := cParIminLimitMax; // High(LimitImax);

              LimitRmin := self.fI6Params[rocCurrent].Rmin;
              RangeRmax := self.fI6Params[rocCurrent].Rrange; // not need to calculate it again by calcRrange()

              AvgConvRCacheSize := self.fI6Params[rocCurrent].AvgResWinSize;
              AvgConvRIndex := -1 - self.fI6Params[rocCurrent].AvgResDelay;
            end;

{$ifdef EXTERNAL_PROCEDURES} 
            retVal := OK;
            self.fTestStatus[tkI6_InsulationTest].ReadyToStart := true;
            self.fTestKind[rocRequested] := tkI6_InsulationTest;
{$else}
            try
              b := true;
              { Set Digital Out for K1 - normal test }
              b := b and (self.writeCoil(DigOut_K1_HV_Normal, not self.fI6Params[rocCurrent].Inverted) = OK);

              { Opt.: set Digital Out for K2, inverted test instead of K1 }
              b := b and (self.writeCoil(DigOut_K2_HV_Inverted, self.fI6Params[rocCurrent].Inverted) = OK);

              { Opt.: set additional Digital Output for K3 USense }
              b := b and (self.writeCoil(DigOut_K3_HV_Sense, self.fI6Params[rocCurrent].Usense) = OK);

              { Opt.: set Ramp Function with CW B7 in R0 and R1 for Addresses 0x0800 and 0x0801 - R0 = start value, R1 = time value }
              b := b and (self.rampProg(Addr_OutImg_AO1 - 1, self.fI6Params[rocCurrent].Ustart, self.fI6Params[rocCurrent].RampTime) = OK);

              { Set NO Current range - for IS test, no current range is selected, HG40 will run on automatic current range mode. }

              { Setting of AO and disabling of K Discharge DO is moved to the actTestStart procedure itself. }

              if (b) then
              begin
                retVal := OK;
                self.fTestStatus[tkI6_InsulationTest].ReadyToStart := true;
                self.fTestKind[rocRequested] := tkI6_InsulationTest;
                sleep(10);
              end
              else
              begin
                retVal := ErrorCommunication;
              end;
            except
              retVal := ErrorCommunication;
            end;
{$endif}
          end;

          tkH6_HighVoltageTest:
          begin
            self.fTestStatus[tkNoTest].ReadyToStart := false;
            self.fTestStatus[tkI6_InsulationTest].ReadyToStart := false;

            self.fH6Params[rocCurrent] := self.fH6Params[rocRequested];

            self.clearStatusRec(self.fTestStatus[tkH6_HighVoltageTest]);
            with self.fTestStatus[tkH6_HighVoltageTest] do
            begin
              if (self.fH6Params[rocCurrent].Usense) then
              begin
                SensingEnabled := true;
//                LimitUsenseMin := Trunc(cUminPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult);
//                LimitUsenseMax := Trunc(cUmaxPercDef * self.fI6Params[rocCurrent].Unom * cScale16bWriteMult * cUrangeWriteMult);
//                if (LimitUsenseMax > $7FFF) then LimitUsenseMax := $7FFF;
                LimitUsenseMin := cUminPercDef * self.fH6Params[rocCurrent].Unom;
                LimitUsenseMax := cUmaxPercDef * self.fH6Params[rocCurrent].Unom;
                if (LimitUsenseMax > cParUstartLimitMax) then LimitUsenseMax := cParUstartLimitMax;
              end
              else
              begin
                SensingEnabled := false;
                LimitUsenseMin := cParUstartLimitMin; // Low(LimitUsenseMin);
                LimitUsenseMax := cParUstartLimitMax; // High(LimitUsenseMax);
              end;

//              LimitUmin := Trunc(cUminPercDef * self.fI6Params[rocCurrent].Unom * cScale24bWriteMult * cUrangeWriteMult);
//              LimitUmax := Trunc(cUmaxPercDef * self.fI6Params[rocCurrent].Unom * cScale24bWriteMult * cUrangeWriteMult);
//              if (LimitUmax > $7FFFFF) then LimitUmax := $7FFFFF;
              LimitUmin := cUminPercDef * self.fH6Params[rocCurrent].Unom;
              LimitUmax := cUmaxPercDef * self.fH6Params[rocCurrent].Unom;
              if (LimitUmax > cParUstartLimitMax) then LimitUmax := cParUstartLimitMax;

//              LimitImin := Trunc(self.fH6Params[rocCurrent].Imin * cScale24bWriteMult * cIrangeWriteMult_10mA);
//              LimitImax := Trunc(self.fH6Params[rocCurrent].Imax * cScale24bWriteMult * cIrangeWriteMult_10mA);
//              if (LimitImax > $7FFFFF) then LimitImax := $7FFFFF;
              LimitImin := self.fH6Params[rocCurrent].Imin;
              LimitImax := self.fH6Params[rocCurrent].Imax;

              LimitRmin := 0.0;  // no limit for resistance
              RangeRmax := 0.0;
            end;

{$ifdef EXTERNAL_PROCEDURES}
            retVal := OK;
            self.fTestStatus[tkH6_HighVoltageTest].ReadyToStart := true;
            self.fTestKind[rocRequested] := tkH6_HighVoltageTest;
{$else}
            try
              b := true;
              { Set Digital Out for K1 - normal test }
              b := b and (self.writeCoil(DigOut_K1_HV_Normal, not self.fH6Params[rocCurrent].Inverted) = OK);

              { Opt.: set Digital Out for K2, inverted test instead of K1 }
              b := b and (self.writeCoil(DigOut_K2_HV_Inverted, self.fH6Params[rocCurrent].Inverted) = OK);

              { Opt.: set additional Digital Output for K3 USense }
              b := b and (self.writeCoil(DigOut_K3_HV_Sense, self.fH6Params[rocCurrent].Usense) = OK);

              { Opt.: set Ramp Function with CW B7 in R0 and R1 for Addresses 0x0800 and 0x0801 - R0 = start value, R1 = time value }
              b := b and (self.rampProg(Addr_OutImg_AO1 - 1, self.fH6Params[rocCurrent].Ustart, self.fH6Params[rocCurrent].RampTime) = OK);

              { Set Current range 10 mA }
              b := b and (self.writeCoil(DigOut_10mA_Range, true) = OK);

              { Setting of AO and disabling of K Discharge DO is moved to the actTestStart procedure itself. }
              if (b) then
              begin
                retVal := OK;
                self.fTestStatus[tkH6_HighVoltageTest].ReadyToStart := true;
                self.fTestKind[rocRequested] := tkH6_HighVoltageTest;
                sleep(10);
              end
              else
              begin
                retVal := ErrorCommunication;
              end;
            except
              retVal := ErrorCommunication;
            end;
{$endif}
          end;

          else  // anything else than I6 and H6
          begin
            retVal := ErrorInvalidTest;
          end;
        end;
      end;
    end;

    { --- }
    actTestStart:
    begin
      if (self.fTestRun <> tkNoTest) then
      begin { test already runs - unable to start new test }
        retVal := ErrorAlreadyInTest;
      end
      else if (self.fRequests[aAction].fVarAddr <> cNoAddr) then
      begin { request for test start has to have address 0 }
        retVal := ErrorInvalidAddress;
      end
      else if (not (TTestKind(self.fRequests[aAction].fVarParam) in [tkI6_InsulationTest, tkH6_HighVoltageTest])) then
      begin { requested not supported test (or even no test) }
        retVal := ErrorInvalidTest;
      end   
      else if (TTestKind(self.fRequests[aAction].fVarParam) <> self.fTestKind[rocRequested]) then
      begin { requested start of another test than the one prepared last time }
        retVal := ErrorNotYetPreparedTest;
      end
      else if (not self.fTestStatus[self.fTestKind[rocRequested]].ReadyToStart) then
      begin { test was not yet prepared by devTestPrepXX() method }
        retVal := ErrorNotYetPreparedTest;
      end
      else
      begin { ready for starting of the test }
        try
          b := true;
          rv := ErrorCommunication;

          self.fTestStatus[self.fTestKind[rocRequested]].ReadyToStart := false;   // disabled repeated run without call to devTestPrep_XX()

          { Check if HG40 is ready. }
{$ifndef STANDALONE_IO40}
          b := b and (self.readRegister(AddrRng_InImg_DI_Min, w) = OK);
          if (b) then
          begin
            if (((1 shl DigIn_HG40_Ready) and w) = 0) then
            begin
              b := false;
              rv := ErrorHG40NotReady;
            end;
          end;
{$endif}

          { Set Analog Out Unom }
          x := 0;
          case self.fTestKind[rocRequested] of
            tkI6_InsulationTest:  x := self.fI6Params[rocCurrent].Unom;
            tkH6_HighVoltageTest: x := self.fH6Params[rocCurrent].Unom;
            else b := false;
          end;
          
          x := (x - self.fCalibOffset[BKUNom]) * fCalibFactor[BKUNom];
          if (x < 0) then x := 0;
          w := Trunc(x * cUrangeWriteMult * cScale16bWriteMult);
          if (w > $7FFF) then w := $7FFF;
          b := b and (self.writeRegister(Addr_OutImg_AO1, w) = OK);

          { Set Analog Out for K Discharge }
          b := b and (self.writeCoil(DigOut_K_Discharge, true) = OK);

          { 10ms Sleep }
          sleep(10);

          { Enable generator HG40 via digital output }
          b := b and (self.writeCoil(DigOut_HV_Enable, true) = OK);

          if (b) then
          begin
            self.fTestKind[rocCurrent] := self.fTestKind[rocRequested];

            tt := 0.0;
            rt := 0.0;
            rd := false;
            case self.fTestKind[rocCurrent] of
              tkI6_InsulationTest:
              with self.fI6Params[rocCurrent] do
              begin
                tt := TestTime;
                rt := RampTime;
                rd := RampDown;
              end;

              tkH6_HighVoltageTest:
              with self.fH6Params[rocCurrent] do
              begin
                tt := TestTime;
                rt := RampTime;
                rd := RampDown;
              end;
            end;

            self.fTimeTestStart := timeGetTime;
            self.fTimeWaitVoltStart := self.fTimeTestStart + Trunc(1000.0 * rt);
            self.fTimeRampUpEnd := self.fTimeWaitVoltStart + CWaitforVoltageTimeout;
            self.fTimeRampDnStart := self.fTimeRampUpEnd + Trunc(1000.0 * tt);
            self.fTimeTestEnd := self.fTimeRampDnStart;
            if (rd) then self.fTimeTestEnd := self.fTimeTestEnd + Trunc(1000.0 * rt);

            { Time intent since minimum resistance limit is evaluated }
            self.fTimeDetectStart := self.fTimeRampUpEnd;
            if (self.fTestKind[rocCurrent] = tkI6_InsulationTest) then
            with self.fI6Params[rocCurrent] do
            begin
              tt := 10.0 * DetectDelay * TestTime;  // [ms]
              dw := Trunc(tt);
              if (dw < cDetectDelayMinimum_ms) then dw := cDetectDelayMinimum_ms; // internal minimum of 150 ms
              if (dw > cDetectDelayMaximum_ms) then dw := cDetectDelayMaximum_ms; // internal maximum of 999.9 s
              self.fTimeDetectStart := self.fTimeDetectStart + dw;
            end;

            { Time intent since minimum voltage limit is evaluated }
            if (self.fTimeRampUpEnd - self.fTimeTestStart < cVoltageCheckStart_ms) then
            begin
              self.fTimeUminCheck := self.fTimeRampUpEnd + cVoltageCheckStart_ms;
            end
            else
            begin
              self.fTimeUminCheck := self.fTimeRampUpEnd;
            end;

            self.fUpdateNext := self.fTimeTestStart + 10;  // next update

            self.fTestRun := self.fTestKind[rocCurrent];

            retVal := OK;
          end
          else
          begin
            retVal := rv;
          end;
        except
          retVal := ErrorCommunication;
        end;
      end;
    end;
  end;

  self.fRequests[aAction].fRetVal := retVal;
  self.fRequests[aAction].fReady := true;     // action done - waiting for processing of results and ack
end;


procedure TCommThreadIS1885L.doProcessReading(const aTestKind: TTestKind);
const
  cICalib: array [TIRange] of TCalibBasicItem = ( HG40I1uA, HG40I100uA, HG40I10mA );

var
  w:  Word;
  x:  Double;
  dw: LongWord;
  di: Integer;
  ix: Double;
  cr: Boolean;
  ci: TCalibBasicItem;
  n:  Integer;
  i:  Integer;
  
begin
  if (not (aTestKind in [tkI6_InsulationTest, tkH6_HighVoltageTest])) then exit;

  with self.fTestStatus[aTestKind] do
  begin
         if (Finishing)                                     then TestPhase := tpFinishing
    else if (self.fTimeLastRead <= self.fTimeWaitVoltStart) then TestPhase := tpWaitVolt
    else if (self.fTimeLastRead <= self.fTimeRampUpEnd)     then TestPhase := tpRampUp
    else if (self.fTimeLastRead >= self.fTimeRampDnStart)   then TestPhase := tpRampDown
    else                                                         TestPhase := tpTest;

    { Digital inputs processing }
    w := self.fInputImage[AddrRng_InImg_DI_Min];

    ReadHG40Ready := (((1 shl DigIn_HG40_Ready) and w) <> 0); // HG40 indication

    ReadSPO := (((1 shl DigIn_HG40_SPO) and w) <> 0); // short-circuit detection flag

    if (((1 shl DigIn_HG40_100uA) and w) <> 0)     then ReadIrange := ir100uA
    else if (((1 shl DigIn_HG40_10mA) and w) <> 0) then ReadIrange := ir10mA
    else                                                ReadIrange := ir1uA;   // current range

    if (Finished) then
    begin
      TestPhase := tpIdle;
    end
    else if (TestPhase in [tpRampUp, tpTest, tpRampDown]) then
    begin
      { Prepare current range multiplier }
      case ReadIrange of
        ir1uA:    ix := cIrangeReadMult_1uA;
        ir100uA:  ix := cIrangeReadMult_100uA;
        ir10mA:   ix := cIrangeReadMult_10mA;
        else      ix := cIrangeReadMult_10mA;
      end;

      { Analog inputs processing }
      dw := 0;
      w := self.fInputImage[AddrRng_InImg_HG40I_Max];
      if ((WordRec(w).Hi and $80) <> 0) then LongRec(dw).Bytes[3]:= $FF;
      LongRec(dw).Bytes[2] := WordRec(w).Hi;
      LongRec(dw).Bytes[1] := WordRec(w).Lo;
      w := self.fInputImage[AddrRng_InImg_HG40I_Min];
      LongRec(dw).Bytes[0] := WordRec(w).Hi;
      di := -Integer(dw);
      if (di < 0) then di := 0;
      dw := LongWord(di);
      ReadI := dw;  // 24b value of current measurement
      x := ix * cScale24bReadMult * di;
      ci := cICalib[ReadIrange];
      ConvI := (x - self.fCalibOffset[ci]) * fCalibFactor[ci];
      if (ConvI < 0.0) then ConvI := 0.0;

      dw := 0;
      w := self.fInputImage[AddrRng_InImg_HG40U_Max];
      if ((WordRec(w).Hi and $80) <> 0) then LongRec(dw).Bytes[3] := $FF;
      LongRec(dw).Bytes[2] := WordRec(w).Hi;
      LongRec(dw).Bytes[1] := WordRec(w).Lo;
      w := self.fInputImage[AddrRng_InImg_HG40U_Min];
      LongRec(dw).Bytes[0] := WordRec(w).Hi;
      di := Integer(dw);
      if (di < 0) then di := 0;
      dw := LongWord(di);
      ReadU := dw;  // 24b value of voltage measurement 
      x := cUrangeReadMult * cScale24bReadMult * di;
      ci := HG40U;
      ConvU := (x - self.fCalibOffset[ci]) * fCalibFactor[ci];
      if (ConvU < 0.0) then ConvU := 0.0;

      w := self.fInputImage[T_IS1885L.AINr2Addr(AlogIn_IHV)];
      ReadI_BK := w shl 8; // 16b value of current measurement treated as 24b
      x := cIrangeReadMult_10mA * cScale24bReadMult * dw; { as stated fixed 10 mA current range on AI2 }
      ci := BKIReal;
      ConvI_BK := (x - self.fCalibOffset[ci]) * fCalibFactor[ci];
      if (ConvI_BK < 0.0) then ConvI_BK := 0.0;

      w := self.fInputImage[T_IS1885L.AINr2Addr(AlogIn_UHV)];
      ReadU_BK := w shl 8; // 16b value of voltage measurement treated as 24b   
      x := cUrangeReadMult * cScale24bReadMult * dw;
      ci := BKUReal;
      ConvU_BK := (x - self.fCalibOffset[ci]) * fCalibFactor[ci];
      if (ConvU_BK < 0.0) then ConvU_BK := 0.0;

      w := self.fInputImage[T_IS1885L.AINr2Addr(AlogIn_TRMS_Usense)];
      ReadUsense := w;  // 16b value of sense voltage (connected to TRMS output of U40)
      x := cUrangeReadMult * cScale16bReadMult * w;
      ci := Usense;
      ConvUsense := (x - self.fCalibOffset[ci]) * fCalibFactor[ci];
      if (ConvUsense < 0.0) then ConvUsense := 0.0;

      { Iteration of averaging algorithm }
      Inc(AvgConvRIndex);
      if (AvgConvRIndex >= 0) then
      begin
        n := AvgConvRIndex mod AvgConvRCacheSize;

        AvgConvUCache[n] := ConvU;
        AvgConvICache[n] := ConvI;
                  
        if (AvgConvRIndex >= AvgConvRCacheSize) then n := AvgConvRCacheSize - 1;
        AvgConvU := 0.0;
        AvgConvI := 0.0;
        for i := n downto 0 do
        begin
          AvgConvU := AvgConvU + AvgConvUCache[i];
          AvgConvI := AvgConvI + AvgConvICache[i];
        end;

        if (n > 0) then
        begin // after averaging 
          Inc(n);       // n contains number of elements of calculated sum
          AvgConvU := AvgConvU / n;
          AvgConvI := AvgConvI / n;   

          if (IsZero(AvgConvI)) then
          begin
            AvgConvR := RangeRmax;
            AvgFlagOL := true;
          end
          else
          begin
            AvgConvR := AvgConvU / AvgConvI;
            AvgFlagOL := (AvgConvR > RangeRmax);
            if (AvgFlagOL) then AvgConvR := RangeRmax;
          end;
        end;
      end
      else
      begin
        cr := (ConvI > 0.0);
        if (cr) then
        begin
          ConvR := ConvU / ConvI;
          FlagOL := (ConvR > RangeRmax);
          if (FlagOL) then ConvR := RangeRmax;
        end
        else
        begin
          ConvR := RangeRmax;
          FlagOL := true;
        end;

        AvgConvU := 0.0;
        AvgConvI := 0.0;
        AvgConvR := ConvR;
        AvgFlagOL := FlagOL;
      end;

      { Keep there last measured values from the test time }
      if (TestPhase in [tpWaitVolt, tpRampUp, tpTest]) then
      begin
        LastUsense := ConvUsense;
        LastU := ConvU;
        LastI := ConvI;
        LastR := ConvR;
        LastOL := FlagOL;
      end;

{$ifdef MIN_MAX}
      if (TestPhase = tpTest) then
      begin
        if (YetNoMinMax) then
        begin           
          YetNoMinMax := false;
          
          MinReadIrange := ReadIrange;
          MaxReadIrange := ReadIrange;
          MinReadUsense := ReadUsense;
          MaxReadUsense := ReadUsense;
          MinReadU := ReadU;
          MaxReadU := ReadU;
          MinReadI := ReadI;
          MaxReadI := ReadI;
          if (cr) then
          begin
            MinConvR := ConvR;
            MaxConvR := ConvR;
          end;
        end
        else
        begin
          if (MinReadIrange > ReadIrange) then MinReadIrange := ReadIrange;
          if (MaxReadIrange < ReadIrange) then MaxReadIrange := ReadIrange;
          if (MinReadUsense > ReadUsense) then MinReadUsense := ReadUsense;
          if (MaxReadUsense < ReadUsense) then MaxReadUsense := ReadUsense;
          if (MinReadU > ReadU) then MinReadU := ReadU;
          if (MaxReadU < ReadU) then MaxReadU := ReadU;
          if (MinReadI > ReadI) then MinReadI := ReadI;
          if (MaxReadI < ReadI) then MaxReadI := ReadI;
          if (cr) then
          begin
            if ( (Math.IsNaN(MinConvR)) or (MinConvR > ConvR) ) then MinConvR := ConvR;
            if ( (Math.IsNaN(MaxConvR)) or (MaxConvR < ConvR) ) then MaxConvR := ConvR;
          end;
        end;
      end;
{$endif}
    end;
  end;
end; 


procedure TCommThreadIS1885L.doUpdateStatus(const aTestKind: TTestKind);
begin
  if (not (aTestKind in [tkI6_InsulationTest, tkH6_HighVoltageTest])) then
  begin { cannot update unsupported test }
    exit;
  end
  else if (not Assigned(self.fOnUpdate[aTestKind])) then
  begin { cannot update if no event handler is prepared }
    exit;
  end
  else
  with self.fTestStatus[aTestKind] do
  begin
    self.fCSTestStat.Enter;
    self.fOnUpdate[aTestKind](self);
    self.fCSTestStat.Leave;
  end;
end;


procedure TCommThreadIS1885L.clearStatusRec(var aRec: TTestStatus);
begin
  with aRec do
  begin
    ReadyToStart        := false;                          
    Finishing           := false;
    Finished            := false;
    ErrorCode           := 0;
    ElapsedTime         := 0;
    TestPhase           := tpIdle;
    SensingEnabled      := false;
    LimitUsenseMin      := 0;
    LimitUsenseMax      := 0;
    LimitUmin           := 0;
    LimitUmax           := 0;
    LimitImin           := 0;
    LimitImin           := 0;
    ReadHG40Ready       := false;
    ReadSPO             := false;
    ReadIrange          := ir10mA;
    ReadUsense          := 0;
    ReadU               := 0;
    ReadI               := 0;
    ConvUsense          := -1.0;
    ConvU               := -1.0;
    ConvI               := -1.0;
    ConvR               := -1.0;
    ConvI_BK            := -1.0;
    ConvU_BK            := -1.0;
    FlagOL              := false;
    LastUsense          := -1.0;
    LastU               := -1.0;
    LastI               := -1.0;
    LastR               := -1.0;
    LastOL              := false;
{$ifdef MIN_MAX}
    YetNoMinMax         := true;
    MinReadIrange       := 0;
    MinReadUsense       := 0;
    MinReadU            := 0;
    MinReadI            := 0;
    MinConvR            := NaN;
    MaxReadIrange       := 0;
    MaxReadUsense       := 0;
    MaxReadU            := 0;
    MaxReadI            := 0;
    MaxConvR            := NaN;
{$endif}
  end;
end;


procedure TCommThreadIS1885L.hndlConnect(aSender: TObject);
var
  i:  Integer;
  
begin                      
  with self.fLastCommErr do
  begin
    kind := cekNoError;
    funcCode := 0;
    errCode  := 0;
    respBuf.Header.TransactionID := 0;
    respBuf.Header.ProtocolID := 0;
    respBuf.Header.RecLength := 0;
    respBuf.Header.UnitID := 0;
    respBuf.FunctionCode := 0;
    for i := Low(respBuf.MBPData) to High(respBuf.MBPData) do respBuf.MBPData[i] := 0;
  end;                  
  self.fCommErrCounter[cekNoError] := 0;
  self.fCommErrCounter[cekResponseError] := 0;
  self.fCommErrCounter[cekResponseMismatch] := 0;
  self.fConnected := true;
end;


procedure TCommThreadIS1885L.hndlDisconnect(aSender: TObject);
begin
  self.fConnected := false;
end; 


procedure TCommThreadIS1885L.hndlRespErr(const aFunctionCode, aErrorCode: Byte; const aResponseBuffer: TModBusResponseBuffer);
begin
  with self.fLastCommErr do
  begin
    kind := cekResponseError;
    funcCode := aFunctionCode;
    errCode  := aErrorCode;
    respBuf.Header := aResponseBuffer.Header;
    respBuf.FunctionCode := aResponseBuffer.FunctionCode;
    respBuf.MBPData := aResponseBuffer.MBPData;
  end;
  Inc(self.fCommErrCounter[cekResponseError]);
end;


procedure TCommThreadIS1885L.hndlRespMismatch(const aFunctionCode, aErrorCode: Byte; const aResponseBuffer: TModBusResponseBuffer);
begin
  with self.fLastCommErr do
  begin
    kind := cekResponseMismatch;
    funcCode := aFunctionCode;
    errCode  := aErrorCode;
    respBuf.Header := aResponseBuffer.Header;
    respBuf.FunctionCode := aResponseBuffer.FunctionCode;
    respBuf.MBPData := aResponseBuffer.MBPData;
  end;    
  Inc(self.fCommErrCounter[cekResponseMismatch]);
end;


function TCommThreadIS1885L.writeCoil(const aRegNo: Word; const aValue: Boolean): TRetVal;
begin
  try
    if (self.fModbusClient.WriteCoil(aRegNo, aValue)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end;

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.writeCoils (const aRegNo, aBlocks: Word; const aRegisterData: array of Boolean): TRetVal;
begin
  try
    if (self.fModbusClient.WriteCoils(aRegNo, aBlocks, aRegisterData)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end;

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.readCoil(const aRegNo: Word; var aValue: Boolean): TRetVal;
begin
  try
    if (self.fModbusClient.ReadCoil(aRegNo, aValue)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end; 

  self.fUpdateGapStart := timeGetTime;
end;  


function TCommThreadIS1885L.writeRegister(const aRegNo, aValue: Word): TRetVal;
begin
  try
    if (self.fModbusClient.WriteRegister(aRegNo, aValue)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end; 

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.writeRegisters(const aRegNo: Word; const aBlocks: Word; const aRegisterData: array of Word): TRetVal;
begin
  try
    if (self.fModbusClient.WriteRegisters(aRegNo, aBlocks, aRegisterData)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end; 

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.readRegister(const aRegNo: Word; var aValue: Word): TRetVal;
begin
  try
    if (self.fModbusClient.ReadInputRegister(aRegNo, aValue)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end; 

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.readRegisters(const aRegNo, aBlocks: Word; var aRegisterData: array of Word): TRetVal;
begin
  try
    if (self.fModbusClient.ReadInputRegisters(aRegNo, aBlocks, aRegisterData)) then
      result := OK
    else
      result := ErrorCommunication;
  except
    result := ErrorCommunication;
  end; 

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.regCommRead(const aStatOrCtrlByteAddr: Word; const aRegNr: Byte; var aReadValue: Word): TRetVal;
var        
  SB: Word;
  CB: Word;
  w:  Word;
  b:  Boolean;

begin
  if (aStatOrCtrlByteAddr >= AddrRng_OutImg_AO_Min) then
  begin
    CB := aStatOrCtrlByteAddr;
    SB := aStatOrCtrlByteAddr - OutImg_Offset;
  end
  else
  begin
    CB := aStatOrCtrlByteAddr + OutImg_Offset;
    SB := aStatOrCtrlByteAddr;
  end;

  w := $80 or (aRegNr and $3F);  // MSB is set (reg.comm|read) and lower 6 bits holds register number

  b := true;
  try
    b := b and (self.writeRegister(CB, w) = OK);
    b := b and (self.readRegister(SB + 1, w) = OK);
    b := (self.writeRegister(CB, $00) = OK) and b; // forced disable of register communication mode

    if (b) then
    begin
      result := OK;
      aReadValue := w;
    end
    else
    begin
      result := ErrorCommunication;

      self.writeRegister(CB, $00);
    end;
        
  except
    result := ErrorCommunication;

    self.writeRegister(CB, $00);
  end; 

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.regCommWrite(const aStatOrCtrlByteAddr: Word; const aRegNr: Byte; const aWriteValue: Word): TRetVal;
var        
//  SB: Word;
  CB: Word;
  w:  Word;
  dw: LongWord;
  b:  Boolean;

begin
  if (aStatOrCtrlByteAddr >= AddrRng_OutImg_AO_Min) then
  begin
    CB := aStatOrCtrlByteAddr;
//    SB := aStatOrCtrlByteAddr - OutImg_Offset;
  end
  else
  begin
    CB := aStatOrCtrlByteAddr + OutImg_Offset;
//    SB := aStatOrCtrlByteAddr;
  end;

  w := $C0 or (aRegNr and $3F);  // 2 MSBs are set (reg.comm|write) and lower 6 bits holds register number

  // instead of 2 value array
  LongRec(dw).Hi := w;
  LongRec(dw).Lo := aWriteValue;

  try
    b := true;
    if (b) then
    begin
      try
        b := self.fModbusClient.WriteDWord(CB, dw);
      except
        b := false;
      end
    end;
    b := (self.writeRegister(CB, $00) = OK) and b;

    if (b) then
    begin
      result := OK;
    end
    else
    begin
      result := ErrorCommunication;  

      self.writeRegister(CB, $00);
    end;
        
  except
    result := ErrorCommunication;

    try
      self.writeRegister(CB, $00);
    except
    end;
  end;  

  self.fUpdateGapStart := timeGetTime;
end;


function TCommThreadIS1885L.rampProg(const aAddr: Word; const aUstart, aRampTime: Double): TRetVal;
{$ifdef RAMP_UP}
var          
//  b:    Boolean;
  w:    Word;
  lw:   Word;
{$endif}

begin
{$ifdef RAMP_UP}
//  { Program start voltage }
//  w := Trunc(aUstart * cScale16bWriteMult * cUrangeWriteMult);
//  if (w > $7FFF) then w := $7FFF;
//  b := b and (self.regCommWrite(aAddr, RegNr_Ustart, w) = OK);
//  b := b and (self.regCommRead(aAddr, RegNr_Ustart, w) = OK);
//  dw := w;
//
//  { Program ramp time }
//  w := Trunc(aRampTime * 100.0); { 1x s => 10x ms }
//  b := b and (self.regCommWrite(aAddr, RegNr_RampTime, w) = OK);
//  b := b and (self.regCommRead(aAddr, RegNr_RampTime, w) = OK);
//
//  if (b) then
//  begin
//    result := OK;
//  end
//  else
//  begin
//    result := ErrorCommunication;
//  end;
//
//  if (b) then
//  begin
//    result := OK;
//  end
//  else
//  begin
//    result := ErrorCommunication;
//  end;
                           
  result := OK;
  
  { Program start voltage }
  w := Trunc(aUstart * cScale16bWriteMult * cUrangeWriteMult);
  if (w > $7FFF) then w := $7FFF;
  if (result = OK) then result := self.regCommWrite(aAddr, RegNr_Ustart, w);
  if (result = OK) then result := self.regCommRead(aAddr, RegNr_Ustart, w);

  lw := w;  { TODO: Process read values or not?! }

  { Program ramp time }
  w := Trunc(aRampTime * 100.0); { 1x s => 10x ms }
  if (result = OK) then result := self.regCommWrite(aAddr, RegNr_RampTime, w);
  if (result = OK) then result := self.regCommRead(aAddr, RegNr_RampTime, w);
{$endif}
end;


function TCommThreadIS1885L.request(const aAction: TAction; const aAddr, aParam: Word; const aRegNr: Byte): TRetVal;
begin
  if ( (aAction < Low(aAction)) or (aAction > High(aAction)) ) then
  begin
    result := ErrorInvalidAction;
    exit;
  end;

  if (aRegNr > $3F) then
  begin
    result := ErrorInvalidRegNr;
    exit;
  end;

  if (self.fRequests[aAction].fReq) then
  begin
    result := ErrorBusy;
    exit;
  end;

  with self.fRequests[aAction] do
  begin
    fVarAddr  := aAddr;
    fVarRegNr := aRegNr;
    fVarParam := aParam;

    fReq := true; // give request to the thread
  end;

  result := OK;
end;


procedure TCommThreadIS1885L.passCommParams(const aIP: LongWord; const aPortNr: Word);
begin
  with self.fCommParams[rocRequested] do
  begin
    IPAddress := aIP;
    PortNr := aPortNr;
  end;
end;   


procedure TCommThreadIS1885L.passWriteMultiRegsZero(aNumberOfRegs: Byte);
var
  i:  Integer;

begin
  if (aNumberOfRegs > Length(self.fTempReg[rocRequested])) then aNumberOfRegs := Length(self.fTempReg[rocRequested]);
  
  self.fTempRegLength := aNumberOfRegs;
  for i := 0 to aNumberOfRegs - 1 do
  begin
    self.fTempReg[rocRequested][i] := 0;
  end;
end;


procedure TCommThreadIS1885L.passWriteMultiRegs(aNumberOfRegs: Byte; const aArray: array of Word);
var
  i:  Integer;

begin
  if (aNumberOfRegs > Length(aArray)) then aNumberOfRegs := Length(aArray);
  if (aNumberOfRegs > Length(self.fTempReg[rocRequested])) then aNumberOfRegs := Length(self.fTempReg[rocRequested]);

  self.fTempRegLength := aNumberOfRegs;
  for i := 0 to aNumberOfRegs - 1 do
  begin
    self.fTempReg[rocRequested][i] := aArray[i];
  end;
end;    


procedure TCommThreadIS1885L.passI6Params(const aSrc: TTestParams_I6);
begin
  self.fI6Params[rocRequested] := aSrc;
end;       


procedure TCommThreadIS1885L.passH6Params(const aSrc: TTestParams_H6);
begin
  self.fH6Params[rocRequested] := aSrc;
end;


procedure TCommThreadIS1885L.fetchMultiRegs (const aCount: Byte; var aArray: array of Word);
var
  i:  Integer;

begin
  for i := 0 to aCount - 1 do
  begin
    aArray[i] := self.fTempReg[rocCurrent][i];
  end;
end;   


procedure TCommThreadIS1885L.softReset;
//var
//  roc: TReqOrCurr;
//
begin
//  for roc := Low(roc) to High(roc) do
  begin
    self.fTestStatus[tkI6_InsulationTest].ReadyToStart := false;
    self.fTestStatus[tkH6_HighVoltageTest].ReadyToStart := false;
  end;
end;



function TCommThreadIS1885L.waitForResponse(const aAction: TAction; out aRetVal: TRetVal; out aRetWord: Word; const aTimeout: Word): TRetVal;
var
  t:    LongWord;
  main: Boolean;

begin
  if ( (aAction < Low(aAction)) or (aAction > High(aAction)) ) then
  begin
    result := ErrorInvalidAction;
    exit;
  end;

  main := (GetCurrentThreadId = MainThreadID);

  t := timeGetTime + aTimeout;
  while (timeGetTime < t) do
  with self.fRequests[aAction] do
  begin
    if (fReady) then
    begin
      result := OK;
      aRetVal := fRetVal;
      aRetWord := fRetWord;
      exit;
    end;

    sleep(1);
    if (main) then Application.ProcessMessages;
  end;

  result := ErrorNotYetReady;
end;


function TCommThreadIS1885L.ack(const aAction: TAction): TRetVal;
begin  
  if ( (aAction < Low(aAction)) or (aAction > High(aAction)) ) then
  begin
    result := ErrorInvalidAction;
    exit;
  end;

  with self.fRequests[aAction] do
  begin
    if (fReq) then
    begin
//      if (fReady) then
      begin
        fReq := false;
        fReady := false;
        result := OK;
//      end
//      else
//      begin
//        fReq := false;
//        fReady := false;
//        result := ErrorNotYetReady;
      end;
    end
    else
    begin
      result := ErrorNotRequested;
    end;
  end;
end;


function TCommThreadIS1885L.safeInputImageRead (const aAddr: Word; out aReadValue: Word): TRetVal;
begin
  if (not checkAddressRange(aAddr, AddrRng_InImg_Min, AddrRng_InImg_Max)) then
  begin
    result := ErrorInvalidAddress;
    exit;
  end;

  self.fCSInputImg.Enter;
  result := OK;
  aReadValue := self.fInputImage[aAddr];
  self.fCSInputImg.Leave;
end;



function  T_IS1885L.getCalibValue(ValIndex: TCalibrationParam): Double;
begin
  Result := self.fCalibrationData[Ord(ValIndex)];
end;


procedure T_IS1885L.setCalibValue(ValIndex: TCalibrationParam; Value: Double);
begin
  self.fCalibrationData[Ord(ValIndex)] := Value;
end;


function  T_IS1885L.writeCalibValue(ValIndex: TCalibrationParam; Value: Double): TRetVal;
const
  cAction: TAction = actWriteMultiRegs;

var
  dummy:  Word;
  x:      Single;
  aArray: array [0 .. 1] of Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  self.fCalibrationData[Ord(ValIndex)] := Value;

  x := Value;   // implicit Double -> Single conversion
  aArray[0] := LongRec(x).Lo;
  aArray[1] := LongRec(x).Hi;

  TCommThreadIS1885L(self.fThread).passWriteMultiRegs(2, aArray);

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, AddrSpec_Cal_Data+2*Ord(ValIndex), dummy);
end;  


procedure T_IS1885L.clearAllCalibData;
var
  i:  TCalibrationParam;
  x:  Double;

begin
  for i := Low(i) to High(i) do
  begin
    if (i in cCalParFactors) then x := 1.0
    else                          x := 0.0;
    
    self.fCalibrationData[Ord(i)] := x;
  end;
end;


function T_IS1885L.readAllCalibData (aContinueOnError: Boolean = true): TRetVal;
var
  dummy: Single;

begin
  result := self.readCalibValue(HG40I10mAFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40I100uAFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40I1uAFact, dummy);  
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40UFact, dummy);   
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40I10mAOff, dummy);  
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40I100uAOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40I1uAOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(HG40UOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKUNomFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKUNomOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKURealFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKURealOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKIRealFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(BKIRealOff, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(UsenseFact, dummy);
  if ( (result <> OK) and (not aContinueOnError) ) then exit;

  result := self.readCalibValue(UsenseOff, dummy);  
//  if ( (result <> OK) and (not aContinueOnError) ) then exit;
end;  


procedure T_IS1885L.passAllCalibData;
begin
  with TCommThreadIS1885L(self.fThread) do
  begin
    fCalibFactor[cbiNoCalib] := 1.0;
    fCalibOffset[cbiNoCalib] := 0.0;

    fCalibFactor[BKUNom] := self.fCalibrationData[Ord(BKUNomFact)];
    fCalibOffset[BKUNom] := self.fCalibrationData[Ord(BKUNomOff)];
    
    fCalibFactor[BKUReal] := self.fCalibrationData[Ord(BKURealFact)];
    fCalibOffset[BKUReal] := self.fCalibrationData[Ord(BKURealOff)];
    
    fCalibFactor[BKIReal] := self.fCalibrationData[Ord(BKIRealFact)];
    fCalibOffset[BKIReal] := self.fCalibrationData[Ord(BKIRealOff)];

    fCalibFactor[HG40I1uA] := self.fCalibrationData[Ord(HG40I1uAFact)];
    fCalibOffset[HG40I1uA] := self.fCalibrationData[Ord(HG40I1uAOff)];

    fCalibFactor[HG40I100uA] := self.fCalibrationData[Ord(HG40I100uAFact)];
    fCalibOffset[HG40I100uA] := self.fCalibrationData[Ord(HG40I100uAOff)];

    fCalibFactor[HG40I10mA] := self.fCalibrationData[Ord(HG40I10mAFact)];
    fCalibOffset[HG40I10mA] := self.fCalibrationData[Ord(HG40I10mAOff)];

    fCalibFactor[HG40U] := self.fCalibrationData[Ord(HG40UFact)];
    fCalibOffset[HG40U] := self.fCalibrationData[Ord(HG40UOff)];

    fCalibFactor[Usense] := self.fCalibrationData[Ord(UsenseFact)];
    fCalibOffset[Usense] := self.fCalibrationData[Ord(UsenseOff)];
  end;
end;


function  T_IS1885L.readCalibValue (ValIndex: TCalibrationParam; var Value: Single): TRetVal;
const
  cAction: TAction = actReadMultiRegs;

var
  w:  Word;
  aArray: array [0 .. 1] of Word;

begin
  result := self.checkPrologue;
  if (result <> OK) then exit;

  w := 2;

  result := basicThreadAction(self.fThread, cAction, self.fCommTout, AddrSpec_Cal_Data+2*Ord(ValIndex), w);
  if (result <> OK) then exit;

  TCommThreadIS1885L(self.fThread).fetchMultiRegs(2, aArray);
  
  LongRec(Value).Lo := aArray[0];
  LongRec(Value).Hi := aArray[1];

  self.fCalibrationData[Ord(ValIndex)] := Value;  { implicit typecast from Single (NV memory) to Double (RAM) }
end;


INITIALIZATION
  is1885lObj := nil;


end.
