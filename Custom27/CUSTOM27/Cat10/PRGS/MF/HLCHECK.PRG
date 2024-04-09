*:****************************************************************
*: Program file  : HLCHECK.PRG
*: Program desc. : Cash Application.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/10/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvData_1 , lfvData_2 , lfvData_4
*:               :            : lfvApply  , lfUpdTemp , lfUpdApp
*:               :            : lfTmp2Mast
*:               -----------------------------------------------
*:               : PROCEDURE  : lpShow    , lpSavScr  , lpDelScr
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- llUpdApp   :- Hold true if we update the apply screen.
*-- laVendCod  :- Array to hold the description & codes for Vendor.
*-- lnRecNo    :- Variable to hold the record Number.
*-- lnTotBal   :- Variable to hold the total balance.
*-- lc_Temp    :- Variable to hold the Invoice Lines Temp File Name.
*-- lcStatus   :- Variable to hold the Record Status.
*-- lcStat     :- Variable to hold the Invoice Status.
*-- End Declaration variables.

EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode
DECLARE laKeyField [2,4] , laData[1] , laVendCod[1,2] , laCodInfo [1,10]
STORE .F. TO llFirstApp  , llUpdApp 
STORE 0   TO lnRecNo     , lnRecNo , lnTotBal
STORE ''  TO lc_Temp     , lcStatus, lcStat , laCodInfo ,;
             lcModal , laVendCod , lcScFields
STORE 1   TO lnVendCod  

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'VENCHK'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'VENCHK'
laKeyField[2,4] = 2


IF !gfSetup()
  RETURN
ENDIF  
 
laCodInfo[1,01] = "CTSUVENCD"
laCodInfo[1,02] = "laVendCod"
laCodInfo[1,03] = "lnVendCod"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,10] = "cTsuVenCd"

IF !WEXIST(gcBaseWind)
  lcScFields = 'CTSUVENCD  , CCHECKNO  , dchkdate , NAMOUNTP  , NAMOUNTUN ,'+;
               'CBOLSTAT  '
  SELECT HLCHECKS
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  lc_Temp  = gfTempName()
ENDIF

laDefProc[7] = .F.                       && Use custom Delete procedure  
laDefProc[9] = .F.                       && Use custom Save procedure
 

PUSH KEY
DO (gcScrDir+gcAct_Appl+"\Hlcheck.SPX")
POP KEY


IF glQuitting 
  IF USED('&lc_Temp')
    USE IN &lc_Temp
    ERASE &gcWorkDir.&lc_Temp+'.DBF'
    ERASE &gcWorkDir.&lc_Temp+'.CDX'
    ERASE &gcWorkDir.&lc_Temp+'.FPT'
  ENDIF  
ENDIF

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/10/2002
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow

EXTERNAL ARRAY laScrMode,laGrmntDis

DO CASE

  CASE laScrMode[1]                   && VIEW Mode
    
    IF EMPTY(ALLTRIM(ladata[1]))
      SHOW GET pbApply   DISABLE
    ELSE
      SHOW GET pbApply ENABLE
    ENDIF
    lcStat = ''
    lnVendCod = 1
    lnVendCod = ASCAN(laVendCod,laData[1])/2
    
    SHOW GET laData[2] ENABLE
    SHOW GET lnVendcod ENABLE
    SHOW GET lcStat
    _CUROBJ = OBJNUM(lnVendCod)

    
    
  CASE laScrMode[2]                   && SELECT Mode
    lnVendCod = ASCAN(laVendCod,laData[1])/2
    SHOW GET lnVendCod DISABLE
    DO CASE 
      CASE laData[6] = 'O'
        lcStat = 'Open'
      CASE laData[6] = 'P'
        lcStat = 'Paid'
      CASE laData[6] = 'V'
        lcStat = 'Void'
    ENDCASE

    
    SHOW GET lcStat

  CASE laScrMode[4] .OR. laScrMode[3] && ADD OR EDIT Mode
    SHOW GET laData[2] DISABLE
    SHOW GET lnVendcod DISABLE
  
    SHOW GET laData[3] ENABLE
    SHOW GET laData[4] ENABLE
    SHOW GET pbApply   ENABLE
    SHOW GET laData[5]
    IF laScrMode[4]
      laData[6] = 'O'
      lcStat    = 'Open'
    ENDIF
    SHOW GET lcStat
ENDCASE    

IF !laScrMode[1]
  IF laData[6] = 'V'
    lcStat     = 'Void'  
    SHOW GET pbEdt DISABLE
    SHOW GET pbDlt DISABLE
  ELSE
    IF laScrMode[2]
      SHOW GET pbEdt ENABLE
      SHOW GET pbDlt ENABLE
    ENDIF  
  ENDIF
ENDIF  
SHOW GET lcStat

*-- End OF lpShow.
*:*************************************************************
*: Name      : lfvData_1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Function to Validate laData[1]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_1 ()
*:*************************************************************
*:
FUNCTION lfvData_1

IF lnVendCod <> 0 .AND. !( PADR(ALLTRIM(laData[1]),6) == PADR(ALLTRIM(laVendCod[lnVendCod,2]),6))
  laData[1] = PADR(ALLTRIM(laVendCod[lnVendCod,2]),6)
ENDIF

*-- End OF lfvData_1.
*:*************************************************************
*: Name      : lfvData_2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Function to Validate laData[2]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_2 ()
*:*************************************************************
*:
FUNCTION lfvData_2
Private lnAlias 

lnAlias  = SELECT (0)
IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) AND LASTKEY()= 13
  SELECT hlChecks
  lcExact = SET('EXACT')
  SET EXACT ON
  = SEEK (laData[1]+ALLTRIM(laData[2]))
  SET EXACT &lcExact
  IF FOUND()
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode=.F.
    laScrMode[2]=.T.
    llFirstCon  =.T.
    SHOW GETS 
  ELSE
    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Add;\<Browse;\?\<Reenter
    *- button Number  :- 42003.
    lcMessage = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))+' '+;
                ALLTRIM(laData[2])
    lnOption = gfModalGen('INM00001B42003','DIALOG',lcMessage )    
    DO CASE
      CASE lnOption = 1
        laScrMode   = .F.
        laScrMode[4]= .T.
        llFirstCon  =.T.
        SHOW GETS
      CASE lnOption = 2
        = gfBrows()
        llFirstCon  = .T.        
      CASE lnOption = 3
        STORE '' TO ladata[1] , laData[2] , laData[6]
        STORE 0  TO laData[4] , laData[5]
        
        laScrMode   = .F.
        laScrMode[1]= .T.
        SHOW GETS
    ENDCASE
  ENDIF  
ENDIF
SELECT (lnAlias)

*-- End OF  lfvData_2.
*:*************************************************************
*: Name      : lfvData_4
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Function to Validate laData[4]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_4 ()
*:*************************************************************
*:
FUNCTION lfvData_4

IF laScrMode[4]
  laData[5] = laData[4]
ENDIF
SHOW GET laData[5]

*-- End of lfvData_4.
*:*************************************************************
*: Name      : lfvApply
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Function to call the Apply Screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvApply ()
*:*************************************************************
*:
FUNCTION lfvApply

lnPaid    = 0          && Variable to hold the Amount paid
lnUnappl  = laData[5]  && Variable to hold the Unapplied

IF llFirstApp .OR. laScrMode[2] .OR. llUpdApp = .F. 
  SELECT HLCPAYMN  
  SET ORDER TO TAG VENCHK
  lcIndExp1=SYS(14,VAL(SYS(21)))
  lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  SELECT PADR(laData[1],6) AS 'ctsuvencd',;
         PADR(laData[2],20) AS 'ccheckno',;
         hlcBol.cBolNo ,;
         000000000.00 AS 'namountp',;
         Hlcbol.nbolamont AS 'nbolAmnt',;
         HLCbOL.NBALANCE AS 'nVenBal',;
         0 AS 'nRecNo',;
         'A' AS 'cStatus',;
         'F'AS 'lModifi' ;
    FROM &gcDataDir.HlcBol;
   WHERE Hlcbol.ctsuvencd=laData[1];
     AND HlcBol.cBolStat='O';
   GROUP BY ctsuvencd,ccheckno,cbolno;
   UNION SELECT Hlcpaymn.ctsuvencd,Hlcpaymn.ccheckno,;
         Hlcpaymn.cbolno,Hlcpaymn.namountp,;
         Hlcbol.nbolamont AS 'nbolAmnt',;
         HLCbOL.NBALANCE AS 'nVenBal',;
         RECNO() AS 'nRecNo','S' AS 'cStatus',;
         'T' AS 'lModifi';
    FROM &gcDataDir.Hlcpaymn,&gcDataDir.Hlcbol;
   WHERE Hlcbol.ctsuvencd+Hlcbol.cbolno=;
         Hlcpaymn.ctsuvencd+Hlcpaymn.cbolno;
     AND Hlcpaymn.ctsuvencd=ladata[1];
     AND Hlcpaymn.ccheckno=ladata[2];
   ORDER BY 1,2,3,4 DESC;
    INTO DBF &gcWorkDir.&lc_Temp

  = lfUpdTemp()
  = lfUpdApp ()

  SELECT (lc_Temp)
  INDEX ON &lcIndExp1.&lcTagExp1
ELSE
  SELECT (lc_Temp)
ENDIF


*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlcApply.PRG")

IF llUpdApp
  laData[5] = lnUnappl
  SHOW GET laData[5]
ENDIF

llFirstApp = .F.

SELECT hlChecks

*-- End OF lfvApply.
*:*************************************************************
*: Name      : lfUpdTemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Funtion to update the Temp Files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfUpdTemp ()
*:*************************************************************
*:
FUNCTION lfUpdTemp

lcVenCode =' '
lcCheckNo =' '
lcBolNo   =' '
lnBolAmnt = 0
lnVenBal  = 0

SELECT (lc_Temp)

COUNT FOR ctsuvencd = laData[1] TO lnRecords
lnRecNo = 1
GO TOP

lcArea = SELECT()  
lnRecNo = 1
*-- Count all the records to be processed to be used in the 
*-- Thermometer.
lnCurLine = 0

ITEM_key = lcVenCode+lcCheckNo+lcBolNo+STR(lnBolAmnt)+STR(lnVenBal)
SCAN
  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = lnCurLine + 1
  *--Thermometers.
  = gfThermo(lnRecords,lnCurLine,"Selecting..",lcArea))

  IF ITEM_KEY <> ctsuvencd+ccheckno+cbolno+STR(nbolAmnt)+STR(nVenBal)
     ITEM_key = ctsuvencd+ccheckno+cbolno+STR(nbolAmnt)+STR(nVenBal)
  ELSE   
    DELETE 
  ENDIF   
ENDSCAN

IF WEXIST('gwdThermo')
  = gfThermo(lnRecords,lnRecords,"Selecting..","")
ENDIF

*-- End Of lfUpdTemp
*:*************************************************************
*: Name      : lfUpdApp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Function to Update the Bol File.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfUpdApp ()
*:*************************************************************
*:
FUNCTION lfUpdApp

lnTotBal = 0

SELECT hlcBOL
= SEEK (laData[1])
SCAN WHILE CTSUVENCD = laData[1]
  IF CbolStat <> 'V'
    lnTotBal = lnTotBal+nBalance          
  ENDIF  
ENDSCAN  

*-- End OF lfUpdApp.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : Save The ladata into Base file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lpSavScr
*:*************************************************************
*:
* This procedure replaces the default Save procedure for the push button
* "SAVE"
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode    

IF laScrMode[4]             && Add mode
   APPEND BLANK
ENDIF


GATHER FROM laData FIELDS &lcScFields MEMO
*-- Update the Status
REPLACE cBolStat WITH IIF(cBolStat = 'V',cBolStat,IIF(NAMOUNTUN=0,'P','O'))

=gfAdd_Info()

IF USED('&lc_Temp')
  SELECT (lc_Temp)

  COUNT FOR ctsuvencd = laData[1] TO lnRecords

  lcArea = SELECT()  
  lnRecNo = 1
  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = 0

  = SEEK (laData[1])
  SCAN REST WHILE ctSuvenCd = laData[1]
    *-- Count all the records to be processed to be used in the 
    *-- Thermometer.
    lnCurLine = lnCurLine + 1
    *--Thermometers.
    = gfThermo(lnRecords,lnCurLine,"Updating checks file.",lcArea))
    
    IF SEEK(ctSuvenCd+cBolNo,'hlcBOL')
      IF hlcBOL.cBolStat = 'O' .OR. hlcBOL.cBolStat = 'P'
        REPLACE hlcBOL.nBalance WITH nVenBal
        REPLACE hlcBOL.cBolStat WITH IIF(nVenBal=0,'P','O')
      ENDIF
    ENDIF
  ENDSCAN  

  IF WEXIST('gwdThermo')
    = gfThermo(lnRecords,lnRecords,"Updating checks file.","")
  ENDIF

  SELECT (lc_Temp)
  GO TOP
  SCAN 
    IF nAmountp = 0
      REPLACE cstatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)  && DELETE
      DELETE
    ENDIF
  ENDSCAN  

  IF llUpdApp
    =lFTMP2MAST('hlCPaymn',lc_Temp)
    llUpdApp = .F.
  ENDIF

  SELECT hlCPaymn
  GO TOP
   SCAN 
    IF nAmountp = 0
      DELETE
    ENDIF
  ENDSCAN  
ENDIF

SELECT hlChecks

*-- End OF lpSavScr.
*:*************************************************************
*: Name      : lpDelScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/10/2002
*: Purpose   : delete The ladata into Base file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lpDelScr
*:*************************************************************
*:
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr

EXTERNAL ARRAY laScrMode

IF laData[4] = laData[5]
  SELECT hlChecks
  laData[6] = 'V'
  GATHER FROM laData FIELDS &lcScFields MEMO
  =gfAdd_Info()
  laScrMode    = .F.
  laScrMode[1] = .T.
ELSE
   *-- XXXXX 's paid by this check. Cannot delete 
  =gfModalGen('INM213B000','ALERT','BOL')
  *- Message Text   :- XXXXX 's paid by this check. Cannot delete.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','Bol(s) paid by this check. Cannot delete ')
  laScrMode     = .F.
  laScrMode[2]  = .T.
ENDIF  

*-- End OF lpDelScr.
*:*************************************************************
*: Name      : lfTmp2Mast
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/10/2002
*: Purpose   : Update Or Add records from temp File to master file 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*:
*: Function to update records of master file from temp file 
*: Temp file has to have 2 additional fields nRecNo and cStatus to hold
*: the phisical records no and the action status done to this record
*:
FUNCTION lfTmp2Mast

PARAMETERS lcMastFile,lcTempFile

PRIVATE    lcMastFile,lcTempFile,lcSavAlias

lcSavAlias = SELECT(0)
lcSourcDir = IIF (UPPER(LEFT(lcMastFile,2))="SY",gcSyshome,gcDatadir)

lcSaveDel = SET ('DELETE')
SET DELETE OFF

SELECT (lcTempFile)
*** Scan through all the Added,Modified or Deleted records
SCAN FOR cStatus <> 'S'
  DO CASE 
    *** New added record   
    CASE cStatus = 'A'                    
      SCATTER MEMVAR MEMO
      SELECT  (lcMastFile)  
      IF SEEK(' ')                        && Chek if there is empty 
        RECALL                            && Deleted records to recall
        GATHER MEMVAR MEMO
      ELSE  
        INSERT INTO &lcSourcDir.&lcMastFile FROM MEMVAR
      ENDIF  
      =gfAdd_Info()
    *** Record was modified
    CASE cStatus = 'M'                    
      SCATTER MEMVAR MEMO                 && Collect data from temp
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo
      GATHER  MEMVAR MEMO                 && Replace master data 

    *** Record was deleted
    CASE DELETED()
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo

      SCATTER MEMVAR MEMO BLANK           && Empty the record befor
      GATHER  MEMVAR MEMO                 && delete it

      DELETE                              && Delete recored not in temp
  ENDCASE

  SELECT  (lcTempFile)
  REPLACE cStatus WITH "S"
ENDSCAN  

SET DELETE &lcSaveDel
SELECT (lcSavAlias)

*-- End Of lfTmp2Mast.
*:*************************************************************

