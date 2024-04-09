*:****************************************************************
*: Program file  : HLBOL.PRG
*: Program desc. : Bill of Lading.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/09/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvData_1, lfvData_2  , lfvData_6
*:               :            : lfvCont  , lfTmp2Mast.
*:               -----------------------------------------------
*:               : PROCEDURE  : lpShow  , lpSavScr , lpDelScr.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- laVendCod :- Array to hold the description & codes for Vendor.
*-- lcStatus  :- Variable to hold the record status in the temp file.
*-- lnLinNo   :- Variable to hold the line no.
*-- lnBalance :- Variable to hold the balance.
*-- lnOldTot  :- Variable to hold the old total. 
*-- llFirstCon:- hold true if we call the  Containers screen.
*-- llUpdCon  :- If we will update the contener table.
*-- laCodInfo :- Array hold the vendor code
*-- End Declaration variables.

EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode
DECLARE laKeyField [2,4],laData[1] , laCodInfo [1,10] ,laVendCod[1,2]

STORE ''  TO lcStatus ,laCodInfo , lcModal , laVendCod , lcScFields
STORE 0   TO lnLinNo , lnBalance , lnOldTot
STORE .F. To llFirstCon , llUpdCon
STORE 1   TO lnVendCod
 
laKeyField[1,1] = 'lnVendCod'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'VENBOL'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'VENBOL'
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
  SELECT HLCBOL
  lcScFields = 'CTSUVENCD , CBOLNO   , CBOLSTAT   , DSAILDATE ,DDUEDATE  ,'+;
               'NBOLAMONT , NBALANCE '
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  lc_Temp  = gfTempName()
  =gfwCodePop ( @laCodInfo, "CTSUVENCD" ,"N" )
ENDIF

laDefProc[7] = .F.                       && Use custom Delete procedure  
laDefProc[9] = .F.                       && Use custom Save procedure
 

PUSH KEY
DO (gcScrDir+gcAct_Appl+"\hlBol.SPX")
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
*: Date      : 01/09/2002
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
  CASE laScrMode[1]                   && SELECT Mode
    lnVendCod = 1
    lnVendCod = ASCAN(laVendCod,laData[1])/2
    SHOW GET lnVendcod  ENABLE
    SHOW GET laData[2]  ENABLE
    STORE SPACE(1)  TO lcStatus 
    _CUROBJ = OBJNUM(lnVendCod)
  CASE laScrMode[2]                   && VIEW Mode
    lnVendCod = ASCAN(laVendCod,laData[1])/2
    SHOW GET lnVendCod DISABLE
    
    SHOW GET pbCont ENABLE
    SHOW GET pbEdt  ENABLE
    SHOW GET pbDlt  ENABLE


  CASE laScrMode[4] .OR. laScrMode[3] && ADD OR EDIT Mode

    SHOW GET lnVendcod  DISABLE
    SHOW GET laData[2]  DISABLE
  
    SHOW GET laData[4] ENABLE
    SHOW GET laData[5] ENABLE
    SHOW GET laData[6] ENABLE
    SHOW GET pbCont    ENABLE
    SHOW GET laData[7]
ENDCASE    

IF !laScrMode[1]       && Select Mode
  DO CASE
    CASE laData[3] = 'O'   
      lcStatus = 'Open'
      IF laScrMode[2]
        SHOW GET pbEdt  ENABLE
        SHOW GET pbDlt  ENABLE
      ENDIF  
    CASE laData[3] = 'V'
      lcStatus = 'Void'  
      SHOW GET pbEdt  DISABLE
      SHOW GET pbDlt  DISABLE
    CASE laData[3] = 'P'
      lcStatus = 'Paid'  
      SHOW GET pbEdt  DISABLE
      SHOW GET pbDlt  DISABLE
    OTHERWISE
      lcStatus = ''  
  ENDCASE
ENDIF  
SHOW GET lcStatus

IF laScrMode[3]
  lnOldTot = 0  
  lnOldTot = laData[6]
ENDIF

*-- End Of lpShow.
*:*************************************************************
*: Name      : lfvData_1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
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

IF lnVendCod<> 0 .AND. !( PADR(ALLTRIM(laData[1]),6) == PADR(ALLTRIM(laVendCod[lnVendCod,2]),6))
  laData[1] = PADR(ALLTRIM(laVendCod[lnVendCod,2]),6)
ENDIF

*-- End OF lfvData_1.
*:*************************************************************
*: Name      : lfvData_2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
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
  SELECT HLCBOL
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
        laData[3]   = 'O'
        laData[4]   = DATE()
        laData[5]   = DATE()
        llFirstCon  =.T.
        SHOW GETS
      CASE lnOption = 2
        = gfBrows()
        llFirstCon  = .T.        
      CASE lnOption = 3
        STORE '' TO ladata[1] , laData[2]      
        laScrMode   = .F.
        laScrMode[1]= .T.
        SHOW GETS
    ENDCASE
  ENDIF  
ENDIF
SELECT (lnAlias)

*-- End OF  lfvData_2.
*:*************************************************************
*: Name      : lfvData_6
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Function to Validate laData[6]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_6 ()
*:*************************************************************
*:
FUNCTION lfvData_6

IF laScrMode[4]
  laData[7] = laData[6]
ELSE
  IF laData[6] <>  lnOldTot
    IF laData[6] > lnOldTot
      laData[7] = laData[7] + (laData[6]-lnOldTot)
    ELSE
      IF lnOldTot-laData[6] > laData[7]
        =gfModalGen('INM212B000','ALERT',STR(lnOldTot-laData[7],10,2))
        laData[6] = lnOldTot
        _CUROBJ = OBJNUM(laData[6])
        SHOW GET laData[6]
      ENDIF
    ENDIF
  ENDIF
ENDIF

SHOW GET laData[7]

*-- End Of lfvData_6.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
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

=gfAdd_Info()

IF llUpdCon
  =lFTMP2MAST('HLCONTNR',lc_Temp)
  llUpdCon = .F.
ENDIF

SELECT hlCBOL

*-- End Of lpSavScr.
*:*************************************************************
*: Name      : lpDelScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
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

lcCheckNo = ''
lnPaid    = 0

IF laData[7] = laData[6]
  SELECT hlCBOL
  laData[3] = 'V'
  GATHER FROM laData FIELDS &lcScFields MEMO
  =gfAdd_Info()
  laScrMode    = .F.
  laScrMode[1] = .T.
ELSE
  *- Message Text   :- XXX is Already paid for this BOL. Please Update your Cheks first
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ',STR(lnOldTot-laData[7],10,2) +' is Already paid for this BOL. Please Update your Cheks first')
  laScrMode     = .F.
  laScrMode[2]  = .T.
ENDIF

*-- End Of lpDelScr.
*:*************************************************************
*: Name      : lfvCont
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Function to Validate laData[6]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCont ()
*:*************************************************************
*:
FUNCTION lfvCont

IF llFirstCon .OR. laScrMode[2] .OR. llUpdCon = .F.

  SELECT HLCONTNR  
  SET ORDER TO TAG VENBOL
  lcIndExp1=SYS(14,VAL(SYS(21)))
  lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus';
    FROM &gcDataDir.HLCONTNR;
    WHERE HLCONTNR.CTSUVENCD = laData[1];
    AND HLCONTNR.CBOLNO      = laData[2];
    INTO DBF &gcWorkDir.&lc_Temp

  SELECT (lc_Temp)
  INDEX ON &lcIndExp1.&lcTagExp1
  COUNT FOR CTSUVENCD <> CHR(255) .AND. !DELETED() TO lnLinNo
ELSE
  SELECT (lc_Temp)
  COUNT FOR CTSUVENCD <> CHR(255) .AND. !DELETED() TO lnLinNo
ENDIF

*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlContan.PRG")

llFirstCon = .F.

SELECT hlCBOL

*-- End OF lfvCont.
*:*************************************************************
*: Name      : lfTmp2Mast
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
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
