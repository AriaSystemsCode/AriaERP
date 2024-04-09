*:****************************************************************
*: Program file  : HLCHKHD.PRG
*: Program desc. : Cash Application
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/06/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lpShow  , lfvData_1 , lfvData_3
*:               :            : lfvApply, updtherm1 ,lfVendor
*:               :            : lfBalance
*:               -----------------------------------------------
*:               : PROCEDURE  : lpSavScr, lpDelScr ,lpDelScr
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lnRecNo    :- Variable to hold the record Number.
*-- lc_Temp    :- Variable to hold the Invoice Lines Temp File Name.
*-- lcStatus   :- Variable to hold the Record Status.
*-- lcStat     :- Variable to hold the Invoice Status.
*-- lnPaid     :- Variable to hold the Amount paid.
*-- lnUnappl   :- Variable to hold the Unapplied.
*-- End Declaration variables.



EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode

DECLARE laKeyField [1,4],laData[1]


STORE ' ' TO lc_Temp  , lcStatus  , lcStat , lcSelect , lcScFields , lcModal
STORE 0   TO lnRecNo 
STORE .F. TO llUpdApp


laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'VENCHK'
laKeyField[1,4] = 1


IF !gfSetup()
  RETURN
ENDIF  


lcBrFields  = "CCHECKNO  :H='Check No.',"+;
              "DCHECKDAT :H='Check Date',"+;
              "CBOLSTAT   = IIF(CBOLSTAT='O','Open',"+;
              "IIF(CBOLSTAT='P','Paid','Void')) :H= 'Check Status',NAMOUNTP :H='Amount Paid'"

IF !WEXIST(gcBaseWind)
  lcScFields = 'CCHECKNO  , DCHECKDAT , NAMOUNTP  ,CBOLSTAT  ,NAMOUNTUN '
  SELECT HlcHeckh
  SCATTER FIELDS &lcScFields TO laData BLANK
  lc_Temp  = gfTempName()
ENDIF

laDefProc[7] = .F.                       && Use custom Delete procedure  
laDefProc[9] = .F.                       && Use custom Save procedure
 
PUSH KEY
DO (gcScrDir+gcAct_Appl+"\HlchkHd.SPX")
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
*: Date      : 01/06/2002
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow


DO CASE

  CASE laScrMode[1]                   && SELECT Mode
    STORE '' TO ladata[1] , lcStat
    STORE 0 TO ladata[3] , ladata[5]
    STORE {} TO ladata[2]
    SHOW GET lcStat
    SHOW GET laData[1] ENABLE
    _CUROBJ = OBJNUM(laData[1])
    
  CASE laScrMode[2]                   && VIEW Mode
    DO CASE 
      CASE laData[4] = 'O'
        lcStat = 'Open'
      CASE laData[4] = 'P'
        lcStat = 'Paid'
      CASE laData[4] = 'V'
        lcStat = 'Void'
    ENDCASE
    SHOW GET lcStat
    SHOW GET pbApply ENABLE

  CASE laScrMode[4]  && ADD  Mode
    SHOW GET laData[1] DISABLE  
    SHOW GET laData[3] ENABLE
    laData[4] = 'O'
    lcStat    = 'Open'
    SHOW GET lcStat
    SHOW GET pbApply   ENABLE
  
  CASE laScrMode[3]
    SHOW GET laData[1] DISABLE
    lcStat = IIF(laData[4]='O','Open',IIF(laData[4]='P','Paid','Void'))
    SHOW GET laData[3] ENABLE
    SHOW GET lcStat
    SHOW GET pbApply   ENABLE
    llUpdApp  = .F.
ENDCASE    

IF !laScrMode[1]
  IF laData[4] = 'V'
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

*-- End Of lpShow
*:*************************************************************
*: Name      : lfvData_1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
*: Purpose   : Function to Validate laData[1]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_1()
*:*************************************************************
*:
FUNCTION lfvData_1
PRIVATE lnAlias , lcExact
 
lnAlias = SELECT (0)

IF !EMPTY(ALLTRIM(laData[1])) AND LASTKEY()= 13
  SELECT hlcheckh
  lcExact = SET('EXACT')
  SET EXACT ON
  = SEEK (PADR(laData[1],20))
  SET EXACT &lcExact
  IF FOUND()
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode=.F.
    laScrMode[2]=.T.
    SHOW GETS 
  ELSE
    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Add;\<Browse;\?\<Reenter
    *- button Number  :- 42003.
    lnOption = gfModalGen('INM00001B42003','DIALOG',ALLTRIM(laData[1]))
    DO CASE

      CASE lnOption = 1
        laScrMode   = .F.
        laScrMode[4]= .T.
        SHOW GETS
      CASE lnOption = 2
        = gfBrows()
      CASE lnOption = 3
        STORE '' TO laData[1]
        laScrMode = .F.
        laScrMode[1] = .T.
        SHOW GETS
    ENDCASE  
  ENDIF  
ENDIF
  
SELECT (lnAlias)

*-- End OF lfvData_1.
*:*************************************************************
*: Name      : lfvData_3
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
*: Purpose   : Function to Validate laData[3]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_3()
*:*************************************************************
*:
FUNCTION lfvData_3

IF laScrMode[4]
  laData[5] = laData[3]
ENDIF  
SHOW GET laData[5]
SHOW GET laData[3]

*-- End Of lfvData_3.
*:*************************************************************
*: Name      : lfvApply
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
*: Purpose   : Call Apply Invoices Screen for payment.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvApply()
*:*************************************************************
*:
FUNCTION lfvApply

lnPaid    = 0
lnUnappl  = laData[5]

DO CASE
  CASE laScrMode[2]
    SELECT HLCHECKH.CCHECKNO,HLCHECKL.CTSUVENCD,;
           HLCHECKL.CINVTYPE,HLCHECKL.CCINVNO,;
           HLCHECKL.NCINTVAL,HLCHECKL.NAMOUNTP,;
           VAL(STR((HLCINVHD.NCINTVAL-HLCINVHD.NAMOUNTP),12,2));
           AS 'nBalance',;
           lfVendor(HLCHECKL.CTSUVENCD) AS 'cVendor',;
           RECNO() AS 'nRecNo','S' AS 'cStatus',;
           'T' AS 'lModifi';
      FROM &gcDataDir.HLCHECKH,&gcDataDir.HLCHECKL,;
           &gcDataDir.HLCINVHD,&gcDataDir.CODES;
     WHERE HLCHECKL.CCHECKNO = HLCHECKH.CCHECKNO;
       AND (HLCHECKL.CTSUVENCD+HLCHECKL.CINVTYPE+HLCHECKL.CCINVNO=;
           HLCINVHD.CTSUVENCD+HLCINVHD.CINVTYPE+HLCINVHD.CCINVNO;
       AND HLCHECKH.CCHECKNO = laData[1]);
     GROUP BY HLCHECKH.CCHECKNO,HLCHECKL.CTSUVENCD,;
           HLCHECKL.CINVTYPE,HLCHECKL.CCINVNO;
      INTO DBF &gcWorkDir.&lc_Temp
      
    SELECT (lc_Temp)
    llUpdApp = .F.

  CASE laScrMode[3]
    IF llUpdApp = .F.
      SELECT Hlcinvhd
      SET ORDER TO TAG Veninv 
      lcIndExp1=SYS(14,VAL(SYS(21)))
      lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
      SELECT hlcheckl
      
      lcSelect= "PADR(laData[1],20) AS 'ccheckno',"+;
                "HLCINVHD.CTSUVENCD,HLCINVHD.CINVTYPE,"+;
                "HLCINVHD.CCINVNO,HLCINVHD.NCINTVAL,"+;
                "000000000.00 AS 'NAMOUNTP',"+;
                "HLCINVHD.NCINTVAL-HLCINVHD.NAMOUNTP "+;
                "AS 'nBalance',lfVendor(HLCINVHD.CTSUVENCD)"+;
                " AS 'cVendor',0 AS 'nRecNo','A' AS 'cStatus',"+;
                "'F' AS 'lModifi'"
             
      SELECT &lcSelect;
        FROM &gcDataDir.HLCINVHD,&gcDataDir.CODES;
       WHERE hlcinvhd.namountp = 0 ;
          OR (hlcinvhd.ncintval > hlcinvhd.namountp);
       GROUP BY CCHECKNO,CTSUVENCD,CINVTYPE,CCINVNO;
       UNION SELECT HLCHECKH.CCHECKNO,HLCHECKL.CTSUVENCD,;
             HLCHECKL.CINVTYPE,HLCHECKL.CCINVNO,;
             HLCHECKL.NCINTVAL,HLCHECKL.NAMOUNTP,;
             HLCINVHD.NCINTVAL-HLCINVHD.NAMOUNTP AS 'nBalance',;
             lfVendor(HLCHECKL.CTSUVENCD) AS 'cVendor',;
             RECNO() AS 'nRecNo','S' AS 'cStatus',;
             'T' AS 'lModifi';
        FROM &gcDataDir.HLCHECKH,&gcDataDir.HLCHECKL,;
             &gcDataDir.HLCINVHD,&gcDataDir.CODES;
       WHERE HLCHECKL.CCHECKNO=HLCHECKH.CCHECKNO;
         AND (HLCHECKL.CTSUVENCD+HLCHECKL.CINVTYPE+HLCHECKL.CCINVNO=;
             HLCINVHD.CTSUVENCD+HLCINVHD.CINVTYPE+HLCINVHD.CCINVNO;
         AND HLCHECKH.CCHECKNO=laData[1]);
        INTO DBF &gcWorkDir.&lc_Temp
    
      SELECT (lc_Temp)
      INDEX ON &lcIndExp1.&lcTagExp1
    ELSE
      SELECT (lc_Temp) 
    ENDIF
      
  CASE laScrMode[4]

    IF llUpdApp = .F.
      SELECT Hlcinvhd
      SET ORDER TO TAG Veninv 
      lcIndExp1=SYS(14,VAL(SYS(21)))
      lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
      SELECT hlcheckl

      SELECT PADR(laData[1],20) AS 'ccheckno',;
             HLCINVHD.CTSUVENCD,HLCINVHD.CINVTYPE,;
             HLCINVHD.CCINVNO,HLCINVHD.NCINTVAL,;
             000000000.00 AS 'NAMOUNTP',;
             VAL(STR((HLCINVHD.NCINTVAL-HLCINVHD.NAMOUNTP),12,2));
              AS 'nBalance',;
             lfVendor(HLCINVHD.CTSUVENCD) AS 'cVendor',;
             0 AS 'nRecNo','A' AS 'cStatus',;
             'F' AS 'lModifi';
        FROM &gcDataDir.HLCINVHD,&gcDataDir.CODES;
       WHERE hlcinvhd.namountp = 0 ;
          OR (hlcinvhd.ncintval <> hlcinvhd.namountp);
       GROUP BY CCHECKNO,CTSUVENCD,CINVTYPE,CCINVNO;
        INTO DBF &gcWorkDir.&lc_Temp
      SELECT (lc_Temp)
      INDEX ON &lcIndExp1.&lcTagExp1
    ELSE
      SELECT (lc_Temp)
    ENDIF  
ENDCASE  

	
*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlchkln.PRG")


IF llUpdApp
  laData[5] = lnUnappl
  SHOW GET laData[5]
ENDIF

SELECT hlCheckh

*-- End OF lfvApply.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
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

DO CASE 
  CASE laData[5] = 0
    laData[4] = 'P'
  CASE laData[5] > 0
    laData[4] = 'O'
ENDCASE

GATHER FROM laData FIELDS &lcScFields MEMO

=gfAdd_Info()


IF USED('&lc_Temp')
  SELECT (lc_Temp)
  COUNT FOR cCheckNo = ALLTRIM(laData[1]) TO lnRecords
  lcArea = SELECT()  
  lnRecNo = 1
  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = 0

  
  SCAN 
    *-- Count all the records to be processed to be used in the 
    *-- Thermometer.
    lnCurLine = lnCurLine + 1
    *--Thermometers.
    = gfThermo(lnRecords,lnCurLine,"Updating checks file.",lcArea))

    IF SEEK(ctSuvenCd+cinvtype+ccinvno,'Hlcinvhd')
      REPLACE Hlcinvhd.namountp WITH (&lc_Temp..NCINTVAL-&lc_Temp..nBalance)
    ENDIF
    SELECT (lc_Temp)
  ENDSCAN  
  
  IF WEXIST('gwdThermo')
    = gfThermo(lnRecords,lnRecords,"Updating checks file.","")
  ENDIF


  SELECT (lc_Temp)
  SCAN 
    IF nAmountp = 0
      REPLACE cstatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)  && DELETE
      DELETE
    ENDIF
  ENDSCAN  

  IF llUpdApp
    =lFTMP2MAST('hlCheckl',lc_Temp)
    llUpdApp = .F.
  ENDIF

  SELECT hlCheckl
  SCAN 
    IF nAmountp = 0
      DELETE
    ENDIF
  ENDSCAN  
ENDIF

SELECT hlCheckh

*-- End Of lpSavScr
*:*************************************************************
*: Name      : lpDelScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
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


IF laData[3] = laData[5]
  SELECT hlCheckh
  laData[4] = 'V'
  GATHER FROM laData FIELDS &lcScFields MEMO
  =gfAdd_Info()
  laScrMode    = .F.
  laScrMode[1] = .T.
ELSE
  *- Message Text   :- Invoice(s) paid by this check. Cannot delete.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','Invoice(s) paid by this check. Cannot delete.')
  laScrMode    = .F.
  laScrMode[2] = .T.
ENDIF  

*-- End Of lpDelScr
*:*************************************************************
*: Name      : lfVendor
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
*: Purpose   : Valid The vendor
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfVendor ()
*:*************************************************************
*:
FUNCTION lfVendor
PARAMETER lcVendor

RETURN gfCodDes(lcVendor,'CTSUVENCD ')
*RETURN ALLTRIM(LOOKUP(CODES.CDISCREP,lcVendor,CODES.CCODE_NO,"CODES"))

*-- End Of lfVendor
*:*************************************************************
*: Name      : lfBalance
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/06/2002
*: Purpose   : Call Invoices Screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfBalance()
*:*************************************************************
*:
FUNCTION lfBalance

RETURN VAL(STR((HLCINVHD.NCINTVAL-HLCINVHD.NAMOUNTP),12,2))

*-- End OF lfBalance.
*:*************************************************************
*: Name      : lfTmp2Mast
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
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
