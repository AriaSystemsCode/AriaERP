*:****************************************************************
*: Program file  : HLCINHD.PRG
*: Program desc. : Commercial Invoice Header.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/06/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvVend   , lfvInvType, lfvData_2
*:               :            : lfvDetail , lfvMef    , lfUpdMef
*:               :            : lfTmp2Mast, 
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
*-- llFirstInv  :- Variable Hold True or Faluse if we call Invoive Screen.
*-- llFirstMef  :- Variable Hold True or Faluse if we call Mef Screen.
*-- llUpdInv    :- Variable Hold True or Faluse if we will update Invoice.
*-- llUpdMef    :- Variable Hold True or Faluse if we will update Mef.
*-- llMessage   :- Variable for the thermo message.
*-- lnTotUnit   :- Variable to hold the total units.
*-- lnTotDoz    :- Variable to hold the total dozen
*-- lnTotVal    :- Variable to hold the total value.
*-- lcTempInv   :- Variable to hold the Invoice Lines Temp File Name.
*-- lcTempMef   :- Variable to hold the MEF Temp Nile Name.
*-- lcVendDis   :- Variable to hold the Vendor name.
*-- lcStyNo     :- Variable to hold the Style No.
*-- lcQuata     :- Variable to hold the Quota Category.
*-- lcChief     :- Variable to hold the Chief Code.
*-- lcConst     :- Variable to hold the Construction Code.
*-- lcGarmn     :- Variable to hold the Garment Code.
*-- lcMessage   :- Variable to Message.
*-- laCodInfo   :- Array Hold the code for Vendor Code.
*-- laVendCod   :- Array Hold Vendor Code.

*-- End Declaration variables.


EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode

DECLARE laKeyField [3,4],laData[1]

*** Array to hold the description & codes for : ***

DECLARE laCodInfo [1,10] ,laVendCod[1,2]    && Vendor Code

STORE .F. TO llFirstInv , llFirstMef , llUpdInv , llUpdMef , llMessage
STORE  0  TO lnTotUnit  , lnTotDoz   , lnTotVal 
STORE ' ' TO lcTempInv  , lcTempMef  , lcVendDis, lcStyNo , lcQuata ,;
             lcChief    , lcConst    , lcGarmn  , lcMessage,lcScFields,;
             laCodInfo  , laVendCod  , lcModal
STORE 1 TO lnVendCod , puInvType


laCodInfo[1,01] = "CTSUVENCD"
laCodInfo[1,02] = "laVendCod"
laCodInfo[1,03] = "lnVendCod"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,10] = "cTsuVenCd"

laKeyField[1,1] = 'lnVendCod'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'VENINV'
laKeyField[1,4] = 1
laKeyField[2,1] = 'puInvType'
laKeyField[2,2] = .F.
laKeyField[2,3] = 'VENINV'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[2]'
laKeyField[3,2] =.T.
laKeyField[3,3] = 'VENINV'
laKeyField[3,4] = 3

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)
  SELECT hlcInvHd
  lcScFields= 'cTsuvencd , CCINVNO   , DCINVDATE , CCINVESEL , CCINCONNO  ,'+;
              'CCINTERMS , NCINPACKG , cInvType  , NCINTUNIT , NCINTDOZ   ,'+;
              'NCINTVAL  , MCINVNOTE , CCINSHIPV , NAMOUNTP  '
  
  SCATTER FIELDS &lcScFields Memo TO laData BLANK
  *-- Assign type Regular to ladata[8]
  ladata[8] = 'R'
  lcTempInv = gfTempName()
  lcTempMef = gfTempName()
  STORE 1 TO lnVendCod , laData[7]
ENDIF

laDefProc[7] = .F.                       && Use custom Delete procedure  
laDefProc[9] = .F.                       && Use custom Save procedure


PUSH KEY
DO (gcScrDir+gcAct_Appl+"\Hlcinhd.SPX")
POP KEY


IF glQuitting 
  IF USED('&lcTempInv')
    USE IN &lcTempInv
    ERASE &gcWorkDir.&lcTempInv+'.DBF'
    ERASE &gcWorkDir.&lcTempInv+'.CDX'
    ERASE &gcWorkDir.&lcTempInv+'.FPT'
  ENDIF  
  IF USED('&lcTempMef')
    USE IN &lcTempMef
    ERASE &gcWorkDir.&lcTempMef+'.DBF'
    ERASE &gcWorkDir.&lcTempMef+'.CDX'
    ERASE &gcWorkDir.&lcTempMef+'.FPT'
  ENDIF  
ENDIF
 
RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/19/2001
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
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
    SCATTER FIELDS &lcScFields Memo TO laData BLANK
    IF EMPTY(laData[1])
      STORE 1 TO lnVendCod
    ENDIF
    puInvType = 1
    laData[7] = 1
    laData[8] = 'R'
    laData[14]= 0.00
    SHOW GET laData[7]
    SHOW GET laData[14]
    SHOW GET lnVendCod ENABLE
    SHOW GET puInvType ENABLE
    SHOW GET laData[2] ENABLE
    _CUROBJ = OBJNUM(lnVendCod)
    
  CASE laScrMode[2]                   && VIEW Mode
    lnVendCod = ASUBSCRIPT(laVendCod ,ASCAN(laVendCod,laData[1]),1)
    puInvType = IIF(EMPTY(laData[8]),1,AT(cInvType,'RSB'))
    SHOW GET laData[3]  DISABLE
    SHOW GET laData[4]  DISABLE
    SHOW GET laData[5]  DISABLE
    SHOW GET laData[6]  DISABLE
    SHOW GET laData[12] DISABLE
    SHOW GET laData[13] DISABLE
    SHOW GET laData[9]  
    SHOW GET laData[10] 
    SHOW GET laData[11] 
    SHOW GET laData[14] 
    SHOW GET lnVendCod  DISABLE
    SHOW GET puInvType  DISABLE
    SHOW GET laData[2]  DISABLE
    SHOW GET pbZoom     ENABLE
    SHOW GET pbDetail   ENABLE
    SHOW GET pbMef      ENABLE
  
  CASE laScrMode[4] .OR. laScrMode[3] && ADD OR EDIT Mode
  
    SHOW GET lnVendCod DISABLE
    SHOW GET puInvType DISABLE
    SHOW GET laData[2] DISABLE

  
    SHOW GET laData[3]  ENABLE
    SHOW GET laData[4]  ENABLE
    SHOW GET laData[5]  ENABLE
    SHOW GET laData[6]  ENABLE
    SHOW GET laData[12] ENABLE
    SHOW GET laData[13] ENABLE
    SHOW GET pbZoom     ENABLE
    SHOW GET pbDetail   ENABLE
    SHOW GET pbMef      ENABLE
    SHOW GET laData[9]  
    SHOW GET laData[10] 
    SHOW GET laData[11]
    SHOW GET laData[14] 
    SHOW GET laData[7]  ENABLE
    llFirstInv = .T.
    llFirstMef = .T.
    llUpdInv   = .F.
    llUpdMef   = .F.
ENDCASE    

*-- End Of lpShow.
*:*************************************************************
*: Name      : lfvVend
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid the Vendor Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvVend()
*:*************************************************************
*:
FUNCTION lfvVend

IF !( PADR(ALLTRIM(laData[1]),6) == PADR(ALLTRIM(laVendCod[lnVendCod,2]),6))
  laData[1] = PADR(ALLTRIM(laVendCod[lnVendCod,2]),6)
ENDIF

*-- End Of lfvVend.
*:*************************************************************
*: Name      : lfvInvType
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/19/2001
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvInvType ()
*:*************************************************************
*:
FUNCTION lfvInvType

DO CASE
  CASE puInvType = 1
    laData[8] = "R"
  CASE puInvType = 2
    laData[8] = "S"
  CASE puInvType = 3
    laData[8] = "B"
ENDCASE 

*-- End Of lfvInvType.
*:*************************************************************
*: Name      : lfvData_2
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid ladata[2]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_2()
*:*************************************************************
*:
FUNCTION lfvData_2
 
IF !EMPTY(laData[1]) AND !EMPTY(laData[8]);
   AND !EMPTY(laData[2]) AND LASTKEY()= 13
  SELECT HLCINVHD
  lcExact = SET('EXACT')
  SET EXACT ON
  = SEEK (laData[1]+laData[8]+ALLTRIM(laData[2]))
  SET EXACT &lcExact
  IF FOUND()
    SCATTER FIELDS &lcScFields MEMO TO laData
    
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode=.F.
    laScrMode[2]=.T.
    llFirstInv  =.T.
    llFirstMef  =.T.
    SHOW GETS 
  ELSE
    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Add;\<Browse;\?\<Reenter
    *- button Number  :- 42003.
  
    lcMessage = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))+' '+;
                IIF(laData[8]='R','Regulars',IIF(laData[8]='S','Seconds','Balances'));
                +' '+ALLTRIM(laData[2])
    lnOption = gfModalGen('INM00001B42003','DIALOG',lcMessage )
    DO CASE
      CASE lnOption = 1
        laScrMode   = .F.
        laScrMode[4]= .T.
        laData[3]   = DATE()
        llFirstInv  =.T.
        llFirstMef  =.T.
        SHOW GETS
    
      CASE lnOption = 2
        SELECT HLCINVHD
        LOCATE
        = gfBrows()
        llFirstInv  = .T.
        llFirstMef  = .T.

      CASE lnOption = 3
        STORE '' TO laData[1] , laData[2]
        laScrMode   = .F.
        laScrMode[1]= .T.
        SHOW GETS

    ENDCASE  
  ENDIF  
ENDIF

*-- End Of lfvData_2. 
*:*************************************************************
*: Name      : lfvDetail
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Detail invoice.
*:*************************************************************
*: Calls     : Do hlCinln.prg "Commercial Invoice Line program."
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvDetail()
*:*************************************************************
*:
FUNCTION lfvDetail

llNew     = .T.
lnTotUnt  = laData[9]
lnTotDoz  = laData[10]   
lnTotVal  = ROUND(laData[11],2)

IF llFirstInv .OR. laScrMode[2] .OR. llUpdInv = .F.

  SELECT HLCINVLN  
  SET ORDER TO TAG VENINV
  lcIndExp=SYS(14,VAL(SYS(21)))
  lcTagExp= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus';
    FROM &gcDataDir.HLCINVLN;
   WHERE HLCINVLN.CTSUVENCD+HLCINVLN.CINVTYPE+HLCINVLN.CCINVNO=;
         laData[1]+laData[8]+laData[2];
    INTO DBF &gcWorkDir.&lcTempInv;
   ORDER BY cCvlLinNo

  SELECT (lcTempInv)
  INDEX ON &lcIndExp.&lcTagExp
  COUNT FOR CSTYNUM <> CHR(255) .AND. !DELETED() TO lnLinNo
ELSE
  SELECT (lcTempInv)
  COUNT FOR CSTYNUM <> CHR(255) .AND. !DELETED() TO lnLinNo
ENDIF



*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlCinln.PRG")


llFirstInv = .F.

laData[9]  = lnTotUnt 
laData[10] = lnTotDoz  
laData[11] = ROUND(lnTotVal,2)

SHOW GET laData[9]
SHOW GET laData[10]
SHOW GET laData[11]

SELECT hlCInvHd

*-- End Of lfvDetail.
*:*************************************************************
*: Name      : lfvMef
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Detail invoice.
*:*************************************************************
*: Calls     : Do hlcMef.prg " Multiple Entry Form program"
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvMef()
*:*************************************************************
*:
FUNCTION lfvMef

IF llFirstMef .OR. laScrMode[2] .OR. llUpdMef = .F.

  llMessage = .T.
  SELECT HLCMEF  
  SET ORDER TO TAG VENINV
  lcIndExp1=SYS(14,VAL(SYS(21)))
  lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus',;
          SPACE(1) AS 'Item_Key';
    FROM &gcDataDir.HLCMEF;
   WHERE HLCMEF.CTSUVENCD+HLCMEF.CINVTYPE+HLCMEF.CCINVNO=;
         laData[1]+laData[8]+laData[2];
    INTO DBF &gcWorkDir.&lcTempMef

  =lfUpdMef()
  SELECT (lcTempMef)
  INDEX ON &lcIndExp1.&lcTagExp1
ELSE
  llMessage = .T.
  =lfUpdMef()
  SELECT (lcTempMef)
ENDIF


*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlcMef.PRG")

llFirstMef = .F.

SELECT hlcInvHd

*-- End Of lfvMef.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
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

llMessage = .F.

IF laScrMode[4]             && Add mode
   APPEND BLANK
ENDIF
  
GATHER FROM laData FIELDS &lcScFields MEMO

=gfAdd_Info()

IF llUpdInv
  =lFTMP2MAST('hlCInvln',lcTempInv)
  llUpdInv = .F.
ENDIF

IF llUpdMef
  =lfUpdMef()
  =lFTMP2MAST('hlcMef',lcTempMef)
  llUpdMef = .F.
ELSE
  SELECT HLCMEF  
  SET ORDER TO TAG VENINV
  lcIndExp1=SYS(14,VAL(SYS(21)))
  lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  IF !USED('&lcTempMef')
    SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus',;
           SPACE(1) AS 'Item_Key';
      FROM &gcDataDir.HLCMEF;
     WHERE HLCMEF.CTSUVENCD+HLCMEF.CINVTYPE+HLCMEF.CCINVNO=;
           laData[1]+laData[8]+laData[2];
      INTO DBF &gcWorkDir.&lcTempMef
  ENDIF    
  = lfUpdMef()
  SELECT (lcTempMef)
  INDEX ON &lcIndExp1.&lcTagExp1
  =lFTMP2MAST('hlcMef',lcTempMef)
  llUpdMef = .F.
ENDIF

SELECT hlCInvHd

*-- End OF lpSavScr.
*:*************************************************************
*: Name      : lpDelScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Save The ladata into Base file.
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

*** Delete all Records for this Vendor + Invoice Number
SELECT hlCInvln
lnRecNo = IIF(RECNO() > RECCOUNT(),0,RECNO())
COUNT FOR CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2] TO lnRecords

IF lnRecNo > 0
  GOTO lnRecNo
ENDIF
    
lnRecNo = 1
lcArea = SELECT()
lnCurLine = 0
  
= SEEK (laData[1]+laData[8]+laData[2])
SCAN WHILE CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2]
  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = lnCurLine + 1
  *--Thermometers.
  = gfThermo(lnRecords,lnCurLine,"Deleting Related Records in the Invoice line File",lcArea))
   IF CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2]
    DELETE
  ELSE
    LOOP  
  ENDIF  
ENDSCAN

IF WEXIST('gwdThermo')
  = gfThermo(lnRecords,lnRecords,"Deleting Related Records in the Invoice line File","")
ENDIF

*** Delete all Records for this Vendor + Invoice Number
SELECT hlcMef
lnRecNo = IIF(RECNO() > RECCOUNT(),0,RECNO())
COUNT FOR CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2] TO lnRecords

IF lnRecNo > 0
  GOTO lnRecNo
ENDIF
    
lnRecNo = 1
lnCurLine = 0
lcArea = SELECT()
  
= SEEK (laData[1]+laData[8]+laData[2])
SCAN WHILE CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2]
  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = lnCurLine + 1
  *--Thermometers.
  = gfThermo(lnRecords,lnCurLine,"Deleting Related Records in the Invoice line File",lcArea))
  
  IF CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2]
    DELETE
  ELSE
    LOOP  
  ENDIF  
ENDSCAN

IF WEXIST('gwdThermo')
  = gfThermo(lnRecords,lnRecords,"Deleting Related Records in the M.E.F. File","")
ENDIF

  
*** Delete the user record from the Header File 
SELECT hlCInvHd
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE
laScrMode    = .F.
laScrMode[1] = .T.

*-- End of lpDelScr.
*:*************************************************************
*: Name      : lfUpdMef
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfUpdMef ()
*:*************************************************************
*:
FUNCTION lfUpdMef

lnRecNo = 0

IF USED('&lcTempInv') AND laScrMode[2] = .F.
  SELECT(lcTempInv)
  IF !SEEK(laData[1]+laData[8]+laData[2])
    SELECT hlcInvLn
    lnRecArea = IIF(RECNO() > RECCOUNT(),0,RECNO())
    SEEK laData[1]+laData[8]+laData[2]
  ENDIF
  lnRecArea = IIF(RECNO() > RECCOUNT(),0,RECNO())
  GO TOP
ELSE  
  SELECT hlcInvLn
  lnRecArea = IIF(RECNO() > RECCOUNT(),0,RECNO())
  = SEEK (laData[1]+laData[8]+laData[2])
ENDIF  

lnRecNo = IIF(RECNO() > RECCOUNT(),0,RECNO())

COUNT FOR CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2] TO lnRecords


IF lnRecArea > 0
  GOTO lnRecArea
ENDIF

*-- Count all the records to be processed to be used in the 
*-- Thermometer.
lnCurLine = 0

lnRecNo = 1
lcArea = SELECT()

= SEEK (laData[1]+laData[8]+laData[2])
SCAN WHILE CTSUVENCD+CINVTYPE+CCINVNO = laData[1]+laData[8]+laData[2]

  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  lnCurLine = lnCurLine + 1
  *--Thermometers.
  = gfThermo(lnRecords,lnCurLine,"Updating invoice line...",lcArea))
  
  
  lcStyNo = cstynum
  SELECT hlcStyle
  IF SEEK(lcStyNo)
    lcQuata = ctsuqtcat
    lcChief = ctsuchwcd
    lcConst = ctsuconcd
    lcGarmn = ctsugarcd
  ENDIF
  SELECT (lcTempMef)

  LOCATE FOR ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd =;
             lcQuata+lcChief+lcConst+lcGarmn

  IF FOUND()    && If the record is alredy exist in the file
    REPLACE Item_key WITH 'T'      
    SELECT(lcArea)
    LOOP
  ELSE
    SELECT (lcTempMef)
    APPEND BLANK
    REPLACE cStatus   WITH "A",;
            nRecNo    WITH 0,;
            CTSUVENCD WITH laData[1],; 
            CINVTYPE  WITH laData[8],;
            CCINVNO   WITH laData[2],;
            CTSUQTCAT WITH lcQuata,;
            CTSUCHWCD WITH lcChief,;
            CTSUCONCD WITH lcConst,;
            CTSUGARCD WITH lcGarmn,;
            LCUTA     WITH .F.,;
            LSEWA     WITH .F.,;
            LCUTB     WITH .F.,;
            LSEWB     WITH .F.,;
            DEXPORTA  WITH CTOD('00/00/00'),;
            DEXPORTB  WITH laData[3],;
            Item_Key  WITH 'T'
  ENDIF
  SELECT(lcArea)  
ENDSCAN 


IF WEXIST('gwdThermo')
  = gfThermo(lnRecords,lnRecords,"Updating invoice line...","")
ENDIF


SELECT(lcArea)  

IF lnRecArea > 0
  GOTO lnRecArea
ENDIF

* This step is to delete the extra records in the hlcMef file 
* wish should not be thier.

SELECT (lcTempMef)
GO TOP
SCAN 
  IF Item_Key = 'T'
    REPLACE Item_Key WITH ' '
  ELSE
    REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    DELETE
  ENDIF
ENDSCAN

SELECT(lcArea)  

*-- End Of lfUpdMef.
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
