*:***************************************************************************
*: Program file  : MFOPMG
*: Program desc. : Operations Management Report
*: For Report    : MFOPMGA.FRX
*: System        : Aria 4XP
*: Modules       : MF,PO,MA
*: Developer     : AHMED SHOUKRY MOHAMMED (ASM)
*: Date          : 02/26/2006
*: Issue #       : 037649
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Mod.    :
*: B608676,1 MMT 09/03/2008 Fix bug of error when user print all CTs[T20080501.0006]
*! B608915,1 MMT 07/01/2009 Fix bug of wrong scale when preview report twice[T20080619.0059]
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[T20090831.0015]
*: E302605 : MOS 05/18/2009 - Print Cutting Ticket Notepad  - T20090204.0043 
*: B609454,1 MMT 11/10/2010 Operation Managment form does not call optional programs[T20100827.0013]
*:***************************************************************************


lcStTime   = TIME()    && Time in which we start collect data.
LOCAL lcTmpFile
LOCAL ARRAY laFileStru[1]
loOgScroll.cCRorientation = 'P'
*! Adjust Report Variables 
DIMENSION laFrom[5], laTo[5], laCompAdd[6], laFOprD[7], laTOprD[7]
DIMENSION laFromOpr[7,2], laToOpr[7,2]
STORE SPACE(0) TO laFrom,laTo,laCompAdd,lcFDesc,lcTDesc,lcFName,lcTName,;
                  lcOpr, lcStyTitle, laOldVal

STORE SPACE(0) TO lcContCode
STORE SPACE(0) TO lcCutTikNo

m.cTarget = ''
llLogo = .F.
* Two arrays to get operation data from the codes file
laFromOpr[1,1] = 'GLACCOUNT'
laFromOpr[1,2] = 'laFOprD[1]'
laFromOpr[2,1] = 'COPERSEQ'
laFromOpr[2,2] = 'laFOprD[2]'
laFromOpr[3,1] = 'LINHOUSE'
laFromOpr[3,2] = 'laFOprD[3]'
laFromOpr[4,1] = 'CCONTCODE'
laFromOpr[4,2] = 'laFOprD[4]'
laFromOpr[5,1] = 'CCONTNAME'
laFromOpr[5,2] = 'laFOprD[5]'
laFromOpr[6,1] = 'LMFGOPR'
laFromOpr[6,2] = 'laFOprD[6]'
laFromOpr[7,1] = 'LEADTIME'
laFromOpr[7,2] = 'laFOprD[7]'


lcLogoPic = gfTempName()
lcTOpr    = SPACE(0)

**********
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
SELECT SYCCOMP
SEEK gcAct_Comp &&x
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , '@R '+lcPhonPict)
DO lfShiftArr WITH laCompAdd

lcStyTitle = gfItemMask('HI')
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
SELECT OBJLINK
SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE &&x

IF !USED('TMPCUTTKTH')
  DO CASE
    CASE lcImTyp = 'M'
       =gfOpenTable('POSHDR','POSHDR','SH','TMPCUTTKTH')
       lcBsWrk = 'PU'
    CASE lcImTyp = 'T'
      =gfOpenTable('POSHDR','POSHDR','SH','TMPMMFGORDH')
*      =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH','TMPMMFGORDH')
      lcBsWrk = 'PF'
    CASE lcImTyp = 'I'
       =gfOpenTable('POSHDR','POSHDR','SH','TMPPOSHDR')
*       =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH','TMPPOSHDR')
       lcBsWrk = 'PP'
  ENDCASE
  =gfOpenTable(oAriaApplication.DataDir+'BOMLINE','MFGOPR','SH','TMPBOMLINE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH','TMPSTYLE')
  =gfOpenTable(oAriaApplication.DataDir+'MFGOPRHD','MFGOPRHD','SH','TMPMFGOPRHD')
  *Open MFGOPRHD again with that alias to get the target operation data from
  =gfOpenTable(oAriaApplication.DataDir+'MFGOPRHD','MFGOPRHD','SH','lcTrgtOp')
ENDIF

IF gfSeek('*' + 'LOGO' , 'OBJLINK') AND gfSeek(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  *:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[Start]
*!*	  SELECT gobject;
*!*	   FROM Objects         ;
*!*	   WHERE Objects.cobject_id = lcObj_Id ;
*!*	   INTO CURSOR (lcLogoPic)
  SELECT mimgpath;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
  *:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[End] 
ENDIF

****!
****!

* Add the filter of status
IF lcStatus <> "L"
   **!lcRpExp = '(' +lcRpExp+ ") .AND. ("+lcFile+".Status = '"+lcStatus+"')"
ENDIF

SELECT MFGOPRHD
DO CASE
  CASE lcImTyp = 'M' .OR. lcImTyp = 'T'
    **!SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
  CASE lcImTyp = 'I'
    **!SET RELATION TO 'P'+cTktNo INTO (lcFile) ADDITIVE
ENDCASE
*B802150,1 AMM Set relation to let the date filter works.
**!SET RELATION TO cImTyp+cTktNo+cOprCode INTO MFGOPRDT ADDITIVE
*B802150,1 AM end

IF lcImTyp = 'I'
  lcTkt  = lfCheckFilter(3, "IIF(.T.,MFGOPRHD.CTKTNO,'')")
ELSE
  lcTkt  = lfCheckFilter(3, 'MFGOPRHD.CTKTNO')
ENDIF 
lcOprCode  = lfCheckFilter(3, 'MFGOPRHD.COPRCODE')

IF !EMPTY(lcOprCode)
  lcOpr = loOGScroll.gfTempName()	
  llUseOpert = IIF(LEN(lcOprCode)>0,.T.,.F.) AND lfConvertToCursor(lcOprCode,'COPRCODE',lcOpr)
ENDIF 

lcCont = lfCheckFilter(1, 'MFGOPRHD.CCONTCODE')
lcDate = IIF(lcRpExp='BETWEEN(',LEFT(lcRpExp,56),'.T.')
lcRpExp  = IIF(lcRpExp='BETWEEN(',SUBSTR(lcRpExp,62),lcRpExp)
IF EMPTY(lcRpExp)
  lcRpExp = '.T.'
ENDIF
**********
IF loOgScroll.llOGFltCh 
  =lfCollectData()
*B608915,1 MMT 07/01/2009 Fix bug of wrong scale when preview report twice[Start]
ELSE
  SELECT STYLE
  IF !('SCALE' $ UPPER(SET("Relation")))
    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  ENDIF   
*B608915,1 MMT 07/01/2009 Fix bug of wrong scale when preview report twice[End]  
ENDIF

*B609454,1 MMT 11/10/2010 Operation Managment form does not call optional programs[Start]
=lfOptProg()
*B609454,1 MMT 11/10/2010 Operation Managment form does not call optional programs[End]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*WAIT WINDOW LANG_Arsjour_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(lcMainF))) + LANG_Arsjour_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Arsjour_SecondMsg TIMEOUT 2
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMainF))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
SELECT (lcMainF)
* Display the report
DO gfDispRe WITH (lcFormName)


SELECT (lcMainF)
**!SET RELATION TO
**!USE 
**!ERASE (gcWorkDir+lcMainF+'.DBF')
**!ERASE (gcWorkDir+lcMainF+'.CDX')
**!IF USED(lcTrgtOp)
  **!USE IN (lcTrgtOp)
**!ENDIF

RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED SHOUKRY MOHAMMED (ASM)
*! Date      : 02/26/2006
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*!*  USE IN OBJECTS
*!*  USE IN OBJLINK
*!*  USE IN APVENDOR
*!*  USE IN WareHous
*!*  USE IN SCALE
*!*  USE IN STYLE
*!*  USE IN SYCCOMP
*!*  USE IN CUTTKTH
=gfOpenTable(oAriaApplication.DataDir+'OBJECTS','OBJECTID','SH')
=gfOpenTable(oAriaApplication.DataDir+'OBJLINK','OBJLNKTY','SH')
=gfOpenTable(oAriaApplication.DataDir+'MFGOPRHD','MFGOPRHD','SH')
=gfOpenTable(oAriaApplication.DataDir+'MFGOPRDT','TKTOPTRN','SH')
=gfOpenTable(oAriaApplication.DataDir+'APVENDOR','VENCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'WareHous','WareHous','SH')
=gfOpenTable(oAriaApplication.DataDir+'BOMLINE','MFGOPR','SH')
=gfOpenTable(oAriaApplication.DataDir+'CTKTBOM','CTKTBOM','SH')
=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')
=gfOpenTable('POSHDR','POSHDR','SH','CUTTKTH')
*-E302605 : MOS 05/18/2009 - Print Cutting Ticket Notepad  - T20090204.0043 [START]
*=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH','MMFGORDH')
=gfOpenTable('POSHDR','POSHDR','SH','MMFGORDH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD','NOTEPAD','SH')
*-E302605 : MOS 05/18/2009 - Print Cutting Ticket Notepad  - T20090204.0043 [END]

=gfOpenTable('POSHDR','POSHDR','SH')



DO CASE
  CASE oariaapplication.activemoduleid = 'MF'
    lcImTyp = 'M'
    lcFile  = 'CUTTKTH'
  CASE oariaapplication.activemoduleid  ='PO'
    lcImTyp = 'I'
    lcFile  = 'POSHDR'
  CASE oariaapplication.activemoduleid ='MA'
    lcImTyp = 'T'
    lcFile  = 'MMFGORDH'
ENDCASE

RETURN


*!*************************************************************
*! Name      : lfCollectData
*! Developer : Hossam El Etreby (HDM)
*! Date      : 02/26/2006
*! Purpose   : Collects the data from the Tables
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCollectData

* Create the temporary file
SELECT MFGOPRDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]
laFileStru[lnFileStru+1,1] = 'cTarget'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
FOR lnLoop = 1  TO 1
  STORE '' TO laFileStru[lnFileStru+lnLoop ,7],laFileStru[lnFileStru+lnLoop  ,8],;
              laFileStru[lnFileStru+lnLoop ,9],laFileStru[lnFileStru+lnLoop  ,10],;
              laFileStru[lnFileStru+lnLoop ,11],laFileStru[lnFileStru+lnLoop ,12],;
              laFileStru[lnFileStru+lnLoop ,13],laFileStru[lnFileStru+lnLoop ,14],;
              laFileStru[lnFileStru+lnLoop ,15],laFileStru[lnFileStru+lnLoop ,16]

  STORE 0 TO  laFileStru[lnFileStru+lnLoop ,17],laFileStru[lnFileStru+lnLoop ,18]
ENDFOR 


lcMainF    = gfTempName()
= gfCrtTmp(lcMainF,@laFileStru,"cTktNo+cOprCode+cTarget+cLotNo+Item+TranCd+cTrgOpr+cTrgLot" ,lcMainF,.F.)
*CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
*INDEX ON cTktNo+cOprCode+cTarget+cLotNo+TranCd+cTrgOpr+cTrgLot TAG (lcMainF)
*,INDEX ON cTktNo+cOprCode+cTarget+cLotNo+Item+TranCd+cTrgOpr+cTrgLot TAG (lcMainF)

SELECT MFGOPRHD
gfSeek('**!!$$') && Zap the Cursor View
lnCase=0
DO CASE
  CASE !EMPTY(lcTkt) AND RECCOUNT(lcTkt)>0
    lnCase=1
    lcTmpFile = gfTempName()
    SELECT (lcTkt)
    SCAN
      lcTemp = PO
      gfSqlRun("Select * From MFGOPRHD (INDEX=MFGOPRHD) Where cImTyp ='"+lcImTyp+"' AND CTKTNO = '"+lcTemp+"'",'TMPMFGOPRHD',.t.,lcTmpFile)
      SELECT MFGOPRHD
      APPEND FROM (DBF(lcTmpFile))
      SELECT (lcTkt)
    ENDSCAN
  CASE !EMPTY(lcOpr) and USED(lcOpr) AND RECCOUNT(lcOpr)>0
    lnCase=2
    lcTmpFile = gfTempName()
    SELECT (lcOpr)
    SCAN
      lcTemp = COPRCODE
      gfSqlRun("Select * From MFGOPRHD Where COPRCODE = '"+lcTemp+"'",'TMPMFGOPRHD',.t.,lcTmpFile)
      SELECT MFGOPRHD
      APPEND FROM (DBF(lcTmpFile))
      SELECT (lcOpr)
    ENDSCAN
  CASE !EMPTY(lcCont) AND RECCOUNT(lcCont)>0
    lnCase=3
    lcTmpFile = gfTempName()
    SELECT (lcCont)
    SCAN
      lcTemp = cVendCode
      gfSqlRun("Select * From MFGOPRHD Where CCONTCODE = '"+lcTemp+"'",'TMPMFGOPRHD',.t.,lcTmpFile)
      SELECT MFGOPRHD
      APPEND FROM (DBF(lcTmpFile))
      SELECT (lcCont)
    ENDSCAN
  OTHERWISE
    lnCase=4
    lcTmpFile = gfTempName()
    gfSqlRun("Select * From MFGOPRHD",'TMPMFGOPRHD',.t.,lcTmpFile)
    SELECT MFGOPRHD
    APPEND FROM (DBF(lcTmpFile))
  
ENDCASE
***
* Fill the temporary file with required data
SELECT MFGOPRHD
SCAN FOR &lcRpExp
  lcTrgOpr = SPACE(6)
  *-- Get next operation title
  **!lnRecNo = RECNO()
  * Get next operation sequence number =(current Operation Seq+1)
  lcOprSeq  = PADL(ALLTRIM(STR(VAL(cOperSeq)+1)),2,'0')
  SELECT TMPMFGOPRHD
  =gfSeek(lcImTyp+MFGOPRHD.cTktNo)
  *B602304,1 AMM Adjust the locate to fit if the user put a leading zero
  *LOCATE REST FOR cOperSeq=lcOprSeq
  LOCATE REST FOR PADL(ALLTRIM(cOperSeq),2,'0') = lcOprSeq
  *B602304,1 AMM end
  IF FOUND()
    lcTrgOpr = cOprCode
  ENDIF
  **!GOTO (lnRecNo)
  SELECT MFGOPRHD
  
  IF gfSeek(lcImTyp+cTktNo+cOprCode,'MFGOPRDT')
    SELECT MFGOPRDT
    SCAN WHILE cImTyp+cTktNo+cOprCode= lcImTyp+MFGOPRHD.cTktNo+MFGOPRHD.cOprCode ;
         FOR TranCd='1' .and. IIF(EMPTY(lcRpLot),.T.,cLotNo=lcRpLot) AND EVALUATE(lcDate)
      m.cTarget = lcTrgOpr
      SCATTER MEMVAR MEMO
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDSCAN
  ENDIF

ENDSCAN
SET RELATION TO
*B602304,1 AMM Use the file again to get the target operation data from.
**!USE (gcDataDir+'MFGOPRHD') AGAIN ALIAS (lcTrgtOp) IN 0 ORDER TAG MFGOPRHD
*B602304,1 AMM end

SELECT (lcFile)
gfSeek('**!!$$') && Zap the Cursor View
SELECT Style
gfSeek('**!!$$') && Zap the Cursor View
SELECT BomLine
gfSeek('**!!$$') && Zap the Cursor View
SELECT 'lcTrgtOp'
gfSeek('**!!$$') && Zap the Cursor View
SELECT (lcMainF)
GO TOP
SCAN
  gfSeek(lcBsWrk+cTktNo,'Tmp'+lcFile)
  SELECT (lcFile)
  APPEND FROM (DBF('Tmp'+lcFile))
  SELECT (lcMainF)
  
*: B608676,1 MMT 09/03/2008 Fix bug of error when user print all CTs[Start]
*!*	  gfSeek(Item,'TmpStyle')
*!*	  SELECT Style
*!*	  APPEND FROM (DBF('TmpStyle'))
*: B608676,1 MMT 09/03/2008 Fix bug of error when user print all CTs[End]

  SELECT (lcMainF)

  gfSeek(cimtyp+ctktno+SPACE(6)+'1'+IIF(lcImTyp='T','0002','0001')+Item+SPACE(4)+cOprCode,'TmpBomLine')
  SELECT BomLine
  APPEND FROM (DBF('TmpBomLine'))
  SELECT (lcMainF)
  
  gfSeek(cimtyp+cTktNo+cTarget,'TMPMFGOPRHD')
  SELECT 'lcTrgtOp'
  APPEND FROM (DBF('TMPMFGOPRHD'))
  SELECT (lcMainF)
ENDSCAN
lcM="'"+lcBsWrk+"'+cTktNo"
SET RELATION TO &lcM INTO (lcFile) ADDITIVE 
IF lcStatus <> "L"
  DELETE ALL FOR EVALUATE(lcFile+".Status <> '"+lcStatus+"'")
  PACK
ENDIF
**!SET RELATION TO Item INTO STYLE ADDITIVE
SET RELATION TO Item INTO STYLE ADDITIVE
SET RELATION TO cimtyp+cTktNo+cOprCode INTO MFGOPRHD ADDITIVE
* Make this relation to get the unit cost of each element.
*B802793,1 AMM Adjust the relation
*SET RELATION TO cimtyp+ctktno+SPACE(6)+'1'+Item+Color+SPACE(25)+cOprCode INTO BOMLINE ADDITIVE
**!SET RELATION TO cimtyp+ctktno+SPACE(6)+'1'+Item+Color+cOprCode INTO BOMLINE ADDITIVE
SET RELATION TO cimtyp+ctktno+SPACE(6)+'1'+IIF(lcImTyp='T','0002','0001')+Item+SPACE(4)+cOprCode INTO BOMLINE ADDITIVE && What is cinvtypc ?
*B802793,1 AMM end
lcM="'S'+LEFT(Item,"+ALLTRIM(STR(lnMajSize))+")"
SET RELATION TO &lcM INTO Objlink ADDITIVE
*B602304,1 AMM Set relation to get target operation data
SET RELATION TO cimtyp+cTktNo+cTarget INTO 'lcTrgtOp' ADDITIVE
*B602304,1 AMM end
SELECT Scale
gfSeek('')
SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT (lcMainF)
GO TOP


RETURN

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : MFOPMGA.FRX
*!*************************************************************
*! Calls       : gfRltFld(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
* If the operation changed, get the new codes.

*B125607,1 BWA 01/04/2005 Fix the bug of printing duplicate vendor address.[START]
*IF lcOpr # &lcMainF..cOprCode
*B129132,1 EIH 09/15/2005 Add check for the cut ticket number .[Begin]
*IF (lcOpr # &lcMainF..cOprCode) .OR. (lcContCode # &lcMainF..cContCode)
IF (lcOpr # &lcMainF..cOprCode) .OR. (lcContCode # &lcMainF..cContCode) .OR.  (lcCutTikNo # &lcMainF..ctktno)
  lcCutTikNo = &lcMainF..ctktno
  *B129132,1 EIH 09/15/2005 [END]
  
  lcContCode = &lcMainF..cContCode
  *B125607,1 BWA 01/04/2005.[END]

  lcOpr = &lcMainF..cOprCode
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  * Get the main operation name and addresses
  lcFDesc   = gfCodDes(&lcMainF..cOprCode, 'MFGCODE')
  =gfRltFld(&lcMainF..cOprCode , @laFromOpr, 'MFGCODE')
  *B602304,1 AMM Don't get operation data from the codes file, get it from 
  *B602304,1 AMM MFGOPRHD instead
  *IF !EMPTY(&lcMainF..cTarget)
  *  lcTDesc   = gfCodDes(&lcMainF..cTarget, 'MFGCODE')
  *  =gfRltFld(&lcMainF..cTarget , @laToOpr, 'MFGCODE')
  *ELSE
  *  STORE SPACE(0) TO lcTDesc, laTOprD
  *ENDIF 
  IF !EMPTY(&lcMainF..cTarget) .AND. !EOF('lcTrgtOp')
    lcTDesc = gfCodDes(&lcMainF..cTarget, 'MFGCODE')
    lcTOpr  = EVAL('lcTrgtOp.cContCode')
    lcTName = EVAL('lcTrgtOp.cContName')
  ELSE
    STORE SPACE(0) TO lcTDesc, lcTOpr, lcTName
  ENDIF
  *B602304,1 AMM end

  *-- Search the contractor code in the vendor file
  *B602304,1 AMM Get the vendor data if the operation is InHouse = 'NO'
  *IF SEEK(ALLTRIM(MFGOPRHD.cContCode),'APVENDOR')
  IF !MFGOPRHD.lInHouse .AND. !EMPTY(MFGOPRHD.cContCode) AND gfSeek(ALLTRIM(MFGOPRHD.cContCode),'APVENDOR')  
  *B602304,1 AMM end
    lcFName   = APVENDOR.CVenComp
    * Get the vendor addresses
    laFrom[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
    laFrom[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
    laFrom[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
    laFrom[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
    laFrom[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
    DO lfShiftArr WITH laFrom
  ELSE
    STORE SPACE(0) TO laFrom,lcFName
    *B602304,1 AMM Get the contractor name entered by cost sheet program
    lcFName  = MFGOPRHD.cContName
    *B602304,1 AMM end
  ENDIF
  
  *--If this is last operation, get the warehouse addresses
  *B602304,1 AMM start, that array not used any more, use this variable instead
  *IF EMPTY(laTOprD[4])
  IF EMPTY(lcTOpr)
  *B602304,1 AMM end
    *B802150,1 AMM Adjust the seek exp. in case of seeking in POSHDR file
    *IF SEEK(&lcMainF..CTKTNo,lcFile) .AND. SEEK(&lcFile..cWareCode,'WareHous')
    IF !EOF(lcFile) .AND. !EMPTY(&lcFile..cWareCode) AND gfSeek(&lcFile..cWareCode,'WareHous')
    *B802150,1 AMM end
      lcTDesc = '** None **'
      *B602304,1 AMM start, That array not used any more, use this variable instead
      *laTOprD[4] = WareHous.cWareCode
      lcTOpr  = WareHous.cWareCode
      *B602304,1 AMM end
      lcTName = Warehous.cDesc
      laTo[1] = gfGetAdr('Warehous' , '' , '' , '' , 1)
      laTo[2] = gfGetAdr('Warehous' , '' , '' , '' , 2)
      laTo[3] = gfGetAdr('Warehous' , '' , '' , '' , 3)
      laTo[4] = gfGetAdr('Warehous' , '' , '' , '' , 4)
      laTo[5] = gfGetAdr('Warehous' , '' , '' , '' , 5)
      DO lfShiftArr WITH laTo
    ELSE
      STORE SPACE(0) TO laTo,lcTName
    ENDIF
  ELSE
    *B602304,1 AMM Get the vendor data from the vendor file in case of 
    *B602304,1 AMM InHouse=No only.
    *IF SEEK(ALLTRIM(laTOprD[4]),'APVENDOR')
    IF !lcTrgtOp.lInHouse .AND. !EMPTY(lcTOpr) AND gfSeek(ALLTRIM(lcTOpr),'APVENDOR')
    *B602304,1 AMM end
      lcTName   = APVENDOR.CVenComp
      * Get the vendor addresses
      laTo[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
      laTo[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
      laTo[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
      laTo[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
      laTo[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
      DO lfShiftArr WITH laTo
    ELSE
      STORE SPACE(0) TO laTo,lcTName
      *B602304,1 AMM If InHouse = Yes, get the contractor name entered by 
      *B602304,1 AMM Cost sheet program
      lcTName  = MFGOPRHD.cContName
      *B602304,1 AMM end
    ENDIF
  ENDIF
  SELECT (lnAlias)
ENDIF
RETURN ''

*!*************************************************************
*! Name      : lfShiftArr
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Collapse thhe elements of the array
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShiftArr()
*!*************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Store the old value.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

lcCurrObj = ogsys18()
laOldVal = ALLTRIM(EVALUATE(ogsys18()))

*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Valid function of the contractor setting in the option grid
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj
lnAlias = SELECT(0)

lcVar = ogsys18() && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(ogsys18())) && Varible to hold the current field value

SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj )
  IF !gfSeek(lcObj , 'APVENDOR')
    =gfApVnBrow(@lcObj)
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE
      &lcVar = laOldVal
    ENDIF
  ENDIF
ELSE
  IF !gfSeek(lcObj , 'APVENDOR')
    *WAIT WINDOW "This contractor doesn't exist in the vendor file !!" 
    =gfModalGen('TRM00001B00000','DIALOG','This contractor')
  ENDIF
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get title to be displayed (PO or cuttkt or MFG order)
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
DO CASE
  CASE oariaapplication.activemoduleid ='PO'
    lcTit = 'Purchase Order'
  CASE oariaapplication.activemoduleid ='MA'
    lcTit = 'MFG Order'
  CASE oariaapplication.activemoduleid ='MF'
    lcTit = ALLTRIM(gfGetMemvar('M_PRDLNLBL',oAriaApplication.ActiveCompanyID))  
ENDCASE
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
lcReturn = ""     
DO CASE
CASE lnArrayType = 1 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
  ENDIF
CASE lnArrayType = 2  
  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
  ENDIF
CASE lnArrayType = 3  
  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
  ENDIF
ENDCASE  
RETURN lcReturn




*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Parameters: Start collection date,End collection date
*!*************************************************************
*! Returns   : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 09/14/2005
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
CASE   ALLTRIM(lcFieldName) = 'COPRCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*************************************************************
