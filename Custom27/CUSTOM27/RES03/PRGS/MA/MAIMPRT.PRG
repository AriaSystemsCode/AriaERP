*!****************************************************************************
*! Program file        : MAIMPRT.PRG
*! Program description : Import Rolls from Excel files
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Material - (MA)
*! Developer Name      : HBG - Hend Ghanem
*! Date completed      : 03/01/2005
*! Tracking Job Number : C#126689
*!****************************************************************************
* This program reads from an Excel file , import data to a Fox2 file , reads from this file
* appends Rolls and update Rolls file 

**********************************
* Excel sheet Format :
**********************************
* A --> Field Holds PO NUMBER
* B --> Field Holds Roll Key
* C --> Field Holds Color
* D --> Field Holds New Roll Key
* E --> Field Holds Qty

lcPathName = ""
DO (gcScrDir+gcWinAppl+"\MAIMPRT.SPX")

*!*************************************************************
*! Name      : lfvgetFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/01/2005
*! Purpose   : Function to get the Excel file Path
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvgetFile()
*!*************************************************************
FUNCTION lfvgetFile

lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select') 

IF EMPTY(lcPathName)
  SHOW GET PbProceed DISABLE && Disable the Proceed Button
ELSE
  SHOW GET PbProceed ENABLE  && Enable the Proceed Button
ENDIF

SHOW GET lcPathName
_CUROBJ = OBJNUM(PbProceed)

*-- End Of lfvgetFile

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/01/2005
*! Purpose   : To import the excel file 
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************

FUNCTION lfvProceed

IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName)
ENDIF

IF EMPTY(lcPathName)
  *-- Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF

*-- Check if File Is .XLS file.
IF !(".XLS" $ lcPathName)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This file can not be selected you must select a file of .Xls type.')
    _CUROBJ = OBJNUM(lcPathName)
    RETURN
ENDIF

IF !FILE(lcPathName)   
  *-- "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF


Private llImpErr, lcDefDir, lcXLS,lcAlias

*--Open Needed files
=lfOpenFls()
*--Create needed temp files
Private lcTmpRolls, lcSumRolls

lcTmpRolls = gfTempName()
lcSumRolls = gfTempName()
lcXls = 'SHEET'
=lfCratTemp()

If !Used(lcXLS)
  Select 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'                               
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFA TO (gcWorkDir)
  IMPORT FROM (lcPathName) TYPE XLS
  lcXLS = DBF()
  lcAlias = ALIAS()  
  Use (lcXLS) In 0 Again ALIAS 'SHEET'
  Select (lcALIAS)
  Use 
  lcXLS = 'SHEET'
  SELECT (lcXLS)
  SET DEFAULT TO &lcDefDir
  ON ERROR &lcErrStr
  WAIT CLEAR
  IF llImpErr
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Excel file format!')      
    =lfCloseFls()	
    _CUROBJ = OBJNUM(lcPathName)
    RETURN
  ENDIF
  SELECT (lcXLS)
  LOCATE
  IF TYPE(lcXLS+'.A') = 'U' OR TYPE(lcXLS+'.B') = 'U' OR TYPE(lcXLS+'.C') = 'U' OR;
     TYPE(lcXLS+'.D') = 'U' OR TYPE(lcXLS+'.E') = 'U' OR;
     !(UPPER('PO NO') $ UPPER(&lcXLS..A)) OR !(UPPER('Invoice No') $ UPPER(&lcXLS..B)) OR;
     !(UPPER('Color') $ UPPER(&lcXLS..C)) OR !(UPPER('Roll NO') $ UPPER(&lcXLS..D)) OR;
     !(UPPER('Qty') $ UPPER(&lcXLS..E))
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid file. Please check on the structure of the source file.')      
    =lfCloseFls()	
    _CUROBJ = OBJNUM(lcPathName)
    RETURN
  ENDIF   
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is another file with the same name of '+;
                                           'the selected Excel file , Please rename.')
   =lfCloseFls()
   _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF
SELECT (lcXLS) 
GO TOP 
DELETE && Delete the first row which hold the Fields headings
DELETE ALL FOR EMPTY(A) && Delete all empty records
SET DELETED ON

*--Index  Color.
SELECT (lcXLS) 
INDEX ON ALLTRIM(C) TAG ROLLS 


SELECT (lcXLS) 
LOCATE 
lcPONum = ALLTRIM(&lcXLS..A)
lcInvNum = ALLTRIM(SUBSTR(&lcXLS..B,2,LEN(&lcXLS..B)))
STORE .F. TO llError , llPoErr , llInvErr , llTranErr , llQtyErr ,;
			 llRollErr , llColrErr , llRollFnd
 
IF SEEK('P'+PADR(&lcXLS..A,6),'POFLN')
  lcItemCod = POFLN.FABRIC
  lcItemColor = POFLN.COLOR
  SELECT POFLN
  LOCATE REST FOR cmattype+pomat+fabric+color+trancd = 'P'+PADR(&lcXLS..A,6)+lcItemCod+lcItemColor+'2'
  IF !FOUND()
    llError = .T.
    llTranErr = .T.
  ENDIF
ELSE
  llError = .T.
  llTranErr = .T.  
ENDIF
IF !llError
  SELECT (lcXLS) 
  LOCATE
  lcPONum = ALLTRIM(&lcXLS..A)
  lcInvNum = ALLTRIM(SUBSTR(&lcXLS..B,2,LEN(&lcXLS..B)))
  SCAN
    IF lcPONum <> ALLTRIM(&lcXLS..A)
      llError = .T.
      llPoErr = .T.
    ENDIF
    IF lcInvNum <> ALLTRIM(SUBSTR(&lcXLS..B,2,LEN(&lcXLS..B)))
      llError  = .T.
      llInvErr = .T.
    ENDIF
    IF SEEK(PADR(lcItemCod,7)+PADR(&lcXLS..B,20)+PADR(&lcXLS..C,6),lcSumRolls)
      SELECT (lcSumRolls)
      REPLACE Qty 	 WITH Qty + VAL(&lcXLS..E) 
    ELSE
      SELECT (lcSumRolls)    
      APPEND BLANK
      REPLACE ITEM   WITH lcItemCod,;
      		  RollID WITH PADR(&lcXLS..B,20),;
      		  COLOR  WITH PADR(&lcXLS..C,6),;
      		  Qty 	 WITH VAL(&lcXLS..E) 
     ENDIF
  ENDSCAN
  lcRollID  = ''
  SCAN
    lcPO = &lcXLS..A
    lcRoll = &lcXLS..B
    lnSumQty  = 0
    SET ORDER TO ROLLS In ROLLS
    IF SEEK(PADR(&lcXLS..B,20)+PADR(lcItemCod,7),'ROLLS','ROLLS')
      IF SEEK(PADR(&lcXLS..B,20)+PADR(lcItemCod,7)+PADR(&lcXLS..C,6),'ROLLS','ROLLS')
        SELECT ROLLS
        SCATTER MEMVAR MEMO
        lnRQty = ROLLS.nQty
        =SEEK(PADR(lcItemCod,7)+PADR(&lcXLS..B,20)+PADR(&lcXLS..C,6),lcSumRolls)
        lnSumQty = &lcSumRolls..Qty
        IF lnRQty = lnSumQty
          m.cRollID = ALLTRIM(SUBSTR(&lcXLS..B,2,LEN(&lcXLS..B)))+ALLTRIM(&lcXLS..D)
          SET ORDER TO ROLLITEM In ROLLS
          IF !SEEK(m.cRollID,'ROLLS','ROLLITEM') AND !SEEK(m.cRollID,lcTmpRolls,'ROLLS')
            m.NQty    = VAL(&lcXLS..E)
            m.nQtybal = VAL(&lcXLS..E)
            m.Color   = PADR(&lcXLS..C,6)
            INSERT INTO (lcTmpRolls) FROM MEMVAR
            =lfAddUsrDt(lcTmpRolls)
          ELSE
            llError = .T.  
            llRollFnd = .T.
            lcRollID = m.cRollID
            EXIT
          ENDIF  
        ELSE
          llError = .T.  
          llQtyErr = .T.
        ENDIF
      ELSE
        llError = .T.
        llColrErr = .T.
      ENDIF  
    ELSE
      llError = .T. 
      llRollErr = .T.
    ENDIF  
  ENDSCAN
ENDIF

IF llError
  DO CASE
    CASE llPOErr
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
      'Program Allows importing one PO per file. Please make sure that you have details of one PO in the source file.')
    CASE llInvErr
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
      'Program Allows importing one shipment per file. Please make sure that you have details of one shipment with one supplier invoice bunber in the source file.')
    CASE llQtyErr   
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
      'total breakdown Qty does not match with total Qty on the bulk receiving.')
    CASE llTranErr 
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
      'Can not locate a bulk receiving transaction for PO number : '+lcPoNum)
    CASE llRollErr 
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
       'Can not locate a bulk receiving for Invoice number : '+lcInvNum)
    CASE llColrErr
      = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
       'Color codes on excel sheet does not match bulk receving color code.')
    CASE llRollFnd
      = gfModalGen('QRM36064B00000','ALERT',ALLTRIM(lcRollID))       
   ENDCASE
  _CUROBJ = OBJNUM(lcPathName)
ELSE
  =lfSaveData()    
ENDIF

USE IN &lcXls

*!**************************************************************************
*!* Name        : lfOpenFls
*!* Developer    : Hend Ghanem (HBG)
*!* Date         : 03/01/2005
*!* Purpose     : Open needed files
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfOpenFls()
*!***************************************************************************
FUNCTION lfOpenFls

=gfOpenFile(gcDataDir+'POFLN','POFLN','SH')
=gfOpenFile(gcDataDir+'ROLLS','ROLLS','SH')
*-- end of lfOpenFls.

*!**************************************************************************
*!* Name        : lfCloseFls
*!* Developer    : Hend Ghanem (HBG)
*!* Date         : 03/01/2005
*!* Purpose     : Close temp files
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  =lfCloseFls()
*!***************************************************************************
FUNCTION lfCloseFls

IF USED(lcTmpRolls)
  USE IN (lcTmpRolls)
ENDIF
ERASE (gcWorkDir+lcTmpRolls+'.DBF')
ERASE (gcWorkDir+lcTmpRolls+'.CDX')

IF USED(lcXls)
  USE IN (lcXls)
ENDIF
ERASE (gcWorkDir+lcXls+'.DBF')

*-- End of Function lfCloseFls.
*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer    : Hend Ghanem (HBG)
*!* Date         : 03/01/2005
*!* Purpose     : Save temp file to Actual orders
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfSaveData()
*!***************************************************************************
FUNCTION lfSaveData

SELECT (lcSumRolls)
SCAN 
  SET ORDER TO ROLLS IN ROLLS
  IF SEEK(PADR(RollID,20)+PADR(Item,7)+PADR(Color,6),'ROLLS','ROLLS')
    SElECT ROLLS
    DELETE
  ENDIF
ENDSCAN

SELECT (lcTmpRolls)
SCAN 
  SCAT MEMVAR MEMO
  INSERT INTO ROLLS FROM MEMVAR
ENDSCAN

=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Material receiving is completed successfully.')
lcPathName = ""
SHOW GET lcPathName DISABLE
SHOW GET PbProceed  DISABLE
_CUROBJ = OBJNUM(lcPathName)
  
*-- End of lfSaveData.
*!*************************************************************
*!* Name      : lfCratTemp
*!* Developer : Hend Ghanem (HBG)
*!* Date      : 03/01/2005
*!* Purpose   : Create temp files
*!*************************************************************
*!* Calls     : gfCrtTmp
*!*************************************************************
*!* Passed Parameters  :  None
*!*************************************************************
*!* Returns            :  None
*!*************************************************************
*!* Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp

PRIVATE laFileStru,lnFileStru,laIndex,lnAlias
lnAlias = SELECT()
SELECT ROLLS
=AFIELDS(laFileStru)
DECLARE laIndex[1,2]
laIndex[1,1] = 'crollid+crollitem+color+trancd'
laIndex[1,2] = 'ROLLS'
=gfCrtTmp(lcTmpRolls,@laFileStru,@laIndex)
SET ORDER TO TAG 'ROLLS' IN (lcTmpRolls)

DIMENSION laFileStru[4,4]
laFileStru[1,1] = 'Item'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 7
laFileStru[1,4] = 0
laFileStru[2,1] = 'RollID'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 20
laFileStru[2,4] = 0
laFileStru[3,1] = 'Color'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 6
laFileStru[3,4] = 0
laFileStru[4,1] = 'Qty'
laFileStru[4,2] = 'N'
laFileStru[4,3] = 12
laFileStru[4,4] = 3
DECLARE laIndex[1,2]
laIndex[1,1] = 'ITEM+RollID+color'
laIndex[1,2] = 'ROLLS'
=gfCrtTmp(lcSumRolls,@laFileStru,@laIndex)


SELECT(lnAlias)
*-- end of lfCratTemp.

*!**************************************************************************
*!* Name        : lfAddUsrDt
*!* Developer    : Hend Ghanem (HBG)
*!* Date         : 03/01/2005
*!* Purpose     : Add user id ,date  and time to files08/01/2002
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfAddUsrDt()
*!***************************************************************************
FUNCTION lfAddUsrDt
PARAMETERS lcAlias
PRIVATE lcCurrAls
lcCurrAls = SELECT()
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
REPLACE cAdd_User WITH gcUser_ID,;
        dAdd_Date WITH gdSysDate,;
        cAdd_Time WITH Time()
        
SELECT (lcCurrAls)        
*-- end of lfAddUsrDt.

