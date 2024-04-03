*:**********************************************************************
*: Procedure file      : ARPINVHK.PRG
*: Program description :
*: Module              : Accounts Receivable
*: Developer           : Heba Fathi (HFK)
*: Tracking Job Number : #C200482
*: Date                : 07/19/2004
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*! modifications
*! B129226,1 HFK 08/17/2005 ERORR when preview then print from PDF
*:**********************************************************************
#INCLUDE R:\a4xpdemo\Aria4xp\reports\arpinvhk.h

IF !USED('WAREHOUS')
  =gfOpenFile(oAriaApplication.DataDir + "WAREHOUS" , "WAREHOUS" , 'SH')
ENDIF

IF !USED('SPCK_HDR')
  =gfOpenFile(oAriaApplication.DataDir + "SPCK_HDR" , "SPCK_HDR" , 'SH')
ENDIF

IF !USED('SYCCURR')
  =gfOpenFile(oAriaApplication.SysPath + "SYCCURR" , "CCURRCODE" , 'SH')
ENDIF

IF !USED('CUTPICK')
  =gfOpenFile(oAriaApplication.DataDir + "CUTPICK" , "CUTORD" , 'SH')
ENDIF

IF !USED('ORDLINE')
  =gfOpenFile(oAriaApplication.DataDir + "ORDLINE" , "ORDLINE" , 'SH')
ENDIF


*-- Search for company Phone number in laCompAdd
PRIVATE lnPhonePos , lcTaxRefr
lnPhonePos = ASCAN(laCompAdd,TRANSFORM(lcCompPhon , lcPhonPict))
IF lnPhonePos > 0
  laCompAdd[lnPhonePos] = laCompAdd[lnPhonePos]
ENDIF

*-- Case the user select sort by style.
SELECT InvHdr

LOCATE FOR &lcRpExp
IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  *-=gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SELECT INVLINE
=AFIELDS(laFileStru)
IF ASCAN(laFileStru,"CTKTNO") = 0

  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,18]

  laFileStru[lnFileStru +1,1] = 'CTKTNO'
  laFileStru[lnFileStru +1,2] = 'C'
  laFileStru[lnFileStru +1,3] = 6
  laFileStru[lnFileStru +1,4] = 0

  laFileStru[lnFileStru +2,1] = 'CustPO'
  laFileStru[lnFileStru +2,2] = 'C'
  laFileStru[lnFileStru +2,3] = 15
  laFileStru[lnFileStru +2,4] = 0

  FOR lnI = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru+1,lnI], laFileStru[lnFileStru+2,lnI]
  ENDFOR 
ENDIF 

lcStylIndx = loOGScroll.gfTempName()
gfCrtTmp(lcStylIndx,@laFileStru,,"",.F.)
SELECT &lcStylIndx
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINE OF (oAriaApplication.WorkDir + INVLINE + '.CDX')


*Copy Invoice header structure. [Begin] 
SELECT INVHDR
=AFIELDS(laFileStru)
lcInvHdr = loOGScroll.gfTempName()
gfCrtTmp(lcInvHdr,@laFileStru,,"",.F.)
SELECT &lcInvHdr
INDEX ON INVOICE TAG INVHDR OF (oAriaApplication.WorkDir + lcInvHdr+ '.CDX')
*Copy Invoice header structure. [End]

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE

PRIVATE lnConsLine , lcDateCent , lcInvLnTag
lcDateCent = SET('CENTURY')
SET CENTURY ON

lnConsLine = 0
SCAN FOR &lcASExp

  *-lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR.INVOICE
  lnConsLine = 0
  IF INVHDR.CONSOL = 'Y'
     
    =SEEK(INVHDR.Invoice,'CONSINVH') AND SEEK('O'+CONSINVH.ORDER,'ORDHDR')

    *-Scatter Invoice records. [Begin]
    SCATTER MEMVAR MEMO
    m.CLOC_NO   = ORDHDR.CLOC_NO
    m.DLOCISSUE = ORDHDR.DLOCISSUE 
    m.CLOCBANK  = ORDHDR.CLOCBANK     
    SELECT(lcInvHdr)
    IF !SEEK(lcInvcee)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDIF  
    *-Scatter Invoice records. [End]    
    
    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = lcInvcee
      *-WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      WAIT WINDOW LANG_ARPINVHK_Select + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
  
      STORE '' TO m.CTKTNO , m.CustPO

      IF SEEK("2"+CONSINVL.order+STR(m.LineNo,6),'Cutpick')
        m.CTKTNO   = Cutpick.CTKTNO
      ENDIF
      IF SEEK('O'+ CONSINVL.order+STR(m.lineno,6),'Ordline')
        m.CustPO   = Ordline.CustPO
      ENDIF
      m.LineNo     = lnConsLine
      
      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')
      IF BETWEEN(lnRecNoSt,1,RECCOUNT('STYLE'))
        GOTO lnRecNoSt IN STYLE
      ENDIF  
      SELECT InvLine
      lcInvLnTag = TAG()
      SET ORDER TO TAG InvLines
      m.Desc1 = IIF(SEEK(m.Style+m.Invoice),Desc1,'')
      SET ORDER TO TAG (lcInvLnTag)
      
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

    ENDSCAN
  ELSE

    *-Scatter Invoice records. [Begin]
    =SEEK('O'+INVHDR.ORDER,'ORDHDR')
    SCATTER MEMVAR MEMO
    m.CLOC_NO   = ORDHDR.CLOC_NO
    m.DLOCISSUE = ORDHDR.DLOCISSUE 
    m.CLOCBANK  = ORDHDR.CLOCBANK     
    SELECT(lcInvHdr)
    IF !SEEK(lcInvcee)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDIF  
    *-Scatter Invoice records. [End]

    SELECT INVLINE
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      *-WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      WAIT WINDOW LANG_ARPINVHK_Select + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      
      STORE '' TO m.CTKTNO , m.CustPO
      IF SEEK("2"+InvLine.order+STR(m.LineNo,6),'Cutpick')
        m.CTKTNO   = Cutpick.CTKTNO
      ENDIF

      IF SEEK('O'+ InvLine.order+STR(m.lineno,6),'Ordline')
        m.CustPO = Ordline.CustPO
      ENDIF

      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

    ENDSCAN
  ENDIF
  
ENDSCAN

SET CENTURY &lcDateCent

             *-- Section break the relatoins --*
SELECT InvHdr

SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CONSINVL

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

*--Clos the invline and open the temp. file with the invline name.

SELECT INVLINE

*-- B129226,1 HFK [Start]
*!*  CLOSE INDEX
*!*  USE IN INVLINE
*!*  USE IN (lcStylIndx)
*!*  USE (oAriaApplication.WorkDir+lcStylIndx) IN 0 ALIAS INVLINE EXCLUSIVE

*-CLOSE INDEX
USE IN INVLINE
*-USE IN (lcStylIndx)
USE (oAriaApplication.WorkDir+lcStylIndx) AGAIN IN 0 ALIAS INVLINE EXCLUSIVE
*-- B129226,1 HFK [Start]

SELECT INVLINE
SET INDEX TO (oAriaApplication.WorkDir+INVLINE+'.cdx')
SET ORDER TO TAG Invline OF (oAriaApplication.WorkDir+InvLine+'.cdx')

USE IN INVHDR
USE IN (lcInvHdr)

USE (oAriaApplication.WorkDir+lcInvHdr) AGAIN IN 0 ALIAS INVHDR EXCLUSIVE
SELECT INVHDR
INDEX ON INVOICE TAG INVHDR OF (oAriaApplication.WorkDir + lcInvHdr + '.CDX')

             *-- Section Create the new relatoins --*
SELECT INVHDR

IF llPrntInst .OR. llRpInvNot
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  IF cFile_Num <> '1'
    REPLACE cFile_Num WITH '1'     && add this line of code to always print Invoice Notepad only.
  ENDIF  
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
LOCATE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE , INVLINE.Style) INTO STYLE ADDITIVE
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE
SET RELATION TO INVHDR.CWARECODE INTO WAREHOUS ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , INVLINE
ELSE
  SET SKIP TO INVLINE
ENDIF
SELECT INVHDR
*-hfk, 05/10/2005
lcRecCount = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'Selected &lcRecCount RECORDS FOR REPORT...' TIMEOUT 1
*-hfk, 05/10/2005
loogScroll.cCROrientation = 'P'
lcoldExp = loOGScroll.lcRpExp
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
_screen.Visible = .T.  

SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

SELECT InvHdr
CLOSE INDEX
USE IN InvHdr
=gfOpenFile(oAriaApplication.DataDir+"InvHdr","InvHdr",'SH')

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(oAriaApplication.DataDir+"InvLine","InvLine",'SH')

                     *-- End of the Program --*
