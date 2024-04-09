****************************************************************************
*: Program file      : SOFPODT.PRG
*: Program desc.     : FRESH PRODUCE CUSTOM ORDER DETAIL
*: System            : Aria Apparel System (A27).
*: Module            : (SO)
*: Developer         : TAK
*: Date              : 10/20/2000
*:**************************************************************************

*--Grid Files,
STORE  '' TO lcAccFile,lcStyFile
STORE .F. TO llActSel,llStysel


WAIT WINDOW 'Start Collecting Orders...' NOWAIT

*-- To check if there is any selected Accounts.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcAccFile) AND USED(lcAccFile)
    SELECT (lcAccFile)
    GO TOP
    llActSel = !EOF()
  ENDIF  
ENDIF


*-- To check if there is any selected Styles major.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GO TOP
    llStysel= !EOF()
  ENDIF  
ENDIF


*--Default the order status to Open and Hold if not selected.
IF ! ('ORDHDR.STATUS' $ lcRpExp)
  lcRpExp = lcRpExp + " AND ORDHDR.STATUS $ 'OH' "
ENDIF  


IF lcDateType <> 'S'  AND OCCURS('ORDHDR.START',lcRpExp) <> 0
  lcRpExp = STRTRAN(lcRpExp,'ORDHDR.START','ORDHDR.'+IIF(lcDateType = 'C','COMPLETE','ENTERED'))
ENDIF


lcTmpOrd = gfTempName()

SELECT STYLE 
SET ORDER TO TAG STYLE

SELECT ORDLINE
SET RELATION TO Style INTO Style
SET RELATION TO 'M'+Account INTO CUSTOMER ADDI
  
COPY STRU TO (gcWorkDir+lcTmpOrd)

=gfOpenFile(gcWorkDir+lcTmpOrd,'','EX')

*--Dyelot field used for Rep1.
DO CASE 
  CASE lcRpSort = 'S'  
    INDEX ON Style+Dyelot+Order+STR(LineNo,6) TAG (lcTmpOrd)
  CASE lcRpSort = 'R'  
    INDEX ON Dyelot+Style+Order+STR(LineNo,6) TAG (lcTmpOrd)
ENDCASE  


IF llStysel
  SELECT ORDLINE
  SET ORDER TO TAG Ordlines
  SET RELATION TO Account+cordtype+order INTO ORDHDR ADDI
  SELECT (lcStyFile)
  SCAN
    lcMStyle =  PADR(cStyMajor,12)
    SELECT ORDLINE
    =SEEK(lcMStyle)
    SCAN WHILE Style = lcMStyle FOR &lcRpExp
       WAIT WINDOW 'Collecting Orders :'+Order+' / '+Style NOWAIT
       =lfUpdTmp()
    ENDSCAN
  ENDSCAN  
ELSE
  IF llActSel
    SELECT ORDLINE
    SET ORDER TO TAG Ordline
    SELECT (lcAccFile)
    SCAN
      lcAccount=Account 
      SELECT ORDHDR 
      =SEEK(lcAccount) 
      SCAN WHILE Account = lcAccount
         SELECT ORDLINE
         =SEEK('O'+ORDHDR.Order)
         SCAN WHILE cordtype+order+STR(lineno,6) = 'O'+ORDHDR.Order FOR &lcRpExp
           WAIT WINDOW 'Collecting Orders :'+Order+' / '+Style NOWAIT
           =lfUpdTmp()
         ENDSCAN
      ENDSCAN
    ENDSCAN
  ELSE
    SELECT ORDLINE
    SCAN FOR &lcRpExp
       WAIT WINDOW 'Collecting Orders :'+Order+' / '+Style NOWAIT
       =lfUpdTmp()
    ENDSCAN
  ENDIF
ENDIF
WAIT CLEAR


SELECT (lcTmpOrd)
GO TOP
IF EOF()
  *--No records was selected, Nothing to print!!
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVI TO SCREEN
  RETURN
ENDIF


= gfDispRe(lcRpForm)


SELECT ORDLINE
SET RELATION TO
SELECT STYLE 
SET ORDER TO TAG CSTYLE


IF USED(lcTmpOrd)
  SELECT (lcTmpOrd)
  USE
  ERASE(gcWorkDir+lcTmpOrd+'.DBF')
  ERASE(gcWorkDir+lcTmpOrd+'.CDX')
ENDIF

RETURN

*********************************************************
FUNCTION lfUpdTmp

SCATTER MEMVAR
SELECT (lcTmpOrd)
APPEND BLANK
GATHER MEMVAR
REPLACE Dyelot    WITH ORDHDR.Rep1 ,;
        dAdd_date WITH ORDHDR.Entered,;
        Poalo1    WITH MAX(Qty1 - Pik1,0),;
        Poalo2    WITH MAX(Qty2 - Pik2,0),;               
        Poalo3    WITH MAX(Qty3 - Pik3,0),;
        Poalo4    WITH MAX(Qty4 - Pik4,0),;
        Poalo5    WITH MAX(Qty5 - Pik5,0),;
        Poalo6    WITH MAX(Qty6 - Pik6,0),;
        Poalo7    WITH MAX(Qty7 - Pik7,0),;
        Poalo8    WITH MAX(Qty8 - Pik8,0),;
        Tot_POalo WITH Poalo1+Poalo2+Poalo3+Poalo4+Poalo5+Poalo6+Poalo7+Poalo8,;
        Desc1     WITH CUSTOMER.BtName 


*********************************************************
FUNCTION lfwRepWhen



*********************************************************


