*:************************************************************************
*: Procedure file: ARPINVLU.PRG for LUC10
*:         System: ARIA 2.4
*:         Module: Accounts Recevible
*:         Author: AYMAN MAHMOUD AHMED
*:  
*:
*:************************************************************************
*C200788,1 TMI 05/14/2007 Allow to show style.ccomcode for all countries,
*                         Show the invoice note before the shortage message
*:***************************************************************************

IF !llIsAparel
  lcskpexpr="INVLINE , SPCK_LIN"
ENDIF
IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
   lcskpexpr="(lcTmpDbt) , INVLINE , ARINSTMD"
  ENDIF
ELSE
  IF !llIsAparel
    lcskpexpr='INVLINE'
  ENDIF
ENDIF


*! Cursors to get diviosn data and image build .... Begin
SELECT CCODE_NO,CDISCREP FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTFIELD='N' INTO CURSOR CODTAB READWRITE 
SELECT CODTAB
INDEX ON CCODE_NO TAG CODS
SELECT CCODE_NO,CDISCREP,CRLTD_VLU FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTD_NAM='CDIVIMGP' INTO CURSOR CODIMG READWRITE 
SELECT CODIMG
INDEX ON CCODE_NO TAG CODS
*! Cursors to get diviosn data and image build .... End

*! Cursor to get data in required index build .... Begin
SELECT * FROM INVLINE WHERE INVOICE='******' INTO CURSOR TINVLINE READWRITE 
SELECT TINVLINE 
SET RELATION to
INDEX ON INVOICE+STYLE TAG INVSTY

SELECT * FROM NOTEPAD INTO CURSOR NOTTAB READWRITE 
SELECT NOTTAB 
INDEX ON KEY TAG KKEY
*! Cursor to get data in required index build .... End

SELECT InvHdr
SET SKIP to

*! Fill TINVLINE with filtered data .... Begin
IF LCRPPRST='N'
  LCRPPRST=SPACE(1)
ENDIF

SCAN FOR EVALUATE(LCRPexp)
LCINV=invoice
SELECT * FROM invline WHERE invline.invoice=LCINV INTO CURSOR templine READWRITE 
SELECT tinvline
APPEND FROM DBF('templine') 
ENDSCAN
*! Fill TINVLINE with filtered data .... End

IF USED(lcTmpDbt) AND RECCOUNT(lcTmpDbt)>0
  SELECT Invhdr
  SET RELATION OFF INTO invline
  SELECT (lcTmpDbt)
  SET RELATION OFF INTO invline
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO TINVLINE ADDITIVE
  
ELSE
  SELECT Invhdr
  SET RELATION OFF INTO invline
  SET RELATION TO invoice INTO tinvline ADDITIVE 
endif  

SELECT Invhdr
SET RELATION TO CDIVISION INTO CODIMG ADDITIVE 
SET RELATION TO CDIVISION INTO CODTAB ADDITIVE 
SET RELATION TO Invhdr.CCURRCODE+"/"+CUSTOMER.CCONT_CODE INTO NOTTAB ADDITIVE 


SELECT invline
SET RELATION to
SELECT STYLE
SET RELATION to

SELECT tinvline 

SET ORDER TO TAG STYLE IN STYLE
SET RELATION TO IIF(!EMPTY(tinvline.ALTSTYLE) , tinvline.ALTSTYLE ,tinvline.Style) INTO STYLE
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
SET RELATION TO "S" + tinvline.Account + tinvline.Style INTO SPCK_LIN ADDITIVE
SET RELATION TO  INVOICE+STR(LINENO,6) INTO INVLINE ADDITIVE 

SELECT STYLE
SET ORDER TO TAG SCALE IN SCALE
SET RELATION TO 'S' + Scale INTO SCALE

IF !USED('SINTCUR')
  lcSelectCommand1 = [SELECT CCONT_CODE,LEUROPCOM FROM SYCINT ]
  lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand1,"","SINTCUR","",;
      oAriaApplication.SystemConnectionString,3,"")
  SELECT SINTCUR 
  =CURSORSETPROP("Buffering" ,3,'SINTCUR')
  INDEX ON CCONT_CODE TAG CCONT_CODE
  *C200788,1 TMI [Start] set the relation below
  *SELECT CUSTOMER
  *SET RELATION TO CCONT_CODE INTO  SINTCUR  
  *C200788,1 TMI [End  ] 
ENDIF

*C200788,1 TMI [Start] Check the relation every time the report run
SELECT CUSTOMER
IF !'CCONT_CODE INTO SINTCUR'$UPPER(SET('RELATION'))
  SET RELATION TO CCONT_CODE INTO SINTCUR ADDITIVE
ENDIF
SELECT INVHDR
IF !'"C"+INVOICE INTO NOTEPAD'$UPPER(SET('RELATION'))
  SET RELATION TO 'C'+INVOICE INTO NOTEPAD ADDITIVE
ENDIF
*C200788,1 TMI [End  ] 

SELECT Invhdr
lcskpexpr=STRTRAN(lcskpexpr,'invline','tinvline',-1,-1,1)
SET SKIP TO &lcskpexpr
GO top
