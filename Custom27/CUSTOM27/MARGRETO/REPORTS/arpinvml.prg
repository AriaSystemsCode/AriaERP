*:***************************************************************************
*: Program file  : ARPINVML
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR MARGARET O`LEARY
*! Date          : 07/26/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVML
*:***************************************************************************
*: This Report Program is due to C101521 ...
*:***************************************************************************

*-- Section select the invoices match criteria and copy it to the lcInvTemp

CREAT TABLE (gcWorkDir+lcPagesno) ( KeyField C(10),GrpPageCnt n(15) ,Inv1 c(6) ,Inv2 c(6)      ,;
             Inv3 c(6) , Inv4 c(6) , Inv5 c(6) ,Inv6 c(6) , Inv7 c(6) , Inv8 c(6) , Inv9 c(6) ,;
             Inv10 c(6),Pik1 c(6) ,Pik2 c(6) , Pik3 c(6)  , Pik4 c(6) , Pik5 c(6) , Pik6 c(6) ,;
             Pik7 c(6) ,Pik8 c(6) , Pik9 c(6) ,Pik10 c(6) , mGrpInv M(10), nRecBforN N(15) ,;
             nDiscount n(13,2) , nFreight n(13,2) , nOther n(13,2) , nTotalChg n(13,2))
             
INDEX ON KeyField TAG (lcPagesno)  ADDITIVE

SELECT InvHdr
=AFIELDS(laFileStru)
PRIVATE lnI
lnI = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'KeyField'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

*man
*=gfCrtTmp(lcInvTemp,@laFileStru,'Invoice',lcInvTemp)
CREATE TABLE (gcWorkDir+lcInvTemp) FROM ARRAY laFileStru
INDEX ON keyfield+invoice+invline.style TAG invkey of (gcWorkDir+lcInvTemp) 
INDEX ON INVOICE TAG invoice of (gcWorkDir+lcInvTemp) ADDITIVE

PRIVATE lcRelExp,lcSkipExp
SELECT InvHdr
SET RELATION TO Invoice INTO InvLine
SET SKIP TO InvLine

SET RELATION TO 'O'+ ORDER INTO ORDHDR ADDI
PRIVATE lcKeyExp

SELECT InvHdr
lcCurScale = " "
lcInvoice  = SPACE(1)
lcPiktkt   = SPACE(1)
SCAN FOR &lcRpExp
*  lcKeyExp = SYS(2007,INVHDR.ACCOUNT     + INVHDR.STORE       + INVHDR.CTERMCODE     +;
                      INVHDR.SHIPVIA     + INVHDR.CUSTPO      + DTOS(INVHDR.SHIPDATE)+;
                      CUSTOMER.CADDRESS1 + CUSTOMER.CADDRESS2 + CUSTOMER.CADDRESS3   +;
                      CUSTOMER.CADDRESS4 + CUSTOMER.CADDRESS5 + CUSTOMER.CADDRESS6   +;
                      CUSTOMER.CADDRESS12+ CUSTOMER.CADDRESS22+ CUSTOMER.CADDRESS32  +;
                      CUSTOMER.CADDRESS42+ CUSTOMER.CADDRESS52+ CUSTOMER.CADDRESS62) 
  lcKeyExp = SYS(2007,INVHDR.ACCOUNT     + INVHDR.STORE       + INVHDR.CTERMCODE     +;
                      INVHDR.SHIPVIA     + INVHDR.CUSTPO      + DTOS(INVHDR.SHIPDATE)+;
                      CUSTOMER.CADDRESS1 + CUSTOMER.CADDRESS2 + CUSTOMER.CADDRESS3   +;
                      CUSTOMER.CADDRESS4 + CUSTOMER.CADDRESS5 + CUSTOMER.CADDRESS6   +;
                      ORDHDR.CADDRESS1   + ORDHDR.CADDRESS2   + ORDHDR.CADDRESS3     +;
                      ORDHDR.CADDRESS4   + ORDHDR.CADDRESS5   + ORDHDR.cDivision)                       
  IF !SEEK(InvHdr.Invoice,lcInvTemp)
    SCATTER MEMVAR MEMO
    m.KeyField = lcKeyExp
    SELECT(lcInvTemp)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
  =lfPageCount()
  lcCurScale = Style.Scale
  lcInvoice  = InvHdr.Invoice
  lcPiktkt   = InvHdr.Piktkt
ENDSCAN
*MAN
SET ORDER TO TAG INVKEY IN (lcInvTemp)
*--Select the tempfile to related it to the INVLINE file
SELECT InvHdr
*MAN
SET RELATION OFF INTO INVLINE 
*SET RELATION TO
SELECT (lcInvTemp)
GO TOP
SET RELATION TO KeyField INTO (lcPagesNo) ADDITIVE
SET RELATION TO &lcInvTemp..INVOICE INTO INVHDR ADDITIVE
SET RELATION TO &lcInvTemp..INVOICE INTO INVLINE ADDITIVE
SET SKIP TO InvLine
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

*!*************************************************************
*! Name        : lfPageCount
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 07/26/1999
*! Purpose     : To know the number of the lines and specify the pages
*!*************************************************************
*! Called from : ARPINV.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPageCount()
*!*************************************************************

FUNCTION lfPageCount

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT (lcPagesno)
IF !SEEK(lcKeyExp)
  APPEND BLANK
  REPLACE KeyField WITH lcKeyExp
ENDIF

lnCurMem = SET("MEMOWIDTH")
SET MEMOWIDTH TO 95
lnMemLin = MEMLINES(mGrpInv)

REPLACE GrpPageCnt WITH GrpPageCnt + IIF(Style.Scale <> lcCurScale,2,1) ,;
        nRecBforN  WITH nRecBforN  + IIF(Style.Scale <> lcCurScale,2,1)

*IF InvHdr.Invoice <> lcInvoice
*  REPLACE mGrpInv WITH mGrpInv + IIF(SEEK ('C'+ INVOICE ,'NOTEPAD' ),NOTEPAD.MNOTES,'')
*  REPLACE GrpPageCnt WITH GrpPageCnt - lnMemLin + MEMLINES(mGrpInv)
*ENDIF

*bas
IF llRpInvNot                       &&Variable hold the notepad
  IF InvHdr.Invoice <> lcInvoice
    REPLACE mGrpInv WITH mGrpInv + IIF(SEEK ('C'+ INVOICE ,'NOTEPAD' ),NOTEPAD.MNOTES,'')
    REPLACE GrpPageCnt WITH GrpPageCnt - lnMemLin + MEMLINES(mGrpInv)
  ENDIF
ENDIF

IF InvHdr.Invoice <> lcInvoice
  REPLACE nDiscount  WITH nDiscount  + INVHDR.Discount                              ,;
          nFreight   WITH nFreight   + INVHDR.Freight                               ,;
          nOther     WITH nOther     + INVHDR.Cod +INVHDR.Insur +INVHDR.Tax_amt     ,;
          nTotalChg  WITH nTotalChg  + INVHDR.TotalChg   
ENDIF        
*bas
  
SET MEMOWIDTH TO lnCurMem

IF InvHdr.Invoice <> lcInvoice
  FOR lnI = 1 TO 10
    lcInvFld = 'Inv'+ALLTRIM(STR(lnI))
    IF EMPTY(&lcInvFld)
      REPLACE &lcInvFld WITH &lcInvTemp..Invoice
      EXIT
    ENDIF
  ENDFOR
ENDIF        
IF !EMPTY(InvHdr.Piktkt) .AND. InvHdr.Piktkt <> lcPiktkt
  FOR lnI = 1 TO 10
    lcPikFld = 'Pik'+ALLTRIM(STR(lnI))
    IF EMPTY(&lcPikFld)
      REPLACE &lcPikFld WITH &lcInvTemp..Piktkt
      EXIT
    ENDIF
  ENDFOR
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfSolShp
*! Developer : Bassem Rafaat (BWA)
*! Date      : 07/26/1999
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************

FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lnCusRecNo

DECLARE laSoldTo[5,1] , laShipTo[5,1] 
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

laSoldTo[1] = CUSTOMER.cAddress12
laSoldTo[2] = CUSTOMER.cAddress22
laSoldTo[3] = CUSTOMER.cAddress32
laSoldTo[4] = CUSTOMER.cAddress42
laSoldTo[5] = CUSTOMER.cAddress52
=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
  
ELSE 
  
  laShipTo[1] = CUSTOMER.cAddress1
  laShipTo[2] = CUSTOMER.cAddress2
  laShipTo[3] = CUSTOMER.cAddress3
  laShipTo[4] = CUSTOMER.cAddress4
  laShipTo[5] = CUSTOMER.cAddress5
ENDIF 

=lfAdrShift('laShipTo')
*MAN
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lnCusRecNo = RECNO('CUSTOMER')
lcCustVend = LOOKUP(CUSTOMER.cCusVend, 'M'+ORDHDR.Account,CUSTOMER.Account, 'CUSTOMER')
IF BETWEEN(lnCusRecNo, 1, RECCOUNT('CUSTOMER'))
  GO lnCusRecNo IN CUSTOMER
ENDIF  

RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

********************************
FUNCTION lfPageNo
PARAMETERS lcDumdy
lnPage=lnPage+1
lnPageNo = Ceiling(lnCount/25)

***************
FUNCTION lfEndGrp
PARAMETERS lcDumdy
lnEndGrpPa = lnPage
