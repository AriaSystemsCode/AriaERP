*:***************************************************************************
*: Program file  : ALPKTKML
*: Program desc. : ALLOCATION PIKTICKT FOR MARGARET 
*! Date          : 08/31/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKML
*:***************************************************************************
*: This Report Program is due to C101558 ...
*:***************************************************************************

CREAT TABLE (gcWorkDir+lcPagesno) ( KeyField C(10),GrpPageCnt n(3) ,Ord1 c(6) ,Ord2 c(6)      ,;
             Ord3 c(6) , Ord4 c(6) , Ord5 c(6) ,Ord6 c(6) , Ord7 c(6) , Ord8 c(6) , Ord9 c(6) ,;
             Ord10 c(6),Pik1 c(6) ,Pik2 c(6) , Pik3 c(6)  , Pik4 c(6) , Pik5 c(6) , Pik6 c(6) ,;
             Pik7 c(6) ,Pik8 c(6) , Pik9 c(6) ,Pik10 c(6) , mGrpOrdNot M(10), nRecBforN N(15))
INDEX ON KeyField TAG (lcPagesno)  ADDITIVE


SELECT (lcTmpOrdL)
=AFIELDS(laFileStru)
PRIVATE lnI
lnI = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'KeyField'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

CREATE TABLE (gcWorkDir+lcPikTemp) FROM ARRAY laFileStru
INDEX ON keyfield+Piktkt+Ordline.Style TAG ORDKEY of (gcWorkDir+lcPikTemp)
INDEX ON PIKTKT TAG PIKTKT of (gcWorkDir+lcPikTemp) ADDITIVE


IF !USED(lcInvHdr)
  =gfOpenFile(gcDataDir+"Invhdr","Invhdr",'SH', @lcInvHdr, .T.)
ENDIF
IF !USED(lcInvLine)  
  =gfOpenFile(gcDataDir+"Invline","InvLineO", 'SH', @lcInvLine, .T.)
ENDIF  

PRIVATE lcKeyExp
SELECT (lcTmpOrdL)
lcCurScale =  ' '
lcPiktkt   =  SPACE(1)
lcOrder    =  SPACE(1)
SCAN FOR &lcRpExp
  lcKeyExp = SYS(2007,ORDHDR.ACCOUNT     + ORDHDR.STORE       + ORDHDR.CTERMCODE                 +;
                      ORDHDR.SHIPVIA     + ORDHDR.CUSTPO      + DTOS(EVAL(LCINVHDR+'.SHIPDATE')) +;
                      CUSTOMER.CADDRESS1 + CUSTOMER.CADDRESS2 + CUSTOMER.CADDRESS3               +;
                      CUSTOMER.CADDRESS4 + CUSTOMER.CADDRESS5 + CUSTOMER.CADDRESS6               +;
                      ORDHDR.CADDRESS1   + ORDHDR.CADDRESS2   + ORDHDR.CADDRESS3                 +;
                      ORDHDR.CADDRESS4   + ORDHDR.CADDRESS5   + ORDHDR.cDivision)                       
  IF !SEEK(&lcTmpOrdL..PIKTKT,lcPikTemp) .AND. cGrupDetal = 'D'
    SCATTER MEMVAR MEMO 
    m.KeyField = lcKeyExp
    SELECT (lcPikTemp)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
  IF cGrupDetal = 'D'
    =lfPageCount()
    lcCurScale = Style.Scale
    lcPiktkt   = &lcTmpOrdL..PIKTKT
    lcOrder    = &lcTmpOrdL..Order
    IF lcKeyExp <> KeyField
      lcCurScale =  ' '
    ENDIF  
  ENDIF
ENDSCAN

SET ORDER TO TAG ORDKEY IN (lcPikTemp)
SELECT (lcPikTemp)
GO TOP 
SET RELATION TO KeyField INTO (lcPagesNo)      ADDITIVE
SET RELATION TO &lcPikTemp..PIKTKT INTO PIKTKT ADDITIVE
SET RELATION TO Style INTO STYLE               ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE         ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR        ADDITIVE
SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE

*--RELATION TO GET THE INVOICE.SHIPDAT
SELECT ORDLINE
SET RELATION TO ORDLINE.ORDER + STR(LineNo,6) INTO &LCINVLINE ADDITIVE
SELECT (LCINVLINE)
SET RELATION TO EVAL(LCINVLINE+'.INVOICE') INTO &LCINVHDR ADDITIVE

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcPikTemp)
GO TOP

*!*************************************************************
*! Name        : lfPageCount
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/31/1999
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
lnMemLin = MEMLINES(mGrpOrdNot)

REPLACE GrpPageCnt WITH GrpPageCnt + IIF(Style.Scale <> lcCurScale,2,1) ,;
        nRecBforN  WITH nRecBforN  + IIF(Style.Scale <> lcCurScale,2,1)
        
IF llRpOrdNot
  IF &lcTmpOrdL..PIKTKT <> lcPiktkt
    REPLACE mGrpOrdNot WITH mGrpOrdNot + IIF(SEEK ('B'+ ORDER ,'NOTEPAD' ),NOTEPAD.MNOTES,'')
    REPLACE GrpPageCnt WITH GrpPageCnt - lnMemLin + MEMLINES(mGrpOrdNot)
  ENDIF
ENDIF
SET MEMOWIDTH TO lnCurMem

IF &lcTmpOrdL..PIKTKT <> lcPiktkt
  FOR lnI = 1 TO 10
    lcPikFld = 'Pik'+ALLTRIM(STR(lnI))
    IF EMPTY(&lcPikFld)
      REPLACE &lcPikFld WITH &lcTmpOrdL..PIKTKT
      EXIT
    ENDIF
  ENDFOR
ENDIF        

IF &lcTmpOrdL..Order <> lcOrder
  FOR lnI = 1 TO 10
    lcOrdFld = 'Ord'+ALLTRIM(STR(lnI))
    IF EMPTY(&lcOrdFld)
      REPLACE &lcOrdFld WITH &lcTmpOrdL..Order
      EXIT
    ENDIF
  ENDFOR
ENDIF

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfSolShp
*! Developer : Bassem Rafaat (BWA)
*! Date      : 08/31/1999
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

SELECT CUSTOMER
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

lcTerms    = gfCodDes(ORDHDR.cTermCode , 'CTERMCODE')
lcShipVia  = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA')
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

*!*************************************************************
*! Name      : lfPageNo
*! Developer : Bassem Rafaat (BWA)
*! Date      : 08/31/1999
*! Purpose   : Function to Get the Page #
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnPageNo
*!*************************************************************

FUNCTION lfPageNo
PARAMETERS lcDumdy
lnPage=lnPage+1
lnPageNo = Ceiling(lnCount/25)

*!*************************************************************
*! Name      : lfPageNo
*! Developer : Bassem Rafaat (BWA)
*! Date      : 08/31/1999
*! Purpose   : Function to save the Page #
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnPageNo
*!*************************************************************

FUNCTION lfEndGrp
PARAMETERS lcDumdy
lnEndGrpPa = lnPage

