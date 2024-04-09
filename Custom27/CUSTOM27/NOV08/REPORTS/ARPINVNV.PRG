*:***************************************************************************
*: Program file  : ARPINVNV.PRG
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR NOVUM FASHION
*: Date          : 06/30/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVNV
*:***************************************************************************
*: This Report Program is due to C123359 ...
*:***************************************************************************
*:Modifications  :
*:B123712,1 ASH 07/18/2004 1- Move the company address 1 line up. (In FRX)
*:B123712,1                2- Don't print the duplicate scale code.
*:B123712,1                3- Change the font of style no, color amd description to be as size breakdows. (In FRX)
*:B123769,1 BWA 07/27/2004 Add page number and fix the Descreption.
*:***************************************************************************

SELECT INVHDR
SET SKIP TO

SELECT INVHDR
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT INVLINE
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

                           *--The Color , Style Length--*
STORE 0 TO lnClrLnNv , lnClrPosNv
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrPosNv = laItemSeg[lnCount,4]
    lnClrLnNv  = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*--The Style Length
STORE 0 TO lnLenStyNv
lnLenStyNv = LEN(gfItemMask('PM'))

                         *--End The Color , Style Length--*
              
CREATE CURSOR (lcCopisTmp) (Copy C(1))
SELECT (lcCopisTmp)

INSERT INTO (lcCopisTmp) (Copy) VALUES ('A')
INSERT INTO (lcCopisTmp) (Copy) VALUES ('B')
INSERT INTO (lcCopisTmp) (Copy) VALUES ('C')
INSERT INTO (lcCopisTmp) (Copy) VALUES ('D')
INDEX ON Copy TAG (lcCopisTmp) OF (lcCopisTmp)

*B123769,1 BWA 07/27/2004 Add page number and fix the Descreption.[START]
*--Section get the page number.
=lfPageNO()
*B123769,1 BWA 07/27/2004.[END]

SELECT INVHDR
SET RELATION TO '' INTO (lcCopisTmp) ADDITIVE

*B123769,1 BWA 07/27/2004 Add new relation.[START]
SET RELATION TO INVHDR.INVOICE INTO (lcPageNo) ADDITIVE
*B123769,1 BWA 07/27/2004.[END]

SELECT (lcCopisTmp)
SET RELATION TO INVHDR.Invoice INTO INVLINE ADDITIVE
SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account , 'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.order INTO Ordhdr ADDITIVE

SELECT INVLINE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE ,INVLINE.Style) INTO STYLE
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

*B123769,1 BWA 07/27/2004 Add new relation.[START]
SET RELATION TO INVLINE.INVOICE + STR(INVLINE.LINENO,6) INTO (lcDscrpion) ADDITIVE
*B123769,1 BWA 07/27/2004.[END]

SELECT INVHDR
SET SKIP TO (lcCopisTmp) , INVLINE

                       *-- Report Code End  --*
************************************************************************************************

                       *-- Function Section --*
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/30/2004
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

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName  

  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5

ELSE 

  lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)  
  
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF 
=lfAdrShift('laShipTo')

lcDivLName = gfCodDes(INVHDR.CDIVISION , 'CDIVISION')
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lnCusRecNo = RECNO('CUSTOMER')
lcCustVend = LOOKUP(CUSTOMER.cCusVend, 'M'+ORDHDR.Account,CUSTOMER.Account, 'CUSTOMER')
IF BETWEEN(lnCusRecNo, 1, RECCOUNT('CUSTOMER'))
  GO lnCusRecNo IN CUSTOMER
ENDIF  

RETURN ''

*--End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/30/2004
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
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--End of lfAdrShift.
*!*************************************************************
*! Name        : lfSclSze
*! Developer   : Bassem Rafaat (BWA)
*!Date          : 06/30/2004
*! Purpose     : To get the scale and the size
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C123359
*!*************************************************************
*! Example     : lfSclSze
*!*************************************************************
FUNCTION lfSclSze
PARAMETER lcSclSze
PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz

DIMENSION laScale[3,9]
STORE SPACE(0) TO laScale
lnSclCnt = 0

lcAlias = ALIAS()

SELECT INVLINE
lcKey = EVAL(KEY())

SCAN WHILE Invoice = INVHDR.INVOICE  .AND. lnSclCnt < 3
  *B123712,1 ASH 07/18/2004 (Begin) Fix bug of printing the same scale more than once.
  *IF ASCAN(laScale,INVLINE.SCALE) = 0 .AND. SEEK('S'+INVLINE.SCALE,'SCALE')
  IF ASCAN(laScale,ALLTRIM(INVLINE.SCALE)) = 0 .AND. SEEK('S'+INVLINE.SCALE,'SCALE')
  *B123712,1 ASH 07/18/2004 (End)
      lnSclCnt = lnSclCnt + 1
      laScale[lnSclCnt,1] = ALLTRIM(SCALE.Scale)
      For lnCountSc = 1 TO 8 
        lcX = STR(lnCountSc,1)
        laScale[lnSclCnt,lnCountSc+1] = ALLTRIM(SCALE.SZ&lcX)
      ENDFOR
  ENDIF
ENDSCAN


SEEK(lcKey)
SELECT (lcAlias)

RETURN ''

*-- End of lfSclSze.
*B123769,1 BWA 07/27/2004 Add page number and fix the Descreption.[START]
*!*************************************************************
*! Name      : lfPageNO
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/26/2004
*! Purpose   : Function to Get the page number.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPageNO
PRIVATE lcAlias , lcKey , lcRelaion

lcAlias = ALIAS()
lcKey = EVAL(KEY())

*--Temp file hold the pageNo.
CREAT TABLE (gcWorkDir+lcPageNo) ( Invoice C(6), PageNoFld n(10))
INDEX ON Invoice TAG Invoice of (gcWorkDir+lcPageNo)

*--Temp file hold the 2 lines of the descreption.
CREAT TABLE (gcWorkDir+lcDscrpion) ( Invoice C(6), LineNoFld N(6) , DescFld1 C(19) , DescFld2 C(50))
INDEX ON Invoice + STR(LineNoFld,6) TAG Invoice of (gcWorkDir+lcDscrpion)

SELECT INVHDR
lcRelaion = SET("RELATION")
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

STORE SPACE(0) TO lcNewInv , lcScale
lnContLn = 0

lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SCAN FOR &lcASExp
  IF !EMPTY(lcNewInv)
    IF lcNewInv # INVLINE.INVOICE
      INSERT INTO (lcPageNo) (INVOICE , PageNoFld) VALUES (lcNewInv , CEILING(lnContLn/23))
      lnContLn = 0
    ENDIF
  ENDIF
  lcNewInv = INVLINE.INVOICE

  SELECT INVLINE
  SCAN FOR INVOICE +STR(LINENO,6) = INVHDR.INVOICE
    IF lcScale # ALLTRIM(INVLINE.SCALE)
      lnContLn = lnContLn + 1
    ENDIF
    =lfgetDesc()
    =SEEK(INVLINE.INVOICE + STR(INVLINE.LINENO,6) , lcDscrpion)
    IF !EMPTY(&lcDscrpion..DescFld2)
	  lnContLn = lnContLn + 1
    ENDIF

    lcScale  = ALLTRIM(INVLINE.SCALE)  
    lnContLn = lnContLn + 1
  ENDSCAN
ENDSCAN

*--The last invoice or if the user select 1 invoice.
IF !EMPTY(lcNewInv)
  INSERT INTO (lcPageNo) (INVOICE , PageNoFld) VALUES (lcNewInv , CEILING(lnContLn/23))
ENDIF

SELECT INVHDR
SET RELATION TO &lcRelaion

SELECT (lcAlias)
SEEK(lcKey)

*--End of lfPageNO.
*!*************************************************************
*! Name      : lfgetDesc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/26/2004
*! Purpose   : Function to Get the descreption.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfgetDesc
PRIVATE lcAlias , lcKey , lcRelaion

lcAlias = ALIAS()
lcKey = EVAL(KEY())
=SEEK(IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE , INVLINE.STYLE) , "STYLE")

SELECT (lcDscrpion)
APPEND BLANK
REPLACE Invoice   WITH INVLINE.INVOICE ,;
        LineNoFld WITH INVLINE.LINENO
  
  *--20 is Imaginary number.
  lnchr = 0
  DescText = ALLTRIM(STYLE.CSTYGROUP) + " " + ALLTRIM(INVLINE.DESC1)
  FOR lnSerch = 1 TO 20

    IF ATC(" " , DescText , lnSerch) < 20        && The first field take 20 chr.
      lnchr = IIF(ATC(" " , DescText , lnSerch) > lnchr , ATC(" " , DescText , lnSerch) , lnchr)

      llcheck = (ATC(" " , DescText , lnSerch) = 0 AND LEN(DescText) < 20)
      IF ATC(" " , DescText , lnSerch) # 0 OR llcheck
        REPLACE DescFld1 WITH LEFT(ALLTRIM(STYLE.CSTYGROUP) + " " + ALLTRIM(INVLINE.DESC1) , IIF(llcheck , LEN(DescText),lnchr - 1))
        IF ATC(" " , DescText , lnSerch) = 0
          EXIT
        ENDIF
      ELSE
        REPLACE DescFld2 WITH SUBSTR(ALLTRIM(STYLE.CSTYGROUP) + " " + ALLTRIM(INVLINE.DESC1) , lnchr + 1)
        EXIT
      ENDIF
    ENDIF
  ENDFOR        

SELECT (lcAlias)
SEEK(lcKey)
      
*--End of lfgetDesc.
*B123769,1 BWA 07/27/2004.[END]