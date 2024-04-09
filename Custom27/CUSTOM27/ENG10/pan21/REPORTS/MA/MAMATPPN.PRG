*:****************************************************************************************
*: Program file  : MAMATPPN.PRG
*: Program desc. : Material Purchase Order [For Cust PAN21]
*: System        : Aria Apparel System [A27].
*: Module        : Material Module  [MA]
*: Developer     : HEND GHANEM - (HBG) 
*: Date          : 01/27/2006
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  :
*:               ------------------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#129683,1         : 
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************

DECLARE laWarHous[6]
*-- Fill Vendor array with its data [Begin]
laWarHous[1] = WareHous.cDesc
laWarHous[2] = gfGetAdr('WareHous' , '' , '' , '' , 1)
laWarHous[3] = gfGetAdr('WareHous' , '' , '' , '' , 2)
laWarHous[4] = gfGetAdr('WareHous' , '' , '' , '' , 3)
laWarHous[5] = gfGetAdr('WareHous' , '' , '' , '' , 4)
laWarHous[6] = gfGetAdr('WareHous' , '' , '' , '' , 5)

*-- Get the vendor addresses
= lfAdrShift('laWarHous')
lcWarHInfo = ALLTRIM(laWarHous[1]) + ', '
FOR lnI = 2 TO 5
  lcWarHInfo = lcWarHInfo + ALLTRIM(laWarHous[lnI]) + SPACE(1)
ENDFOR  

DECLARE laMerch[1,2] , laMerchTel[1,2] , laMerchFax[1,2],laGarTech[1,2],laGTechTel[1,2],laGTechFax[1,2],LaPurch[1,2]

STORE '' TO  laMerch,laMerchTel, laMerchFax,laGarTech,laGTechTel,laGTechFax
lcSelected = ALIAS()           && Save Current Alias.
SELECT SYDFIELD
SET ORDER TO TAG Cfld_name 
SELECT (lcSelected)            && Restore Alias.
=lfUDFDesc('CMERCH',@laMerch)
=lfUDFDesc('CMERCHTEL',@laMerchTel)
=lfUDFDesc('CMERCHFAX',@laMerchFax)
=lfUDFDesc('CGTECH',@laGarTech)
=lfUDFDesc('CGTECHTEL',@laGTechTel)
=lfUDFDesc('CGTECHFAX',@laGTechFax)
=lfUDFDesc('CPURCH',@LaPurch)
lcSelected = ALIAS()           && Save Current Alias.
SELECT SYDFIELD
SET ORDER TO 
=gfOpenFile(gcDataDir+'POFLN','POFLN','SH','POFLN_A')
SELECT (lcSelected)            && Restore Alias.

*-- End Of Coding.



*:*************************************************************
*: Name        : lfGetEml
*: Developer   : Ehab Ismail Hamed (EIH)
*: Date        : 10/19/2005
*: Purpose     : Get Emails.
*:*************************************************************
*: Calls       : lfMailNote .
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : =lfGetEml()
*:*************************************************************
*
FUNCTION lfGetEml
PARA lcDummy
PRIVATE lnAlias
lnAlias = 0 
lnAlias = SELECT(0)
lcCMERCH = lfMailNote(POFHDR.CMerch)
lcCGTECH = lfMailNote(POFHDR.CGtech)
lcCPURCH = lfMailNote(POFHDR.CPURCH)
SELECT(lnAlias)

*-- End Of lfGetEml

*!**************************************************************************
*! Name      : lfUDFDesc
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Get the description of required user defined field
*!**************************************************************************
*! Example   : =lfUDFDesc()
*!**************************************************************************
FUNCTION lfUDFDesc
PARAMETER lcUDFName,laUDFArr
lnMemoWdth=SET('MEMOWIDTH')
SET MEMOWIDTH TO 255
IF SEEK(UPPER(lcUDFName), 'SYDFIELD')
  lcMvEntres = MLINE(SYDFIELD.MVENTRIES,1)
  IF EMPTY(lcMvEntres)
    RETURN
  ENDIF
  lnPos=AT('~',lcMvEntres)
  lcAllValue = LEFT(lcMvEntres , lnPos)
  lcCodes    = STRTRAN(lcMvEntres,lcAllValue,'')
  lcCodes    = STRTRAN(lcCodes,'@','')
  lcAllValue    = STRTRAN(lcAllValue,'~','')
  lnOccurs = OCCURS('|',lcAllValue)
  lnOccurs = lnOccurs + 2 
  DIMENSION laUDFArr[lnOccurs - 1 ,2]
  STORE '' TO laUDFArr
  IF lnOccurs = 2
    laUDFArr[1,1] = lcCodes
    laUDFArr[1,2] =  lcAllValue
    RETURN laUDFArr
  ELSE
    FOR lnCnt = 1 TO lnOccurs - 1
      lnPos=AT('|',lcCodes)
      laUDFArr[lnCnt,1] = LEFT(lcCodes,lnPos - 1)
      lcCodes    = STRTRAN(lcCodes,laUDFArr[lnCnt,1]+'|','')
      lnPos=AT('|',lcAllValue)
      laUDFArr[lnCnt,2] = LEFT(lcAllValue,lnPos - 1)
      lcAllValue  = STRTRAN(lcAllValue,laUDFArr[lnCnt,2]+'|','')
      IF OCCURS('|',lcAllValue) = 0
        EXIT
      ENDIF
    ENDFOR
    laUDFArr[lnCnt+1,1] = lcCodes
    laUDFArr[lnCnt+1,2] = lcAllValue
  ENDIF
ENDIF  
SET MEMOWIDTH TO lnMemoWdth

RETURN laUDFArr

*-- End Of lfUDFDesc

*!**************************************************************************
*! Name      : lfMailNote
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : get Notepad templete for merch and cgtech to get the emial fields. 
*!**************************************************************************
*! Example   : =lfMailNote()
*!**************************************************************************
FUNCTION lfMailNote
PARAMETER lcTarget

lcAlias = ALIAS()
lnMemoWdth=SET('MEMOWIDTH')
SET MEMOWIDTH TO 255

IF SEEK('T'+ALLTRIM(UPPER(lcTarget)),'Notepad')
  lcMailNote = SUBSTR(MLINE(NOTEPAD.MNOTES,1),1,45)
ELSE
  lcMailNote = ''
ENDIF
SET MEMOWIDTH TO lnMemoWdth
SELECT (lcAlias)
RETURN lcMailNote


*!**************************************************************************
*! Name      : lfMaxPage
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Get max number of pages for that po .
*!**************************************************************************
*! Example   : =lfMaxPage()
*!**************************************************************************
FUNCTION lfMaxPage
PARA lcDummy
lcAlias = ALIAS()
STORE 1 TO lnLines
STORE '' TO lcMaxPg 
SELECT POFHDR
lcPoMat = POFHDR.POMAT
IF llRpPrtPn 
  IF !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))
    lnI = MEMLINES(ALLTRIM(NOTEPAD_A.MNOTES))
    DO WHILE EMPTY(MLINE(NOTEPAD_A.MNOTES,lnI))
      lnI = lnI - 1
    ENDDO
    lnLines = lnLines + lnI
  ENDIF  
ENDIF
lnLines = lnLines + 4
SELECT POFLN_a
LOCATE
=SEEK('P'+lcPoMat)
SCAN REST WHILE cmattype+pomat+fabric+color+trancd = 'P'+lcPoMat
  IF trancd = '1'
    lnLines = lnLines + 1
  ENDIF  
ENDSCAN
SELECT POFHDR
lcMaxPg = STR(CEILING(lnLines / 25))

SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfPurchase
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Get data for Purchase.
*!**************************************************************************
*! Example   : =lfPurchase()
*!**************************************************************************
FUNCTION lfPurchase
PARA lcDummy

lcAlias = ALIAS()
STORE '' TO lcPurchase
IF !EMPTY(ALLTRIM(POFHDR.cOrdPl))
  SELECT NOTEPAD
  IF SEEK('T'+ALLTRIM(UPPER(POFHDR.cOrdPl)),'Notepad')
    lcPurchase = Notepad.cdesc
 ENDIF
ELSE
  lcPurchase = ''
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : MAMATPO.PRG 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Fill variables for Notepad printing with its 
*!           : corressponding data. 
*!*************************************************************
*! Called from : Detail band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes
PARA lcDummy

*E300847,4 *-- if external call
IF llExternal
  RETURN ''
ENDIF

STORE '' TO lcTitle,lcNotes

*-- if user want to print Material PO notepad and you find notes in notepad file.
IF llRpPrtPn AND !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))

  lcTitle = IIF(POFHDR.CMATTYPE = 'P','Purchase Order ',;
            IIF(POFHDR.CMATTYPE = 'R','Return Purchase Order ','Contract ')) +;
            'Notepad'
  lcNotes = ALLTRIM(NOTEPAD_A.MNOTES)

ENDIF   && end if user want to print Material PO notepad.

RETURN ''
*-- end of lfGetNotes

