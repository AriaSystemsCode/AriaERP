*:***************************************************************************
*: Program file  : ALPKLSNK.PRG
*: Program desc. : CUSTOMIZED SO ALLOCATION PACKING LIST FORM FOR NIK NAK
*: Date          : 08/18/2004
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C123648
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKLSNK
*:***************************************************************************
*: Modifications :
***************************************************************************

PRIVATE lcAlias
lcAlias = ALIAS()

IF USED(lclinesNK)
  USE IN (lclinesNK)
  ERASE (gcWorkDir + 'lclinesNK.CDX')
ENDIF

*--Initialize the variables.
STORE 0 TO lnClrLnNK , lnClrPosNK , lnStyLnNK , lnStyPosNK
=lfChkStrct()

*--Main function of the program.
=lfCreatTmp()  && Create work cursor.
=lfCollData()  && Collect the data.

*--Section break the relation and create the new one.
SELECT (lcPakLnTmp)
IF ('STYLE' $ SET('RELATION'))
  SET RELATION OFF INTO STYLE
ENDIF

SELECT (lclinesNK)
IF !('STYLE' $ SET('RELATION'))
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF

SELECT PACK_HDR
SET RELATION OFF INTO (lcPakLnTmp)
SET RELATION TO PACK_NO INTO (lclinesNK)

SELECT(lcAlias)
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcPackNo , lcRecNo
STORE SPACE(0) TO lcPackNo

lcAlias = ALIAS()
SELECT (lclinesNK)
SET ORDER TO TAG (lclinesNK)

SELECT (lcPackTmp)
lcRecNo = EVAL(KEY())
SCAN 
  SELECT (lcPakLnTmp)
  IF !('STYLE' $ SET('RELATION'))
    SET RELATION TO STYLE INTO STYLE ADDITIVE
  ENDIF

  SCAN WHILE PACK_NO + STR(nOrdLineNO,6) + STYLE = EVAL(lcPackTmp +'.PACK_NO')
    IF EMPTY(lcPackNo) OR (lcPackNo # EVAL(lcPakLnTmp+".PACK_NO"))
      lcPackNo = EVAL(lcPakLnTmp+".PACK_NO")
      =lfInsrtRec()
    ELSE
      IF SEEK(EVAL(lcPakLnTmp+'.PACK_NO') , lclinesNK )
        lcStyClrPk =lfGetStyl(lcPakLnTmp)

        SELECT (lclinesNK)
        llFoundRec = .T.
        SCAN WHILE PACK_NO + STYLE + STR(nOrdLineNO,6) = EVAL(lcPakLnTmp+'.PACK_NO')
          lcStyClrLn =lfGetStyl(lclinesNK)
          IF EVAL(lcPakLnTmp+'.PACK_NO') == EVAL(lclinesNK+'.PACK_NO') AND lcStyClrPk == lcStyClrLn AND ;
             LEFT(SCALE.SCALE,1) == LEFT(EVAL(lclinesNK +'.SCALE'),1)

            llFoundRec = .F.           && Found the record to update in it.
            FOR lnAddVal = 1 TO 8
              lcAddVal = ALLTRIM(STR(lnAddVal))
              IF EMPTY(SCALE.SZ&lcAddVal)
                EXIT
              ENDIF
              FOR lnChkScl = 1 TO 16
                lcChkScl = ALLTRIM(STR(lnChkScl))
                IF SCALE.SZ&lcAddVal == &lclinesNK..Sz&lcChkScl
                  REPLACE &lclinesNK..Qty&lcChkScl WITH &lclinesNK..Qty&lcChkScl + &lcPakLnTmp..Qty&lcAddVal
                  EXIT
                ENDIF
              ENDFOR
            ENDFOR
            REPLACE &lclinesNK..TotQty WITH &lclinesNK..TotQty + &lcPakLnTmp..TotQty ;
                    &lclinesNK..WEIGHT WITH &lclinesNK..WEIGHT + &lcPakLnTmp..WEIGHT ;
                    &lclinesNK..To_crt WITH &lcPakLnTmp..To_crt
            EXIT
          ENDIF
        ENDSCAN
        
        *--Case new record.
        IF llFoundRec
          =lfInsrtRec()
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT (lclinesNK)
SET ORDER TO TAG 'PACKCRTN'

SELECT (lcPackTmp)
=SEEK(lcRecNo)

SELECT(lcAlias)

*--End of lfCollData
*!*************************************************************
*! Name      : lfInsrtRec
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Insert new recored in the temp file.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfInsrtRec()
*!*************************************************************
FUNCTION lfInsrtRec
PRIVATE lcRecNo

SELECT (lcPakLnTmp)
SCATTER MEMVAR MEMO
SELECT (lclinesNK)
APPEND BLANK
GATHER MEMVAR MEMO
REPLACE &lclinesNK..Scale WITH LEFT(SCALE.Scale,1)

SELECT SCALE
lcRecNo = EVAL(KEY())
lnEmptyFld = 0

SCAN REST WHILE TYPE + SCALE + PREPAK = "S" + LEFT(EVAL(lclinesNK+'.Scale'),1)
  SELECT (lclinesNK)
    IF lnEmptyFld > 16
      EXIT
    ENDIF
    FOR lnCrtTmp = 1 TO 16
    lcNumSiz = ALLTRIM(STR(lnCrtTmp))
    IF EMPTY(&lclinesNK..Sz&lcNumSiz)
      EXIT
    ENDIF
    lnEmptyFld = lnCrtTmp
  ENDFOR

  FOR lnCrtTmp = 1 TO 8
	lnLinChk = ALLTRIM(STR(lnCrtTmp + lnEmptyFld))
    lcNumSiz = ALLTRIM(STR(lnCrtTmp))
    IF EMPTY(SCALE.SZ&lcNumSiz) OR lnCrtTmp + lnEmptyFld > 16
      EXIT
    ENDIF
    REPLACE &lclinesNK..Sz&lnLinChk WITH SCALE.SZ&lcNumSiz ,;
            &lclinesNK..Nrf&lnLinChk WITH SCALE.Cnrfcode&lcNumSiz
  ENDFOR
ENDSCAN
=SEEK(lcRecNo)

SELECT (lcPakLnTmp)

*--End of lfInsrtRec.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

DIMENSION lalinesNK[ALEN(laPckLinSt,1),ALEN(laPckLinSt,2)]
=ACOPY(laPckLinSt , lalinesNK)

FOR lnCrtTmp = 9 TO 16
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("lalinesNK", "Qty"  + lcNumSiz ,"N",5,0)
ENDFOR

=lfAddField("lalinesNK", "Scale" ,"C",5,0)
FOR lnCrtTmp = 1 TO 16
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("lalinesNK", "Sz"  + lcNumSiz ,"C",5,0)
ENDFOR

FOR lnCrtTmp = 1 TO 16
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("lalinesNK", "Nrf" + lcNumSiz ,"C",5,0)
ENDFOR

CREATE CURSOR (lclinesNK) FROM ARRAY lalinesNK
SELECT (lclinesNK)
ZAP

INDEX ON PACK_NO + STYLE + STR(nOrdLineNO,6) TAG (lclinesNK) OF (gcWorkDir + lclinesNK + '.CDX')
INDEX ON PACK_NO + STR(NO_CART,4) + STYLE TAG 'PACKCRTN' OF (gcWorkDir + lclinesNK+'.CDX') ADDITIVE

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnNK  = LEN(laItemSeg[lnCount,3])
    lnClrPosNK = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnNK  = LEN(laItemSeg[lnCount,3])
    lnStyPosNK = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!*************************************************************
*! Name      : lfGetStyl
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfGetStyl()
*!*************************************************************
FUNCTION lfGetStyl
PARAMETER lcNameFile

lcSparator = SUBSTR(EVAL(lcNameFile+'.STYLE') , lnStyPosNK + lnStyLnNK , 1)
lcStyColor = SUBSTR(EVAL(lcNameFile+'.STYLE') , lnStyPosNK , lnStyLnNK) + lcSparator +;
             SUBSTR(EVAL(lcNameFile+'.STYLE') , lnClrPosNK , lnClrLnNK) 

RETURN lcStyColor

*--End of lfGetStyl.
*!*************************************************************
*! Name      : lfGrpSetes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfGrpSetes()
*!*************************************************************
FUNCTION lfGrpSetes
PARAMETER lcGrpSets

lnGrdTotWg = 0

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)

lcSkipExpr = ''
SET SKIP TO

SELECT INVLINE
SET RELATION OFF INTO STYLE

SELECT (lclinesNK)
SET RELATION OFF INTO STYLE

SELECT ORDLINE
SET RELATION OFF INTO STYLE
SELECT (lnCurAlias)

PRIVATE lcLocExpr

DO CASE 
  CASE nRprtTyp = 1
    SELECT INVLINE
    lcLinFile = "INVLINE"
    lcPackNo = INVOICE
    lcLocExpr = "INVOICE = '" + lcPackNo + "' AND ORDER = '" + ORDHDR.ORDER + "'"

  CASE nRprtTyp = 2
    SELECT (lclinesNK)
    lcLinFile = lclinesNK
    lcPackNo = pack_no
    lcLocExpr = ""

  CASE nRprtTyp = 3
    SELECT ORDLINE
    lcLinFile = "ORDLINE"
    lcPackNo = "O"+ORDER
    lcLocExpr = "cORDTYPE+ORDER = '" + lcPackNo + "' AND PIKTKT = '" + ORDLINE.PIKTKT + "'"
   
ENDCASE  

lcSkipTag = ORDER()
lcCurRec = EVALUATE(KEY())
SET ORDER TO (lcSkipTag) DESC
IF SEEK(lcPackNo)
  IF !EMPTY(lcLocExpr)    
    LOCATE REST WHILE &lcLocExpr
  ENDIF
  lnLastRec = RECNO()
ENDIF
SET ORDER TO (lcSkipTag) ASCE
=SEEK(lcCurRec)

SET RELATION TO  STYLE INTO STYLE ADDITIVE
SELECT (lnCurAlias)
lcSkipExpr = IIF(nRprtTyp=1,"INVLINE",IIF(nRprtTyp=2,lclinesNK,"ORDLINE"))
SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ''
*-- END OF  lfGrpSetes
*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Evaluate Non Major Code and Description
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
FUNCTION lfNonMjDes
PARAMETER lcNonMjDes 

PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes,lnAlias
STORE '' TO lcTemp , lcNonMjDes , lnAlias
lnAlias = SELECT()

SELECT(lcPackTmp)
lcStyle = &lclinesNK..STYLE
lnI = 0

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  lcTemp = ''

  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSegs[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"ICSEGVAL")
        lcTemp = ALLTRIM(ICSEGVAL.cISgValSd)
      ENDIF
    *-- Season, Color, Division, or lcStyle group Segment.
    CASE laMajSegs[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSegs[lnI,1] = "Z"
          lcCodeExpr = "SEASON"    
        CASE laMajSegs[lnI,1] = "C"
          lcCodeExpr = "COLOR"    
        CASE laMajSegs[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"    
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"    
      ENDCASE
      lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.F.))

    *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"SCALE")
        lcTemp = ALLTRIM(SCALE.cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
ENDFOR

lcStyle    = IIF(lnExtScPos = 0,lcStyle,LEFT(lcStyle,LEN(lcStyle)-lnExtScLen))
lcStyleExp = lcStyle + 'ToCut' + lcNonMjDes
SELECT(lnAlias)

RETURN ''

*-- End Of lfNonMjDes.

*!**************************************************************************
*! Name      : lfNkHdVar
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 07/01/1999
*! Purpose   : 1 - Hold Address In Shipto Array
*!           : 2 - Get the division long name.
*!**************************************************************************
*! Called from : Report Group Header band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfNkHdVar()
*!**************************************************************************
* MHMas per tony Mail
FUNCTION lfNkHdVar
PARAMETER  LCPARM

= gfRltFld(OrdHdr.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

DIMENSION laShipTo[5,1]
laShipTo = ''

DIMENSION laSoldTo[5,1]
laSoldTo = ''

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfAdrShift('laSoldTo')

PRIVATE lcDistCntr , lcAlasCust
lcAlasCust = SELECT(0)
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  DIMENSION laShipTo[6,1]
  laShipTo = ''
  laShipTo[1] = ORDHDR.STName
  laShipTo[2] = ORDHDR.cAddress1
  laShipTo[3] = ORDHDR.cAddress2
  laShipTo[4] = ORDHDR.cAddress3
  laShipTo[5] = ORDHDR.cAddress4
  laShipTo[6] = ORDHDR.cAddress5
  =lfAdrShift('laShipTo')
ELSE
  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF


  =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)
  =lfAdrShift('laShipTo')

  DIMENSION laShipTo[6,1]
  =AINS(laShipTo,1)

  laShipTo[1,1] = Customer.StName
  =lfAdrShift('laShipTo')

  IF BETWEEN(lnCusRec , 1 , RECCOUNT('CUSTOMER'))
    GOTO lnCusRec IN CUSTOMER
  ENDIF
ENDIF
SELECT(lcAlasCust)


STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfNkHdVar
