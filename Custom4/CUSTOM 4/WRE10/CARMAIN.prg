*!**************************************************************************
*! Name      : CARMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : CAROLE WREN, INC Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201304,1
*!**************************************************************************
*! Modifications
*! C201304,2 MMT 02/23/2011 fix bug of invalid vendor value can be entred in UDF of Cut tkt screen{T20101109?.0012}
*! C201312,1 MMT 03/13/2011 Custom Cutting ticket form CW for Carole Wren{T20101109.0011}
*! C201379,1 MMT 08/29/2011 Replace the Values of field TONUM1 and TONUM2 with CTONUMBER and CMILL IN POSHDR[T20110819.0007]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfFactCode
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : Validate Factory Field (Vendor)
*!**************************************************************************
FUNCTION lfFactCode
lPARAMETERS lcRetValue

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lcCurVal, lnPos
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcCurVal  = PADR(lcCurVar.Value, 8)
llRetVal  = .T.
LOCAL loVendor, lcVendor
lcVendor = gfTempName()
loVendor = CREATEOBJECT("RemoteTable", "APVENDOR", "VenCode", lcVendor)
IF !EMPTY(lcCurVal) AND ( !loVendor.SEEK(lcCurVal) OR !("C" $ EVALUATE(lcVendor + '.cVenSupTyp')) )
  IF loVendor.llNative
    SELECT (lcVendor)
    LOCATE FOR "C" $ cVenSupTyp
    llFoundCont = FOUND()
  ELSE
    llFoundCont = loVendor.SQLRun("SELECT TOP 1 * FROM APVendor WHERE CHARINDEX('C', cVenSupTyp) <> 0")
  ENDIF
  IF llFoundCont
    *-- Filter vendor browse according to Supp. Type = 'C' --> Contractor
    =gfApVnBrow(@lcCurVal, .F., 'C')
    IF !EMPTY(lcCurVal)
      =loVendor.SEEK(lcCurVal)
      loOGScroll.laOGFxFlt[lnPos,6] = lcCurVal      && Update the field
    ELSE
      *! C201304,2 MMT 02/23/2011 fix bug of invalid vendor value can be entred in UDF of Cut tkt screen{Start}
      *llRetVal = .F.
      IF !EMPTY(lcCurVar.OldValue) AND loVendor.SEEK(lcCurVar.OldValue)
        loOGScroll.laOGFxFlt[lnPos,6] = lcCurVar.OldValue
      ELSE
        loOGScroll.laOGFxFlt[lnPos,6] = ''
      ENDIF
      *! C201304,2 MMT 02/23/2011 fix bug of invalid vendor value can be entred in UDF of Cut tkt screen{End}

    ENDIF
  ENDIF
ENDIF
loVendor = .NULL.

SELECT (lnAlias)
RETURN llRetVal

*!**************************************************************************
*! Name      : lfUPDSTYPRIC
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : Update cstyprice from ave_cost
*!**************************************************************************
FUNCTION lfUPDSTYPRIC
IF (loFormSet.ACTIVEMODE = 'E' AND EMPTY(cstyprice)) OR loFormSet.ACTIVEMODE = 'A'
  REPLACE cstyprice WITH ALLTRIM(STR(ave_Cost,10,2))
  gfReplace([cstyprice  WITH ALLTRIM(STR(Ave_Cost,10,2))])
ENDIF  

*!**************************************************************************
*! Name      : lfGETSTYPRIC
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : Update Cuttkt cstyprice from Style.cstyprice
*!**************************************************************************
FUNCTION lfGETSTYPRIC
IF loFormSet.ACTIVEMODE = 'A'
  REPLACE cstyprice WITH &lcItem..cstyprice 
ENDIF
*!**************************************************************************
*! Name      : lfADJUDFARR
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : Adjust UDF array to remove summary fields not repeated fields
*!**************************************************************************
FUNCTION lfADJUDFARR
IF TYPE('LOFORMSET.laSumUDF[1]') = 'U'
  LOFORMSET.AddProperty('laSumUDF[1]','')
ENDIF 
FOR lnT=1 TO ALEN(LOFORMSET.laUserFields,1)
  IF ALLTRIM(LOFORMSET.laUserFields[lnT,1]) <> 'ZZZZZ' AND ISDIGIT(RIGHT(ALLTRIM(LOFORMSET.laUserFields[lnT,1]),1))
    lcMainFld = SUBSTR(ALLTRIM(LOFORMSET.laUserFields[lnT,1]),1,LEN(ALLTRIM(LOFORMSET.laUserFields[lnT,1]))-1)
    IF ASCAN(LOFORMSET.laSumUDF,lcMainFld ,1,0,1,2) = 0
      FOR lnF=1 TO ALEN(LOFORMSET.laUserFields,1)
        IF ALLTRIM(lcMainFld) == ALLTRIM(LOFORMSET.laUserFields[lnF,1],1)
          IF EMPTY(LOFORMSET.laSumUDF[1])
            LOFORMSET.laSumUDF[1] = lcMainFld
          ELSE
            DIMENSION LOFORMSET.laSumUDF[ALEN(LOFORMSET.laSumUDF,1)+1]
            LOFORMSET.laSumUDF[ALEN(LOFORMSET.laSumUDF,1)] = lcMainFld
          ENDIF
          *ADEL(LOFORMSET.laUserFields,lnF)
          LOFORMSET.laUserFields[lnF,1] = 'ZZZZZ'
        ENDIF
     ENDFOR   
   ENDIF 
  ENDIF
ENDFOR 
=ASORT(LOFORMSET.laUserFields)
FOR lnX = ALEN(LOFORMSET.laUserFields,1) TO 1 STEP -1
  IF ALLTRIM(LOFORMSET.laUserFields[lnX,1]) = 'ZZZZZ'
    DIMENSION LOFORMSET.laUserFields[lnX-1,8]
  ENDIF
ENDFOR

*!**************************************************************************
*! Name      : lfUPDHDUDF
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/18/2011
*! Purpose   : update summary fields from repeated fields
*!**************************************************************************
FUNCTION lfUPDHDUDF

*C201379,1 MMT 08/29/2011 Replace the Values of field TONUM1 and TONUM2 with CTONUMBER and CMILL IN POSHDR[START]
REPLACE TONUM1 WITH IIF(!EMPTY(CTONUMBER),CTONUMBER,TONUM1);
        TONUM2 WITH IIF(!EMPTY(CMILL),CMILL,TONUM2) IN (loFormSet.Ariaform1.oFormClass.TktHeader)    
*C201379,1 MMT 08/29/2011 Replace the Values of field TONUM1 and TONUM2 with CTONUMBER and CMILL IN POSHDR[END]


IF !EMPTY(LOFORMSET.laSumUDF[1])
  FOR lnB =1 TO ALEN(LOFORMSET.laSumUDF,1)
    lcMainFld = ALLTRIM(LOFORMSET.laSumUDF[lnB])
    lcFieldValue = ""
    FOR lnA=1 TO 9
      lcChkFld = loFormSet.Ariaform1.oFormClass.TktHeader+'.'+lcMainFld+ALLTRIM(STR(lnA))
      IF TYPE('&lcChkFld') <> 'U' AND !EMPTY(&lcChkFld)
        IF TYPE('&lcChkFld') = 'D'
          lcFieldValue = lcFieldValue + DTOC(&lcChkFld)+CHR(13)+CHR(10) 
        ELSE
          IF TYPE('&lcChkFld') = 'N'
            lcFieldValue = lcFieldValue + STR(&lcChkFld)+CHR(13)+CHR(10)           
          ELSE
            lcFieldValue = lcFieldValue + &lcChkFld+CHR(13)+CHR(10)           
          ENDIF  
        ENDIF
      ENDIF		
    ENDFOR
    REPLACE (lcMainFld) WITH lcFieldValue IN (loFormSet.Ariaform1.oFormClass.TktHeader)    
  ENDFOR   
ENDIF  
*C201312,1 MMT 03/13/2011 Custom Cutting ticket form CW for Carole Wren{T20101109.0011}[Start]
*!**************************************************************************
*! Name      : lfCustCode
*! Developer : Mariam Mazhar[MMT]
*! Date      : 03/13/2011
*! Purpose   : Validate Customer code Field 
*!**************************************************************************
FUNCTION lfCustCode
lPARAMETERS lcRetValue

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lnPos
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcCurVal  = PADR(lcCurVar.Value, 5)
llRetVal  = .T.
LOCAL loCustomer, lcCustomer
lcCustomer= gfTempName()
loCustomer= CREATEOBJECT("RemoteTable", "CUSTOMER", "CUSTOMER", lcCustomer)
IF !EMPTY(lcCurVal) AND (!loCustomer.SEEK('M'+lcCurVal))
  DO CUSBROWM WITH lcCurVal
  IF !EMPTY(lcCurVal)
    =loCustomer.SEEK('M'+lcCurVal)
    loOGScroll.laOGFxFlt[lnPos,6] = lcCurVal      && Update the field
  ELSE
    IF !EMPTY(lcCurVar.OldValue) AND loCustomer.SEEK('M'+lcCurVar.OldValue)
      loOGScroll.laOGFxFlt[lnPos,6] = lcCurVar.OldValue
    ELSE
      loOGScroll.laOGFxFlt[lnPos,6] = ''
    ENDIF
  ENDIF
ENDIF
loCustomer= .NULL.

SELECT (lnAlias)
RETURN llRetVal
*C201312,1 MMT 03/13/2011 Custom Cutting ticket form CW for Carole Wren{T20101109.0011}[End]