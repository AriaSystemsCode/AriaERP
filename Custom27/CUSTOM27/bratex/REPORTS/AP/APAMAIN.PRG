*:***************************************************************************
*: Program file  : APAMAIN.PRG
*: Program desc. : CUSTOMIZED PRINT CHECH FOR BRATEX.
*: Date          : 07/24/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT PAYABLE (AP)
*: Developer     : ALBERT RAIF ATTIA (ALB)
*: Tracking Job Number: 
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO APAMAIN
*:***************************************************************************
*: Modifications :
*****************************************************************************
*!***************************************************************************
*! Name      : lfCallFrx
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 07/24/2002
*! Purpose   : Convert Number to String
*!***************************************************************************
*! Called from : APAMAIN.PRG
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*: Passed Parameters  : LCRETURN Return function value to lfUsrVldFn()
*:                      LCPARM   Have "1lnTotallcLineTwo" value
*!***************************************************************************
*! Return      : ....
*!***************************************************************************
*! Example     : = lfCallFrx()
*!***************************************************************************
FUNCTION lfCallFrx
PARAMETER LCRETURN , LCPARM

lcLang   = VAL(LEFT(LCPARM , 1))
lnNo2Str = EVAL(SUBSTR(LCPARM , 2 , 7))
lcSecLn  = SUBSTR(LCPARM , 9 , 9)

LCRETURN = lfNo2Str(1,lnNo2Str,lcSecLn)
*******************************************************************************
**                                                                           **
**  Function   : lfNo2Str                                                    **
**               Converts a number to a string in English or Spanish         **
**  Developer  : Wael M. Abo-Shawareb                                        **
**  Parameters : id_land - "integer"; 1 for Spanish, 2 For English           **
**               pr_num  - number to be converted                            **
**  Returns    : A string representing the number                            **
**                                                                           **
*******************************************************************************
*******************************************************************************


FUNCTION lfNo2Str
PARAMETERS lnLangSel,lnNo2Conv,lcSecLine

IF lnNo2Conv >= 1000000000
  RETURN ''
ENDIF
DIMENSION lacardinal(2,45)
	
=lfFilAray()

lcRetVal = lfNumToStr(lnLangSel, lnNo2Conv)
RELEASE lacardinal

IF LEN(lcRetVal) > 72
  lcTempStr    = SUBSTR(lcRetVal,1,71)
  lnEndOfLine  = RAT(' ',lcTempStr)
  lcTempStr    = SUBSTR(lcTempStr,1,lnEndOfLine) 
  &lcSecLine   = SUBSTR(lcRetVal,lnEndOfLine+1)
  lcRetVal     = lcTempStr 
ELSE  
  &lcSecLine = ''
ENDIF

RETURN lcRetVal
*******************************************************************************
Function lfNumToStr
PARAMETERS lnidioma, lnNo2Convero
	ln_grp1 = 0
	ln_grp2 = 0
	ln_grp3 = 0
*	lnCount = 0
	lcPalabra = ''
	lr_m = lnNo2Convero
	IF (lnNo2Convero >= 1000000)
		ln_grp1  = INT(lnNo2Convero / 1000000)
		IF lnNo2Convero = 999999999.99
		   ln_grp1 = 999
		ENDIF
		
		IF (ln_grp1 > 1)
			lcPalabra = lfTraduccion(lnidioma, ln_grp1,lcPalabra)
		ENDIF
		
		IF (ln_grp1 = 1)
			IF (lnidioma = 1)
				lcPalabra = lcPalabra + 'UN MILLON '
			ELSE
				lcPalabra = lcPalabra + 'ONE MILLION '
			ENDIF
		ELSE
			IF (lnidioma = 1)
      	lcPalabra = lcPalabra + 'MILLONES '
      ELSE
      	lcPalabra = lcPalabra + 'MILLIONS '
			ENDIF
		ENDIF
		
		lnNo2Convero = lnNo2Convero - ln_grp1 * 1000000
	ENDIF
	
	IF (lnNo2Convero >= 1000)
  	ln_grp1 = INT(lnNo2Convero / 1000)
    ln_grp2 = lnNo2Convero % 1000

		IF (ln_grp1 > 1) OR ((ln_grp1=1) AND (lnidioma=2))
			lcPalabra = lfTraduccion(lnidioma, ln_grp1,lcPalabra)
		ENDIF
		
		IF (lnidioma = 1)
			lcPalabra = lcPalabra + 'MIL '
		ELSE
			lcPalabra = lcPalabra + 'THOUSAND '
		ENDIF
		lcPalabra = lfTraduccion(lnidioma, ln_grp2,lcPalabra)
	ELSE
  	ln_grp1 = INT(lnNo2Convero)
    ln_grp2 = 0
    lcPalabra = lfTraduccion(lnidioma, ln_grp1, lcPalabra)
	ENDIF
	
	lnDecimal = VAL(ALLT(STR((lr_m - Int(lr_m)) * 100)))
	IF (lnidioma = 1)
  	lcPalabra = lcPalabra + IIF(lnDecimal > 0, 'Con ' + lfNumToStr(lnidioma, lnDecimal), '')
	ELSE
		lcPalabra = lcPalabra + IIF(lnDecimal > 0, 'AND ' + ALLTRIM(STR(lnDecimal)) + '/100', '')
	ENDIF
	
RETURN lcPalabra
*ENDFUNC
*******************************************************************************
FUNCTION lfTraduccion 
PARAMETERS lnidioma, lnNum, lcPalabra

  lnNumero = 0.00
  lnCen = 0
  lnDec = 0
  lnUni = 0
  lnIdx1 = 0
  
  IF (lnNum > 99)
		lnCen = INT(lnNum / 100)
    lnNum = lnNum % 100
    IF lnCen = 1 AND INT(lnNum) = 0
    	lcPalabra = lcPalabra + 'CIEN '
   	ELSE
	    lcPalabra = lcPalabra + lacardinal[lnidioma,lnCen+36]
	  ENDIF
	ENDIF
	
	If (lnNum > 29)
  	lnDec = INT(lnNum / 10)
    lnUni = lnNum % 10
    If (lnUni > 0)
    	IF (lnidioma = 1)
    		lcPalabra = lcPalabra + lacardinal[lnidioma,lnDec+27] + 'Y ' + lacardinal[lnidioma,lnUni]
    	ELSE
    		lcPalabra = lcPalabra + lacardinal[lnidioma,lnDec+27] + lacardinal[lnidioma,lnUni]
    	ENDIF
    ELSE
    	lcPalabra = lcPalabra + lacardinal[lnidioma,lnDec + 27]
    ENDIF
  ELSE
  	IF (INT(lnNum) > 0)
  		lcPalabra = lcPalabra + lacardinal[lnidioma,lnNum]
  	ENDIF
  ENDIF 
RETURN lcPalabra
*******************************************************************************
PROCEDURE lfFilAray
	lacardinal[01,01] = 'UNO '
	lacardinal[01,02] = 'DOS '
	lacardinal[01,03] = 'TRES '
	lacardinal[01,04] = 'CUATRO '
	lacardinal[01,05] = 'CINCO '
	lacardinal[01,06] = 'SEIS '
	lacardinal[01,07] = 'SIETE '
	lacardinal[01,08] = 'OCHO '
	lacardinal[01,09] = 'NUEVE '
	lacardinal[01,10] = 'DIEZ '
	lacardinal[01,11] = 'ONCE '
	lacardinal[01,12] = 'DOCE '
	lacardinal[01,13] = 'TRECE '
	lacardinal[01,14] = 'CATORCE '
	lacardinal[01,15] = 'QUINCE '
	lacardinal[01,16] = 'DIECISEIS '
	lacardinal[01,17] = 'DIECISIETE '
	lacardinal[01,18] = 'DIECIOCHO '
	lacardinal[01,19] = 'DIECINUEVE '
	lacardinal[01,20] = 'VEINTE '
	lacardinal[01,21] = 'VEINTIUNO '
	lacardinal[01,22] = 'VEINTIDOS '
	lacardinal[01,23] = 'VEINTITRES '
	lacardinal[01,24] = 'VEINTICUATRO '
	lacardinal[01,25] = 'VEINTICINCO '
	lacardinal[01,26] = 'VEINTISEIS '
	lacardinal[01,27] = 'VEINTISIETE '
	lacardinal[01,28] = 'VEINTIOCHO '
	lacardinal[01,29] = 'VEINTINUEVE '
	lacardinal[01,30] = 'TREINTA '
	lacardinal[01,31] = 'CUARENTA '
	lacardinal[01,32] = 'CINCUENTA '
	lacardinal[01,33] = 'SESENTA '
	lacardinal[01,34] = 'SETENTA '
	lacardinal[01,35] = 'OCHENTA '
	lacardinal[01,36] = 'NOVENTA '
	lacardinal[01,37] = 'CIENTO '
	lacardinal[01,38] = 'DOSCIENTOS '
	lacardinal[01,39] = 'TRESCIENTOS '
	lacardinal[01,40] = 'CUATROCIENTOS '
	lacardinal[01,41] = 'QUINIENTOS '
	lacardinal[01,42] = 'SEISCIENTOS '
	lacardinal[01,43] = 'SETECIENTOS '
	lacardinal[01,44] = 'OCHOCIENTOS '
	lacardinal[01,45] = 'NOVECIENTOS '

	lacardinal[02,01] = 'ONE '
	lacardinal[02,02] = 'TWO '
	lacardinal[02,03] = 'THREE '
	lacardinal[02,04] = 'FOUR '
	lacardinal[02,05] = 'FIVE '
	lacardinal[02,06] = 'SIX '
	lacardinal[02,07] = 'SEVEN '
	lacardinal[02,08] = 'EIGHT '
	lacardinal[02,09] = 'NINE '
	lacardinal[02,10] = 'TEN '
	lacardinal[02,11] = 'ELEVEN '
	lacardinal[02,12] = 'TWELVE '
	lacardinal[02,13] = 'THIRTEEN '
	lacardinal[02,14] = 'FOURTEEN '
	lacardinal[02,15] = 'FIFTEEN '
	lacardinal[02,16] = 'SIXTEEN '
	lacardinal[02,17] = 'SEVENTEEN '
	lacardinal[02,18] = 'EIGHTEEN '
	lacardinal[02,19] = 'NINETEEN '
	lacardinal[02,20] = 'TWENTY '
	lacardinal[02,21] = 'TWENTY ONE '
	lacardinal[02,22] = 'TWENTY TWO  '
	lacardinal[02,23] = 'TWENTY THREE '
	lacardinal[02,24] = 'TWENTY FOUR '
	lacardinal[02,25] = 'TWENTY FIVE '
	lacardinal[02,26] = 'TWENTY SIX '
	lacardinal[02,27] = 'TWENTY SEVEN '
	lacardinal[02,28] = 'TWENTY EIGTH '
	lacardinal[02,29] = 'TWENTY NINE '
	lacardinal[02,30] = 'THIRTY  '
	lacardinal[02,31] = 'FORTY '
	lacardinal[02,32] = 'FIFTY '
	lacardinal[02,33] = 'SIXTY '
	lacardinal[02,34] = 'SEVENTY '
	lacardinal[02,35] = 'EIGHTY '
	lacardinal[02,36] = 'NINETY '
	lacardinal[02,37] = 'ONE HUNDRED '
	lacardinal[02,38] = 'TWO HUNDRED '
	lacardinal[02,39] = 'THREE HUNDRED '
	lacardinal[02,40] = 'FOUR HUNDRED '
	lacardinal[02,41] = 'FIVE HUNDRED '
	lacardinal[02,42] = 'SIX HUNDRED '
	lacardinal[02,43] = 'SEVEN HUNDRED '
	lacardinal[02,44] = 'EIGHT HUNDRED '
	lacardinal[02,45] = 'NINE HUNDRED '
RETURN
*ENDPROC

RETURN LCRETURN