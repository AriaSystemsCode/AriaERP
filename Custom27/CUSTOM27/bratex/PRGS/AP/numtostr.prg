*******************************************************************************
*******************************************************************************
**                                                                           **
**  Function   : NumToStr                                                    **
**               Converts a number to a string in English or Spanish         **
**  Developer  : Wael M. Abo-Shawareb                                        **
**  Parameters : id_land - "integer"; 1 for Spanish, 2 For English           **
**               pr_num  - number to be converted                            **
**  Returns    : A string representing the number                            **
**                                                                           **
*******************************************************************************
*******************************************************************************

PROCEDURE numToStr
PARAMETERS id_lang, pr_num

	IF PARAMETERS() = 2
		IF !BETWEEN(id_lang, 1, 2)
		   WAIT WINDOW 'Language must be "1" for Spanish, or "2" for English...'
		   RETURN ''
		ENDIF
		
	  IF pr_num >= 1000000000
  	   WAIT WINDOW 'Number must be less than 1,000,000,000'
    	 RETURN ''
	  ENDIF
	ELSE
		WAIT WINDOW 'Parameter messing...'
		RETURN ''
	ENDIF

	DIMENSION cardinal(2,45)
		
	=FillArray()

	cRet = fnNumToStr(id_lang, pr_num)
	RELEASE Cardinal

RETURN cRet
*ENDPROC
*******************************************************************************
Function fnNumToStr
PARAMETERS pi_idioma, pr_numero
	li_grp1 = 0
	li_grp2 = 0
	li_grp3 = 0
	li_i = 0
	ls_Palabra = ''
	lr_m = pr_numero
	IF (pr_numero >= 1000000)
		Li_grp1  = INT(pr_numero / 1000000)
		IF pr_numero = 999999999.99
		   Li_grp1 = 999
		ENDIF
		
		IF (li_grp1 > 1)
			ls_palabra = FnTraduccion(pi_idioma, li_grp1,ls_palabra)
		ENDIF
		
		IF (li_grp1 = 1)
			IF (PI_IDIOMA = 1)
				ls_palabra = ls_palabra + 'UN MILLON '
			ELSE
				ls_palabra = ls_palabra + 'ONE MILLION '
			ENDIF
		ELSE
			IF (PI_IDIOMA = 1)
      	ls_palabra = ls_palabra + 'MILLONES '
      ELSE
      	ls_palabra = ls_palabra + 'MILLIONS '
			ENDIF
		ENDIF
		
		pr_numero = pr_numero - li_grp1 * 1000000
	ENDIF
	
	IF (pr_numero >= 1000)
  	li_grp1 = INT(pr_numero / 1000)
    li_grp2 = pr_numero % 1000

		IF (li_grp1 > 1) OR ((li_grp1=1) AND (pi_idioma=2))
			ls_palabra = fnTraduccion(pi_idioma, li_grp1,ls_palabra)
		ENDIF
		
		IF (PI_IDIOMA = 1)
			ls_palabra = ls_palabra + 'MIL '
		ELSE
			ls_palabra = ls_palabra + 'THOUSAND '
		ENDIF
		ls_palabra = FnTraduccion(pi_idioma, li_grp2,ls_palabra)
	ELSE
  	li_grp1 = INT(pr_numero)
    li_grp2 = 0
    ls_palabra = FnTraduccion(pi_idioma, li_grp1, ls_palabra)
	ENDIF
	
	*SET STEP ON
	nDecimal = VAL(ALLT(STR((lr_m - Int(lr_m)) * 100)))
	IF (PI_IDIOMA = 1)
  	ls_palabra = ls_palabra + IIF(nDecimal > 0, 'Con ' + fnNumToStr(pi_idioma, nDecimal), '')
	ELSE
		ls_palabra = ls_palabra + IIF(nDecimal > 0, 'AND ' + ALLTRIM(STR(nDecimal)) + '/100', '')
	ENDIF
	
RETURN ls_palabra
*ENDFUNC
*******************************************************************************
FUNCTION FnTraduccion 
PARAMETERS pi_idioma, pi_num, ps_palabra

	li_numero = 0.00
  li_cen = 0
  li_dec = 0
  li_uni = 0
  li_idx1 = 0
  
  IF (pi_num > 99)
		li_cen = INT(pi_num / 100)
    pi_num = pi_num % 100
    IF li_cen = 1 AND INT(pi_num) = 0
    	ps_palabra = ps_palabra + 'CIEN '
   	ELSE
	    ps_palabra = ps_palabra + cardinal[pi_idioma,li_cen+36]
	  ENDIF
	ENDIF
	
	If (pi_num > 29)
  	li_dec = INT(pi_num / 10)
    li_uni = pi_num % 10
    If (li_uni > 0)
    	IF (PI_IDIOMA = 1)
    		Ps_palabra = ps_palabra + cardinal[pi_idioma,li_dec+27] + 'Y ' + cardinal[pi_idioma,li_uni]
    	ELSE
    		Ps_palabra = ps_palabra + cardinal[pi_idioma,li_dec+27] + cardinal[pi_idioma,li_uni]
    	ENDIF
    ELSE
    	ps_palabra = ps_palabra + Cardinal[pi_idioma,li_dec + 27]
    ENDIF
  ELSE
  	IF (INT(pi_num) > 0)
*  		wait wind str(pi_num)
  		ps_palabra = ps_palabra + Cardinal[pi_idioma,pi_num]
  	ENDIF
  ENDIF 
RETURN ps_palabra
*ENDFUNC
*******************************************************************************
PROCEDURE FillArray
	Cardinal[01,01] = 'UNO '
	Cardinal[01,02] = 'DOS '
	Cardinal[01,03] = 'TRES '
	Cardinal[01,04] = 'CUATRO '
	Cardinal[01,05] = 'CINCO '
	Cardinal[01,06] = 'SEIS '
	Cardinal[01,07] = 'SIETE '
	Cardinal[01,08] = 'OCHO '
	Cardinal[01,09] = 'NUEVE '
	Cardinal[01,10] = 'DIEZ '
	Cardinal[01,11] = 'ONCE '
	Cardinal[01,12] = 'DOCE '
	Cardinal[01,13] = 'TRECE '
	Cardinal[01,14] = 'CATORCE '
	Cardinal[01,15] = 'QUINCE '
	Cardinal[01,16] = 'DIECISEIS '
	Cardinal[01,17] = 'DIECISIETE '
	Cardinal[01,18] = 'DIECIOCHO '
	Cardinal[01,19] = 'DIECINUEVE '
	Cardinal[01,20] = 'VEINTE '
	Cardinal[01,21] = 'VEINTIUNO '
	Cardinal[01,22] = 'VEINTIDOS '
	Cardinal[01,23] = 'VEINTITRES '
	Cardinal[01,24] = 'VEINTICUATRO '
	Cardinal[01,25] = 'VEINTICINCO '
	Cardinal[01,26] = 'VEINTISEIS '
	Cardinal[01,27] = 'VEINTISIETE '
	Cardinal[01,28] = 'VEINTIOCHO '
	Cardinal[01,29] = 'VEINTINUEVE '
	Cardinal[01,30] = 'TREINTA '
	Cardinal[01,31] = 'CUARENTA '
	Cardinal[01,32] = 'CINCUENTA '
	Cardinal[01,33] = 'SESENTA '
	Cardinal[01,34] = 'SETENTA '
	Cardinal[01,35] = 'OCHENTA '
	Cardinal[01,36] = 'NOVENTA '
	Cardinal[01,37] = 'CIENTO '
	Cardinal[01,38] = 'DOSCIENTOS '
	Cardinal[01,39] = 'TRESCIENTOS '
	Cardinal[01,40] = 'CUATROCIENTOS '
	Cardinal[01,41] = 'QUINIENTOS '
	Cardinal[01,42] = 'SEISCIENTOS '
	Cardinal[01,43] = 'SETECIENTOS '
	Cardinal[01,44] = 'OCHOCIENTOS '
	Cardinal[01,45] = 'NOVECIENTOS '

	Cardinal[02,01] = 'ONE '
	Cardinal[02,02] = 'TWO '
	Cardinal[02,03] = 'THREE '
	Cardinal[02,04] = 'FOUR '
	Cardinal[02,05] = 'FIVE '
	Cardinal[02,06] = 'SIX '
	Cardinal[02,07] = 'SEVEN '
	Cardinal[02,08] = 'EIGHT '
	Cardinal[02,09] = 'NINE '
	Cardinal[02,10] = 'TEN '
	Cardinal[02,11] = 'ELEVEN '
	Cardinal[02,12] = 'TWELVE '
	Cardinal[02,13] = 'THIRTEEN '
	Cardinal[02,14] = 'FOURTEEN '
	Cardinal[02,15] = 'FIFTEEN '
	Cardinal[02,16] = 'SIXTEEN '
	Cardinal[02,17] = 'SEVENTEEN '
	Cardinal[02,18] = 'EIGHTEEN '
	Cardinal[02,19] = 'NINETEEN '
	Cardinal[02,20] = 'TWENTY '
	Cardinal[02,21] = 'TWENTY ONE '
	Cardinal[02,22] = 'TWENTY TWO  '
	Cardinal[02,23] = 'TWENTY THREE '
	Cardinal[02,24] = 'TWENTY FOUR '
	Cardinal[02,25] = 'TWENTY FIVE '
	Cardinal[02,26] = 'TWENTY SIX '
	Cardinal[02,27] = 'TWENTY SEVEN '
	Cardinal[02,28] = 'TWENTY EIGTH '
	Cardinal[02,29] = 'TWENTY NINE '
	Cardinal[02,30] = 'THIRTY  '
	Cardinal[02,31] = 'FORTY '
	Cardinal[02,32] = 'FIFTY '
	Cardinal[02,33] = 'SIXTY '
	Cardinal[02,34] = 'SEVENTY '
	Cardinal[02,35] = 'EIGHTY '
	Cardinal[02,36] = 'NINETY '
	Cardinal[02,37] = 'ONE HUNDRED '
	Cardinal[02,38] = 'TWO HUNDRED '
	Cardinal[02,39] = 'THREE HUNDRED '
	Cardinal[02,40] = 'FOUR HUNDRED '
	Cardinal[02,41] = 'FIVE HUNDRED '
	Cardinal[02,42] = 'SIX HUNDRED '
	Cardinal[02,43] = 'SEVEN HUNDRED '
	Cardinal[02,44] = 'EIGHT HUNDRED '
	Cardinal[02,45] = 'NINE HUNDRED '
RETURN
*ENDPROC