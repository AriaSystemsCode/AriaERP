****************************************************************************
*: Program file  : ALPKLSBR.PRG --- ALPKLSBR.FRX
*: Program desc. : Custom PL Form for Brisco
*: System        : Brisco
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Ahmed Sherif
*: Date          : 04/12/2018
*:**************************************************************************
IF !llOGFltCh
  RETURN 
ENDIF

=gfOpenFile(gcDataDir+"PROFLIST","PROFILE",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFVALU","PROFILE",'SH', .T., .T.)
WORKFILE = ' '
=lfCreateTemp()

SET SKIP TO

SELECT (WORKFILE)
INDEX ON Pack_No+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp)
INDEX ON Pack_No+STR(no_cart,4)+STYLE TAG 'PACKCRTN' ADDITIVE

SELECT (lcPakLnTmp)
LOCATE
nLine = 0
SCAN
	SCATTER MEMVAR MEMO
	=SEEK(&lcPakLnTmp..STYLE,lcStyleFile)
	lcScale = &lcStyleFile..SCALE
	=SEEK('S'+lcScale,lcScaleFile)
	FOR lnCount = 1 TO &lcScaleFile..CNT
		lcCount = ALLTRIM(STR(lnCount,1))
		IF &lcPakLnTmp..Qty&lcCount <> 0
			nLine = nLine+1
			SELECT (WORKFILE)
			APPEND BLANK
			GATHER MEMVAR MEMO
			REPLACE nQty    WITH &lcPakLnTmp..Qty&lcCount
			IF !EMPTY(&lcStyleFile..qty_ctn)
				REPLACE nCSPK   WITH &lcStyleFile..qty_ctn
				REPLACE nCSNum  WITH CEILING(&lcPakLnTmp..Qty&lcCount/IIF(&lcStyleFile..qty_ctn>0,&lcStyleFile..qty_ctn,1))
			ELSE
				m.nCSPK =lfGetCSPK()
				REPLACE nCSPK   WITH m.nCSPK
				REPLACE nCSNum  WITH IIF(m.nCSPK>0,CEILING(&lcPakLnTmp..Qty&lcCount/m.nCSPK),0)
			ENDIF
			REPLACE cSize   WITH ALLTRIM(&lcScaleFile..sz&lcCount)
			REPLACE nlineno WITH nLine
		ENDIF
	ENDFOR
ENDSCAN


SELECT(lcPackTmp)
SET RELATION TO

lcPakLnTmp = WORKFILE

SELECT (lcPakLnTmp)
LOCATE
IF  EOF()
	RETURN
ENDIF

SELECT(lcPackTmp)
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + STORE INTO &lcCustomer ,;
    "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
    "B" + ORDER             INTO &lcNotePad  ,;
    Pack_No                 INTO (lcPakLnTmp) ,;
    "O" + ORDER             INTO &lcOrdhdr  ,;
    invoice                 INTO &lcInvLnTmp ,;
    piktkt                  INTO &lcTempPikTkt ADDITIVE

SET SKIP TO (lcPakLnTmp)
SELECT (lcPackTmp)
LOCATE

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/01/2007
*! Purpose   : create temporary files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION lfCreateTemp

	SELECT (lcPakLnTmp)
	= AFIELDS(laTempStru)
	lnTempStr = ALEN(laTempStru,1)
	DIMENSION laTempStru[lnTempStr + 5, 18]

	*-- nIn_Trans :  field to calculate in transit
	laTempStru[lnTempStr+1 ,1] = 'nQty'
	laTempStru[lnTempStr+1 ,2] = 'N'
	laTempStru[lnTempStr+1 ,3] = 8
	laTempStru[lnTempStr+1 ,4] = 0

	laTempStru[lnTempStr+2 ,1] = 'nCSPK'
	laTempStru[lnTempStr+2 ,2] = 'N'
	laTempStru[lnTempStr+2 ,3] = 8
	laTempStru[lnTempStr+2 ,4] = 0

	laTempStru[lnTempStr+3 ,1] = 'nCSNum'
	laTempStru[lnTempStr+3 ,2] = 'N'
	laTempStru[lnTempStr+3 ,3] = 8
	laTempStru[lnTempStr+3 ,4] = 0

	laTempStru[lnTempStr+4 ,1] = 'cSize'
	laTempStru[lnTempStr+4 ,2] = 'C'
	laTempStru[lnTempStr+4 ,3] = 5
	laTempStru[lnTempStr+4 ,4] = 0


	laTempStru[lnTempStr+5 ,1] = 'nLineNo'
	laTempStru[lnTempStr+5 ,2] = 'N'
	laTempStru[lnTempStr+5 ,3] = 6
	laTempStru[lnTempStr+5 ,4] = 0

	FOR lnCount = 1 TO 5
		STORE ' ' TO  laTempStru[lnTempStr+lncount,7],laTempStru[lnTempStr+lncount,8],;
			laTempStru[lnTempStr+lncount,9],laTempStru[lnTempStr+lncount,10],;
			laTempStru[lnTempStr+lncount,11],laTempStru[lnTempStr+lncount,12],;
			laTempStru[lnTempStr+lncount,13],laTempStru[lnTempStr+lncount,14],;
			laTempStru[lnTempStr+lncount,15],laTempStru[lnTempStr+lncount,16]
		STORE 0 TO    laTempStru[lnTempStr+lncount,17] ,laTempStru[lnTempStr+lncount,18]
	ENDFOR

	WORKFILE = loOGScroll.gfTempName()
	gfCrtTmp(WORKFILE,@laTempStru,"pack_no+STYLE+STR(nOrdLineNO,6)+STR(nlineno,6)+cSize",WORKFILE,.F.)

*!*************************************************************
*! Name      : lfGetCSPK
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/01/2007
*! Purpose   : Function to Get Profile Codes.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetCSPK
	PRIVATE lnAlias
	lnAlias = ALIAS()
	SELECT CODES
	SET ORDER TO 2
	=SEEK('N'+"CPRO_CODE ",'CODES')

	SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+"CPRO_CODE "
		IF ALLTRIM(cdiscrep) = "Case Pack"
			EXIT
		ENDIF
	ENDSCAN

	*IF SEEK("ST"+LEFT(Style,130)+codes.ccode_no,"PROFVALU") OR SEEK("ST"+LEFT(LEFT(Style,12)+"-******",130)+codes.ccode_no,"PROFVALU")
	*! B609771,1 MMT 12/14/2011 Custom Invoice form prints incorrect pack qty [Start]
	*IF SEEK('SOO'+ &WORKFILE..order+STR(&WORKFILE..LINENO,6)+"     1"+SPACE(111)+codes.ccode_no,"PROFVALU")
	IF SEEK('SO'+ PADR('O'+&lcOrdhdr..Order+STR(&WORKFILE..nOrdLineNO,6),130)+CODES.ccode_no,"PROFVALU")
	*! B609771,1 MMT 12/14/2011 Custom Invoice form prints incorrect pack qty [End]
		SELECT (lnAlias)
	*! B608317,1 SSH 10/11/2007 Incorrect Profile value the substring should be to the first space[T20070712.0026]
	*RETURN VAL(LEFT(PROFVALU.cpro_value,2))
		RETURN VAL(SUBSTR(ALLTRIM(PROFVALU.cpro_value),1,AT(SPACE(1) ,ALLTRIM(PROFVALU.cpro_value))-1))
	*! B608317,1 SSH [END]
	ELSE
		SELECT (lnAlias)
		RETURN 0
	ENDIF

FUNCTION lfGetTerms
	PRIVATE lnAlias
	lnAlias = ALIAS()
	lcTermCode = &lcOrdhdr..Ctermcode
	SELECT CODES
	LOCATE FOR ccode_no = lcTermCode AND crltfield = 'N'
	SELECT (lnAlias)
	RETURN ALLTRIM(CODES.Cdiscrep)
ENDFUNC
	
	
