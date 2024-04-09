*:*************************************************************
*: Name      : lfBRTSO  lfBRTSO
*: Developer : Waleed Hamed (WLD)
*: Date      : 04/03/2003
*: Purpose   : GET BERYTOS SALES ORDER #
*:*************************************************************
*: Called from : POSTYPHK.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : lcReturn
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfUsrVldFn('lfBRTSO','POSTYPHK')
*:*************************************************************
*: Modifications :
*:B607343,1 SSE 06/12/2003 Fix bug NotePad is not printing across the whole line.
*:B124541,1 NNA 10/21/2004 Fix bug about error while trying to print without selecting any trans.
*:C125092,1 BWA 11/18/2004 Add new field in the FRX which is the Status field.
*:*************************************************************

*B607343,1 Run the report from the temp PosHdr file. [Begin]
llPoDisp = .F.
DO lpBrFrmTmp
*B607343,1 Run the report from the temp PosHdr file. [End]

FUNCTION lfBRTSO
PARAMETERS lcReturn
PRIVATE lcBerytoSo
Store '' to lcBerytoSo
lcAlias = ALIAS()

IF !USED('CUTPICK')
	=gfopenfile(gcdatadir+'CUTPICK','CUTPICK','SH')
ENDIF
select CUTPICK
IF SEEK('2'+PosHDR.Po,'CUTPICK')
	lcBerytoSo = order
ENDIF
lcreturn = lcBerytoSo
select &lcAlias

*:*************************************************************
*: Name      : lfGmaStypo
*: Developer : Waleed Hamed (WLD)
*: Date      : 04/03/2003
*: Purpose   : To delete * form Style and PO notes.
*:*************************************************************
*: Called from : POSTYPHK.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : lcReturn
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfUsrVldFn('lfGmaStyPO','POSTYPHK')
*:*************************************************************
*:
FUNCTION lfgmastypo
PARAMETERS lcreturn
PRIVATE lclstnote
STORE '' TO lclstnote

IF !EMPTY(lcnotes)
	lnnotline = 1
	lnoldmemw = SET("MEMOWIDTH")

    *B607343,1 Increase line width. [Begin]  
    *SET MEMOWIDTH TO 75
    SET MEMOWIDTH TO 100
    *B607343,1 Increase line width. [End]

	lnmemlins = MEMLINES(lcnotes)
	DIMENSION lanot[lnMemLins]
	DO WHILE lnnotline <= lnmemlins
		IF  '*' <> LEFT(MLINE(lcnotes,lnnotline),1)
			lanot[lnNotLine]= MLINE(lcnotes,lnnotline)
		ELSE
			lanot[lnNotLine] = ''
		ENDIF
		lnnotline = lnnotline + 1
	ENDDO
	FOR i=1 TO   lnmemlins
		IF !EMPTY(lanot[I])
			IF i <> lnmemlins
				lclstnote = lclstnote + lanot[i]+ CHR(13)
			ELSE
				lclstnote = lclstnote + lanot[i]
			ENDIF
		ENDIF
	ENDFOR
	lcreturn = lclstnote
ENDIF

*!***************************************************************************
*! Name      : lpBrFrmTmp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2003
*! Purpose   : Browse data from temp files instead of POSHDR and POSLINE 
*!             To overcome slow in report display
*!***************************************************************************
*! Called from : POSTYPHK.FRX
*!***************************************************************************
*B607343,1
FUNCTION lpBrFrmTmp
PRIVATE lcHdrReln,lcHdrSkip,lcHdrOrd,lcLnReln,lcLnSkip,lnPos,lcTmpPOhdr
*--Save POSHDR,POSLN relations
SELECT POSHDR
lcHdrReln = SET('RELATION')
lcHdrSkip = SET('SKIP')
lcHdrOrd = ORDER()
SET RELATION TO
lcTmpPOhdr = gfTempName()
COPY STRUCTURE TO (gcWorkDir+lcTmpPOhdr) WITH CDX

*-* SELECT POSLN
*-* lcLnReln = SET('RELATION')
*-* lcLnSkip = SET('SKIP')
*-* SET RELATION TO 

*--Make a new copy of POSHDR and POSLN files in a temp file with only selected po's
lnPos = ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'POSHDR.PO') , 1)
lcSlcted = laOgFxFlt[lnPos,6]

*B124541,1 NNA 10/21/2004 (Begin) Return if user didn't select any transaction
IF !USED(lcSlcted) .OR. RECCOUNT(lcSlcted)=0
  =gfModalGen("TRM000000B00000","DIALOG",'','','You have to select a transaction')
  RETURN
ELSE
*B124541,1 NNA (End)

  SELECT (lcSlcted)
  LOCATE
  SCAN FOR SEEK('P'+&lcSlcted..PO,'POSHDR')
    SELECT POSHDR
    SCATTER MEMVAR MEMO
    INSERT INTO (gcWorkDir+lcTmpPOhdr) FROM MEMVAR
  ENDSCAN
  IF USED(lcTmpPoHdr)
    USE IN (lcTmpPoHdr)
  ENDIF

*B124541,1 NNA (Begin)
ENDIF
*B124541,1 NNA (End)

*--Open the temp file with the same alias of POSHDR
USE (gcWorkDir+lcTmpPOhdr) AGAIN IN POSHDR ALIAS POSHDR

SELECT POSHDR
SET RELATION TO &lcHdrReln
SET SKIP TO &lcHdrSkip
SET ORDER TO TAG &lcHdrOrd
LOCATE

*--Run the report
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*--Reopen the original POSHDR file
USE (gcDataDir+'POSHDR') IN POSHDR ORDER TAG (lcHdrOrd)
SET RELATION TO &lcHdrReln
SET SKIP TO &lcHdrSkip

*--Erase the temp file
ERASE (gcWorkDir+lcTmpPOhdr+'.DBF')
ERASE (gcWorkDir+lcTmpPOhdr+'.CDX')
ERASE (gcWorkDir+lcTmpPOhdr+'.FTP')
*-- End of lpBrFrmTmp.