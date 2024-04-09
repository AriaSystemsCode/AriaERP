*:***************************************************************************
*: Program file        : ARPINVGM.PRG
*: Program description : ACCOUNT RECEIVABLE INVOICE FOR GMA ACCOSORIES
*: Module              : ACCOUNT RECEIVABLE (AR)
*: Developer           : Mohamed SHokry (MHM)      
*: Tracking Job Number : 
*: Date                : 01/10/2007
*:***************************************************************************
*: Calls :             
*:             Programs: 
*:              Screens: 
*:      Global Function:
*:***************************************************************************
*: Called From: ARPINV.PRG
*:**********************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVBR
*:**************************************************************************
*! Modifications:
*! B608295,1 MMT 09/27/2007 fix bug of wrong ups Tracking No. printed for invoice has not PIKTKT[T20070712.0026]
*! B608317,1 SSH 10/11/2007 Incorrect Profile value the substring should be to the first space[T20070712.0026]
*! B609771,1 MMT 12/14/2011 Custom Invoice form prints incorrect pack qty [T20111123.0024]
*:**************************************************************************

*-- Open needed Tables remotely [Start]
lcTrackNo = ''
=gfOpenFile(gcDataDir+"PACK_HDR","Pack_hdr",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFLIST","PROFILE",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFVALU","PROFILE",'SH', .T., .T.)
*-- Open needed Tables remotely [End]
=lfCreateTemp()
lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)

SELECT INVHDR
SET SKIP TO
SCAN FOR &lcRpEXP
  =SEEK(INVOICE,'INVLINE')
  m.nlineno = 0
  SELECT INVLINE
  SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE
    SCATTER MEMVAR MEMO
    =SEEK('S'+INVLINE.Scale,'SCALE')
    FOR lnCount = 1 TO SCALE.CNT
      lcCount = ALLTRIM(STR(lnCount,1))
      IF InvLine.Qty&lcCount <> 0 
	      m.nlineno = m.nlineno+1
	      SELECT (WORKFILE)
	      APPEND BLANK
	      GATHER MEMVAR MEMO
	      REPLACE nQty    WITH InvLine.Qty&lcCount
	      IF !EMPTY(Style.qty_ctn)
	        REPLACE nCSPK   WITH Style.qty_ctn
    	    REPLACE nCSNum  WITH CEILING(InvLine.Qty&lcCount/IIF(Style.qty_ctn>0,Style.qty_ctn,1))
	      ELSE
	        m.nCSPK =lfGetCSPK()
	        REPLACE nCSPK   WITH m.nCSPK
	        REPLACE nCSNum  WITH IIF(m.nCSPK>0,CEILING(InvLine.Qty&lcCount/m.nCSPK),0)
	      ENDIF
	      REPLACE DESC1   WITH ALLTRIM(DESC1) 
	      REPLACE cSize   WITH ALLTRIM(Scale.sz&lcCount)
	      REPLACE nlineno WITH m.nlineno
	  ENDIF    
    ENDFOR
  ENDSCAN
ENDSCAN

SELECT (WORKFILE)
LOCATE
IF  EOF()
  RETURN
ENDIF

SELECT INVHDR
IF llRpInvNot
  SET RELATION OFF INTO (lcTmpDbt)
  SET RELATION TO '2' INTO (lcTmpDbt)ADDITIVE
ENDIF

SET RELATION OFF INTO (WORKFILE)
SET RELATION OFF INTO PACK_HDR

SET RELATION TO INVHDR.INVOICE INTO (WORKFILE) ADDITIVE
SET RELATION TO INVHDR.ORDER INTO PACK_HDR ADDITIVE

SET SKIP TO (WORKFILE)
SELECT INVHDR
LOCATE

*!*************************************************************
*! Name      : gfGetZone
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/01/2007
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION lfGetUPS
PARAMETER lcReturn

IF TYPE('loArUps')<>'O'
  loArUps = CreateObject('RemoteTable','ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("Datasession"),,.T.)
ENDIF 

*B608295,1 MMT 09/27/2007 fix bug of wrong ups Tracking No. printed for invoice has not PIKTKT[Start]
IF !EMPTY(INVHDR.piktkt)
*B608295,1 MMT 09/27/2007 fix bug of wrong ups Tracking No. printed for invoice has not PIKTKT[End]
  loArUps.SEEK(INVHDR.piktkt )
  lcTrackNo = ARUPSSHP.ctrack_no
*B608295,1 MMT 09/27/2007 fix bug of wrong ups Tracking No. printed for invoice has not PIKTKT[Start]
ELSE
  lcTrackNo =''
ENDIF 
*B608295,1 MMT 09/27/2007 fix bug of wrong ups Tracking No. printed for invoice has not PIKTKT[End]  
lcReturn = .T.
*!
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

SELECT INVLINE
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

FOR lncount = 1 TO 5 
   STORE ' ' TO  laTempStru[lnTempStr+lncount,7],laTempStru[lnTempStr+lncount,8],;
            laTempStru[lnTempStr+lncount,9],laTempStru[lnTempStr+lncount,10],;
            laTempStru[lnTempStr+lncount,11],laTempStru[lnTempStr+lncount,12],;
            laTempStru[lnTempStr+lncount,13],laTempStru[lnTempStr+lncount,14],;
            laTempStru[lnTempStr+lncount,15],laTempStru[lnTempStr+lncount,16]
  STORE 0 TO    laTempStru[lnTempStr+lncount,17] ,laTempStru[lnTempStr+lncount,18]
ENDFOR
 

WORKFILE = loOGScroll.gfTempName()
gfCrtTmp(WORKFILE,@laTempStru,"invoice+STR(lineno,6)+STR(nlineno,6)+cSize",WORKFILE,.F.)

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
IF SEEK('SO'+ PADR('O'+&WORKFILE..order+STR(&WORKFILE..LINENO,6),130)+codes.ccode_no,"PROFVALU") 
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