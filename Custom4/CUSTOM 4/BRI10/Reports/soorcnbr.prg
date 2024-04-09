*:***************************************************************************
*: Program file        : SOORDBR.PRG
*: Program description : Custom Order confirmation for Brisco
*: Module              : SO (AR)
*: Developer           : MOhamed Shokry (MHM)      
*: Tracking Job Number : C038272
*: Date                : 12/10/2006
*:***************************************************************************
*: Calls :             
*:             Programs: 
*:              Screens: 
*:      Global Function:
*:***************************************************************************
*: Called From: SOORDBR.PRG
*:**********************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOORDBR
*:**************************************************************************
*! Modifications:
*:**************************************************************************

*-- Open needed Tables remotely [Start]
lcTrackNo = ''
WORKFILE = ''


=gfOpenFile(gcDataDir+"PACK_HDR","ORDERPCK",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PACK_HDR","ORDERPCK",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFLIST","PROFILE",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFVALU","PROFILE",'SH', .T., .T.)

*-- Open needed Tables remotely [End]
=lfCreateTemp()


m.nlineno = 0
lcOrder = ""
SELECT (lcTempOrd)
SCAN 
    IF lcOrder  <> Order 
      m.nlineno = 0
      lcOrder  = Order 
    ENDIF
    SCATTER MEMVAR MEMO
    =SEEK('S'+&lcTempOrd..Scale,'SCALE')
    FOR lnCount = 1 TO SCALE.CNT
      lcCount = ALLTRIM(STR(lnCount,1))
      IF &lcTempOrd..Qty&lcCount <> 0 
	      m.nlineno = m.nlineno+1
	      
	      SELECT (WORKFILE)
	      APPEND BLANK
	      GATHER MEMVAR MEMO
	      REPLACE nQty    WITH &lcTempOrd..Qty&lcCount

	      IF !EMPTY(Style.qty_ctn)
	        REPLACE nCSPK   WITH Style.qty_ctn
	        REPLACE nCSNum  WITH CEILING(&lcTempOrd..Qty&lcCount/IIF(Style.qty_ctn>0,Style.qty_ctn,1))
	      ELSE
	        m.nCSPK =lfGetCSPK()
  	        REPLACE nCSNum  WITH IIF(m.nCSPK>0,CEILING(&lcTempOrd..Qty&lcCount/m.nCSPK),0)
	      ENDIF
	      
	      REPLACE DESC1   WITH ALLTRIM(DESC1) 
	      REPLACE cSize   WITH ALLTRIM(Scale.sz&lcCount)
	      REPLACE nlineno WITH m.nlineno
	      =lfprofline()
	  ENDIF    
    ENDFOR
ENDSCAN

SELECT (lcTempOrd )
SET RELATION OFF INTO OBJLINK_A 
SET RELATION OFF INTO SCALE 
SET RELATION OFF INTO Style  

lcTitle = "NOTES"
LOCATE
IF llPrntBoth
  SELECT (lcTempOrd )
  SET RELATION OFF INTO (lcNoteLns)
  SELECT (WORKFILE)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  *lcSkipExpr = [Ordline,&lcNoteLns]
  *ramy
  *lcSkipExpr = [&lcTempOrd,&lcNoteLns]
  
  *:B802910,1 BWA  12/30/1999 Fix the bug of not printing the notepad.[START]
  *lcSkipExpr = [&lcTempOrd]
  lcSkipExpr = [&lcTempOrd,&lcNoteLns]
  *:B802910,1 BWA  12/30/1999 [END]
ENDIF
lcTempOrd = WORKFILE

SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT ORDHDR
LOCATE
SET RELATION TO "B"+ order INTO Notepad ADDITIVE
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE
SET SKIP TO (lcTempOrd)

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Heba Fathi (HFK)
*! Date      : 11/11/2004
*! Purpose   : create temporary files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION lfCreateTemp

SELECT (lcTempOrd)
= AFIELDS(laTempStru)
lnTempStr = ALEN(laTempStru,1)
DIMENSION laTempStru[lnTempStr + 15, 18]

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

laTempStru[lnTempStr+6 ,1] = 'cProfil1'
laTempStru[lnTempStr+6 ,2] = 'C'
laTempStru[lnTempStr+6 ,3] = 30
laTempStru[lnTempStr+6 ,4] = 0


laTempStru[lnTempStr+7 ,1] = 'cProfil2'
laTempStru[lnTempStr+7 ,2] = 'C'
laTempStru[lnTempStr+7 ,3] = 30
laTempStru[lnTempStr+7 ,4] = 0


laTempStru[lnTempStr+8 ,1] = 'cProfil3'
laTempStru[lnTempStr+8 ,2] = 'C'
laTempStru[lnTempStr+8 ,3] = 30
laTempStru[lnTempStr+8 ,4] = 0


laTempStru[lnTempStr+9 ,1] = 'cProfil4'
laTempStru[lnTempStr+9 ,2] = 'C'
laTempStru[lnTempStr+9 ,3] = 30
laTempStru[lnTempStr+9 ,4] = 0

laTempStru[lnTempStr+10 ,1] = 'cProfil5'
laTempStru[lnTempStr+10 ,2] = 'C'
laTempStru[lnTempStr+10 ,3] = 30
laTempStru[lnTempStr+10 ,4] = 0


laTempStru[lnTempStr+11 ,1] = 'cProfil6'
laTempStru[lnTempStr+11 ,2] = 'C'
laTempStru[lnTempStr+11 ,3] = 30
laTempStru[lnTempStr+11 ,4] = 0

laTempStru[lnTempStr+12 ,1] = 'cProfil7'
laTempStru[lnTempStr+12 ,2] = 'C'
laTempStru[lnTempStr+12 ,3] = 30
laTempStru[lnTempStr+12 ,4] = 0

laTempStru[lnTempStr+13 ,1] = 'cProfil8'
laTempStru[lnTempStr+13 ,2] = 'C'
laTempStru[lnTempStr+13 ,3] = 30
laTempStru[lnTempStr+13 ,4] = 0

laTempStru[lnTempStr+14 ,1] = 'cProfil9'
laTempStru[lnTempStr+14 ,2] = 'C'
laTempStru[lnTempStr+14 ,3] = 30
laTempStru[lnTempStr+14 ,4] = 0

laTempStru[lnTempStr+15 ,1] = 'cProfil10'
laTempStru[lnTempStr+15 ,2] = 'C'
laTempStru[lnTempStr+15 ,3] = 30
laTempStru[lnTempStr+15 ,4] = 0


FOR lncount = 1 TO 15 
   STORE ' ' TO  laTempStru[lnTempStr+lncount,7],laTempStru[lnTempStr+lncount,8],;
            laTempStru[lnTempStr+lncount,9],laTempStru[lnTempStr+lncount,10],;
            laTempStru[lnTempStr+lncount,11],laTempStru[lnTempStr+lncount,12],;
            laTempStru[lnTempStr+lncount,13],laTempStru[lnTempStr+lncount,14],;
            laTempStru[lnTempStr+lncount,15],laTempStru[lnTempStr+lncount,16]
  STORE 0 TO    laTempStru[lnTempStr+lncount,17] ,laTempStru[lnTempStr+lncount,18]
ENDFOR
 

WORKFILE = loOGScroll.gfTempName()

IF lcRpSortBy = 'S'
  gfCrtTmp(WORKFILE,@laTempStru,"CORDTYPE + ORDER + STORE + STYLE+cSize",WORKFILE,.F.)
ELSE
  gfCrtTmp(WORKFILE,@laTempStru,"CORDTYPE + ORDER + STORE + STR(nLINENO,6)+ cSize",WORKFILE,.F.)
ENDIF

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Heba Fathi (HFK)
*! Date      : 11/11/2004
*! Purpose   : create temporary files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION lfprofline

SELECT Profvalu 
lnLineNo = 0
IF SEEK('SOO'+ &lcTempOrd..order+STR(&lcTempOrd..LINENO,6))
  SCAN REST WHILE cpro_type+ckey+cpro_code = 'SOO'+ &lcTempOrd..order+STR(&lcTempOrd..LINENO,6)
    lnLineNo = lnLineNo+1
    lcLineNo = ALLTRIM(STR(lnLineNo,2))
    REPLACE &WORKFILE..cProfil&lcLineNo WITH ALLTRIM(Profvalu .cpro_value)
  ENDSCAN
ENDIF
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

IF SEEK("ST"+LEFT(Style,130)+codes.ccode_no,"PROFVALU") OR SEEK("ST"+LEFT(LEFT(Style,12)+"-******",130)+codes.ccode_no,"PROFVALU")
  SELECT (lnAlias)
  RETURN VAL(LEFT(cpro_value,2))
ELSE
  SELECT (lnAlias)
  RETURN 0  
ENDIF