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
*: B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found" [T20070529.0009]
*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [T20071026.0030]
*: C200965,2 MMT 03/26/2008 Change form to print infromation from CRM  [T20071026.0030]
*: C200965,3 MMT 05/04/2008 Bug of not printing case pack qty		   [T20071026.0030]
*: C200990,1 MMH 05/08/2008 add color to the report and also some piks
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [T20080825.0001]       
*! B608775,1 MMT 12/25/2008 Fix bug of wrong page# in So confirmation report[T20080825.0001]
*: B609435,1 MMT 10/14/2010 Modify report to always get the column CS PK from the order line[T20100922.0018]
*:**************************************************************************

*-- Open needed Tables remotely [Start]
lcTrackNo = ''
WORKFILE = ''

*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start]       
lnPcnt    = 1
lcLastOrd = SPACE(6)
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       


=gfOpenFile(gcDataDir+"PACK_HDR","ORDERPCK",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PACK_HDR","ORDERPCK",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFLIST","PROFILE",'SH', .T., .T.)
=gfOpenFile(gcDataDir+"PROFVALU","PROFILE",'SH', .T., .T.)

*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
=gfOpenFile(gcDataDir+"ORDDSGN","ORDLINE",'SH', .T., .T.)
*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]

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
    *: C200990,1 MMH 05/08/2008 add color to the report and also some piks [Start]
	    SELECT STYLE  
	    IF (cclrprss1= "SP" OR cclrprss2= "SP" OR cclrprss3= "SP" OR cclrprss4= "SP")
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order  WITH &lcTempOrd..Order
	        REPLACE llRpSP WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpSP WITH .T.
	      ENDIF
	      
	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "HT" OR cclrprss2= "HT" OR cclrprss3= "HT" OR cclrprss4= "HT")
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order  WITH &lcTempOrd..Order
	        REPLACE llRpHT WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpHT WITH .T.
	      ENDIF
	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "SC" OR cclrprss2= "SC" OR cclrprss3= "SC" OR cclrprss4= "SC")
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order  WITH &lcTempOrd..Order
	        REPLACE llRpSC WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpSC WITH .T.
	      ENDIF
	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "JC" OR cclrprss2= "JC" OR cclrprss3= "JC" OR cclrprss4= "JC")
	      
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order   WITH &lcTempOrd..Order
	        REPLACE llRpJC  WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpJC  WITH .T.
	      ENDIF
	      
	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "EMB" OR cclrprss2= "EMB" OR cclrprss3= "EMB" OR cclrprss4= "EMB")
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order    WITH &lcTempOrd..Order
	        REPLACE llRpEMB  WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpEMB  WITH .T.
	      ENDIF
	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "SAM" OR cclrprss2= "SAM" OR cclrprss3= "SAM" OR cclrprss4= "SAM")
	      
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order    WITH &lcTempOrd..Order
	        REPLACE llRpSAM  WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpSAM  WITH .T.
	      ENDIF

	    ENDIF

	    SELECT STYLE  
	    IF (cclrprss1= "PKG" OR cclrprss2= "PKG" OR cclrprss3= "PKG" OR cclrprss4= "PKG")
	      IF !SEEK(&lcTempOrd..Order,lcTmphdr)
	        SELECT (lcTmphdr)
	        APPEND BLANK
	        REPLACE Order    WITH &lcTempOrd..Order
	        REPLACE llRpPKG  WITH .T.
	      ELSE
	        SELECT (lcTmphdr)
	        REPLACE llRpPKG  WITH .T.
	      ENDIF
	    ENDIF
    
    *: C200990,1 MMH 05/08/2008 add color to the report and also some piks [End]
    SELECT (lcTempOrd)
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
          *B609435,1 MMT 10/14/2010 Modify Brisco custom order confirmation report to always get the column CS PK from the order line[Start]
*!*		      IF !EMPTY(Style.qty_ctn)
*!*		        REPLACE nCSPK   WITH Style.qty_ctn
*!*		        REPLACE nCSNum  WITH CEILING(&lcTempOrd..Qty&lcCount/IIF(Style.qty_ctn>0,Style.qty_ctn,1))
*!*		        
*!*		        *: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
*!*		        REPLACE nCasPk  WITH &lcTempOrd..Qty&lcCount/IIF(Style.qty_ctn>0,Style.qty_ctn,1)
*!*				*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]
*!*		      ELSE
*!*	          *B609435,1 MMT 10/14/2010 Modify Brisco custom order confirmation report to always get the column CS PK from the order line[End]	      
	        m.nCSPK =lfGetCSPK()
    	    SELECT (WORKFILE)
  	      REPLACE nCSNum  WITH IIF(m.nCSPK>0,CEILING(&lcTempOrd..Qty&lcCount/m.nCSPK),0)
  	        
  	        *: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
  	        
  	        *: C200965,1 MMT 05/04/2008 Fix bug of not printing case pack [Start]
  	      REPLACE nCSPK   WITH m.nCSPK
  	        *: C200965,1 MMT 05/04/2008 Fix bug of not printing case pack [End]
  	        
	        REPLACE nCasPk  WITH IIF(m.nCSPK>0,&lcTempOrd..Qty&lcCount/m.nCSPK,0)
	        *: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]

  	    *B609435,1 MMT 10/14/2010 Modify Brisco custom order confirmation report to always get the column CS PK from the order line[Start]    
	      *ENDIF
          *B609435,1 MMT 10/14/2010 Modify Brisco custom order confirmation report to always get the column CS PK from the order line[END]	      
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
*: C200990,1 MMH 05/08/2008 add color to the report and also some piks [Start]
SET RELATION TO order INTO (lcTmphdr) ADDITIVE
*: C200990,1 MMH [End]

SET SKIP TO (lcTempOrd)

*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start]       
llOneOrder = .F.
lnPosOrder = ASCAN(loOgScroll.laOgVRFlt,"ORDHDR.ORDER")
IF lnPosOrder > 0 
  lnPosOrder = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosOrder,1)
  lcCursorOrder= loOgScroll.laOgVRFlt[lnPosOrder,6]
  IF !EMPTY(lcCursorOrder)
    SELECT(lcCursorOrder)
    LOCATE
    IF !EOF()
      COUNT FOR !DELETED() TO lnCntOrd
      IF lnCntOrd = 1 
        llOneOrder = .T.
      ENDIF 
    ENDIF 
  ENDIF   
ENDIF 



*B608775,1 MMT 12/25/2008 Fix bug of wrong page# in So confirmation report[Start]
*!* llGetPage = .T.
*!*	SELECT ordhdr 
*!*	lcOldDev = SET("Device") 
*!*	SET DEVICE TO SCREEN && FILE (oAriaApplication.WorkDir+WORKFILE+'.txt')
*!*	REPORT FORM oAriaApplication.ReportHome+'SO\SOORCNBR.FRx' FOR Eval(lcRpExp) NOCONSOLE 
*!*	IF llOneOrder 
*!*	  SELECT (lcTempOrd)
*!*	  REPLACE ALL  nPageNo WITH _pageTotal  IN (lcTempOrd)
*!*	ENDIF
*!*	SET DEVICE TO &lcOldDev 
*!*	llGetPage = .F.
*!*	SELECT ordhdr 
lcFileOld = oAriaApplication.gcoutFile 
lcOldDevice = oAriaApplication.gcDevice 
lcOldType = loogscroll.cTextRepType 

oAriaApplication.gcDevice = 'FILE'
oAriaApplication.gcoutFile = oAriaApplication.Workdir+loogscroll.gftempName()+'.PDF'
loogscroll.cTextRepType = "PDF"
loogScroll.cCROrientation = 'P'
loOGScroll.cCRPapersize = 'LETTER'

llGetPage = .T.
SELECT ordhdr 
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
IF llOneOrder 
  SELECT (lcTempOrd)
  REPLACE ALL  nPageNo WITH _pageTotal  IN (lcTempOrd)
ENDIF
llGetPage = .F.
SELECT ordhdr 

oAriaApplication.gcDevice = lcOldDevice 
oAriaApplication.gcoutFile = lcFileOld 
loogscroll.cTextRepType = lcOldType 
*B608775,1 MMT 12/25/2008 Fix bug of wrong page# in So confirmation report[End]
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       


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

*:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[Start]
*DIMENSION laTempStru[lnTempStr + 15, 18]

*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
*DIMENSION laTempStru[lnTempStr + 6, 18]

*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start]       
*DIMENSION laTempStru[lnTempStr + 7, 18]
DIMENSION laTempStru[lnTempStr + 8, 18]
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       

*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]

*:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[End]

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


*:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[Start]
*!*	laTempStru[lnTempStr+6 ,1] = 'cProfil1'
*!*	laTempStru[lnTempStr+6 ,2] = 'C'
*!*	laTempStru[lnTempStr+6 ,3] = 30
*!*	laTempStru[lnTempStr+6 ,4] = 0


*!*	laTempStru[lnTempStr+7 ,1] = 'cProfil2'
*!*	laTempStru[lnTempStr+7 ,2] = 'C'
*!*	laTempStru[lnTempStr+7 ,3] = 30
*!*	laTempStru[lnTempStr+7 ,4] = 0


*!*	laTempStru[lnTempStr+8 ,1] = 'cProfil3'
*!*	laTempStru[lnTempStr+8 ,2] = 'C'
*!*	laTempStru[lnTempStr+8 ,3] = 30
*!*	laTempStru[lnTempStr+8 ,4] = 0


*!*	laTempStru[lnTempStr+9 ,1] = 'cProfil4'
*!*	laTempStru[lnTempStr+9 ,2] = 'C'
*!*	laTempStru[lnTempStr+9 ,3] = 30
*!*	laTempStru[lnTempStr+9 ,4] = 0

*!*	laTempStru[lnTempStr+10 ,1] = 'cProfil5'
*!*	laTempStru[lnTempStr+10 ,2] = 'C'
*!*	laTempStru[lnTempStr+10 ,3] = 30
*!*	laTempStru[lnTempStr+10 ,4] = 0


*!*	laTempStru[lnTempStr+11 ,1] = 'cProfil6'
*!*	laTempStru[lnTempStr+11 ,2] = 'C'
*!*	laTempStru[lnTempStr+11 ,3] = 30
*!*	laTempStru[lnTempStr+11 ,4] = 0

*!*	laTempStru[lnTempStr+12 ,1] = 'cProfil7'
*!*	laTempStru[lnTempStr+12 ,2] = 'C'
*!*	laTempStru[lnTempStr+12 ,3] = 30
*!*	laTempStru[lnTempStr+12 ,4] = 0

*!*	laTempStru[lnTempStr+13 ,1] = 'cProfil8'
*!*	laTempStru[lnTempStr+13 ,2] = 'C'
*!*	laTempStru[lnTempStr+13 ,3] = 30
*!*	laTempStru[lnTempStr+13 ,4] = 0

*!*	laTempStru[lnTempStr+14 ,1] = 'cProfil9'
*!*	laTempStru[lnTempStr+14 ,2] = 'C'
*!*	laTempStru[lnTempStr+14 ,3] = 30
*!*	laTempStru[lnTempStr+14 ,4] = 0

*!*	laTempStru[lnTempStr+15 ,1] = 'cProfil10'
*!*	laTempStru[lnTempStr+15 ,2] = 'C'
*!*	laTempStru[lnTempStr+15 ,3] = 30
*!*	laTempStru[lnTempStr+15 ,4] = 0


*!*	FOR lncount = 1 TO 15 
*!*	   STORE ' ' TO  laTempStru[lnTempStr+lncount,7],laTempStru[lnTempStr+lncount,8],;
*!*	            laTempStru[lnTempStr+lncount,9],laTempStru[lnTempStr+lncount,10],;
*!*	            laTempStru[lnTempStr+lncount,11],laTempStru[lnTempStr+lncount,12],;
*!*	            laTempStru[lnTempStr+lncount,13],laTempStru[lnTempStr+lncount,14],;
*!*	            laTempStru[lnTempStr+lncount,15],laTempStru[lnTempStr+lncount,16]
*!*	  STORE 0 TO    laTempStru[lnTempStr+lncount,17] ,laTempStru[lnTempStr+lncount,18]
*!*	ENDFOR

laTempStru[lnTempStr+6 ,1] = 'mProfile'
laTempStru[lnTempStr+6 ,2] = 'M'
laTempStru[lnTempStr+6 ,3] = 10
laTempStru[lnTempStr+6 ,4] = 0


*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
*FOR lncount = 1 TO 6
laTempStru[lnTempStr+7 ,1] = 'nCasPk'
laTempStru[lnTempStr+7 ,2] = 'N'
laTempStru[lnTempStr+7 ,3] = 7
laTempStru[lnTempStr+7 ,4] = 2

*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start]       
*FOR lncount = 1 TO 7
laTempStru[lnTempStr+8 ,1] = 'nPageNo'
laTempStru[lnTempStr+8 ,2] = 'N'
laTempStru[lnTempStr+8 ,3] = 7
laTempStru[lnTempStr+8 ,4] = 0
FOR lncount = 1 TO 8
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       

*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]
   STORE ' ' TO  laTempStru[lnTempStr+lncount,7],laTempStru[lnTempStr+lncount,8],;
            laTempStru[lnTempStr+lncount,9],laTempStru[lnTempStr+lncount,10],;
            laTempStru[lnTempStr+lncount,11],laTempStru[lnTempStr+lncount,12],;
            laTempStru[lnTempStr+lncount,13],laTempStru[lnTempStr+lncount,14],;
            laTempStru[lnTempStr+lncount,15],laTempStru[lnTempStr+lncount,16]
  STORE 0 TO    laTempStru[lnTempStr+lncount,17] ,laTempStru[lnTempStr+lncount,18]
ENDFOR 
*:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[End]


WORKFILE = loOGScroll.gfTempName()

IF lcRpSortBy = 'S'
  *: C200965,2 MMT 03/26/2008 Change form to print infromation from CRM  [Start]
  *gfCrtTmp(WORKFILE,@laTempStru,"CORDTYPE + ORDER + STORE + STYLE+cSize",WORKFILE,.F.)
  gfCrtTmp(WORKFILE,@laTempStru,"CORDTYPE + ORDER + STORE + STYLE",WORKFILE,.F.)
  *: C200965,2 MMT 03/26/2008 Change form to print infromation from CRM  [End]
ELSE
  gfCrtTmp(WORKFILE,@laTempStru,"CORDTYPE + ORDER + STORE + STR(nLINENO,6)+ cSize",WORKFILE,.F.)
ENDIF

*: C200990,1 MMH 05/08/2008 add color to the report and also some piks [Start]
*create temp file for the needed printed pixs
DIMENSION laTempStru[8, 18]

*-- nIn_Trans :  field to calculate in transit
laTempStru[1 ,1] = 'Order'
laTempStru[1 ,2] = 'C'
laTempStru[1 ,3] = 6
laTempStru[1 ,4] = 0

laTempStru[2 ,1] = 'llRpSP'
laTempStru[2 ,2] = 'L'
laTempStru[2 ,3] = 1
laTempStru[2 ,4] = 0

laTempStru[3 ,1] = 'llRpHT'
laTempStru[3 ,2] = 'L'
laTempStru[3 ,3] = 1
laTempStru[3 ,4] = 0

laTempStru[4 ,1] = 'llRpSC'
laTempStru[4 ,2] = 'L'
laTempStru[4 ,3] = 1
laTempStru[4 ,4] = 0

laTempStru[5 ,1] = 'llRpJC'
laTempStru[5 ,2] = 'L'
laTempStru[5 ,3] = 1
laTempStru[5 ,4] = 0

laTempStru[6 ,1] = 'llRpEMB'
laTempStru[6 ,2] = 'L'
laTempStru[6 ,3] = 1
laTempStru[6 ,4] = 0

laTempStru[7 ,1] = 'llRpSAM'
laTempStru[7 ,2] = 'L'
laTempStru[7 ,3] = 1
laTempStru[7 ,4] = 0

laTempStru[8 ,1] = 'llRpPKG'
laTempStru[8 ,2] = 'L'
laTempStru[8 ,3] = 1
laTempStru[8 ,4] = 0

FOR lncount = 1 TO 8

   STORE ' ' TO  laTempStru[lncount,7],laTempStru[lncount,8],;
            laTempStru[lncount,9],laTempStru[lncount,10],;
            laTempStru[lncount,11],laTempStru[lncount,12],;
            laTempStru[lncount,13],laTempStru[lncount,14],;
            laTempStru[lncount,15],laTempStru[lncount,16]
  STORE 0 TO    laTempStru[lncount,17] ,laTempStru[lncount,18]
ENDFOR 


gfCrtTmp(lcTmphdr,@laTempStru,"Order",lcTmphdr,.F.)

*C200990,1 MMH 05/08/2008 [End]

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

*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
IF SEEK(&lcTempOrd..order+STR(&lcTempOrd..LINENO,6),'ORDDSGN')
  REPLACE &WORKFILE..mProfile WITH 'Design#:'+ padr(ORDDSGN.cdesignid,17)
ENDIF 
*: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]


SELECT Profvalu 
lnLineNo = 0
IF SEEK('SOO'+ &lcTempOrd..order+STR(&lcTempOrd..LINENO,6))
  SCAN REST WHILE cpro_type+ckey+cpro_code = 'SOO'+ &lcTempOrd..order+STR(&lcTempOrd..LINENO,6)
    lnLineNo = lnLineNo+1
    lcLineNo = ALLTRIM(STR(lnLineNo,2))
    *:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[Start]
    *REPLACE &WORKFILE..cProfil&lcLineNo WITH ALLTRIM(Profvalu .cpro_value)
    REPLACE &WORKFILE..mProfile WITH &WORKFILE..mProfile +IIF(!EMPTY(&WORKFILE..mProfile)," ",'')+;
    								 padr(Profvalu .cpro_value,25)
	*:B608155,1 MMT 07/08/2007 fix bug of error "Cprofile11 is not found"[End]
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
*!*	FUNCTION lfGetCSPK
*!*	PRIVATE lnAlias

*!*	lnAlias = ALIAS()
*!*	SELECT CODES
*!*	SET ORDER TO 2
*!*	=SEEK('N'+"CPRO_CODE ",'CODES')

*!*	SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+"CPRO_CODE "
*!*	  IF ALLTRIM(cdiscrep) = "Case Pack"
*!*	    EXIT  
*!*	  ENDIF
*!*	ENDSCAN

*!*	*: C200965,1 MMT 05/04/2008 Fix bug of not printing case pack [Start]
*!*	*IF SEEK("ST"+LEFT(Style,130)+codes.ccode_no,"PROFVALU") OR SEEK("ST"+LEFT(LEFT(Style,12)+"-******",130)+codes.ccode_no,"PROFVALU")
*!*	*IF SEEK("ST"+PADR(Style,130)+codes.ccode_no,"PROFVALU") OR SEEK("ST"+padr(LEFT(Style,12)+"-******",130)+codes.ccode_no,"PROFVALU")
*!*	IF SEEK("SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6),"PROFVALU") 
*!*	  SELECT PROFVALU
*!*	  llCheckto =.F.
*!*	  SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6)
*!*	    IF PROFVALU.cpro_code = codes.ccode_no
*!*	      llCheckto = .T.
*!*	      EXIT
*!*	    ENDIF
*!*	  ENDSCAN 
*!*	  IF llCheckto 
*!*	*: C200965,1 MMT 05/04/2008 Fix bug of not printing case pack [End]

*!*	   SELECT (lnAlias)
*!*	  *: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [Start]
*!*	  *RETURN VAL(LEFT(cpro_value,2))
*!*	    RETURN VAL(SUBSTR(ALLTRIM(PROFVALU.cpro_value),1,AT(SPACE(1) ,ALLTRIM(PROFVALU.cpro_value))-1))
*!*	  ELSE
*!*	    SELECT (lnAlias)
*!*	    RETURN 0  
*!*	  ENDIF
*!*	  *: C200965,1 MMT 03/13/2008 Change form to print infromation from CRM  [End]
*!*	ELSE
*!*	  SELECT (lnAlias)
*!*	  RETURN 0  
*!*	ENDIF
FUNCTION lfGetCSPK
PRIVATE lnAlias
PRIVATE lcCasPkcod,lcBoxCode
STORE "" TO lcCasPkcod,lcBoxCode

lnAlias = ALIAS()
SELECT CODES
SET ORDER TO 2
=SEEK('N'+"CPRO_CODE ",'CODES')

SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+"CPRO_CODE "
  IF ALLTRIM(cdiscrep) = "Case Pack"
    lcCasPkcod = codes.ccode_no
  ENDIF

  IF ALLTRIM(cdiscrep) = "Box Type"
    lcBoxCode= codes.ccode_no
  ENDIF
  
ENDSCAN

IF SEEK("SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6),"PROFVALU") 
  SELECT PROFVALU
  SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6)
    IF PROFVALU.cpro_code = lcBoxCode AND ALLTRIM(PROFVALU.cpro_value) <> "Bulk Pack"
      lcBoxProf = 'Y'
    ENDIF
  ENDSCAN 

  =SEEK("SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6),"PROFVALU") 
  SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+&lcTempOrd..ORDER+STR(&lcTempOrd..lineno,6)
    IF PROFVALU.cpro_code = ALLTRIM(lcCasPkcod)
      SELECT (lnAlias)
      RETURN VAL(SUBSTR(ALLTRIM(PROFVALU.cpro_value),1,AT(SPACE(1) ,ALLTRIM(PROFVALU.cpro_value))-1))
    ENDIF
  ENDSCAN 
  SELECT (lnAlias)
  RETURN 0  
ELSE
  SELECT (lnAlias)
  RETURN 0  
ENDIF

*!*************************************************************
*! Name      : lfUpdate
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/01/2007
*! Purpose   : Function to Update flag and also to check for colors and Pix.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*: C200990,1 MMH 05/08/2008 add color to the report and also some piks [Start]
FUNCTION lfUpdate

*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start]       
*IF SYS(2040)='2'
IF SYS(2040)='2' AND !llGetPage 
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       

  REPLACE ORDHDR.Printed WITH .T.
ENDIF
RETURN .T.


*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [Start] 
*!*************************************************************
*! Name      : lfpCount
*! Developer : MAriam Mazhar (MMT)
*! Date      : 09/04/2008
*! Purpose   : Function to Calculate No. of Pages Per Order
*!*************************************************************      
FUNCTION lfpCount

lcAlis = SELECT(0)
SELECT (lcTempOrd)
lnRecNum = RECNO()
IF (lcLastOrd <> OrdHdr.Order) 
  IF !EMPTY(lcLastOrd)
    REPLACE ALL  nPageNo WITH lnPcnt  FOR ORDER = lcLastOrd  IN (lcTempOrd)
  ENDIF 
  IF BETWEEN(lnRecNum ,1,RECCOUNT())
    GO RECORD lnRecNum IN (lcTempOrd)
  ENDIF
  lcLastOrd = OrdHdr.Order
  lnPcnt    = 1                
ELSE
  lnPcnt    = lnPcnt +  1                
ENDIF
SELECT(lcAlis)
*: B608677,1 MMT 09/04/2008 Fix bug of Wrong total Order Number of pages [End]       