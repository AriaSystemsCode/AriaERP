*:***************************************************************************
*: Program file  : ARCDEP
*: Program desc. : Print Customer Departments
*: For Report    : (N000573)
*: System        : Aria Advantage Series.4XP
*: Module        : AR
*: Developer     : Mariam Mazhar[MMT]
*:***************************************************************************
*N000682,1 MMT 02/05/2013 Globalization changes[Start]
#INCLUDE R:\aria4xp\reports\arcdep.h
*N000682,1 MMT 02/05/2013 Globalization changes[End]
IF looGscroll.llOGFltCh
  lfCrtTempFile()
  lfCollect()
ENDIF

DIMENSION loOGScroll.laCRParams[1,2]
*--Report Name
loOGScroll.laCRParams[1,1] = 'ReportName'

* N000682 ,1 Thabet Handle globalization issues [Start]
*loOGScroll.laCRParams[1,2] = 'Departments Report'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOGScroll.laCRParams[1,2] = LANG_DEPARTMENTS_REPORT
loOGScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEPARTMENTS_REPORT,oAriaApplication.GetHeaderText("LANG_DEPARTMENTS_REPORT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]

SELECT(lcReportFile)
IF RECCOUNT()=0   &&if the file is empty
*--no records to display
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF  &&endif the file is not empty


COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF'
DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF'

loogScroll.cCROrientation = 'P'

= gfDispRe()
*!**************************************************************************
*! Name      : lfwGrid
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/04/2007
*! Purpose   : FUNCTION TO ENABLE/DISABLE PREVIEW & RUN PUSH BUTTON
*!**************************************************************************
*! Called from : [Option Grid When function]
*!**************************************************************************
*! Example     : =lfwGrid()
*!**************************************************************************
*
FUNCTION lfwGrid
*!*	lnDeptPos = ASCAN(laOGFxFlt,'CUSTDEPT.DEPT')
*!*	IF lnDeptPos > 0
*!*	  = lfDeptEnab()
*!*	ENDIF
= gfOpenTable(oAriaApplication.DataDir + 'Customer' ,'Customer','SH')
= gfOpenTable(oAriaApplication.DataDir + 'CUSTDEPT' ,'CUSTDEPT','SH')

SELECT CUSTDEPT
DIMENSION laFileStruct[1,18]
AFIELDS(laFileStruc)
=gfCrtTmp(lcTmpBrow,@laFileStruc,"ACCOUNT+DEPT",lcTmpBrow,.T.)


SELECT CUSTDEPT
IF gfseek('')
  SELECT CUSTDEPT
  SCAN
    SCATTER MEMO MEMVAR
    INSERT INTO (lcTmpBrow) FROM  MEMVAR
  ENDSCAN
ENDIF
*-- End of lfwGrid.

*!**************************************************************************
*! Name      : lfDeptEnab
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/04/2007
*! Purpose   : To Disable or Enable the department
*!**************************************************************************
*! Example   : = lfDeptEnab()
*!**************************************************************************
*!
FUNCTION lfDeptEnab
lnDeptPos = ASUBSCRIPT(laOGVrFlt,lnDeptPos,1)
laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnDeptPos] = llDeptStat
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDeptPos)) + ',6]')
*-- End of lfDeptEnab.

*!**************************************************************************
*! Name      : lfSRACCT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/04/2007
*! Purpose   : called from the Reset of the account browse
*!**************************************************************************
FUNCTION lfSRACCT
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'R'  && Set code

   SELECT (lcTmpBrow)
   ZAP
   lnAccount = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
   lcFileSele = IIF(!EMPTY(laOGFxFlt[lnAccount,6]) and USED(laOGFxFlt[lnAccount,6]) and RECCOUNT(laOGFxFlt[lnAccount,6]) > 0 ,laOGFxFlt[lnAccount,6],"")

   IF !EMPTY(lcFileSele)
     SELECT(lcFileSele)
     LOCATE
     IF !EOF()
	     SCAN
	      SELECT CUSTDEPT
		  IF gfseek(&lcFileSele..ACCOUNT)
  	        SELECT CUSTDEPT
		    SCAN REST WHILE ACCOUNT+DEPT = &lcFileSele..ACCOUNT
		      SCATTER MEMO MEMVAR
		      INSERT INTO (lcTmpBrow) FROM  MEMVAR
		    ENDSCAN
	       ENDIF
	     ENDSCAN
	     IF ASCAN(laOGFxFlt,'CUSTDEPT.DEPT') # 0
	  	 *!*	  *-- Get the position of the vendor in the varaible filter
		 lnAccountDep = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'CUSTDEPT.DEPT'),1)
		  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnAccountDep + 1] = .T.
		  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnAccountDep)) + ',6]')
		 ENDIF
	  ELSE
	    SELECT CUSTDEPT
		IF gfseek('')
		  SELECT CUSTDEPT
		  SCAN
		    SCATTER MEMO MEMVAR
		    INSERT INTO (lcTmpBrow) FROM  MEMVAR
		  ENDSCAN
		ENDIF

*!*			IF ASCAN(laOGFxFlt,'CUSTDEPT.DEPT') # 0
*!*				*!*	  *-- Get the position of the vendor in the varaible filter
*!*	  			  laOGFxFlt[lnVendorPo,6] = ''
*!*				  lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'CUSTDEPT.DEPT'),1)
*!*				  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .F.
*!*				  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
*!*		    ENDIF
*!*		    lnVendorPo = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt, ASCAN(LOOGSCROLL.laOGFxFlt,'CUSTDEPT.DEPT'),1)
*!*			  lcSeleItem = LOOGSCROLL.laOGFxFlt[lnVendorPo,6]
*!*			  IF !EMPTY(lcSeleItem) AND USED(lcSeleItem) AND RECCOUNT(lcSeleItem)>0
*!*			    SELECT (lcSeleItem)
*!*			    ZAP
*!*			  ENDIF
	  ENDIF
   ELSE
     IF EMPTY(lcFileSele)OR (!EMPTY(lcFileSele) AND (!USED(lcFileSele) OR RECCOUNT(lcFileSele)= 0))
       SELEct(lcTmpBrow)
       ZAP
*!*	       IF ASCAN(LOOGSCROLL.laOGFxFlt,'CUSTDEPT.DEPT') # 0
*!*	  		  *-- Get the position of the vendor in the varaible filter
*!*			  lnVendorPo = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt, ASCAN(LOOGSCROLL.laOGFxFlt,'CUSTDEPT.DEPT'),1)
*!*			  lcSeleItem = LOOGSCROLL.laOGFxFlt[lnVendorPo,6]
*!*			  IF !EMPTY(lcSeleItem) AND USED(lcSeleItem) AND RECCOUNT(lcSeleItem)>0
*!*			    SELECT (lcSeleItem)
*!*			    ZAP
*!*			  ENDIF
*!*				IF ASCAN(laOGFxFlt,'CUSTDEPT.DEPT') # 0
*!*				*!*	  *-- Get the position of the vendor in the varaible filter
*!*	  			  laOGFxFlt[lnVendorPo,6] = ''
*!*				  lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'CUSTDEPT.DEPT'),1)
*!*				  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .F.
*!*				  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
*!*				ENDIF
*!*			ENDIF
		SELECT CUSTDEPT
		IF gfseek('')
		  SELECT CUSTDEPT
		  SCAN
		    SCATTER MEMO MEMVAR
		    INSERT INTO (lcTmpBrow) FROM  MEMVAR
		  ENDSCAN
		ENDIF
     ENDIF
  ENDIF
ENDCASE


*!*************************************************************
*! Name      : lfCrtTempFile
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/04/2007
*! Purpose   : create report temp file
*!*************************************************************
FUNCTION lfCrtTempFile

IF USED(lcReportFile)
  SELECT(lcReportFile)
  ZAP
ENDIF

SELECT CUSTDEPT
DIMENSION laFileStruct[1,18]
lnField = AFIELDS(laFileStruc)
DIMENSION laFileStruc[lnField +1,18]

laFileStruc[lnField +1,1] = 'cTermDesc'
laFileStruc[lnField +1,2] = 'C'
laFileStruc[lnField +1,3] = 30
laFileStruc[lnField +1,4] = ''

STORE ' ' TO laFileStruc[lnField +1,7],laFileStruc[lnField +1,8],;
             laFileStruc[lnField +1,9],laFileStruc[lnField +1,10],;
             laFileStruc[lnField +1,11],laFileStruc[lnField +1,12],;
             laFileStruc[lnField +1,13],laFileStruc[lnField +1,14],;
             laFileStruc[lnField +1,15],laFileStruc[lnField +1,16]
STORE 0 TO   laFileStruc[lnField +1,17] ,laFileStruc[lnField +1,18]

=gfCrtTmp(lcReportFile,@laFileStruc,"ACCOUNT+DEPT",lcReportFile,.T.)



*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/04/2007
*! Purpose   : collect report data
*!*************************************************************
FUNCTION lfCollect

llSelectedAcc = .F.
lcSelectedAcc  = ''
lnPos = ASCAN(loOGScroll.laogFxflt,'CUSTOMER.ACCOUNT')
IF lnPos <> 0
  lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
  lcSelectedAcc = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
  IF !EMPTY(lcSelectedAcc) AND USED(lcSelectedAcc)
    SELECT(lcSelectedAcc)
    LOCATE
    IF !EOF()
      llSelectedAcc = .T.
    ENDIF
  ENDIF
ENDIF


llSelectedDep = .F.
lcSelectedDep  = ''
lnPos = ASCAN(loOGScroll.laogFxflt,'CUSTDEPT.DEPT')
IF lnPos <> 0
  lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
  lcSelectedDep = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
  IF !EMPTY(lcSelectedDep) AND USED(lcSelectedDep)
    SELECT(lcSelectedDep)
    LOCATE
    IF !EOF()
      llSelectedDep = .T.
    ENDIF
  ENDIF
ENDIF

IF llSelectedAcc
  SELECT(lcSelectedAcc)
  SCAN
    SELECT CUSTDEPT
    IF gfSeek(&lcSelectedAcc..Account)
      SCAN REST WHILE ACCOUNT+DEPT = &lcSelectedAcc..Account  FOR IIF(llSelectedDep ,SEEK(DEPT ,lcSelectedDep),.T.)
        SCATTER MEMO MEMVAR
        INSERT INTO (lcReportFile) FROM MEMVAR
        REPLACE cTermDesc WITH ALLTRIM(gfCodDes(m.CTERMCODE,'CTERMCODE'))IN (lcReportFile)
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  SELECT CUSTDEPT
  gfSeek('')
  SCAN FOR IIF(llSelectedDep ,SEEK(DEPT,lcSelectedDep),.T.)
    SCATTER MEMO MEMVAR
    INSERT INTO (lcReportFile) FROM MEMVAR
    REPLACE cTermDesc WITH ALLTRIM(gfCodDes(m.CTERMCODE,'CTERMCODE')) IN (lcReportFile)
  ENDSCAN
ENDIF
