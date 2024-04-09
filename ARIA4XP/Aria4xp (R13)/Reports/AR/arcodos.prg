*:***************************************************************************
*: Program file  : ARCODOS
*: Program desc. : COD outStanding report
*: For Report    : (N000576)
*: System        : Aria Advantage Series.4XP
*: Module        : AR
*: Developer     : Mariam Mazhar[MMT]
*:***************************************************************************
*
#INCLUDE R:\Aria4xp\reports\ar\arcodos.H
IF loOgScroll.llOGFltCh && OG Filters changed
  lfCreatTemp()
  lfCollect()
ELSE
  USE oAriaApplication.WorkDir +  lcWorkTEMP + ".DBF" IN 0
ENDIF

IF RECCOUNT(lcWorkTemp) = 0
  = gfModalGen('TRM00052B00000','DIALOG' )
  IF USED(lcWorkTEMP)
    SELECT (lcWorkTEMP)
    USE
  ENDIF
  RETURN
ENDIF

DIMENSION loOgScroll.laCRParams[1,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = Lang_C_O_D_OUTSTANDING
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_C_O_D_OUTSTANDING,oAriaApplication.GetHeaderText("Lang_C_O_D_OUTSTANDING",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkTEMP + ".DBF"
SELECT(lcWorkTEMP)
USE
gfDispRe()
RETURN

*!*************************************************************
*! Name      : lfwogwhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/15/2007
*! Purpose   : OG When Function
*!*************************************************************
FUNCTION lfwogwhen
  IF llMultCurr AND !USED('SYCCURR')
    llOpenCurr = gfOpenTable(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ENDIF
  =gfOpenTable('DEBIT','DEBIT','SH')
  =gfOpenTable('INVHDR','INVHDR','SH')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH')
*!*************************************************************
*! Name      : lfInvSet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/15/2007
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfInvSet
PARAMETERS lcParm
*-- Set Code
IF lcParm = 'S'
  SELECT INVHDR
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
ELSE  && Reset code
  SELECT INVHDR
  SET RELATION OFF INTO CUSTOMER
ENDIF
*-- end of lfInvSet.
*:**************************************************************************
*:* Name        : lfCreatTemp
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 01/15/2007
*:* Purpose     : Function to adjust date
*:***************************************************************************
FUNCTION lfCreatTemp
DIMENSION laTempacstru[1,18]
SELECT Debit
lnTableLen =AFIELDS(laTempacstru)
DIMENSION laTempacstru[lnTableLen+7 ,18]

laTempacstru[lnTableLen+1 ,1] = 'cTermcode'
laTempacstru[lnTableLen+1 ,2] = 'C'
laTempacstru[lnTableLen+1 ,3] = 15
laTempacstru[lnTableLen+1 ,4] = ''

laTempacstru[lnTableLen+2 ,1] = 'BTNAME'
laTempacstru[lnTableLen+2 ,2] = 'C'
laTempacstru[lnTableLen+2 ,3] = 30
laTempacstru[lnTableLen+2 ,4] = ''

laTempacstru[lnTableLen+3 ,1] = 'INVDATE'
laTempacstru[lnTableLen+3 ,2] = 'D'
laTempacstru[lnTableLen+3 ,3] = 8
laTempacstru[lnTableLen+3 ,4] = ''

laTempacstru[lnTableLen+4 ,1] = 'ORDER'
laTempacstru[lnTableLen+4 ,2] = 'C'
laTempacstru[lnTableLen+4 ,3] = 6
laTempacstru[lnTableLen+4 ,4] = ''

laTempacstru[lnTableLen+5 ,1] = 'CODTAG'
laTempacstru[lnTableLen+5 ,2] = 'C'
laTempacstru[lnTableLen+5 ,3] = 6
laTempacstru[lnTableLen+5 ,4] = ''

laTempacstru[lnTableLen+6 ,1] = 'PIKTKT'
laTempacstru[lnTableLen+6 ,2] = 'C'
laTempacstru[lnTableLen+6 ,3] = 6
laTempacstru[lnTableLen+6 ,4] = ''

laTempacstru[lnTableLen+7 ,1] = 'COD_AMT'
laTempacstru[lnTableLen+7 ,2] = 'N'
laTempacstru[lnTableLen+7 ,3] = 13
laTempacstru[lnTableLen+7 ,4] = 2


FOR nLoop = 1 TO 7
  STORE ' ' TO  laTempacstru[lnTableLen+nLoop ,7],laTempacstru[lnTableLen+nLoop  ,8],;
            laTempacstru[lnTableLen+nLoop ,9],laTempacstru[lnTableLen+nLoop ,10],;
            laTempacstru[lnTableLen+nLoop ,11],laTempacstru[lnTableLen+nLoop ,12],;
            laTempacstru[lnTableLen+nLoop ,13],laTempacstru[lnTableLen+nLoop ,14],;
            laTempacstru[lnTableLen+nLoop ,15],laTempacstru[lnTableLen+nLoop ,16]
  STORE 0 TO    laTempacstru[lnTableLen+nLoop ,17] ,laTempacstru[lnTableLen+nLoop ,18]
ENDFOR


= gfCrtTmp(lcWorkTemp,@laTempacstru,'account+tran+cinstalno+DTOS(trandate)',lcWorkTemp,.F.)


*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 01/15/2007
*:* Purpose     : Function to Collect data
*:***************************************************************************
FUNCTION lfCollect

*Account
lcAccFile  = ''
llUseAcc   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'DEBIT.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  llUseAcc    = IIF(!EMPTY(lcAccFile) .AND. USED(lcAccFile) .AND. RECCOUNT(lcAccFile)>0,.T.,.F.)
ENDIF
IF llUseAcc
  SELECT(lcAccFile)
  LOCATE
  IF EOF()
    llUseAcc = .F.
  ENDIF
ENDIF

*Invoice
lcInvFile  = ''
llUseInv   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.INVOICE'),1)
IF lnPosition > 0
  lcInvFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseInv    = IIF(!EMPTY(lcInvFile) .AND. USED(lcInvFile) .AND. RECCOUNT(lcInvFile)>0,.T.,.F.)
ENDIF
IF llUseInv
  SELECT(lcInvFile)
  LOCATE
  IF EOF()
    llUseInv  = .F.
  ENDIF
ENDIF

*Currency
IF llMultCurr
  lcCurFile  = ''
  llUseCur   = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'DEBIT.CCURRCODE'),1)
  IF lnPosition > 0
    lcCurFile= LOOGSCROLL.laOGFxFlt[lnPosition,6]
    llUseCur= IIF(!EMPTY(lcCurFile) .AND. USED(lcCurFile) .AND. RECCOUNT(lcCurFile)>0,.T.,.F.)
  ENDIF
  IF llUseCur
    SELECT(lcCurFile)
    LOCATE
    IF EOF()
      llUseCur = .F.
    ENDIF
  ENDIF
ENDIF

SELECT Debit
gfSetorder('Debit')

IF llUseAcc
  SELECT (lcAccFile)
  LOCATE
  SCAN
    IF gfSeek(&lcAccFile..Account,'Debit')
      SELECT Debit
      SCAN REST WHILE ACCOUNT+TRAN+CINSTALNO+DTOS(TRANDATE) = &lcAccFile..Account FOR ;
        IIF(llMultCurr AND llUseCur,SEEK(DEBIT.CCURRCODE,lcCurFile),.T.) AND ;
        gfSeek(Debit.tran,'INVHDR','INVHDR') AND IIF(llUseInv,SEEK(Invhdr.Invoice,lcInvFile),.T.) AND ;
        (INVHDR.COD_FLAG = 'Y' OR !EMPTY(INVHDR.CODTAG))
        SCATTER MEMO MEMVAR
        m.cTermcode = SUBSTR(gfCodDes(InvHdr.cTermcode,'cTermcode'),1,15)
		m.BTNAME    = IIF(gfSeek('M'+Debit.account,'Customer','Customer'),Customer.BTNAME,'')
		m.INVDATE	=INVHDR.INVDATE
		m.ORDER		=INVHDR.ORDER
		m.CODTAG	=INVHDR.CODTAG
		m.PIKTKT	=INVHDR.PIKTKT
		m.COD_AMT	=INVHDR.COD_AMT

        INSERT INTO (lcWorkTemp) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  gfSeek('')
  SCAN FOR ;
    IIF(llMultCurr AND llUseCur,SEEK(DEBIT.CCURRCODE,lcCurFile),.T.) AND ;
    gfSeek(Debit.tran,'INVHDR','INVHDR') AND IIF(llUseInv,SEEK(Invhdr.Invoice,lcInvFile),.T.) AND ;
    (INVHDR.COD_FLAG = 'Y' OR !EMPTY(INVHDR.CODTAG))
    SCATTER MEMO MEMVAR
    m.cTermcode = SUBSTR(gfCodDes(InvHdr.cTermcode,'cTermcode'),1,15)
	m.BTNAME    = IIF(gfSeek('M'+Debit.account,'Customer','Customer'),Customer.BTNAME,'')
	m.INVDATE	=INVHDR.INVDATE
	m.ORDER		=INVHDR.ORDER
	m.CODTAG	=INVHDR.CODTAG
	m.PIKTKT	=INVHDR.PIKTKT
	m.COD_AMT	=INVHDR.COD_AMT

    INSERT INTO (lcWorkTemp) FROM MEMVAR
  ENDSCAN
ENDIF
*
