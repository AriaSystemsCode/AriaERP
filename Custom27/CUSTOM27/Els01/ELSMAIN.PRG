*!**************************************************************************
*! Name      : ELSMAIN.PRG             C200162
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 27/02/2001
*! Purpose   : Main program For SHAMSI.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! Modifications
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : AddOptn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Add a new Option to the the menu Bar
*!**************************************************************************
*! Example   : lfAddOptn()
*!**************************************************************************
*!
FUNCTION lfADDOPTN

IF !llImpCost 
  DEFINE BAR 11 OF _OPTIONPOP PROMPT 'Distribution Process'  SKIP FOR EMPTY(laScrMode[4])
  ON SELECTION BAR 11 OF _OPTIONPOP DO lpDistSty IN ELSMAIN
ELSE
  DEFINE BAR 10 OF _OPTIONPOP PROMPT 'Distribution Process'  SKIP FOR EMPTY(laScrMode[4])
  ON SELECTION BAR 10 OF _OPTIONPOP DO lpDistSty IN ELSMAIN
ENDIF  
*-End 
*!**************************************************************************
*! Name      : lpDistSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Add a new Option to the the menu Bar
*!**************************************************************************
*! Example   : DO lfAddOptn()
*!**************************************************************************
*!
PROCEDURE lpDistSty
PRIVATE lcTargLoct
STORE '' TO lcTargLoct

IF EMPTY(laTarget)
  IF FILE(gcDataDir+'Targets.MEM')
    RESTORE FROM (gcDataDir+'Targets.MEM') ADDI
    =gfSubStr(lcTargLoct,@laTarget,'|')
  ENDIF
ENDIF
IF EMPTY(lcSrcLct)
  =lfShowScr()
ENDIF

*-- function to get balance Qty
=lfGtBalQty()

lcEscInt = ON('KEY','ESC')
lcHotKey = ON('KEY','CTRL+A')
ON KEY LABEL ESC DO lfVLcCan
ON KEY LABEL Ctrl+A
DO (gcScrDir+"PO\PODISTLC.SPX")
ON KEY LABEL ESC &lcEscInt
ON KEY LABEL CTRL+A &lcHotKey

IF !EMPTY(laTarget)
  lcTargLoct = ''
  FOR lnTrgCnt = 1 TO ALEN(laTarget,1)
    lcTrgCnt   = STR(lnTrgCnt,1)
    lcTargLoct = lcTargLoct + laTarget[lnTrgCnt] + '|'
  ENDFOR
  SAVE ALL LIKE lcTargLoct TO (gcDataDir+'Targets.MEM')
ENDIF

*!**************************************************************************
*! Name      : lfvSrcWhrL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Valid the source location
*!**************************************************************************
*! Example   : lfAddOptn()
*!**************************************************************************
*!
FUNCTION lfvSrcWhrL

IF llBrowse OR !EMPTY(lcSrcLct)
  IF llBrowse OR !SEEK( PADR(lcSrcLct,6),'WAREHOUS' )
    lcSrcLct = gfBrowWare( .F. )
  ENDIF
  IF !EMPTY(lcSrcLct)
    lcOldSt = lcSrcLct
    =lfSHOWSCR()
    lcSrcLct = lcOldSt
    SHOW GETS WINDOW ('PODISTLC') ENABLE
    SELECT DISTINCT cWareCode;
           FROM WareHous;
           WHERE cWareCode <> lcSrcLct;
           INTO ARRAY laSource
  ENDIF  

  llBrowse = .F.
ENDIF 

SELECT POSHDR
IF EMPTY(lcSrcLct) 
  RETURN
ENDIF

laVAddrs[1] = WareHous.cDesc
laVAddrs[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laVAddrs[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laVAddrs[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laVAddrs[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laVAddrs[6] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)

*--Currency also releted to vendor but we get it only if no lines.
IF lfCkEOF(lcPoLine)
  STORE gcBaseCurr TO laData[26],laData[27]
  STORE 1 TO laData[28],laData[29]
ENDIF

*--Show new vendor information.
SHOW GET laVAddrs[1]
SHOW GET laVAddrs[2]
SHOW GET laVAddrs[3]
SHOW GET laVAddrs[4]
SHOW GET laVAddrs[5]
SHOW GET laVAddrs[6]
SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET laData[29]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
RETURN
*!**************************************************************************
*! Name      : lfvTarget
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : mover for source and target warehouse
*!**************************************************************************
*! Example   : =lfvTarget()
*!**************************************************************************
*!
FUNCTION lfvTarget
= gfMover(@laSource,@laTarget,'Select target locations',.T.,'')

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Validate the Style
*!**************************************************************************
*! Example   : =lfvStyle()
*!**************************************************************************
*!
FUNCTION lfvStyle
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
lcStyLoc =IIF(llBrowse,'?',lcStyLoc )
SELECT STYLE
SET ORDER TO TAG StyQlty
lcQltyKey  = laStyGrd[lnGrade,2]
lnItemSz   = gfItemMask('PI')
IF llBrowse OR SUBSTR(lcStyLoc ,1,1) <> SPACE(1)
  llbrowse = .F.
  IF  !SEEK(lcQltyKey+ALLTRIM(lcStyLoc ),'STYLE')
    SET FILTER TO cStyGrade=lcQltyKey
    GO TOP
    IF EOF()
      *--No styles found for this quality. 
      =gfModalGen('TRM42087B42000','DIALOG')
      lcStyLoc  = SPACE(19)
    ELSE
      lcHdEsc1= ON('KEY','ESC')
      ON KEY LABEL ESC 
      lcStyLoc  = gfStyBrw('I',lcStyLoc,'',.F.)
      ON KEY LABEL ESC &lcHdEsc1
    ENDIF 
    SELECT STYLE
    SET FILTER TO 
  ELSE
    IF lcStyLoc # STYLE.STYLE .AND. gfGetMemVar('M_USEEXSSC') .AND. ;
           LEN(ALLTRIM(lcStyLoc )) = LEN(lnItemSz)-3
      lnBrR    = 0
      DIMENSION laQtyArr[17,3]
      laQtyArr[1,1]   = 'QTY1'
      laQtyArr[1,2]   = 6
      laQtyArr[1,3]   = 0
      laQtyArr[2,1]   = 'QTY2'
      laQtyArr[2,2]   = 6
      laQtyArr[2,3]   = 0
      laQtyArr[3,1]   = 'QTY3'
      laQtyArr[3,2]   = 6
      laQtyArr[3,3]   = 0
      laQtyArr[4,1]   = 'QTY4'
      laQtyArr[4,2]   = 6
      laQtyArr[4,3]   = 0
      laQtyArr[5,1]   = 'QTY5'
      laQtyArr[5,2]   = 6
      laQtyArr[5,3]   = 0
      laQtyArr[6,1]   = 'QTY6'
      laQtyArr[6,2]   = 6
      laQtyArr[6,3]   = 0
      laQtyArr[7,1]    = 'QTY7'
      laQtyArr[7,2]   = 6
      laQtyArr[7,3]   = 0
      laQtyArr[8,1]   = 'QTY8'
      laQtyArr[8,2]   = 6
      laQtyArr[8,3]   = 0
      laQtyArr[9,1]   = 'TOTQTY'
      laQtyArr[9,2]   = 7
      laQtyArr[9,3]   = 0
      laQtyArr[10,1]  = 'GROS_PRICE'
      laQtyArr[10,2]  = 8
      laQtyArr[10,3]  = 2
      laQtyArr[11,1]  = 'DISC_PCNT'
      laQtyArr[11,2]  = 4
      laQtyArr[11,3]  = 2
      laQtyArr[12,1]  = 'NCOST1'
      laQtyArr[12,2]  = 9
      laQtyArr[12,3]  = 2
      laQtyArr[13,1]  = 'PPQTY'
      laQtyArr[13,2]  = 4
      laQtyArr[13,3]  = 0
      laQtyArr[14,1]  = 'NCOST2'
      laQtyArr[14,2]  = 9
      laQtyArr[14,3]  = 2
      laQtyArr[15,1]  = 'NCOST3'
      laQtyArr[15,2]  = 9
      laQtyArr[15,3]  = 2
      laQtyArr[16,1]  = 'NCOST4'
      laQtyArr[16,2]  = 9
      laQtyArr[16,3]  = 2
      laQtyArr[17,1]  = 'NCOST5'
      laQtyArr[17,2]  = 9
      laQtyArr[17,3]  = 2

      DIMENSION laTitArr[16,2]
      laTitArr[1,1] = 'STYLE'
      laTitArr[1,2] = lcStyHdr
      laTitArr[2,1] = 'REFERENCE'
      laTitArr[2,2] = 'Refer'
      laTitArr[3,1] = 'CVENSTY'
      laTitArr[3,2] = 'Ven. Sty.'
      laTitArr[4,1] = 'CPrepak'
      laTitArr[4,2] = 'P'
      laTitArr[5,1] = 'CPPQTY'
      laTitArr[5,2] = 'P. Qty.'
      laTitArr[6,1] = 'CGROS_PRICE'
      laTitArr[6,2] = 'GPrice'
      laTitArr[7,1] = 'CDISC_PCNT'
      laTitArr[7,2] = 'Disc'
      laTitArr[8,1] = 'CNCOST1'
      laTitArr[8,2] = 'Price'
      laTitArr[9,1] = 'cVenSty'
      laTitArr[9,2] = 'Vendor Style'
      laTitArr[10,1] = 'Reference'
      laTitArr[10,2] = 'Reference'
      laTitArr[11,1] = 'CWarCust'
      laTitArr[11,2] = 'S'
      laTitArr[12,1] = 'cWareCode'
      laTitArr[12,2] = 'ShipTo'
      laTitArr[13,1] = 'CNCOST2'
      laTitArr[13,2] = lcISlbl2
      laTitArr[14,1] = 'CNCOST3'
      laTitArr[14,2] = lcISlbl3
      laTitArr[15,1] = 'CNCOST4'
      laTitArr[15,2] = lcISlbl4
      laTitArr[16,1] = 'CNCOST5'
      laTitArr[16,2] = lcISlbl5

      LNP='999999'
      lcg='9999999.99'
      lcg1='999999.99'

      lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Style:H='STYLE':R,"+;
        "Reference:H='Reference',cVenSty:H='Vendor Sty.',"+;
        IIF(llDyelot .AND. STYLE.cDye_Flg = 'Y',"Dyelot :H='Dyelot':P='@!':W=lfwOV():V=lfvDyelot(.T.),",'')+;
        IIF(llMultiWare,"cWarCust :H='SHipTo L/C':P='@!':V=lfvPOPUP(), "+;
        "cWareCode:H='Ship To':W=lfwOV():V=lfvF(),Store:H='Store':W=lfwOV():V=lfvF(),",'')+;
        "Prepak:H='P':W=lfwOV():V=lfvF():P='!',cPpQty:H='P. Qty.':W=lfwOV():V=lfvF():P='9999',cTotQty:H='Pieces':W=lfwOV():P='9999999':V=lfvF()," 

      lcBrowStr = lcBrowStr +;
        "cQty1:H='Qty1':V=lfvF():P=LNP,cQty2:H='Qty2':W=laScl[2]:V=lfvF():P=LNP,cQty3:H='Qty3':W=laScl[3]:V=lfvF():P=LNP,"+;
        "cQty4:H='Qty4':W=laScl[4]:V=lfvF():P=LNP,"
      lcBrowStr = lcBrowStr +;
        "cQty5:H='Qty5':W=laScl[5]:V=lfvF():P=LNP,cQty6:H='Qty6':W=laScl[6]:V=lfvF():P=LNP,"+;
        "cQty7:H='Qty7':W=laScl[7]:V=lfvF():P=LNP,cQty8:H='Qty8':W=laScl[8]:V=lfvF():P=LNP," +;
        "cGros_Price:H='GPrice':W=lfwOV():V=lfvF():P=lcg1,"  +;
        "CDISC_PCNT:H='Disc':W=lfwOV():V=lfvF():P='99.99',cNCost1:H='Price':W=lfwOV():V=lfvF():P=lcg" +;
        ",cNCost2:H=lcISlbl2:W=lfwOV():V=lfvF():P=lcg,cNCost3:H=lcISlbl3:W=lfwOV():V=lfvF():P=lcg,cNCost4:H=lcISlbl4:W=lfwOV():V=lfvF():P=lcg,cNCost5:H=lcISlbl5:W=lfwOV():V=lfvF():P=lcg"
  
      DO (gcAppHome+'SOESSQTY.FXP') WITH ALLTRIM(lcStyLoc ),m.store,;
         lcPOLine, lcBrowStr,'',laQtyArr,laTitArr,'PO'

      RETURN
    ELSE
      SET FILTER TO cStyGrade=lcQltyKey
      GO TOP
      IF EOF()
        *--No styles found for this quality. 
        =gfModalGen('TRM42087B42000','DIALOG')
        lcStyLoc = SPACE(19)
      ELSE
        lcHdEsc1= ON('KEY','ESC')
        ON KEY LABEL ESC 
        IF lcAType = 'D' .AND. lcStyLoc # STYLE.STYLE .AND. ;
             LEN(ALLTRIM(lcStyLoc)) = LEN(lnItemSz) - LEN(laMajSeg[lnColStru,3]) 
          IF laData[72]#'*' .AND. SEEK(lcQltyKey+ALLTRIM(lcStyLoc)+laData[72],'STYLE')
            lcStyLoc = ALLTRIM(lcStyLoc)+laData[72]
          ELSE
            lcNonMaj = '?'
            lcNonMaj = gfStyBrw('N', LEFT(ALLTRIM(lcStyLoc),lnMjrWid),lcNonMaj,.F.)
            IF !EMPTY(lcNonMaj)
              lcStyLoc = ALLTRIM(lcStyLoc)+lcNonMaj
            ELSE
              lcStyLoc = SPACE(19)
            ENDIF
          ENDIF
        ELSE
          lcStyLoc = gfStyBrw('I',lcStyLoc,'',.F.)
        ENDIF
        
        ON KEY LABEL ESC &lcHdEsc1
      ENDIF 
      SELECT STYLE
      SET FILTER TO 
    ENDIF
  ENDIF
ENDIF
SET ORDER TO TAG Style
lcStyDesc = IIF(!EMPTY(lcStyLoc) AND !EOF('STYLE'),STYLE.Desc1,'')
SHOW GET lcStyDesc

SELECT POSHDR
IF SUBSTR(lcStyLoc,1,1) = SPACE(1)
  RETURN
ENDIF

llAbort =.F.
*-- Validation checks loop for Style.....
DO WHILE .T.
  IF !STYLE.Make
    STORE 0 TO m.nCost1,m.nCost2,m.nCost3,m.nCost4,m.nCost5,m.Gros_Price    
    
  ELSE 
    *-Conflict ! only purchased styles are allowed to enter here, Cannot proceed!
    =gfModalGen('TRM34039B34000','DIALOG')
    llAbort=.T.
    EXIT
  ENDIF 
  
  IF Style.cDivision <> ALLTRIM(laData[4])
    *-Conflict ! styles restricted to division XXXX, Cannot proceed!
    =gfModalGen('TRM34041B34000','DIALOG',ALLTRIM(laDiv[lnDiv,1]) )        
    llAbort=.T.
    EXIT
  ENDIF

  IF !EMPTY(Style.cPurCode) AND Style.cPurCode <> ALLTRIM(laData[67])
    *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
    =gfModalGen('TRM34110B34000','DIALOG',ALLTRIM(laPurCode[lnPurCode,1]) )
    llAbort=.T.
    EXIT
  ENDIF

  IF STYLE.Status='X'
    *-This is a canceled style. Not allowed to enter here, Cannot proceed!
    =gfModalGen('TRM34040B34000','DIALOG')        
    llAbort=.T.
    EXIT
  ENDIF

  IF !SEEK(PADR(lcStyLoc,19)+PADR(lcSrcLct,6)+SPACE(10),'STYDYE')
    IF llPosSetN
      *-Style: xxx is not assigned to location: xxx. "ok"
      =gfModalGen('TRM34048B34000','DIALOG',ALLTRIM(lcStyLoc)+'|'+ALLTRIM(lcSrcLct))
      llAbort=.T.
      EXIT
    ELSE    
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcStyLoc)+'|'+ALLTRIM(lcSrcLct)) = 1
        DO gpAdStyWar WITH lcStyLoc,SPACE(10),lcSrcLct
        FOR lnCnt = 1 TO 8
          lcI=STR(lnCnt,1)
          *SHOW GET LAPLANQTY[&lcI] ENABLE 
          SHOW GET LAPLANQTY[lnCnt] ENABLE 
        ENDFOR
      ELSE
        FOR lnCntr = 1 TO 8
          *lcI=STR(I,1)
          *SHOW GET LAPLANQTY[&lcI] DISABLE 
          SHOW GET LAPLANQTY[lnCntr] DISABLE 
        ENDFOR
        _CUROBJ = OBJNUM(ibLStyle)
        llAbort=.T.
        EXIT
      ENDIF
    ENDIF
  ENDIF
  m.cWareCode = SPACE(6)
  EXIT
ENDDO

IF llAbort
  lcStyLoc   = SPACE(lnStyleWid)
  lcStyDesc = SPACE(0)
  FOR lnCnter = 1 TO 8
    *lcI=STR(I,1)
    *SHOW GET LAPLANQTY[&lcI] DISABLE 
    SHOW GET LAPLANQTY[lnCnter] DISABLE 
  ENDFOR
  _CUROBJ = OBJNUM(ibLStyle)
  RETURN
ENDIF
=SEEK('S'+Style.Scale,'Scale') 
lnCntr = 0
DIMENSION  laAvalQty[9] ,laPlanqty [9] ,laBalnQty[9],laOldBalan[9]
STORE 0 TO laAvalQty, laPlanqty , laBalnQty,laOldBalan

FOR lnCntr =1 TO 8
  lcI=STR(lnCntr,1)
  lcSize&lcI = Scale.sz&lcI
  *SHOW GET LAPLANQTY[&lcI] ENABLE 
  SHOW GET LAPLANQTY[lnCntr] ENABLE 
ENDFOR
SHOW GETS WINDOW ('PODISTLC') 

SELECT (lcPOLine)
m.cStyType = lcAType
m.Trancd = '1'
lnLines  = lnLines + 1
m.LineNo = lnLines
m.Po     = laData[1]
m.Vendor = lcSrcLct
m.Scale  = Style.Scale
m.PrePak = Style.CbuyPrePk
m.cVenSty= Style.cVenSty

llConCst=.T.  
*--Read equivalent costs.
m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)
IF !llMulCurr OR STYLE.cPriceCur=laData[26]
  m.Disc_Pcnt  = STYLE.Disc_Pcnt
  m.Gros_Price = IIF(STYLE.Gros_Price=0 AND STYLE.Disc_Pcnt=0,m.nCost1,STYLE.Gros_Price)
ELSE
  m.Disc_Pcnt  = 0
  m.Gros_Price = m.nCost1
ENDIF

RETURN
*-- end of lfvStyle.

*!**************************************************************************
*! Name      : lfSHOWSCR
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : Refresh if in case of add mode
*!**************************************************************************
*! Example   : lfSHOWSCR()
*!**************************************************************************
*!
FUNCTION lfSHOWSCR
STORE '' TO lcSize1 , lcSize2 , lcSize3 , lcSize4 , lcSize5 , lcSize6 , lcSize7
STORE '' TO lcSize8 ,lcSrcLct , lcStyLoc, lcStyDesc
DIMENSION  laAvalQty[9] ,laPlanqty [9] ,laBalnQty[9],laSource[1],laOldBalan[9]
STORE 0 TO laAvalQty, laPlanqty , laBalnQty,laOldBalan

*!**************************************************************************
*! Name      : lfvPlanQ
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : validate Plan Qty
*!**************************************************************************
*! Example   : =lfvPlanQ()
*!**************************************************************************
*!
FUNCTION lfvPlanQ

*-- To get the stock qty
=SEEK(lcStyLoc+lcSrcLct,'StyDye')
lnCnt = 0
FOR lnCnt = 1 TO 8
  lcI = STR(lnCnt,1)
  laPlanqty[lnCnt] = StyDye.stk&lcI
  laPlanqty[9]    = laPlanqty[9] + laPlanqty[lnCnt]
ENDFOR

=lfvAvlQ()

SHOW GETS WINDOW ('PODISTLC') 

*!**************************************************************************
*! Name      : lfvAvlQ
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : Calculate Available Qty
*!**************************************************************************
*! Example   : =lfvAvlQ()
*!**************************************************************************
*!
FUNCTION lfvAvlQ
PRIVATE lnAlias , lcOldOrd
PRIVATE lnLcqty1 ,lnLcqty2 ,lnLcqty3 ,lnLcqty4 ,lnLcqty5 ,lnLcqty6 ,lnLcqty7 ,lnLcqty8 
STORE 0 TO lnLcqty1 ,lnLcqty2 ,lnLcqty3 ,lnLcqty4 ,lnLcqty5 ,lnLcqty6 ,lnLcqty7 ,lnLcqty8 
lnAlias = SELECT()
SELECT POSLN
lcOldOrd = SET('ORDER')
SET ORDER TO Poslns
=SEEK(lcStyLoc+'N','POSLN')
SCAN REST WHILE style+cstytype+po+STR(lineno,6)+trancd = lcStyLoc+'N'

  *B606451,1 KHM 09/03/2002 (Begin) Get only the Inter-Location POs for the source location.
  IF SEEK('N'+PosLn.PO,'PosHdr') AND PosHdr.Vendor <> lcSrcLct
    LOOP
  ENDIF
  *B606451,1 KHM 09/03/2002 (End)

  lnCntr = 0
  FOR lnCntr = 1 TO 8
    lcI=STR(lnCntr,1)
    IF trancd = '1'
        lnLcqty&lcI = lnLcqty&lcI + POSLN.Qty&lcI
    ENDIF  
    IF trancd = '6'
        lnLcqty&lcI = lnLcqty&lcI - POSLN.Qty&lcI
    ENDIF  
  ENDFOR
ENDSCAN
lnCtr = 0
FOR lnCtr = 1 TO 8
  lcI=STR(lnCtr,1)
  laAvalQty[lnCtr] = laPlanqty[lnCtr] - lnLcqty&lcI 
ENDFOR
laAvalQty[9] = laAvalQty[1] + laAvalQty[2] + laAvalQty[3] + laAvalQty[4]+;
               laAvalQty[5] + laAvalQty[6] + laAvalQty[7] + laAvalQty[8]

SET ORDER TO &lcOldOrd
SELECT(lnAlias)
*--End
*!**************************************************************************
*! Name      : lfVLcOk
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : validate Ok Button
*!**************************************************************************
*! Example   : =lfVLcOk()
*!**************************************************************************
*!
FUNCTION lfVLcOk
PRIVATE lnAlias
STORE 0 TO lnLineNo , m.LineNo
IF EMPTY(laTarget)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to select at least one target. Cannot proceed.')
  RETURN
ENDIF
IF EMPTY(lcStyLoc)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to select a Style. Cannot proceed.')
  RETURN
ENDIF

IF ASCAN(laTarget,ALLTRIM(lcSrcLct)) > 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Source location: "+ALLTRIM(lcSrcLct)+" is exist in the target locations. Cannot proceed.")
  RETURN
ENDIF

llMultiWare=.T.

laData[2]  = lcSrcLct
*-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
SELECT (lcPoLine)
LOCATE
IF EOF()
  FOR I = 1 to ALEN(laTarget,1)
    IF EMPTY(laTarget[I])
      LOOP
    ENDIF
    lcI=ALLTRIM(STR(I,3))
    IF !SEEK(PADR(lcStyLoc,19)+PADR(ALLTRIM(laTarget[&lcI]),6)+SPACE(10),'STYDYE')
      IF gfModalGen('QRM34048B34002','DIALOG',ALLTRIM(lcStyLoc)+'|'+ALLTRIM(laTarget[&lcI])) = 1
        DO gpAdStyWar WITH lcStyLoc,SPACE(10),laTarget[&lcI]
      ELSE
        LOOP  
      ENDIF  
    ENDIF
    =SEEK(lcStyLoc,'Style')
    lnAlias = select()
    SELECT(lcPoLine)
    lnLineNo = lnLineNo +1
    m.Trancd = '1'
    m.LineNo = lnLineNo
    m.Style = lcStyLoc
    m.cWareCode = laTarget[I]
    m.Vendor = lcSrcLct
    m.cStyType = "N"
    m.Po     = laData[1]
    m.Scale  = Style.Scale
    m.PrePak = Style.CbuyPrePk
    m.cVenSty= Style.cVenSty
    m.nCost1 = IIF(!llMulCurr OR STYLE.cPriceCur=laData[26],Style.nICost1,0)
    m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
    m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
    m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
    m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)
    m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
    m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
    m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
    m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
    m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)
    m.Disc_Pcnt  = STYLE.Disc_Pcnt
    m.Gros_Price = IIF(STYLE.Gros_Price=0 AND STYLE.Disc_Pcnt=0,m.nCost1,STYLE.Gros_Price)
   
    APPEND BLANK
    GATHER MEMVAR 

    SELECT(lnAlias)
  ENDFOR
  llCalFrmOp = .T.
ENDIF
CLEAR READ  

*!**************************************************************************
*! Name      : lfVLcCan
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : validate Cancel Button
*!**************************************************************************
*! Example   : lfVLcCan()
*!**************************************************************************
*!
FUNCTION lfVLcCan
PRIVATE lnAlias 
lnAlias = select()
SELECT(lcPoLine)
LOCATE
IF EOF()
  STORE '' TO lcSize1 , lcSize2 , lcSize3 , lcSize4 , lcSize5 , lcSize6 , lcSize7
  STORE '' TO lcSize8 ,lcSrcLct , lcStyLoc
  DIMENSION  laAvalQty[9] ,laPlanqty [9] ,laBalnQty[9],laSource[1],laTarget[1],laOldBalan[9]
  STORE 0 TO laAvalQty, laPlanqty , laBalnQty, laOldBalan
  STORE '' TO laSource, laTarget ,lcSrcLct ,lcStyDesc
ENDIF
SELECT (lnAlias)
CLEAR READ

*!**************************************************************************
*! Name      : lfGtBalQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/03/2001
*! Purpose   : validate balance Qty for the second time the user enters to screen
*!**************************************************************************
*! Example   : =lfGtBalQty()
*!**************************************************************************
*!
FUNCTION lfGtBalQty
PRIVATE lnAlias 
lnAlias = select()
SELECT(lcPoLine)
LOCATE
IF !EOF()
  SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 ;
        TO laBalnQty[1] ,laBalnQty[2] ,laBalnQty[3] ,laBalnQty[4] ,laBalnQty[5] ,;
           laBalnQty[6] ,laBalnQty[7] ,laBalnQty[8]
          
  laBalnQty[9] = laBalnQty[1] + laBalnQty[2] + laBalnQty[3] + laBalnQty[4]+;
                 laBalnQty[5] + laBalnQty[6] + laBalnQty[7] + laBalnQty[8]
  *-- To get total quantity for the BALANCE
  lnCountr = 0
  FOR lnCountr = 1 TO 8
    laOldBalan[lnCountr]   = laBalnQty[lnCountr]
    laBalnQty[9]           = laBalnQty[9] +laBalnQty[lnCountr]
  ENDFOR

  *-- To reduce the balance from the Plan qty.
  lnCntr = 0
  FOR lnCntr = 1 to 8
    laBalnQty[lnCntr] = laPlanqty[lnCntr] - laBalnQty[lnCntr]    
  ENDFOR
  laBalnQty[9] = laBalnQty[1] + laBalnQty[2] + laBalnQty[3] + laBalnQty[4]+;
                 laBalnQty[5] + laBalnQty[6] + laBalnQty[7] + laBalnQty[8]
ENDIF

*!**************************************************************************
*! Name      : lfCHECKQTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Check for the balance Qty
*!**************************************************************************
*! Example   : lfCHECKQTY()
*!**************************************************************************
*!
FUNCTION lfCHECKQTY

=lfGtBalQty()
lnCnt = 0
FOR lnCnt = 1 to 8
  IF laBalnQty[lnCnt] <> 0 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Distributed quantities are not equal to the plan quantities. Cannot proceed.')
    llRetrn = .T.
    RETURN
  ENDIF
ENDFOR

*!**************************************************************************
*! Name      : lfvQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Validate the quantities
*!**************************************************************************
*! Example   : lfvQty()
*!**************************************************************************
*!
FUNCTION lfvQty
PARAMETERS lnQtyNo

laBalnQty[lnQtyNo] = laPlanqty[lnQtyNo] - laOldBalan[lnQtyNo]

laPlanqty[9] = laPlanqty[1] + laPlanqty[2] + laPlanqty[3] + laPlanqty[4]+;
               laPlanqty[5] + laPlanqty[6] + laPlanqty[7] + laPlanqty[8]
laBalnQty[9] = laBalnQty[1] + laBalnQty[2] + laBalnQty[3] + laBalnQty[4]+;
               laBalnQty[5] + laBalnQty[6] + laBalnQty[7] + laBalnQty[8]

FOR lnCnter = 1 TO 9
  SHOW GET LAPLANQTY[lnCnter]
ENDFOR
SHOW GETS

*!**************************************************************************
*! Name      : lfQtyFocus
*! Developer : Mohamed Shokry (MHM)
*! Date      : 27/02/2000
*! Purpose   : Set the focus on the qty1
*!**************************************************************************
*! Example   : lfQtyFocus()
*!**************************************************************************
*!
FUNCTION lfQtyFocus

IF WONTOP() = lcBrTtl1 .OR. WONTOP() = lcBrTtl2
  ACTIVATE WINDOW (lcWinC22)
  _CUROBJ = OBJNUM(m.Qty1)
ENDIF

*!**************************************************************************
*! Name      : lfDeline
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/11/2001
*! Purpose   : Deleting batch line.
*!**************************************************************************
*! Example   : lfDeline()
*!**************************************************************************
*!
FUNCTION lfDeline

IF gfModalGen("QRM00000B34001","DIALOG","",.F.,"Do you want to add the removed line to any other batch?") = 2
  IF SEEK('N'+EVALUATE(lcTmpLine+'.CUTTKT')+EVALUATE(lcTmpLine+'.STYLE')+STR(EVALUATE(lcTmpLine+'.NLINENO'),6)+'1','POSLN')
    SELECT POSLN
    SCATTER MEMO MEMVAR
    IF !USED(lcTmpPosln)
      =AFIELDS(laFStru)
      CREATE CURSOR (lcTmpPosln) FROM ARRAY laFStru
    ENDIF
    SELECT (lcTmpPosln)
    APPEND BLANK
    GATHER MEMO MEMVAR
    REPLACE REFERENCE WITH 'HOLD'
  ENDIF
ENDIF
*-- end of lfDeline.

*!**************************************************************************
*! Name      : lfEditLine
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/11/2001
*! Purpose   : Editing batch line.
*!**************************************************************************
*! Example   : lfEditLine()
*!**************************************************************************
*!
FUNCTION lfEditLine

IF _CUROBJ<>OBJNUM(laCan[lnCnxt]) AND lcStatus = 'Approved' AND lnNewBal<0
  =gfModalGen('INM00000B34000','DIALOG',"",.F.,"Entered quantity exceeds the new balance and it will be reduced.")
  DO CASE
    CASE _CUROBJ = OBJNUM(laSok[lnCnxt])
      laSok[lnCnxt] = laSok[lnCnxt] + lnNewBal
      laSok[lnCnxt] = IIF(laSok[lnCnxt]<0,0,laSok[lnCnxt])
      SHOW GET laSok[lnCnxt]
    CASE _CUROBJ = OBJNUM(laDam1[lnCnxt])
      laDam1[lnCnxt] = laDam1[lnCnxt] + lnNewBal
      laDam1[lnCnxt] = IIF(laDam1[lnCnxt]<0,0,laDam1[lnCnxt])
      SHOW GET laDam1[lnCnxt]
    CASE _CUROBJ = OBJNUM(laDam2[lnCnxt])
      laDam2[lnCnxt] = laDam2[lnCnxt] + lnNewBal
      laDam2[lnCnxt] = IIF(laDam2[lnCnxt]<0,0,laDam2[lnCnxt])
      SHOW GET laDam2[lnCnxt]
  ENDCASE
  STORE 0 TO laBal[lnCnxt], lnNewBal
ENDIF
*-- end of lfEditLine.

*!**************************************************************************
*! Name      : lfSaveLine
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/11/2001
*! Purpose   : Update the hold POSLN records.
*!**************************************************************************
*! Example   : lfSaveLine()
*!**************************************************************************
*!
FUNCTION lfSaveLine

IF USED(lcTmpPosln)
  PRIVATE lcOrder
  SELECT (lcTmpLine)
  lcOrder = SET('ORDER')
  SET ORDER TO TAG TmpLine3
  SELECT (lcTmpPosln)
  LOCATE
  IF !EOF()
    SCAN
      SCATTER MEMO MEMVAR
      IF !SEEK(PO+STR(LINENO,6),lcTmpLine)
        IF SEEK('N'+PO+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
          SELECT POSLN
          GATHER MEMO MEMVAR
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcTmpLine)
  SET ORDER TO &lcOrder.
ENDIF
*-- end of lfSaveLine.

*!**************************************************************************
*! Name      : lfDelIntPo
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/19/2001
*! Purpose   : check if Inter Location PO found in open or approved batch.
*!**************************************************************************
*! Example   : lfDelIntPo()
*!**************************************************************************
*!
FUNCTION lfDelIntPo

PRIVATE lnAlias, lcOrderH, lcOrderL, llOpenL, llOpenH, lcBatSt
lnAlias = SELECT(0)
STORE .F. TO llOpenL, llOpenH
STORE SPACE(0) TO lcOrderH, lcOrderL
IF USED('CTKTRCVL')
  SELECT CTKTRCVL
  lcOrderL = SET('ORDER')
  llOpenL = .F.
ELSE
  = gfOpenFile(gcDataDir+'CTKTRCVL','CTKTRCVLC','SH')
  llOpenL = .T.
ENDIF
SELECT CTKTRCVL
IF !llOpenL
  SET ORDER TO TAG CTKTRCVLC
ENDIF
IF SEEK('N'+laData[1])
  IF USED('CTKTRCVH')
    SELECT CTKTRCVH
    lcOrderH = SET('ORDER')
    llOpenH = .F.
  ELSE
    = gfOpenFile(gcDataDir+'CTKTRCVH','CTKTRCVH','SH')
    llOpenH = .T.
  ENDIF
  SELECT CTKTRCVH
  IF !llOpenH
    SET ORDER TO TAG CTKTRCVH
  ENDIF
  SELECT CTKTRCVL
  SCAN REST WHILE cType+Cuttkt+Style+Dyelot+STR(nLineno,6)+Tmprcvnum+Trancd = 'N'+laData[1]
    IF SEEK('N'+TMPRCVNUM,'CTKTRCVH') .AND. CTKTRCVH.CSTATUS $ 'AO'
      llNotCan = .T.
      lcBatSt = IIF(CTKTRCVH.CSTATUS='O','open','approved')
      = gfModalGen("INM00000B00000","DIALOG","",.F.,"This inter location PO has a line in "+;
                   lcBatSt + " batch # " + ALLTRIM(CTKTRCVH.TMPRCVNUM) + " , Cannot cancel.")
      EXIT
    ENDIF
  ENDSCAN
  IF !llOpenH .AND. !EMPTY(lcOrderH)
    SELECT CTKTRCVH
    SET ORDER TO &lcOrderH.
  ELSE
    USE IN CTKTRCVH
  ENDIF
ENDIF

IF !llOpenL
  SELECT CTKTRCVL
  SET ORDER TO &lcOrderL.
ELSE
  USE IN CTKTRCVL
ENDIF
SELECT (lnAlias)
*-- end of lfDelIntPo.

*!**************************************************************************
*! Name      : lfRemLine
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/19/2001
*! Purpose   : check if Inter Location PO found in open or approved batch.
*!**************************************************************************
*! Example   : lfRemLine()
*!**************************************************************************
*!
FUNCTION lfRemLine

PRIVATE lnAlias, lcOrderH, lcOrderL, llOpenL, llOpenH, lcBatSt
lnAlias = SELECT(0)
STORE .F. TO llOpenL, llOpenH
STORE SPACE(0) TO lcOrderL, lcOrderH
IF USED('CTKTRCVL')
  SELECT CTKTRCVL
  lcOrderL = SET('ORDER')
  llOpenL = .F.
ELSE
  = gfOpenFile(gcDataDir+'CTKTRCVL','CTKTRCVLC','SH')
  llOpenL = .T.
ENDIF
SELECT CTKTRCVL
IF !llOpenL
  SET ORDER TO TAG CTKTRCVLC
ENDIF
IF SEEK('N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
        STR(EVALUATE(lcPoLine+'.LineNo'),6))
  IF USED('CTKTRCVH')
    SELECT CTKTRCVH
    lcOrderH = SET('ORDER')
    llOpenH = .F.
  ELSE
    = gfOpenFile(gcDataDir+'CTKTRCVH','CTKTRCVH','SH')
    llOpenH = .T.
  ENDIF
  SELECT CTKTRCVH
  IF !llOpenH
    SET ORDER TO TAG CTKTRCVH
  ENDIF
  SELECT CTKTRCVL
  SCAN REST WHILE CTYPE+CUTTKT+STYLE+DYELOT+STR(NLINENO,6)+TMPRCVNUM+TRANCD = ;
                  'N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
                  STR(EVALUATE(lcPoLine+'.LineNo'),6)
    IF SEEK('N'+CTKTRCVL.TMPRCVNUM,'CTKTRCVH') .AND. CTKTRCVH.CSTATUS $ 'AO'
      llCantDele = .T.
      lcBatSt = IIF(CTKTRCVH.CSTATUS='O','open','approved')
      = gfModalGen("INM00000B00000","DIALOG","",.F.,"This line exist in "+;
                   lcBatSt + " batch # " + ALLTRIM(TMPRCVNUM) + " , Cannot Remove.")
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF !llOpenH .AND. !EMPTY(lcOrderH)
  SELECT CTKTRCVH
  SET ORDER TO &lcOrderH.
ELSE
  USE IN CTKTRCVH
ENDIF
IF !llOpenL
  SELECT CTKTRCVL
  SET ORDER TO &lcOrderL.
ELSE
  USE IN CTKTRCVL
ENDIF
SELECT (lnAlias)
*-- end of lfRemLine.

*!**************************************************************************
*! Name      : lfEditSize
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/19/2001
*! Purpose   : check if Inter Location PO found in open or approved batch.
*!**************************************************************************
*! Example   : lfEditSize()
*!**************************************************************************
*!
FUNCTION lfEditSize

PRIVATE lnAlias, lcOrderH, lcOrderL, llOpenL, llOpenH
lnAlias = SELECT(0)
STORE .F. TO llOpenL, llOpenH
STORE SPACE(0) TO lcOrderL, lcOrderH
IF USED('CTKTRCVL')
  SELECT CTKTRCVL
  lcOrderL = SET('ORDER')
  llOpenL = .F.
ELSE
  = gfOpenFile(gcDataDir+'CTKTRCVL','CTKTRCVLC','SH')
  llOpenL = .T.
ENDIF
SELECT CTKTRCVL
IF !llOpenL
  SET ORDER TO TAG CTKTRCVLC
ENDIF
IF SEEK('N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
        STR(EVALUATE(lcPoLine+'.LineNo'),6))
  IF USED('CTKTRCVH')
    SELECT CTKTRCVH
    lcOrderH = SET('ORDER')
    llOpenH = .F.
  ELSE
    = gfOpenFile(gcDataDir+'CTKTRCVH','CTKTRCVH','SH')
    llOpenH = .T.
  ENDIF
  SELECT CTKTRCVH
  IF !llOpenH
    SET ORDER TO TAG CTKTRCVH
  ENDIF
  SELECT CTKTRCVL
  SCAN REST WHILE CTYPE+CUTTKT+STYLE+DYELOT+STR(NLINENO,6)+TMPRCVNUM+TRANCD = ;
                  'N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
                  STR(EVALUATE(lcPoLine+'.LineNo'),6)
    IF SEEK('N'+CTKTRCVL.TMPRCVNUM,'CTKTRCVH') .AND. CTKTRCVH.CSTATUS = 'A'
      llNotEdit = .T.
      = gfModalGen("INM00000B00000","DIALOG","",.F.,"This line exist in "+;
                   "approved batch # " + ALLTRIM(TMPRCVNUM) + " , Cannot change.")
      EXIT
    ENDIF
  ENDSCAN
  IF !llOpenH .AND. !EMPTY(lcOrderH)
    SELECT CTKTRCVH
    SET ORDER TO &lcOrderH.
  ELSE
    USE IN CTKTRCVH
  ENDIF
ENDIF

IF !llOpenL
  SELECT CTKTRCVL
  SET ORDER TO &lcOrderL.
ELSE
  USE IN CTKTRCVL
ENDIF
SELECT (lnAlias)
*-- end of lfEditSize.

*!**************************************************************************
*! Name      : lfDisbWare
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/19/2001
*! Purpose   : check if Inter Location PO found in open or approved batch
*!             to disable the warehouse field.
*!**************************************************************************
*! Example   : lfDisbWare()
*!**************************************************************************
*!
FUNCTION lfDisbWare

PRIVATE lnAlias, lcOrderH, lcOrderL, llOpenL, llOpenH
lnAlias = SELECT(0)
STORE .F. TO llOpenL, llOpenH
STORE SPACE(0) TO lcOrderL, lcOrderH
IF USED('CTKTRCVL')
  SELECT CTKTRCVL
  lcOrderL = SET('ORDER')
  llOpenL = .F.
ELSE
  = gfOpenFile(gcDataDir+'CTKTRCVL','CTKTRCVLC','SH')
  llOpenL = .T.
ENDIF
SELECT CTKTRCVL
IF !llOpenL
  SET ORDER TO TAG CTKTRCVLC
ENDIF
IF SEEK('N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
        STR(EVALUATE(lcPoLine+'.LineNo'),6))
  IF USED('CTKTRCVH')
    SELECT CTKTRCVH
    lcOrderH = SET('ORDER')
    llOpenH = .F.
  ELSE
    = gfOpenFile(gcDataDir+'CTKTRCVH','CTKTRCVH','SH')
    llOpenH = .T.
  ENDIF
  SELECT CTKTRCVH
  IF !llOpenH
    SET ORDER TO TAG CTKTRCVH
  ENDIF
  SELECT CTKTRCVL
  SCAN REST WHILE CTYPE+CUTTKT+STYLE+DYELOT+STR(NLINENO,6)+TMPRCVNUM+TRANCD = ;
                  'N'+laData[1]+EVALUATE(lcPoLine+'.Style')+EVALUATE(lcPoLine+'.Dyelot')+;
                  STR(EVALUATE(lcPoLine+'.LineNo'),6)
    IF SEEK('N'+CTKTRCVL.TMPRCVNUM,'CTKTRCVH') .AND. CTKTRCVH.CSTATUS $ 'AO'
      SHOW GET m.cWareCode DISABLE
      EXIT
    ENDIF
  ENDSCAN
  IF !llOpenH .AND. !EMPTY(lcOrderH)
    SELECT CTKTRCVH
    SET ORDER TO &lcOrderH.
  ELSE
    USE IN CTKTRCVH
  ENDIF
ENDIF

IF !llOpenL
  SELECT CTKTRCVL
  SET ORDER TO &lcOrderL.
ELSE
  USE IN CTKTRCVL
ENDIF
SELECT (lnAlias)
*-- end of lfDisbWare.

*!**************************************************************************
*! Name      : lfCancel
*! Developer : (AMH) Ahmed Maher
*! Date      : 04/08/2001
*! Purpose   : Cancel Batches.
*!**************************************************************************
*! Example   : lfCancel()
*!**************************************************************************
*!
FUNCTION lfCancel

PRIVATE lnAlias
IF gfModalGen("QRM00000B34001","DIALOG","",.F.,"Do you want to add the lines of this batch to any other batch?") = 2
  lnAlias = SELECT(0)
  SELECT (lcTmpLine)
  LOCATE
  SCAN FOR TRANCD = '1'
    IF SEEK('N'+CUTTKT+STYLE+STR(NLINENO,6)+'1','POSLN')
      SELECT POSLN
      REPLACE REFERENCE WITH 'HOLD'
    ENDIF
  ENDSCAN
ENDIF
*-- end of lfCancel.

*:**************************************************************************
*:* Name        : lfGnUPCClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/24/2001
*:* Purpose     : Add UPC for color-size
*:***************************************************************************
*:* Called from : lpSavScr
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  =lfGnUPCClr()
*:***************************************************************************
*:C200171,1
*:***************************************************************************
FUNCTION lfGnUPCClr
*-- Open Needed files
STORE .F. TO llOpStUPC
IF !USED('STYLEUPC')
  llOpStUPC = gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
ENDIF

=SEEK(&lcColorFil..STYLE,'STYLE')

*-- Generate a unique UPC# for current style - it must be of width 6
*Wab
*lcUPC = PADL(SYS(2007,STYLE.STYLE),5,'0')
*wab
=SEEK('S'+STYLE.SCALE,'SCALE')
IF !SEEK(STYLE.STYLE,'STYLEUPC')
  
  *-- For the generated UPC add a line in STYLEUPC for each size
  FOR lnCount = 1 TO SCALE.CNT
    lcSz = STR(lnCount,1)
    *wab
    *INSERT INTO STYLEUPC (STYLE,SIZE,CUPCNUM1) VALUES (STYLE.STYLE,lcSz,lcUPC+lcSz)
    lcUPC = gfSequence('CUPCELNO')
    INSERT INTO STYLEUPC (STYLE,SIZE,CUPCNUM1) VALUES (STYLE.STYLE,lcSz,lcUPC)
    *wab
  NEXT  
ENDIF

IF llOpStUPC
  USE IN STYLEUPC
ENDIF
*-- end of lfGnUPCClr.
*:**************************************************************************
*:* Name        : lfGnUPCWh
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/24/2001
*:* Purpose     : Add UPC to EDICATGD
*:***************************************************************************
*:* Called from : lfSaveWare(),gpAdStyWar
*:***************************************************************************
*:* Passed Parameters :lcWhCode   - Warehous code
*:*                   :lcSty      - STYLE Color
*:*                   :llFrmAdWre - .T. :: called from gpAdStyWar
*:*                   :             .F. :: called from lfSaveWare()
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : =lfGnUPCWh(&lcWareFil..CWARECODE,&lcWareFil..STYLE,.T.)
*:***************************************************************************
*:C200171,1
*:***************************************************************************

FUNCTION lfGnUPCWh
*-- lcWhCode,lcSty,llFrmAdWre &&params are Defined befor calling this function
PRIVATE lcScale

STORE .F. TO llOopEdiH,llOpEdiD,llOpStUPC,llOpTrans
IF !USED('EDICATGH')
  llOopEdiH = gfOpenFile(gcDataDir+'EDICATGH','ACCOUNT','SH')
ENDIF
IF !USED('EDICATGD')
  llOpEdiD  = gfOpenFile(gcDataDir+'EDICATGD','ACCOUNT','SH')
ENDIF
IF !USED('STYLEUPC')
  llOpStUPC = gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
ENDIF
IF !USED('EDITRANS')
  llOpTrans= gfOpenFile(gcDataDir+'EDITRANS','Typekey','SH')
ENDIF
*-- Locate the warehous to get CSITEID
=SEEK(lcWhCode,'WAREHOUS')
*-- Get account from codes.dbf  
lcAccVal = ''
DIME laSiteRlt[1,2]
laSiteRlt[1,1] = 'ACCOUNT'
laSiteRlt[1,2] = 'lcAccVal'
= gfRltFld(WAREHOUS.CSITEID,@laSiteRlt,'CSITEID')  
lcAccVal = ALLT(lcAccVal)

*-- CHECK IF THE RELATED FIELD ACCOUNT (lcAccVal) IS  in EDICATGH.CPARTNER then add UPC's to EDICATGD
IF llFrmAdWre
  *-- Global Call ( from gpAdStyWar )    
  IF !EMPTY(lcAccVal) .AND. SEEK('A'+lcAccVal,'EDICATGH') 
    =SEEK('S'+STYLE.SCALE,'SCALE')
    SELECT EDICATGH
    IF !SEEK(TYPE+CPARTNER+CSELCCODE+STYLE.STYLE,'EDICATGD')
      *-- type+cpartner+cselccode+style+size
      FOR lnCntSz = 1 TO SCALE.CNT
        *-- Add a line in EDICATGD for each size
        =SEEK(STYLE.STYLE+STR(lnCntSz,1),'STYLEUPC')
        SELECT EDICATGD
        APPEND BLANK        
        REPLACE TYPE      WITH 'A',;
                CPARTNER  WITH lcAccVal,;
                CSELCCODE WITH EDICATGH.CSELCCODE,;
                STYLE     WITH STYLE.STYLE,;
                SIZE      WITH STR(lnCntSz,1),;
                CUPC      WITH STYLEUPC.CUPCNUM1,;
                CEDIDSTAT WITH 'A',;
                DATE      WITH DATE(),;
                LTRANSMIT WITH .T.
      NEXT
      REPLACE EDICATGH.LTRANSMIT WITH .T.
      SELECT EDITrans
      IF SEEK("832" + PADR(EDICatgH.cSelcCode , 20) + EDICatgH.Type + EDICatgH.cPartner)
        IF cStatus <> "N"
           REPLACE cStatus WITH "N"
        ENDIF
      ELSE
        APPEND BLANK
        REPLACE cEDITrnTyp WITH "832"              ,;
                Key        WITH EDICatgH.cSelcCode ,;
                Type       WITH EDICatgH.Type      ,;
                cPartner   WITH EDICatgH.cPartner  ,;
                cStatus    WITH "N"
      ENDIF
    ENDIF
  ENDIF

ELSE
  *-- Local Call ( from within ICSTYLE.PRG )
  IF !EMPTY(lcAccVal) .AND. SEEK('A'+lcAccVal,'EDICATGH') AND !EMPTY(laColor[1])
    FOR lnCntClr = 1 TO ALEN(laColor,1)
      *--Loop through colors      
      =SEEK(SUBSTR(lcSty,1,lnStyleWid)+lcSepart+laColor[lnCntClr],'STYLE')
      =SEEK('S'+STYLE.SCALE,'SCALE')
      SELECT EDICATGH
      IF !SEEK(TYPE+CPARTNER+CSELCCODE+STYLE.STYLE,'EDICATGD')
        *-- type+cpartner+cselccode+style+size
        FOR lnCntSz = 1 TO SCALE.CNT
          *-- Add a line in EDICATGD for each size          
          =SEEK(STYLE.STYLE+STR(lnCntSz,1),'STYLEUPC')
          SELECT EDICATGD
          APPEND BLANK
          REPLACE TYPE      WITH 'A',;
                  CPARTNER  WITH lcAccVal,;
                  CSELCCODE WITH EDICATGH.CSELCCODE,;
                  STYLE     WITH STYLE.STYLE,;
                  SIZE      WITH STR(lnCntSz,1),;
                  CUPC      WITH STYLEUPC.CUPCNUM1,;
                  CEDIDSTAT WITH 'A',;
                  DATE      WITH DATE(),;
                  LTRANSMIT WITH .T.
        NEXT
        REPLACE EDICATGH.LTRANSMIT WITH .T.
        SELECT EDITrans
        IF SEEK("832" + PADR(EDICatgH.cSelcCode , 20) + EDICatgH.Type + EDICatgH.cPartner)
          IF cStatus <> "N"
             REPLACE cStatus WITH "N"
          ENDIF
        ELSE
          APPEND BLANK
          REPLACE cEDITrnTyp WITH "832"              ,;
                  Key        WITH EDICatgH.cSelcCode ,;
                  Type       WITH EDICatgH.Type      ,;
                  cPartner   WITH EDICatgH.cPartner  ,;
                  cStatus    WITH "N"
        ENDIF
      ENDIF
    NEXT
  ENDIF
ENDIF

*-- Close tables
IF llOopEdiH
  USE IN EDICATGH
ENDIF
IF llOpEdiD 
  USE IN EDICATGD
ENDIF
IF llOpStUPC
  USE IN STYLEUPC
ENDIF
IF llOpTrans
  USE IN EDITRANS
ENDIF
*-- end of lfGnUPCWh.