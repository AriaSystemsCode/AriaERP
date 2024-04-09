*!**************************************************************************
*! Name      : VNMRMAIN.PRG
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/11/2003
*! Purpose   : VAN MAR Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C037017,1
*!**************************************************************************
*! Modifications
*!B121218,1 MHM 03/01/2004 Change Account field to be custom field "cAccCode"
*!B126157,1 MHM 02/17/2005 Change Account field to be custom field "cVanCode"
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*:**************************************************************************
*:* Name        : lfPoStyPD
*:* Developer   : MHM - MOHAMED SHOKRY
*:* Date        : 09/11/2003
*:* Purpose     : Define pad line for po Detail entry screen
*:***************************************************************************
*:* Called from : PoStyle.prg
*:***************************************************************************
*:* Example     : = lfPoStyPD()
*:***************************************************************************
*C037017,1
FUNCTION lfPoStyPD
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Po Detail Entry Screen'  ;
             SKIP FOR (lnActFolder<>2) 
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO gfDoTriger WITH "POSTY",PADR("POSTYSCR",10)
*-- end of lfPoQOrdPD.

*:**************************************************************************
*:* Name        : lfPOSTYSCR
*:* Developer   : MHM - MOHAMED SHOKRY
*:* Date        : 09/11/2003
*:* Purpose     : Define pad line for po Detail entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  =lfPOSTYSCR()
*:***************************************************************************
*C037017,1
FUNCTION lfPOSTYSCR
SELECT (lcPOLine)
*--LOOP TO Update Discreption
SCAN
  REPLACE CCOMPLDESC WITH IIF(SEEK(&lcPOLine..Style,'STYLE'),STYLE.Desc1,SPACE(35))
ENDSCAN
LOCATE

SCATTER MEMVAR MEMO
*--Open needed files

lcBrowTtl = 'Original Detail'
*STORE {} TO ldshpsdat
PUSH KEY

*--Define keys to navigate throgh the browse by keys

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl) 


lcAlias = SELECT(0)
lcOrd = ORDER(lcPoLine)   

*-- Create temp colors , sizes file
lcModStat = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')

DO (gcScrDir+'PoStyVM.SPX')
POP KEY

SELECT (lcAlias)
SET ORDER TO &lcOrd IN &lcPoLine
LOCATE
*--Refresh the detail screen in the postyle.spx
=lfvBrow1()
SHOW WINDOW (lcWinC22) REFRESH SAME
SHOW GETS WINDOW (lcWinC22) ENABLE ONLY
IF ! llMultiWare
  SHOW GET lnLShpLoc   DISABLE
  SHOW GET pbLShpTo    DISABLE
  SHOW GET m.cWareCode DISABLE      
  SHOW GET m.Store     DISABLE      
ENDIF
SHOW GET m.Style DISABLE
SHOW GET ibStyle DISABLE


*-- end of lfPOSTYSCR.
*:**************************************************************************
*:* Name        : lptab
*:* Developer   : MHM - MOHAMED SHOKRY
*:* Date        : 09/11/2003
*:* Purpose     : Backtab function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpBktab()
*:***************************************************************************
*
FUNCTION lptab
DO CASE
	CASE WONTOP() = lcBrowTtl 
      ACTIVATE WINDOW PoStyVm22
    CASE WONTOP() = "PoStyVm22"
      ACTIVATE WINDOW PoStYVm2
    OTHERWISE
    *--to validate Account
      
      *B121218,1 MHM 03/01/2004 [Start]
      *IF _CUROBJ = OBJNUM(m.account)

      *!B126157,1 MHM 02/17/2005 Change Account [Start]
      *IF _CUROBJ = OBJNUM(m.cAccCode)
      IF _CUROBJ = OBJNUM(m.cVanCode)
      *!B126157,1 MHM [End]
      
        =lfvAccount()
      ENDIF
ENDCASE
*_CUROBJ = _CUROBJ + 1

RETURN
*-- end of lpBktab.


*:*************************************************************
*: Name          : lfBrowLine
*: Developer    : MHM - MOHAMED SHOKRY
*: Date         : 09/11/2003
*: Purpose       : browse lines for the selected P/O
*:*************************************************************
*: Calls         : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example       : =lfBrowLine()
*:*************************************************************
*
FUNCTION lfBrowLine

lcSelect = SELECT()
lnMarker = RECNO()

IF llQtyPSize
  lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [Style :30 :H=lcStyHdr :R,]+;
             [Qty1  :H='Size1':R,]+;
             [Qty2  :H='Size2':R,]+;
             [Qty3  :H='Size3':R,]+;
             [Qty4  :H='Size4':R,]+;
             [Qty5  :H='Size5':R,]+;
             [Qty6  :H='Size6':R,]+;
             [Qty7  :H='Size7':R,]+;
             [Qty8  :H='Size8':R,]+;
             [Prepak:H='PP':R,]+;
             [PPQty :H='PPQt':R,]+;
             [Reference :H='Reference':R,]+;
             [cVenSty :H='Vendor Style':R]
ELSE
  IF lcAtype = "N" AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
  
    *B121218,1 MHM 03/01/2004 [Start]
    *lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
    *           [Style   :30 :H=lcStyHdr :R,]+;
    *           [lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='Ship To':R,]+;
    *           [TotQty  :H='Qty'            :R:P='9999999']

    *!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
    *lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [Style   :30 :H=lcStyHdr :R,]+;
               [lcShToTTl=IIF(!EMPTY(m.cAccCode),m.cAccCode,cWareCode) :H='Ship To':R,]+;
               [TotQty  :H='Qty'            :R:P='9999999']
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [Style   :30 :H=lcStyHdr :R,]+;
               [lcShToTTl=IIF(!EMPTY(m.cVanCode),m.cVanCode,cWareCode) :H='Ship To':R,]+;
               [TotQty  :H='Qty'            :R:P='9999999']

    *!B126157,1 MHM  [End]
   
   IF llCostPrv
      lcBrD1fld = lcBrD1fld+[,Gros_Price :H='Price '+lcPfrnsmbl :R:P='999999999.999',]+;
                            [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='99999999999.999']
   ENDIF                         

  ELSE
  lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [Style   :30 :H=lcStyHdr :R,]+;
             [Prepak  :H='P' :P='!':R,]+;
             [PPQty   :H='PPQty':P='9999':R,]

  IF lcAType = 'N'
    *B121218,1 MHM 03/01/2004 [Start]
    *lcBrD1fld=lcBrD1fld+[lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='Ship To':R,]

    *!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
    *lcBrD1fld=lcBrD1fld+[lcShToTTl=IIF(!EMPTY(m.cAccCode),m.cAccCode,cWareCode) :H='Ship To':R,]
    lcBrD1fld=lcBrD1fld+[lcShToTTl=IIF(!EMPTY(m.cVanCode),m.cVanCode,cWareCode) :H='Ship To':R,]
    *!B126157,1 MHM [End]
    
  ENDIF

  IF llCostPrv
    
    lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                        [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
   
    IF lcAType <> 'N'
      lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                          [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
    ENDIF                      

    IF !llMulCurr OR (laData[26]=gcBaseCurr AND laData[27]=gcBaseCurr) 
     
      lcBrD1fld=lcBrD1fld+[nCost1  :H='Net '+lcPfrnsmbl :R:P='999999999.999',]+;
                          [TotQty  :H='Qty'            :R:P='9999999',]+;         
                          [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='99999999999.999']
    
    ELSE
      lcBrD1fld=lcBrD1fld+[TotQty  :H='Qty'            :R:P='9999999',]+;
                          [lnAmount=(nECost1*TotQty) :H='Amount' :P='999999999.999']
    ENDIF

  ELSE
    lcBrD1fld=lcBrD1fld+[TotQty  :H='Qty'  :R:P='9999999']
  ENDIF

  IF lcAType = 'N'
    lcBrD1fld=lcBrD1fld+[,Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                        [Disc_Pcnt  :H='Disc.'  :R:P='%99.99']
  ENDIF                      
  IF lcAType <> 'N' AND llMultiWare

    *B121218,1 MHM 03/01/2004 [Start]
    *lcBrD1fld=lcBrD1fld+[,lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H=IIF(lcAType='R','Ship From','Ship To')]

    *!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
    *lcBrD1fld=lcBrD1fld+[,lcShToTTl=IIF(!EMPTY(m.cAccCode),m.cAccCode,cWareCode) :H=IIF(lcAType='R','Ship From','Ship To')]
    lcBrD1fld=lcBrD1fld+[,lcShToTTl=IIF(!EMPTY(m.cVanCode),m.cVanCode,cWareCode) :H=IIF(lcAType='R','Ship From','Ship To')]
    *!B126157,1 MHM [End]
    
    *B121218,1 MHM 03/01/2004 [End]
    
  ENDIF

  IF llCostPrv
    lcBrD1fld=lcBrD1fld+[,nECost1 :H='Base Price' :R:P='999999999.999']
  ENDIF
  ENDIF
ENDIF

IF lcAType = 'P' AND llDispPric
  lcBrD1fld = lcBrD1fld + [,nSelPrice :H='Selling Price' :R:P='999999999.99',;
            nGrosMrgn :H='Gross Margin' :R:P='9999.99']
ENDIF
lcBrD1fld = lcBrD1fld + IIF(llDyelot,;
                        [,Dyelot :H= 'Dyelot' :R :20],'')

*m.CCOMPLDESC = LEFT(Style.Desc1,35)
SELECT (lcPOLine)
BROWSE FIELDS &lcBrD1fld ;
       NOAPPEND ;
	   NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
  	   NOWAIT   ;
	   SAVE     ;
  	   TITLE (lcBrowTtl) ;
       WHEN lfwBrowvm1() ;
	  WINDOW Postyvm2 ;
	  IN WINDOW Postyvm
SELECT (lcSelect)	

*!*************************************************************
*! Name      : lfwBrowvm1
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : When Browse function.
*!*************************************************************
FUNCTION lfwBrowvm1

lnMarker = RECNO()
IF &lcPOLine..Style+&lcPOLine..Dyelot+STR(&lcPOLine..LineNo,6)<>m.Style+m.Dyelot+STR(m.LineNo,6)
  =lfGetLinVm()
ENDIF

IF lcAType = 'A'
  lcConfig = IIF(SEEK("AD" + &lcPOLine..PO + STR(&lcPOLine..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
ENDIF
SHOW GET pbConfig,1 PROMPT lcConfig 
SHOW GET PbClos ENABLE

*!*************************************************************
*! Name      : lfvStyleVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvStyleVm
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

m.Style=IIF(llBrowse,'?',m.Style)
SELECT STYLE
SET ORDER TO TAG StyQlty
lcQltyKey=laStyGrd[lnGrade,2]
lnItemSz   = gfItemMask('PI')

llBrowseEx = llbrowse
IF llBrowse OR SUBSTR(m.Style,1,1) <> SPACE(1)

  llbrowse = .F.
  IF  !SEEK(lcQltyKey+ALLTRIM(m.Style),'STYLE') .AND. !(gfGetMemVar('M_USEEXSSC'))
    SET FILTER TO cStyGrade=lcQltyKey
    GO TOP
    IF EOF()
      *--No styles found for this quality. 
      =gfModalGen('TRM42087B42000','DIALOG')
      m.Style = SPACE(19)
    ELSE
      lcHdEsc1= ON('KEY','ESC')
      ON KEY LABEL ESC 
      m.Style = gfStyBrw('I',m.Style,'',.F.)
      ON KEY LABEL ESC &lcHdEsc1
      =lfBrowLine()
    ENDIF 
    SELECT STYLE
    SET FILTER TO 
  ELSE
    SET FILTER TO cStyGrade=lcQltyKey
    GO TOP
    IF EOF()
      *--No styles found for this quality. 
      =gfModalGen('TRM42087B42000','DIALOG')
      m.Style = SPACE(19)
    ELSE
      lcHdEsc1= ON('KEY','ESC')
      ON KEY LABEL ESC 
      IF lcAType = 'D' .AND. m.STYLE # STYLE.STYLE .AND. ;
           LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz) - LEN(laMajSeg[lnColStru,3]) 
        IF laData[72]#'*' .AND. SEEK(lcQltyKey+ALLTRIM(m.STYLE)+laData[72],'STYLE')
          m.STYLE = ALLTRIM(m.STYLE)+laData[72]
        ELSE
          lcNonMaj = '?'
          lcNonMaj = gfStyBrw('N', LEFT(ALLTRIM(m.STYLE),lnMjrWid),lcNonMaj,.F.)
          IF !EMPTY(lcNonMaj)
            m.Style = ALLTRIM(m.STYLE)+lcNonMaj
          ELSE
            m.Style = SPACE(19)
          ENDIF
        ENDIF
      ELSE
        m.Style = gfStyBrw('I',m.Style,'',.F.)
      ENDIF
       
      ON KEY LABEL ESC &lcHdEsc1
      =lfBrowLine()
      SELECT STYLE
      SET FILTER TO 
    ENDIF
  ENDIF
ENDIF
SET ORDER TO TAG Style
lcStyDesc = IIF(!EMPTY(m.Style) AND !EOF('STYLE'),STYLE.Desc1,'')
SHOW GET lcStyDesc

SELECT POSHDR
IF SUBSTR(m.Style,1,1) = SPACE(1)
  RETURN
ENDIF

llAbort =.F.
*-- Validation checks loop for Style.....
DO WHILE .T.

  IF IIF(lcAType$'DA' .AND. ASCAN(laEvntTrig , PADR('DOSCRN',10)) = 0,STYLE.Make,!STYLE.Make)
    STORE 0 TO m.nCost1,m.nCost2,m.nCost3,m.nCost4,m.nCost5,m.NGPRICE
    IF lcAType = 'D' .AND. !EMPTY(laData[72]) .AND. laData[72]#'*' AND lnColStru # 0 .AND. ;
              SUBSTR(STYLE.STYLE,laMajSeg[lnColStru,4],LEN(laMajSeg[lnColStru,3])) # laData[72]
      *C200080,1 AMM Message : "Style color doesn't match dye color, cannot accept" 
      =gfModalGen('TRM38174B00000','DIALOG')
      llAbort=.T.
      EXIT
    ENDIF

    IF lcAType$'DP'
      IF !llOpnBom
        llOpnBom = lfOpn_Rest(gcDataDir+'BOM','BOM')
      ENDIF  

      IF !(laData[3]$'HB')
        IF SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
          SELECT BOM 
          LOCATE REST WHILE cItmMajor=SUBSTR(m.Style,1,lnMjrWid) ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(m.Style,19))
          IF !FOUND()
            *-No cost lines found in the cost sheet, Cannot proceed!
            =gfModalGen('TRM34037B34000','DIALOG')        
            llAbort=.T.
            SELECT POSHDR
            EXIT
          ENDIF
        ELSE
          *-No cost lines found in the cost sheet, Cannot proceed!
          =gfModalGen('TRM34037B34000','DIALOG')        
          llAbort=.T.
          EXIT
        ENDIF
      ELSE 
        IF !SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
          *C200080,1 AMM If the style has no cost sheet, don't continue in case of dye order
          *-Warrning ! This style has no cost sheet.
          *=gfModalGen('INM34038B34000','DIALOG')        
          IF lcAType = 'D' 
            *-No cost lines found in the cost sheet, Cannot proceed!
            =gfModalGen('TRM34037B00000','DIALOG')        
            llAbort=.T.
            EXIT
          ELSE
            =gfModalGen('INM34038B34000','DIALOG')        
          ENDIF
        ENDIF 
      ENDIF
    ENDIF
   
  ELSE 
    IF lcAType $ 'DA' .AND. ASCAN(laEvntTrig , PADR('DOSCRN',10)) = 0
      =gfModalGen('TRM38015B34000','DIALOG','manufactured styles')
    ELSE
      =gfModalGen('TRM34039B34000','DIALOG')
    ENDIF
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

  IF lcAType = 'D'
    SELECT BOM
    =SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
    LOCATE FOR mfgcode=lcDyeOpr WHILE citmmajor=SUBSTR(m.Style,1,lnMjrWid)
    IF !FOUND()
      *C200080,1 AMM Message : 'Style cost sheet has no dye operation ['+lcDyeOpr+'], Cannot accept this style in Dye order.'
      =gfModalGen('TRM38177B00000','DIALOG',lcDyeOpr)
      *C200080,1 AMM end
      llAbort=.T.
      EXIT
    ENDIF
    lcDyeOpNo = BOM.TYP
  ENDIF

  IF lcAType='C' AND lfVldSCnr()
    *-The Style: xxx valid in another contract for the same vendor and period, Cannot proceed!
    =gfModalGen('TRM34018B34000','DIALOG',m.Style)
    llAbort=.T.
    EXIT
  ENDIF

  IF lcAType$'NA' AND !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+SPACE(10),'STYDYE')

    IF llPosSetN
      *-Style: xxx is not assigned to location: xxx. "ok"
      =gfModalGen('TRM34048B34000','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(laData[2]))
      llAbort=.T.
      EXIT
    ELSE    
    
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(laData[2])) = 1
        DO gpAdStyWar WITH m.Style,SPACE(10),laData[2]
      ELSE
        llAbort=.T.
        EXIT
      ENDIF
    ENDIF
  ENDIF

  IF !llMultiWare
    *--If ship to customer the location will be the ship drop location.
    m.cWareCode = IIF(lnShpLoc = 2,laData[69],laData[19])
    lnLShpLoc   = lnShpLoc
    
    m.Account   = IIF(lnShpLoc = 2,laData[69],'')
    
    m.Store     = laData[70]

    IF lnShpLoc = 1
      IF !SEEK(PADR(m.Style,19)+PADR(m.cWareCode,6)+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+m.cWareCode) = 1
          DO gpAdStyWar WITH m.Style,SPACE(10),m.cWareCode
        ELSE
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF
    
    ELSE  && lnShpLoc = 2
      IF !SEEK(PADR(m.Style,19)+lcDropLoc+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+lcDropLoc) = 1
          DO gpAdStyWar WITH m.Style,SPACE(10),lcDropLoc
        ELSE
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF

    ENDIF
  ELSE
    IF llWareHous
      *E301291,1 AMM Consider the case of adornment order
      m.cWareCode = IIF(lcAType$'NA' AND PADR(laData[2],6)=STYLE.CDefWare,SPACE(6),STYLE.CDefWare)
    ELSE  
      lnLShpLoc = 2
      m.cWareCode = SPACE(5)
    ENDIF
  ENDIF

  EXIT
ENDDO

IF llAbort
  m.Style   = SPACE(lnStyleWid)
  lcStyDesc = SPACE(0)
  =lfRefresh('PoStyVm2')
  SHOW GETS WINDOW PoStyVm2 DISABLE ONLY
  SHOW GET pbNew ENABLE
  _CUROBJ = OBJNUM(pbNew)
  RETURN
ENDIF

SELECT (lcPOLine)
NewLine = .T.
*--Donnot check existance of the style if Inter Location P/o and multi shipto 
*--location ,becouse normally is to send same style to different locations.
IF !(lcAType$'NA' AND llMultiWare)
  IF SEEK(m.Style)
    *-Warning: This Style has been entered on this '+lcPOType+'!
    =gfModalGen('INM34042B34000','DIALOG',lcPOType)        
  ENDIF
ENDIF

SELECT (lcPOLine)
m.cStyType = lcAType
m.Trancd = '1'
lnLines  = lnLines + 1
m.LineNo = lnLines
m.Po     = laData[1]
m.Vendor = laData[2]
m.Scale  = Style.Scale

*HDM Make it hold buy prepack as default ==>m.PrePak = Style.PrePak
m.PrePak = Style.CbuyPrePk
m.cVenSty= Style.cVenSty

IF lcAtype='C' .OR. !lfFrmCont()
  *E301291,1 AMM Consider the case of adornment order
  IF !(lcAType $ 'AD')
    m.nCost1 = IIF(!llMulCurr OR STYLE.cPriceCur=laData[26],Style.nICost1,0)
    m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
    m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
    m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
    m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)
    IF !llMulCurr OR STYLE.cPriceCur=laData[26]
      m.Disc_Pcnt  = STYLE.Disc_Pcnt
      m.NGPRICE = IIF(STYLE.Gros_Price=0 AND STYLE.Disc_Pcnt=0,m.nCost1,STYLE.Gros_Price)
    ELSE
      m.Disc_Pcnt  = 0
      m.NGPRICE = m.nCost1
    ENDIF
    *B602968,1 End.
  ELSE
    m.Disc_Pcnt  = 0
    m.NGPRICE = m.nCost1

    IF lcAType='A'
      m.nCost1 = Style.nMCost1
      STORE 0 TO m.nCost2,m.nCost3,m.nCost4,m.nCost5
    ELSE  && lcAType='D'
      FOR lnI = 1 TO 5
        lcI = STR(lnI,1)
        m.nCost&lcI = IIF(lcDyeOpNo = lcI AND (STYLE.nStyWeight * laData[73])<>0 ,(STYLE.nStyWeight * laData[73]),STYLE.nMCost&lcI )
      ENDFOR
    ENDIF
  ENDIF

  llConCst=.F.  
ELSE
  llConCst=.T.  
ENDIF

IF lcAType = 'A' 
  IF SEEK(STYLE.Style+laData[2]+SPACE(10),'STYDYE') 
    m.NGPRICE = IIF(llAvgCost,;
                 IIF(llWareHous,StyDye.Ave_Cost,Style.Ave_Cost),Style.TotCost)
  ELSE
    m.NGPRICE  = IIF(llAvgCost, Style.Ave_Cost,Style.TotCost)
  ENDIF
  m.Disc_Pcnt  = 0
  m.nCost1     = m.NGPRICE
ENDIF


*--Read equivalent costs.
m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

INSERT INTO (lcPOLine) FROM MEMVAR
REPLACE &lcPOLine..lFlag WITH llConCst


=lfGetLinVm()

IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N'))

  SELECT STYLE
  SKIP 1
  lcNexSty = STYLE.STYLE
  SKIP -1
  SELECT (lcPOLine) 
ENDIF

= lfAdjDye()
RETURN
*-- end of lfvStyle.


*!*************************************************************
*! Name      : lfGetLinVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate the line information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGetLinVm
SELECT (lcPOLine)
SCATTER MEMVAR
*lcStyDesc = IIF(SEEK(m.Style,'STYLE'),STYLE.Desc1,SPACE(20))
llFoundScl=SEEK('S'+m.Scale,'Scale')
lcSz1=IIF(llFoundScl,SCALE.SZ1,'')
lcSz2=IIF(llFoundScl,SCALE.SZ2,'')
lcSz3=IIF(llFoundScl,SCALE.SZ3,'')
lcSz4=IIF(llFoundScl,SCALE.SZ4,'')
lcSz5=IIF(llFoundScl,SCALE.SZ5,'')
lcSz6=IIF(llFoundScl,SCALE.SZ6,'')
lcSz7=IIF(llFoundScl,SCALE.SZ7,'')
lcSz8=IIF(llFoundScl,SCALE.SZ8,'')
m.ngprice = m.GROS_PRICE
lnLShpLoc=IIF(!EMPTY(m.Account),2,1)
IF ( laScrMode[3] OR laScrMode[4] ) AND lnActFolder=2
  IF lfCkEOF(lcPoLine) OR llZoom
    SHOW GETS WINDOW PoStyVm22 DISABLE ONLY
    IF !llZoom AND laData[3]<>'C'
      SHOW GET PBNEW ENABLE
    ENDIF
  ELSE
    IF laData[3]<>'C'
      SHOW GETS WINDOW PoStyVm22 ENABLE ONLY
    ELSE
      SHOW GETS WINDOW PoStyVm22 DISABLE ONLY  
    ENDIF
    IF ! llMultiWare
      SHOW GET lnLShpLoc   DISABLE
      SHOW GET pbLShpTo    DISABLE
      SHOW GET m.cWareCode DISABLE      
      SHOW GET m.Store     DISABLE      
    ENDIF
  ENDIF
  SHOW GET m.Style DISABLE
  SHOW GET ibStyle DISABLE

ELSE
  SHOW GETS WINDOW PoStyVm22 DISABLE ONLY
ENDIF
IF llImpCost AND lcAType='P' AND ( (laScrMode[3] AND laData[3]='H') OR laScrMode[4] )
  lcLnShMod = IIF(lfCkEOF(lcPoLine),'DISABLE','ENABLE')
  SHOW GET pbCTGnLns &lcLnShMod  
ENDIF
SHOW GET pbKey ENABLE
SHOW GET PbClos ENABLE
lnMarker = RECNO()
=lfRefresh('PoStyVm22')
 SHOW GETS WINDOW PoStyVm22

SHOW WINDOW PoStyVm2 REFRESH SAME

*!*************************************************************
*! Name      : lfvBrows
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Valid Browse function.
*!*************************************************************
FUNCTION lfvclose
Clear Read


*!*************************************************************
*! Name      :  lfvPiecsVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate Style pieces
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*
FUNCTION lfvPiecsVm

PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.TotQty = EVAL(VARREAD())
ENDIF

IF !EMPTY(m.Prepak) AND m.TotQty<>0 AND SEEK('P'+m.Scale+m.Prepak,'Scale')
  lnAlias = SELECT()
  SELECT SCALE
  lnPPakQ=Scale.PPTot
  SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
  =SEEK('S'+m.Scale)
  SELECT (lnAlias)
  
  IF MOD(m.TotQty,lnPPakQ) <> 0 
    *-Total qty is not evenly divisible by the prepak qty 9999.
    =gfModalGen('TRM34044B34000','DIALOG',STR(lnPPakQ,4) )
    _CUROBJ = OBJNUM(m.Prepak)
    RETURN
  ENDIF

  FOR I=1 TO 8
    lcI=STR(I,1)
    m.Qty&lcI = m.TotQty * ( laPPq[I] / lnPPakQ )
    *E301274,1 AMM start, Update table
    IF llFrmGBrw
      REPLACE Qty&lcI   WITH m.Qty&lcI,;
              cQty&lcI  WITH m.cQty&lcI
    ENDIF
    IF (!llFrmGBrw .AND. !lfvSzQtyVm(I)) .OR. (llFrmGBrw .AND. !lfexSzQty(I) )    
      _CUROBJ = OBJNUM(m.TotQty)
      RETURN .F.
    ENDIF 
  ENDFOR 

ELSE

  *-- if scale has one size.
  IF Scale.Cnt = 1
    m.Qty1 = m.TotQty
    IF llFrmGBrw
      =lfexSzQty(1)
    ELSE
      =lfvSzQtyVm(1) AND gfUpdate()
    ENDIF
    
  ENDIF  && end if scale has one size.

ENDIF
=SEEK('S'+m.Scale,'Scale')
SHOW GETS WINDOW PoStyVm22 ONLY
SHOW WINDOW (lcBrowTtl) REFRESH SAME
FLUSH

*!*************************************************************
*! Name      : lfvSzQtyVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate Style size quantity
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvSzQtyVm
PARAMETERS lnSize
lcSz = STR(lnSize,1)


*-Cannot accept -ve quantity.
IF m.Qty&lcSz<0
  =gfModalGen('TRM34051B34000','DIALOG')
  m.Qty&lcSz = &lcPOLine..Qty&lcSz
  SHOW GET m.Qty&lcSz
  RETURN .F.
ENDIF

IF lcAtype = "N" AND laScrMode[3] .AND. m.Qty&lcSz <> &lcPOLine..Qty&lcSz;
                 .AND. ASCAN(laEvntTrig , PADR('EDITSIZE',10)) <> 0
  PRIVATE llNotEdit
  llNotEdit = .F.
  =gfDoTriger('POINTRC',PADR('EDITSIZE',10))
  IF llNotEdit
    m.Qty&lcSz = &lcPOLine..Qty&lcSz
    SHOW GET m.Qty&lcSz
    RETURN .F.
  ENDIF
ENDIF

IF m.Qty&lcSz = &lcPOLine..Qty&lcSz
  IF lnSize = Scale.Cnt 
    
    IF m.TotQty <> &lcPOLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,10))) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
      _CUROBJ = OBJNUM(m.Prepak)
    ELSE
      m.TotQty = &lcPOLine..TotQty
      SHOW GET m.TotQty
      IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N')) AND ;
           lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'

         _CUROBJ = OBJNUM(pbnew)
         KEYBOARD CHR(13) CLEAR
         
      ENDIF
    ENDIF
  ENDIF
  RETURN
ENDIF

IF laScrMode[3]
  lnAlias = SELECT()
  lnRecQt = 0
   IF  SEEK(lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6),'POSLN') 
    SELECT POSLN
    lnSvRec = RECNO()
    SCAN WHILE cStyType+Po+Style+STR(Lineno,6)=lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6) FOR Trancd $ '2456' 
      lnRecQt = lnRecQt + Qty&lcSz
    ENDSCAN
    GO lnSvRec
  ENDIF  
  SELECT (lnAlias)

  *-Quantity cannot be less than the received quantity.
  IF m.Qty&lcSz < lnRecQt
    =gfModalGen('TRM34043B34000','DIALOG')        
    m.Qty&lcSz = &lcPOLine..Qty&lcSz
    SHOW GET m.Qty&lcSz
    RETURN .F.
  ENDIF

  IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH'
    llFound = SEEK('P'+laData[1]+&lcPOline..Style+STR(&lcPOline..LineNo,6),'POSLN')
    llFound = llFound AND ( m.Qty&lcSz  < POSLN.Ord&lcSz )
    llFound = llFound AND SEEK('2'+laData[1]+&lcPOline..Style,'CutPick')
    IF llFound AND !lfChkOrdQt(lcSz)
      m.Qty&lcSz = &lcPOLine..Qty&lcSz
      SHOW GET m.Qty&lcSz
      _CUROBJ=_CUROBJ
      RETURN
    ELSE
      SHOW GET m.Qty&lcSz
    ENDIF
  ENDIF
ENDIF

laData[36] = laData[36] - &lcPOLine..Qty&lcSz + m.Qty&lcSz
laData[40] = laData[40] - &lcPOLine..Qty&lcSz + m.Qty&lcSz
llRecalTot = .T.

SELECT (lcPOHdr)
REPLACE nStyOrder WITH laData[36],;
        Open      WITH laData[40]
SELECT (lcPOLine)
REPLACE Qty&lcSz WITH m.Qty&lcSz ,;
        TotQty   WITh Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 

IF lnSize = Scale.Cnt 
  IF m.TotQty <> &lcPOLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,10))) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
    _CUROBJ = OBJNUM(m.Prepak)
  ELSE
    m.TotQty = &lcPOLine..TotQty
    SHOW GET m.TotQty
  ENDIF
ENDIF
SHOW WINDOW (lcBrowTtl) REFRESH SAME

IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N')) AND ;
   lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'  AND m.TotQty = &lcPOLine..TotQty
  _CUROBJ = OBJNUM(pbnew)
  KEYBOARD CHR(13) CLEAR
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvPPQtyvm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate Style prepak quantity
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvPPQtyvm

PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.PPQty = VAL( EVAL(VARREAD()))
ENDIF

IF (!llFrmGBrw .AND. m.PPQty = EVAL(lcPOLine+'.PPQty') AND m.Prepak <> EVAL(lcPOLine+'.Prepak')) ;
     .OR. (llFrmGBrw .AND. m.PPQty = VAL(lcOldVal))
  RETURN
ENDIF

IF !EMPTY(m.Prepak) AND m.PPQty <> 0 AND SEEK('P'+m.Scale+m.Prepak,'Scale')
  lnAlias = SELECT()
  SELECT SCALE
  lnPPakQ=Scale.PPTot
  SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
  =SEEK('S'+m.Scale)
  SELECT (lnAlias)
  m.TotQty  = m.PPQty * lnPPakQ
  FOR I=1 TO 8
    lcI=STR(I,1)
    m.Qty&lcI = m.PPQty * laPPq[I]
    IF (!llFrmGBrw .AND. !lfvSzQtyVm(I)) .OR. (llFrmGBrw .AND. !lfexSzQty(I) )
      _CUROBJ = OBJNUM(m.TotQty)
      RETURN .F.
    ENDIF 
  ENDFOR 
 
  SHOW GET m.TotQty
ENDIF
=SEEK('S'+m.Scale,'Scale')
IF !llFrmGBrw

  SELECT (lcPOLine)
  REPLACE PPQty WITH m.PPQty
  SHOW GETS WINDOW PoStyVm22 ONLY
  SHOW WINDOW (lcBrowTtl) REFRESH SAME
  =lfRefresh('PoStyVm22')
  FLUSH
ELSE
  REPLACE TotQty    WITH m.TotQty,;
          cTotQty   WITH STR(m.TotQty,7),;
          cStatus   WITH IIF(cStatus='N','N','M')
ENDIF
RETURN


*!*************************************************************
*! Name      : FUNCTION lfvPrDscVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate Style price discount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrDscVm()
*!*************************************************************
*
FUNCTION lfvPrDscVm
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.Disc_Pcnt = EVAL(VARREAD())
ENDIF

IF IIF(llFrmGBrw, m.Disc_Pcnt # lcOldVal, m.Disc_Pcnt <> EVAL(lcPOLine+'.Disc_Pcnt'))
  m.nCost1 = ROUND(m.NGPRICE*(100-m.Disc_Pcnt)/100,3)
  IF llFrmGBrw
    REPLACE nCost1 WITH m.nCost1
  ENDIF
  =lfvCostVm('1',.T.,llFrmGBrw)
ENDIF
*--
*!*************************************************************
*! Name      : lfvCostVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Valid Cost elements.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*
FUNCTION lfvCostVm

PARA lcCstNo,llPercent,llFrmGBrw
IF IIF(llFrmGBrw ,EVAL(lcTmpSize+'.nCost'+lcCstNo) ,EVAL(LCPOLINE+'.nCost'+lcCstNo))<> m.nCost&lcCstNo
  lnAlias = SELECT()
  SELECT IIF(llFrmGBrw,lcTmpSize,lcPOLine)
  IF llFrmGBrw
    lnOldVal = m.nCost&lcCstNo.
    lnOldEVal= m.nECost&lcCstNo.
    SCATTER MEMVAR MEMO
  ELSE
    lnOldVal =nCost&lcCstNo
    lnOldEVal=nECost&lcCstNo
  ENDIF
  m.nECost&lcCstNo=lfvEquCost(lcCstNo,m.nCost&lcCstNo,IIF(lcCstNo='1',laData[28],laData[29]),IIF(lcCstNo='1',lnCurrUnt1,lnCurrUnt2))
 
  REPLACE nCost&lcCstNo  WITH nCost&lcCstNo  + m.nCost&lcCstNo  - lnOldVal
  REPLACE nECost&lcCstNo WITH nECost&lcCstNo + m.nECost&lcCstNo - lnOldEVal
  FLUSH

  IF lcCstNo = '1'
    IF llPercent OR ;
       (m.NGPRICE<>0 AND ROUND((m.NGPRICE*(1-m.Disc_Pcnt/100)),3) = m.nCost1)
      m.Disc_Pcnt = m.Disc_Pcnt
    ELSE
      IF !llPercent AND VARREAD() <> "GROS_PRICE"
        m.Disc_Pcnt = MAX(IIF(m.NGPRICE=0,0,100-m.nCost1*100/m.NGPRICE),0)
        IF m.Disc_Pcnt = 0
          m.NGPRICE = m.nCost1
        ENDIF  
      ENDIF  
    ENDIF
    IF m.NGPRICE = 0 AND m.Disc_Pcnt=0
      m.NGPRICE = m.nCost1
    ENDIF

    REPLACE Gros_Price WITH m.NGPRICE ,;
            Disc_Pcnt  WITH m.Disc_Pcnt
    IF llFrmGBrw
      REPLACE cGros_Price WITH STR(m.NGPRICE,9,2),;
              cDisc_Pcnt  WITH STR(m.Disc_Pcnt,5,2),;
              nCost1      WITH m.nCost1            ,;
              cNcost1     WITH STR(m.nCost1,10,2),;
              cStatus     WITH IIF(cStatus='N','N','M')
              
    ENDIF
    
    SHOW GET m.NGPRICE
    SHOW GET m.Disc_Pcnt
    SHOW GET m.nCost1
  
  ENDIF  
   IF lcAType = 'P'  .AND. llDispPric
     =lfvSelPrice(.T.)
   ENDIF 
  SHOW GET m.nECost&lcCstNo
  SHOW WINDOW (lcBrowTtl) REFRESH SAME
  llRecalTot = .T.
  SELECT(lnAlias)
ENDIF  


*!*************************************************************
*! Name      : lfvRemovVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Remove P/O line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvRemovVm

llCantDele = .F.
lnAlias    = SELECT()
DO WHILE laScrMode[3] 
  IF !( laData[3] $ 'OHB' )
    *-Only open,hold or bid +lcPOType+ available to remove the line, cannot delete!
    =gfModalGen('TRM34032B34000','DIALOG',lcPOType )
    llCantDele = .T.
    EXIT
  ENDIF

  IF lcAtype = "N" .AND. ASCAN(laEvntTrig , PADR('REMLINE',10)) <> 0
    =gfDoTriger('POINTRC',PADR('REMLINE',10))
    IF llCantDele
      EXIT
    ENDIF
  ENDIF

  lcLineKey=lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6)
  SELECT POSLN
  IF SEEK(lcLineKey+'2') OR SEEK(lcLineKey+'4') OR SEEK(lcLineKey+'5')
    *-Style for this line not allowed to delete. Shipment has been received.
    lcTxtMsg = IIF(POSLN.TranCd='6',' There is issued quantity',' Shipment has been received')
    =gfModalGen('TRM34033B34000','DIALOG',lcTxtMsg)
    llCantDele = .T.
    EXIT
  ENDIF 

  IF lcAType $ 'NA' AND SEEK(lcLineKey+'6')
    *--This line has issued quantity, deleting this line will Reverse issued P/O line stock.
    *--Are you sure you want to delete? Yes/No.
    IF gfModalGen('INM34168B34001','DIALOG') = 2
      llCantDele = .T.
      EXIT
    ENDIF
  ENDIF

  IF SEEK(lcLineKey+'3')
    *-Style for this line not allowed to delete. Shipment in transit.
    =gfModalGen('TRM34033B34000','DIALOG',' Shipment in transit')
    llCantDele = .T.
    EXIT
  ENDIF
  IF lfStyInBom(&lcPOLine..Lineno,laData[1])
    *-Receiving quantities have been found against this style. Cannot delete.
    =gfModalGen('TRM34034B34000','DIALOG')
    llCantDele = .T.
  ENDIF
  EXIT
ENDDO

*--If cannot delete this line.
IF llCantDele
  SELECT(lnAlias)  
  RETURN
ENDIF


*-Since this line has allocated quantities from order(s),
*-deleting it will release the order(s) allocation.
*-Are you sure you want to delete this line?
IF laScrMode[3] AND lcAType='P' AND SEEK('2'+laData[1]+&lcPOLine..Style,'CUTPICK')
  IF gfModalGen('QRM34035B34001','DIALOG') = 2
    RETURN 
  ENDIF
ELSE
*-Are you sure you want to delete this line?
  IF gfModalGen('QRM34036B34001','DIALOG') = 2
    RETURN
  ENDIF
ENDIF

laData[36] = laData[36] - m.TotQty
laData[40] = laData[40] - m.TotQty
llRecalTot = .T.
SELECT (lcPOLine)

PRIVATE lnRcno
lnRcno = 0
SKIP -1
IF BOF()
  GOTO TOP  
ELSE
  lnRcno = RECNO()
  SKIP
ENDIF

BLANK
DELETE

IF lnRcno = 0  
  GOTO TOP
ELSE
  GOTO (lnRcno)
ENDIF  

IF lcAType='R' AND EOF()
  laData[71] = SPACE(6)
ENDIF  

IF lcAType = 'A'
  
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    *-- Do nothing validation is in JLMAIN
  ELSE
    SELECT (lcT_BomVar)
    =SEEK("AD"+laData[1]+STR(&lcPOLine..LineNo,6))
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
      REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    ENDSCAN
    DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
  ENDIF     

ENDIF

=lfBrowLine()    
=lfGetLinVm()
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields
SELECT (lcPOLine)
RETURN

*!*************************************************************
*! Name      : lfvPrPckVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate Style prepak
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvPrPckVm

PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.Prepak = EVAL(VARREAD())
ENDIF

PRIVATE lcBrFields,lcFile_Ttl

IF (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(m.Scale,@m.Prepak)
  =SEEK('S'+m.Scale,'Scale')
ENDIF

IF llFrmGBrw
  IF PrePak # lcOldVal
    REPLACE Prepak    WITH m.Prepak,;
            cStatus   WITH IIF(cStatus='N','N','M')
    m.PPQTY = 0
  ENDIF
ELSE

  IF &lcPOLine..Prepak <> m.Prepak
    REPLACE &lcPOLine..Prepak WITH m.Prepak
    IF EMPTY(&lcPOLine..Prepak)
      REPLACE &lcPOLine..PPQty WITH 0
      m.PPQTY = 0
      SHOW GET m.PPQty
    ELSE
      SHOW GET m.Prepak ENABLE
   ENDIF
   SHOW GETS WINDOW PoStyVm22 ONLY
   SHOW WINDOW (lcBrowTtl) REFRESH SAME
   FLUSH
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvNewVm
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : New po line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvNewVm

IF EMPTY(laData[2])
  *-You have to enter the xxx code.
  =gfModalGen('TRM34020B34000','DIALOG',IIF(lcAType$'NA','source location','vendor')+'|'+' ')
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

IF !llMultiWare AND EMPTY(laData[19]) AND EMPTY(laData[69])
  *-You have to enter the xxx code.
  =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
  _CUROBJ = IIF(lnShpLoc=1,OBJNUM(laData[19]),OBJNUM(laData[69]))
  RETURN
ENDIF

SELECT (lcPOLine)
lcLstSty = m.Style
SCATTER MEMVAR BLANK
IF lcAType='N'
  m.Style = lcLstSty
ENDIF

IF lcAType = 'A'
  lcConfig = "\<Configure"
ENDIF
SHOW GET pbConfig,1 PROMPT lcConfig 

STORE ' ' TO lcStyDesc
SHOW GETS WINDOW PoStyVm22 DISABLE ONLY
SHOW GET ibStyle ENABLE
SHOW GET m.Style ENABLE

=lfRefresh('PoStyVm22')

IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N'))
  m.Style = lcNexSty
  =SEEK(m.Style , 'STYLE')
ENDIF
_CUROBJ = OBJNUM(m.Style)
RETURN

*!*************************************************************
*! Name      : lfvAccount
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate account
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount


*B121218,1 MHM 03/01/2004 [Start]
*IF !EMPTY(m.account) .AND. !SEEK('M'+m.account,'CUSTOMER')
*  xAccount = m.account

*!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
*IF !EMPTY(m.cAccCode) .AND. !SEEK('M'+m.cAccCode,'CUSTOMER')
  *xAccount = m.cAccCode
IF !EMPTY(m.cVanCode) .AND. !SEEK('M'+m.cVanCode,'CUSTOMER')  
  xAccount = m.cVanCode
*!B126157,1 MHM [End]

*B121218,1 MHM 03/01/2004 [End]

  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  
  *B121218,1 MHM 03/01/2004 [Start]
  *m.account = xAccount

  *!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
  *m.cAccCode = xAccount
  m.cVanCode = xAccount
  *!B126157,1 MHM [End]
  
ENDIF

*B121218,1 MHM 03/01/2004 [Start]
*REPLACE &LCPOLINE..Account WITH m.Account

*!B126157,1 MHM 02/17/2005 Change Account 'cAccCode' [Start]
*REPLACE &LCPOLINE..cAccCode  WITH m.cAccCode 
REPLACE &LCPOLINE..cVanCode  WITH m.cVanCode 
*!B126157,1 MHM [End]

*B121218,1 MHM 03/01/2004 [End]

*!*************************************************************
*! Name      : lfvGPrice
*: Developer : MHM - MOHAMED SHOKRY
*: Date      : 09/11/2003
*! Purpose   : Validate G price
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvGPricVM

PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.NGPRICE = EVAL(VARREAD())
  *--- SSH
ENDIF
llUpdPoPr   = IIF(gfGetMemVar('M_UPDPOPR') = 'Y',.T.,.F.)
IF llUpdPoPr AND STYLE.cPriceCur = laData[26] AND (STYLE.NICOST1 <> 0) AND (m.NGPRICE <> STYLE.NICOST1)
  IF gfModalGen('QRM34183B34001','DIALOG',STR(STYLE.NICOST1,2)) = 2
    m.NGPRICE = STYLE.NICOST1
  ENDIF
ENDIF

IF lcAType='D'
  m.Disc_Pcnt = 0
ENDIF


IF (!llFrmGBrw .AND. m.NGPRICE <> EVAL(lcPOLine+'.Gros_Price')) ;
    .OR. (llFrmGBrw .AND. m.NGPRICE # lcOldVal)

  m.nCost1 = ROUND(m.NGPRICE*(100-m.Disc_Pcnt)/100,3)
  IF llFrmGBrw
    REPLACE nCost1 WITH m.nCost1 
  ENDIF

  =lfvCostVm('1',.F.,llFrmGBrw)
ENDIF

REPLACE &LCPOLINE..ngPrice WITH m.ngPrice

