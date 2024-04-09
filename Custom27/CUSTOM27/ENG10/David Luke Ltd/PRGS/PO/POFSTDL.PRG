*!****************************************************************
*! Program file  : POFSTDL.PRG
*! Program desc. : FORECAST program
*! For screen    : POFSTDL.SPX
*! System        : Aria Apparel System - Version 2.7.
*! Module        : Style Purchased Order (PO)
*! Developer     : Mostafa Rawash (MMR)
*! Date          : 01/06/2006
*!****************************************************************
*! Passed Parameters  : None.
*!****************************************************************
*!C#130192,1.
*!****************************************************************
*!Modifications  :
*:C130192,1 MMR 03/12/2006 Fix the bug of not saving some styles
*:C130192,2 MMR 04/11/2006 Fix the bug of not collecting some sales and purchase
*:C130192,2 MMR            because of internal error in the ACopy() function.
*!****************************************************************

EXTERNAL ARRAY laData , laDefProc , laKeyField
DECLARE laShowFig[15,14],laAllData[137,1],laWareType[2] , laDataPrc[137,1] ,laShowSiz[1,4] ,laShowStk[1,4] 
STORE 0 TO I,lnRow,lnScaleCnt,lnCountr ,lnMonth,lnYear,lnCont  
STORE SPACE(0) TO lcAlias,X ,lcMnth,lcEnable ,lcDisable ,lcOld
STORE {} TO ldDate
dimension arrStruc[1] ,arrStock[1] ,arrMnthSal[15] ,arrMnthPur[15] ,arrMonth[15,3] ,larrEnable[15,4]
STORE ''  TO lcStyDesc, lcMjrPct, lcClrDesc, lcScFields , lcColStyle
STORE ''  TO lcNxt , lcPrv ,lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS ,lcEnblePrc
STORE 0   TO lnYear , lnMonth , lnPurAdjPc , lnSalAdjPc , lnTSalFcst , lnTPurFcst
STORE 0   TO lnAvgCost , lnPriceA , lnSepMins , lnTBkAmt , lnTPosMnth , lnSetDeci
STORE 0   TO lnRowCnt  , lnPrStock
STORE .F. TO llbrowse , llMScale
STORE '' TO  lcMonth, lcYear 
STORE 0  TO laShowFig , laAllData , laDataPrC  ,laShowStk
STORE SPACE(0) TO laShowSiz ,larrEnable
laWareType[1] = 'Single'
laWareType[2] = 'Multiple'
llMScale      = gfGetMemVar('M_USEEXSSC')
STORE 2   TO lnScaleCnt
STORE 3   TO lnXPos
STORE 3   TO lnYPos
STORE 7   TO lnZPos           && the Movement Step From Month to another inside the array
lcBlckClr  = "RGB(0,0,0,255,255,255)"
lcBlkYello = "RGB(0,0,0,255,255,0)"

lcBluClr   =  "RGB(0,0,198,255,255,255)"
lcYellClr  =  "RGB(0,0,198,255,255,0)"

STORE ''  TO lcVendor,lcWareCode,lcPCurr,lcDCurr,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5
lcTmpFrCst = gfTempName()
lcTmpMonth = gfTempName()
lcTmpIndex = gfTempName()
lcTmpScale = gfTempName()
lcTmpMain  = gfTempName()
*-- Hold the number of term,ship,term
STORE  1  TO lnTerm,lnShip,lnType,puType
lnSetDeci = SET('DECIMALS')
SET DECIMALS TO
*--ToolBar external proc.
laDefProc[1]  = .F.             && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.             && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.             && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
laDefProc[10] = .F.             && Disable the control panel Close proc.(lpClsScr)
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[5]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
DIMENSION laTerm[1], laCodInfo [2,10] ,laShip[1]
STORE "" TO laTerm, laCodInfo, laShip

STORE .F. TO llAlowNew
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
*-- To get the Color position and length.
STORE 0 TO lnClrPos,lnClrLen
lcMjrPct  = gfItemMask('PM')
lnMjrWid  = LEN(lcMjrPct)
lnSepMins = lfvGtSclSp()           && the length of Scale with Separator.
=lfGetColor()
DECLARE laKeyField [2,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'STYLE'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'STYLE'
laKeyField[2,4] = 2


llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
llEditExRt = gfGetMemVar('LLEDITEXRA')
llMulCurr  = gfGetMemVar('llMulCurr')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')

*-- To get the include pattern No setup.
llUpdPatrn = gfGetMemVar('M_ADDPATNO')
lnSessNo = gnProgCopy
IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(lcBaseWind)
  lcYear  = STR(YEAR(gdSysDate),4,0)
  lcMonth = CMONTH(gdSysDate)

 *-- Create temp cursor to Browse styles for generate POs.
  CREATE TABLE (gcWorkDir+lcTmpFrCst) (cRow C(3) ,cDesc1 c(10) ,cDesc2 c(10) ,cDesc3 c(10) ,;
                cDesc4 c(10) ,Stok1 N(14,2) ,Stok2 N(14,2) ,Stok3 N(14,2) ,Stok4 N(14,2))
  INDEX ON cRow TAG (lcTmpFrCst) OF (lcTmpFrCst)
  SET ORDER TO
  CREATE TABLE (gcWorkDir+lcTmpMonth) (cRow C(3)    ,SF1 N(14,2)  ,PF1 N(14,2) ,;                             
                                       SF2 N(14,2)  ,PF2 N(14,2)               ,;
                                       SF3 N(14,2)  ,PF3 N(14,2)               ,; 
                                       SF4 N(14,2)  ,PF4 N(14,2)               ,;  
                                       nYear N(4,0) ,nMnth N(2,0) ,cMnth C(10))
  INDEX ON cRow TAG (lcTmpMonth) OF (lcTmpMonth)
  index on STR(nyear,4)+cMnth tag (lcTmpIndex) OF (lcTmpMonth)
  SET ORDER TO
  SELECT SCALE
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cStyle'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 19
  laFileStru[lnFileStru,4] = 0 
  CREATE CURSOR (lcTmpScale) FROM ARRAY laFileStru
  SELECT FORECAST
  =AFIELDS(laFileStr)  
  CREATE CURSOR (lcTmpMain) FROM ARRAY laFileStr
  *-- Screen fields variable, to be used to build the laData array.
  SELECT YEARINPO
  lcScFields ='STYLE,COLOR'

  SCATTER FIELDS &lcScFields. TO laData BLANK

ENDIF 
*--Activate Options pad.
=lfActPad()
DO (gcScrDir+gcWinAppl+"\POFSTDL.SPX")

IF glQuitting
  USE IN (lcTmpFrCst)
  USE IN (lcTmpMonth)
ENDIF

*--Release Option pad also
RELEASE PAD _Option OF _MSYSMENU
*-- End OF Code.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!C#122961,1
FUNCTION lfvStyle

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT STYLE
SET ORDER TO TAG CSTYLE

llbrowse = llbrowse OR '?' $ laData[1]
IF llbrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'STYLE'))
  IF !llbrowse .AND. !EMPTY(laData[1]) .AND. !SEEK(laData[1],'STYLE')
    *-- give user message. to browse or reenter.
    IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'Style : '+laData[1]+'is not found in the data file' ) = 2
      laData[1] = ''
      _CUROBJ = OBJNUM(laData[1])
      RETURN
    ENDIF
  ENDIF
  llbrowse = .F.
  laData[1] = gfStyBrw('M',laData[1],"",.F.)
ENDIF
SELECT STYLE
SET ORDER TO TAG STYLE

SELECT (lnAlias)
*-- End OF lfvStyle.

*!*************************************************************
*! Name      : lfvColor
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Valid function to validate Color.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!C#122961,1
FUNCTION lfvColor
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT STYLE
llbrowse = llbrowse OR '?' $ laData[2]

laData[2] = IIF(llbrowse,'?',laData[2])

IF EMPTY(laData[2])
  RETURN
ENDIF

IF llBrowse .OR. !MDOWN()
  llCanBrow = .T.
  IF !EMPTY(laData[1]) .AND. SEEK(PADR(laData[1],lnMjrWid),'STYLE')
    LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                  FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
    IF FOUND()
      llCanBrow = .F.
    ENDIF
  ELSE
    LOCATE
  ENDIF
  
  IF llCanBrow
    IF !EMPTY(laData[1])
      lcItemCd  = gfStyBrw('N',PADR(laData[1],lnMjrWid),laData[2],.F.)
      laData[2] = SUBSTR(lcItemCd ,lnClrPos-lnMjrWid-1,lnClrLen)
    ELSE
      lcItemCd = gfStyBrw('I','','',.F.)
      laData[1]  = SUBSTR(lcItemCd ,1,lnMjrWid)
      laData[2]  = SUBSTR(lcItemCd ,lnClrPos,lnClrLen)
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[2])
  lcStyDesc  = Style.DESC1
  lcClrDesc  = gfCodDes(laData[2], 'COLOR     ')
  laData[1]  = PADR(laData[1],lnMjrWid)
  laScrMode   = .F.
  laScrMode[2]= .T.
  SHOW GETS
  SHOW GET laData[1] DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET ibStyle   DISABLE
  SHOW GET ibColor   DISABLE
ENDIF
=lfRefresh()
llBrowse = .F.
SELECT(lnAlias)
RETURN
*-- End OF lfvColor.

*!*************************************************************
*! Name      : lpShow
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Show function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*!C#122961,1
FUNCTION lpShow
PRIVATE lcOldAlis
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE ''         TO laData[1] , laData[2] , lcStyDesc , lcClrDesc
    STORE 'DISABLE'  TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
    STORE SPACE(0)   TO laAllData , laDataPrC ,laShowSiz
    STORE 0 TO laShowStk ,laShowFig ,lnPurAdjPc ,arrStock,arrMnthSal ,arrMnthPur ,arrMonth
    STORE 0 TO lnPurAdjPc
    SHOW GET ibStyle    ENABLE
    SHOW GET ibColor    ENABLE
    
    =lfRefresh()
  
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
    lcStyDesc  = Style.DESC1
    lcClrDesc  = gfCodDes(laData[2], 'COLOR     ')
    *-- recollect the data again.
       STORE SPACE(0) TO laShowSiz
       STORE 0 TO laShowStk ,laShowFig ,lnPurAdjPc
       dimension arrStruc[1] ,arrStock[1] ,arrMnthSal[15] ,arrMnthPur[15] ,arrMonth[15,3] ,larrEnable[15,4]
       STORE 0 TO arrStock,arrMnthSal ,arrMnthPur ,arrMonth 
       =lfCollect()
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET lcStyDesc
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
    *--mhm2006 this part always change the color if you go with mover so we commnet it
    *lnCont = lnCont+1
    *IF lnCont=1
    *  lcOld =Ladata[1]
    *ELSE
    *  Ladata[1] = lcOld 
    *ENDIF  
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    lcOldAlis = ALIAS()
    SELECT (lcTmpFrCst)
    IF RECNO() = 1
      SHOW GET lnPurAdjPc ENABLE
    ELSE
      SHOW GET lnPurAdjPc DISABLE
    ENDIF    
    IF RECCOUNT()=1
      SHOW GET pbNxtScl DISABLE
      SHOW GET pbPrvsScl DISABLE
    ELSE  
      DO CASE
        CASE RECNO() = 1  
          SHOW GET pbNxtScl ENABLE
          SHOW GET pbPrvsScl DISABLE
        CASE RECNO() = RECCOUNT()  
          SHOW GET pbNxtScl DISABLE
          SHOW GET pbPrvsScl ENABLE
        CASE RECNO ()>1
          SHOW GET pbNxtScl ENABLE
          SHOW GET pbPrvsScl ENABLE
      ENDCASE    
    ENDIF 
    *--mhm2006
    *laData[2] = SUBSTR(laData[1],lnClrPos,lnClrLen)
    laData[2] = SUBSTR(Style.Style,lnClrPos,lnClrLen)
    *--mhm2006
    laData[1] = PADR(laData[1],lnMjrWid)
    SELECT (lcOldAlis)
    =lfPFStatus()
  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
    *-- recollect the data again.
     STORE SPACE(0) TO laShowSiz
     STORE 0 TO laShowStk ,laShowFig ,lnPurAdjPc
     =lfCollect()
     SHOW GET ibStyle    DISABLE
     SHOW GET ibColor    DISABLE
ENDCASE
SHOW GET pbDlt       DISABLE
SHOW GET pbUsrFields DISABLE
SHOW GET pbNotePad   DISABLE
SHOW GET pbAudTrail  DISABLE
=lfChkLstRc()
*-- End OF lpShow.
*!*************************************************************
*! Name      : lfActPad
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
*!C#122961,1
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Calculate Opening Stock Figure'
ON SELECTION BAR 1 OF _OPTIONPOP DO lfvDate

*-- End of lfActPad.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to collect the data  Temp Files.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfCollect ()
*!*************************************************************
FUNCTION lfCollect
PRIVATE lcOldALIas,lnRecNo
STORE 0 TO lnScaleCnt,lnSclCnt,lcOldALIas,lnRecNo
lnCountr = 0
lcOldALIas = ALIAS()
lnRecNo  = RECNO('STYLE')
SELECT(lcTmpFrCst)
ZAP
SELECT(lcTmpMonth)
ZAP
SELECT(lcTmpMonth)
ZAP
SELECT(lcTmpMain)
ZAP
SELECT(lcTmpScale)
ZAP
=lfGetScale()        &&Function To Get the Scales SIZES
SELECT (lcTmpFrCst)
APPEND BLANK
REPLACE cRow WITH '001'
lnRow = 1
FOR I = 1 TO lnScaleCnt
    IF lnCountr = 4
      lnCountr = 1
      APPEND BLANK
      lnRow = lnRow + 1
      IF lnRow<10
        REPLACE cRow WITH '00'+ALLTRIM(STR(lnRow))
      ELSE
        IF lnRow>10 AND lnRow<100
          REPLACE cRow WITH '0'+ ALLTRIM(STR(lnRow))
        ELSE
          REPLACE cRow WITH  ALLTRIM(STR(lnRow))
        ENDIF  
      ENDIF
      X = STR(lnCountr,1)
      REPLACE cDesc&X WITH arrStruc[1,I]
    ELSE
      lnCountr = lnCountr+1
      X = STR(lnCountr,1)
      REPLACE cDesc&X WITH arrStruc[1,I]
    ENDIF   
ENDFOR
lnCountr = 0 
GO TOP
FOR I = 1 TO lnScaleCnt
    IF lnCountr = 4
      lnCountr = 1
      SKIP
      *--mhm2006
      IF EOF()
        SKIP -1
      ENDIF
      
      X = STR(lnCountr,1)
      REPLACE Stok&X WITH arrStock[1,I]
    ELSE
      lnCountr = lnCountr+1
      X = STR(lnCountr,1)
      REPLACE Stok&X WITH arrStock[1,I]
    ENDIF   
ENDFOR
SET ORDER TO (lcTmpFrCst)
GO TOP
FOR I = 1 TO 4
  X = STR(I,1)
  laShowSiz[1,I] = cDesc&X
  IF !EMPTY(cDesc&X)
    laShowStk[1,I] = Stok&X
  ELSE
    laShowStk[1,I] = ''
  ENDIF  
ENDFOR
IF RECCOUNT()>1
  SHOW GET pbNxtScl ENABLE
ENDIF
SELECT (lcTmpMonth)
SET ORDER TO
SET FILTER TO
FOR M = 1 TO 15
  APPEND BLANK
  REPLACE cRow WITH '001'
  lnRow = 1
  lnCountr = 0
  FOR I = 1 TO lnScaleCnt
    IF lnCountr = 4
      lnCountr = 1
      APPEND BLANK
      lnRow = lnRow + 1
      IF lnRow<10
        REPLACE cRow WITH '00'+ALLTRIM(STR(lnRow))
      ELSE
        IF lnRow>10 AND lnRow<100
          REPLACE cRow WITH '0'+ ALLTRIM(STR(lnRow))
        ELSE
          REPLACE cRow WITH  ALLTRIM(STR(lnRow))
        ENDIF  
      ENDIF
      X = STR(lnCountr,1)
      REPLACE SF&X  WITH arrMnthSal[M,I]
      REPLACE nYear WITH arrMonth[M,1]
      REPLACE nMnth WITH arrMonth[M,2]
      REPLACE cMnth WITH arrMonth[M,3]  
    ELSE
      lnCountr = lnCountr+1
      X = STR(lnCountr,1)
      REPLACE SF&X WITH arrMnthSal[M,I]
      REPLACE nYear WITH arrMonth[M,1]
      REPLACE nMnth WITH arrMonth[M,2]
      REPLACE cMnth WITH arrMonth[M,3]  
    ENDIF   
  ENDFOR
ENDFOR  
lnCountr = 0 
GO TOP
FOR M = 1 TO 15
  lnCountr = 0
  FOR I = 1 TO lnScaleCnt
    IF lnCountr = 4
      lnCountr = 1
      SKIP
      *--mhm2006
      IF EOF()
        SKIP -1
      ENDIF
      
      X = STR(lnCountr,1)
      REPLACE PF&X WITH arrMnthPur[M,I]
    ELSE
      lnCountr = lnCountr+1
      X = STR(lnCountr,1)
      REPLACE PF&X WITH arrMnthPur[M,I]
    ENDIF 
  ENDFOR
  IF EOF()
    GO BOTTOM
  ELSE      
    SKIP 
    *--mhm2006
    IF EOF()
      SKIP -1
    ENDIF
    
  ENDIF 
ENDFOR
=lfShowFrst()
GO lnRecNo IN STYLE
SELECT (lcOldALIas)
*-- End OF lfCollect

*!*************************************************************
*! Name      : lfShowFrst
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Show the first page.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfShowFrst()
*!*************************************************************
FUNCTION lfShowFrst
STORE SPACE(0) TO larrEnable
SELECT (lcTmpMonth)
SET ORDER TO (lcTmpMonth)
GO TOP
SET FILTER TO cRow = EVAL(lcTmpFrCst+'.cRow')
FOR M = 1 TO 15
  laShowFig[M,1] = cMnth
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc1'))
    laShowFig[M,2]  = SF1
    laShowFig[M,3]  = PF1
    larrEnable[M,1] ='ENABLE'
  ELSE
    laShowFig[M,2]  = 0
    laShowFig[M,3]  = 0
    larrEnable[M,1] ='DISABLE'
  ENDIF  
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc2'))
    laShowFig[M,5]  = SF2
    laShowFig[M,6]  = PF2
    larrEnable[M,2] ='ENABLE'
  ELSE
    laShowFig[M,5]  = 0
    laShowFig[M,6]  = 0
    larrEnable[M,2] ='DISABLE'
  ENDIF  
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc3'))
    laShowFig[M,8]  = SF3
    laShowFig[M,9]  = PF3
    larrEnable[M,3] ='ENABLE'
  ELSE
    laShowFig[M,8]  = 0
    laShowFig[M,9]  = 0
    larrEnable[M,3] ='DISABLE'
  ENDIF  
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc4'))
    laShowFig[M,11] = SF4
    laShowFig[M,12] = PF4
    larrEnable[M,4] ='ENABLE'
  ELSE
    laShowFig[M,11] = 0
    laShowFig[M,12] = 0
    larrEnable[M,4] ='DISABLE'
  ENDIF  
  IF RECNO() = 1
      laShowFig[M,4]   = (EVAL(lcTmpFrCst+'.Stok1')-laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (EVAL(lcTmpFrCst+'.Stok2') -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (EVAL(lcTmpFrCst+'.Stok3') -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (EVAL(lcTmpFrCst+'.Stok4') -laShowFig[M,11])+laShowFig[M,12]
  ELSE
      *--mhm2006
      *laShowFig[M,4]   = (laShowFig[(M-1),4] -laShowFig[M,2])+laShowFig[M,3]
      *laShowFig[M,7]   = (laShowFig[(M-1),7] -laShowFig[M,5])+laShowFig[M,6]
      *laShowFig[M,10]  = (laShowFig[(M-1),10] -laShowFig[M,8])+laShowFig[M,9]
      *laShowFig[M,13]  = (laShowFig[(M-1),13] -laShowFig[M,11])+laShowFig[M,12]

      laShowFig[M,4]   = (laShowFig[(IIF(M = 1,1,M-1)),4] -laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (laShowFig[(IIF(M = 1,1,M-1)),7] -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (laShowFig[(IIF(M = 1,1,M-1)),10] -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (laShowFig[(IIF(M = 1,1,M-1)),13] -laShowFig[M,11])+laShowFig[M,12]
      *--mhm2006
  ENDIF  
  SKIP  
  *--mhm2006
  IF EOF()
    SKIP -1
  ENDIF
  
ENDFOR
SELECT (lcTmpFrCst)
IF RECCOUNT() = 1
  M = 0
  SELECT(lcTmpMonth)
  SCAN
  M = M+1
  laShowFig[M,14] = PF1+PF2+PF3+PF4 
  ENDSCAN 
ENDIF  

*-- End OF lfShowFrst

*!*************************************************************
*! Name      : lfvPF
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Validate the PF Field.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfvPF()
*!*************************************************************
FUNCTION lfvPF
PARAMETERS lnX,lnY
PRIVATE lnRecno ,lnOldY
lnOldY = lnY
DO CASE                        && TO Match the PF number in the temp file with the screen.
  CASE lnY = 3
    lnY = 1
  CASE lnY = 6
    lnY = 2
  CASE lnY = 9
    lnY = 3
  CASE lnY = 12
    lnY = 4
ENDCASE
x =''
X = STR(lnY,1)
SELECT (lcTmpMonth)
lcOrder=ORDER()
GO TOP
DO CASE
  CASE lnX =1
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 2
    SKIP 1
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 3
    SKIP 2
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 4
    SKIP 3
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 5
    SKIP 4
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 6
    SKIP 5
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 7
    SKIP 6
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 8
    SKIP 7
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 9
    SKIP 8
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 10
    SKIP 9
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 11
    SKIP 10
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 12
    SKIP 11
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 13
    SKIP 12
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 14
    SKIP 13
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
  CASE lnx = 15
    SKIP 14
    REPLACE PF&X WITH laShowFig[lnX,lnOldY]
ENDCASE
SET ORDER TO &lcOrder
FOR M = 1 TO 15
  IF  M = 1
      laShowFig[M,4]   = (EVAL(lcTmpFrCst+'.Stok1')-laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (EVAL(lcTmpFrCst+'.Stok2') -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (EVAL(lcTmpFrCst+'.Stok3') -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (EVAL(lcTmpFrCst+'.Stok4') -laShowFig[M,11])+laShowFig[M,12]
  ELSE
      laShowFig[M,4]   = (laShowFig[(M-1),4] -laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (laShowFig[(M-1),7] -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (laShowFig[(M-1),10] -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (laShowFig[(M-1),13] -laShowFig[M,11])+laShowFig[M,12]
  ENDIF
ENDFOR
lcoldAlia = ALIAS()
SELECT (lcTmpFrCst)
IF RECCOUNT() = 1 OR RECNO() = RECCOUNT()
  =lfTotal()  
ENDIF
SELECT(lcoldAlia)      
*Show Get laShowFig[lnX,lnOldY]
SHOW GETS
=lfRefresh()
*-- End OF lfvPF

*!*************************************************************
*! Name      : lfvPrc
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Validate the Percent Field.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfvPrc()
*!*************************************************************
FUNCTION lfvPrc
PRIVATE lcOldAlis ,lcMonth ,lnYear ,lnMonth ,lnCount ,lnRcecNo ,lnMnth ,lcFilt ,lcOrdr
STORE 0 TO lnYear ,lnMonth ,lnCount,lnRcecNo,lnMnth
STORE SPACE(0) TO lcYear ,lcMonth
lcOldAlis = ALIAS()
SELECT (lcTmpMonth)
lcFilt =FILTER()
lcOrdr = ORDER() 
SET FILTER TO
SET ORDER  TO 
GO TOP
REPLACE ALL SF1 WITH SF1*((100+lnPurAdjPc)/100) ,SF2 WITH  SF2*((100+lnPurAdjPc)/100) ,;
            SF3 WITH SF3*((100+lnPurAdjPc)/100) ,SF4 WITH  SF4*((100+lnPurAdjPc)/100) 
SET FILTER TO &lcFilt
SET ORDER TO &lcOrdr
SELECT (lcOldAlis)
=lfShowFrst()
SHOW GETS
=lfRefresh()
*-- End OF lfvPrc

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Save/Update.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpSavScr
PRIVATE lnAlias , lnRecNo , lnSalVal , lnPurVal
STORE 0 TO lnSalVal , lnPurVal
lnAlias = SELECT(0)
lnRecNo = RECNO('FORECAST')
=lfFilMainTmp()
SELECT FORECAST
SET ORDER TO Forecasty
SELECT (lcTmpMain)
GO TOP
SCAN
  SCATTER MEMVAR
  IF SEEK(STR(NYEAR,4,0) + STR(NMONTH,2,0) + STYLE ,'FORECAST')
    SELECT FORECAST
    GATHER MEMVAR
  ELSE
    SELECT FORECAST
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF  
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT('FORECAST'))
  GOTO lnRecNo IN FORECAST
ENDIF
SELECT (lcTmpMonth)
SET RELATION TO
SELECT (lnAlias)
*-- End Of lpSavScr.

*!*************************************************************
*! Name      : lfFilMainTmp
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to create the Main file for saving.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfFilMainTmp()
*!*************************************************************
FUNCTION lfFilMainTmp
PRIVATE lcMonth
STORE 0 TO lnYear ,lnMonth ,lnCount,lnRcecNo,lnMnth
STORE SPACE(0) TO lcYear ,lcMonth
DIMENSION arrPF[15,lnScaleCnt+2] ,arrSF[15,lnScaleCnt+2]


SELECT (lcTmpMonth)
SET FILTER TO
SET ORDER  TO 
SET RELATION TO CRow INTO (lcTmpFrCst)
GO TOP
SCAN
  IF STR(nyear,4)+cMnth <> lcYear+lcMonth  OR (RECNO() = 1)
    lnMonth = lnMonth + 1
    lcMonth = cMnth 
    lnMnth  = nMnth
    lnYear  = nYear
    lcYear  = STR(nyear,4)
    nCount  = 0
    lnRcecNo = RECNO()
    SET ORDER TO lcTmpIndex
    SEEK lcYear+lcMonth
    SCAN REST WHILE STR(nyear,4)+cMnth = lcYear+lcMonth
      FOR I = 1 TO 4
        X =STR(I,1)
        IF !EMPTY[EVAL(lcTmpFrCst+'.cDesc&X')]
          nCount =nCount+1
          arrSF[lnMonth,nCount] = SF&X
          arrPF[lnMonth,nCount] = PF&X
        ELSE
          EXIT  
        ENDIF
      ENDFOR      
    ENDSCAN
    arrSF[lnMonth,lnScaleCnt+1] = lnYear
    arrSF[lnMonth,lnScaleCnt+2] = lnMnth
    arrPF[lnMonth,lnScaleCnt+1] = lnYear
    arrPF[lnMonth,lnScaleCnt+2] = lnMnth
    SET ORDER TO
    IF lnRcecNo<>RECNO()
      GOTO lnRcecNo
    ENDIF  
  ENDIF 
ENDSCAN
lnScle = 0
FOR M = 1 TO 15
  SELECT (lcTmpScale)
  lnScle = 0
  SCAN
    lnScle = lnScle +EVAL(lcTmpScale+'.cnt')
    SELECT (lcTmpMain)
    APPEND BLANK
    REPLACE STYLE     WITH  EVAL(lcTmpScale+'.cStyle') 
    REPLACE nYear     WITH  arrSF[M,lnScaleCnt+1]
    REPLACE nMonth    WITH  arrSF[M,lnScaleCnt+2]
    REPLACE cAdd_User WITH gcUser_id
    REPLACE cAdd_Time WITH TIME()
    REPLACE dAdd_Date WITH gdSysDate
    FOR I = 1 TO EVAL(lcTmpScale+'.cnt')
      x =STR(I,1)
      IF lnScle = EVAL(lcTmpScale+'.cnt')
        *--mhm2006 
        *REPLACE nsalfrcst&X WITH arrSF[M,I]
        REPLACE nsalfrcst&X WITH arrMnthSal[M,I]
        *--mhm2006
        REPLACE npurfrcst&X WITH arrPF[M,I]
        REPLACE nOpStock&X  WITH arrStock[1,I]
      ELSE
        *--mhm2006
        *REPLACE nsalfrcst&X WITH arrSF[M,lnScle-EVAL(lcTmpScale+'.cnt')+I]
        REPLACE nsalfrcst&X WITH arrMnthSal[M,lnScle-EVAL(lcTmpScale+'.cnt')+I]
        *--mhm2006
        REPLACE npurfrcst&X WITH arrPF[M,lnScle-EVAL(lcTmpScale+'.cnt')+I]
        REPLACE nOpStock&X  WITH arrStock[1,lnScle-EVAL(lcTmpScale+'.cnt')+I]
      ENDIF  
    ENDFOR
    IF EVAL(lcTmpScale+'.cnt') >4  AND  EVAL(lcTmpScale+'.cnt')< 8
      *C130192,1 MMR 03/12/2000 Fix the bug of not saving some styles
      *lnScleRemin = 8 -I
      *IF !(lnScleRemin=0)
        *FOR Y = lnScleRemin TO 8
        FOR Y = I TO 8
      *C130192,1 MMR
          x =STR(Y,1)
          REPLACE nsalfrcst&X WITH 0
          
          REPLACE npurfrcst&X WITH 0
          REPLACE nOpStock&X  WITH 0
        ENDFOR
      *C130192,1 MMR 03/12/2000 Fix the bug of not saving some styles
      *ENDIF
      *C130192,1 MMR
      *--mhm
    ENDIF  
  ENDSCAN 
ENDFOR
*-- End Of lfFilMainTmp.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnClrPos = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnClrLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR
*-- End of lfGetColor.
*!*************************************************************
*! Name      : lfChangPos
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Valid funtion of the pbprvsscl button
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcPos,lnValue
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfChangPos()
*!*************************************************************
FUNCTION lfChangPos
PARAMETERS lcPos
PRIVATE lcMonth
STORE 0 TO lnYear ,lnMonth ,lnCount,lnRcecNo,lnMnth
STORE SPACE(0) TO lcYear ,lcMonth ,larrEnable
laShowSiz = ''
laShowStk = ''
lnCount = 0
FOR M = 1 TO 15
  laShowFig[M,14] = 0
ENDFOR  
SELECT(lcTmpFrCst)
IF lcPos = 'NXT'
  SKIP
  IF RECCOUNT()=RECNO() 
    lcNxt = 'DISABLE'
    lcPrv = 'ENABLE'
    =lfTotal()
  ELSE
    lcNxt = 'ENABLE'
    lcPrv = 'ENABLE'
  ENDIF  
ELSE
  SKIP -1
  IF RECNO() = 1
    lcNxt = 'ENABLE'
    lcPrv = 'DISABLE'
  ELSE
    lcNxt = 'ENABLE'
    lcPrv = 'ENABLE'
  ENDIF  
ENDIF  
IF laScrMode[3] = .T.
  IF RECNO() = 1
    lcEnblePrc = 'ENABLE'
  ELSE
    lcEnblePrc = 'DISABLE'
  ENDIF    
ENDIF
FOR I = 1 TO 4
  X = STR(I,1)
  laShowSiz[1,I] = cDesc&X
  IF !EMPTY(cDesc&X)
    laShowStk[1,I] = Stok&X
  ELSE
    laShowStk[1,I] = ''
  ENDIF  
ENDFOR
SELECT(lcTmpMonth)
SET FILTER TO cRow = EVAL(lcTmpFrCst+'.cRow')
GO TOP
FOR M = 1 TO 15
  lnCount = lnCount+1
  laShowFig[M,1] = cMnth
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc1'))
    laShowFig[M,2]  = SF1
    laShowFig[M,3]  = PF1
    larrEnable[M,1] ='ENABLE'
  ELSE
    laShowFig[M,2]  = 0
    laShowFig[M,3]  = 0
    larrEnable[M,1] ='DISABLE'
  ENDIF
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc2'))
    laShowFig[M,5]  = SF2
    laShowFig[M,6]  = PF2
    larrEnable[M,2] ='ENABLE'
  ELSE
    laShowFig[M,5]  = 0
    laShowFig[M,6]  = 0
    larrEnable[M,2] ='DISABLE'
  ENDIF 
   
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc3'))
    laShowFig[M,8]  = SF3
    laShowFig[M,9]  = PF3
    larrEnable[M,3] ='ENABLE'
  ELSE
    laShowFig[M,8]  = 0
    laShowFig[M,9]  = 0
    larrEnable[M,3] ='DISABLE'
  ENDIF  
  IF !EMPTY(EVAL(lcTmpFrCst+'.cDesc4'))
    laShowFig[M,11] = SF4
    laShowFig[M,12] = PF4
    larrEnable[M,4] ='ENABLE'
  ELSE
    laShowFig[M,11] = 0
    laShowFig[M,12] = 0
    larrEnable[M,4] ='DISABLE'
  ENDIF  
  IF lnCount = 1
      laShowFig[M,4]   = (EVAL(lcTmpFrCst+'.Stok1')-laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (EVAL(lcTmpFrCst+'.Stok2') -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (EVAL(lcTmpFrCst+'.Stok3') -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (EVAL(lcTmpFrCst+'.Stok4') -laShowFig[M,11])+laShowFig[M,12]
  ELSE
      laShowFig[M,4]   = (laShowFig[(M-1),4] -laShowFig[M,2])+laShowFig[M,3]
      laShowFig[M,7]   = (laShowFig[(M-1),7] -laShowFig[M,5])+laShowFig[M,6]
      laShowFig[M,10]  = (laShowFig[(M-1),10] -laShowFig[M,8])+laShowFig[M,9]
      laShowFig[M,13]  = (laShowFig[(M-1),13] -laShowFig[M,11])+laShowFig[M,12]
  ENDIF  
  SKIP  
  *--mhm2006
  IF EOF()
    SKIP -1
  ENDIF
  
ENDFOR
IF laScrMode[4]=.T.
  =lfPFStatus()
ELSE
  FOR M = 1 TO ALEN(larrEnable,1)
    SHOW GET laShowFig[M,3]
    SHOW GET laShowFig[M,6]
    SHOW GET laShowFig[M,9]
    SHOW GET laShowFig[M,12]
  ENDFOR
ENDIF
SHOW GET pbNxtScl   &lcNxt
SHOW GET pbPrvsScl  &lcPrv
SHOW GET lnPurAdjPc &lcEnblePrc
=lfRefresh()
*-- End of lfChangPos.

*!*************************************************************
*! Name      : lfPFStatus
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Show the PF Status
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfTotal()
*!*************************************************************
FUNCTION lfPFStatus

FOR M = 1 TO ALEN(larrEnable,1)
  SHOW GET laShowFig[M,3]  &larrEnable[M,1]
  SHOW GET laShowFig[M,6]  &larrEnable[M,2]
  SHOW GET laShowFig[M,9]  &larrEnable[M,3]
  SHOW GET laShowFig[M,12] &larrEnable[M,4]
ENDFOR

*-- End of lfPFStatus.
*!*************************************************************
*! Name      : lfTotal
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to calculate the TOTAL FIELD
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcPos,lnValue
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfTotal()
*!*************************************************************
FUNCTION lfTotal
PRIVATE lcMonth
STORE 0 TO lnYear ,lnMonth ,lnCount,lnRcecNo,lnMnth
STORE SPACE(0) TO lcYear ,lcMonth
FOR M = 1 TO 15
  laShowFig[M,14] = 0
ENDFOR  
lcOldAlis = ALIAS()
SELECT (lcTmpMonth)
lcFilt =FILTER()
lcOrdr = ORDER() 
SET FILTER TO
SET ORDER  TO 
GO TOP
SCAN
  IF STR(nyear,4)+cMnth <> lcYear+lcMonth  OR (RECNO() = 1)
    lnMonth = lnMonth + 1
    lcMonth = cMnth 
    lcYear  = STR(nyear,4)
    lnRcecNo = RECNO()
    SET ORDER TO lcTmpIndex
    SEEK lcYear+lcMonth
    SCAN REST WHILE STR(nyear,4)+cMnth = lcYear+lcMonth
      laShowFig[lnMonth,14] = laShowFig[lnMonth,14] + (PF1+PF2+PF3+PF4)
    ENDSCAN
    SET ORDER TO
    IF RECNO()<>lnRcecNo
      GOTO lnRcecNo
    ENDIF  
  ENDIF 
ENDSCAN
SET FILTER TO &lcFilt
SET ORDER TO &lcOrdr
SELECT (lcOldAlis)
*-- End of lfTotal.

*!*************************************************************
*! Name      : lfGetScale
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to get the scale count of style.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetScale()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetScale

SELECT STYLE
IF SEEK(PADR(laData[1],lnMjrWid))
  SCAN REST WHILE STYLE = PADR(laData[1],lnMjrWid);
              FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
    IF SEEK('S'+SCALE,'SCALE')
      lnScaleCnt = lnScaleCnt + Scale.cnt
      dimension arrStruc[1,alen(arrStruc,2)+Scale.cnt]
      FOR I = 1 TO Scale.cnt
        X = STR(I,1)
        IF lnScaleCnt = Scale.cnt
          STORE Scale.SZ&X TO arrStruc[1,I] 
        ELSE
          STORE Scale.SZ&X TO arrStruc[1,lnScaleCnt-Scale.cnt+I] 
        ENDIF  
      ENDFOR   
      =lfGetOpenStk()       &&Function to get the open stock
      =lfGetMonth()         &&Function to get Months Data    
    ENDIF
    SELECT SCALE
    SCATTER MEMVAR MEMO
    SELECT (lcTmpScale)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cStyle WITH STYLE.STYLE
  ENDSCAN
ENDIF
*-- End of lfGetScale.

*!*************************************************************
*! Name      : lfGetOpenStk
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function To Get the Open Stock.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetOpenStk()
*!*************************************************************
Function lfGetOpenStk   
Private lcOrder,lnForcstRec
I=0
X=''
dimension arrStock[1,alen(arrStock,2)+Scale.cnt]
SELECT FORECAST
lcOrder     = ORDER()
SET ORDER TO Forecast
IF SEEK(STYLE.STYLE+STR(0,4)+STR(0,2))
  FOR I = 1 TO Scale.cnt
    X = STR(I,1)
    IF lnScaleCnt = Scale.cnt
      STORE nopstock&X TO arrStock[1,I] 
    ELSE
      STORE nopstock&X TO arrStock[1,lnScaleCnt-Scale.cnt+I] 
    ENDIF  
  ENDFOR 
ELSE
  FOR I = 1 TO Scale.cnt
    X = STR(I,1)
    IF lnScaleCnt = Scale.cnt
      STORE 0 TO arrStock[1,I] 
    ELSE
      STORE 0 TO arrStock[1,lnScaleCnt-Scale.cnt+I] 
    ENDIF  
  ENDFOR   
ENDIF    
SELECT FORECAST
SET ORDER TO &lcOrder  
SELECT Scale
*--End OF lfGetOpenStk
*!*************************************************************
*! Name      : lfGetMonth
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Get months data.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnForMonth,lnForYear
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfGetMonth()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetMonth
PRIVATE lnMonth,lnYear,lcMonth
I=0
X=''
lDSysDate = {}
lnMonth = Month(GDSYSDATE)
lnYear  = Year(GDSYSDATE)
lcMonth = cMonth(GDSYSDATE)
SELECT Forecast
SET ORDER TO Forecast
*C130192,2 MMR 04/11/2006 Fix the bug of not collecting some sales and purchase
*C130192,2 MMR            because of internal error in the ACopy() function.[Start]
dimension arrSalTmp[15]
arrSalTmp=0
IF alen(arrMnthSal,2)<>0
  dimension arrSalTmp[15,alen(arrMnthSal,2)]
ENDIF
*C130192,2 MMR.[End]
=ACOPY(arrMnthSal,arrSalTmp)
DIMENSION arrMnthSal[15,alen(arrMnthSal,2)+Scale.cnt]
*--mhm2006 1
STORE 0 TO arrMnthSal 
*--mhm2006 1
IF alen(arrSalTmp,2)<>0
  FOR Y = 1 TO 15
    FOR Z = 1 TO alen[arrSalTmp,2]
      arrMnthSal[Y,Z] = arrSalTmp[Y,Z]
    ENDFOR
  ENDFOR  
ENDIF
*C130192,2 MMR 04/11/2006 Fix the bug of not collecting some sales and purchase
*C130192,2 MMR            because of internal error in the ACopy() function.[Start]
DIMENSION arrPurTmp[15]
arrPurTmp=0
IF alen(arrMnthPur,2)<>0
  dimension arrPurTmp[15,alen(arrMnthPur,2)]
ENDIF
*C130192,2 MMR.[End]
=ACOPY(arrMnthPur,arrPurTmp)
DIMENSION arrMnthPur[15,alen(arrMnthPur,2)+Scale.cnt]
*--mhm2006 1
STORE 0 TO arrMnthPur
*--mhm2006 1
IF alen(arrPurTmp,2)<>0
  FOR Y = 1 TO 15
    FOR Z = 1 TO alen[arrPurTmp,2]
      arrMnthPur[Y,Z] = arrPurTmp[Y,Z]
    ENDFOR
  ENDFOR  
ENDIF
IF SEEK (STYLE.STYLE)
  FOR lnM = 1 TO 15
    IF SEEK (STYLE.STYLE+STR(lnYear,4)+STR(lnMonth,2))
      FOR I = 1 TO Scale.cnt
        X = STR(I,1)
        IF lnScaleCnt = Scale.cnt
          STORE Forecast.nsalfrcst&X TO arrMnthSal[lnM,I] 
        ELSE
          STORE Forecast.nsalfrcst&X TO arrMnthSal[lnM,lnScaleCnt-Scale.cnt+I] 
        ENDIF  
      ENDFOR
      I = 0   
      FOR I = 1 TO Scale.cnt
        X = STR(I,1)
        IF lnScaleCnt = Scale.cnt
          STORE Forecast.npurfrcst&X TO arrMnthPur[lnM,I]
        ELSE
          STORE Forecast.npurfrcst&X TO arrMnthPur[lnM,lnScaleCnt-Scale.cnt+I] 
        ENDIF  
      ENDFOR   
    ELSE
      FOR I = 1 TO Scale.cnt
        X = STR(I,1)
        IF lnScaleCnt = Scale.cnt
          STORE 0 TO arrMnthSal[lnM,I] 
        ELSE
          STORE 0 TO arrMnthSal[lnM,lnScaleCnt-Scale.cnt+I] 
        ENDIF  
      ENDFOR   
      I = 0   
      FOR I = 1 TO Scale.cnt
        X = STR(I,1)
        IF lnScaleCnt = Scale.cnt
          STORE 0 TO arrMnthPur[lnM,I]
        ELSE
          STORE 0 TO arrMnthPur[lnM,lnScaleCnt-Scale.cnt+I] 
        ENDIF  
      ENDFOR   
    ENDIF  
    STORE  lnYear  to arrMonth[lnM,1]
    STORE  lnMonth to arrMonth[lnM,2]
    STORE  lcMonth to arrMonth[lnM,3]
    IF lnMonth=12
      lnMonth  = 1
      lnYear   = lnYear+1
      lcMonth  ="January"
    ELSE
      lnMonth  = lnMonth+1
      MyDate= "01/"+alltrim(str(lnMonth))+"/2006"
      lDMyDate = CTOD(MyDate)
      lcMonth  =cMonth(lDMyDate)
    ENDIF  
  ENDFOR 
ELSE
  FOR lnM = 1 TO 15
    FOR I = 1 TO Scale.cnt
      X = STR(I,1)
      IF lnScaleCnt = Scale.cnt
        STORE 0 TO arrMnthSal[lnM,I] 
      ELSE
        STORE 0 TO arrMnthSal[lnM,lnScaleCnt-Scale.cnt+I] 
      ENDIF  
    ENDFOR   
    I = 0   
    FOR I = 1 TO Scale.cnt
      X = STR(I,1)
      IF lnScaleCnt = Scale.cnt
        STORE 0 TO arrMnthPur[lnM,I]
      ELSE
        STORE 0 TO arrMnthPur[lnM,lnScaleCnt-Scale.cnt+I] 
      ENDIF  
    ENDFOR 
    STORE  lnYear  to arrMonth[lnM,1]
    STORE  lnMonth to arrMonth[lnM,2]
    STORE  lcMonth to arrMonth[lnM,3]
    IF lnMonth=12
       lnMonth  = 1
       lnYear   = lnYear+1
       lcMonth  ="January"
    ELSE
      lnMonth  = lnMonth+1
      MyDate= "01/"+alltrim(str(lnMonth))+"/2006"
      lDMyDate = CTOD(MyDate)
      lcMonth  =cMonth(lDMyDate)
    ENDIF  
  ENDFOR 
ENDIF  
*-- End of lfGetMonth.

*!*************************************************************
*! Name      : lfvDefWare
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Validation of the warehouse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefWare()
*!*************************************************************
*!C#122961,1
FUNCTION lfvDefWare

IF llBrowse OR !SEEK(lcWareCode,'WAREHOUS')
  lcWareCode = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
ENDIF
llBrowse = .F.
*-- End of lfvDefWare.

*!*************************************************************
*! Name      : lfvDefPCurr
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Validation of the default price currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefPCurr()
*!*************************************************************
*!C#122961,1

*FUNCTION lfvDefPCurr

*-- if the entered currency does not exist in the currency code file
*IF llBrowse OR !SEEK(lcPCurr,"SycCurr")
*  IF !gfcurrbrow(@lcPCurr)
*    lcPCurr = gcBaseCurr
*  ENDIF
*ENDIF
*llBrowse = .F.
**-- get the final exchange rate of the price currency
*lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
**-- if there is no valid rate
*IF lnPRate <= 0
*  IF gfModalGen('INM36004B36001','DIALOG','price') = 1
*    lcPCurr = gcBaseCurr
*    lnPRate = 1
*    lnUnit1 = 1
*  ELSE
*    _CUROBJ = OBJNUM(lcPCurr)
*  ENDIF
*ENDIF
*IF lcPCurr = gcBaseCurr OR !llEditExRt
*  SHOW GET lnPRate DISABLE
*ELSE
*  SHOW GET lnPRate ENABLE
*ENDIF
*-- End of lfvDefPCurr.

*!*************************************************************
*! Name      : lfGetStyle
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to get the correct style record.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetStyle()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetStyle

PRIVATE lnAlias
lnAlias = SELECT(0)
IF SEEK(PADR(lcStyle,lnMjrWid),'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(lcStyle,lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = lcColor .AND.;
                    SCALE = PADR(POTOBUY.CSIZECODE,3)
ENDIF
SELECT (lnAlias)
*-- End of lfGetStyle.



*!*************************************************************

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
*!C#122961,1
FUNCTION lfDMainLot

IF WONTOP()= lcStyleTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTabKey  WITH 'POSELST2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'POSELST2','pbCancel'
ENDIF
SET DECIMALS TO lnSetDeci
*-- End of lfDMainLot

*!*************************************************************
*! Name      : lpTabKey
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTabKey WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
*!C#122961,1
PROCEDURE lpTabKey
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
*!C#122961,1
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!**************************************************************************
*! Name      : lfVariabls
*! Developer : Mostafa Rwasah (MMR)
*! Date      :01/06/2006
*! Purpose   : Function to save/restore variabls
*!**************************************************************************
*! Example   : =lfVariabls()
*!**************************************************************************
*!C#122961,1
FUNCTION lfVariabls
PARAMETERS llRestore

FOR lnI = 1 TO 11
  IF llRestore
    &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
  ELSE
    laSaveVar[lnI,2] = EVALUATE(laSaveVar[lnI,1])
  ENDIF
ENDFOR
*-- End of lfVariabls.

*!*************************************************************
*! Name      : lfvGtSclSp
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function returns the length of Scale with Separator.
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfvGtSclSp()
*!*************************************************************
*!C#122961,1
FUNCTION lfvGtSclSp
PRIVATE lnAlias
lnAlias = SELECT(0)
IF llMScale
  llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  LOCATE FOR cItemRecty='U' AND cISegType='S'
  IF FOUND()
    SKIP -1
    lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
  ELSE
    lnSepMins = 3
  ENDIF
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
ELSE
  lnSepMins = 0
ENDIF
SELECT(lnAlias)
RETURN (lnSepMins)

*--End of lfvGtSclSp.


*!*************************************************************
*! Name      : lpClsScr
*! Developer : Mostafa Rwasah (MMR)
*! Date      : 01/06/2006
*! Purpose   : Function to Validae the close button in the pannel
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lpClsScr()
*!*************************************************************
*!C#122961,1
FUNCTION lpClsScr
IF !EMPTY(lcBaseFile)
  SCATTER FIELDS &lcScFields MEMO TO laData
  laData[1]  = PADR(laData[1],lnMjrWid)
ENDIF  
IF laScrMode[1] .OR. laScrMode[2]
  SET DECIMALS TO lnSetDeci
ENDIF
*--End of lpClsScr.

*!*************************************************************
*! Name      : lfvDate
*! Developer : Mostafa Rawash[MMR]
*! Date      : 09/13/1997
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrT() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvDate
PRIVATE lcOldAlias ,lnStyRec,lnFrcstRec,lnCountLn,lnTotRec,lcOrder
ON KEY LABEL CTRL+ENTER
llSuppress = .F.
DIMENSION laSortDesc[2,1] , laSortVal[2,1] ,laOpenStk[1,8]
STORE 0 TO laOpenStk ,lnCountLn ,lnTotRec1,lnStyRec ,lnFrcstRec
*B607117,1 KHM 06/01/2003 (Begin) Saving the ESC key and restore it after calling the option grid.
lcEscapeKy = ON("KEY","ESC")
lcExpr    = gfOpGrid('POFSTDL' , .T.)
ON KEY LABEL ESCAPE &lcEscapeKy
IF TYPE('lcExpr')='L'
  lcExpr ='.F.'
ENDIF  
IF lcExpr ='.T.'
  lcOldAlias =ALIAS()
  SELECT ORDHDR
  SET ORDER TO ORDHDR
  SELECT ORDLINE
  SET ORDER TO Ordlines
  SELECT POSHDR
  SET ORDER TO POSHDR
  SELECT POSLN
  SET ORDER TO Poslns
  SELECT FORECAST
  lnForcstRec = RECNO()
  lcOrder     = ORDER()
  SET ORDER TO Forecast
  IF EMPTY(ldDate)
    ldDate = GDSYSDATE
  ENDIF 
  SELECT STYLE
  lnTotRec = RECCOUNT()
  lnStyRec  = RECNO()
  GO TOP
  SCAN
    WAIT WINDOW 'UPDATE FORECAST FILE WITH STYLE# '+STYLE.STYLE NOWAIT
    FOR I = 1 TO 8
      X = STR(I,1)
      STORE STYLE.STK&X TO laOpenStk[1,I] 
      SELECT ORDLINE
      IF SEEK (STYLE.STYLE)
        SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = STYLE.STYLE
          =SEEK(ORDLINE.cordtype+ORDLINE.order,'ORDHDR')
          IF ORDHDR.complete <= ldDate AND !(ORDHDR.STATUS $ 'HXC')
            lnOrd =0 
            lnOrd = laOpenStk[1,I] - ORDLINE.QTY&X
            STORE lnOrd TO laOpenStk[1,I] 
          ENDIF  
        ENDSCAN   
      ENDIF   
     SELECT POSLN
     SET ORDER TO Poslns
     IF SEEK (STYLE.STYLE)
       SCAN REST WHILE style+cstytype+po+STR(lineno,6)+trancd = STYLE.STYLE
         =SEEK(POSLN.cstytype+POSLN.PO,'POSHDR')
         IF POSHDR.available <= ldDate AND !(POSHDR.STATUS $ 'HXS') 
           IF TRANCD = '1' 
             X = STR(I,1)
             lnOrd =0 
             lnOrd = laOpenStk[1,I] + POSLN.QTY&X
             STORE lnOrd TO laOpenStk[1,I] 
           ELSE
             X = STR(I,1)
             lnOrd =0 
             lnOrd = laOpenStk[1,I] - POSLN.QTY&X
            STORE lnOrd TO laOpenStk[1,I] 
           ENDIF  
          ENDIF
        ENDSCAN   
      ENDIF  
    ENDFOR   
    I=0
    X=''
    SELECT FORECAST
    IF SEEK(STYLE.STYLE+STR(0,4)+STR(0,2))
      FOR I = 1 TO ALEN(laOpenStk,2)
        X = STR(I,1)
        REPLACE nopstock&X WITH laOpenStk[1,I] 
      ENDFOR   
    ELSE
      APPEND BLANK
      REPLACE STYLE      WITH STYLE.STYLE ;
              nYear      WITH 0000        ; 
              nMonth     WITH 00          ;
              cAdd_User  WITH gcUser_id   ;
              cAdd_Time  WITH TIME()      ;
              dAdd_Date  WITH gdSysDate   
      FOR I = 1 TO ALEN(laOpenStk,2)
        X = STR(I,1)
        *--mhm2006
        *REPLACE nsalfrcst&X  WITH 0              ;
        *        npurfrcst&X  WITH 0              ;
        *        nopstock&X   WITH laOpenStk[1,I]

        REPLACE npurfrcst&X  WITH 0              ;
                nopstock&X   WITH laOpenStk[1,I]
        *--mhm2006
      ENDFOR   
    ENDIF
  ENDSCAN
  IF RECNO() <> lnStyRec
    GOTO lnStyRec
  ENDIF
  SELECT FORECAST
  IF RECNO() <> lnForcstRec
    GOTO lnForcstRec
  ENDIF
  SET ORDER TO &lcOrder
  SELECT (lcOldAlias)   
ENDIF  
*-- end of lfvDate.
*!*************************************************************
*! Name      : lfvEndDate
*! Developer : Mostafa Rawash (MMR)
*! Date      : 01/06/2006
*! Purpose   : Valid function for Calculation period.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =lfvEndDate()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfvEndDate
llRpKeyOff = (ldRpEndDat < gdSysDate)  && Key off value (Yes or No)
=lfOGShowGet('llRpKeyOff')
ldDate = ldRpEndDat
RETURN .T.
*-- end of lfvEndDate.

*!*************************************************************
*! Name        : lfWhenRep
*! Developer   : Mostafa Rwasah (MMR)
*! Date        : 01/06/2006
*! Purpose     : Option Grid When Function
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfWhenRep
*-- end of lfWhenRep.

*!*************************************************************
*! Name      : gfCPBrows     C#122961,1
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Validation of the browse button in the pannel
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel browse button
*!->
FUNCTION gfCPBrows

SELECT (lcBaseFile)
=gfBrows()
laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
laScrMode   = .F.
laScrMode[2]= .T.
SHOW GETS
*-- End of gfCpBrows.

*!*************************************************************
*! Name      : lpTopScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Top record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpTopScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)

IF SEEK(laData[1],'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF

*-- End OF lpTopScr.

*!*************************************************************
*! Name      : lpBtmScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Bottom record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpBtmScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF SEEK(laData[1],'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF
*-- End OF lpBtmScr.

*!*************************************************************
*! Name      : lpNxtScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Next record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpNxtScr

lcStyle   = laData[1]
lcColor   = laData[2]
laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF laData[1]+laData[2] = lcStyle+lcColor
  SELECT STYLE
  LOCATE REST FOR PADR(STYLE,lnMjrWid) # laData[1] .OR.;
                  SUBSTR(STYLE,lnClrPos,lnClrLen) # laData[2]
  laData[1] = PADR(STYLE.STYLE,lnMjrWid)
  laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
ENDIF
*-- End OF lpNxtScr

*!*************************************************************
*! Name      : lpPrvScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Previus record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpPrvScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF SEEK(PADR(laData[1],lnMjrWid),'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF
*-- End OF lpPrvScr.

*!*************************************************************
*! Name      : lfChkLstRc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Chick the last record in the sytle file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*!C#122961,1
FUNCTION lfChkLstRc

lnRecNo = RECNO('STYLE')
GO BOTTOM IN STYLE
lcStyle = PADR(STYLE.STYLE,lnMjrWid)
lcColor = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF laData[1]+laData[2] = lcStyle+lcColor
  STORE 'DISABLE' TO laCtrStat[2],laCtrStat[3]
  SHOW GET pbBtm DISABLE
  SHOW GET pbNxt DISABLE
ENDIF
GO lnRecNo IN STYLE
*-- End of lfChkLstRc.
