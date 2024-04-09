*!****************************************************************
*! Program file  : POSPLIT.PRG
*! Program desc. : Spliting po's from master po
*! For screen    : POSPLIT.SPX
*! System        : Aria Apparel System - Version 2.7.
*! Module        : Style Purchase Order (PO)
*! Developer     : Ehab Ismail (EIH)
*! Date          : 07/09/2005
*!****************************************************************
*! Passed Parameters  : None.
*!****************************************************************
*!Related to     : C#128051
*!****************************************************************
*!Modifications  :
*B039723,1 EIH 09/22/2005 Add split all options on that custom.
*B607918,1 TMI 01/03/2007 Fix a bug that the user defined field cconpo is empitied when saving the splitted PO. (T20060915.0004)
*!****************************************************************
EXTERNAL ARRAY laData,laKeyField,laDefProc 

DIMENSION laScrMode[4],laBrowArr[1],laKeyField[1,4],laBrowArr[1]
STORE '' TO lcVendor,lcNewVend
STORE ' '  TO lcWinCh1,lcWinCh2,lcWinCh3 ,laData
STORE '' TO lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8,lcOldValue
STORE 0  TO lnMarker
STORE .F. TO laScrMode
laScrMode[1] = .T.
DIMENSION laOpenQty[8],laSpltQty[8]
lcPoLnBrow = 'Purchase Order Detail'
STORE '' TO lcSclDesc,lcPONo,lcStyPict,lcTmpPoHdr,lcTmpPoLns,lcNewPO
STORE 0 TO laOpenQty,laSpltQty
STORE 0 TO lnOTotQty,lnSTotQty
lcStyPict  = gfItemMask('PI')
llClose    = .T.           && Variable to check the status of pbClose
llBrowse   = .F.           && Variable for browsing.
lcWindTitl = "Split Purchase Order" 
lcBrowBmp  = gcBmpHome + "ExtKey.BMP"
SHOW GET pbBrws  DISABLE
laCtrStat[10]  =  "DISABLE" 
SHOW GET pbNotePad DISABLE
glFromBrow = .F.
*-- procedures (lpNxtScr ,lpPrvScr ,lpSavScr, lpClsScr ,lpDelScr )
laDefProc[01]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[02]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[03]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[04]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
laDefProc[09]  = .F.              && Disable the control panel save proc.  (lpSavScr)

SHOW GET pbDlt   DISABLE
laCtrStat[8]  =  "DISABLE" 


*-- The screen keys
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.T.
laKeyField[1,3] = "POSHDR"
laKeyField[1,4] = 1

STORE '' TO laData
IF !GFSETUP()
  RETURN
ENDIF

lcScFields    = 'PO' 
lcWinCh1    = gfTempName()
lcWinCh2    = gfTempName()
lcWinCh3    = gfTempName()

lcTmpPoHdr = gfTempName()
lcTmpPoLns = gfTempName()
SELECT POSHDR
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'NewVend'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 8
laFileStru[lnFileStru+1,4] = 0

CREATE TABLE (gcWorkDir+lcTmpPoHdr) FROM ARRAY laFileStru 
INDEX ON  cstytype+po TAG Poshdr ADDITIVE
SELECT POSLN
SCATTER MEMVAR BLANK MEMO
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+9,4]
FOR I = 1 TO 8
  LCI =ALLTRIM(STR(I))
  laFileStru[lnFileStru+I,1] = 'SpltQty&LCI'
  laFileStru[lnFileStru+I,2] = 'N'
  laFileStru[lnFileStru+I,3] = 6
  laFileStru[lnFileStru+I,4] = 0
ENDFOR

laFileStru[lnFileStru+9,1] = 'SpltTQty'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 7
laFileStru[lnFileStru+9,4] = 0

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
DIMENSION laFileStru[lnFileStru+10,4]
laFileStru[lnFileStru+10,1] = 'nSpltAll'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 2
laFileStru[lnFileStru+10,4] = 0
*B039723,1 EIH 09/22/2005 [End]

CREATE TABLE (gcWorkDir+lcTmpPoLns) FROM ARRAY laFileStru 
INDEX ON  cstytype+po+style+STR(lineno,6)+trancd  TAG Posln ADDITIVE
DO (gcScrDir + gcWinAppl + '\POSPLIT.SPR')   
RELEASE WINDOW (lcPoLnBrow)


*!*************************************************************
*! Name      : lfReadAct
*! Developer : Ehab Ismail (EIH)
*! Date      : 07/09/2005
*! Purpose   : READ Activate function of invoice screen
*!*************************************************************
*! Calls     : gfStopBrow,lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPoLnBrow)
ON KEY LABEL CTRL+E DO gfCPEdit
ON KEY LABEL CTRL+N DO gfCPNext
ON KEY LABEL CTRL+I DO gfCPPrvis
ON KEY LABEL CTRL+T DO gfCPSelect
ON KEY LABEL CTRL+F DO gfCPTop
ON KEY LABEL CTRL+L DO gfCPBttm
ON KEY LABEL CTRL+R DO gfCPBrows
ON KEY LABEL CTRL+S DO gfCPSave

*--End of Function lfReadAct.
*!*************************************************************
*! Name      : lfReadDAct
*! Developer : Ehab Ismail(NNA)
*! Date      : 07/09/2005
*! Purpose   : READ Deactivate function of screen Ord100
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfReadDAct()
*!*************************************************************
FUNCTION lfReadDAct
*ON KEY LABEL ALT+B
IF WONTOP() = (lcWindTitl)
  *-- Set browsing variable
  glFromBrow  = .T.
ELSE
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB DO lpTab WITH ;
IIF(WONTOP()=lcWinCh1,lcPoLnBrow,IIF(WONTOP()=lcPoLnBrow,lcWinCh3,'gwcContrl1')),;
IIF(WONTOP()=lcPoLnBrow,'laSpltQty[1]',IIF(WONTOP()=lcWinCh3,'pbTop','pbTop'))

ON KEY LABEL BACKTAB DO lpBackTab WITH ;
IIF(WONTOP()=lcPoLnBrow,'gwcContrl1',lcWinCh3),IIF(WONTOP()='gwcContrl1','laSpltQty[1]','laData[1]')

*--End of Function lfReadDAct.
*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Validate PO #
*!*************************************************************
*! Calls     : gfBrows()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_1()
*!*************************************************************
FUNCTION lfvData_1
lcAType = 'P'
IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK('P'+laData[1],'POSHDR') )
  lcPo=laData[1]
  SELECT POSHDR
  SET FILTER TO cStyType='P'
  DO POSBrow WITH lcPo,ALLTRIM(laData[2]),lcAType
  laData[1]=lcPo
  llBrowse = .F.
ENDIF 

RETURN


*--End of Function lfvData_1.
*!***************************************************************************
*! Name      : lpShow
*! Developer : EHAB ISMAIL (EIH)
*! Date      : 06/20/2005
*! Purpose    : Screen modes controlling procedure.
*!***************************************************************************
*! Calls      : None.
*!***************************************************************************
*! Parameters : None.
*!***************************************************************************
*! Returns    : None.
*!***************************************************************************
*! Example    : DO lpShow
*!***************************************************************************
PROCEDURE lpShow
SHOW GET pbDlt      DISABLE
SHOW GET pbCpCalndr DISABLE
SHOW GET pbcptask   DISABLE
SHOW GET pbcpCalc   DISABLE

DO CASE
  *----> SELECT MODE <---
  CASE laScrMode[1]
    STORE SPACE(0)  TO laData[1] 
    SELECT (lcTmpPoHdr)
    ZAP
    SELECT (lcTmpPoLns)
    ZAP
    = lfBrows()
    = lfwBrows()
    SELECT(lcTmpPoLns)
    LOCATE
   lcVendor = ''
   ACTIVATE WINDOW (lcWinCh1) SAME
   SHOW GET laData[1]   ENABLE
    _CUROBJ = OBJNUM(laData[1])
  *---->VIEW MODE <----
  CASE laScrMode[2]
    SELECT (lcTmpPoHdr)
    ZAP
    SELECT (lcTmpPoLns)
    ZAP
    = lfCollect()
    lcNewVend = ''
    SELECT(lcTmpPoLns)
    LOCATE
    = lfBrows()
    = lfwBrows()
    SELECT(lcTmpPoLns)
    LOCATE
    SHOW GET ladata[1] DISABLE    

  
  *---->EDIT MODE <----
  CASE laScrMode[3]
    
    STORE .T. TO llCUpdate,llUpdated
    STORE .F. TO llCSave,llShow
    = lfwBrows()
    SELECT(lcTmpPoLns)
    SHOW GET ladata[1] DISABLE     
    SHOW GET lcVendor DISABLE    
    _CUROBJ = OBJNUM(lcNewVend)
    SHOW GET ibVendor  ENABLE
  *----> ADD MODE <----
  CASE laScrMode[4] 

      
ENDCASE  
SHOW GET pbBrws  DISABLE
laCtrStat[10]  =  "DISABLE" 
SHOW GET pbNotePad DISABLE

*--End of Function lpShow.
*!*************************************************************
*! Name      : lfBrows
*! Developer : NADER NABIL (NNA)
*! Date      : 06/20/2005
*! Purpose   : To brows the selected of Order.
*!*************************************************************
*! Example   :  lfBrows()
*!*************************************************************
FUNCTION lfBrows
PRIVATE lnAlias
lnAlias  = SELECT()
SELECT (lcTmpPoLns)
LOCATE
lnMarker = RECNO()
lcBrFlds = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
		   [STYLE :R :H= 'Style   - Color' , QTY1 :R :H='Qty1',QTY2 :R :H='Qty2',QTY3 :R :H='Qty3',QTY4 :R :H='Qty4',QTY5 :R :H='Qty5',;
           QTY6 :R :H='Qty6',QTY7 :R :H='Qty7',QTY8 :R :H='Qty8',TOTQTY :R :H='TotQty']
BROWSE FIELDS &lcBrFlds;
       WINDOW    (lcWinCh2)  ;
       IN WINDOW (gcBaseWind) ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       WHEN lfwBrows()   ; 
       VALID :F lfvBrows();
       TITLE (lcPoLnBrow)
ACTIVATE WINDOW (lcPoLnBrow)
SELECT (lnAlias)

*--End of Function lfBrows.
*!*************************************************************
*! Name      : lfClearKey
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

*ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+Z
ON KEY LABEL ALT+A
ON KEY LABEL ALT+C
ON KEY LABEL CTRL+E 
ON KEY LABEL CTRL+N 
ON KEY LABEL CTRL+I 
ON KEY LABEL CTRL+T 
ON KEY LABEL CTRL+F 
ON KEY LABEL CTRL+L 
ON KEY LABEL CTRL+R 
ON KEY LABEL CTRL+S 

*--End of Function lfClearKey.
*!***************************************************************************
*! Name       : lfwBrows
*! Developer  : Ehab Ismail (EIH)
*! Date       : 06/20/2005
*! Purpose    : "WHEN" function of the Dept. browse.
*!***************************************************************************
*! Calls      : None.
*!***************************************************************************
*! Parameters : None.
*!***************************************************************************
*! Returns    : None.
*!***************************************************************************
*! Example    : = lfwBrows()
*!***************************************************************************
FUNCTION lfwBrows

SELECT (lcTmpPoLns)
SET RELATION TO 'S'+SCALE INTO SCALE
lnMarker = RECNO(lcTmpPoLns)
lnOTotQty = 0
FOR I = 1 TO 8
  LCI = ALLTRIM(STR(I))
  laOpenQty[I] = EVAL(lcTmpPoLns+'.QTY&LCI') &&IIF(QTY&LCI<>0,EVAL(lcTmpPoLns+'.cNewLoc&LCI'),'')
  lnOTotQty    = lnOTotQty + laOpenQty[I] 
  SHOW GET laOpenQty[I] DISABLE
  laSpltQty[I] = EVAL(lcTmpPoLns+'.SpltQty&LCI')
  IF laScrMode[3] = .T. 
    IF laOpenQty[I] == 0  
      SHOW GET laSpltQty[I] DISABLE
    ELSE
      SHOW GET laSpltQty[I] ENABLE
    ENDIF
  ENDIF
ENDFOR   
lnSTotQty = &lcTmpPoLns..SpltTQty
SHOW GET lnOTotQty DISABLE
SHOW GET lnSTotQty DISABLE
*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
IF lnOTotQty > 0 AND laScrMode[3]
  SHOW GET nSPLTALL ENABLE
ELSE
  SHOW GET nSPLTALL DISABLE
ENDIF
*B039723,1 EIH 09/22/2005 [End]
IF laScrMode[3] = .T. 
  SHOW GET lcNewVend ENABLE
ENDIF
SCATTER MEMVAR MEMO
SHOW GETS WINDOW (lcWinCh3) ONLY
_CUROBJ = OBJNUM(laSpltQty[1])
=lfRefresh(lcWinCh3)

*--End of Function lfwBrows.
*!***************************************************************************
*! Name       : lfvBrows
*! Developer  : EHAB ISMAIL (EIH)
*! Date       : 06/20/2005
*! Purpose    : "Valid" function of the Dept. browse.
*!***************************************************************************
*! Calls      : None.
*!***************************************************************************
*! Parameters : None.
*!***************************************************************************
*! Returns    : None.
*!***************************************************************************
*! Example    : = lfvBrows()
*!***************************************************************************
FUNCTION lfvBrows
IF WONTOP() = (lcWindTitl)
  =gfStopBrow() 
ENDIF
*--End of Function lfvBrows
*!*************************************************************
*! Name      : lfCollect
*! Developer : Ehab Ismail (EIH)
*! Date      : 07/07/2005
*! Purpose   : Function to collect the data Temp Files.
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

IF laScrMode[2] AND !EMPTY(laData[1])
  STORE 0 TO SpltQty1 , SpltQty2 ,SpltQty3,SpltQty4,SpltQty5,SpltQty6,SpltQty7,SpltQty8,SpltTQty
  SELECT POSHDR
  IF SEEK('P'+laData[1])
    SCATTER MEMVAR MEMO
    SELECT (lcTmpPoHdr)
    APPEND BLANK
    GATHER MEMVAR MEMO
    lcVendor = &lcTmpPoHdr..vendor
    SELECT POSLN
    =SEEK('P'+laData[1])
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1] FOR trancd = '1'
      SCATTER MEMVAR MEMO 
      SELECT (lcTmpPoLns)
      APPEND BLANK
      *B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
      m.NSPLTALL = 0
      *B039723,1 EIH 09/22/2005 [End]
      GATHER MEMVAR MEMO
      SELECT POSLN
    ENDSCAN
    SELECT POSLN
    =SEEK('P'+laData[1])
    *SUPTRACT RECIVED QTY'S FROM lcTmpPoLns
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1] FOR trancd = '5'    
      SELECT (lcTmpPoLns)
      =SEEK('P'+laData[1]+POSLN.style)
      
      FOR I = 1 TO 8
        LCI =ALLTRIM(STR(I))
        REPLACE &lcTmpPoLns..QTY&LCI  WITH  &lcTmpPoLns..QTY&LCI - POSLN.QTY&LCI
      ENDFOR
      REPLACE &lcTmpPoLns..TotQTY  WITH  &lcTmpPoLns..TotQTY  - POSLN.TotQTY
    ENDSCAN
  ENDIF
  SELECT (lcTmpPoLns)
  LOCATE
ENDIF

*--End of Function lfCollect.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfOldvalue
lcOldValue = EVALUATE(SYS(18))

*-- End Of Function lfoldvalue.
*!*************************************************************
*! Name      : gfCPBrows 
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
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
FUNCTION gfCPBrows
*B607918,1 TMI [Start] use the actual name of the base file
*SELECT (lcBaseFile)
SELECT POSHDR
*B607918,1 TMI [End  ] 
=gfBrows()
laData[1] = POSHDR.PO
laScrMode   = .F.
laScrMode[2]= .T.
=lpShow()
=lfBrows()
SHOW GETS
*-- End of gfCpBrows.
*!*************************************************************
*! Name      : lpTopScr
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Top record in Navigation pb.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpTopScr
SELECT POSHDR
SET FILTER TO POSHDR.cstytype = 'P' AND  (POSHDR.STATUS = 'H') AND POSHDR.Open > 0 
GO TOP
laData[1] = POSHDR.PO
IF SEEK('P'+laData[1],'POSHDR')
  =lpShow()
  =lfBrows()
ENDIF
SELECT POSHDR
SET FILTER TO 

*-- End OF lpTopScr.
*!*************************************************************
*! Name      : lpBtmScr
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Bottom record in Navigation pb.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpBtmScr
SELECT POSHDR
SET FILTER TO POSHDR.cstytype = 'P' AND  (POSHDR.STATUS = 'H') AND POSHDR.Open > 0 
GO BOTTOM
laData[1] = POSHDR.PO
IF SEEK('P'+laData[1],'POSHDR')
  =lpShow()
  =lfBrows()
ENDIF

SELECT POSHDR
SET FILTER TO
*-- End OF lpBtmScr.
*!*************************************************************
*! Name      : lpNxtScr
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Next record in Navigation pb.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpNxtScr
SELECT POSHDR
SET FILTER TO POSHDR.cstytype = 'P' AND  (POSHDR.STATUS = 'H') AND POSHDR.Open > 0 
IF !EOF() AND !(cstytype = 'P' AND STATUS = 'H' AND POSHDR.Open > 0)
  SKIP
ENDIF
laData[1] = POSHDR.PO
=lpShow()
=lfBrows()
SELECT POSHDR
SET FILTER TO

*-- End OF lpNxtScr
*!*************************************************************
*! Name      : lpPrvScr
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Previus record in Navigation pb.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpPrvScr

SELECT POSHDR
SET FILTER TO cstytype = 'P' AND STATUS = 'H' AND POSHDR.Open > 0 

IF !EOF() AND !(cstytype = 'P' AND STATUS = 'H' AND POSHDR.Open > 0)
  SKIP -1
ENDIF
laData[1] = POSHDR.PO
=lpShow()
=lfBrows()

SELECT POSHDR
SET FILTER TO

*-- End OF lpPrvScr.
*!*************************************************************
*! Name      : lpTab
*! Developer : Ehab Ismail (EIH)
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
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
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Ehab Ismail (EIH)
*! Date      : 06/20/2005
*! Purpose   : Save/Update.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpSavScr
PRIVATE lnAlias ,lnRecno
DIMENSION laPoLnsQty[9]
STORE .F. TO llContinue 
STORE 0 TO laPoLnsQty
STORE 0 TO lnTotSplit 
SELECT (lcTmpPoHdr)
lcNewPO = gfSequence('PO','','',&lcTmpPoHdr..CDIVISION)

STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5 ,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5
           

SELECT (lcTmpPoLns)
LOCATE

SELECT POSLN
=SEEK('P'+laData[1]) 
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1] FOR trancd = '1'
  SCATTER MEMVAR MEMO
  lnRecno = RECNO()
  
  IF &lcTmpPoLns..SpltTQty > 0
    llContinue = .T.
    lnTotSplit = lnTotSplit + &lcTmpPoLns..SpltTQty
    
    lnTCost1 = lnTCost1 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Ncost1 )
    lnTCost2 = lnTCost2 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Ncost2 )
    lnTCost3 = lnTCost3 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Ncost3 )
    lnTCost4 = lnTCost4 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Ncost4 )
    lnTCost5 = lnTCost5 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Ncost5 )    
    
    lnTFCost1= lnTFCost1+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Necost1 )    
    lnTFCost2= lnTFCost2+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Necost2 )    
    lnTFCost3= lnTFCost3+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Necost3 )    
    lnTFCost4= lnTFCost4+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Necost4 )        
    lnTFCost5= lnTFCost5+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..Necost5 )    
       
    APPEND BLANK
    GATHER MEMVAR MEMO
    FOR I = 1 TO 8
      LCI =ALLTRIM(STR(I))
      REPLACE POSLN.QTY&LCI WITH &lcTmpPoLns..SpltQty&LCI
    ENDFOR
    REPLACE POSLN.TotQTY   WITH  &lcTmpPoLns..SpltTQty , ;
            POSLN.trancd    WITH  '5'
             
    *APPEND FOR NEW PO
    APPEND BLANK
    GATHER MEMVAR MEMO
    FOR I = 1 TO 8
      LCI =ALLTRIM(STR(I))
      REPLACE POSLN.QTY&LCI WITH &lcTmpPoLns..SpltQty&LCI
    ENDFOR
    REPLACE POSLN.TotQTY   WITH  &lcTmpPoLns..SpltTQty , ;
            POSLN.VENDOR    WITH  lcNewVend            , ;
            POSLN.po        WITH  lcNewPO               
           
  ENDIF
  GOTO lnRecno
  SELECT (lcTmpPoLns)
  SKIP
  SELECT POSLN
ENDSCAN     &&END OF lcTmpPoLNS

IF !llContinue 
  =gfModalGen('INM00000B00000','','',''," Can't generate PO with empty split Qty ")

  llmessag = .T.
  STORE .F. TO llShow,llCSave
  RETURN llmessag
ELSE
  STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
  SELECT (lcTmpPoHdr)
  SCATTER MEMVAR MEMO
  SELECT POSHDR
  APPEND BLANK
  GATHER MEMVAR MEMO 
  

  REPLACE POSHDR.OPEN      WITH  lnTotSplit        ,;
          POSHDR.nstyorder WITH  lnTotSplit        ,;
          POSHDR.CANCEL    WITH  0                 ,;
          POSHDR.RECEIVE   WITH  0                 ,;
          POSHDR.VENDOR    WITH  lcNewVend         ,;
          POSHDR.po        WITH  lcNewPO           ,;    
          POSHDR.parentpo  WITH  laData[1]         ,;
          POSHDR.Nicost1      WITH  lnTCost1       ,;
          POSHDR.Nicost2      WITH  lnTCost2       ,;
          POSHDR.Nicost3      WITH  lnTCost3       ,;          
          POSHDR.Nicost4      WITH  lnTCost4       ,;                    
          POSHDR.Nicost5      WITH  lnTCost5       ,;                              
          POSHDR.Nfcost1      WITH  lnTFCost1      ,;                              
          POSHDR.Nfcost2      WITH  lnTFCost2      ,;                              
          POSHDR.Nfcost3      WITH  lnTFCost3      ,;                              
          POSHDR.Nfcost4      WITH  lnTFCost4      ,;                              
          POSHDR.Nfcost5      WITH  lnTFCost5      ,;                                                                      
          POSHDR.pototal      WITH  lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5                  

          
  
  LOCATE
  =SEEK('P'+laData[1]) 
  *we must update in canceled 
  REPLACE POSHDR.OPEN    WITH  POSHDR.OPEN - lnTotSplit  ,; 
          POSHDR.CANCEL  WITH  POSHDR.CANCEL + lnTotSplit 
  
  IF POSHDR.OPEN = 0
    REPLACE POSHDR.status   WITH  'X' 
  ENDIF
  
  *B607918,1 TMI [Start] empty this variable to prevent updating the user defined field cconpo with empty value
  *                           This bug happened as follows :
  *                           The screen mode is changed to Select mode in the following line, in this step the laUsrField
  *                           is empitied, but the GFCPSAVE function is called after the following lines automatically with the empty array
  *                           and update this value in the user defined fields
  *                           The function that make this update checks that the variable laBaseFile is not empty, so making it empty string 
  *                           will help the updating process will not take place.  
  lcBaseFile = ''  
  *B607918,1 TMI [End  ] 
  
  laScrMode[3] = .F.
  laScrMode[1] = .T.
  SHOW GET ladata[1] ENABLE
  SHOW GETS
  lcNewVend = ''
  
  *-- Give the numbers of the generated PO
  =gfModalGen('INM00000B00000','','',''," Generated new p/o#  "+ ALLTRIM(lcNewPO))
  =lfGenCstSH()
  *FOR COST SHEET
  
  lcNewPO   = ''

ENDIF
  *-- End Of lpSavScr.
*--------------------------------MAIN PROG ------------------------------------------
*!***************************************************************
* PROG: lfBrowPo
* DESC: PROGRAM TO BROWSE THE STYLE PURCHASE ORDER
* PARA: THE PO AND VENDOR IS PASSED AS A PARAMETER
*PARAMETER :
*--------------------------------------------------------------
*!***************************************************************
FUNCTION lfBrowPo
PRIVATE lcBrFields,lcAlias
lcTitle  = 'Select Purchase Order'
lcAlias  = ALIAS()   && Save the current alias.
lcBrFields =   [PO        :R :H='P/O #':8,]+;
  	           [Status    :R :H='S':2,]+;
               [Vendor    :R :H='Vendor':11,]+;
	           [Entered   :R :H='Entered':10,]+;
	           [Complete  :R :H='Complete':10,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]

SELECT POSHDR
SET ORDER TO TAG POSHDR
=SEEK('P'+laData[1])
lcBrowExp = "FOR POSHDR.cstytype = 'P' AND  (POSHDR.STATUS = 'H') AND POSHDR.Open > 0 AND EMPTY(POSHDR.PARENTPO) "
llWasSel=gfBrows(lcBrowExp , ['laData(1)'])
SELECT (lcAlias)  
RETURN llWasSel

*:*************************************************************
*! Name      : lfvPoNo
*! Developer : Ehab Ismail (EIH)
*! Date      : 04/10/97
*! Purpose   : Valid function po#
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPoNo()
*!*************************************************************
FUNCTION lfvPoNo

IF llBrowse AND EMPTY(laData[1])
  laData[1]= "?"
ENDIF

IF !EMPTY(laData[1])
  IF llBrowse OR  !( SEEK('P'+laData[1],'POSHDR') AND  POSHDR.STATUS ='H' AND EMPTY(POSHDR.PARENTPO))
     = lfBrowPo()
     =lfBrows() 
     llBrowse = .F.
  ELSE
    laScrMode   = .F.
    laScrMode[2]= .T.
    SHOW GETS  
  ENDIF
ENDIF
*:*************************************************************
*! Name      : lfvSptQty1
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty1()
*!*************************************************************
FUNCTION lfvSptQty1
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty1 < laSpltQty[1]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[1] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty1 WITH 0  , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty1 WITH laSpltQty[1] , ;
        &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  


*:*************************************************************
*! Name      : lfvSptQty2
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty2()
*!*************************************************************
FUNCTION lfvSptQty2
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty2 < laSpltQty[2]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[2] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty2 WITH 0  , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty2 WITH laSpltQty[2] , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty3
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty3()
*!*************************************************************
FUNCTION lfvSptQty3
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty3 < laSpltQty[3]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[3] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty3 WITH 0  ,;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty3 WITH laSpltQty[3] , ;
        &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE


*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty4
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty4()
*!*************************************************************
FUNCTION lfvSptQty4
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty4 < laSpltQty[4]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[4] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty4 WITH 0 , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty4 WITH laSpltQty[4] , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE
*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty5
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty5()
*!*************************************************************
FUNCTION lfvSptQty5
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty5 < laSpltQty[5]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[5] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty5 WITH 0  , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty5 WITH laSpltQty[5] ,;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty6
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty1 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty6()
*!*************************************************************
FUNCTION lfvSptQty6
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty6 < laSpltQty[6]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[6] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty6 WITH 0  ,;
		  &lcTmpPoLns..SpltTQty WITH lnSTotQty  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty6 WITH laSpltQty[6] ,;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty7
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty7 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty7()
*!*************************************************************
FUNCTION lfvSptQty7
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty7 < laSpltQty[7]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[7] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty7 WITH 0 , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
REPLACE &lcTmpPoLns..SpltQty7 WITH laSpltQty[7] , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
SHOW GET lnSTotQty DISABLE


*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 [End]

SELECT (lcAlias)  

*:*************************************************************
*! Name      : lfvSptQty8
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Valid function SptQty8 .
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSptQty8()
*!*************************************************************
FUNCTION lfvSptQty8
lcAlias  = ALIAS()   && Save the current alias.
SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty8 < laSpltQty[8]
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  laSpltQty[8] = 0
  lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]
  REPLACE &lcTmpPoLns..SpltQty8 WITH 0 ,;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSTotQty = laSpltQty[1] + laSpltQty[2]+laSpltQty[3]+laSpltQty[4]+laSpltQty[5]+laSpltQty[6]+laSpltQty[7]+laSpltQty[8]

REPLACE &lcTmpPoLns..SpltQty8 WITH laSpltQty[8], ;
        &lcTmpPoLns..SpltTQty WITH lnSTotQty

SHOW GET lnSTotQty DISABLE

*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
REPLACE NSPLTALL WITH 0
SHOW GET NSPLTALL
*B039723,1 EIH 09/22/2005 Add split all options on that custom.[End]

SELECT (lcAlias)  



*:*************************************************************
*! Name      : lfvNewVen
*! Developer : Ehab Ismail (EIH)
*! Date      : 04/10/97
*! Purpose   : Valid function vendor code.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvNewVen()
*!*************************************************************
FUNCTION lfvNewVen
PRIVATE lcObjNam , lcObjVal , llObjRet

IF EMPTY(lcNewVend)
  lcNewVend = "?"
ENDIF

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SET ORDER TO TAG Vencode IN APVENDOR
*-- IF The user want to Browse or if the Vendor he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR')
  llBrowse = .T.
  llObjRet = gfApVnBrow(@lcObjVal,.F.,'S')
  lcObjVal = IIF(llObjRet , lcObjVal , SPACE(01))
  lcNewVend= lcObjVal 
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
SELECT (lcTmpPoHdr)
REPLACE NewVend WITH lcNewVend
SHOW GET lcNewVend ENABLE 
IF EMPTY(lcNewVend)    
  _CUROBJ = OBJNUM(lcNewVend)
ELSE
  _CUROBJ = OBJNUM(laSpltQty[1])  
ENDIF

*-- End OF lfvNewVen



*:*************************************************************
*! Name      : lfvBrowVnd
*! Developer : Ehab Ismail Hamed
*! Date      : 07/05/2005
*! Purpose   : Brow function vendor code.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvBrowVnd()
*!*************************************************************
FUNCTION lfvBrowVnd
llBrowse = .T.
llObjRet = gfApVnBrow(@lcNewVend,.F.,'S')
lcNewVend= IIF(llObjRet , lcNewVend , SPACE(01))
llBrowse = .F.
SELECT (lcTmpPoHdr)
REPLACE NewVend WITH lcNewVend
SHOW GET lcNewVend ENABLE    
_CUROBJ = OBJNUM(lcNewVend)

*!*************************************************************
*! Name      : lfvGenCstSH
*! Developer : Ehab Ismail (EIH)
*! Date      : 04/10/97
*! Purpose   : Function to generate the cost sheet.
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
FUNCTION lfGenCstSH

lcCrtCSH   = gfGetMemVar('M_CRTCSTSH')

IF EMPTY(lcCrtCSH)
  lcCrtCSH = 'L'
ENDIF
  
lcAType = 'P'  
IF lcCrtCSH = 'I' 
  *--Would you like to create the purchase order cost sheet ?
  IF gfModalGen('QRM34064B34001','DIALOG',IIF(lcAType='D','Dye Order','PO')) = 2
     
     laScrMode=.F.
     laScrMode[1]= .T.
     llNoShow    = .F.
     llCallShow  = .T.
     llShow      = .F.
     SHOW GET ladata[1] ENABLE
     lcNewVend = ''
     lcNewPO   = ''
     SHOW GETS
     RETURN
  ENDIF  
ENDIF

laScrMode=.F.
laScrMode[1]= .T.
llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.

lcParameter = "'" + lcNewPO + "'"+IIF(lcCrtCSH="T",",.T.",',.F.')
DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
RETURN


*B039723,1 EIH 09/22/2005 Add split all options on that custom.[Begin]
*!*************************************************************
*! Name      : lfvSpltAll
*! Developer : Ehab Ismail (EIH)
*! Date      : 09/12/2005
*! Purpose   : Function To Split All QTY in The Current Line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSpltAll()
*!*************************************************************
FUNCTION lfvSpltAll
STORE 0 TO lnSTotQty
IF &lcTmpPoLns..nSpltAll = 1
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    laSpltQty[I] = laOpenQty[I] 
    REPLACE &lcTmpPoLns..SpltQty&LCI WITH &lcTmpPoLns..QTY&LCI
    lnSTotQty = lnSTotQty + &lcTmpPoLns..QTY&LCI
    SHOW GET laSpltQty[I]
  ENDFOR
  REPLACE &lcTmpPoLns..SpltTQty WITH lnSTotQty 
  SHOW GET lnSTotQty
ELSE
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    laSpltQty[I] = 0 
    REPLACE &lcTmpPoLns..SpltQty&LCI WITH 0
    SHOW GET laSpltQty[I]
  ENDFOR
  REPLACE &lcTmpPoLns..SpltTQty WITH 0
  lnSTotQty  = 0
  SHOW GET lnSTotQty
ENDIF
*B039723,1 EIH 09/22/2005 [End]


FUNCTION lfvRunRep
wait window '44444'

*-- end of lfvRunRep.

*!*************************************************************
*! Name      : gfvUsrFld
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 01/03/2007
*! Purpose   : A modified version to override the global function "gfvUsrFld" so that the user can not update the user defined fields from 
*!             the split screen.
*!*************************************************************
*B607918,1 
FUNCTION gfvUsrFld
=gfOpGrid(SUBSTR(gcBaseWind,4),.T.,'XP')  && passing the 'XP' parameter means that the option grid is opened in view mode only

*-- end of gfvUsrFld.