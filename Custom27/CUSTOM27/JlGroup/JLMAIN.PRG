*!**************************************************************************
*! Name      : JLMAIN.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/15/2001
*! Purpose   : J & L Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*B605251,1 Fix bug of not getting the default Finishing Level.
*B605487,1 MHM 02/24/2002 make location ediatble
*B605969,1 RAE 06/04/2002 Remove the thread color sequence duplication from THREADCL file.
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfJLOrdSav
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/15/2001
*! Purpose   : Save data to BomVar & Custom Thread file. 
*! Reference : C102231,1
*!**************************************************************************
*! Example   : lfJLOrdSav()
*!**************************************************************************
*
FUNCTION lfJLOrdSav

SELECT (lcT_BomVar)
LOCATE
*IF !EOF() AND (USED('ThreadCl') OR (laScrMode[4] AND !EMPTY(laData[43])))
IF RECCOUNT() > 0  
  *-- Open the thread color file.
  *B804365,1 Change index to be on Serial instead of Thread Color. [Begin]
  *llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadCl','SH')
  llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadClS','SH')
  *B804365,1 Change index to be on Serial instead of Thread Color. [End]
  
  PRIVATE lcAlias , lcSetDelet , lcOrdLnTag , lcBomVrTag
  lcAlias = ALIAS()  
  lcSetDelet = SET('DELETE')
  SET DELETE OFF
  
  IF laScrMode[3]
    PRIVATE lcTmpBomV2
    lcTmpBomV2 = gfTempName()
    
    SELECT BomVar
    lcBomVrTag = TAG()
    SET ORDER TO TAG cDsgnCode

    COPY STRUCTURE TO (gcWorkDir+lcTmpBomV2) WITH CDX
    USE (gcWorkDir+lcTmpBomV2) IN 0
  ENDIF  

  SELECT (lcOrdLine)
  lcOrdLnTag = TAG()
  SET ORDER TO TAG OrdLine

  *-- If Edit mode
  IF laScrMode[3]
    SCAN
      IF DELETED()
        SELECT (lcT_BomVar)
        SCAN
          SCATTER MEMVAR
          m.cItmMsjor = &lcOrdLine..Style
          m.LineNo    = &lcOrdLine..LineNo
         
          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
          DELETE
        ENDSCAN
      ELSE
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcOrdLine..LineNo
        SCAN
          SCATTER MEMVAR
          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
         
          IF DELETED(lcT_BomVar)
            DELETE
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN

    SELECT (lcTmpBomV2)
    SCAN FOR !EMPTY(cDsgnCode)
    
      *B605487,1 mhm 02/24/2002 remove cstylepos from index becuase we change it [Start]
      *IF SEEK(cIDType+cCost_ID+STR(LineNo,6)+cDsgnCode+cStylePos,'BomVar')        
      IF SEEK(cIDType+cCost_ID+STR(LineNo,6)+cDsgnCode,'BomVar')
      *B605487,1 mhm  [End]

        SELECT BomVar
        =RLOCK()                  && Lock the record after saving
        REPLACE cNameDrp1 WITH &lcTmpBomV2..cNameDrp1 , ;
                cNameDrp2 WITH &lcTmpBomV2..cNameDrp2 , ;
                cNameDrp3 WITH &lcTmpBomV2..cNameDrp3 , ;
                nStitches WITH &lcTmpBomV2..nStitches

        *B605487,1 mhm 02/24/2002 update cstylepos [Start]
        REPLACE cStylePos WITH &lcTmpBomV2..cStylePos        
        *B605487,1 mhm  [End]

        UNLOCK                      && UnLock the record after saving        
        *B605969,1 RAE Update the BOMVAR file after deleting the record. [Start] 
        IF DELETED()
          RECALL
        ENDIF
        *B605969,1 RAE [End]
        IF DELETED(lcTmpBomV2)
          =RLOCK()                  && Lock the record after saving
          DELETE
        ENDIF    
      ELSE
        IF !DELETED()
          SCATTER MEMVAR
          SELECT BomVar
          APPEND BLANK
          GATHER MEMVAR
        ENDIF        
      ENDIF
    ENDSCAN
    
    *B605059,1 Update the thread color file.   [Begin]
    *SELECT (lcTmpThClr)
    *SCAN FOR !EMPTY(cThreadCl)
    *  *B804365,1 Seek on Serial instead of Thread color. [Begin]
    *  *IF SEEK(cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl,'ThreadCl')  
    *  IF SEEK(cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos + cSerial,'ThreadCl')
    *  *B804365,1 Seek on Serial instead of Thread color. [End]
    *  *B804439,1 Scatter the record. [Begin]
    *    SCATTER MEMVAR MEMO
    *    *B804439,1 Scatter the record. [End]
    *    SELECT ThreadCl
    *    =RLOCK()                  && Lock the record after saving
    *    
    *    *B804439,1 Replace the Thread color code. [Begin]
    *    *REPLACE cComment1 WITH &lcTmpThClr..cComment1
    *    IF DELETED()
    *      IF !DELETED(lcTmpThClr)
    *        APPEND BLANK
    *        GATHER MEMVAR MEMO  
    *      ENDIF  
    *    ELSE
    *      REPLACE cThreadCl WITH &lcTmpThClr..cThreadCl , cComment1 WITH &lcTmpThClr..cComment1          
    *    ENDIF        
    *    *B804439,1 Replace the Thread color code. [End]
    *
    *   UNLOCK                    && Lock the record after saving
    *    IF DELETED(lcTmpThClr) OR (SEEK(cIDType + cCost_ID + STR(LineNo,6),'BomVar') AND DELETED('BomVar'))
    *      SELECT ThreadCl
    *      =RLOCK()                  && Lock the record after saving
    *      DELETE
    *    ENDIF    
    *  ELSE
    *    IF !DELETED()
    *      SCATTER MEMVAR MEMO
    *      SELECT ThreadCl
    *      APPEND BLANK
    *      GATHER MEMVAR MEMO
    *      IF SEEK(cIDType + cCost_ID + STR(LineNo,6),'BomVar') AND DELETED('BomVar')
    *        =RLOCK()                  && Lock the record after saving
    *        DELETE          
    *      ENDIF
    *    ENDIF        
    *  ENDIF
    *ENDSCAN
    SELECT ThreadCl    
    =SEEK("SO"+laData[1]) 
    DELETE REST WHILE cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos = "SO"+laData[1]
    SELECT (lcTmpThClr)
    SCAN FOR !EMPTY(cThreadCl) .AND. !DELETED() .AND. cCost_ID = laData[1]
      *B605969,1 RAE Update the THREADCL file. [Start]
      IF lfChkBom()
      *B605969,1 RAE [End]
        SELECT (lcTmpThClr)
        SCATTER MEMVAR MEMO
        SELECT ThreadCl
        =SEEK("SO"+laData[1]) 
        LOCATE REST WHILE cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos = "SO"+laData[1];
                          FOR DELETED()               
        IF FOUND()
          RECALL 
        ELSE
          APPEND BLANK
        ENDIF         
        GATHER MEMVAR MEMO
      *B605969,1 RAE [Start]
      ENDIF
      *B605969,1 RAE [End]
    ENDSCAN    
    *B605059,1 Update the thread color file.   [End]

    *B804368,1 Update the Pick ticket type. [Begin]
    PRIVATE llDsgExist
    llDsgExist = .F.
    IF SEEK("SO"+laData[1],'BomVar')
      SELECT BomVar
      SCAN REST WHILE cidtype + ccost_id + STR(lineno,6) = "SO" + laData[1]
        IF !DELETED()
          llDsgExist = .T.
          EXIT
        ENDIF  
      ENDSCAN
      
      PRIVATE llOpnPikTk
      llOpnPikTk = gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'OrdPik','SH')

      SELECT PikTkt
      IF SEEK(laData[1])
        SCAN REST WHILE Order + PikTkt = laData[1]
          REPLACE cPickType WITH IIF(llDsgExist,"A","B")
        ENDSCAN
      ENDIF
      IF llOpnPikTk
        USE IN PikTkt
      ENDIF      
    ENDIF
    *B804368,1 Update the Pick ticket type. [End]

  ELSE           && Add mode laScrMode[4].
    SCAN
      IF !DELETED()
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcOrdLine..LineNo , ;
                   cCost_ID WITH laData[1]
        SELECT BomVar
        APPEND FROM (gcWorkDir + lcT_BomVar) FOR !DELETED() AND !EMPTY(cDsgnCode)
      ENDIF
    ENDSCAN
    
    SELECT (lcTmpThClr)  
    REPLACE ALL cCost_ID WITH laData[1]
    
    *B804439,1 Add only undeleted records. [Begin]
    *SELECT ThreadCl    
    *APPEND FROM (gcWorkDir + lcTmpThClr) FOR !DELETED() AND !EMPTY(cThreadCl)
    SCAN FOR !DELETED() AND !EMPTY(cThreadCl)
      SCATTER MEMVAR MEMO
      INSERT INTO ThreadCl FROM MEMVAR
    ENDSCAN
    *B804439,1 Add only undeleted records. [End]
    
  ENDIF
  *-- Endif of Edit mode

  SELECT (lcOrdLine)
  SET ORDER TO TAG (lcOrdLnTag)

  IF laScrMode[3]
    SELECT BomVar
    SET ORDER TO TAG (lcBomVrTag)
  ENDIF  

  IF USED('BomVar2')
    USE IN BomVar2
  ENDIF

  SELECT (lcTmpThClr)
  ZAP
  
  SET DELETE &lcSetDelet
  SELECT (lcAlias)    && Restore old alias.
  
ENDIF

*B804439,1 Zap Temp thread color file. [Begin]
=lfJLZapTmp()
*B804439,1 Zap Temp thread color file. [End]
*-- End of lfJLOrdSav.

*!**************************************************************************
*! Name      : lfJLAddBar
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/15/2001
*! Purpose   : Add a new custom Bar to option's pad in Sales Order screen.
*! Reference : C102231,1
*!**************************************************************************
*! Example   : lfJLOrdSav()
*!**************************************************************************
*
FUNCTION lfJLAddBar
DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Design Information Screen' SKIP FOR laScrMode[1] OR EMPTY(laData[2]) OR !llBomVarnt

*C102255,1 Add a new custom bar for J & L for Merchandise screen. [Begin] 
DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Finishing Level Screen' SKIP FOR laScrMode[1]
*C102255,1 Add a new custom bar for J & L for Merchandise screen. [End]
*-- End of lfJLAddBar.

*!**************************************************************************
*! Name      : lfRunJLInf
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/15/2001
*! Purpose   : Run custom Design Info screen from Option's pad in Sales Order Screen.
*! Reference : C102231,1
*!**************************************************************************
*! Example   : lfRunJLInf()
*!**************************************************************************
*
FUNCTION lfRunJLInf
PRIVATE lcOrdAlias

lcOrdAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF BAR() = 17 OR BAR() = 18
  PUSH MENU _MSYSMENU
  RELEASE PAD _INQUIRY OF _MSYSMENU

  DO CASE
    CASE BAR() = 17
      DO (gcAppHome+'SOJLINF.FXP') WITH "SO" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar , lnActFolder
      SHOW GET pbConfig,1 PROMPT lcConfig ENABLE
    
    *C102255,1 Run Mechandise Information screen. [Begin] 
    CASE BAR() = 18
      DO (gcScrDir+gcWinAppl+ "\SoJLMer.SPX")
    *C102255,1 Run Mechandise Information screen. [End] 
  
  ENDCASE    

  POP MENU _MSYSMENU
ELSE
  DO (gcAppHome+'SOJLINF.FXP') WITH "SO" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar , lnActFolder
  SHOW GET pbConfig,1 PROMPT lcConfig ENABLE
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcOrdAlias)
*-- End of lfRunJLInf.

*!**************************************************************************
*! Name      : lfSOJLMer
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/27/2001
*! Purpose   : Save cLevel , cLabel fields from Temp Ordline to Ordline file. 
*! Reference : C102255,1
*!**************************************************************************
*! Example   : lfSOJLMer()
*!**************************************************************************
*
FUNCTION lfSOJLMer
PRIVATE lcAlias , lcOrdLnTag , lnOrdLnRec , lcTmpTag , lnTmpRecNo
lcAlias = ALIAS()

SELECT (lcOrdLine)
*lnTmpRecNo = RECNO()
lcTmpTag = TAG()
SET ORDER TO TAG OrdLine

SELECT OrdLine
*lnOrdLnRec = RECNO()
lcOrdLnTag = TAG()
SET ORDER TO TAG OrdLine
IF SEEK(lcOrdType + laData[1])
  SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = lcOrdType + laData[1]
    IF SEEK(cOrdType + Order + STR(LineNo,6),lcOrdLine)
      =RLOCK()
      
      *B605251,1 Remove the N/A from file. [Begin]
      *REPLACE cLevel WITH &lcOrdLine..cLevel , ;
      *        cLabel WITH &lcOrdLine..cLabel
      REPLACE cLevel WITH IIF(laScrMode[4] AND &lcOrdLine..cLevel = "N/A","",&lcOrdLine..cLevel) , ;
              cLabel WITH &lcOrdLine..cLabel
      *B605251,1 Remove the N/A from file. [End]

      UNLOCK
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcOrdLine)
SET ORDER TO TAG (lcTmpTag)

SELECT OrdLine
SET ORDER TO TAG (lcOrdLnTag)

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lfSOJLMer.

*:**************************************************************************
*:* Name        : lfAccumOTS
*:* Entry       : C102237
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/16/2001
*:* Purpose     : A new line will be added right below each style sizw shown in the OTS 
*:*             : screen. This line will show the accumulative OTS balance
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAccumOTS()
*:***************************************************************************
FUNCTION lfAccumOTS
PRIVATE lnCnt
*-- loop throgh (lctmpots)
SELECT (lcTmpots)
GO TOP
*-- For each line in (lctmpots) add accumulative quantities line
FOR lnCnt = 1 TO RECCOUNT()-1
  GOTO lnCnt
  SCATTER MEMVAR
  APPEND BLANK
  REPLACE STYLE  WITH m.STYLE         ,;
          SZCnt  WITH m.SZCnt         ,;
          nQty1  WITH m.nQty1         ,;
          nQty2  WITH nQty1+m.nQty2   ,;
          nQty3  WITH nQty2+m.nQty3   ,;
          nQty4  WITH nQty3+m.nQty4   ,;
          nQty5  WITH nQty4+m.nQty5   ,;
          nQty6  WITH nQty5+m.nQty6   ,;
          nQty7  WITH nQty6+m.nQty7   ,;
          nQty8  WITH nQty7+m.nQty8   ,;
          nQty9  WITH nQty8+m.nQty9   ,;
          nQty10 WITH nQty9+m.nQty10  ,;
          nQty11 WITH m.nQty11        ,;
          nQty12 WITH m.nQty12            
NEXT
*-- end of lfAccumOTS.

*!**************************************************************************
*! Name      : lfAddBarJL
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/07/2001
*! Purpose   : Add a new custom Bar to option's pad in Sales Order screen.
*! Reference : C102357,1
*!**************************************************************************
*! Example   : lfAddBarJL()
*!**************************************************************************
*
FUNCTION lfAddBarJL
IF !llImpCost
  DEFINE BAR 11 OF _OPTIONPOP PROMPT '\<Design Information Screen' SKIP FOR laScrMode[1]
ELSE
  DEFINE BAR 10 OF _OPTIONPOP PROMPT '\<Design Information Screen' SKIP FOR laScrMode[1]
ENDIF
*--end of lfAddBarJL.

*!**************************************************************************
*! Name      : lfRunInfJL
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/07/2001
*! Purpose   : Run custom Design Info screen from Option's pad in Sales Order Screen.
*! Reference : C102357,1
*!**************************************************************************
*! Example   : lfRunInfJL()
*!**************************************************************************
*
FUNCTION lfRunInfJL
PRIVATE lcPoAlias

lcPoAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF (BAR() = 10 .AND. llImpCost) .OR. (BAR() = 11 .AND. !llImpCost)
  PUSH MENU _MSYSMENU
  RELEASE PAD _OPTIONPOP OF _MSYSMENU
  DO (gcAppHome+'SOJLINF.FXP') WITH "AD" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar , lnActFolder
  POP MENU _MSYSMENU
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcPoAlias)
*-- End of lfRunInfJL.

*!**************************************************************************
*! Name      : lfUpdIssAd
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/13/2001
*! Purpose   : custom issue adornment order for JL.
*! Reference : C102358,5
*!**************************************************************************
*! Example   : lfUpdIssAd()
*!**************************************************************************
*
FUNCTION LFUPDISSAD

*B804493,1 AMH Add new variables to open/close files [Start]
*PRIVATE lnAlias, lcOldStyDy, lcOldCutPk, lcOldPosLn
*= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
*= gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
PRIVATE lnAlias, lcOldStyDy, lcOldCutPk, lcOldPosLn, llOpenOrdH, llOpenOrdL, llOpenBomv, llOpenThrd
*khm1
STORE .F. TO llOpenOrdH, llOpenOrdL, llOpenBomv, llOpenThrd
*khm1
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
  llOpenOrdH = .T.
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
  llOpenOrdL = .T.
ENDIF
IF !USED('BOMVAR')
  = gfOpenFile(gcDataDir+'BOMVAR','BOMVAR','SH')
  llOpenBomv = .T.
ENDIF
IF !USED('THREADCL')

  *B605503,1 AMH Open threadcl file with threadcls index [Start]
  *= gfOpenFile(gcDataDir+'THREADCL','THREADCL','SH')
  = gfOpenFile(gcDataDir+'THREADCL','THREADCLS','SH')
  *B605503,1 AMH [End]
  
  llOpenThrd = .T.
ENDIF
*B804493,1 AMH [End]

SELECT STYDYE
lcOldStyDy = SET('ORDER')
SET ORDER TO TAG Stydye

SELECT CUTPICK
lcOldCutPk = SET('ORDER')
SET ORDER TO TAG Cutpkord

SELECT POSLN
lcOldPosLn = SET('ORDER')
SET ORDER TO TAG Posln   &&*cstytype+po+style+STR(lineno,6)+trancd.

llOpenPikt = .F.
IF !USED('PIKTKT')
  =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH')
  llOpenPikt = .T.
ENDIF

*--If pulled at zero.
DIME laZeroArry[18]
laZeroArry = 0

SELECT (lcTmpLine)
LOCATE
SCAN FOR TranCd = '2'
  *--Update Ordered qty.
  FOR lnI =1 TO 8
    lcI = STR(lnI,1)
    REPLACE Ord&lcI WITH Qty&lcI
  ENDFOR
  REPLACE TotOrd WITH TotQty
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewQArr

  * 1- update Posln

  *B605968,1 KHM 05/22/2002 (Begin) Changing SEEK to be IF SEEK.
  *=SEEK('A'+Po+Style+STR(Lineno,6)+'1','POSLN')
  IF SEEK('A'+Po+Style+STR(Lineno,6)+'1','POSLN')
  *B605968,1 KHM 05/22/2002 (End)
    lcTarWarCd = POSLN.cWareCode
    SELECT POSLN
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOLDQArr
    lnOldTotQy = TotQty
    =RLOCK()
    GATHER FROM laNewQArr FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    UNLOCK
  *B605968,1 KHM 05/22/2002 (Begin)
  ENDIF
  *B605968,1 KHM 05/22/2002 (End)
  
  * 2- Update Poshdr
  SELECT POSHDR
  *B605968,1 KHM 05/22/2002 (Begin) Changing SEEK to be IF SEEK.
  *=SEEK('A'+&lcTmpLine..Po)
  IF SEEK('A'+&lcTmpLine..Po)
  *B605968,1 KHM 05/22/2002 (End)
  
    =RLOCK()
    REPLACE nStyOrder WITH nStyOrder - (lnOldTotQy - POSLN.TotQty ),;
            Open      WITH Open      - (lnOldTotQy - POSLN.TotQty ),;
            nICost1   WITH nICost1   - ((lnOldTotQy - POSLN.TotQty )* POSLN.nECost1),;
            nFCost1   WITH nICost1
    UNLOCK
  *B605968,1 KHM 05/22/2002 (Begin)
  ENDIF
  *B605968,1 KHM 05/22/2002 (End)

  * 3- Update CutPick
  SELECT CUTPICK

  *B605968,1 KHM 05/22/2002 (Begin) Changing SEEK to be IF SEEK.
  *=SEEK('2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6),'CUTPICK')
  IF SEEK('2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6),'CUTPICK')
  *B605968,1 KHM 05/22/2002 (End)
  
    =RLOCK()
    GATHER FROM laNewQArr FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    IF TotQty=0
      DELETE
    ENDIF
    UNLOCK
  *B605968,1 KHM 05/22/2002 (Begin)
  ENDIF
  *B605968,1 KHM 05/22/2002 (End)

    
  * 4- Update Order files.
  SELECT ORDHDR
  
  *B605968,1 KHM 05/22/2002 (Begin) Changing SEEK to be IF SEEK.
  *=SEEK('O'+CUTPICK.Order,'ORDHDR')
  IF SEEK('O'+CUTPICK.Order,'ORDHDR')
  *B605968,1 KHM 05/22/2002 (End)
    
    =RLOCK()
    REPLACE TotCut WITH TotCut - ( lnOldTotQy - &lcTmpLine..TotQty)
    UNLOCK
  *B605968,1 KHM 05/22/2002 (Begin)
  ENDIF
  *B605968,1 KHM 05/22/2002 (End)
  
  SELECT ORDLINE

  *B605968,1 KHM 05/22/2002 (Begin) Changing SEEK to be IF SEEK.
  *=SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
  IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')      
  *B605968,1 KHM 05/22/2002 (End)  
    =RLOCK()
    REPLACE TotCut WITH TotCut - ( lnOldTotQy - &lcTmpLine..TotQty),;
            TotPik WITH TotPik - ( lnOldTotQy - &lcTmpLine..TotQty)
    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      REPLACE Cut&lcSz WITH Cut&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo]),;
              Pik&lcSz WITH Pik&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo])
    ENDFOR
    IF TotPik=0
      REPLACE PIKTKT  WITH ' ',;
              PIKDATE WITH {},;
              PICKED  WITH .F.
    ENDIF
    UNLOCK
  *B605968,1 KHM 05/22/2002 (Begin)
  ENDIF
  *B605968,1 KHM 05/22/2002 (End)
  
  *B804493,1 AMH Break down the order line into two lines [Start]
  *B804493,1 AMH for order line qty greater than the ador. order line qty
  IF TOTQTY > CUTPICK.TOTQTY
    SCATTER MEMVAR MEMO
    =RLOCK()
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE QTY&lcI.  WITH EVALUATE('CUTPICK.QTY'+lcI),;
              BOOK&lcI. WITH EVALUATE('CUTPICK.QTY'+lcI)
    ENDFOR
    REPLACE TOTQTY    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TOTBOOK   WITH TOTQTY
    UNLOCK
    PRIVATE lnLastLine
    lnLastLine = LINENO
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+CUTPICK.Order
      IF LINENO > lnLastLine
        lnLastLine = LINENO
      ENDIF
    ENDSCAN
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE LINENO    WITH lnLastLine + 1,;
            CWARECODE WITH ORDHDR.CWARECODE,;
            TOTQTY    WITH MAX(TOTQTY - CUTPICK.TOTQTY,0),;
            TOTBOOK   WITH MAX(TOTBOOK - CUTPICK.TOTQTY,0),;
            TOTCUT    WITH 0,;
            TOTPIK    WITH 0,;
            TOT_POALO WITH 0,;
            PIKTKT    WITH '',;
            PIKDATE   WITH {},;
            PICKED    WITH .F.,;
            INVOICE   WITH '',;
            INVDATE   WITH {}
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE QTY&lcI.   WITH MAX(EVALUATE('QTY'+lcI) - EVALUATE('CUTPICK.QTY'+lcI),0),;
              BOOK&lcI.  WITH MAX(EVALUATE('BOOK'+lcI) - EVALUATE('CUTPICK.QTY'+lcI),0),;
              CUT&lcI.   WITH 0,;
              PIK&lcI.   WITH 0,;
              POALO&lcI. WITH 0,;
              NPCK&lcI.  WITH 0
    ENDFOR
    =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
    IF SEEK('SO'+ORDER+STR(LINENO,6),'BOMVAR')
      SELECT BOMVAR
      PRIVATE lnBomRecNo
      SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = 'SO'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
        SCATTER MEMVAR
        lnBomRecNo = RECNO()
        m.LineNo = lnLastLine + 1
        APPEND BLANK
        GATHER MEMVAR
        IF BETWEEN(lnBomRecNo,1,RECCOUNT())
          GOTO lnBomRecNo
        ENDIF
      ENDSCAN
      IF SEEK('SO'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'THREADCL')
        SELECT THREADCL
        PRIVATE lcThrdKey
        
        *B605503,1 AMH Scan while the foll index expration [Start]
        *SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6)+cDsgnCode+cStylePos+cThreadCl =;
                        'SO'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
        SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6)+cDsgnCode+cStylePos+cSerial =;
                        'SO'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
        *B605503,1 AMH [End]
        
          SCATTER MEMVAR
          lcThrdKey = EVALUATE(KEY())
          m.LineNo = lnLastLine + 1
          APPEND BLANK
          GATHER MEMVAR
          SEEK lcThrdKey
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
  *B804493,1 AMH [End]

  *--Update Piktkt.
  SELECT PikTkt
  IF SEEK(POSLN.ShipNo)
    =RLOCK()
    REPLACE Status    WITH 'P',;
            cWareCode WITH lcTarWarCd
    UNLOCK 
  ENDIF

  * 6- Update style.
  =SEEK(POSLN.Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE WIP&lcSz WITH WIP&lcSz + laNewQArr[lnSizeNo],;
            Alo&lcSz WITH Alo&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo])
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8,;
          TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 


  SELECT STYDYE
  =SEEK(POSLN.Style+lcTarWarCd+SPACE(10))
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE WIP&lcSz WITH WIP&lcSz + laNewQArr[lnSizeNo]
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
  UNLOCK 


  * 7- Update styDye.
  IF ORDLINE.cWareCode = ORDHDR.cWareCode
    SELECT ORDLINE
    =RLOCK()
    REPLACE cWareCode WITH lcTarWarCd
    UNLOCK
  
    SELECT STYDYE
    =SEEK(POSLN.Style+ORDLINE.cWareCode+SPACE(10))
    =RLOCK()
    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      REPLACE Alo&lcSz WITH Alo&lcSz + laNewQArr[lnSizeNo],;
              Ord&lcSz WITH Ord&lcSz + laNewQArr[lnSizeNo]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
    UNLOCK 

    SELECT STYDYE
    =SEEK(POSLN.Style+ORDHDR.cWareCode+SPACE(10))
    =RLOCK()
    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      REPLACE Alo&lcSz WITH Alo&lcSz - laOLDQArr[lnSizeNo],;
              Ord&lcSz WITH Ord&lcSz - laNewQArr[lnSizeNo]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
    UNLOCK 
  ENDIF

  SELECT (lcTmpLine)
ENDSCAN

SELECT POSLN
SET ORDER TO &lcOldPosLn.

SELECT CUTPICK
SET ORDER TO &lcOldCutPk.

SELECT STYDYE
SET ORDER TO &lcOldStyDy.

SELECT (lcTmpLine)
DELETE ALL FOR clotno = 'X'

IF llOpenPikt
  USE IN PIKTKT
ENDIF

*B804493,1 AMH close files [Start]
IF llOpenOrdH
  USE IN ORDHDR
ENDIF
IF llOpenOrdL
  USE IN ORDLINE
ENDIF
IF llOpenBomv
  USE IN BOMVAR
ENDIF
IF llOpenThrd
  USE IN THREADCL
ENDIF
*B804493,1 AMH [End]

SELECT (lcTmpLine)
RETURN

*!**************************************************************************
*! Name      : lfJLPoSav
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/21/2001
*! Purpose   : Save data to BomVar & Custom Thread file. 
*! Reference : C102357,1 <NEW>
*!**************************************************************************
*! Example   : lfJLOrdSav()
*!**************************************************************************
*
FUNCTION lfJLPoSav

SELECT (lcT_BomVar)
LOCATE

*IF !EOF() AND (USED('ThreadCl') OR (laScrMode[4] AND !EMPTY(laData[43])))
IF RECCOUNT() > 0  
  *-- Open the thread color file.
  llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadCl','SH')
  
  PRIVATE lcAlias , lcSetDelet , lcOrdLnTag , lcBomVrTag
  lcAlias = ALIAS()  
  lcSetDelet = SET('DELETE')
  SET DELETE OFF

  IF laScrMode[3]
    PRIVATE lcTmpBomV2
    lcTmpBomV2 = gfTempName()
    
    SELECT BomVar
    lcBomVrTag = TAG()
    SET ORDER TO TAG cDsgnCode

    COPY STRUCTURE TO (gcWorkDir+lcTmpBomV2) WITH CDX
    USE (gcWorkDir+lcTmpBomV2) IN 0
  ENDIF  

  SELECT (lcPoLine)

  *-- If Edit mode
  IF laScrMode[3]
    SCAN
      IF DELETED()
        SELECT (lcT_BomVar)
        SCAN
          SCATTER MEMVAR
          m.cItmMsjor = &lcPoLine..Style
          m.LineNo    = &lcPoLine..LineNo
         
          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
          DELETE
        ENDSCAN
      ELSE
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcPoLine..LineNo
     
        SCAN
          SCATTER MEMVAR 

          SELECT (lcTmpBomV2)
          APPEND BLANK
          GATHER MEMVAR
         
          IF DELETED(lcT_BomVar)
            DELETE
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN

    SELECT (lcTmpBomV2)
    SCAN FOR !EMPTY(cDsgnCode)
      IF SEEK(cIDType+cCost_ID+STR(LineNo,6)+cDsgnCode+cStylePos,'BomVar')        
        SELECT BomVar
        =RLOCK()                  && Lock the record before saving
        REPLACE cNameDrp1 WITH &lcTmpBomV2..cNameDrp1 , ;
                cNameDrp2 WITH &lcTmpBomV2..cNameDrp2 , ;
                cNameDrp3 WITH &lcTmpBomV2..cNameDrp3 , ;
                nStitches WITH &lcTmpBomV2..nStitches
        UNLOCK                    && UnLock the record after saving
        
        IF DELETED(lcTmpBomV2)
          =RLOCK()                  && Lock the record after saving
          DELETE
        ENDIF    
      ELSE
        IF !DELETED()
          SCATTER MEMVAR
          SELECT BomVar
          APPEND BLANK
          GATHER MEMVAR
        ENDIF        
      ENDIF
    ENDSCAN
          
    SELECT (lcTmpThClr)
    SCAN FOR !EMPTY(cThreadCl)
      IF SEEK(cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl,'ThreadCl')  
        SELECT ThreadCl
        =RLOCK()                  && Lock the record before saving
        REPLACE cComment1 WITH &lcTmpThClr..cComment1
        UNLOCK                    && Lock the record after saving
        
        IF DELETED(lcTmpThClr) OR (SEEK(cIDType + cCost_ID + STR(LineNo,6),'BomVar') AND DELETED('BomVar'))
          SELECT ThreadCl
          =RLOCK()                  && Lock the record before saving
          DELETE
        ENDIF    
      ELSE
        IF !DELETED()
          SCATTER MEMVAR MEMO
          SELECT ThreadCl
          APPEND BLANK
          GATHER MEMVAR MEMO
          IF SEEK(cIDType + cCost_ID + STR(LineNo,6),'BomVar') AND DELETED('BomVar')
            =RLOCK()                  && Lock the record before saving
            DELETE          
          ENDIF
        ENDIF        
      ENDIF
    ENDSCAN

  ELSE           && Add mode laScrMode[4].
    SCAN
      IF !DELETED()
        SELECT (lcT_BomVar)
        REPLACE ALL LineNo WITH &lcpoline..LineNo , ;
                    cCost_ID WITH laData[1]
      
        SELECT BomVar
        APPEND FROM (gcWorkDir + lcT_BomVar) FOR !DELETED() AND !EMPTY(cDsgnCode)
      ENDIF
    ENDSCAN

    SELECT (lcTmpThClr)  
    REPLACE ALL cCost_ID WITH laData[1]
    
    SELECT ThreadCl
    APPEND FROM (gcWorkDir + lcTmpThClr) FOR !DELETED() AND !EMPTY(cThreadCl)
  ENDIF
  *-- Endif of Edit mode

  IF laScrMode[3]
    SELECT BomVar
    SET ORDER TO TAG (lcBomVrTag)
  ENDIF  

  IF USED('BomVar2')
    USE IN BomVar2
  ENDIF

  SET DELETE &lcSetDelet
  SELECT (lcAlias)    && Restore old alias.
  
ENDIF  
*-- End of lfJLPoSav.

*!**************************************************************************
*! Name      : lfRemLine
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/25/2001
*! Purpose   : custom remove line from adornment order for JL.
*! Reference : C102357,1 <NEW>
*!**************************************************************************
*! Example   : lfRemLine()
*!**************************************************************************
*
FUNCTION lfRemLine
PRIVATE lnAlias, lcOldStyDy

lnAlias = SELECT(0)
SELECT STYDYE
lcOldStyDy = SET('ORDER')
SET ORDER TO TAG Stydye

=SEEK(PosLn.Style,'STYLE')
SELECT STYLE
=RLOCK()
FOR lnSizeNo = 1 TO 8
  lcSz = STR(lnSizeNo,1)
  REPLACE INTRANS&lcSz WITH INTRANS&lcSz - PosLn.qty&lcSz
ENDFOR
REPLACE TOTINTRN WITH INTRANS1+INTRANS2+INTRANS3+INTRANS4+INTRANS5+INTRANS6+INTRANS7+INTRANS8
UNLOCK 

SELECT STYDYE
=SEEK(PosLn.Style+PosLn.cWareCode+SPACE(10))
=RLOCK()
FOR lnSizeNo = 1 TO 8
  lcSz = STR(lnSizeNo,1)
  REPLACE INTRANS&lcSz WITH INTRANS&lcSz - PosLn.qty&lcSz
ENDFOR
REPLACE TOTINTRN WITH INTRANS1+INTRANS2+INTRANS3+INTRANS4+INTRANS5+INTRANS6+INTRANS7+INTRANS8
UNLOCK 

SELECT STYDYE
SET ORDER TO &lcOldStyDy.
SELECT (lnAlias)
RETURN
*---end of lfRemLine.

*!**************************************************************************
*! Name      : lfRemAloLn
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/25/2001
*! Purpose   : custom remove allocation of removed line from adornment order for JL.
*! Reference : C102357,1 <NEW>
*!**************************************************************************
*! Example   : lfRemAloLn()
*!**************************************************************************
*
FUNCTION lfRemAloLn
PRIVATE lnAlias, lcOldCutPk, lcOldPosLn, lcTranCd

DECLARE laOLDQArr[1,8], laNewQArr[1,8]
lnAlias = SELECT(0)
SELECT CUTPICK
lcOldCutPk = SET('ORDER')
SET ORDER TO TAG Cutpkord

SELECT POSLN
lcOldPosLn = SET('ORDER')
SET ORDER TO TAG Posln   &&*cstytype+po+style+STR(lineno,6)+trancd.
lcTranCd = POSLN.TRANCD

IF SEEK('2'+PosLn.Po+STR(PosLn.LineNo,6),'CUTPICK') .AND.;
   SEEK(CSTYTYPE+PO+STYLE+STR(LINENO,6)+'6')
  SELECT STYDYE
  =SEEK(PosLn.Style+PosLn.cWareCode+SPACE(10))
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE Alo&lcSz WITH Alo&lcSz - PosLn.qty&lcSz,;
            Ord&lcSz WITH Ord&lcSz - PosLn.qty&lcSz
  ENDFOR
  REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
          TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 

  SELECT STYDYE
  =SEEK(PosLn.Style+SUBSTR(PosLn.VENDOR,1,6)+SPACE(10))
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE Alo&lcSz WITH Alo&lcSz + PosLn.qty&lcSz,;
            Ord&lcSz WITH Ord&lcSz + PosLn.qty&lcSz
  ENDFOR
  REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
          TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK
  IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
    SELECT ORDLINE
    =RLOCK()
    REPLACE CWARECODE WITH SUBSTR(POSLN.VENDOR,1,6)
    UNLOCK
  ENDIF
ENDIF

SELECT POSLN
SEEK CSTYTYPE+PO+STYLE+STR(LINENO,6)+lcTranCd
SET ORDER TO &lcOldPosLn.

SELECT CUTPICK
SET ORDER TO &lcOldCutPk.
SELECT (lnAlias)
RETURN
*---end of lfRemAloLn.

*!**************************************************************************
*! Name      : lfCancelPo
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/25/2001
*! Purpose   : custom issue adornment order for JL.
*! Reference : C102357,1 <NEW>
*!**************************************************************************
*! Example   : lfCancelPo()
*!**************************************************************************
*
FUNCTION lfCancelPo
PRIVATE lnAlias, lcOldStyDy, lcOldPosLn

lnAlias = SELECT(0)
SELECT STYDYE
lcOldStyDy = SET('ORDER')
SET ORDER TO TAG Stydye

SELECT POSLN
lcOldPosLn = SET('ORDER')
SET ORDER TO TAG Posln   &&*cstytype+po+style+STR(lineno,6)+trancd.
SEEK 'A'+POSHDR.PO

llOpenPikt = .F.
IF !USED('PIKTKT')
  =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH')
  llOpenPikt = .T.
ENDIF

SELECT PikTkt
IF SEEK(POSLN.ShipNo)
  =RLOCK()
  REPLACE Status    WITH 'O',;
          cWareCode WITH SUBSTR(POSLN.VENDOR,1,6)
  UNLOCK
ENDIF

IF llOpenPikt
  USE IN PIKTKT
ENDIF

SELECT POSLN
SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'A'+POSHDR.PO FOR TRANCD = '6'
  =SEEK(PosLn.Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE STK&lcSz     WITH STK&lcSz + PosLn.Qty&lcSz
  ENDFOR
  REPLACE TOTSTK   WITH STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8
  UNLOCK

  SELECT STYDYE
  =SEEK(PosLn.Style+SUBSTR(PosLn.VENDOR,1,6)+SPACE(10))
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE STK&lcSz     WITH STK&lcSz + PosLn.Qty&lcSz
  ENDFOR
  REPLACE TOTSTK   WITH STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8
  UNLOCK
  =lfRemLine()
  =lfRemAloLn()
  SELECT POSLN
  DELETE
ENDSCAN

SELECT POSLN
SET ORDER TO &lcOldPosLn.

SELECT STYDYE
SET ORDER TO &lcOldStyDy.
SELECT (lnAlias)
RETURN
*---end of lfCancelPo.

*!**************************************************************************
*! Name      : lfJLDummy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/20/2001
*! Purpose   : Dummy function with no use. 
*! Reference : B804324,1
*!**************************************************************************
*! Example   : lfJLDummy()
*!**************************************************************************
*
FUNCTION lfJLDummy
RETURN .T.
*-- End of lfJLDummy

*!**************************************************************************
*! Name      : lfJLZapTmp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/04/2001
*! Purpose   : Zap the static files. 
*! Reference : B804439,1
*!**************************************************************************
*! Example   : lfJLZapTmp()
*!**************************************************************************
*
FUNCTION lfJLZapTmp
PRIVATE lcAlias
lcAlias = ALIAS()

IF USED(lcTmpThClr)
  SELECT (lcTmpThClr)
  ZAP
ENDIF  

SELECT (lcAlias)    && Restore file.
*-- End of lfJLZapTmp

*!**************************************************************************
*! Name      : lfvAdorOrd
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/20/2001
*! Purpose   : Valid Adornment order Field in issue Adornment order screen.
*! Reference : B804539,1
*!**************************************************************************
*! Example   : lfvAdorOrd()
*!**************************************************************************
*
FUNCTION lfvAdorOrd
IF SEEK('2'+lcTCode,'CUTPICK')
  PRIVATE lnCurAlias, llOpnOrdHd
  lnCurAlias = SELECT(0)
  IF !USED('ORDHDR')
    llOpnOrdHd = gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
  ENDIF
  SELECT CUTPICK
  SCAN REST WHILE TranCd+CtktNo+Style = '2'+lcTCode
    IF SEEK('O'+ORDER,'ORDHDR') .AND. ORDHDR.STATUS = 'H'
      =gfModalGen('TRM00000B42000','ALERT','','',"This adornment order is assigned to "+;
                  "an on hold sales order # "+ORDER+". Cannot issue.")
      llAbort = .T.
      EXIT
    ENDIF
  ENDSCAN
  IF llOpnOrdHd
    USE IN ORDHDR
  ENDIF
  SELECT (lnCurAlias)
ENDIF
*-- End of lfvAdorOrd.

*!**************************************************************************
*! Name      : lfChkBom
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/04/2002
*! Purpose   : Get the updated values from BOMVAR to update THREADCL.
*! Reference : B605969,1
*!**************************************************************************
*! Example   : lfChkBom()
*!**************************************************************************

FUNCTION lfChkBom
PRIVATE lnAlias,llFound

lnAlias = SELECT(0)
llFound = .F.
SELECT BOMVAR
LOCATE FOR cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos = ;
           &lcTmpThClr..cIDType+&lcTmpThClr..cCost_ID+STR(&lcTmpThClr..LineNo,6)+ ;
           &lcTmpThClr..cDsgnCode+&lcTmpThClr..cStylePos

llFound = FOUND()
SELECT(lnAlias)
RETURN(llFound)
*-- End of lfChkBom.