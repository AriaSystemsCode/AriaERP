*!**************************************************************************
*! Name      : MBIMAIN.PRG
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2001
*! Purpose   : 
*! Reference : C#200217,1
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*!Modifications
*C200358,1 TMI 06/17/2002 Create a Quick Order Entry screen
*B606087,1 TMI 07/11/2002 Assure that Dyelot entred last is returned
*B606386,1 ABD 08/15/2002 Need to  use the TAB key to move between the fields 
*B606386,1 ABD            As this is the only easy way to operate - the fields on 
*B606386,1 ABD            top of the screen use the TAB key so it should 
*B606386,1 ABD            be standard for the whole screen.
*B606324,1 RAE 09/26/2002 Fix the values of ALO1 to ALO8 fields in STYLE and STYDYE files.
*B606725,1 ABD 12/25/2002 Fix bug that the ordhdr not updating with the add information
*B606959,1 ASH 02/16/2003 1- Double check that the piktkt opened with the right tag.
*B606959,4 ASH 05/06/2003 2- Double check that the pointer is at the right record in ORDHDR.
*B607116,1 ABD 05/04/2003 Ignore piktkt that hold Complete and Canncel Status from the browse.
*B607266,1 ABD 07/01/2003 ignore piktkt = '******'
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*B122433,1 TMI 05/18/2004 Add season check
*C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*C123847,1 TMI 02/17/2005 Autopick for DIR03
*!*****************************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!***************************************************************
*! Name      : lfCHNGFRM
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2001
*! Purpose   : 
*!***************************************************************
*!
FUNCTION lfCHNGFRM

lcFormName = IIF(lcRpPrtFrm = 'D' ,lcOldForm , 'ALPKTKMD')

=gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
=lfRepPltFr(lcFormName)

*!**************************************************************************
*! Name      : lfComplExp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/28/2001
*! Purpose   : To add the region to lcRpExp.
*!**************************************************************************
*! Example   : =lfComplExp()
*!**************************************************************************
*:C200240,1 , C200241,1
FUNCTION lfComplExp
IF !EMPTY(lcRegion)
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + [Customer.Region $ lcRegion]
ENDIF
*-- End of lfComplExp

*!**************************************************************************
*! Name      : lfCheckReg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/25/2001
*! Purpose   : To check for Region value if changed.
*!**************************************************************************
*! Example   : =lfCheckReg()
*!**************************************************************************
*:C200240,1 , C200241,1
FUNCTION lfCheckReg
lcRegion = ''
=ASORT(laRpTRegn)
FOR lnInd = 1 TO ALEN(laRpTRegn)
  lcRegion = lcRegion + PADR(laRpTRegn[lnInd],6) + SPACE(1)
ENDFOR
lcRegion = STRTRAN(ALLTRIM(lcRegion),' ',"|")

IF !llOGFltCh AND !(lcRegion == lcOldRegn)
  llOGFltCh = .T.
ENDIF
lcOldRegn = lcRegion
*-- End of lfCheckReg.

*!**************************************************************************
*! Name      : lfFillMovr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/25/2001
*! Purpose   : To collect data for the region mover.
*!**************************************************************************
*! Example   : =lfFillMovr()
*!**************************************************************************
*:C200240,1 , C200241,1
FUNCTION lfFillMovr
PARAMETER lfDummy
lfDummy = .T.

DIMENSION laRpSRegn[1,1],laRpTRegn[1,1]
STORE '' TO laRpSRegn,laRpTRegn

*--The Region.
SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO + ' - ' + cDiscRep ;
FROM CODES ;
WHERE cDefCode + cFld_Name + cCode_No + cDiscRep + cRltd_Nam = "NREGION" ;
AND crltField = 'N';
INTO ARRAY laRpSRegn

RETURN lfDummy
*-- End of lfFiLLMovr.

*!**************************************************************************
*! Name      : lfvRegion
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/25/2001
*! Purpose   : To open the region mover.
*!**************************************************************************
*! Example   : =lfvRegion()
*!**************************************************************************
*:C200240,1 , C200241,1
FUNCTION lfvRegion
PARAMETER lfDummy
lfDummy = .T.

= gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')
RETURN lfDummy
*-- End of lfvRegion.

*!***************************************************************
*! Name      : lfSavScr
*! Developer : Mohamed Shokry Mohamed (MHM)
*! Date      : 10/21/2001
*! Purpose   : Save screen
*!***************************************************************
*!C200236,1
FUNCTION lfSavScr
PRIVATE llAddToSO

DECLARE laDivType[1,2] 
lcDivType = ''
**- Array to get the Division Type 
laDivType[1,1] = 'CDIVTYPE'     
laDivType[1,2] = 'lcDivType'

=gfRltFld(laData[15],@laDivType,'CDIVISION')
llAddToSO  = gfGetMemVar('M_ADD_SO')   && setting For Add to existing SO
llEDITToSO = gfGetMemVar('M_EDIT_SO')  && setting For EDIT to existing SO

*C037816,1 MHM 04/06/2004 consedr Bin Location Concept in saving Piktkt[Start]
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
IF TYPE("llUseBin") = "U"
  llUseBin = .f.
ENDIF
*C037816,1 MHM [End]

*--open piktkt file
=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'PikTkt','SH'),.T.)

*-- update piktkt yes or no
llUpPkTk = .F.
*C123847,1  TMI [Start] check availble stock to allocate

*- If there is enough inventory to pick the whole order then it should be picked with 
*- no notification to the user
PRIVATE llEnough,Dir_llAloc,Dir_lnAvl,llUpdDirALO
STORE .T. TO llEnough,Dir_llAloc,llUpdDirALO
STORE 0   TO Dir_lnAvl
SELECT (lcOrdLine)
LOCATE
SCAN
  IF STYDYE.STK1 - STYDYE.ALO1 < QTY1 - PIK1 .OR. ;
     STYDYE.STK2 - STYDYE.ALO2 < QTY2 - PIK2 .OR. ;
     STYDYE.STK3 - STYDYE.ALO3 < QTY3 - PIK3 .OR. ;
     STYDYE.STK4 - STYDYE.ALO4 < QTY4 - PIK4 .OR. ;
     STYDYE.STK5 - STYDYE.ALO5 < QTY5 - PIK5 .OR. ;
     STYDYE.STK6 - STYDYE.ALO6 < QTY6 - PIK6 .OR. ;
     STYDYE.STK7 - STYDYE.ALO7 < QTY7 - PIK7 .OR. ;
     STYDYE.STK8 - STYDYE.ALO8 < QTY8 - PIK8
     llEnough = .F.
  ENDIF
  *- tmi 03/02/2005
  *- The value of the variable "Dir_lnAvl" is not intented , I check that if there is 
  *- a not picked qty and there is a stock available to allocate then add a positive value to 
  *- "Dir_lnAvl" , if it is still 0 then a message appears says that "there is no stock to allocate"
  *- tmi 03/02/2005
  Dir_lnAvl = Dir_lnAvl +;
            IIF(QTY1>PIK1 .AND. STYDYE.STK1-STYDYE.ALO1>0 , STYDYE.STK1-STYDYE.ALO1 ,0 )+;
            IIF(QTY2>PIK2 .AND. STYDYE.STK2-STYDYE.ALO2>0 , STYDYE.STK2-STYDYE.ALO2 ,0 )+;
            IIF(QTY3>PIK3 .AND. STYDYE.STK3-STYDYE.ALO3>0 , STYDYE.STK3-STYDYE.ALO3 ,0 )+;
            IIF(QTY4>PIK4 .AND. STYDYE.STK4-STYDYE.ALO4>0 , STYDYE.STK4-STYDYE.ALO4 ,0 )+;
            IIF(QTY5>PIK5 .AND. STYDYE.STK5-STYDYE.ALO5>0 , STYDYE.STK5-STYDYE.ALO5 ,0 )+;
            IIF(QTY6>PIK6 .AND. STYDYE.STK6-STYDYE.ALO6>0 , STYDYE.STK6-STYDYE.ALO6 ,0 )+;
            IIF(QTY7>PIK7 .AND. STYDYE.STK7-STYDYE.ALO7>0 , STYDYE.STK7-STYDYE.ALO7 ,0 )+;
            IIF(QTY8>PIK8 .AND. STYDYE.STK8-STYDYE.ALO8>0 , STYDYE.STK8-STYDYE.ALO8 ,0 )
ENDSCAN

*//* *- If the order can only be partially picked i.e. at least one item can be picked - then the 
*//* *- user should be taken straight to the Sales Order Allocation screen with the Order Details 
*//* *- pre-loaded and the options set to Pick
*//* IF ASCAN(laEvntTrig,PADR('SODIRALO',10))<>0 
*//*   SAVE TO (gcWorkDir+lcFolder) ALL LIKE Dir_*
*//*   IF !llEnough .AND. laScrMode[4]
*//*     *- return to SOUPDATE.PRG and from there call the trigger "SODIRALO" that calls the alordal.prg and allocates
*//* *    SAVE TO (gcWorkDir+lcFolder) ALL LIKE lnAvlbl
*//*     *C123847,1  TMI [Start] this is for test , remove comment
*//*     RETURN .F.
*//*     *C123847,1  TMI [End  ] 
*//*   ENDIF
*//* ENDIF
*//* *C123847,1  TMI [End  ] 

IF laScrMode[4]

  DO CASE
    CASE lcDivType = 'I'                  && Inquire
        *--Would you like to allocate sales order?
        *C123847,1  TMI [Start] 
        IF Dir_lnAvl > 0
          *C123847,1  TMI [End  ] 
          *--   <YES>-->1     <NO>-->2
          IF gfModalGen('QRM32097B32000','DIALOG') = 2
            RETURN .F.
          ENDIF
          llUpPkTk = .T.
          *C123847,1  TMI [Start] 
        ELSE
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is no stock available to Allocate.')
          RETURN .F.
        ENDIF
        *C123847,1  TMI [End  ] 
    CASE lcDivType = 'N' OR EMPTY(lcDivType)        && Never, Use the normal way for saving SO
       RETURN .F.
  ENDCASE
  SELECT (lcOrdLine)
  lcOldOrd = ORDER()
  SET ORDER TO 
  *C123847,1  TMI [Start] 
  IF EOF()
    GO BOTTOM
  ENDIF
  *C123847,1  TMI [End  ] 
  lnRecNo = RECNO()

  SCAN

    *C123847,1  TMI [Start] update pik qty's with the available only
    IF ASCAN(laEvntTrig,PADR('SODIRALO',10))=0
    *C123847,5  TMI [End  ] 

      REPLACE PIK1   WITH QTY1    ,;
              PIK2   WITH QTY2    ,;
              PIK3   WITH QTY3    ,;
              PIK4   WITH QTY4    ,;
              PIK5   WITH QTY5    ,;
              PIK6   WITH QTY6    ,;
              PIK7   WITH QTY7    ,;
              PIK8   WITH QTY8    ,;
              TotPik WITH TotQTY  

      *C123847,1  TMI [Start] update pik qty's with the available only
    ELSE    
      =lfUpdPk()
    ENDIF
    *C123847,1  TMI [End  ] 
  ENDSCAN
  *C123847,5  TMI [End  ] 
    
  SET ORDER TO &lcOldOrd
  GOTO lnRecNo
  *--Check for llAddToSO
  IF llAddToSO AND !llMultiSt 
    lcPiktkt=lfBrowse()
  ELSE
    lcPiktkt =''
  ENDIF
  *--End check
ELSE
  *200236,4 [Start]
  IF lcDivType = 'N' OR EMPTY(lcDivType)        && Never, Use the normal way for Editing SO
    RETURN .F.
  ENDIF
  *200236,4 [End]

  *C123847,1  TMI [Start] 
  IF ASCAN(laEvntTrig,PADR('CHKAVL',10))<>0 

    lcPiktkt1 = lfChkAvl()
    SELECT &lcOrdLine
  
  ELSE
  
    *C123847,1  TMI [End  ] 

    *--Would you like to allocate existing sales order?
    *--   <YES>-->1     <NO>-->2
    IF llEditToSo AND gfModalGen('QRM32098B32000','DIALOG') = 1
      llEditToSo = .T.    && Add to existing Pick Ticket
    ELSE
      llEditToSo = .F.   && Generate new Pick Ticket
    ENDIF

    *--if in edit mode will check 
    *--1 - if normal SO and we add to new line
    *--2 - we add to picked SO
    SELECT (lcOrdLine)
    lnRecNo = RECNO()
    lcPiktkt1 = ''
    LOCATE FOR Store = m.Store AND !EMPTY(PikTkt)
    IF FOUND()
      lcPiktkt1 = piktkt
      m.pikdate = pikdate 
      m.picked  = picked  
    ENDIF
    
    *200236,5 MHM [Start]
    IF !llEDITToSo 
      RETURN .F.
    ENDIF
    *200236,5 MHM [End]
  
    *C123847,1  TMI [Start] close the above if statemnet
  ENDIF
  *C123847,1  TMI [End  ] 
  
  IF EMPTY(lcPiktkt1)             && means normal SO
    *200236,5 MHM [Start]
    *IF !llEDITToSo 
    *  RETURN .F.
    *ENDIF
    *200236,5 MHM [End]
  
    lcPiktkt =''
    
    *C123847,5  TMI [Start] 
    IF ASCAN(laEvntTrig,PADR('SODIRALO',10))=0  
       *C123847,5  TMI [End  ] 
     
      *C200236,4 to update QTY in case of multi store and store not found[start]
      SCAN FOR Store = m.Store
        *- Check the Date of pick tcket in case of addning new line. 
        REPLACE PIK1   WITH QTY1      ,;
              PIK2   WITH QTY2      ,;
              PIK3   WITH QTY3      ,;
              PIK4   WITH QTY4      ,;
              PIK5   WITH QTY5      ,;
              PIK6   WITH QTY6      ,;
              PIK7   WITH QTY7      ,;
              PIK8   WITH QTY8      ,;
              TotPik WITH TotQTY  
      ENDSCAN
      *C200236,4 [End]
      
      *C123847,5  TMI [Start] 
    ENDIF
    *C123847,5  TMI [End  ] 

  ELSE
    lcPiktkt = &lcOrdLine..piktkt            && picked SO 
    *C123847,1  TMI [Start] update the variable "lcPiktkt" with the selected value from lcPiktkt1
    IF ASCAN(laEvntTrig,PADR('SODIRALO',10))<>0  
      lcPiktkt = lcPiktkt1
    ENDIF
    *C123847,1  TMI [End  ] 
    
    *C123847,5  TMI [Start] 
    IF ASCAN(laEvntTrig,PADR('SODIRALO',10))=0  
      *C123847,5  TMI [End  ] 

      SCAN FOR Store = m.Store
      
        *- Check the Date of pick tcket in case of addning new line. 
      
        REPLACE piktkt  WITH lcPiktkt ,;
              pikdate WITH m.pikdate,;
              Picked WITH .T.       ,;
              PIK1   WITH QTY1      ,;
              PIK2   WITH QTY2      ,;
              PIK3   WITH QTY3      ,;
              PIK4   WITH QTY4      ,;
              PIK5   WITH QTY5      ,;
              PIK6   WITH QTY6      ,;
              PIK7   WITH QTY7      ,;
              PIK8   WITH QTY8      ,;
              TotPik WITH TotQTY  
      ENDSCAN
      
      *C123847,5  TMI [End  ] 
    ENDIF
    *C123847,5  TMI [End  ] 

  ENDIF  
  llUpPkTk = .T.
ENDIF
*- If we are in the edit mode.
IF laScrMode[3]
  DO CASE
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "O" .AND. ASCAN(laEvntTrig , PADR('CHG_OPN',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_OPN',10))
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "H" .AND. ASCAN(laEvntTrig , PADR('CHG_HLD',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_HLD',10))
    OTHERWISE
      DECLARE laOrdInfo[1]
      =gfSubStr(ALLTRIM(lcScFields) , @laOrdInfo , ',')
      llModOrd = .F.
      SELECT OrdHdr
      FOR lnOrdCnt = 1 TO ALEN(laOrdInfo)
        IF EVALUATE(laOrdInfo[lnOrdCnt]) <> laData[lnOrdCnt]
          llModOrd = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llModOrd .AND. ASCAN(laEvntTrig , PADR('MOD_ORD',10)) <> 0
        llReturn = gfDoTriger('SOORD',PADR('MOD_ORD',10))
      ENDIF
  ENDCASE
ENDIF

*- get the oreder number in case of manual or sequence order.
laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y',lfGtOrder(),gfSequence('ORDER','','',laData[15])))
lcCurrOrd = laData[1]

IF llBomVarnt .AND. laScrMode[4] 
  SELECT (lcOrdLine)
  lnRecNo = RECNO()
  GO TOP

  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    SELECT (lcT_BomVar)
    REPLACE ALL cCost_Id WITH laData[1]
  ELSE
    SCAN
      SELECT (lcT_BomVar)
      REPLACE ALL cCost_Id WITH laData[1] ;
      FOR cIdType+cCost_ID+STR(LineNo,6)="SO"+ &lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
    ENDSCAN  
  ENDIF  

  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ENDIF
  *-delete any record without order no and pack The temp file 
  SELECT (lcT_BomVar)
  DELETE FOR EMPTY(cCost_Id)
  PACK
ENDIF

*-- If the function was not called from EDI
IF !llFromEDI

  =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  SELECT UnCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)

  REPLACE cCurrObj WITH 'pbSav'
ENDIF    && End of IF !llFromEDI

SELECT (lcOrdHdr)
lcEdiOrd = Order
=RLOCK()
REPLACE Order WITH laData[1]
UNLOCK
SHOW GET laData[1] DISABLE
*-- update the allocated quantity in case we come in edit mode.
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. USED(lcAlocated)
  SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
  SELECT (lcAlocated)
  GO TOP
  IF !EOF()
    WAIT 'Updating alocated quantity...' WINDOW NOWAIT
    IF 'PO' $ gcCmpModules  && if we have style purchase order.
      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
      =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
    ENDIF
    IF 'MF' $ gcCmpModules && if we have munf. module.
      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
      =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
    ENDIF
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
    
    PRIVATE lcOrdLnOrd , lnOrdLnRec
    SELECT (lcOrdLine)
    lnOrdLnRec = RECNO()
    lcOrdLnOrd = TAG()
    SET ORDER TO TAG OrdLine    
    
    SELECT (lcAlocated)
    DO WHILE !EOF()
      lcKey = TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE
      SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcKey
        lcHdrAlo = IIF(TranCd='1','CutTktH','PosHdr')
        lcDetAlo = IIF(TranCd='1','CutTktL','PosLn')
        =SEEK(lcKey,'CUTPICK')
        SCATTER MEMVAR
        SELECT (lcHdrAlo)
        =SEEK(IIF(m.TranCd='1',m.CTKTNO,'P'+m.CTKTNO))
        =RLOCK()
        IF &lcAlocated..nSteps < 1
          REPLACE TotOrd WITH TotOrd - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 2

          PRIVATE lnQtyUpd , lnLoop , lcLoopStr , lnUpdSNo
          lnQtyUpd = CutPick.TotQty  
          lnUpdSNo = LEN(ALLTRIM(&lcAlocated..cUpdSizes))
          FOR lnLoop = 1 TO lnUpdSNo 
            lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
            lcLoopStr = ALLTRIM(lcLoopStr)
            lnQtyUpd = lnQtyUpd - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
          ENDFOR
          
          IF m.TranCd = '1'
            REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty + lnQtyUpd ,;
                    Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty + lnQtyUpd
          ELSE
            REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty + lnQtyUpd ,;
                    Open      WITH Open  - CUTPICK.TotQty + lnQtyUpd
          ENDIF

          IF SEEK(&lcAlocated..Style,'Style')
            PRIVATE lnLoop , lcLoopStr
            FOR lnLoop = 1 TO lnUpdSNo
              lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
              lcLoopStr = ALLTRIM(lcLoopStr)
              SELECT Style
              =RLOCK()
              REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                      NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
              UNLOCK
            ENDFOR
          ENDIF  

          IF SEEK(lcOrdType+&lcAlocated..Order+STR(ALLTRIM(&lcAlocated..cOrdLine),6),lcOrdLine) AND ;
             SEEK(&lcAlocated..Style + &lcOrdLine..cWareCode  ,'StyDye')

            PRIVATE lnLoop , lcLoopStr
            FOR lnLoop = 1 TO lnUpdSNo
              lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
              lcLoopStr = ALLTRIM(lcLoopStr)
              SELECT StyDye
              =RLOCK()
              REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                      NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
              UNLOCK
            ENDFOR
          ENDIF  

          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        UNLOCK
        SELECT (lcDetAlo)
        =SEEK(IIF(m.TranCd='2','P','')+m.CTKTNO+m.Style+m.cTktLineNo+'1')
        =RLOCK()
        IF &lcAlocated..nSteps < 3
          REPLACE ORD1   WITH ORD1   - CUTPICK.Qty1   + m.Qty1 ,;
                  ORD2   WITH ORD2   - CUTPICK.Qty2   + m.Qty2 ,;
                  ORD3   WITH ORD3   - CUTPICK.Qty3   + m.Qty3 ,;
                  ORD4   WITH ORD4   - CUTPICK.Qty4   + m.Qty4 ,;
                  ORD5   WITH ORD5   - CUTPICK.Qty5   + m.Qty5 ,;
                  ORD6   WITH ORD6   - CUTPICK.Qty6   + m.Qty6 ,;
                  ORD7   WITH ORD7   - CUTPICK.Qty7   + m.Qty7 ,;
                  ORD8   WITH ORD8   - CUTPICK.Qty8   + m.Qty8 ,;
                  TOTORD WITH TOTORD - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 4
          
          REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
                  Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
                  Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
                  Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
                  Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
                  Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
                  Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
                  Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
                  TotQty WITH TotQty - CUTPICK.TotQty + lnQtyUpd                  

          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 4
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        UNLOCK
        SELECT CUTPICK
        =RLOCK()
        IF m.TOTQTY = 0
          DELETE
        ELSE
          =RLOCK()
          GATHER MEMVAR
          UNLOCK
        ENDIF
        UNLOCK
      ENDSCAN
    ENDDO
    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')

    SELECT (lcOrdLine)
    SET ORDER TO TAG &lcOrdLnOrd
    IF BETWEEN(RECNO(),1,RECCOUNT())
      GOTO lnOrdLnRec
    ENDIF
  ENDIF
ENDIF
=gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3]
  SELECT (lcOrdLine)
  SCAN
    IF Picked .AND. SEEK(PikTkt,'PikTkt')
      SELECT PikTkt
      SET ORDER TO TAG Ordpik
      SCAN FOR Order+Piktkt = EVAL(lcOrdHdr+'.Order')
        =RLOCK() 
        REPLACE CustPo WITH laData[4]
        UNLOCK
      ENDSCAN  
      SET ORDER TO TAG Piktkt
    ENDIF
  ENDSCAN
ENDIF

SELECT ORDLINE
SET RELATION TO
SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
LOCATE
WAIT 'Updating order lines...' WINDOW NOWAIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

*200236,5 MHM Define array to store old data of pik [Start]
DIMENSION laOldAlo[9]
STORE 0 TO laOldAlo
*200236,5 MHM [Start]

STORE 0 TO lnCancel,lnCancelAmt,lnCanBulk
lnLineCount=IIF(SEEK(lcOrdType+laData[1],'ORDHDR'),ORDHDR.LastLine,0)

*200236,5 remove falg and make it per file to check updating [start]
*SCAN FOR IIF(OrdHdr.Status='B',.T.,Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. TotQty > 0))
SCAN FOR IIF(OrdHdr.Status='B',.T., !DELETED() .AND. TotQty > 0)
*200236,5 [end]
  *-- Decrease Depleted bulk orders from warehouse Ordered Quantity 
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. !DELETED() .AND. ;
  *   (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 1 .AND. ;
  *  SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine') .AND.;
  *  SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')

  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 1 .AND. ;
    SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine') .AND.;
    SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye') AND (Flag$'MN')

    SELECT StyDye
    =RLOCK('StyDye')
    REPLACE Ord1 WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2 WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3 WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4 WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5 WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6 WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7 WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8 WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
            
    UNLOCK IN 'StyDye'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF

  *-- Update Warehouse Ordered Quantity
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
  *   SEEK(Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
     SEEK(Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye') AND (Flag$'MN')
  *200236,5 [End]
  
    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

    *--If style was changed in the same line, don't update by subtract old
    *--and add new because it is a different style, you should subtract
    *--from old style and add to new style.
    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    IF llStyChgd
      =lfOrdQUpd('STYDYE')
    ELSE
      
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT StyDye                           
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
              Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
              Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
              Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
              Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
              Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
              Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
              Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
              TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'StyDye'
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF

  *-- Decrease Depleted bulk orders from Style Ordered Quantity 
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !DELETED() .AND. ;
  *   (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 3 .AND.;
  *   SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'ORDLINE') .AND. ;
  *   SEEK(OrdLine.Style,'Style')
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 3 .AND.;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'ORDLINE') .AND. ;
     SEEK(OrdLine.Style,'Style') AND (Flag$'MN')
  *200236,5 [End]

    SELECT Style 
    =RLOCK('Style')
    REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2   WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3   WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4   WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5   WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6   WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7   WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8   WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'Style'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  *-- Update Style Ordered Quantity 
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' AND laData[5]<>'B' AND nSteps < 4 AND SEEK(Style,'Style')
  *  =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
  IF lcOrdType='O' AND laData[5]<>'B' AND nSteps < 4 AND SEEK(Style,'Style')
    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')AND (Flag$'MN')
  *200236,5 ,[end]

    *--If style was changed in the same line, don't update by subtract old
    *--and add new because it is a different style, you should subtract
    *--from old style and add to new style.
    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    IF llStyChgd
      =lfOrdQUpd('STYLE')
    ELSE

      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT Style
      =RLOCK('Style')
      REPLACE  Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
               Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
               Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
               Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
               Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
               Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
               Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
               Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
               TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'Style'
    ENDIF  
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF

  *-- Cancel Depleted bulk orders Quantity 
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. ;
  *   (laScrMode[4] .OR. OrdHdr.Status='B') .AND. !DELETED() .AND. ;
  *   SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')
  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. !DELETED() .AND. ;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine') .AND. (Flag$'MN')
  *200236,5 [End]
  
    lnCanBulk = MIN(OrdLine.Qty1,Qty1)+MIN(OrdLine.Qty2,Qty2)+;
                MIN(OrdLine.Qty3,Qty3)+MIN(OrdLine.Qty4,Qty4)+;
                MIN(OrdLine.Qty5,Qty5)+MIN(OrdLine.Qty6,Qty6)+;
                MIN(OrdLine.Qty7,Qty7)+MIN(OrdLine.Qty8,Qty8)
    lnCancel    = lnCancel    + lnCanBulk
    lnCancelAmt = lnCancelAmt + lnCanBulk*OrdLine.Price
    lnOpmAmnt = lnCanBulk*OrdLine.Price &lcExRSin laData[34] &lcUntSin laData[50]

    *-- Decrease Depleted bulk orders Quantity & Amount in the Style History file
    IF SEEK(Style+lcBulkYear,'icStyHst') .AND. &lcOrdLine..nSteps < 5
      lnOrdAmt = lnOpmAmnt &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
              nOrdQty           WITH nOrdQty           - lnCanBulk ,;
              nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt  ,;
              nOrdAmt           WITH nOrdAmt           - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    
    *-- Update Bulk order line open and book quantity
    IF nSteps < 6
      =RLOCK('ORDLINE')
      SELECT ORDLINE
      =RLOCK()
      REPLACE Qty1   WITH MAX(Qty1-&lcOrdLine..Qty1,0),;
              Qty2   WITH MAX(Qty2-&lcOrdLine..Qty2,0),;
              Qty3   WITH MAX(Qty3-&lcOrdLine..Qty3,0),;
              Qty4   WITH MAX(Qty4-&lcOrdLine..Qty4,0),;
              Qty5   WITH MAX(Qty5-&lcOrdLine..Qty5,0),;
              Qty6   WITH MAX(Qty6-&lcOrdLine..Qty6,0),;
              Qty7   WITH MAX(Qty7-&lcOrdLine..Qty7,0),;
              Qty8   WITH MAX(Qty8-&lcOrdLine..Qty8,0),;
              Totqty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

      REPLACE Book1   WITH MAX(Book1-&lcOrdLine..Book1,0),;
              Book2   WITH MAX(Book2-&lcOrdLine..Book2,0),;
              Book3   WITH MAX(Book3-&lcOrdLine..Book3,0),;
              Book4   WITH MAX(Book4-&lcOrdLine..Book4,0),;
              Book5   WITH MAX(Book5-&lcOrdLine..Book5,0),;
              Book6   WITH MAX(Book6-&lcOrdLine..Book6,0),;
              Book7   WITH MAX(Book7-&lcOrdLine..Book7,0),;
              Book8   WITH MAX(Book8-&lcOrdLine..Book8,0),;
              TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8

      UNLOCK IN 'ORDLINE'
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
  
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. ;
  *   Picked .AND. SEEK(PikTkt,'PikTkt')
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. ;
     Picked .AND. SEEK(PikTkt,'PikTkt').AND. (Flag$'MN')
  *200236,5 [End]
  
    SELECT PikTkt
    =RLOCK() 
    REPLACE Store  WITH &lcOrdLine..Store ,;
            CustPo WITH &lcOrdLine..CustPo

    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

  *-- Update orders Quantity & Amount in the Style History file
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(Style+lcGlYear,'icStyHst') .AND. ;
  *   &lcOrdLine..nSteps < 7
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(Style+lcGlYear,'icStyHst') .AND. ;
     &lcOrdLine..nSteps < 7 .AND. (Flag$'MN')
  *200236,5 [End]
  
    lnOrdAmt = ;
    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]) - ;
    IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
    SELECT icStyHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ;
            IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdQty WITH nOrdQty- IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt WITH nOrdAmt + lnOrdAmt
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF

  *-- Check if any C/Ts have been assigned to the bulk order.
  *-- If any, Release allocation from the bulk order and allocate them
  *-- to the newly received distribution order.
  SELECT (lcOrdLine)
  
  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' AND laScrMode[4] AND 'MF' $ gcCmpModules AND ;
  *   !EMPTY(&lcOrdHdr..cFromOrder) .AND. &lcOrdLine..nSteps < 9
  IF lcOrdType='O' AND laScrMode[4] AND 'MF' $ gcCmpModules AND ;
     !EMPTY(&lcOrdHdr..cFromOrder) .AND. &lcOrdLine..nSteps < 9 .AND. (Flag$'MN')
  *200236,5 [End]
  
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0

      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD

      IF !SEEK('1'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'1',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF   

      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 9
    UNLOCK
  ENDIF
  *-- Check if any POs have been assigned to the bulk order.
  *-- If any, Release allocation from the bulk order and allocate them
  *-- to the newly received distribution order.
  SELECT (lcOrdLine)

  *200236,5 Add falg and make it per file to check updating [start]
  *IF lcOrdType='O' AND laScrMode[4] AND 'PO' $ gcCmpModules .AND. ;
  *   !EMPTY(&lcOrdHdr..cFromOrder) AND &lcOrdLine..nSteps < 10
  IF lcOrdType='O' AND laScrMode[4] AND 'PO' $ gcCmpModules .AND. ;
     !EMPTY(&lcOrdHdr..cFromOrder) AND &lcOrdLine..nSteps < 10 .AND. (Flag$'MN')
  *200236,5 [End]
    
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0

      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD


      IF !SEEK('2'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'2',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 10
    UNLOCK
  ENDIF
   
  SCATTER MEMVAR MEMO

  m.Start    = laData[9]
  m.Complete = IIF(!llCDPerL .OR. EMPTY(m.Complete),laData[10],m.Complete )
  m.Flag     = SPACE(1)

  *200236,5 MHM save old piked data [Start]
  laOldAlo[1] = ORDLINE.PIK1
  laOldAlo[2] = ORDLINE.PIK2
  laOldAlo[3] = ORDLINE.PIK3
  laOldAlo[4] = ORDLINE.PIK4
  laOldAlo[5] = ORDLINE.PIK5
  laOldAlo[6] = ORDLINE.PIK6
  laOldAlo[7] = ORDLINE.PIK7
  laOldAlo[8] = ORDLINE.PIK8
  laOldAlo[9] = ORDLINE.TOTPIK
  *200236,5 MHM save old piked data [End]
  *--If multi add new one
  SELECT ORDLINE
  lcOldOrd = ORDER()
  SET ORDER TO Ordlinst
  llnotfound = .F.
  IF !SEEK(&lcOrdLine..cordtype +&lcOrdLine..order + &lcOrdLine..store+m.style)
    llnotfound =.T.
  ENDIF
  SET ORDER TO &lcOldOrd
  SELECT (lcOrdLine)

  m.Order    = laData[1]
  DO CASE
    CASE !llFromEDI AND !DELETED() .AND. !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'OrdLine')
      lnLineCount=lnLineCount+1
      m.LineNo = lnLineCount
      *--not found and have piktkt
      IF llnotfound .AND. !EMPTY(lcPiktkt)
        m.LineNo = ordhdr.lastline + 1
      ENDIF
      INSERT INTO ORDLINE FROM MEMVAR
      IF SEEK(lcOrdType+laData[1],'ORDHDR')
        SELECT ORDHDR
        REPLACE LastLine WITH lnLineCount
      ENDIF
    CASE (llFromEDI .AND. SEEK('T'+Order+STR(LineNo,6),'OrdLine') ) .OR. ;
         (!llFromEDI AND !DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine'))
      m.cOrdType = IIF(llFromEDI,'O',m.cOrdType)
      lnLineCount = IIF(llFromEDI,m.LineNo,lnLineCount)
      SELECT ORDLINE
      
      IF llMFDsPrc .OR. llPODsPrc
        lnOldPrc = Price 
        lnOldQty = TotQty
        GATHER MEMVAR MEMO
        lnOldQty = IIF(llUpdAlo,TotQty,lnOldQty)
        =lfUpdtPoCT(Order,Style,STR(LineNo,6),TotQty,Price,lnOldPrc,lnOldQty,.F.)
      ELSE
        GATHER MEMVAR MEMO
      ENDIF
      
    CASE DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
      SELECT ORDLINE

      lcOrder  = Order
      lclineNo = STR(LineNo,6)
      lcStyle  = Style
      lnTotQty = 0
      lnPrice  = 0
      lnOldPrc = Price
      lnOldQty = TotQty
      DELETE
      IF llMFDsPrc .OR. llPODsPrc
        =lfUpdtPoCT(lcOrder,lcStyle,lclineNo,lnTotQty,lnPrice,lnOldPrc,lnOldQty,.T.)      
      ENDIF
  ENDCASE
  
  SELECT (lcOrdLine)

  =RLOCK()
  *200236,5 MHM Retrive old piked data [Start]
  *REPLACE Order  WITH laDAta[1] ,;
  *          LineNo WITH m.LineNo
  *=SEEK(ORDLINE.cOrdType+ORDLINE.Order+STR(ORDLINE.LineNo,6))
  *C123847,5  TMI [Start] do not update lcOrdline if called from "lfNPkOrdLn"
  IF ASCAN(laEvntTrig,'CHKAVL')<>0 
    *- Just ignor the else part
    REPLACE Order  WITH laDAta[1] ;
            LineNo WITH m.LineNo  ;
            PICKED WITH TOTPIK>0 
            
  ELSE
    *C123847,5  TMI [End  ]    
  
    REPLACE Order  WITH laDAta[1] ,;
            LineNo WITH m.LineNo,;
            Pik1   WITH laOldAlo[1],; 
            Pik2   WITH laOldAlo[2],; 
            Pik3   WITH laOldAlo[3],; 
            Pik4   WITH laOldAlo[4],; 
            Pik5   WITH laOldAlo[5],; 
            Pik6   WITH laOldAlo[6],; 
            Pik7   WITH laOldAlo[7],; 
            Pik8   WITH laOldAlo[8],; 
            TOTPIK WITH laOldAlo[9] 

    *C123847,5  TMI [End  ] Close the above if trigger
  ENDIF
  *C123847,5  TMI [End  ] 
  
  *200236,5 MHM  [End]
  UNLOCK
  
ENDSCAN

SELECT ORDLINE
IF SEEK(lcOrdType+laData[1])
  IF laData[7]
    REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) ;
                 WHILE cOrdType+Order=lcOrdType+laData[1]
  ELSE                
    REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) , ;
                 CustPo WITH laData[4] WHILE cOrdType+Order=lcOrdType+laData[1]
  ENDIF               
ENDIF

IF llBomVarnt
  =gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
ENDIF
 
=gfCloseFile('icStyHst')
=gfCloseFile('ORDCANLN')

IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfCloseFile('CUTPICK')
ENDIF

SET DELETE ON
WAIT 'Updating order header...' WINDOW NOWAIT

=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')

SELECT ORDHDR

IF !llFromEDI .AND. !SEEK(lcOrdType+laData[1])
  *ash1
  *INSERT INTO ORDHDR (cOrdType,Order) VALUES (lcOrdType,laData[1])
  APPEND BLANK 
  REPLACE cOrdType WITH lcOrdType ,;
          Order    WITH laData[1]
ENDIF
IF llFromEDI .AND. SEEK('T'+lcEdiOrd)
  REPLACE cOrdType WITH 'O' ,;
          Order    WITH laData[1]
ENDIF

*B606959,4 ASH 05/06/2003 (Begin) Assign the current order to a variable.
lctstord=ladata[1]
*B606959,4 ASH 05/06/2003 (End)

*-- Update orders Quantity & Amount in the Customer History file
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(laData[2]+lcGlYear,'arCusHst') .AND. ;
   &lcOrdHdr..nSteps < 1
  lnOrdAmt = laData[42] &lcExRSin laData[34] &lcUntSin laData[50] - ;
  IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
  SELECT arCusHst
  =RLOCK()
  REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdQty WITH nOrdQty -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
          nOrdAmt WITH nOrdAmt + lnOrdAmt
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF


IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. ;
   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
  *-- Add bulk orders approve Amount in the header of new order from bulk order .
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE APPRAMT  WITH MIN(&lcOrdHdr..OPENAMT,ORDHDR.APPRAMT)
  UNLOCK

  *-- Decrease bulk orders approve Amount in the header of bulk order.
  SELECT ORDHDR
  =RLOCK()
  REPLACE APPRAMT  WITH MAX(0,APPRAMT - &lcOrdHdr..OPENAMT)
  UNLOCK
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B') .AND. ;
   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')

  *-- Decrease bulk orders depleted Quantity & Amount in the Customer History file
  IF &lcOrdHdr..nSteps < 2 .AND. SEEK(laData[2]+lcBulkYear,'arCusHst')
    lnOrdAmt = lnCancelAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
            nOrdQty           WITH nOrdQty           - lnCancel ,;
            nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
            nOrdAmt           WITH nOrdAmt           - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK('ORDHDR')

    IF BULK = 'Y'
      REPLACE Book      WITH Book      - lnCancel   ,;
              BookAmt   WITH BookAmt   - lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
   
    ELSE
      REPLACE Cancel    WITH Cancel    + lnCancel   ,;
              Cancelamt WITH Cancelamt + lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
    ENDIF
    UNLOCK IN 'ORDHDR'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF  
  IF OrdHdr.Status='X' .AND. &lcOrdHdr..nSteps < 4 .AND. ;
    SEEK('M'+OrdHdr.Account,'Customer')
    SELECT Customer
    =RLOCK('Customer')
    REPLACE nBulk WITH nBulk - 1
    UNLOCK IN 'Customer'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
ENDIF

=gfCloseFile('arCusHst')
IF lcOrdType='O' AND laData[5]<>'B' AND (laScrMode[4] OR OrdHdr.Status='B') AND ;
   &lcOrdHdr..Bulk='Y' AND &lcOrdHdr..nSteps < 5 AND SEEK('M'+laData[2],'Customer')
  SELECT Customer
  =RLOCK()
  REPLACE nBulk WITH nBulk + 1
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF
SELECT ORDHDR
*B606959,4 ASH 05/06/2003 (Begin) Double check that the pointer is at the correct record in ORDHDR.
FLUSH
IF !SEEK(lcOrdType+laData[1]) 
  IF !SEEK(lcOrdType+lctstord)
    lcFilHandl = FCREAT('&gcWorkDir.SOAL.txt')
    =FPUTS(lcFilHandl,ORDER('ORDHDR'))
    =FPUTS(lcFilHandl,lcOrdType)
    =FPUTS(lcFilHandl,laData[1])
    =FPUTS(lcFilHandl,lcTstOrd)
    Wait Window 'Please send ' + gcWorkDir + 'SOAL.TXT to attachments@aria.com.eg'
    =FCLOSE(lcFilHandl)
    RETURN .F.
  ENDIF
ENDIF
*B606959,4 ASH 05/06/2003 (End)
IF !EOF()
  =RLOCK()
  laData[5] = IIF(laData[41] = 0  ,IIF(laData[37]>0,'C','X'),laData[5])

  GATHER FROM laData FIELDS &lcScFields

  REPLACE cOrdType   WITH lcOrdType ,;
          LastLine   WITH lnLineCount  ,;
          cFromOrder WITH &lcOrdHdr..cFromOrder ,;
          StName     WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
          cAddress1  WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
          cAddress2  WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
          cAddress3  WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
          cAddress4  WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
          cAddress5  WITH IIF(laData[51],lcShipAdd5,SPACE(30)) ,;
          TotCut     WITH &lcOrdHdr..TotCut                    ,;
          APPRAMT    WITH &lcOrdHdr..APPRAMT 

  UNLOCK
ENDIF

*-- If the function was not called from EDI
IF !llFromEDI
  
  SELECT unCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  REPLACE STATUS WITH 'C'
  UNLOCK
  llContinue = .F.
  UNLOCK 
  
ENDIF    && End of IF !llFromEDI

WAIT CLEAR 
IF laScrMode[4] AND !llFromEDI
  *-- Message : 32045
  *-- Order has been saved as xxxxxx
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1])

ENDIF
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)

IF !llFromEDI AND lcOrdType='O' AND 'EB' $ gcCmpModules
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    SELECT EDITRANS
    IF !SEEK('855'+PADR(laData[1],20)+'A'+laData[2])
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1],'A',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  =gfCloseFile('EDIACPRT')
  =gfCloseFile('EDIPD')
  =gfCloseFile('EDITRANS')
ENDIF

PRIVATE llChanged
llChanged = .F.

IF &lcOrdHdr..lFromWeb AND 'CR' $ gcCmpModules AND &lcOrdHdr..cOrdType = "T" AND &lcOrdHdr..Status $ 'HO' AND lcOldStats = 'B'
  IF SEEK("T"+&lcOrdHdr..Order,'OrdLine')
    SELECT OrdLine

    DO WHILE SEEK("T"+&lcOrdHdr..Order,'OrdLine')
      =RLOCK()
      REPLACE cOrdType WITH "O"
      UNLOCK      
    ENDDO
    
    IF SEEK('T'+&lcOrdHdr..Order,'OrdHdr')
      SELECT OrdHdr
      =RLOCK()
      REPLACE cOrdType WITH "O"
      
      llChanged = .T.
      
      UNLOCK
    ENDIF
  ENDIF    

  *-- Get the Memory Variables without checking for the CRM module.
  =gfOpenFile(gcDataDir+'CRMesag',gcDataDir+'TransType','SH')
  APPEND BLANK
  =gfAdd_Info('CRMesag')
  REPLACE cTransType WITH 'O' , cTransNo WITH &lcOrdHdr..Order , lApprove WITH .T. , ;
          cMailFrom WITH gfGetMemVar('M_NOTEMAIL') , dTransDate WITH gdSysDate, cMailTo WITH ;
          IIF(SEEK('M'+&lcOrdHdr..Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'') , ;
          cMsgSubjct WITH "Your Sales Order# " + &lcOrdHdr..Order + ' has been approved.' , ; 
          cLetterId WITH gfGetMemVar('M_SOAPR') , lSent WITH .F.
  =gfCloseFile('CRMesag')  
    
ENDIF

IF llChanged
  llWebOrdUp = .T.
ENDIF  
*--in case of always or inquire and choose yes
IF lcDivType = 'A' OR llUpPkTk 
  SELECT(lcOrdLine)
  REPLACE ALL picked WITH .T.
  LOCATE
  =lfSavPkTk(lcPiktkt,lcOrdLine)
ENDIF   

SELECT ORDHDR

*B606725,1 ABD - Fix bug that the ordhdr not updating with the add information.  [Begin]
IF laScrMode[4]
  =gfAdd_Info('ORDHDR')
ENDIF  
*B606725,1 ABD - [End]

*C123847,1  TMI [Start] Add a trigger to update the order charge file , 
*                       also update the styhist file with the used employee units
*-IF laScrMode[4] .AND. ASCAN(laEvntTrig , PADR('CHRGORD',10)) <> 0
IF ASCAN(laEvntTrig , PADR('CHRGORD',10)) <> 0
  =gfDoTriger('SOORD',PADR('CHRGORD',10))
ENDIF
*C123847,1  TMI [End  ] 

RETURN .T.

*!*************************************************************
*! Name      : lfOrdQUpd                         
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/01/2001
*! Purpose   : Update style orderd quantity in case of order
*!             line style was changed.
*!*************************************************************
*! Calls     : lfSavScr
*!*************************************************************
*! Parameters: lcFlToUpd-> Alias to update (STYLE or STYDYE).
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!C200236,1
FUNCTION lfOrdQUpd
PARA lcFlToUpd

*--Update New Style.
SELECT (lcFlToUpd)
=RLOCK()
REPLACE Ord1   WITH Ord1   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty1),;
        Ord2   WITH Ord2   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty2),;
        Ord3   WITH Ord3   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty3),;
        Ord4   WITH Ord4   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty4),;
        Ord5   WITH Ord5   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty5),;
        Ord6   WITH Ord6   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty6),;
        Ord7   WITH Ord7   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty7),;
        Ord8   WITH Ord8   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty8),;
        TotOrd WITH TotOrd + IIF(OrdHdr.Status='B',0,&lcOrdLine..TotQty)
UNLOCK

*--Update Old Style.
=SEEK(OrdLine.Style+IIF(lcFlToUpd='STYLE','',OrdLine.cWareCode+SPACE(10)),lcFlToUpd)

=RLOCK()
REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1),;
        Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2),;
        Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3),;
        Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4),;
        Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5),;
        Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6),;
        Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7),;
        Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8),;
        TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)
UNLOCK
RETURN

*!*************************************************************
*! Name      : lfUpdtPoCT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/01/2001
*! Purpose   : update selling price in po/ct lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcOrder	---> sales order no
*!             LcStyle  ---> Style COde
*!             lcLineNo ---> sales order line no
*!             lnTotQty ---> new qty
*!             lnPrice  ---> new price
*!             lnOldPrc ---> old price 
*!             lnOldQty ---> old qty
*!			   llRecDel ---> record is deleted (remove line)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdtPoCT()
*!*************************************************************
*!C200236,1
FUNCTION lfUpdtPoCT
PARAMETER  lcOrder,LcStyle,lcLineNo,lnQty,lnPrice,lnOldPrc,lnOldQty,llRecDel

=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLS','SH')
SELECT CUTPICK
IF SEEK('2'+lcOrder+lcLineNo)
  *-- there are a po generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '2'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percent
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)     && new generated qty 
    IF SEEK('P'+lnCtktNo+lcStyle,'POSLN')	
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty));
                   /(POSLN.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty)+;
                    (lnPrice*lnTotQty)) / (POSLN.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnTotCost= nEcost1+nEcost2+nEcost3+nEcost4+nEcost5
      lnRotSub    = IIF(llStyMark,lnTotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - lnTotCost)/lnRotSub)*100)
      REPLACE POSLN.nSelPrice  WITH lnNewPrc ,;
              POSLN.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN  
ENDIF
SELECT CUTPICK
IF SEEK('1'+lcOrder+lcLineNo)
  *-- there are a CT generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '1'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percentg
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)   && new generated qty 
    IF SEEK(lcStyle+lnCtktNo+'1','CUTTKTL')
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty));
                   /(CUTTKTL.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty)+;
                   (lnPrice*lnTotQty)) / (CUTTKTL.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnRotSub   = IIF(llStyMark,Style.TotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - Style.TotCost)/lnRotSub)*100)
      REPLACE CUTTKTL.nSelPrice  WITH lnNewPrc ,;
              CUTTKTL.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : lfSavPkTk
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/21/2001
*! Purpose   : Save Order Piktkt Information
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfSavPkTk()
*!*************************************************************
*!C200236,1
FUNCTION lfSavPkTk
PARAMETER lcPikTktNo,lc_TmpOrdL
PRIVATE lcPikTktNo, lcChgPkTKt, lnCurTag ,llNotPiked,lnOldTag

*B606959,1 ASH 02/16/2003 (Begin) To make sure that this is the correct tag.
lnOldTag = 0
SELECT PikTkt
lnOldTag = INT(VAL(SYS(21)))
SET ORDER TO TAG OrdPik
*B606959,1 ASH 02/16/2003 (End)

SELECT ORDLINE
SET RELATION OFF INTO (lc_TmpOrdL)
lnCurTag = VAL(SYS(21))
SET ORDER TO TAG ORDLINE
SELECT (lc_TmpOrdL)
SET ORDER TO TAG ORDLINST
SET RELATION TO cOrdType+order+STR(lineno,6) INTO ORDLINE ADDITIVE
LOCATE

STORE ' ' TO lcChgPkTKt
STORE .F. TO llNotPiked
*--We initialize the variable as it is piked
llGenPkTk = .T.

SCAN
  SCATTER MEMVAR MEMO
  DO CASE
    CASE llGenPkTk  ;
      .AND. ORDLINE.PikTkt = REPLICATE("*",FSIZE("PIKTKT"))
      IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode 
        lcChgPkTKt = m.Order + m.Store + m.cWareCode 
        lcPikTktNo = lfGetPkTkt(m.Order, laData[15], m.Store, m.cWareCode)
      ENDIF               
      IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
        INSERT INTO PIKTKT;
             (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
             VALUES;
            (lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')            
        =gfAdd_Info('PIKTKT')
      ENDIF  

      *-- Update ORDLINE
      SELECT ORDLINE
      REPLACE PikTkt  WITH lcPikTktNo,;
      		  PikDate WITH PIKTKT.Date

    CASE Picked
      *-- If this is a new pick, generate a new picking ticket

      *C200236,4 if not available stoke [Start]
      *-- Update ORDLINE
      *C123847,5  TMI [Start] if in case of DIR03 do not run this funtion, since I have priviously updated the allocated qty in lfUpdPk funciton 
      IF ASCAN(laEvntTrig,'CHKAVL')<>0
        *- Just skip running lfStkAvail funtion
        SAVE TO (gcWorkDir+lcWinCh4+'.PIK') ALL LIKE lcOrdLine  && To create a new piktkt no immediatly
      ELSE
        *C123847,5  TMI [End  ]   

        =lfStkAvail()
        
        *C123847,5  TMI [Start] close the if statement
      ENDIF
      *C123847,5  TMI [End  ] 
      
      IF &lc_TmpOrdL..TotPik = 0
        llNotPiked = .T.
        *--if no stoke avaialbe then ampty all
        SELECT ORDLINE
        REPLACE TotPik  WITH 0  ,;
                piktkt  WITH '' ,;
                pikdate WITH {},;
                Picked WITH .F. ,;
                Pik1   WITH 0,;
                Pik2   WITH 0,;
                Pik3   WITH 0,;
                Pik4   WITH 0,;
                Pik5   WITH 0,;
                Pik6   WITH 0,;
                Pik7   WITH 0,;
                Pik8   WITH 0
        LOOP              
      ENDIF        

      *C200236,4 if not available stoke [End]
      IF ORDHDR.Multi = "Y" AND (lcChgPkTKt <> m.Order + m.Store + m.cWareCode)
        lcPikTktNo = ''
      ENDIF
      IF !ORDLINE.Picked AND EMPTY(lcPikTktNo)
        *-- Generate a different picking ticket per store/warehouse
        IF llGenPkTk 
          IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode 
            lcChgPkTKt = m.Order + m.Store + m.cWareCode 
            lcPikTktNo = IIF(EMPTY(m.Piktkt),;
                         lfGetPkTkt(m.Order, laData[15], m.Store, m.cWareCode),;
                      m.PikTkt)
          ENDIF               
          IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
            INSERT INTO PIKTKT;
            (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
            VALUES;
            (lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')                
            =gfAdd_Info('PIKTKT')
          ENDIF  
        ELSE
          lcPikTktNo = REPLICATE("*", FSIZE("PIKTKT","PIKTKT"))
    	  SET ORDER TO TAG PIKTKT IN PIKTKT
 	  	  IF !SEEK(lcPikTktNo, 'PIKTKT')
            INSERT INTO PIKTKT (Piktkt) VALUES (lcPikTktNo)
            =gfAdd_Info('PIKTKT')
          ENDIF  
          SET ORDER TO TAG ORDPIK IN PIKTKT
 	  	ENDIF
   	  ENDIF	
   	  
      *-- Update allocated and ordered quantities in STYDYE
      *-- Update Warehouse record in STYDYE file.
      *C123847,1  TMI [Start] Update STYDYE,STYLE allocation fields for DIR03
      IF ASCAN(laEvntTrig,PADR('SODIRALO',10)) <> 0
        * note : this code will be entered in Add mode only 
        PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
        IF llUpdDirALO
          SELECT &lc_TmpOrdL 
          lnQty1 = IIF( STYDYE.STK1-STYDYE.ALO1 >0 , MIN(STYDYE.STK1-STYDYE.ALO1 , PIK1) , 0 )
          lnQty2 = IIF( STYDYE.STK2-STYDYE.ALO2 >0 , MIN(STYDYE.STK2-STYDYE.ALO2 , PIK2) , 0 )
          lnQty3 = IIF( STYDYE.STK3-STYDYE.ALO3 >0 , MIN(STYDYE.STK3-STYDYE.ALO3 , PIK3) , 0 )
          lnQty4 = IIF( STYDYE.STK4-STYDYE.ALO4 >0 , MIN(STYDYE.STK4-STYDYE.ALO4 , PIK4) , 0 )
          lnQty5 = IIF( STYDYE.STK5-STYDYE.ALO5 >0 , MIN(STYDYE.STK5-STYDYE.ALO5 , PIK5) , 0 )
          lnQty6 = IIF( STYDYE.STK6-STYDYE.ALO6 >0 , MIN(STYDYE.STK6-STYDYE.ALO6 , PIK6) , 0 )
          lnQty7 = IIF( STYDYE.STK7-STYDYE.ALO7 >0 , MIN(STYDYE.STK7-STYDYE.ALO7 , PIK7) , 0 )
          lnQty8 = IIF( STYDYE.STK8-STYDYE.ALO8 >0 , MIN(STYDYE.STK8-STYDYE.ALO8 , PIK8) , 0 )

          SELECT STYDYE
          REPLACE Alo1  WITH Alo1 + lnQty1 ,;
                  Alo2  WITH Alo2 + lnQty2 ,;
                  Alo3  WITH Alo3 + lnQty3 ,;
                  Alo4  WITH Alo4 + lnQty4 ,;
                  Alo5  WITH Alo5 + lnQty5 ,;
                  Alo6  WITH Alo6 + lnQty6 ,;
                  Alo7  WITH Alo7 + lnQty7 ,;
                  Alo8  WITH Alo8 + lnQty8 ,;
       		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6 + Alo7 + Alo8
    
          SELECT STYLE      
          REPLACE Alo1  WITH Alo1 + lnQty1 ,;
                  Alo2  WITH Alo2 + lnQty2 ,;
                  Alo3  WITH Alo3 + lnQty3 ,;
                  Alo4  WITH Alo4 + lnQty4 ,;
                  Alo5  WITH Alo5 + lnQty5 ,;
                  Alo6  WITH Alo6 + lnQty6 ,;
                  Alo7  WITH Alo7 + lnQty7 ,;
                  Alo8  WITH Alo8 + lnQty8 ,;
       		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6 + Alo7 + Alo8
    
        ENDIF
      ELSE
        *C123847,1  TMI [End  ] 
      
        SELECT STYDYE
        *B606324,1 RAE Fix the values of ALO1 to ALO8 fields in STYLE and STYDYE files. [start]      
        *IF SEEK(m.Style + laData[31] + SPACE(10))
        IF SEEK(m.Style + m.cWarecode + SPACE(10))
          *REPLACE Alo1   WITH MAX(Alo1 + IIF(Stk1-Alo1> ORDLINE.Pik1,ORDLINE.Pik1,Stk1-Alo1),0),;
                  Alo2   WITH MAX(Alo2 + IIF(Stk2-Alo2> ORDLINE.Pik2,ORDLINE.Pik2,Stk2-Alo2),0),;
                  Alo3   WITH MAX(Alo3 + IIF(Stk3-Alo3> ORDLINE.Pik3,ORDLINE.Pik3,Stk3-Alo3),0),;
                  Alo4   WITH MAX(Alo4 + IIF(Stk4-Alo4> ORDLINE.Pik4,ORDLINE.Pik4,Stk4-Alo4),0),;
                  Alo5   WITH MAX(Alo5 + IIF(Stk5-Alo5> ORDLINE.Pik5,ORDLINE.Pik5,Stk5-Alo5),0),;
                  Alo6   WITH MAX(Alo6 + IIF(Stk6-Alo6> ORDLINE.Pik6,ORDLINE.Pik6,Stk6-Alo6),0),;
                  Alo7   WITH MAX(Alo7 + IIF(Stk7-Alo7> ORDLINE.Pik7,ORDLINE.Pik7,Stk7-Alo7),0),;
                  Alo8   WITH MAX(Alo8 + IIF(Stk8-Alo8> ORDLINE.Pik8,ORDLINE.Pik8,Stk8-Alo8),0),;
     		        TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                              Alo5 + Alo6 + Alo7 + Alo8
           
        REPLACE Alo1  WITH MAX(Alo1 + IIF(Stk1-Alo1> (ORDLINE.Pik1-laOldAlo[1]),(ORDLINE.QTY1-laOldAlo[1]),Stk1-Alo1),0),;
                Alo2  WITH MAX(Alo2 + IIF(Stk2-Alo2> (ORDLINE.Pik2-laOldAlo[2]),(ORDLINE.QTY2-laOldAlo[2]),Stk2-Alo2),0),;
                Alo3  WITH MAX(Alo3 + IIF(Stk3-Alo3> (ORDLINE.Pik3-laOldAlo[3]),(ORDLINE.QTY3-laOldAlo[3]),Stk3-Alo3),0),;
                Alo4  WITH MAX(Alo4 + IIF(Stk4-Alo4> (ORDLINE.Pik4-laOldAlo[4]),(ORDLINE.QTY4-laOldAlo[4]),Stk4-Alo4),0),;
                Alo5  WITH MAX(Alo5 + IIF(Stk5-Alo5> (ORDLINE.Pik5-laOldAlo[5]),(ORDLINE.QTY5-laOldAlo[5]),Stk5-Alo5),0),;
                Alo6  WITH MAX(Alo6 + IIF(Stk6-Alo6> (ORDLINE.Pik6-laOldAlo[6]),(ORDLINE.QTY6-laOldAlo[6]),Stk6-Alo6),0),;
                Alo7  WITH MAX(Alo7 + IIF(Stk7-Alo7> (ORDLINE.Pik7-laOldAlo[7]),(ORDLINE.QTY7-laOldAlo[7]),Stk7-Alo7),0),;
                Alo8  WITH MAX(Alo8 + IIF(Stk8-Alo8> (ORDLINE.Pik8-laOldAlo[8]),(ORDLINE.QTY8-laOldAlo[8]),Stk8-Alo8),0),;
     		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                            Alo5 + Alo6 + Alo7 + Alo8
  
        *B606324,1 RAE [end]
        ENDIF      
        *-- If dyelots, update dyelot record in STYDYE file
        *--laSetups[14,1] use dyelot Y/N
  
        *B606324,1 RAE Fix the values of ALO1 to ALO8 fields in STYLE and STYDYE files. [start]      
        *IF laSetups[14,1] = 'Y' .AND. STYLE.cDye_flg = 'Y' .AND.;
          SEEK(m.Style + laData[31] + ORDLINE.Dyelot, 'STYDYE')
  
        IF laSetups[14,1] = 'Y' .AND. STYLE.cDye_flg = 'Y' .AND.;
          SEEK(m.Style + m.cWarecode + ORDLINE.Dyelot, 'STYDYE')
          *REPLACE Alo1   WITH MAX(Alo1 + IIF(Stk1-Alo1> ORDLINE.Pik1,ORDLINE.Pik1,Stk1-Alo1),0),;
                  Alo2   WITH MAX(Alo2 + IIF(Stk2-Alo2> ORDLINE.Pik2,ORDLINE.Pik2,Stk2-Alo2),0),;
                  Alo3   WITH MAX(Alo3 + IIF(Stk3-Alo3> ORDLINE.Pik3,ORDLINE.Pik3,Stk3-Alo3),0),;
                  Alo4   WITH MAX(Alo4 + IIF(Stk4-Alo4> ORDLINE.Pik4,ORDLINE.Pik4,Stk4-Alo4),0),;
                  Alo5   WITH MAX(Alo5 + IIF(Stk5-Alo5> ORDLINE.Pik5,ORDLINE.Pik5,Stk5-Alo5),0),;
                  Alo6   WITH MAX(Alo6 + IIF(Stk6-Alo6> ORDLINE.Pik6,ORDLINE.Pik6,Stk6-Alo6),0),;
                  Alo7   WITH MAX(Alo7 + IIF(Stk7-Alo7> ORDLINE.Pik7,ORDLINE.Pik7,Stk7-Alo7),0),;
                  Alo8   WITH MAX(Alo8 + IIF(Stk8-Alo8> ORDLINE.Pik8,ORDLINE.Pik8,Stk8-Alo8),0),;
        		    TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6   + Alo7 + Alo8
  
        REPLACE Alo1  WITH MAX(Alo1 + IIF(Stk1-Alo1> (ORDLINE.Pik1-laOldAlo[1]),(ORDLINE.QTY1-laOldAlo[1]),Stk1-Alo1),0),;
                Alo2  WITH MAX(Alo2 + IIF(Stk2-Alo2> (ORDLINE.Pik2-laOldAlo[2]),(ORDLINE.QTY2-laOldAlo[2]),Stk2-Alo2),0),;
                Alo3  WITH MAX(Alo3 + IIF(Stk3-Alo3> (ORDLINE.Pik3-laOldAlo[3]),(ORDLINE.QTY3-laOldAlo[3]),Stk3-Alo3),0),;
                Alo4  WITH MAX(Alo4 + IIF(Stk4-Alo4> (ORDLINE.Pik4-laOldAlo[4]),(ORDLINE.QTY4-laOldAlo[4]),Stk4-Alo4),0),;
                Alo5  WITH MAX(Alo5 + IIF(Stk5-Alo5> (ORDLINE.Pik5-laOldAlo[5]),(ORDLINE.QTY5-laOldAlo[5]),Stk5-Alo5),0),;
                Alo6  WITH MAX(Alo6 + IIF(Stk6-Alo6> (ORDLINE.Pik6-laOldAlo[6]),(ORDLINE.QTY6-laOldAlo[6]),Stk6-Alo6),0),;
                Alo7  WITH MAX(Alo7 + IIF(Stk7-Alo7> (ORDLINE.Pik7-laOldAlo[7]),(ORDLINE.QTY7-laOldAlo[7]),Stk7-Alo7),0),;
                Alo8  WITH MAX(Alo8 + IIF(Stk8-Alo8> (ORDLINE.Pik8-laOldAlo[8]),(ORDLINE.QTY8-laOldAlo[8]),Stk8-Alo8),0),;
     		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                            Alo5 + Alo6 + Alo7 + Alo8
  
        *B606324,1 RAE [end]
  	  ENDIF	  
        *-- Update the style  record in STYLE file.
        SELECT STYLE      
        *B606324,1 RAE Fix the values of ALO1 to ALO8 fields in STYLE and STYDYE files. [start]
        *REPLACE Alo1   WITH MAX(Alo1 + IIF(Stk1-Alo1> ORDLINE.Pik1,ORDLINE.Pik1,Stk1-Alo1),0),;
                 Alo2   WITH MAX(Alo2 + IIF(Stk2-Alo2> ORDLINE.Pik2,ORDLINE.Pik2,Stk2-Alo2),0),;
                 Alo3   WITH MAX(Alo3 + IIF(Stk3-Alo3> ORDLINE.Pik3,ORDLINE.Pik3,Stk3-Alo3),0),;
                 Alo4   WITH MAX(Alo4 + IIF(Stk4-Alo4> ORDLINE.Pik4,ORDLINE.Pik4,Stk4-Alo4),0),;
                 Alo5   WITH MAX(Alo5 + IIF(Stk5-Alo5> ORDLINE.Pik5,ORDLINE.Pik5,Stk5-Alo5),0),;
                 Alo6   WITH MAX(Alo6 + IIF(Stk6-Alo6> ORDLINE.Pik6,ORDLINE.Pik6,Stk6-Alo6),0),;
                 Alo7   WITH MAX(Alo7 + IIF(Stk7-Alo7> ORDLINE.Pik7,ORDLINE.Pik7,Stk7-Alo7),0),;
                 Alo8   WITH MAX(Alo8 + IIF(Stk8-Alo8> ORDLINE.Pik8,ORDLINE.Pik8,Stk8-Alo8),0),;
        	       TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                             Alo5 + Alo6   + Alo7 + Alo8                      
   
        REPLACE Alo1  WITH MAX(Alo1 + IIF(Stk1-Alo1> (ORDLINE.Pik1-laOldAlo[1]),(ORDLINE.QTY1-laOldAlo[1]),Stk1-Alo1),0),;
                Alo2  WITH MAX(Alo2 + IIF(Stk2-Alo2> (ORDLINE.Pik2-laOldAlo[2]),(ORDLINE.QTY2-laOldAlo[2]),Stk2-Alo2),0),;
                Alo3  WITH MAX(Alo3 + IIF(Stk3-Alo3> (ORDLINE.Pik3-laOldAlo[3]),(ORDLINE.QTY3-laOldAlo[3]),Stk3-Alo3),0),;
                Alo4  WITH MAX(Alo4 + IIF(Stk4-Alo4> (ORDLINE.Pik4-laOldAlo[4]),(ORDLINE.QTY4-laOldAlo[4]),Stk4-Alo4),0),;
                Alo5  WITH MAX(Alo5 + IIF(Stk5-Alo5> (ORDLINE.Pik5-laOldAlo[5]),(ORDLINE.QTY5-laOldAlo[5]),Stk5-Alo5),0),;
                Alo6  WITH MAX(Alo6 + IIF(Stk6-Alo6> (ORDLINE.Pik6-laOldAlo[6]),(ORDLINE.QTY6-laOldAlo[6]),Stk6-Alo6),0),;
                Alo7  WITH MAX(Alo7 + IIF(Stk7-Alo7> (ORDLINE.Pik7-laOldAlo[7]),(ORDLINE.QTY7-laOldAlo[7]),Stk7-Alo7),0),;
                Alo8  WITH MAX(Alo8 + IIF(Stk8-Alo8> (ORDLINE.Pik8-laOldAlo[8]),(ORDLINE.QTY8-laOldAlo[8]),Stk8-Alo8),0),;
     		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                            Alo5 + Alo6 + Alo7 + Alo8
        *B606324,1 RAE [end]        
        
        *C123847,1  TMI [Start]  Close the above If Statement for DIR03
      ENDIF
      *C123847,1  TMI [End  ] 
      *-- Update ORDLINE
      SELECT ORDLINE
      lcCrPkTkt   = ORDLINE.PikTkt
      m.PikTkt = IIF(EMPTY(lcPikTktNo),lcCrPkTkt,lcPikTktNo)
      *C123847,1  TMI [Start] Locate the piktkt record 
      IF ASCAN(laEvntTrig,PADR('SODIRALO',10))<>0
        =SEEK(laData[1],'PIKTKT')
      ENDIF
      *C123847,1  TMI [End  ] 
      m.PikDate = IIF(m.PikTkt <> "******", PIKTKT.Date, gdSysDate)
      m.Start = laData[9]
      *C037816,1 MHM 04/06/2004 Svae data to Bin Location files[Start]
      IF llUseBin
        =lfALSAVAUT()
      ENDIF
      SELECT ORDLINE
      *C037816,1 MHM [End]
      GATHER MEMVAR MEMO
      *C200236,4 if not available stoke [Start]
      REPLACE PIK1   WITH &lcOrdLine..PIK1 ,;
              PIK2   WITH &lcOrdLine..PIK2 ,;
              PIK3   WITH &lcOrdLine..PIK3 ,;
              PIK4   WITH &lcOrdLine..PIK4 ,;
              PIK5   WITH &lcOrdLine..PIK5 ,;
              PIK6   WITH &lcOrdLine..PIK6 ,;
              PIK7   WITH &lcOrdLine..PIK7 ,;
              PIK8   WITH &lcOrdLine..PIK8 ,;
              TotPik WITH &lcOrdLine..TotPik
      *C200236,4 if not available stoke [End]
      
  ENDCASE
ENDSCAN
*--200236,5 we Remove Message according to tomy mail [Start]
*IF llNotPiked
*  *--32099  massage : "There is no stock available for one or more sizes 
*  *--32099  on some lines. Cannot allocate these lines." 
*  *--32099                  <Ok>
*  = gfModalGen('QRM32099B00000','DIALOG','some lines'+"|"+'allocate these lines.')
*ENDIF
*200236,5 [end]
  *-- Restore relations
SELECT (lc_TmpOrdL)
SET RELATION TO
SET ORDER TO TAG ORDLINE
SELECT ORDLINE
SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO (lc_TmpOrdL) ADDITIVE
SET ORDER TO (lnCurTag)
*B606959,1 ASH 02/16/2003 (Begin) Restore the original tag.
SELECT PikTkt
SET ORDER TO lnOldTag
*B606959,1 ASH 02/16/2003 (End)

*C123847,1  TMI [Start] open alordal screen if there is not enough qty to allocate
*C123847,1  TMI [Start] Call alordal.prg with the order# to allocate
*- note : you should prevent this screen to appear if the user selects the option not to allocate from the mbimain trigger
IF ASCAN(laEvntTrig , PADR('SODIRALO',10)) <> 0
*  IF laScrMode[4]
 
    IF !llEnough 
      IF Dir_lnAvl>0
        PUSH KEY
        ON KEY
        
        *- Open allocation screen from sales order screen only if the sales order screen is openend from the menu
        IF TYPE('lcOrderNo') <> 'C'
          *- Save screen mode variable array
          SAVE TO (gcWorkDir+lcFolder) ALL LIKE laScrMode*
          lcStamp =  ORDHDR.cAdd_User+IIF(EMPTY(ORDHDR.dAdd_Date),'',DTOC(ORDHDR.dAdd_Date))+ORDHDR.cAdd_Time
  
          lcParameter = "'" + laData[1] + "'"
          DO gpDoProg WITH "AWRALORDAL", .F., "AL", lcParameter
          POP KEY
          llShow = .F.    
             
          laScrMode    = .F.
          laScrMode[1] = .T.
          =gfStatic()                      && Save environment before terminate
  
          *- Restore screen mode variable array
          RESTORE FROM (gcWorkDir+lcFolder) ADDITIVE
        ENDIF

      ELSE
        lcMsg = IIF(laScrMode[4],"There is no Stock Available. No lines were picked." ,;
                                 "There is no more stock available to allocate unallocated open qty's") 
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
      ENDIF
    
    ELSE
    
    *  IF laScrMode[4]
        SELECT PIKTKT
        lcSvOrd = ORDER('PIKTKT')
        SET ORDER TO ORDPIK
        IF SEEK(laData[1],'PIKTKT')
          lcPkTk = ''
          SCAN REST WHILE ORDER+PIKTKT = laData[1]
            lcPkTk = lcPkTk + PIKTKT.PIKTKT + ', '
          ENDSCAN
          lcPkTk = LEFT(lcPkTk,LEN(lcPkTk)-2)
          lcPkTk = IIF(','$lcPkTk, 's ',' ')+lcPkTk
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order '+laData[1]+' has been picked complete on P/T'+lcPkTk+'.')
        ENDIF
        SELECT PIKTKT
        SET ORDER TO &lcSvOrd
      ENDIF
    *ENDIF
  
*  ENDIF
ENDIF
*C123847,1  TMI [End  ] 

SELECT ORDHDR

*!*************************************************************
*! Name      : lfBrowse
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/22/2001  
*! Purpose   : Browse PikTkt #
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowse()
*!*************************************************************
*!C200236,1
FUNCTION lfBrowse
PRIVATE lcBrFields,lcAlias,  lnStyOrd, lnAlias,laGtData,;
        lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2 ,lcPiktk
DECLARE laGtData[3] && array to get values from browse
laGtData     = ' '
llBrowse   = IIF(TYPE('llBrowse')='U',.F.,llBrowse) && variable to determine forcing browse or not
lnBrHSRow1 = gnBrFSRow1
lnBrHSCol1 = gnBrFSCol1
lnBrHSRow2 = gnBrHSRow1
lnBrHSCol2 = gnBrHSCol1
lcTitle    = "Orders/PikTkt "
STORE '' TO lcPiktk
lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
             [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
             [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' , STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',IIF(STATUS = 'C','Complete', IIF(STATUS = 'K','Packed','Pulled')))) :R :H= 'Status  ']

lnAlias    = SELECT()
*--get account and store
lcAccot = laData[2]
lcStore = laData[3]

SELECT PIKTKT
lcOldOrdFlt = FILTER()
SET RELATION TO 'O'+order INTO ORDHDR ADDITIVE

*B607116,1 ABD - Ignore piktkt that hold Complete and Canncel Status from the browse. [Begin]
*SET FILTER TO (lcAccot = ACCOUNT .AND. STORE=lcStore .AND. prtflag <> 'P' .AND. ;
*               ORDHDR.Multi <>'Y' .AND. Status<>'X' .AND. laData[15]=ALLTRIM(ORDHDR.cDivision))
*B607266,1 ABD - ignore piktkt = '******'. [Begin]
*SET FILTER TO (lcAccot = ACCOUNT .AND. STORE=lcStore .AND. prtflag <> 'P' .AND. ;
               ORDHDR.Multi <>'Y' .AND. !(Status $ 'XC') .AND. laData[15]=ALLTRIM(ORDHDR.cDivision))
SET FILTER TO (lcAccot = ACCOUNT .AND. STORE=lcStore .AND. prtflag <> 'P' .AND. ;
               ORDHDR.Multi <>'Y' .AND. !(Status $ 'XC') .AND. laData[15]=ALLTRIM(ORDHDR.cDivision) ;
               .AND. PIKTKT # '******' .AND. !EMPTY(PIKTKT))
*B607266,1 ABD - [End]
*B607116,1 ABD - [End]

LOCATE

IF EOF()
  SET FILTER TO &lcOldOrdFlt 
  SET RELATION OFF INTO ORDHDR 
  RETURN ''
ENDIF

llWasSel= ARIABROW([FOR ACCOUNT = lcAccot AND STORE =lcStore AND prtflag <> 'P'],lcTitle,;
          lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2,'','','PikTkt,ORDER',"laGtData")
IF llWasSel
  lcPiktk = laGtData[1]
  laData[1] = laGtData[2]
ELSE
  XDYELOT= SPACE(10)
ENDIF  
IF !EMPTY(lcPiktk)
  STORE .F. TO laScrMode
  laScrMode[3] = .T.
ENDIF

*--Restore old data
SET FILTER TO &lcOldOrdFlt 
SET RELATION OFF INTO ORDHDR 

IF !EMPTY(laData[1])
  SELECT OrdLine
  lcoldOrdr = ORDER()
  SET ORDER TO Ordlinst
  
  =SEEK('O'+laData[1] , 'OrdHdr')
  SELECT (lcOrdLine)
  lcOldOrd = ORDER()
  SET ORDER TO 
  *C123847,1  TMI [Start] 
  IF EOF()
    GO BOTTOM
  ENDIF
  *C123847,1  TMI [End  ] 
  lnRecNo = RECNO()
  LOCATE
  =SEEK(cordtype+laData[1]+store,'ORDLINE')
  laData[35] = laData[35] + OrdHdr.Book
  laData[36] = laData[36] + OrdHdr.BookAmt
  laData[41] = laData[41] + OrdHdr.Open
  laData[42] = laData[42] + OrdHdr.OpenAmt
  laData[5]  =  OrdHdr.STATUS
  lnLineNo   = OrdHdr.LastLine 
  m.piktkt   = ordline.piktkt 
  m.pikdate  = ordline.pikdate
  m.picked   = ordline.picked

  SCAN
    REPLACE Order   WITH laData[1] ,;
            LineNo  WITH lnLineNo+1 ,;
            piktkt  WITH ordline.piktkt ,;
            pikdate WITH ordline.pikdate,;
            picked  WITH ordline.picked
    lnLineNo = lnLineNo+1        
    IF SEEK(cordtype+order+store+style,'ORDLINE')
      =SEEK(cordtype+order,'ORDHDR')
      SCATTER MEMVAR MEMO
      m.qty1 = m.qty1+ordline.qty1
      m.qty2 = m.qty2+ordline.qty2
      m.qty3 = m.qty3 +ordline.qty3
      m.qty4 = m.qty4 +ordline.qty4
      m.qty5 = m.qty5 +ordline.qty5
      m.qty6 = m.qty6 +ordline.qty6
      m.qty7 = m.qty7 +ordline.qty7
      m.qty8 = m.qty8 +ordline.qty8
      m.Totqty   = m.Totqty +ordline.Totqty
      m.Book1    = m.Book1+ordline.Book1
      m.Book2    = m.Book2+ordline.Book2
      m.Book3    = m.Book3+ordline.Book3
      m.Book4    = m.Book4+ordline.Book4
      m.Book5    = m.Book5+ordline.Book5
      m.Book6    = m.Book6+ordline.Book6
      m.Book7    = m.Book7+ordline.Book7
      m.Book8    = m.Book8+ordline.Book8
      m.TotBook  = m.TotBook +ordline.TotBook
      m.LineNo   = ordline.LineNo
      GATHER MEMVAR MEMO
    ENDIF
    REPLACE Picked WITH .T. ,;
            PIK1   WITH QTY1    ,;
            PIK2   WITH QTY2    ,;
            PIK3   WITH QTY3    ,;
            PIK4   WITH QTY4    ,;
            PIK5   WITH QTY5    ,;
            PIK6   WITH QTY6    ,;
            PIK7   WITH QTY7    ,;
            PIK8   WITH QTY8    ,;
            TotPik WITH TotQTY  
  ENDSCAN
  SET ORDER TO &lcOldOrd
  GOTO lnRecNo
  SELECT OrdLine
  SET ORDER TO &lcoldOrdr
ELSE
  SELECT (lcOrdLine)
  
  lcOldOrd = ORDER()
  SET ORDER TO 
  lnRecNo = RECNO()
  LOCATE
  SCAN
    REPLACE PIK1   WITH QTY1    ,;
            PIK2   WITH QTY2    ,;
            PIK3   WITH QTY3    ,;
            PIK4   WITH QTY4    ,;
            PIK5   WITH QTY5    ,;
            PIK6   WITH QTY6    ,;
            PIK7   WITH QTY7    ,;
            PIK8   WITH QTY8    ,;
            TotPik WITH TotQTY  
  ENDSCAN
  SET ORDER TO &lcOldOrd
  GOTO lnRecNo
ENDIF

SELECT (lnAlias)
RETURN lcPiktk

*!*************************************************************
*! Name      : lfGetPkTkt                      
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/01/2001
*! Purpose   : Get a picking ticket for the sended order no.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfGetPkTkt(lcOrdNo , OrdHdr.Division)
*!*************************************************************
*! Modifications     :
*!*************************************************************
*!C200236,1
FUNCTION lfGetPkTkt

PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode , lnAskType

*C123847,1  TMI [Start] if 'Add to a new piktkt' option is selected
IF ASCAN(laEvntTrig,PADR('CHKAVL',10))<>0 .AND. FILE(gcWorkDir+lcWinCh4+'.PIK')
  ERASE (gcWorkDir+lcWinCh4+'.PIK')
  lnAskType = 2
ENDIF
*C123847,1  TMI [End  ] 
  
IF TYPE('lnAskType') $ 'UL' 
  lnAskType = 0
ENDIF

PRIVATE lcOrdNo   , lcDivision , lcCurAlias , lcPikTkt , ;
        llOpnPkTk , lnOldTag   , lnRecPkTk  , laPikNo  , lcExact , llAskUser

llAskUser = (lnAskType = 0)
*-- Save the exact setting.
lcExact = SET("EXACT")

*-- Define array hold the piktkts no. , status , printed or not.
DECLARE laPikNo[1]

*-- Save the current alias.
lnCurAlias = SELECT()

*-- Initialize the needed variables.
STORE ""  TO laPikNo , lcPikTkt
STORE .F. TO llOpnPkTk
STORE 0   TO lnOldTag   , lnRecPkTk

SELECT PikTkt
lnOldTag = INT(VAL(SYS(21)))
SET ORDER TO TAG OrdPik
*-- Save the current record no. if the file was open.
lnRecPkTk = RECNO("PikTkt")

SELECT PikTkt
IF SEEK(lcOrdNo , "PikTkt") 
  LOCATE REST WHILE Order = lcOrdNo;
         FOR !(PikTkt.Status $ "CX") .AND. ;
         Store = lcStore .AND. cWareCode = lcWareCode        
  IF FOUND()       

    *-- If there was any piktkts created for any of the 
    *-- current order lines.
    laPikNo[ALEN(laPikNo,1)] = Piktkt.piktkt + " " + ;
              IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
              IIF(Piktkt.status $ "O ","Open   ",IIF(Piktkt.status="H" , "On hold" , SPACE(7))) + ;
              " " + DTOC(PikTkt.Date)
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo ;
                FOR !(PikTkt.Status $ "CX") .AND. Store = lcStore .AND. cWareCode = lcWareCode
   
      *-- Set excat to off.
      SET EXACT OFF
      IF ASCAN(laPikNo , PikTkt.PikTkt) = 0
      
        IF !EMPTY(laPikNo[ALEN(laPikNo,1)])
        
          DIMENSION laPikNo[ALEN(laPikNo,1)+1]

        ENDIF    && End of IF

        laPikNo[ALEN(laPikNo,1)] = Piktkt.piktkt + " " + ;
                IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
                IIF(Piktkt.status $ "O ","Open   ",IIF(Piktkt.status="H" , "On hold" , SPACE(7))) + ;
                " " + DTOC(PikTkt.Date)
      ENDIF
      *-- Restore the excat setting.
      SET EXACT &lcExact
    ENDSCAN
    IF ALEN(laPikNo,1) > 1
      lsPikLst = 1
      
      IF llAskUser
        DO ALPIKLST.SPR
      ELSE && Add new pick tickt.
        *C123847,5  TMI [Start] create a new piktkt no if in DIR03 
        IF ASCAN(laEvntTrig,'CHKAVL')<>0
          lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
        ELSE
          *C123847,5  TMI [End  ] 

          =lfvGetPik(2)
         
          *C123847,5  TMI [End  ] 
        ENDIF
        *C123847,5  TMI [End  ] 
      ENDIF
      
    ELSE
      *** There is Picking ticket no. : {laPikNo[1]}.     ***
      *** Do you wish to assign a new picking ticket no.? ***
      *** < Add > - < New > ***

      lnAskType = IIF(lnAskType=0,gfModalGen("TRM44011B44000","DIALOG" , SUBSTR(laPikNo[1],1,6)),lnAskType)
      
      IF lnAskType = 1
      
        lcPikTkT = SUBSTR(laPikNo[1],1,6)
      ELSE  
        lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
      ENDIF
    ENDIF
  ELSE
    *-- If the order no. not found in the PikTkt file, create new piktkt.
    lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
  ENDIF
ELSE
  *-- If the order no. not found in the PikTkt file, create new piktkt.
  lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
ENDIF

  *-- If the file was opened in this session, close it.
SELECT PikTkt
SET ORDER TO lnOldTag
*-- File was opened in previous session, Set the opinter to th right record.
IF lnRecPkTk > 0 .AND. lnRecPkTk <= RECCOUNT("PikTkt")
  GOTO lnRecPkTk
ENDIF

*-- Restore the exact setting.
SET EXACT &lcExact
*-- Restore the old alias.
SELECT (lnCurAlias)
*-- Return with the selected picking ticket no.
RETURN lcPikTkt
*-- end of lfGetPkTkt. GFMOD


*!*************************************************************
*! Name      : lfStkAvail
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/01/2001
*! Purpose   : Check Stoke Availability
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfStkAvail()
*!*************************************************************
*!C200236,4
FUNCTION lfStkAvail
PRIVATE lcStr, lnCurAlias , lnStkValue
PRIVATE lnCount &&Tmi 07/15/2002
STORE 0 TO lnStkAvail , lnStkValue

lnCurAlias = SELECT(0)
SELECT STYDYE
FOR lnCount = 1 TO SCALE.Cnt
  lcStr = STR(lnCount,1)
  lnStkValue = STYDYE.Stk&lcStr - STYDYE.Alo&lcStr + &lc_TmpOrdL..pik&lcStr
  
  IF lnStkValue <= 0 
    REPLACE &lc_TmpOrdL..pik&lcStr WITH 0 
  ELSE
    IF ( lnStkValue < OrdLine.pik&lcStr)  
      REPLACE &lc_TmpOrdL..pik&lcStr WITH lnStkValue 
    ELSE
      REPLACE &lc_TmpOrdL..pik&lcStr WITH OrdLine.pik&lcStr 
    ENDIF  

    lnStkAvail = lnStkAvail +&lc_TmpOrdL..pik&lcStr
  ENDIF
  
ENDFOR
REPLACE &lc_TmpOrdL..Totpik WITH lnStkAvail 

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfToDoMbi
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/28/2002
*! Purpose   : Run the To Do Screen after login the system
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfToDoMbi()
*!*************************************************************
*!C200249,7
FUNCTION lfToDoMbi

IF !EMPTY(SYUUSER.CUSR_DCOM) .AND. !EMPTY(SYUUSER.CUSR_DMDL)
  =gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPUSER','SH')
  =gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
  =gfOpenFile(gcDataDir+'ORDHDR',gcSysHome+'ORDHDR','SH')
  =gfOpenFile(gcDataDir+'POSHDR',gcSysHome+'POSHDR','SH')
  =gfOpenFile(gcDataDir+'APVENDOR',gcSysHome+'VENCODE','SH')
  =gfOpenFile(gcDataDir+'CUSTOMER',gcSysHome+'CUSTOMER','SH')
  SELECT STYCRPLN
  IF SEEK(gcUser_ID)
    SCAN REST WHILE CUSER_ID+STYLE+CSTYTYPE+PO+CCRPTMPCOD+STR(NLINENO,4) = gcUser_ID
      IF cAdUsrSchd = 'N' .AND. EMPTY(dApproved) .AND.;
         BETWEEN(dRequired,gdSysDate,gdSysDate + SYUUSER.NPROMDAYS - 1)
        IF CSTYTYPE = 'P'
          =SEEK('P'+PO,'POSHDR')
          =SEEK(POSHDR.VENDOR,'APVENDOR')
        ELSE
          =SEEK('O'+PO,'ORDHDR')
          =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
        ENDIF
        SELECT SYSCHDUL
        APPEND BLANK
        REPLACE CSEQNUMBER WITH gfSequence('CSEQNUMBER'),;
                CUSER_ID   WITH gcUser_ID,;
                CCONTTYPE  WITH IIF(STYCRPLN.CSTYTYPE='P','V','C'),;
                CCONT_ID   WITH IIF(STYCRPLN.CSTYTYPE='P',POSHDR.VENDOR,ORDHDR.ACCOUNT),;
                CONTACT    WITH IIF(STYCRPLN.CSTYTYPE='P',APVENDOR.CVENCOMP,CUSTOMER.STNAME),;
                CPHONE     WITH IIF(STYCRPLN.CSTYTYPE='P',APVENDOR.CPHONENO,CUSTOMER.PHONE1),;
                CTRANTYPE  WITH 'T',;
                DTRANDATE  WITH gdSysDate,;
                CTRANTIME  WITH gfGetTime(),;
                CCOMPLETED WITH 'N',;
                CCOMP_ID   WITH gcAct_Comp,;
                PO         WITH STYCRPLN.PO,;
                STYLE      WITH STYCRPLN.STYLE,;
                CCRPTHOPR  WITH STYCRPLN.CCRPTHOPR,;
                CTASKRESN  WITH 'Required by ' + DTOC(STYCRPLN.DREQUIRED)
        SELECT STYCRPLN
        REPLACE CADUSRSCHD WITH 'Y',;
                CSEQNUMBER WITH SYSCHDUL.CSEQNUMBER
      ENDIF
      IF !EMPTY(dApproved) .AND. !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
        REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
      ENDIF
    ENDSCAN
  ENDIF
  USE IN STYCRPLN
  USE IN SYSCHDUL
  USE IN ORDHDR
  USE IN POSHDR
  USE IN APVENDOR
  USE IN CUSTOMER
  =gfOpenFile(gcSysHome+'SYCMENU',gcSysHome+'PROSS_ID','SH')
  IF SEEK('TASKLST   ')
    DO gpMenuBar WITH CMSTR_NAM , VAL(CBAR_POS)
  ENDIF
  USE IN SYCMENU
ENDIF
*--end of lfToDoMbi

*!***************************************************************
*! Name      : lfUpdDyelt  
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/18/2002
*! Purpose   : Update Dyelot field in ORDLINE
*!***************************************************************
*!C200285,1
FUNCTION lfUpdDyelt

*--- setting in IC Module 'Dyelots used'
IF (gfGetMemVar('M_DYELOT') = 'Y') 
  m.Dyelot = ""
  *B606087,1 TMI [Start] Assure that Dyelot entred last is returned
  *SELECT Dyelot , MAX(Dadd_Date) FROM STYDYE WHERE Style + cWarecode + Dyelot =;
                                                   m.Style + laData[31];
                                             .AND. TotStk > 0 INTO ARRAY laDyelot
  RELEASE laDyelot
  SELECT Dyelot FROM STYDYE WHERE Style + cWarecode + Dyelot = ;
                                  m.Style + laData[31] .AND. !EMPTY(Dyelot) .AND. TotStk > 0 ;
                                  ORDER BY Dadd_Date DESC INTO ARRAY laDyelot
  *B606087,1 TMI [End  ]                                              
  IF TYPE('laDyelot[1,1]') <> "U"
    m.Dyelot = laDyelot[1,1]
  ENDIF  
ENDIF

*:**************************************************************************
*:* Name        : lfQkOrdEnt                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2002
*:* Purpose     : Open Quick order entry screen
*:***************************************************************************
*:* Called from : soord
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkOrdEnt()
*:***************************************************************************
FUNCTION lfQkOrdEnt
PRIVATE laSize,laTot,m.Style,m.TotQty,m.Desc1,m.Gros_Price,m.Disc_Pcnt,m.Price,m.Comm1,;
        lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcTempCur,lcTempNote,lcAlias,lnDMarker ,;
        lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
        llChang,laStyClQty,llDifPrice

*-- laSize   :Array to Hold Sizes descriptions for the current style scale
*-- laTot    :Array to hold Column sum and nTotal field sum
*-- m.Style  :Style
*-- m.TotQty :Total qty - check qty
*-- m.Desc1  :Description
*-- m.Gros_Price:Gross price
*-- m.Disc_Pcnt :Dicount percent
*-- m.Price
*-- m.Comm1
*-- lcScrTtl
*-- lcDet_Ttl
*-- lnClrLen
*-- lnClrPos
*-- lcTempCur
*-- lcTempNote
*-- lcAlias
*-- lnDMarker
*-- lcQkWin0
*-- lcQkWin1
*-- lcQkWin2
*-- lcQkWin3
*-- lcOldValue
*-- lcSepart
*-- lcItemPct
*-- laExtSz
*-- lcOrd
*-- llEdit
*-- llChang        
*-- laStyClQty : Array hold Style-Color Qty - usefull in edited styles, 0 in added ones
        
SELECT (lcOrdLine)
SCATTER MEMVAR BLANK
DIMENSION laSize[16],laTot[1,2],laExtSz[1,2],laStyClQty[1,3]
STORE '' TO laSize,lcOldValue
STORE 0 TO laTot,lnClrLen,lnClrPos,m.Gros_Price,m.Price,m.Disc_Pcnt
STORE 1 TO lnDMarker
STORE .F. TO llEdit,llChang,llDifPrice
lcItemPct = gfItemMask('PI')
lcSepart  = SUBSTR(lcItemPct,lnMajorLen+1,1)
*-- Get color position and color length 
=lfGetClrD()

lcScrTtl  = 'Quick Sales Order Entry Screen.'
lcDet_Ttl = 'Quick Lines entry screen'

lcQkWin0    = gftempname()     &&Temp name for Windows.
lcQkWin1    = gftempname()     &&Temp name for Windows.
lcQkWin2    = gftempname()     &&Temp name for Windows.
lcQkWin3    = gftempname()     &&Temp name for Windows.
lcTempCur   = gfTempName()     && Cursor holds qty /color/size
lcTempNote  = gfTempName()     && Cursor holds Notes

PUSH KEY
 
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
*B606386,1 ABD - Close the local tab and let the standered tab work. [Begin]
*ON KEY LABEL TAB   DO lptab 
*B606386,1 ABD - [End]

ON KEY LABEL BACKTAB DO lpBacktab 

lcAlias = SELECT(0)

lcOrd = ORDER(lcOrdLine)   
SET ORDER TO ORDLINST IN (lcOrdLine)
*--CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)

*-- Create temp files
=lfCrtTmp()   
DO (gcScrDir+'SO\QuickOrd.SPX')
POP KEY

SELECT (lcOrdLine)
SET ORDER TO &lcOrd IN (lcOrdLine)
GO BOTTOM

SELECT (lcAlias)
*-- end of lfQkOrdEnt.

*:**************************************************************************
*:* Name        : lfCrtTmp                                     *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Create temp file to browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtTmp()
*:***************************************************************************
FUNCTION lfCrtTmp
PRIVATE lnCount

CREATE TABLE (gcWorkDir+lcTempNote) (NOTE_MEM M)
APPEND BLANK

CREATE TABLE (gcWorkDir+lcTempCur) (LASTLINE C(1),STYMAJOR C(lnMajorLen),COLOR C(6),COLORDSC C(20),SZ1 N(5),;
             SZ2 N(5),SZ3 N(5),SZ4 N(5),SZ5 N(5),SZ6 N(5),SZ7 N(5),SZ8 N(5),SZ9 N(5),SZ10 N(5),;
             SZ11 N(5),SZ12 N(5),SZ13 N(5),SZ14 N(5),SZ15 N(5),SZ16 N(5),NTOTAL N(6))
INDEX ON LASTLINE+STYMAJOR+COLOR TAG (lcTempCur)

*-- end of lfCrtTmp.

*:**************************************************************************
*:* Name        : lfBrowLine                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Browse Line fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBrowLine()
*:***************************************************************************
FUNCTION lfBrowLine
PARAMETERS llGoTop

SELECT (lcTempCur)
IF llGoTop
  GO TOP
ENDIF  
  
lnDMarker  = RECNO()
lcBrowFlds = "cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
             "COLORDSC :R :H=PADC('Colours',16) :W=.F.,"+;              
             "SZ1  :H=laSize[1]:"+lfLen(1) +":P='9999':V=lfvqFld('1'):W=lfwqFld(1),"+;
             IIF(EMPTY(laSize[2]),'',"SZ2  :H=laSize[2]:"+lfLen(2) +":P='9999':V=lfvqFld('2'):W=lfwqFld(2),")+;
             IIF(EMPTY(laSize[3]),'',"SZ3  :H=laSize[3]:"+lfLen(3) +":P='9999':V=lfvqFld('3'):W=lfwqFld(3),")+;
             IIF(EMPTY(laSize[4]),'',"SZ4  :H=laSize[4]:"+lfLen(4) +":P='9999':V=lfvqFld('4'):W=lfwqFld(4),")+;
             IIF(EMPTY(laSize[5]),'',"SZ5  :H=laSize[5]:"+lfLen(5) +":P='9999':V=lfvqFld('5'):W=lfwqFld(5),")+;
             IIF(EMPTY(laSize[6]),'',"SZ6  :H=laSize[6]:"+lfLen(6) +":P='9999':V=lfvqFld('6'):W=lfwqFld(6),")+;
             IIF(EMPTY(laSize[7]),'',"SZ7  :H=laSize[7]:"+lfLen(7) +":P='9999':V=lfvqFld('7'):W=lfwqFld(7),")+;
             IIF(EMPTY(laSize[8]),'',"SZ8  :H=laSize[8]:"+lfLen(8) +":P='9999':V=lfvqFld('8'):W=lfwqFld(8),")+;
             IIF(EMPTY(laSize[9]),'',"SZ9  :H=laSize[9]:"+lfLen(9) +":P='9999':V=lfvqFld('9'):W=lfwqFld(9),") 
lcBrowFlds = lcBrowFlds +;
             IIF(EMPTY(laSize[10]),'',"SZ10 :H=laSize[10]:"+lfLen(10)+":P='9999':V=lfvqFld('10'):W=lfwqFld(10),")+;   
             IIF(EMPTY(laSize[11]),'',"SZ11 :H=laSize[11]:"+lfLen(11)+":P='9999':V=lfvqFld('11'):W=lfwqFld(11),")+;
             IIF(EMPTY(laSize[12]),'',"SZ12 :H=laSize[12]:"+lfLen(12)+":P='9999':V=lfvqFld('12'):W=lfwqFld(12),")+;
             IIF(EMPTY(laSize[13]),'',"SZ13 :H=laSize[13]:"+lfLen(13)+":P='9999':V=lfvqFld('13'):W=lfwqFld(13),")+;
             IIF(EMPTY(laSize[14]),'',"SZ14 :H=laSize[14]:"+lfLen(14)+":P='9999':V=lfvqFld('14'):W=lfwqFld(14),")+;
             IIF(EMPTY(laSize[15]),'',"SZ15 :H=laSize[15]:"+lfLen(15)+":P='9999':V=lfvqFld('15'):W=lfwqFld(15),")+;
             IIF(EMPTY(laSize[16]),'',"SZ16 :H=laSize[16]:"+lfLen(16)+":P='9999':V=lfvqFld('16'):W=lfwqFld(16),")+;
             "NTOTAL :H='Total':R :P='999999'"

BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvqBrow();       
       WHEN lfwBrowUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcQkWin2) IN WINDOW (lcQkWin0)

*--- Notes
=lfRefresh(lcQkWin0)
=lfRefresh(lcDet_Ttl)

*-- end of lfBrowLine.

*:**************************************************************************
*:* Name        : lfwqFld                                         *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : When fn. for fields in temp browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwqFld()
*:***************************************************************************
FUNCTION lfwqFld
PARAMETERS lnIndex
PRIVATE lcIndex
lcIndex = ALLTRIM(STR(lnIndex))
lcOldValue = SZ&lcIndex
*RETURN EMPTY(LASTLINE)
*-- end of lfwqFld.

*:**************************************************************************
*:* Name        : lfvqFld                                       *C200358,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Valid fn. for fields in temp browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqFld()
*:***************************************************************************
FUNCTION lfvqFld
PARAMETERS lcIndex
PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF lcOldValue #  SZ&lcIndex
  *C200358,4 TMI [Start] Prevent update totals in total field
  IF !EMPTY(LASTLINE)
    REPLACE SZ&lcIndex WITH lcOldValue
    RETURN
  ENDIF
  *C200358,4 TMI [End  ] 

  IF SZ&lcIndex < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    REPLACE SZ&lcIndex WITH lcOldValue
    RETURN
  ENDIF
  IF SZ&lcIndex > 999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|999.')
    REPLACE SZ&lcIndex WITH lcOldValue
    RETURN
  ENDIF

  m.AddSty = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)
  SELECT (lcTempCur)
  *-- Assign a style to a location if not
  lnIncrm = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)    
    IF lnIncrm<VAL(lcIndex) AND VAL(lcIndex)<=lnIncrm+laExtSz[lnCount,2]
      EXIT
    ENDIF
    lnIncrm = lnIncrm + laExtSz[lnCount,2]
  ENDFOR
  IF laSetups[5,2]='Y' .AND. !SEEK(m.AddSty+laExtSz[lnCount,1]+laData[31]+SPACE(10),'StyDye')
    *E300408,1 Message : 40012
    *E300408,1 Style/color xxx is not assigned to warehouse xxx
    *E300408,1 Button : 40002
    *E300408,1 Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+laExtSz[lnCount,1])+'|'+TRIM(laData[31]))=1
      DO gpAdStyWar WITH m.AddSty+laExtSz[lnCount,1],SPACE(10),laData[31]
    ELSE      
      REPLACE SZ&lcIndex WITH 0
      KEYBOARD '{LEFTARROW}'
      RETURN
    ENDIF
  ENDIF
  
  lnLineTot = 0 
  *--Line Total
  FOR lnCount = 1 TO 16
    lcCount = ALLTRIM(STR(lnCount))
    lnLineTot = lnLineTot + SZ&lcCount
  ENDFOR
  REPLACE NTOTAL WITH lnLineTot
  
  *-- Column and All Totals
  SELECT SUM(SZ&lcIndex),SUM(NTOTAL) FROM (lcTempCur) WHERE EMPTY(LASTLINE) INTO ARRAY laTot
  lnRecno = RECNO()
  GO BOTTOM
  REPLACE SZ&lcIndex WITH laTot[1,1],;
          NTOTAL     WITH laTot[1,2]

  llChang = .T.
  IF BETWEEN(lnRecno,1,RECCOUNT()-1)
    GOTO (lnRecno)
  ELSE
    GO TOP
  ENDIF  

  lcSavStat = IIF(llEdit OR laTot[1,2]>0,'ENABLE','DISABLE')
  SHOW GET pbSav &lcSavStat

ENDIF  
*-- end of lfvqFld.

*:**************************************************************************
*:* Name        : lfLen                                       *C200358,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Return the width of the column
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfLen()
*:***************************************************************************
FUNCTION lfLen
PARAMETERS lnIndex
PRIVATE lnRet 
lnRet = IIF( LEN(laSize[lnIndex])<3 , 3 , LEN(laSize[lnIndex])+1 )
RETURN STR(lnRet,1)+IIF(!EMPTY(LASTLINE),":R ","")

*-- end of lfLen.

*:**************************************************************************
*:* Name        : lfvqBrow                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Valid function for browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqBrow()
*:***************************************************************************
FUNCTION lfvqBrow
IF WONTOP() # lcDet_Ttl
  = gfStopBrow()
ENDIF

*:**************************************************************************
*:* Name        : lfwBrowUp                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : When function for browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrowUp()
*:***************************************************************************
FUNCTION lfwBrowUp

lnDMarker = RECNO(lcTempCur)
SHOW WINDOW (lcDet_Ttl) REFRESH
*-- end of lfwBrowUp.

*:**************************************************************************
*:* Name        : lpTab                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/23/2002
*:* Purpose     : Tab fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpTab()
*:***************************************************************************
PROCEDURE lpTab

IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcQkWin3)
  _CUROBJ=OBJNUM(pbSav)  
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*:**************************************************************************
*:* Name        : lpBacktab                                       *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Backtab Trap fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpBacktab ()
*:***************************************************************************
FUNCTION lpBacktab 
IF RECCOUNT(lcTempCur) = 0
  DO CASE
    CASE WONTOP() = lcDet_Ttl
      ACTIVATE WINDOW (lcQkWin1)
    CASE WONTOP() = lcQkWin1
      ACTIVATE WINDOW (lcQkWin3)             
    CASE WONTOP() = lcQkWin3 
      ACTIVATE WINDOW (lcDet_Ttl)
  ENDCASE
ELSE
  IF WONTOP() = lcDet_Ttl
    ACTIVATE WINDOW (lcQkWin1)
    _CUROBJ=OBJNUM(m.Comm1)
  ELSE
    DO CASE
      CASE WONTOP() = lcQkWin1
        IF _CUROBJ = OBJNUM(m.TotQty)
          ACTIVATE WINDOW (lcQkWin3)
          _CUROBJ = OBJNUM(pbExit)
        ELSE
          IF llDifPrice
            IF _CUROBJ = OBJNUM(m.Comm1)
              _CUROBJ = OBJNUM(m.TotQty)
            ENDIF
            IF _CUROBJ = OBJNUM(m.TotQty)
              ACTIVATE WINDOW (lcQkWin3)
              _CUROBJ = OBJNUM(pbExit)             
            ENDIF
          ELSE
            _CUROBJ=_CUROBJ - 1
          ENDIF
        ENDIF
        
      CASE WONTOP() = lcQkWin3
        IF laTot[1,2]=0
          IF _CUROBJ = OBJNUM(pbClear)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ = OBJNUM(pbSav)
          ENDIF
        ELSE
          IF _CUROBJ = OBJNUM(pbSav)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ=_CUROBJ - 1
          ENDIF
        ENDIF
        
    ENDCASE
  ENDIF
ENDIF  
*-- end of lpBacktab .

*:**************************************************************************
*:* Name        : lfvClear                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/2002
*:* Purpose     : Clear valid fucntio for screen QkOrdEnt
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvClear()
*:***************************************************************************
FUNCTION lfvClear

STORE ' ' TO m.Style,m.Desc1,laSize
STORE 0  TO m.Price,m.TotQty,m.Disc_Pcnt,m.Gros_Price,m.Comm1,laTot
STORE  .F. TO llEdit,llChang

SHOW GET m.Style ENABLE 
SHOW GET m.Desc1 DISABLE
SHOW GET m.Price DISABLE
SHOW GET m.TotQty DISABLE
SHOW GET m.Disc_Pcnt DISABLE
SHOW GET m.Gros_Price DISABLE
SHOW GET m.Comm1 DISABLE
SHOW GET pbNote DISABLE
SHOW GET pbClear DISABLE
SHOW GET pbSav   DISABLE
_CUROBJ = OBJNUM(m.Style)

*--Clear notes
SELECT (lcTempNote)
REPLACE NOTE_MEM WITH ''

SELECT (lcTempCur)
ZAP   
=lfBrowLine()

*-- end of lfvClear.


*:**************************************************************************
*:* Name        : lfvQStyle                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/2002
*:* Purpose     : Valid funciton for m.QStyle field
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQStyle()
*:***************************************************************************
FUNCTION lfvQStyle
PRIVATE lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = ALIAS()

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

m.Style = PADR(ALLT(m.Style),lnMajorLen)


IF !EMPTY(m.Style) AND ('?' $ m.Style .OR. !SEEK(m.Style,'STYLE')))
  SELECT STYLE  
  lcGetSty = PADR(gfStyBrw("M" , m.Style , "" , .F.),lnMajorLen)
  llBrowse = .F.
  IF EMPTY(lcGetSty)
    m.Style = lcOldvalue
    _CUROBJ = OBJNUM(m.Style) 
    RETURN
  ELSE
    m.Style = lcGetSty
  ENDIF
ENDIF  

IF SEEK(m.Style,'Style')
  *E300408,1 Message : 32017
  *E300408,1 Style scale not found in the scale file. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32018
  *E300408,1 This is a canceled style. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32019
  *E300408,1 This style/color is on hold. 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  *E300408,1 Message : 32020
  *E300408,1 Styles restricted to XXX!
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *E300408,1 Message : 32021
  *E300408,1 Style XXX date is 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  *B122433,1  TMI [Start] Add season check
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
  *      gfModalGen('TRM32017B00000','ALERT')=1) ;
  * .OR. (Style.Status='X' .AND. ;
  *      gfModalGen('TRM32018B00000','ALERT')=1) ;
  * .OR. (Style.Status='H' .AND. ;
  *      gfModalGen('QRM32019B32003','ALERT')=2) ;
  * .OR. (Style.cDivision <> laData[15] .AND. ;
  *      gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
  * .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
  *      gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
  * .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
  *      gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)  
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
        gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2) ;
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND TYPE('laSetups[15,2]')='L' AND laSetups[15,2] = .F.,.F.,(ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1))        
  *B122433,1  TMI [End  ]  Add season check
  
    STORE lcOldvalue TO m.Style
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  
  *--Get style price according to ordered quantity
  PUSH KEY
  ON KEY
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    lcStyle = STYLE.STYLE
    m.Gros_Price = MAX( lfGetprice(lcStyle,lcPriceLvl,m.TotQty) , 0 )
  ENDIF
  POP KEY

  m.Price    = m.Gros_Price
  m.Desc1    = Style.Desc1
  m.Comm1    = IIF(Style.Commission,laData[28],0)   &&The first commession 
  
  *-- Get Style discount percent and Calculate net price  
  IF !m.lContract
    *-- get the cDiscCode From stydye in every case
    lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    m.Disc_Pcnt = 0 
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the disecound related filed to now which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldstartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldstartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
        lnDisc_Pcnt = m.Disc_Pcnt
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        m.Disc_Pcnt = lnDisc_Pcnt
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  ENDIF 

  *C200358,4 TMI [Start] Show if there are more than one price for each scale
  PRIVATE laPrices,lcStyClr
  SELECT STYLE  
  lcStyClr = SUBSTR(STYLE,1,16)
  IF !llMulCurr .OR. laData[33]=gcBaseCurr
    SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices
    SCAN REST WHILE STYLE = lcStyClr
      llDifPrice = PRICEA <> laPrices[1] .OR. ;
                   PRICEB <> laPrices[2] .OR. ;
                   PRICEC <> laPrices[3]
      IF llDifPrice
        EXIT
      ENDIF             
      SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices             
    ENDSCAN
  ELSE
    SELECT STYPRICE
    LOCATE FOR STYLE=lcStyClr .AND. CCURRCODE = laData[33]
    SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices
    SCAN REST WHILE STYLE = lcStyClr FOR CCURRCODE = laData[33]
      llDifPrice = PRICEA <> laPrices[1] .OR. ;
                   PRICEB <> laPrices[2] .OR. ;
                   PRICEC <> laPrices[3]
      IF llDifPrice
        EXIT
      ENDIF             
      SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices             
    ENDSCAN
  ENDIF
  *C200358,4 TMI [End  ] Show if there are more than one price for each scale 
  
  ************************************************  
  
  lcStat = IIF(!EMPTY(m.Style),'ENABLE','DISABLE')
  SHOW GET pbClear &lcStat  
  
  *--Get Sizes  
  laSize = ''
  =SEEK(m.Style,'STYLE')
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = STYLE.SCALE
  SELECT Scale,CNT FROM SCALE WHERE Type+Scale='S'+SUBSTR(m.SCALE,1,lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
  SELECT SCALE
  =SEEK('S'+laExtSz[1],'SCALE')
  IF !EMPTY(SCALE.CDIM1)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than one dimension.')
    =lfvClear() 
    RETURN
  ENDIF

  *C200358,4 TMI [Start] 
  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR  
  IF lnIncrmnt > 16
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than 16 Sizes.')
    =lfvClear() 
    RETURN
  ENDIF
  *C200358,4 TMI [End  ] 

  *C200358,4 TMI [Start] if style is entred befor , then ignor it  **07/16/2002
  IF SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine))
    *--This Style has entred on this order
    =gfModalGen('INM32029B00000','ALERT',IIF(lcOrdType='C','Contract','Order'))
    =lfvClear() 
    RETURN
  ENDIF
  *C200358,4 TMI [End  ] 
  
  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    =SEEK('S'+laExtSz[lnCount,1],'SCALE')
    FOR lnJ = 1 TO laExtSz[lnCount,2]
      lcZ = STR(lnJ,1)
      laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.Sz&lcZ)
    ENDFOR
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR
  
  *--Clear Notes (if any)
  SELECT (lcTempNote)
  REPLACE NOTE_MEM WITH ''
  
  *--Get Colors
  SELECT (lcTempCur)
  ZAP
  
  *--Create color array
  SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
    WHERE STYLE = PADR(m.Style,lnMajorLen) ;
    INTO ARRAY laClr  
  
  *--Add a line for each color
  PRIVATE lnMaxInd
  lnMaxInd = IIF(ALEN(laClr,1)>=10,10,ALEN(laClr,1))
  FOR lnClrs = 1 TO lnMaxInd
    SELECT (lcTempCur)
    SCATTER MEMVAR BLANK
    M.STYMAJOR = PADR(m.Style,lnMajorLen)
    M.COLOR = laClr[lnClrs]
    M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
    lnIncrmnt = 0
    *--Update Quantites from lcOrdLine if style is entred befor for the same store
    FOR lnCount = 1 TO ALEN(laExtSz,1)
      lcSeekSty = M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen)+laExtSz[lnCount,1]
      IF SEEK(lcOrdType+laData[1]+laData[3]+lcSeekSty,(lcOrdLine))
        FOR lnJ = 1 TO laExtSz[lnCount,2]
          lcZ = STR(lnJ,1)
          lcX = ALLT(STR(lnIncrmnt+lnJ))
          M.SZ&lcX = &lcOrdLine..Qty&lcZ
          m.nTotal = m.nTotal + &lcOrdLine..Qty&lcZ
        ENDFOR
        M.NOTE_MEM = ALLT(&lcOrdLine..Note_Mem)
        IF !EMPTY(M.NOTE_MEM)
          SELECT (lcTempNote)
          REPLACE NOTE_MEM WITH M.NOTE_MEM
        ENDIF        
        *C200358,4 TMI [Start] Save Style and totqty to compre with it in save process
        DIMENSION laStyClQty[ALEN(laStyClQty,1)+1,3]
        laStyClQty[ALEN(laStyClQty,1),1] = &lcOrdLine..Style
        laStyClQty[ALEN(laStyClQty,1),2] = &lcOrdLine..TotBook
        laStyClQty[ALEN(laStyClQty,1),3] = &lcOrdLine..TotQty
        *C200358,4 TMI [End  ] Save Style and totqty to compre with it in save process 
      ENDIF
      lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
    ENDFOR     
    INSERT INTO (lcTempCur) FROM MEMVAR
  ENDFOR  
 
  =SEEK(PADR(m.Style,lnMajorLen),'STYLE')
  
  SELECT CHR(255),'','','Totals',;
         SUM(SZ1),SUM(SZ2),SUM(SZ3),SUM(SZ4),SUM(SZ5),SUM(SZ6),SUM(SZ7),SUM(SZ8),;
         SUM(SZ9),SUM(SZ10),SUM(SZ11),SUM(SZ12),SUM(SZ13),SUM(SZ14),SUM(SZ15),SUM(SZ16),SUM(NTOTAL) ;
         FROM (lcTempCur) ;
         INTO ARRAY laSum

  *--Add total line
  INSERT INTO (lcTempCur) FROM ARRAY laSum
  STORE laSum[21] TO laTot[1,2],m.TotQty
  
  llEdit = laSum[21]>0

  SHOW GET m.Style DISABLE
  SHOW GET m.TotQty ENABLE
  SHOW GET m.Comm1 ENABLE
  SHOW GET pbNote ENABLE
  SHOW GET m.Desc1 DISABLE
  
  IF llDifPrice  
    SHOW GET m.Gros_Price DISABLE
    SHOW GET m.Disc_Pcnt DISABLE
    SHOW GET m.Price DISABLE
  ELSE
    SHOW GET m.Gros_Price ENABLE
    SHOW GET m.Disc_Pcnt ENABLE
    SHOW GET m.Price ENABLE
  ENDIF  
  
  laSavStat = IIF(laSum[21]>0,'ENABLE','DISABLE')
  SHOW GET pbSav &laSavStat

  =lfBrowLine(.T.)
ENDIF

*-- end of lfvQStyle.

*:**************************************************************************
*:* Name        : lfvQkSav                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/2002
*:* Purpose     : Save valid fucntio for screen QkOrdEnt
*:***************************************************************************
*:* Called from : QkOrdEnt.scx
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQkSav()
*:***************************************************************************
FUNCTION lfvQkSav
PRIVATE lcOrd,lnQkCnt,lnJ,lcJ,lcQ,lnIncrmnt,lnK,llDfChkQty


*- Compare check qty with total qty  
llDfChkQty = m.TotQty <> laTot[1,2]
IF llDfChkQty AND gfModalGen('QRM32031B32000','ALERT',ALLTRIM(STR(laTot[1,2],8)))=2  
  RETURN .F.
ENDIF

SELECT (lcTempNote)
SCATTER MEMVAR MEMO

*--Assign variables values
m.CORDTYPE  = lcOrdType
m.ORDER     = laData[1]
m.ACCOUNT   = laData[2]
m.STORE     = laData[3]
m.CustPo    = IIF(!laData[7],&lcOrdHdr..CustPo,'')   &&Save Cust PO # in OrdLine if not Multi Store
m.cWareCode = laData[31]
m.Start     = laData[9]
m.Complete  = laData[10]

SELECT (lcTempCur)
GO TOP
SCAN FOR EMPTY(LASTLINE)
  lnIncrmnt = 0
  FOR lnQkCnt = 1 TO ALEN(laExtSz,1)    
    SELECT (lcTempCur)
    m.Style = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+laExtSz[lnQkCnt,1]
    IF SEEK(m.Style,'STYLE')
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                 m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook
      FOR lnJ = lnIncrmnt + 1 TO lnIncrmnt + laExtSz[lnQkCnt,2]
        lcJ = ALLT(STR(lnJ))
        lcQ = STR(lnJ - lnIncrmnt,1)
        m.TotQty = m.TotQty +  SZ&lcJ
        m.Qty&lcQ = SZ&lcJ
        m.Book&lcQ = SZ&lcJ
      ENDFOR    
      m.TotBook  = m.TotQty
      m.Scale    = STYLE.SCALE     
      m.Desc1    = STYLE.Desc1 
      m.PrePak   = Style.PrePak
      m.Season   = Style.Season
      m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
      m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
      m.Flag     = 'N'      
      lnLines    = lnLines + 1
      m.LineNo   = lnLines      

*-*      IF SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine)) .AND. m.TotQty>0
*-*        SELECT (lcOrdLine)
*-*        REPLACE DESC1      WITH m.Desc1,;
*-*                Price      WITH m.Price,;
*-*                Gros_Price WITH m.Gros_Price ,;
*-*                Disc_Pcnt  WITH m.Disc_Pcnt ,;
*-*                Price      WITH m.Price ,;
*-*                Comm1      WITH m.Comm1                 
*-*        REPLACE QTY1 WITH m.Qty1,;
*-*                QTY2 WITH m.Qty2,;
*-*                QTY3 WITH m.Qty3,;
*-*                QTY4 WITH m.Qty4,;
*-*                QTY5 WITH m.Qty5,;
*-*                QTY6 WITH m.Qty6,;
*-*                QTY7 WITH m.Qty7,;
*-*                QTY8 WITH m.Qty8,;
*-*                TOTQTY WITH m.TOTQty                
*-*        IF &lcOrdLine..FLAG = 'N'
*-*          REPLACE BOOK1 WITH m.BOOK1,;
*-*                  BOOK2 WITH m.BOOK2,;
*-*                  BOOK3 WITH m.BOOK3,;
*-*                  BOOK4 WITH m.BOOK4,;
*-*                  BOOK5 WITH m.BOOK5,;
*-*                  BOOK6 WITH m.BOOK6,;
*-*                  BOOK7 WITH m.BOOK7,;
*-*                  BOOK8 WITH m.BOOK8,;
*-*                  TOTBOOK WITH m.TOTBOOK
*-*        ENDIF
*-*      ELSE
*-*        IF m.TotQty>0
*-*          INSERT INTO (lcOrdLine) FROM MEMVAR
*-*        ELSE
*-*          SELECT (lcOrdLine)
*-*          DELETE
*-*        ENDIF
*-*      ENDIF

      IF !SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine))
        *-- Get price for each line of scales has different prices , 
        *-- or check qty differs from entred qty
        IF llDifPrice
          =lfQkPrice()
        ENDIF
        IF m.TotQty>0
          INSERT INTO (lcOrdLine) FROM MEMVAR
          *--Update laData array 
          laData[35] = laData[35] + m.TotBook
          laData[36] = laData[36] + m.TotBook*m.Price
          laData[41] = laData[41] + m.TotQty
          laData[42] = laData[42] + m.TotQty*m.Price          
        ENDIF        
      ELSE
        IF m.TotQty>0
          SELECT (lcOrdLine)
*          REPLACE DESC1      WITH m.Desc1
*                  Price      WITH m.Price,;
*                  Gros_Price WITH m.Gros_Price ,;
*                  Disc_Pcnt  WITH m.Disc_Pcnt ,;
*                  Price      WITH m.Price ,;
*                  Comm1      WITH m.Comm1                 
          REPLACE QTY1 WITH m.Qty1,;
                  QTY2 WITH m.Qty2,;
                  QTY3 WITH m.Qty3,;
                  QTY4 WITH m.Qty4,;
                  QTY5 WITH m.Qty5,;
                  QTY6 WITH m.Qty6,;
                  QTY7 WITH m.Qty7,;
                  QTY8 WITH m.Qty8,;
                  TOTQTY WITH m.TOTQty                
          IF &lcOrdLine..FLAG = 'N'
            REPLACE BOOK1 WITH m.BOOK1,;
                    BOOK2 WITH m.BOOK2,;
                    BOOK3 WITH m.BOOK3,;
                    BOOK4 WITH m.BOOK4,;
                    BOOK5 WITH m.BOOK5,;
                    BOOK6 WITH m.BOOK6,;
                    BOOK7 WITH m.BOOK7,;
                    BOOK8 WITH m.BOOK8,;
                    TOTBOOK WITH m.TOTBOOK
            *C200358,4 TMI [Start] Updates Booked qty
            IF ASCAN(laStyClQty,m.Style)>0
              lnK = ASUBSCRIPT(laStyClQty,ASCAN(laStyClQty,m.Style),1)
              laData[35] = laData[35] + (m.TotBook-laStyClQty[lnK,2])
              laData[36] = laData[36] + (m.TotBook-laStyClQty[lnK,2])*&lcOrdline..Price
            ENDIF
            *C200358,4 TMI [End  ] 
          ENDIF 
          *C200358,4 TMI [Start] Update qty
          IF ASCAN(laStyClQty,m.Style)>0
            lnK = ASUBSCRIPT(laStyClQty,ASCAN(laStyClQty,m.Style),1)
            laData[41] = laData[41] + (m.TotQty-laStyClQty[lnK,3])
            laData[42] = laData[42] + (m.TotQty-laStyClQty[lnK,3])*&lcOrdline..Price
          ENDIF
          *C200358,4 TMI [End  ] 
          
        ELSE
        
          SELECT (lcOrdLine)
          *C200358,4 TMI [Start] Update qty
          laData[35] = laData[35] - m.TotBook
          laData[36] = laData[36] - m.TotBook*&lcOrdline..Price
          laData[41] = laData[41] - m.TotQty
          laData[42] = laData[42] - m.TotQty*&lcOrdline..Price
          *C200358,4 TMI [End  ] 
          DELETE
        ENDIF
        
      ENDIF

      lnIncrmnt = lnIncrmnt + laExtSz[lnQkCnt,2]
    ENDIF
  ENDFOR
ENDSCAN

*--Save NOTE_MEM
SELECT (lcTempCur)
LOCATE
m.Style = STYMAJOR
SELECT (lcOrdLine)
IF SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine))
  M.NOTE_MEM = &lcTempNote..NOTE_MEM
  *--Clear field note_mem for colors other than the first color filled with qty>0
  SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = lcOrdType+laData[1]+laData[3]+m.Style
    REPLACE NOTE_MEM WITH m.Note_mem
    m.Note_mem = ''      
  ENDSCAN
ENDIF 
*-- Clear screen
=lfvClear()
*-- end of lfvQkSav.

*:**************************************************************************
*:* Name        : lfvqTotQty                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : Valid fn for total qty
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqTotQty()
*:***************************************************************************
FUNCTION lfvqTotQty
IF m.TotQty < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.TotQty = lcOldValue
  _CUROBJ = OBJNUM(m.TotQty)
  RETURN
ENDIF
IF LASTKEY()=15
  ACTIVATE WINDOW (lcQkWin3)
  _CUROBJ=OBJNUM(pbExit)  
ENDIF
=lfDoTab()

*-- end of lfvqTotQty.

*:**************************************************************************
*:* Name        : lfvqStyDesc                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqStyDesc()
*:***************************************************************************
FUNCTION lfvqStyDesc
  
*-- end of lfvqStyDesc.

*:**************************************************************************
*:* Name        : lfvNotes                                         *C200358,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Entering Line notes , that will be saved againest the first 
*                 color that has quantity
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvNotes()
*:***************************************************************************
FUNCTION lfvNotes
PRIVATE lnRecno,lcAlias
SELECT (lcTempNote)
DO (gcScrDir+"ARLNOTES.SPX")
SELECT (lcTempCur)
*-- end of lfvNotes.

*:**************************************************************************
*:* Name        : lfvqPrcDisc                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPrcDisc()
*:***************************************************************************
FUNCTION lfvqGPrice
IF m.Gros_Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ = OBJNUM(m.Gros_Price)
  RETURN
ENDIF
m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
SHOW GET m.Price
=lfDoTab()

*-- end of lfvqGPrice.

*:**************************************************************************
*:* Name        : lfvqPrcDisc                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPrcDisc()
*:***************************************************************************
FUNCTION lfvqPrcDisc
IF m.Disc_Pcnt < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Disc_Pcnt = lcOldValue
  _CUROBJ = OBJNUM(m.Disc_Pcnt)
  RETURN
ENDIF
IF m.Disc_Pcnt > 99.99
  *--Can not exceed 999
  *-- 40171 : ð cannot exceeds ð  
  =gfModalGen('TRM40171B00000','DIALOG','Percent Discount|99.99')
  m.Disc_Pcnt = lcOldValue
  _CUROBJ = OBJNUM(m.Disc_Pcnt)
  RETURN
ENDIF
m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
SHOW GET m.Price
=lfDoTab()
*-- end of lfvqPrcDisc.

*:**************************************************************************
*:* Name        : lfvqNPrice                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPrcDisc()
*:***************************************************************************
FUNCTION lfvqNPrice

IF m.Price < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Price = lcOldValue
  _CUROBJ = OBJNUM(m.Price)
  RETURN
ENDIF

m.Price = IIF(m.Price>m.Gros_Price,m.Gros_Price,m.Price)
m.Disc_Pcnt = IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price)

SHOW GET m.Gros_Price
SHOW GET m.Disc_Pcnt
SHOW GET m.Price

=lfDoTab()
*-- end of lfvqNPrice.


*:**************************************************************************
*:* Name        : lfvqComm                                        *C200358,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/25/2002
*:* Purpose     : Valid fn for commesion 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqComm()
*:***************************************************************************
FUNCTION lfvqComm

*-- end of lfvqComm.

*:**************************************************************************
*:* Name        : lfGetClrD                                      *C200358,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Get Color Position and Color length
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetClrD()
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfvQkClose                                     *C200358,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Close fn. for QkOrdEnt screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQkClose()
*:***************************************************************************
FUNCTION lfvQkClose
PRIVATE llClose
llClose = .T.
IF (llEdit AND llChang) OR (!llEdit AND laTot[1,2]>0)
  *M38093   ð. Are you sure you want to proceed with closing ð?  
  *B32005   \<Proceed;\?\<Cancel
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been '+IIF(llEdit,'changed','entered');
                                                +'|The Quick Order Entry screen')=1)
ENDIF
IF llClose
  ERASE (gcWorkDir+lcTempCur+'.*')
  ERASE (gcWorkDir+lcTempNote+'.*')
  CLEAR READ
ELSE
  ACTIVATE WINDOW (lcDet_Ttl)  
ENDIF

*-- end of lfvQkClose.

*:**************************************************************************
*:* Name        : lfQkPrice                                       *C200358,4
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : Get Prices in save case
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkPrice()
*:***************************************************************************
FUNCTION lfQkPrice

  *--Get style price according to ordered quantity
  PUSH KEY
  ON KEY
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    m.Gros_Price = MAX( lfGetprice(m.Style,lcPriceLvl,m.TotQty) , 0 )
  ENDIF
  POP KEY

  m.Price    = m.Gros_Price
  
  *-- Get Style discount percent and Calculate net price  
  IF !m.lContract
    *-- get the cDiscCode From stydye in every case
    lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    m.Disc_Pcnt = 0 
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the disecound related filed to now which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldstartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldstartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
        lnDisc_Pcnt = m.Disc_Pcnt
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        m.Disc_Pcnt = lnDisc_Pcnt
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  ENDIF 

*-- end of lfQkPrice.


*:**************************************************************************
*:* Name        : lfQkWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : When Fn that disable tab
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkWhen()
*:***************************************************************************
FUNCTION lfQkWhen
*C200358,4 TMI 
lcOldValue = EVALUATE('m.' + SYS(18))
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*-- end of lfQkWhen.

*:**************************************************************************
*:* Name        : lfDoTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : ReDefine tab keys
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDoTab()
*:***************************************************************************
FUNCTION lfDoTab
*C200358,4 TMI 
  *B606386,1 ABD - Close the local tab and let the standered tab work. [Begin]
  *ON KEY LABEL TAB   DO lptab 
  *B606386,1 ABD - [End]
  ON KEY LABEL BACKTAB DO lpBacktab 

*-- end of lfDoTab.

*:**************************************************************************
*:* Name        : lfwGPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/18/2002
*! Purpose      : Disable When function for Style Gross price (Specialized for MBI)
*               : This is to use only price level A
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwGPrice()
*:***************************************************************************
FUNCTION lfwGPrice
RETURN .T.


*!**************************************************************************
*! Name      : lfGetORQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Get Ordered Qty
*!**************************************************************************
*! Example   : = lfALSAVORD()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 Save data to Bin Location files

FUNCTION lfGetORQty
PARAMETER lnQty,lnSize,lcPkTkt,lcOrder

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order
IF lnQty = 0 
  SELECT(lnAlias)
  RETURN
ENDIF

lcSize = ALLT(STR(lnSize,2))

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF

IF !EMPTY(STYLE.cPrimClss&lcSize)
  lcClass =  STYLE.cPrimClss&lcSize
ELSE
  IF !EMPTY(STYLE.cSecClss&lcSize)
    lcClass =  STYLE.cSecClss&lcSize
  ELSE
    lcClass =  STYLE.cRemClss&lcSize
  ENDIF
ENDIF

  
IF !SEEK(lcPkTkt+WHBINLOC.cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 
  =SEEK(WHBINLOC.cWareCode+WHBINLOC.cLocation,'WHSLOC')
  SELECT PKBINLOC
  APPEND BLANK
  REPLACE STYLE      WITH WHBINLOC.Style,;
          CWARECODE  WITH WHBINLOC.cWareCode,;
          PIKTKT     WITH lcPkTkt ,;
          ORDER      WITH lcOrder,;
          cFlatHang  WITH WHSLOC.cFlatHang,;
          clocation  WITH WHBINLOC.cLocation ,;
          Qty&lcSize WITH lnQty,;
          TotQty     WITH lnQty
  REPLACE cBinClass&lcSize  WITH lcClass
  =gfAdd_Info('PKBINLOC')
  IF TotQty = 0
    DELETE
  ENDIF         

ELSE
  SELECT PKBINLOC
  REPLACE Qty&lcSize WITH Qty&lcSize+ lnQty,;
          TotQty     WITH TotQty+lnQty
  REPLACE cBinClass&lcSize  WITH lcClass
  IF TotQty = 0
    DELETE
  ENDIF         
ENDIF
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfALSAVAUT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Save Data to Automatic alocation screen
*!**************************************************************************
*! Example   : = lfALSAVAUT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 Save data to Bin Location files
FUNCTION lfALSAVAUT

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
IF !llUseBin 
  RETURN
ENDIF
DIMENSION laPikQty[9]
STORE 0 TO laPikQty
lnAlias    = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
SELECT WHBINLOC
lcOldOrder = ORDER()
SEt ORDER TO WHBINLST

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

IF SEEK(&lcOrdLine..Style+ &lcOrdLine..cWareCode ,'WHBINLOC') 
  IF SEEK(m.PikTkt+&lcOrdLine..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 
    FOR lnPkCount =1 To 8
      lcPkCount = ALLT(STR(lnPkCount,2))
      SELECT PKBINLOC
      =SEEK(m.PikTkt+&lcOrdLine..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style) 
      SCAN REST WHILE piktkt+cwarecode+clocation+style = m.PikTkt+&lcOrdLine..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style
        laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
      ENDSCAN  
    ENDFOR
    laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
  ENDIF  

  *! C123853,1 MHM 01/15/2005 initiate variable for Qty- ALO [Start]
  lnNetQty = 0
  *! C123853,1 MHM 01/15/2005 [End]

  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
 
    *! C123853,1 MHM 01/15/2005 Function to check classes [Start]
    IF !EMPTY(&lcOrdLine..Pik&lcCnt)
      =lfchkCLas(&lcOrdLine..Style,&lcOrdLine..cWareCode,lcCnt)
    ELSE
      LOOP  
    ENDIF  
    *IF WHBINLOC.QTY&lcCnt >= (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt])
    lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
    IF lnNetQty >= (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt])
    *! C123853,1 MHM 01/15/2005 [End]
    

      REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+ (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt]),;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo + (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt])
              
      =lfGetORQty((&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt]),lnCnt,m.PikTkt,&lcOrdLine..Order)
    ELSE

      *! C123853,1 MHM 01/15/2005 use new variable for Qty- ALO [Start]
      *REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
      *lnRem = (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt]) - WHBINLOC.QTY&lcCnt        
      *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,m.PikTkt,&lcOrdLine..Order)

      REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnNetQty ,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty 
      lnRem = (&lcOrdLine..Pik&lcCnt - laPikQty[lnCnt]) - lnNetQty 
      =lfGetORQty(lnNetQty ,lnCnt,m.PikTkt,&lcOrdLine..Order)
      *! C123853,1 MHM 01/15/2005 [End]

      SKIP
      SCAN REST WHILE STYLE+CWARECODE = &lcOrdLine..Style+ &lcOrdLine..cWareCode

        *! C123853,1 MHM 01/15/2005 Function to check classes [Start]
        =lfchkCLas(&lcOrdLine..Style,&lcOrdLine..cWareCode,lcCnt)
        *IF  WHBINLOC.QTY&lcCnt >= lnRem
        lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
        IF  lnNetQty >= lnRem
        *! C123853,1 MHM 01/15/2005  [End]

          REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnRem,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnRem
          =lfGetORQty(lnRem,lnCnt,m.PikTkt,&lcOrdLine..Order)
          EXIT        
        ELSE

          *! C123853,1 MHM 01/15/2005 use new variable for Qty- ALO [Start]
          *REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
          *        WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
          *lnRem = lnRem - WHBINLOC.QTY&lcCnt        
          *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,m.PikTkt,&lcOrdLine..Order)
          REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+lnNetQty ,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty 
          lnRem = lnRem - lnNetQty 
          =lfGetORQty(lnNetQty ,lnCnt,m.PikTkt,&lcOrdLine..Order)
          *! C123853,1 MHM 01/15/2005 [End]
          
        ENDIF          
      ENDSCAN 
      =SEEK(&lcOrdLine..Style+ &lcOrdLine..cWareCode ,'WHBINLOC')       
    ENDIF  
  ENDFOR
ENDIF

SELECT WHBINLOC
SET ORDER TO &lcOldOrder
SELECT (lnAlias)

*:**************************************************************************
*:* Name        : lfchkCLas
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 01/03/2005
*:* Module      : PO
*:* Purpose     : validate bin location in Po receiving screen
*:***************************************************************************
*C123853,1 MHM 02/06/2005 check for the same class[Start]

FUNCTION lfchkCLas
PARAMETER lcStyle,lcWareCode,lcSize

PRIVATE llCheckHF,llChckPrm ,llChckScn,llChckRem,lnCurrAlis,lnCrreC
STORE .F. TO llCheckHF,llChckPrm ,llChckScn,llChckRem
lnCurrAlis = SELECT(0)

lnCrreC = RECNO('WHBINLOC')
IF !USED('WHSLOC') 
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
ENDIF

=SEEK(lcStyle,'STYLE')


*--check for primary class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cPrimClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) >0
    llChckPrm = .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckPrm 
  SELECT (lnCurrAlis)
  RETURN llChckPrm 
ENDIF


*--check for Secondry class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cSecClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) > 0
    llChckScn= .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckScn
  SELECT (lnCurrAlis)
  RETURN llChckScn
ENDIF

*--check for Secondry class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cRemClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) > 0
    llChckRem= .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckRem
  SELECT (lnCurrAlis)
  RETURN llChckRem
ENDIF

SELECT (lnCurrAlis)
IF BETWEEN(lnCrreC ,1,RECCOUNT('WHBINLOC'))
  GOTO lnCrreC IN WHBINLOC
ENDIF

RETURN .F.
*:**************************************************************************
*:* Name        : lfChkAvl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/16/2005
*:* Purpose     : Check availabel inventory ready to allocate
*:***************************************************************************
*C123847,1
FUNCTION lfChkAvl
PRIVATE lnSlct    
lnSlct = SELECT()

PRIVATE lcUnPkStys,lnResp,lcMsg,lnI,lcI

STORE ' ' TO  lcUnPkStys, lcPiktkt1
    
lcOrdLine = IIF(TYPE('lcOrdLine')='C',lcOrdLine,'ORDLINE')
    
llEditToSo = .T.
SELECT &lcOrdLine
GO TOP
LOCATE FOR CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[1]
    
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[1] 

  IF (QTY1>PIK1 .AND. STYDYE.STK1-STYDYE.ALO1 > 0) .OR. ;
     (QTY2>PIK2 .AND. STYDYE.STK2-STYDYE.ALO2 > 0) .OR. ;
     (QTY3>PIK3 .AND. STYDYE.STK3-STYDYE.ALO3 > 0) .OR. ;
     (QTY4>PIK4 .AND. STYDYE.STK4-STYDYE.ALO4 > 0) .OR. ;
     (QTY5>PIK5 .AND. STYDYE.STK5-STYDYE.ALO5 > 0) .OR. ;
     (QTY6>PIK6 .AND. STYDYE.STK6-STYDYE.ALO6 > 0) .OR. ;
     (QTY7>PIK7 .AND. STYDYE.STK7-STYDYE.ALO7 > 0) .OR. ;
     (QTY8>PIK8 .AND. STYDYE.STK8-STYDYE.ALO8 > 0)
     
    lcUnPkStys = lcUnPkStys + &lcOrdLine..STYLE + ', '

  ENDIF
  
ENDSCAN

PRIVATE lcPkOrd
lcPkOrd = ORDER('PIKTKT')
SELECT PIKTKT
SET ORDER TO TAG ORDPIK

IF !EMPTY(lcUnPkStys)
  lcMsg = 'There is more stock available for style(s) &lcUnPkStys that has  '+; 
          'already picked on this order - Do you want to:'
  *- B44018 : \<Add to existing Picking Ticket     ::  1
  *-          \<Create a new Picking Ticket        ::  2
  lnResp = gfModalGen('INM00000B44018',.F.,.F.,.F.,lcMsg)
  
  *- check if there is actually existing piktkts
  IF lnResp = 1
    =SEEK(laData[1],'PIKTKT')
    LOCATE REST WHILE ORDER+PIKTKT = laData[1] FOR prtflag <> 'P'
    IF !FOUND()
      lnResp = 2
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No unprinted pick ticket exists. A new PikTkt will be created.') 
    ENDIF
  ENDIF

  *- Select an existing piktkt or create a new one 
  lcPiktkt1 = IIF( lnResp = 1 , lfPkBrw() , lfNPkOrdLn() )
  =lfPik(lcPiktkt1)
  
ENDIF    
llUpdDirALO = .F.

SELECT PIKTKT
SET ORDER TO TAG &lcPkOrd
SELECT (lnSlct)

RETURN '' 

*-- end of lfChkAvl.

*!*************************************************************
*! Name      : lfPkBrw
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/08/2005
*! Purpose   : Browse PikTkt #
*!*************************************************************
*C123847,1  
FUNCTION lfPkBrw
PRIVATE lcBrFields,lcAlias, lnAlias,laGtData,lcPiktk,lcOrdNo
lnAlias    = SELECT()

DECLARE laGtData[3] && array to get values from browse
laGtData     = ' '

*- variable to determine forcing browse or not
llBrowse   = IIF(TYPE('llBrowse')='U',.F.,llBrowse) 

lcTitle    = "Orders/PikTkt "
STORE '' TO lcPiktk,lcSelOrd
lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
             [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
             [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' , STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',IIF(STATUS = 'C','Complete', IIF(STATUS = 'K','Packed','Pulled')))) :R :H= 'Status  ']

SELECT PIKTKT
lcOldOrd = ORDER()
SET ORDER TO ORDPIK
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE

lcOrdNo = laData[1]
*- Loop to force the user to select one pick ticket
DO WHILE EMPTY(laGtData[1])
  =ARIABROW([lcOrdNo FOR prtflag <> 'P'],lcTitle,;
            gnBrFSRow1, gnBrFSCol1, gnBrHSRow1, gnBrHSCol1,'','','PikTkt,ORDER',"laGtData")
  IF EMPTY(laGtData[1])
    WAIT WINDOW NOWAIT 'You must select a Piktkt#'
  ENDIF
ENDDO            
lcPiktk  = laGtData[1]
lcSelOrd = laGtData[2]
laScrMode    = .F.
laScrMode[3] = .T.

*--Restore old data
SET RELATION OFF INTO ORDHDR 
SET ORDER TO &lcOldOrd

SELECT (lnAlias)
RETURN lcPiktk

*-- end of lfPkBrw.

*:**************************************************************************
*:* Name        : lfNPkOrdLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/16/2005
*:* Purpose     : && New picked ordline
*:***************************************************************************
*C123847,1
FUNCTION lfNPkOrdLn
PRIVATE lnSlct,lcNPkOrdLn,;
        lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,;
        lcPikTktNo

lnSlct = SELECT()

*- Create a new PikTkt no for the current order
SAVE TO (gcWorkDir+lcWinCh4+'.PIK') ALL LIKE lcWinCh4 && Just create a temp file to check its existance in lfGetPkTkt func.
lcPikTktNo = lfGetPkTkt(m.Order, laData[15], m.Store, m.cWareCode,2)
IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
  INSERT INTO PIKTKT;
    (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status) ;
    VALUES ;
    (lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')
  =gfAdd_Info('PIKTKT')
ENDIF  

RETURN lcPikTktNo 

*:**************************************************************************
*:* Name        : lfPik
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/16/2005
*:* Purpose     : Pick to a selected pick ticket or to a new one
*:***************************************************************************
FUNCTION lfPik
PARAMETERS lcPikTktNo
PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,;
        lnSlct
        
lnSlct = SELECT()

*- Create a temp cursor to hold lcordline records.
*- This since we will add new lines to the temp file lcOrdline , so to retain to original records to operate on
lcNPkOrdLn = gfTempName()
lnLastLine = &lcOrdHdr..LASTLINE
SELECT * FROM (gcWorkDir+lcOrdLine) INTO CURSOR &lcNPkOrdLn
SELECT &lcNPkOrdLn
SCAN
  =SEEK('O'+&lcNPkOrdLn..ORDER+STR(LINENO,6),lcOrdLine)
  SELECT &lcOrdLine
  SCATTER MEMVAR MEMO
  
  lnQty1 = IIF(QTY1>PIK1 .AND. STYDYE.STK1-STYDYE.ALO1 >0 , MIN(STYDYE.STK1-STYDYE.ALO1 , QTY1-PIK1) , 0 )
  lnQty2 = IIF(QTY2>PIK2 .AND. STYDYE.STK2-STYDYE.ALO2 >0 , MIN(STYDYE.STK2-STYDYE.ALO2 , QTY2-PIK2) , 0 )
  lnQty3 = IIF(QTY3>PIK3 .AND. STYDYE.STK3-STYDYE.ALO3 >0 , MIN(STYDYE.STK3-STYDYE.ALO3 , QTY3-PIK3) , 0 )
  lnQty4 = IIF(QTY4>PIK4 .AND. STYDYE.STK4-STYDYE.ALO4 >0 , MIN(STYDYE.STK4-STYDYE.ALO4 , QTY4-PIK4) , 0 )
  lnQty5 = IIF(QTY5>PIK5 .AND. STYDYE.STK5-STYDYE.ALO5 >0 , MIN(STYDYE.STK5-STYDYE.ALO5 , QTY5-PIK5) , 0 )
  lnQty6 = IIF(QTY6>PIK6 .AND. STYDYE.STK6-STYDYE.ALO6 >0 , MIN(STYDYE.STK6-STYDYE.ALO6 , QTY6-PIK6) , 0 )
  lnQty7 = IIF(QTY7>PIK7 .AND. STYDYE.STK7-STYDYE.ALO7 >0 , MIN(STYDYE.STK7-STYDYE.ALO7 , QTY7-PIK7) , 0 )
  lnQty8 = IIF(QTY8>PIK8 .AND. STYDYE.STK8-STYDYE.ALO8 >0 , MIN(STYDYE.STK8-STYDYE.ALO8 , QTY8-PIK8) , 0 )

  IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0
  
    IF EMPTY(&lcOrdLine..PikTkt) .OR. &lcOrdLine..PikTkt = lcPikTktNo

      m.PIK1 = m.PIK1 + lnQty1
      m.PIK2 = m.PIK2 + lnQty2
      m.PIK3 = m.PIK3 + lnQty3
      m.PIK4 = m.PIK4 + lnQty4
      m.PIK5 = m.PIK5 + lnQty5
      m.PIK6 = m.PIK6 + lnQty6
      m.PIK7 = m.PIK7 + lnQty7
      m.PIK8 = m.PIK8 + lnQty8
      m.TotPik = m.Pik1+m.Pik2+m.Pik3+m.Pik4+m.Pik5+m.Pik6+m.Pik7+m.Pik8
      
      m.Picked  = .T.
      m.PIKTKT  = lcPikTktNo
      m.PIKDATE = IIF(&lcOrdLine..PICKED,&lcOrdLine..PIKDATE,gdSysDate)

      SELECT &lcOrdLine
      GATHER MEMVAR MEMORY

    ELSE    
  
      m.Qty1 = lnQty1
      m.Qty2 = lnQty2
      m.Qty3 = lnQty3
      m.Qty4 = lnQty4
      m.Qty5 = lnQty5
      m.Qty6 = lnQty6
      m.Qty7 = lnQty7
      m.Qty8 = lnQty8
      M.TOTQTY = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
      
      *- Reduce current line with the qty's that will be transferred to a new line
      REPLACE QTY1    WITH QTY1-m.Qty1 ;
              QTY2    WITH QTY2-m.Qty2 ;
              QTY3    WITH QTY3-m.Qty3 ;
              QTY4    WITH QTY4-m.Qty4 ;
              QTY5    WITH QTY5-m.Qty5 ;
              QTY6    WITH QTY6-m.Qty6 ;
              QTY7    WITH QTY7-m.Qty7 ;
              QTY8    WITH QTY8-m.Qty8 ;
              TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
              BOOK1   WITH QTY1 ;
              BOOK2   WITH QTY2 ;
              BOOK3   WITH QTY3 ;
              BOOK4   WITH QTY4 ;
              BOOK5   WITH QTY5 ;
              BOOK6   WITH QTY6 ;
              BOOK7   WITH QTY7 ;
              BOOK8   WITH QTY8 ;
              TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 
              
      STORE m.QTY1 TO m.BOOK1
      STORE m.QTY2 TO m.BOOK2
      STORE m.QTY3 TO m.BOOK3
      STORE m.QTY4 TO m.BOOK4
      STORE m.QTY5 TO m.BOOK5
      STORE m.QTY6 TO m.BOOK6
      STORE m.QTY7 TO m.BOOK7
      STORE m.QTY8 TO m.BOOK8
      m.TotQty = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
      STORE m.TotQty TO m.TOTBOOK
      
      =SEEK(&lcOrdLine..STYLE+&lcOrdLine..CWARECODE+SPACE(10),'STYDYE')
      m.PIK1 = MIN( STYDYE.STK1-STYDYE.ALO1 , m.Qty1 )
      m.PIK2 = MIN( STYDYE.STK2-STYDYE.ALO2 , m.Qty2 )
      m.PIK3 = MIN( STYDYE.STK3-STYDYE.ALO3 , m.Qty3 )
      m.PIK4 = MIN( STYDYE.STK4-STYDYE.ALO4 , m.Qty4 )
      m.PIK5 = MIN( STYDYE.STK5-STYDYE.ALO5 , m.Qty5 )
      m.PIK6 = MIN( STYDYE.STK6-STYDYE.ALO6 , m.Qty6 )
      m.PIK7 = MIN( STYDYE.STK7-STYDYE.ALO7 , m.Qty7 )
      m.PIK8 = MIN( STYDYE.STK8-STYDYE.ALO8 , m.Qty8 )
      m.TotPik = m.Pik1+m.Pik2+m.Pik3+m.Pik4+m.Pik5+m.Pik6+m.Pik7+m.Pik8
          
      *- If the same style is added befor to the same piktkt then add it again and update the quantities
      SELECT &lcOrdLine
      LOCATE FOR PIKTKT+STYLE = lcPikTktNo+&lcNPkOrdLn..STYLE
      IF !FOUND() 
  
        *- Add a new line to lcordline with the rest unallocated qty to allocate with a new piktkt
        lnLastLine = lnLastLine + 1    
        M.LINENO  = lnLastLine
        m.Picked  = .T.
        m.PIKTKT  = lcPikTktNo
        m.PIKDATE = gdSysDate
        INSERT INTO &lcOrdLine FROM MEMVAR

      ELSE

        REPLACE QTY1    WITH QTY1+m.Qty1 ;
                QTY2    WITH QTY2+m.Qty2 ;
                QTY3    WITH QTY3+m.Qty3 ;
                QTY4    WITH QTY4+m.Qty4 ;
                QTY5    WITH QTY5+m.Qty5 ;
                QTY6    WITH QTY6+m.Qty6 ;
                QTY7    WITH QTY7+m.Qty7 ;
                QTY8    WITH QTY8+m.Qty8 ;
                TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                BOOK1   WITH QTY1 ;
                BOOK2   WITH QTY2 ;
                BOOK3   WITH QTY3 ;
                BOOK4   WITH QTY4 ;
                BOOK5   WITH QTY5 ;
                BOOK6   WITH QTY6 ;
                BOOK7   WITH QTY7 ;
                BOOK8   WITH QTY8 ;
                TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 ;
                PIK1    WITH PIK1 + m.Pik1 ;
                PIK2    WITH PIK2 + m.Pik2 ;
                PIK3    WITH PIK3 + m.Pik3 ;
                PIK4    WITH PIK4 + m.Pik4 ;
                PIK5    WITH PIK5 + m.Pik5 ;
                PIK6    WITH PIK6 + m.Pik6 ;
                PIK7    WITH PIK7 + m.Pik7 ;
                PIK8    WITH PIK8 + m.Pik8 ;
                TOTPIK  WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8
      ENDIF

    ENDIF

  ENDIF
  
  *- Update stock and allocation data
  IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0 
    SELECT STYDYE
    REPLACE ALO1 WITH ALO1 + lnQty1 ;
            ALO2 WITH ALO2 + lnQty2 ;
            ALO3 WITH ALO3 + lnQty3 ;
            ALO4 WITH ALO4 + lnQty4 ;
            ALO5 WITH ALO5 + lnQty5 ;
            ALO6 WITH ALO6 + lnQty6 ;
            ALO7 WITH ALO7 + lnQty7 ;
            ALO8 WITH ALO8 + lnQty8 ;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
    
    =SEEK(&lcOrdLine..STYLE,'STYLE')
    SELECT STYLE
    REPLACE ALO1 WITH ALO1 + lnQty1 ;
            ALO2 WITH ALO2 + lnQty2 ;
            ALO3 WITH ALO3 + lnQty3 ;
            ALO4 WITH ALO4 + lnQty4 ;
            ALO5 WITH ALO5 + lnQty5 ;
            ALO6 WITH ALO6 + lnQty6 ;
            ALO7 WITH ALO7 + lnQty7 ;
            ALO8 WITH ALO8 + lnQty8 ;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  ENDIF

ENDSCAN
USE IN &lcNPkOrdLn

SELECT &lcOrdHdr
REPLACE LASTLINE WITH lnLastLine

SELECT (lnSlct)
*-- end of lfNPkOrdLn.

*:**************************************************************************
*:* Name        : lfUpdPk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/22/2005
*:* Purpose     : Update pik qty's for DIR03 to pik only lines to pick
*:***************************************************************************
FUNCTION lfUpdPk
PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

lnQty1 = IIF(QTY1>PIK1 .AND. STYDYE.STK1-STYDYE.ALO1 >0 , MIN(STYDYE.STK1-STYDYE.ALO1 , QTY1-PIK1) , 0 )
lnQty2 = IIF(QTY2>PIK2 .AND. STYDYE.STK2-STYDYE.ALO2 >0 , MIN(STYDYE.STK2-STYDYE.ALO2 , QTY2-PIK2) , 0 )
lnQty3 = IIF(QTY3>PIK3 .AND. STYDYE.STK3-STYDYE.ALO3 >0 , MIN(STYDYE.STK3-STYDYE.ALO3 , QTY3-PIK3) , 0 )
lnQty4 = IIF(QTY4>PIK4 .AND. STYDYE.STK4-STYDYE.ALO4 >0 , MIN(STYDYE.STK4-STYDYE.ALO4 , QTY4-PIK4) , 0 )
lnQty5 = IIF(QTY5>PIK5 .AND. STYDYE.STK5-STYDYE.ALO5 >0 , MIN(STYDYE.STK5-STYDYE.ALO5 , QTY5-PIK5) , 0 )
lnQty6 = IIF(QTY6>PIK6 .AND. STYDYE.STK6-STYDYE.ALO6 >0 , MIN(STYDYE.STK6-STYDYE.ALO6 , QTY6-PIK6) , 0 )
lnQty7 = IIF(QTY7>PIK7 .AND. STYDYE.STK7-STYDYE.ALO7 >0 , MIN(STYDYE.STK7-STYDYE.ALO7 , QTY7-PIK7) , 0 )
lnQty8 = IIF(QTY8>PIK8 .AND. STYDYE.STK8-STYDYE.ALO8 >0 , MIN(STYDYE.STK8-STYDYE.ALO8 , QTY8-PIK8) , 0 )

IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0
  REPLACE PIK1 WITH lnQty1 ;
          PIK2 WITH lnQty2 ;
          PIK3 WITH lnQty3 ;
          PIK4 WITH lnQty4 ;
          PIK5 WITH lnQty5 ;
          PIK6 WITH lnQty6 ;
          PIK7 WITH lnQty7 ;
          PIK8 WITH lnQty8 ;
          TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8
ENDIF  

*-- end of lfUpdPk.
