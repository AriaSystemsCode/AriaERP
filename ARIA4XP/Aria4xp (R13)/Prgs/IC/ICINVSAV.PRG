*! Modification
*! N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*! B608073,1 MMT 05/07/2007 fix bug of wrong Cost Value
*T20071102.0018(C200876) TMI 12/30/2007 Save data to Bins when "Use bin locations" is YES
*B610438,1   TMI 07/14/2013 fix bug of  WRONG COST ON STOCK ADJUSTMENT JOURNAL [T20130708.0014] 
*! E303953,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL  
*! E303975,1 SAH 05/17/2018 CONVERT STYINVJL TO SQL 
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011]
*! E303953,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL  
*! E611842,1 MMT 12/18/2018 Call new global function to update new GL tables [GL Enhancement]
*********************************************************************************************************
FUNCTION lfSave
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][Start]
*PARAMETERS llFromEDI
PARAMETERS llFromEDI,llFromBatch
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][End]
*-- Include the .H file for Multi Lingual purpuse.
#INCLUDE R:\ARIA4XP\PRGS\SY\ICINVSAV.H

*N000682,1 11/20/2012 MMT Globlization changes[Start]
lcAriaHFile = ''
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcAriaHFile =oAriaApplication.GetClassHeaderFile(ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\SY\" + ALLTRIM("ICINVSAV")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

*--Read locations.
SELECT (lcTmpAdj)
GO TOP
lcFromWare = cFromWare
lcToWare   = cToWare

*-- Open a temp file to be used in calling gl distributer proc.
lcWorkDir=oAriaApplication.Workdir
IF llGlLink
  *! E303953,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start] 
  *=gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')  
  =gfOpenTable(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
  *! E303953,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO (lcWorkDir+lcTmpGlDt)
  USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF

IF !USED('StyInvJl')

 *E303975,1 SAH 05/17/2018 CONVERT STYINVJL TO SQL [begin]
* =gfOpenFile(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
  =gfOpenTable(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
  *E303975,1 SAH 05/17/2018 CONVERT STYINVJL TO SQL [end]
ENDIF

*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][Start]
lcGLSessionBatch =''
IF llFromBatch
  lcGLSessionBatch =gfSequence('GLSESSION')
ENDIF
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][End]

*--Save to master files.
DIMENSION laToSave[8],laTemp[8]
SELECT (lcTmpAdj)
SCAN FOR !DELETED()

  *--Check if nothing to adjust or transfer (all Zero).
  IF lcType $ 'AT'
    SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8 TO laToSave
    llZeroAdj = .T.
    FOR lnI = 1 To 8
      llZeroAdj = IIF(laToSave[lnI]=0, .T., .F.)
      IF !llZeroAdj
        EXIT
      ENDIF
    ENDFOR
    IF llZeroAdj
      DELETE
      LOOP
    ENDIF
  ENDIF

  = SEEK( Style,'STYLE')
  = SEEK( Style + cFromWare + SPACE(10),'STYDYE')
  llDyeLvl = ( llDyelot AND STYLE.cDye_Flg = 'Y' )
  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
  IF llDyeLvl AND llGoOn
    =SEEK(Style+cFromWare+Dyelot,'STYDYE')
    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE'))
  ENDIF

  SELECT (lcTmpAdj)
  IF !llGoOn
    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('TRM42067B42001','DIALOG',+'|'+IIF(llDyelvl,'/'+LANG_Dyelot,'')+'|'+ALLTRIM(STYLE)+'|'+IIF(llDyelvl,'/'+Dyelot+'',''))
    =gfModalGen('TRM42067B42001','DIALOG',+'|'+IIF(llDyelvl,'/'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Dyelot,oAriaApplication.GetHeaderText("LANG_Dyelot",lcAriaHFile)),'')+'|'+ALLTRIM(STYLE)+'|'+IIF(llDyelvl,'/'+Dyelot+'',''))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    DELETE
    DO lpUnLock
    LOOP
  ENDIF

  *-- Store Style old cost, Old stock, And link code.
  SELECT Style
  lnOldStk   = TotStk
  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  SELECT (lcTmpAdj)


  *--Saving the old quantity in the master file
  *--and the checking of the FROM stock.
  IF lcType = 'T'
    *-- Store the stock in the TO warehouse before updating the master
    *-- file.
    = SEEK( Style + cToWare +IIF(llDyeLvl, Dyelot, ''),'STYDYE')
    SELECT StyDye
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
    lnTotStk = TotStk
    *--If transfer save the link code for to warehouse.
    lcToLink = IIF(llGlLink ,IIF(!EMPTY(Gl_Link),Gl_Link,lcLinkCode),"")

    SELECT (lcTmpAdj)
    GATHER FROM laToSave FIELDS nOldTo1,nOldTo2,nOldTo3,nOldTo4,nOldTo5,nOldTo6,nOldTo7,nOldTo8
    REPLACE nTotOldTo WITH lnTotStk

    = SEEK( Style + cFromWare +IIF(llDyeLvl, Dyelot, ''),'STYDYE')
    *-- We should be sure that the stock in the FROM warehouse if enougth
    *-- to be transfered to the TO warehouse.
    SELECT StyDye
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
    lnTotStk = TotStk

    llZeroStk = .F.
    FOR lnI = 1 TO 8
      llZeroStk = IIF(laToSave[lnI] = 0, .T., .F.)
      IF !llZeroStk
        EXIT
      ENDIF
    ENDFOR

    SELECT (lcTmpAdj)
    IF llZeroStk
      *-Style/xDx : xxx/xxx is out of stock in warehouse: xxx, Cannot transfer.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *=gfModalGen('TRM42068B42001','DIALOG',IIF(llDyelvl,'/'+LANG_Dyelot+': ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare))
      =gfModalGen('TRM42068B42001','DIALOG',IIF(llDyelvl,'/'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Dyelot,oAriaApplication.GetHeaderText("LANG_Dyelot",lcAriaHFile))+': ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      DELETE REST WHILE Style+cFromWare+Dyelot = STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot
      DO lpUnLock
      LOOP
    ENDIF

    *-- Check the stock per size.
    llVldTran = .T.
    FOR lnI = 1 TO 8
      lcAdjNum = 'Adj' + STR(lnI, 1)
      llVldTran = IIF(laToSave[lnI] >= &lcAdjNum OR &lcAdjNum = 0, .T., .F.)
      IF !llVldTran
        EXIT
      ENDIF
    ENDFOR

    *-- If there is any size that does not have enougth stock to
    *-- be transfered then please tell this to the user and ask him
    *-- if he wants to transfer all the stock that exists in the FROM
    *-- warehouse to the TO warehouse.
    IF !llVldTran
      *--The stock level has changed for style/xdx:xxx/xxx in warehouse: xxx Transfer what is available ? \<Yes;\<No.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *IF gfModalGen('TRM42069B42002','DIALOG',IIF(llDyelvl,'/'+LANG_Dyelot+': ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare)) = 1
      IF gfModalGen('TRM42069B42002','DIALOG',IIF(llDyelvl,'/'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Dyelot,oAriaApplication.GetHeaderText("LANG_Dyelot",lcAriaHFile))+': ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare)) = 1
      *N000682,1 11/20/2012 MMT Globlization changes[End]

        *-- If the user wants to transfer the stock, all we have to
        *-- do is to save the FROM warehouse stock in the temp. file
        *-- as the adjustments quantity.
        GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8
        REPLACE TotAdj WITH lnTotStk
      ELSE
        DELETE
        DO lpUnLock
        LOOP
      ENDIF
    ENDIF
  ENDIF  && End if Transfer.

  lcWorkAls = IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')


  SELECT (lcTmpAdj)
  lcCurKey = IIF(!llMultiWH,STyle,Style+cFromWare)
  *--When making Physical inventory and wrong Stock and the user selects to Rebalance.
  *T20060818.0001(C200876) TMI [Start] if Bin Location is used , and ReBalance is issued, then make summation over the selected bin
  *                           and then update the lcWorkAls file from the bininvjl summation
  IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("\\\\\\\",10)) <> 0
    =loIcinva.mDoTrigger(PADR('\\\\\\\',10))
  ENDIF
  *T20060818.0001(C200876) TMI [End  ]
  
  
    *E303975,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*!*	    IF lcType = 'P' AND llReBalance AND SEEK(lcCurKey,'StyInvJl')
    IF lcType = 'P' AND llReBalance AND GFSEEK(lcCurKey,'StyInvJl')
    *E303975,1 SAH CONVERT STYINVJL TO SQL [END]
    
    
    lcDyeLot = DyeLot
    DIMENSION laStyStk[10]
    STORE 0 TO laStyStk
    SELECT StyInvJl
    SUM REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcCurKey FOR cDyeLot = lcDyeLot ;
                   nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8,nTotStk,nStkVal ;
        TO laStyStk[1],laStyStk[2],laStyStk[3],laStyStk[4],laStyStk[5],laStyStk[6],laStyStk[7],laStyStk[8],laStyStk[9],laStyStk[10]

    IF SEEK(lcCurKey+IIF(llDyeLvl,lcDyeLot,''),lcWorkAls) AND ;
        (&lcWorkAls..Stk1<>laStyStk[1] ;
        OR &lcWorkAls..Stk2<>laStyStk[2] ;
        OR &lcWorkAls..Stk3<>laStyStk[3] ;
        OR &lcWorkAls..Stk4<>laStyStk[4] ;
        OR &lcWorkAls..Stk5<>laStyStk[5] ;
        OR &lcWorkAls..Stk6<>laStyStk[6] ;
        OR &lcWorkAls..Stk7<>laStyStk[7] ;
        OR &lcWorkAls..Stk8<>laStyStk[8] ;
        OR &lcWorkAls..TotStk<>laStyStk[9];
        OR &lcWorkAls..nStkVal<>laStyStk[10])
      *--Do Rebalance.
      SELECT (lcWorkAls)
      GATHER FROM laStyStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,nStkVal
      *---------------
    ENDIF
  ENDIF
  *--

  SELECT (lcWorkAls)
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4,OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld WITH lnTotStk



  *--'Start updating'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
   *WAIT WINDOW LANG_Updating+' => '+ALLTRIM(Style)+ IIF(llDyelvl,'/'+&lcTmpAdj->Dyelot,'') NOWAIT
   WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating,oAriaApplication.GetHeaderText("LANG_Updating",lcAriaHFile))+' => '+ALLTRIM(Style)+ IIF(llDyelvl,'/'+&lcTmpAdj->Dyelot,'') NOWAIT
   *N000682,1 11/20/2012 MMT Globlization changes[End]



  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")

  *--Start Updating.
  IF !lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF

  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession
  *B610438,1 TMI 07/14/2013 [Start] - Moidyf the function lfSave in ICINVSAV.PRG to update UNT_COST field in lcTmpAdj with styinvjl.ncost field in case of style costing methods FIFO and LIFO
  IF lcCostMth $ 'FL' 
    replace UNT_COST with styinvjl.ncost 
  ENDIF 
  *B610438,1 TMI 07/14/2013 [End  ] 
  =gfAdd_Info(lcTmpAdj)



  *--Update Master file for locations.
  IF llWareLoc
    SELECT (lcTemLoc)
    SCAN FOR !EMPTY(cLocation) AND !DELETED()
      IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
        SCATTER MEMVAR
        INSERT INTO WhsLoc FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF

  *--Update master Inventory adjustment table.
  SELECT (lcTmpAdj)
  SCATTER MEMVAR MEMO
  INSERT INTO InvtAdj FROM MEMVAR
  *---

  SELECT (lcTmpAdj)
  DO lpUnLock
ENDSCAN
WAIT CLEAR

*--Send product Activity.
IF 'NC' $ oAriaApplication.CompanyInstalledModules AND !llFromEDI
  =lfNCUpdate()
ENDIF

*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][Start]
SELECT (lcTmpAdj)
LOCATE 
IF lcType = 'A' AND llFromBatch AND 'AS' $ oAriaApplication.CompanyInstalledModules
  lfUpdateEDITran(&lcTmpAdj..cSession,&lcTmpAdj..cfromware)
ENDIF
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][End]

*--Clear temp file after saving.
SELECT (lcTmpAdj)

*C131527,1 WSH 07/10/2006 Don't clear here, it will be zapped in the Screen Saving method. [Start]
*ZAP
*C131527,1 WSH 07/10/2006 [End]

*-- Update distripution master file
IF llGlLink
  *!T20060818.0001(C200876),1 TMI Custom Update Gl Bin Location For David Luke[Start]
  IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLUPDTGL",10)) <> 0 .AND. ;
               loIcinva.mDoTrigger(PADR('DLUPDTGL',10))
    *- nothing to do, just skip updating the GL in case of transfer from bins within the same Warehous
  ELSE
  *!T20060818.0001(C200876),1 [End]

    SELECT (lcTmpGlDt)
    *-- Generate a unique session number.
    lcGlSess = gfSequence('GLSESSION')
    
    *! E303953,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
    *REPLACE ALL GLSESSION WITH lcGlSess
	lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')    
    SCAN 
      REPLACE GLSESSION WITH lcGlSess
      =gfAdd_Info(lcTmpGlDt)
      SCATTER MEMVAR memo 
      SELECT GLDIST
      APPEND BLANK
	  GATHER MEMVAR MEMO 
      IF !EMPTY(lcGLDISTCursorUpdate)
        SELECT (lcGLDISTCursorUpdate)
        APPEND BLANK
        GATHER MEMVAR MEMO 
      ENDIF      
    ENDSCAN
    *! E303953,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
    
    *! E303953,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
    *USE
    *SELECT GLDIST
    *APPEND FROM (lcWorkDir+lcTmpGlDt)
    *! E303953,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	    SELECT (lcTmpGlDt)
*!*	    SCAN 
*!*	      SCATTER MEMVAR memo 
*!*	      m.Oid = gfTempName()
*!*	      SELECT GLDIST
*!*	      APPEND BLANK
*!*		  GATHER MEMVAR MEMO 
*!*		  gfReplace('')
*!*	    ENDSCAN  
    *! E303953,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
    SELECT GLDIST
    =gfTableUpdate()
    *! E611842,1 MMT 12/18/2018 Call new global function to update new GL tables [GL Enhancement][Start]
    =gfCreateGLEntries(lcTmpGlDt,"")
    *! E611842,1 MMT 12/18/2018 Call new global function to update new GL tables [GL Enhancement][End]
    USE IN (lcTmpGlDt)
    *! E303953,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL[End] 
    ERASE (lcWorkDir+lcTmpGlDt+'.DBF')

  *!T20060818.0001(C200876),1 Custom Update Gl Bin Location For David Luke[Start]
  ENDIF
  *!T20060818.0001(C200876),1 [End]

ENDIF

*!T20060818.0001(C200876),1 TMI [START] Save Bin location data
IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLSAVDAT",10)) <> 0
  =loIcinva.mDoTrigger(PADR('DLSAVDAT',10))
ENDIF
*!T20060818.0001(C200876),1 TMI [END  ]

WAIT CLEAR

*!*************************************************************
*! Name      : lfRecLock
*! Purpose   : Record Lock.
*!*************************************************************
*! Parameters: lcFile->Locked file.
*!*************************************************************
FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF
  ELSE
    lnRet = .F.
    EXIT
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)

*!*************************************************************
*! Name      : lpUnLock
*! Purpose   : Unlock the saved locked records.
*!*************************************************************
PROCEDURE lpUnLock
*IF !llDyeLvl
UNLOCK ALL
*ENDIF

*!*************************************************************
*! Name      : lfInvUpdt()
*! Purpose   : Update inventory.
*!*************************************************************
FUNCTION lfInvUpdt

SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '
IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

SELECT (lcTmpAdj)
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE IIF(lcType = 'P','IP','IA') TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE dPostDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


*--Adjustment quantity array TRANSFER Case.
IF lcType = 'T'
  DECLARE laAdjust[9]
  FOR I = 1 TO 8
    Z=STR(I,1)
    laAdjust[I] = -Adj&Z
  ENDFOR
  laAdjust[9] = -TotAdj

  *--Call the global function for update style inventory control.
  PRIVATE lcRefer
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcRefer = LANG_To_Loc+" "+lcToWare
  lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_To_Loc,oAriaApplication.GetHeaderText("LANG_To_Loc",lcAriaHFile))+" "+lcToWare
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lcAdjRef = &lcTmpAdj..cRefer
  lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *--Read an issue seesion no to use it in receive if LIFO or FIFO.
  lcIsuSessNo = STYINVJL.cSession

  *C131527,1 WSH 07/13/2006 Replace the Issue Session for Transfer. [Start]
  REPLACE cIssSess WITH lcIsuSessNo IN (lcTmpAdj)
  *C131527,1 WSH 07/13/2006 [End]

  *--Return with no save if function return fulse.
  IF lnRet = 0
    RETURN .F.
  ENDIF

  *! T20060818.0001(C200876),1 handle case to update BinInvJl [Start]
  IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLSVBNI",10)) <> 0
    =loIcinva.mDoTrigger(PADR('DLSVBNI',10))
  ENDIF
  *! T20060818.0001(C200876),1 [End]

  *--Next Adjustment will use the link code for TO Warehouse.
  IF llGlLink
    laGLDistAr[1,1] = lcToLink
    laGLDistAr[2,1] = lcToLink
  ENDIF
ENDIF

*--Warehouse Code.
lcAdjWareH = IIF(lcType = 'T',&lcTmpAdj..cToWare,&lcTmpAdj..cFromWare )


*--If transfer and costing method LIFO or FIFO receive by cost of issue.
IF lcType = 'T' AND lcCostMth $ 'FL'
  SELECT STYINVJL
  
  *E303975,1 SAH 05/17/2018 CONVERT STYINVJL TO SQL [begin]
  *SEEK Style+&lcTmpAdj..cFromWare+lcIsuSessNo
  gfSEEK(&lcTmpAdj..Style+&lcTmpAdj..cFromWare+lcIsuSessNo)
  *E303975,1 SAH 05/17/2018 CONVERT STYINVJL TO SQL [end]
  *E303975,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [begin] 
  *SCAN WHILE Style+cWareCode+cSession = Style+&lcTmpAdj..cFromWare+lcIsuSessNo
  SCAN WHILE Style+cWareCode+cSession = &lcTmpAdj..Style+&lcTmpAdj..cFromWare+lcIsuSessNo
  *E303975,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
    lnSavRcNo = RECNO()
    *--Adjustment quantity array.
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = ABS(nStk&Z)
    ENDFOR
    laAdjust[9] = ABS(nTotStk)

    *--Call the global function for update style inventory control.
    PRIVATE lcRefer
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRefer = LANG_From_Loc+" "+lcFromWare
    lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_From_Loc,oAriaApplication.GetHeaderText("LANG_From_Loc",lcAriaHFile))+" "+lcFromWare
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    lcAdjRef = &lcTmpAdj..cRefer
    lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)

    *! T20060818.0001(C200876),1 TMI 01/15/2005 handle case of updating BININVJL [Start]
    IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLSVBNR",10)) <> 0
      lcRcvSessNo = STYINVJL.cSession
      =loIcinva.mDoTrigger(PADR("DLSVBNR",10))
    ENDIF
    *! T20060818.0001(C200876),1 01/15/2005  [End]

    SELECT STYINVJL
    GOTO lnSavRcNo
  ENDSCAN

ELSE
  *--Adjustment quantity array.
  DECLARE laAdjust[9]
  SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust

  *--Adjustment cost.
  **B608073,1 MMT 05/07/2007 fix bug of wrong Cost Value[Start]
  *lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
  lnACost = IIF(TotAdj>=0,Unt_Cost,Old_Cost)
  **B608073,1 MMT 05/07/2007 fix bug of wrong Cost Value[End]

  *--Type of the adjustment.
  lcAdjTyp = IIF(lcType = 'P','2','1')
  *--Call the global function for update style inventory control.
  PRIVATE lcRefer
  IF lcType = 'T'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRefer = LANG_From_Loc+" "+lcFromWare
    lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_From_Loc,oAriaApplication.GetHeaderText("LANG_From_Loc",lcAriaHFile))+" "+lcFromWare
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)
  ENDIF
  *T20060818.0001(C200876) TMI [Start]
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",cRefer)

  lcAdjRef = &lcTmpAdj..cRefer
  IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLSTYCRL",10)) <> 0 .AND. ;
     gfDoTriger(ALLTRIM(SUBSTR(loIcinva.Name,4)),PADR('ISUSEBIN',10))
    lnRet = 0
    =loIcinva.mDoTrigger(PADR('DLSTYCRL',10))
  ELSE
    *! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][Start]
    *lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
    lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,IIF(llFromBatch,lcGLSessionBatch,.T.),cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)                   
    *! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][End]
  ENDIF

  IF TYPE('loIcinva')='O' .AND. ASCAN(loIcinva.laEvntTrig,PADR("DLSVBNI",10)) <> 0
    lcIsuSessNo = STYINVJL.cSession
    =gfDoTriger(ALLTRIM(SUBSTR(loIcinva.Name,4)),PADR('DLSVBNI',10))
  ENDIF
  *! T20060818.0001(C200876),1 TMI 01/15/2005  [End]
ENDIF

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF

RETURN

*!*************************************************************
*! Name      : lfNCUpdate()
*! Purpose   : Update inventory for NC Edi module.
*!*************************************************************
FUNCTION lfNCUpdate

=gfOpenFile(oAriaApplication.DataDir+'CODES','CODES','SH')
SELECT CODES
SET ORDER TO TAG Idrltfname
=SEEK('NYCSITEID')
LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
lcBackSite = Codes.cCode_No
=SEEK(lcFromWare,'WareHous')
lcFromSite = WareHous.cSiteId
=SEEK(lcToWare,'WareHous')
lcToSite = WareHous.cSiteId

STORE '' TO lcSendTo1,lcSendTo2,lcSendTo3
IF oAriaApplication.CurrentSite = lcBackSite
  IF lcFromSite <> lcBackSite
    lcSendTo1 = lcFromSite
  ENDIF
  IF lcToSite <> lcBackSite
    lcSendTo2 = lcToSite
  ENDIF
ELSE
  lcSendTo1 = lcBackSite
  IF !INLIST(lcFromSite,lcBackSite,oAriaApplication.CurrentSite)
    lcSendTo2 = lcFromSite
  ENDIF
  IF !INLIST(lcToSite,lcBackSite,oAriaApplication.CurrentSite)
    lcSendTo3 = lcToSite
  ENDIF
ENDIF
IF !EMPTY(lcSendTo1) OR !EMPTY(lcSendTo2) OR !EMPTY(lcSendTo3)
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT','ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD','PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS','TYPEKEY','SH')

  SELECT Date FROM (lcTmpAdj) GROUP BY Date INTO CURSOR Dates
  SELECT Dates
  SCAN
      lcDateKey = DTOS(Date)+'-'+DTOS(Date)
      IF !EMPTY(lcSendTo1)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo1
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                     ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
      IF !EMPTY(lcSendTo2)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo2
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                     ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
      IF !EMPTY(lcSendTo3)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo3
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                     ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
  ENDSCAN
  USE IN Dates
ENDIF
RETURN

*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][Start]
*!*************************************************************
*! Name      : lfUpdateEDITran
*! Developer : Mariam Mazhar
*! Date      : 08/15/2018 
*! Purpose   : Update EDITRANS in case of receiving by Batch
*!*************************************************************
FUNCTION lfUpdateEDITran
LPARAMETERS lcSession,lcLocation
IF !USED('EDIACPRT')
  =gfOpenTable(oAriaApplication.DataDir+'EDIACPRT','ACCFACT','SH')
ENDIF  
IF !USED('EDIPD')
  =gfOpenTable(oAriaApplication.DataDir+'EDIPD','PARTTRANS','SH')
ENDIF  
IF !USED('EDITRANS')
  =gfOpenTable(oAriaApplication.DataDir+'EDITRANS','TYPEKEY','SH')
ENDIF
IF Seek(lcLocation,'WareHous','WareHous') And  gfSeek('W'+WareHous.cThrdPLPr,'EDIACPRT','ACCFACT') And ;
   gfSEEK(EDIACPRT.cPartCode+'944','EDIPD','PARTTRANS')
  Select EDITRANS
  If !gfSeek('944'+Padr(lcSession,40)+'W'+WareHous.cThrdPLPr,'EDITRANS','TYPEKEY')
    Insert Into 'EDITRANS' (CEDITRNTYP,Key,Type,CPARTNER,lInterComp) Values ;
      ('944',lcSession,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
    =gfReplace('')  
  Endif
  Replace cStatus With 'N'
  =gfAdd_Info('EDITRANS')
  =gfReplace('')    
  =gfTableUpdate() 
ENDIF
*! E304059,1 MMT 08/15/2018 Modify Inventory adjustment screen to import batch[P20171120.0011][End]