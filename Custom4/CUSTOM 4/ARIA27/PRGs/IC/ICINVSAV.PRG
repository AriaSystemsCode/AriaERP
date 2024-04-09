*********************************************************************************************************
*! Modifications :
*!B804152,1 ADEL 08/01/02   Prompt the user to Rebalance stock when wrong one before Physical Adjusment.       
*!B606489,3 TMI  10/16/2002 Reset the relation of temp file lcTmpAdj to style file when reopen it exclosively
*!B607350,1 KHM  06/29/2003 Fix the bug of end of file encounterd when you make physical 
*!B607350,1                 inventory to more than one style and the first one has wrong stock 
*!B607350,1                 and need to be rebalanced
*!E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*!C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*!C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*********************************************************************************************************
FUNCTION lfSave
PARAMETERS llFromEDI


*-- Open a temp file to be used in calling gl distributer proc.
IF llGlLink
  *E301077,12 open only if not opened.
  IF !laOpFile[2]
    laOpFile[2]=gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
  ENDIF
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  *B602457 MAB The following line was converted to normally open [Begin
  *B602457 because we does not open temp. files using gfopenfile.
  *=gfOpenFile(gcWorkDir+lcTmpGlDt,' ','EX')
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
  *B602457 MAB The following line was converted to normally open [End..
ENDIF  

*E301077,12 open styinvjl file needed for saving.
*E301077,12 open only if not opened.
IF !laOpFile[3]
  laOpFile[3]=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
ENDIF

*--Save to master files.
DIMENSION laToSave[8],laTemp[8]
SELECT (lcTmpAdj)
SCAN

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
  IF llDyeLvl
    SET MULTILOCKS ON
  ENDIF 
  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
  IF llDyeLvl AND llGoOn
    =SEEK(Style+cFromWare+Dyelot,'STYDYE')
    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE'))
  ENDIF  

  SELECT (lcTmpAdj)
  IF !llGoOn
    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
    =gfModalGen('TRM42067B42001','DIALOG',+'|'+IIF(llDyelvl,'/Dyelot','')+'|'+ALLTRIM(STYLE)+'|'+IIF(llDyelvl,'/'+Dyelot+'',''))
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
      =gfModalGen('TRM42068B42001','DIALOG',IIF(llDyelvl,'/Dyelot: ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare))
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
    *-- be transfered then please till this to the user and ask him
    *-- if he wants to transfer all the stock that exists in the FROM
    *-- warehouse to the TO warehouse.
    IF !llVldTran
      *--The stock level has changed for style/xdx:xxx/xxx in warehouse: xxx Transfer what is available ? \<Yes;\<No.
      IF gfModalGen('TRM42069B42002','DIALOG',IIF(llDyelvl,'/Dyelot: ',': ')+'|'+ALLTRIM(Style)+'|'+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare)) = 1
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
  *B804152,1 (Begin) When making Physical Adjustment and wrong Stock and the user selects to Rebalance.
  IF lcType = 'P' AND llReBalance
    
    *B607350,1 KHM 06/29/2003 (Begin) Saving the record pointer because it ic changed in the
    *B607350,1                lfBalanced() function.
    lnTmpAdjRN = RECNO(lcTmpAdj)
    *B607350,1 KHM 06/29/2003 (End)
    =lfBalanced()
    
    *B607350,1 KHM 06/29/2003 (Begin) Reposition the record pointer.
    IF BETWEEN(lnTmpAdjRN,1,RECCOUNT(lcTmpAdj))
      GOTO lnTmpAdjRN IN (lcTmpAdj)
    ENDIF
    *B607350,1 KHM 06/29/2003 (End)

    SELECT IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')
    GATHER FROM laStyStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,nStkVal
    
  ENDIF
  *B804152,1 (End)



  SELECT IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                              OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld    WITH lnTotStk
  REPLACE dPostDate WITH ldPstDate
  
  WAIT WINDOW 'Start updating => '+ALLTRIM(Style)+ ;
              IIF(llDyelvl,'/'+&lcTmpAdj->Dyelot,'') NOWAIT


  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")

  *--Start Updating.
  IF ! lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF
    
  
  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession

  *--Update Master file for locations.
  IF llWareLoc 
    SELECT (lcTemLoc)
    SCAN FOR !EMPTY(cLocation)
      IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
        SCATTER MEMVAR
        INSERT INTO WhsLoc FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF 

  SELECT (lcTmpAdj)
  DO lpUnLock
ENDSCAN

*--Update master adjustment file.
WAIT WINDOW ' Updating the master adjustment file.' NOWAIT

*E301558,1 AHM Send product Activity
IF 'NC' $ gcCmpModules AND !llFromEDI
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
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
  IF gcCurSite = lcBackSite
    IF lcFromSite <> lcBackSite
      lcSendTo1 = lcFromSite
    ENDIF
    IF lcToSite <> lcBackSite
      lcSendTo2 = lcToSite
    ENDIF
  ELSE
    lcSendTo1 = lcBackSite
    IF !INLIST(lcFromSite,lcBackSite,gcCurSite)
      lcSendTo2 = lcFromSite
    ENDIF
    IF !INLIST(lcToSite,lcBackSite,gcCurSite)
      lcSendTo3 = lcToSite
    ENDIF
  ENDIF
  IF !EMPTY(lcSendTo1) OR !EMPTY(lcSendTo2) OR !EMPTY(lcSendTo3)
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

    SELECT Date FROM (lcTmpAdj) GROUP BY Date INTO CURSOR Dates
    SELECT Dates
    SCAN
      lcDateKey = DTOS(Date)+'-'+DTOS(Date)
      IF !EMPTY(lcSendTo1)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo1
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *E037853,1 [End]
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
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *E037853,1 [End]
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
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *E037853,1 [End]
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
ENDIF
*E301558,1 AHM (End)

*!C037816,1 MHM 04/06/2004 Save Bin location custom field for DL[Start]
IF ASCAN(laEvntTrig,PADR("DLSAVDAT",10)) <> 0 
  *! C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
  *=gfDoTriger("ICSTYAD",PADR("DLSAVDAT",10))
  IF lcProgName = "ICSTYPH" 
    =gfDoTriger("ICSTYPH",PADR("DLSAVDAT",10))
  ELSE
    =gfDoTriger("ICSTYAD",PADR("DLSAVDAT",10))
  ENDIF    
  *! C123853,1 MHM 01/15/2005  [End]
ENDIF
*!C037816,1 MHM 04/06/2004 [End]

USE IN (lcTmpAdj)
SELECT InvtAdj
APPEND FROM &gcWorkDir.&lcTmpAdj FOR !DELETED()

*B602457 MAB The following line was converted to normally open [Begin
*B602457 because we does not open temp. files using gfopenfile.
*=gfOpenFile(gcWorkDir+lcTmpAdj,' ','EX')
USE (gcWorkDir+lcTmpAdj) IN 0 EXCLUSIVE
SELECT (lcTmpAdj)
*B602457 MAB The following line was converted to normally open [End..
SET ORDER TO TAG &lcTmpAdj
SELECT (lcTmpAdj)
ZAP
*B606489,3 TMI [START] Reset the relation to the style file after reopen lcTmpAdj file 
SET RELATION TO STYLE INTO STYLE
*B606489,3 TMI [END  ] Reset the relation to the style file after reopen lcTmpAdj file 

*-- Update distripution master file
IF llGlLink
  
  *!C037816,1 MHM 04/06/2004 Custom Update Gl Bin Location For David Luke[Start]
  IF ASCAN(laEvntTrig,PADR("DLUPDTGL",10)) <> 0 
    *! C123853,1 MHM 01/15/2005 handle trigger in physical program too [Start]
    *=gfDoTriger("ICSTYAD",PADR("DLUPDTGL",10))
    IF lcProgName = "ICSTYPH" 
      =gfDoTriger("ICSTYPH",PADR("DLUPDTGL",10))
    ELSE
      =gfDoTriger("ICSTYAD",PADR("DLUPDTGL",10))
    ENDIF     
    *! C123853,1 MHM 01/15/2005  [End]
  ELSE
  *!C037816,1 MHM [End]
  
    WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
    SELECT (lcTmpGlDt)
    *-- Generate a unique session number.
    lcGlSess = gfsequence('GLSESSION')
    REPLACE ALL GLSESSION WITH lcGlSess
    USE
    SELECT GLDIST  
    APPEND FROM &gcWorkDir.&lcTmpGlDt     
    ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
  
  *!C037816,1 MHM 04/06/2004 Custom Update Gl Bin Location For David Luke[Start]
  ENDIF
  *!C037816,1 MHM [End]
  
ENDIF  
WAIT CLEAR

*B802687,1 RAMY [Begin]
*lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
*lcToWare   = SPACE(06)
*lcWFromDsc = SPACE(30)
*lcWToDsc   = SPACE(30)
*SELECT (lcTmpAdj) 
*GO TOP
*=lfActBrow()
*laScrMode = .F.
*laScrMode[1] = .T.
*SHOW GETS  
*RETURN
*B802687,1 RAMY [end]

*!*************************************************************
*! Name      : lfRecLock
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Record Lock.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcFile->Locked file.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRecLock()
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
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Unlock the saved locked records.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
PROCEDURE lpUnLock
IF llDyeLvl
  SET MULTILOCKS OFF
ELSE
  UNLOCK ALL
ENDIF

*!*************************************************************
*! Name      : lfInvUpdt()
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Update inventory.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfInvUpdt()
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
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


SELECT (lcTmpAdj)
*--Adjustment quantity array TRANSFER Case.
IF lcType = 'T'
  DECLARE laAdjust[9]
  FOR I = 1 TO 8
    Z=STR(I,1)
    laAdjust[I] = -Adj&Z
  ENDFOR
  laAdjust[9] = -TotAdj

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start) AAMER 04/19/98
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,cRefer,@laAdjust,0,;
            cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  *E301192 (Start)
  PRIVATE lcRefer 
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  lcRefer = "To location " + lcToWare
  
  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Send the adjustment reference in case of inventory adjustment.
  lcAdjRef = &lcTmpAdj..cRefer
  *lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)
  lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *B802503,1 Reham On 08/10/1999   *** End   ***
  
  *E301192 (End)
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)

  *--Read an issue seesion no to use it in receive if LIFO or FIFO.
  lcIsuSessNo = STYINVJL.cSession
  
  *--Return with no save if function return fulse.
  IF lnRet = 0
    RETURN .F.
  ENDIF  

  *! C123853,1 MHM 01/15/2005 handle case to update BinInvJl [Start]
  IF ASCAN(laEvntTrig,PADR("DLSVBNI",10)) <> 0 
    =gfDoTriger("ICSTYTR",PADR("DLSVBNI",10))
  ENDIF
  *! C123853,1 MHM 01/15/2005  [End]

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
  SEEK Style+&lcTmpAdj..cFromWare+lcIsuSessNo
  SCAN WHILE Style+cWareCode+cSession = Style+&lcTmpAdj..cFromWare+lcIsuSessNo
    lnSavRcNo = RECNO()
    *--Adjustment quantity array.
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = ABS(nStk&Z)
    ENDFOR
    laAdjust[9] = ABS(nTotStk)

    *--Call the global function for update style inventory control.
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,Reference,@laAdjust,;
                   nCost,cTrCode,.T.,cAdjReason,0,'','',@laGLDistAr)
    *E301192 (Start)
    PRIVATE lcRefer 
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,Reference,.T.,cAdjReason,0,'','',@laGLDistAr)
    lcRefer = "From location "+ lcFromWare
    
    *B802503,1 Reham On 08/10/1999   *** Begin ***
    *B802503,1 Send the adjustment reference in case of inventory adjustment.
    lcAdjRef = &lcTmpAdj..cRefer
    *lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)
    lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
    *B802503,1 Reham On 08/10/1999   *** End   ***

    *! C123853,1 MHM 01/15/2005 handle case of updating BININVJL [Start]
    IF ASCAN(laEvntTrig,PADR("DLSVBNR",10)) <> 0 
      lcRcvSessNo = STYINVJL.cSession
      =gfDoTriger("ICSTYTR",PADR("DLSVBNR",10))
    ENDIF
    *! C123853,1 MHM 01/15/2005  [End]
    
    *E301192 (End)
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
    SELECT STYINVJL
    GOTO lnSavRcNo   
  ENDSCAN 

ELSE
  *--Adjustment quantity array.
  DECLARE laAdjust[9]
  SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust

  *--Adjustment cost.
  lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
  *--Type of the adjustment.
  lcAdjTyp = IIF(lcType = 'P','2','1')

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,cRefer,@laAdjust,lnACost,;
                 cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  *E301192 (Start)
  PRIVATE lcRefer 
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 cReason,.T.,cAdjReason,0,'','',@laGLDistAr)
  IF lcType = 'T'
    lcRefer = "From location " + lcFromWare
  ELSE
    lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)
  ENDIF
  
  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Send the adjustment reference in case of inventory adjustment.
  lcAdjRef = &lcTmpAdj..cRefer
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr)  

  *! C123853,1 MHM 01/15/2005 handle trigger in physical program  [Start]
  *lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
  *               lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)

  IF ASCAN(laEvntTrig,PADR("DLSTYCRL",10)) <> 0 
   lnRet = 0
    =gfDoTriger("ICSTYPH",PADR("DLSTYCRL",10))
  ELSE
    lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  ENDIF

  IF ASCAN(laEvntTrig,PADR("DLSVBNI",10)) <> 0 
    lcIsuSessNo = STYINVJL.cSession
    =gfDoTriger("ICSTYTR",PADR("DLSVBNI",10))
  ENDIF
  *! C123853,1 MHM 01/15/2005  [End]

  *B802503,1 Reham On 08/10/1999   *** End   ***
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
ENDIF

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF  
RETURN
