*:************************************************************************
*:
*: Procedure file: GLPURGE.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow       ** Show procedure.
*:                 lfvPrgSum    ** Radio button valid func.
*:                 lfvPrgTrns   ** Radio button purge trn. or trn & bal. valid func.
*:                 lfwPgTrYer   ** Var. lcPgTrYer when func.
*:                 lfvPgTrYer   ** Var. lcPgTrYer valid func.
*:                 lfwPgBlYer   ** Var. lcPgBlYer when func.
*:                 lfvPgBlYer   ** Var. lcPgBlYer valid func.
*:                 lfwSmTrYer   ** Var. lcSmTrYer when func.
*:                 lfvSmTrYer   ** Var. lcSmTrYer valid func.
*:                 lfvType      ** Post,Unpost,Summarize check box valid func.
*:                 lfvProceed   ** Push button proceed valid func.
*:                 lfPurgTrns   ** Purge transactions func.
*:                 lfPurgBlnc   ** Purge balances func.
*:                 lfSumarize   ** Summarization func.
*:                 
*:      Documented   /  /
*:************************************************************************
*E300663,1   HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1               for the changes we have made to that function.
*E300683,5  AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1  ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 	 				SYCFSPRD, SYCFSHLD
*E300789,6  MAB 03/08/99 Remove cComp_ID from files
*E301077,72 WALID 03/08/99 Open files properly
*:************************************************************************
*
lcPgTrYer  = ""      && Purge transactions to year.
lcOldPgTrY = ""      && Keep old Purge transactions to year.

lcPgBlYer  = ""      && Purge balances to year.
lcOldPgBlY = ""      && Keep old Purge balances to year.

lcSmTrYer  = ""      && Summarize transaction to year.
lcOldSmTrY = ""      && Keep old Summarize transaction to year.

lcType     = ""      && Var hold the types available to purge.

rbPrgSum   = 1       && Initial value for radio button var.  {purge/summarize}
rbPrgTrns  = 1       && Initial value for check box var. {purge trans.}

cbPost     = 1       && Var hold the post check box value.
cbUnpost   = 1       && Var hold the uu post check box value.
cbSumrz    = 1       && Var hold the summarize check box value.

llPurg_Don = .F.     && Flag to know if the purging process done 
                     && successfully or not.
llSum_Pst  = .F.     && Flag to know if build temp files for 
                     && posted entries done or not.
llSum_Upst = .F.     && Flag to know if build temp files for 
                     && unposted entries done or not.

laCtrStat  = 'DISABLE' && To disable the browse pad in the menu

*E301077,72 Transfer gfSetup here [begin
IF !gfSetup()    
  RETURN
ENDIF  
*E301077,72 Transfer gfSetup here [end

*** See if there is any history years in the fiscal header ***
*** file for the purpose of purge balances...
*E300692,1 Change file name from SYCFISHD to FISHD
*SELECT SYCFISHD
SELECT FISHD
*E300692,1 end
*IF SEEK(gcAct_Comp+ALLTRIM(STR(lnCurr_yer-2)))

*E300789,6 Index does not have company Id[begin
*IF SEEK(gcPrnt_Cmp+ALLTRIM(STR(lnCurr_yer-2)))
IF SEEK(ALLTRIM(STR(lnCurr_yer-2)))
*E300789,6 Index does not have company Id[end

  *** Flag to display the check box {purge balances up to year} ***
  llObjdisp = .T.
ELSE
  *** Flag to display the check box {purge balances up to year} ***
  llObjdisp = .F.
ENDIF

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars ....

*E301077,72 gfSetup must be before file using [begin
*IF !gfSetup()    
*  RETURN
*ENDIF  
*E301077,72 gfSetup must be before file using [end

*** Call the type function to put the different batches' types in the   ***
*** "lcType" variable because of setting all the check boxes to default ***
*** [X] {or checked} that's mean that all the types should be defaulted ***
*** also...
=lfvType()

lcBach_Tmp = gfTempName()  && Create name for batches temp. file.
lcTrHd_Tmp = gfTempName()  && Create name for transactions temp. file.
lcTrDt_Tmp = gfTempName()  && Create name for entries temp. file.

*** Create temp. file for the transactions header ***
SELECT GLTRNSHD
COPY STRUCTURE TO &gcWorkDir.&lcTrHd_Tmp CDX
SELECT 0
USE &gcWorkDir.&lcTrHd_Tmp EXCLUSIVE

*** Create temp. file for the batches ***
SELECT GLBATCH
COPY STRUCTURE TO &gcWorkDir.&lcBach_Tmp CDX
SELECT 0
USE &gcWorkDir.&lcBach_Tmp EXCLUSIVE

*** We strongly  recommend that a full system backup is  done ***
*** immediately prior to the {purge or summarize} processing. ***
*** Do you want to continue ? 
*** <  Proceed  > - < Cancel > ***
IF gfModalGen("QRM00097B00012","ALERT","purge or summarize") = 1
  *E300683,5 Call *.SPR from screens directory
  * DO GLPURGE.SPR 
  DO (gcScrDir + gcWinAppl + '\GLPURGE.SPR')
  *E300683,5 end   
ENDIF

glQuitting = .T.

*** If quit from the program close all the temp. files ***
*** and delete them from the disk.
IF USED(lcTrHd_Tmp)
  USE IN ALIAS(lcTrHd_Tmp)
ENDIF
ERASE &gcWorkDir.&lcTrHd_Tmp..DBF
*YMO
ERASE &gcWorkDir.&lcTrHd_Tmp..CDX
IF USED(lcTrDt_Tmp)
  USE IN ALIAS(lcTrDt_Tmp)
ENDIF
ERASE &gcWorkDir.&lcTrDt_Tmp..DBF

IF USED(lcBach_Tmp)
  USE IN ALIAS(lcBach_Tmp)
ENDIF
ERASE &gcWorkDir.&lcBach_Tmp..DBF
*YMO
ERASE &gcWorkDir.&lcBach_Tmp..CDX
*!**************************************************************************
*!
*!      PROCEDURE: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  CASE rbPrgSum = 1    && Purge process.

    *** Disable summarize objects. ***
    lcSmTrYer  = ""
    SHOW GET lcSmTrYer DISABLE

    *** Enable purge objects. ***
    IF llObjdisp
      SHOW GET rbPrgTrns,1 ENABLE
      SHOW GET rbPrgTrns,2 ENABLE
      IF rbPrgTrns = 1
        SHOW GET lcPgTrYer ENABLE
        SHOW GET lcPgBlYer DISABLE
        SHOW GET cbPost    ENABLE
        SHOW GET cbUnpost  ENABLE
        SHOW GET cbSumrz   ENABLE
      ELSE
        SHOW GET lcPgTrYer DISABLE
        SHOW GET lcPgBlYer ENABLE
        cbPost   = 1
        cbUnpost = 1
        cbSumrz  = 1
        SHOW GET cbPost   DISABLE
        SHOW GET cbUnpost DISABLE
        SHOW GET cbSumrz  DISABLE
      ENDIF
    ELSE
      IF rbPrgTrns = 1
        SHOW GET lcPgTrYer ENABLE
        SHOW GET lcPgBlYer DISABLE
        SHOW GET cbPost    ENABLE
        SHOW GET cbUnpost  ENABLE
        SHOW GET cbSumrz   ENABLE
      ENDIF
      SHOW GET rbPrgTrns,1 ENABLE
      SHOW GET rbPrgTrns,2 DISABLE
    ENDIF

  CASE rbPrgSum = 2    && Summarize process.
    *** Disable purge objects. ***
    lcPgTrYer  = ""
    SHOW GET lcPgTrYer   DISABLE
    lcPgBlYer  = ""
    SHOW GET lcPgBlYer   DISABLE
    SHOW GET rbPrgTrns,1 DISABLE
    SHOW GET rbPrgTrns,2 DISABLE

    *** Enable summarize objects. ***
    SHOW GET lcSmTrYer ENABLE

    cbPost   = 1
    cbUnpost = 1
    cbSumrz  = 0
    SHOW GET cbPost   ENABLE
    SHOW GET cbUnpost ENABLE
    SHOW GET cbSumrz  DISABLE
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvPrgSum
*!
*!**************************************************************************
*
** Radio button valid func. **

FUNCTION lfvPrgSum

SHOW GETS

DO CASE
  CASE rbPrgSum = 1    && Purge process.
    _CUROBJ = OBJNUM(lcPgTrYer)
  CASE rbPrgSum = 2    && Summarize process.
    _CUROBJ = OBJNUM(lcSmTrYer)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvPrgTrns
*!
*!**************************************************************************
*
** Radio button {purge trans.} or {purge balances & trans.} valid func. **

FUNCTION lfvPrgTrns

DO CASE
  CASE rbPrgTrns = 1          && Check purge transaction.
    *** Save old year then make it empty and disable it. ***
    lcOldPgBlY = lcPgBlYer
    lcPgBlYer  = ""
    SHOW GET lcPgBlYer DISABLE

    lcPgTrYer  = lcOldPgTrY
    SHOW GET lcPgTrYer ENABLE
    _CUROBJ = OBJNUM(lcPgTrYer)
    SHOW GET cbPost    ENABLE
    SHOW GET cbUnpost  ENABLE
    SHOW GET cbSumrz   ENABLE

  CASE rbPrgTrns = 2          && Check purge balances & transaction.
    *** Save old year then make it empty and disable it. ***
    lcOldPgTrY = lcPgTrYer
    lcPgTrYer  = ""
    SHOW GET lcPgTrYer DISABLE

    lcPgBlYer  = lcOldPgBlY
    SHOW GET lcPgBlYer ENABLE
    _CUROBJ = OBJNUM(lcPgBlYer)
    cbPost   = 1
    cbUnpost = 1
    cbSumrz  = 1
    SHOW GET cbPost   DISABLE
    SHOW GET cbUnpost DISABLE
    SHOW GET cbSumrz  DISABLE
ENDCASE

*!**************************************************************************
*!
*!      Function: lfwPgTrYer
*!
*!**************************************************************************
*
** Var. lcPgTrYer when func. **

FUNCTION lfwPgTrYer

lcOldPgTrY = lcPgTrYer

*!**************************************************************************
*!
*!      Function: lfvPgTrYer
*!
*!**************************************************************************
*
** Var. lcPgTrYer valid func. **

FUNCTION lfvPgTrYer

IF lcOldPgTrY <> lcPgTrYer
  IF VAL(lcPgTrYer) >= lnCurr_yer
    *** The entered year should be less than the current year.  You ***
    *** can only purge transaction entries up to the previous year. ***
    *** <  Ok  > ***
    =gfModalGen("TRM02174B00000","DIALOG")
    lcPgTrYer = lcOldPgTrY
    _CUROBJ = OBJNUM(lcPgTrYer)
  ELSE
    *IF !SEEK(gcAct_Comp+ALLTRIM(lcPgTrYer),"SYCFISHD")
  *E300692,1 Change file name from SYCFISHD to FISHD  
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcPgTrYer),"SYCFISHD")

    *E300789,6 Index does not have company Id[begin
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcPgTrYer),"FISHD")
    IF !SEEK(ALLTRIM(lcPgTrYer),"FISHD")
    *E300789,6 Index does not have company Id[end
    
  *E300692,1 end  
      *** This year does not existed in the fiscal header file. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02177B00000","DIALOG")
      lcPgTrYer = lcOldPgTrY
      _CUROBJ = OBJNUM(lcPgTrYer)
    ENDIF
  ENDIF
  
  *** Set both flags of building the temp. file { posted - unposted } ***
  *** to false to rebuild the temp. files again.
  llSum_Pst  = .F.
  llSum_Upst = .F.

ENDIF

*!**************************************************************************
*!
*!      Function: lfwPgBlYer
*!
*!**************************************************************************
*
** Var. lcPgBlYer when func. **

FUNCTION lfwPgBlYer

lcOldPgBlY = lcPgBlYer

*!**************************************************************************
*!
*!      Function: lfvPgBlYer
*!
*!**************************************************************************
*
** Var. lcPgBlYer valid func. **

FUNCTION lfvPgBlYer

IF lcOldPgBlY <> lcPgBlYer
  IF VAL(lcPgBlYer) > lnCurr_yer-2
    *** You can only purge balances up to one ***
    *** year before the previous year.        ***
    *** <  Ok  > ***
    =gfModalGen("TRM02175B00000","DIALOG")
    lcPgBlYer = lcOldPgBlY
    _CUROBJ = OBJNUM(lcPgBlYer)
  ELSE
    *IF !SEEK(gcAct_Comp+ALLTRIM(lcPgBlYer),"SYCFISHD")
  *E300692,1 Change file name from SYCFISHD to FISHD    
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcPgBlYer),"SYCFISHD")

    *E300789,6 Index does not have company Id[begin
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcPgBlYer),"FISHD")
    IF !SEEK(ALLTRIM(lcPgBlYer),"FISHD")
    *E300789,6 Index does not have company Id[end
  
    *E300692,1 end  
      *** This year does not existed in the fiscal header file. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02177B00000","DIALOG")
      lcPgBlYer = lcOldPgBlY
      _CUROBJ = OBJNUM(lcPgBlYer)
    ENDIF
  ENDIF

  *** Set both flags of building the temp. file { posted - unposted } ***
  *** to false to rebuild the temp. files again.
  llSum_Pst  = .F.
  llSum_Upst = .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfwSmTrYer
*!
*!**************************************************************************
*
** Var. lcSmTrYer when func. **

FUNCTION lfwSmTrYer

lcOldSmTrY = lcSmTrYer

*!**************************************************************************
*!
*!      Function: lfvSmTrYer
*!
*!**************************************************************************
*
** Var. lcSmTrYer valid func. **

FUNCTION lfvSmTrYer

IF lcOldSmTrY <> lcSmTrYer
  IF VAL(lcSmTrYer) >= lnCurr_yer
    *** You can only summarize transaction entries ***
    *** up to one year less than the current year. ***
    *** <  Ok  > ***
    =gfModalGen("TRM02176B00000","DIALOG")
    lcSmTrYer = lcOldSmTrY
    _CUROBJ = OBJNUM(lcSmTrYer)
  ELSE
    *IF !SEEK(gcAct_Comp+ALLTRIM(lcSmTrYer),"SYCFISHD")
  *E300692,1 Change file name from SYCFISHD to FISHD      
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcSmTrYer),"SYCFISHD")

    *E300789,6 Index does not have company Id[begin
    *IF !SEEK(gcPrnt_Cmp+ALLTRIM(lcSmTrYer),"FISHD")
    IF !SEEK(ALLTRIM(lcSmTrYer),"FISHD")
    *E300789,6 Index does not have company Id[end

  *E300692,1 end  
      *** This year does not existed in the fiscal header file. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02177B00000","DIALOG")
      lcSmTrYer = lcOldSmTrY
      _CUROBJ = OBJNUM(lcSmTrYer)
    ENDIF
  ENDIF

  *** Set both flags of building the temp. file { posted - unposted } ***
  *** to false to rebuild the temp. files again.
  llSum_Pst  = .F.
  llSum_Upst = .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*
** {Post - Un post - Summarize} check box valid func. **

FUNCTION lfvType

lcType = ""

IF cbUnpost = 1
  lcType = lcType + "OAHEU"
ENDIF

IF cbPost   = 1
  lcType = lcType + "P"
ENDIF

IF cbSumrz  = 1
  lcType = lcType + "Z"
ENDIF

*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
*
** Push button proceed valid func. **

FUNCTION lfvProceed

DO CASE
  *** Purge process. ***
  CASE rbPrgSum = 1
    IF cbPost = 0 .AND. cbUnpost = 0 .AND. cbSumrz = 0
      *** What is the transaction type you want to purge ? ***
      *** You have to select the transaction type you are  ***
      *** going to purge.
      *** <  Ok  > ***
      =gfModalGen("TRM02183B00000","DIALOG","purge|purge")
      _CUROBJ = OBJNUM(cbPost)
      RETURN
    ENDIF
    
    IF EMPTY(lcPgTrYer) .AND. EMPTY(lcPgBlYer)
      *** Please enter the year to purge. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02184B00000","DIALOG","purge")
      IF rbPrgTrns = 1
        _CUROBJ = OBJNUM(lcPgTrYer)
      ELSE
        _CUROBJ = OBJNUM(lcPgBlYer)
      ENDIF
      RETURN
    ENDIF

    DO CASE
      *** Purge transactions only. ***
      CASE rbPrgTrns = 1 .AND. !EMPTY(lcPgTrYer)
        *** Lock all the files that needed in this process. ***
        IF gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT",.T.)
          =lfPurgTrns("CBATSTAT $ lcType .AND. CBATPYR <= lcPgTrYer")
          *** Unlock all the files that locked before purging. ***
          =gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT",.F.)
          
          IF llPurg_Don
            *** Purge process done successfully. ***
            *** <  Ok  > ***
            =gfModalGen("TRM02188B00000","DIALOG")
            _CUROBJ = OBJNUM(pbCancel)
          ENDIF
        ELSE
          
          *** Purge process not completed. ***
          *** <  Ok  > ***
          =gfModalGen("TRM02189B00000","DIALOG")
          _CUROBJ = OBJNUM(pbProceed)
        ENDIF
      
      *** Purge transactions & balances. ***
      CASE rbPrgTrns = 2 .AND. !EMPTY(lcPgBlYer)
        *** Lock all the files that needed in this process. ***
      *E300692,1 Change files name from SYCFISHD and YYCFSPRD to FISHD and FSPRD     
        *IF gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT,GLACBALS,SYCFISHD,SYCFSPRD",.T.)
        IF gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT,GLACBALS,FISHD,FSPRD",.T.)
      *E300692,1 end  
          =lfPurgTrns("CBATPYR <= lcPgBlYer")
          =lfPurgBlnc()
          *** Unlock all the files that locked before purging. ***
        *E300692,1 Change files name from SYCFISHD and YYCFSPRD to FISHD and FSPRD       
          *=gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT,GLACBALS,SYCFISHD,SYCFSPRD",.F.)
          =gfFlock("GLBATCH,GLTRNSHD,GLTRNSDT,GLPTRNHD,GLPTRNDT,GLACBALS,FISHD,FSPRD",.F.)
        *E300692,1 end  
          
          IF llPurg_Don
            *** Purge process done successfully. ***
            *** <  Ok  > ***
            =gfModalGen("TRM02188B00000","DIALOG")
            _CUROBJ = OBJNUM(pbCancel)
          ENDIF
        ELSE
          
          *** Purge process not completed. ***
          *** <  Ok  > ***
          =gfModalGen("TRM02189B00000","DIALOG")
          _CUROBJ = OBJNUM(pbProceed)
        ENDIF
      
    ENDCASE 
  *** Summarize process. ***
  CASE rbPrgSum = 2
    IF cbPost = 0 .AND. cbUnpost = 0
      *** What is the transaction type you want to summarize ? ***
      *** You have to select the transaction type you are  ***
      *** going to summarize.
      *** <  Ok  > ***
      =gfModalGen("TRM02183B00000","DIALOG","summarize|summarize")
      _CUROBJ = OBJNUM(cbPost)
      RETURN
    ENDIF

    IF EMPTY(lcSmTrYer)
      *** Please enter the year to summarize. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02184B00000","DIALOG","summarize")
      _CUROBJ = OBJNUM(lcSmTrYer)
      RETURN
    ENDIF

    *** Call the summarize function with the posted transactions ***
    *** files if the posted check box is choosen.
    IF cbPost = 1
      =lfSumarize('GLPTRNHD','GLPTRNDT','posted',@llSum_Pst)
    ENDIF
    
    *** Call the summarize function with the transactions ***
    *** files if the unposted check box is choosen.
    IF cbUnpost = 1
      =lfSumarize('GLTRNSHD','GLTRNSDT','unposted',@llSum_UPst)
    ENDIF
  
ENDCASE

*!**************************************************************************
*!
*!      Function: lfPurgTrns
*!
*!**************************************************************************
*
** Purge transactions only. **

FUNCTION lfPurgTrns
PARAMETERS lcPrgScope

*** See if there is any record will match the scope or not. ***
SELECT GLBATCH
LOCATE FOR &lcPrgScope

*** Excute the purging processing only if no. of records > 0 ***
IF FOUND()

  ******************************************************************
  *** Set relations between the batch files & transactions files ***
  *** & its entries transactions.                                ***
  ******************************************************************

  *** Set order to tag batchtrn that hold batch no. + trans. no. ***
  *** in all the transaction & posted transaction header file &  ***
  *** in the transaction & posted transaction detail file.       *** 
  SELECT GLBATCH
  SET ORDER TO TAG BATCHNO
  
  SELECT GLTRNSHD
  SET ORDER TO TAG BATCHTRN

  SELECT GLTRNSDT
  SET ORDER TO TAG BATCHTRN

  SELECT GLPTRNHD
  SET ORDER TO TAG BATCHTRN

  SELECT GLPTRNDT
  SET ORDER TO TAG BATCHTRN

  *** Set relation from the batch file into the transaction header ***
  *** , transaction entries , posted  transaction  header , posted ***
  *** transaction entries.
  SELECT GLBATCH
  SET RELATION TO glbatch.cbatchno INTO GLPTRNHD ADDITIVE
  SET RELATION TO glbatch.cbatchno INTO GLPTRNDT ADDITIVE
  SET RELATION TO glbatch.cbatchno INTO GLTRNSHD ADDITIVE
  SET RELATION TO glbatch.cbatchno INTO GLTRNSDT ADDITIVE

  ******************************************************************
        
  SELECT GLBATCH
  lnCurrRec = 0
  lnTotRec  = RECCOUNT()

  SCAN FOR &lcPrgScope
    lnCurrRec = lnCurrRec + 1
    =gfThermo(lnTotRec,lnCurrRec,'Purge entries...',"")

    *** Delete all the transactions header for this batch no. ***
    SELECT GLTRNSHD
    DELETE REST WHILE GLBATCH.CBATCHNO = GLTRNSHD.CBATCHNO

    *** Delete all the transactions entries for this batch no. ***
    SELECT GLTRNSDT
    DELETE REST WHILE GLBATCH.CBATCHNO = GLTRNSDT.CBATCHNO

    *** Delete all the posted transactions header for this batch no. ***
    SELECT GLPTRNHD
    DELETE REST WHILE GLBATCH.CBATCHNO = GLPTRNHD.CBATCHNO

    *** Delete all the posted transactions entries for this batch no. ***
    SELECT GLPTRNDT
    DELETE REST WHILE GLBATCH.CBATCHNO = GLPTRNDT.CBATCHNO

    *** After deleting all the transactions that belongs to this ***
    *** batch no., Delete this batch no. itself.
    SELECT GLBATCH
    DELETE

  ENDSCAN

  IF lnTotRec > lnCurrRec
    FOR lnThCount = lnCurrRec TO lnTotRec
      =gfThermo(lnTotRec,lnThCount,'Purge entries...',"")
    ENDFOR
  ENDIF

  *** Clear the relation. ***
  SELECT GLBATCH
  SET RELATION TO
  llPurg_Don = .T.
ELSE
  *** There is no posted or unposted or summarized records to purge. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02178B00000","DIALOG","posted or unposted or summarized|purge")
  llPurg_Don = .F.
  _CUROBJ = OBJNUM(pbCancel)
  RETURN
ENDIF

*!**************************************************************************
*!
*!      Function: lfPurgBlnc
*!
*!**************************************************************************
*
** Purge balances only. **

FUNCTION lfPurgBlnc

*** Delete all the records  in the  balances file ***
*** that its year equal or less than the enntered ***
*** year.
SELECT GLACBALS
lcPrv1_Tag = SYS(22)
SET ORDER TO TAG FISFYEAR DESCENDING
IF SEEK(lcPgBlYer)
  DELETE REST
ENDIF
SET ORDER TO TAG &lcPrv1_Tag

*** Delete all the records in the fiscal header file ***
*** for the  current company  that its year equal or ***
*** less than the enntered year.
*E300692,1 Change file name from SYCFISHD to FISHD       
*SELECT SYCFISHD
SELECT FISHD
*E300692,1 end
*DELETE ALL FOR CFISFYEAR <= lcPgBlYer .AND. CCOMP_ID = gcAct_Comp

*E300789,6 File does not have ccomp_id field[begin
*DELETE ALL FOR CFISFYEAR <= lcPgBlYer .AND. CCOMP_ID = gcPrnt_Cmp
DELETE ALL FOR CFISFYEAR <= lcPgBlYer
*E300789,6 File does not have ccomp_id field[end

*** Delete all the records in the fiscal period file ***
*** for the  current company  that its year equal or ***
*** less than the enntered year.
*E300692,1 Change file name from SYCFSPRD to FSPRD       
*SELECT SYCFSPRD
SELECT FSPRD
*E300692,1 end
*DELETE ALL FOR CFISFYEAR <= lcPgBlYer .AND. CCOMP_ID = gcAct_Comp

*E300789,6 File does not have ccomp_id field[begin
*DELETE ALL FOR CFISFYEAR <= lcPgBlYer .AND. CCOMP_ID = gcPrnt_Cmp
DELETE ALL FOR CFISFYEAR <= lcPgBlYer
*E300789,6 File does not have ccomp_id field[end

*!**************************************************************************
*!
*!      Function: lfSumarize
*!
*!**************************************************************************
*
** Summarize function. **

FUNCTION lfSumarize
PARAMETERS lcHeader,lcDetail,lcSumType,llBld_Tmp

IF !llBld_Tmp
  *** Collect the enties from the posted transation detail file   ***
  *** record for each {source modul/source journal/year/period/   ***
  *** account} ordered by {year/period/S.M./S.J./debit or credit} ***

  SELECT &lcDetail..cbatchno,&lcDetail..ctranno,&lcDetail..cacctcode,;
         &lcDetail..ctrdtexp,IIF(SIGN(SUM(IIF(&lcDetail..CDRORCR="D",1,;
         -1)*&lcDetail..NAMOUNT))=1,"D","C") AS "CDRORCR",ABS(SUM(IIF;
         (&lcDetail..CDRORCR="D",1,-1)*&lcDetail..NAMOUNT)) AS "NAMOUNT",;
         &lcDetail..dtrnpdate,&lcDetail..ctrnpyr, &lcDetail..ctrnpprd,; 
         &lcDetail..cadd_user,&lcDetail..dadd_date,&lcDetail..cadd_time,;
         &lcDetail..llok_stat,&lcDetail..clok_user,&lcDetail..dlok_date,;
         &lcDetail..clok_time,&lcHeader..CSRCMODUL,&lcHeader..CSRCJRNL,;
         &lcHeader..CSTANDARD,&lcHeader..CCOMP_ID,&lcHeader..CTRNSTAT;
   FROM  &lcDetail,&lcHeader;
   INTO  DBF &gcWorkDir.&lcTrDt_Tmp ;
   WHERE &lcHeader..CTRANNO = &lcDetail..CTRANNO;
         .AND. &lcDetail..CTRNPYR <= lcSmTrYer;
         .AND. &lcHeader..CTRNTYPE <> "Z";
   GROUP BY &lcDetail..CACCTCODE, &lcDetail..CTRNPYR, &lcDetail..CTRNPPRD,;
         &lcHeader..CSRCMODUL,&lcHeader..CSRCJRNL;
   ORDER BY &lcDetail..CTRNPYR,&lcDetail..CTRNPPRD,&lcHeader..CSRCMODUL,;
         &lcHeader..CSRCJRNL,&lcHeader..CSTANDARD,&lcHeader..CTRNSTAT

  IF _TALLY = 0
    *** There is no {posted - unposted} records to summarize. ***
    *** <  Ok  > ***
    =gfModalGen("TRM02178B00000","DIALOG",lcSumType+"|"+"summarize")
    _CUROBJ = OBJNUM(pbCancel)
    RETURN
  ENDIF

  *** Get the current user name. ***
  lcUserNam = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')

  *** This var. hold source (M)odul+source (J)ournal+(Y)ear+ ***
  *** (P)eriod+(S)tandard or statistical. ***
  lcYPMJS    = ""

  *** Set variables to get the transaction no. and the batch no. ***
  lcTrans    = 0
  lcBatch    = 0
  ldDate     = IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                   LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))

  SELECT (lcTrDt_Tmp)
  lnCurrRec = 0
  lnTotRec  = RECCOUNT()
  SCAN
    lnCurrRec = lnCurrRec + 1
    =gfThermo(lnTotRec,lnCurrRec,'Creating summarized entries...',"")

    IF !(&lcTrDt_Tmp..CTRNPYR+&lcTrDt_Tmp..CTRNPPRD+;
        &lcTrDt_Tmp..CSRCMODUL+&lcTrDt_Tmp..CSRCJRNL+;
        &lcTrDt_Tmp..CSTANDARD+&lcTrDt_Tmp..CTRNSTAT == lcYPMJS)

      *** Get new transaction no. ***
      lcTrans = lcTrans + 1

      *** Get new batch no. ***
      lcBatch = lcBatch + 1

      *** Get the begin & end date for the current period ***
      *** according to the current company.
*      IF SEEK(gcAct_comp+&lcTrDt_Tmp..cTrnpyr;
              +&lcTrDt_Tmp..cTrnpprd,"SYCFSPRD")
    *E300692,1 Change file name from SYCFSPRD to FSPRD       
      *IF SEEK(gcPrnt_Cmp+&lcTrDt_Tmp..cTrnpyr;
              +&lcTrDt_Tmp..cTrnpprd,"SYCFSPRD")
      
      *E300789,6 Index does not have company Id[begin
      *IF SEEK(gcPrnt_Cmp+&lcTrDt_Tmp..cTrnpyr;
      *        +&lcTrDt_Tmp..cTrnpprd,"FSPRD")
      IF SEEK(&lcTrDt_Tmp..cTrnpyr+&lcTrDt_Tmp..cTrnpprd,"FSPRD")
      *E300789,6 Index does not have company Id[end

    *E300692,1 end       
         
      *E300692,1 Change file name from SYCFSPRD to FSPRD         
        *ldBeginDat = SYCFSPRD.dFsppbgdt
        *ldEndDat   = SYCFSPRD.dFsppendt
        ldBeginDat = FSPRD.dFsppbgdt
        ldEndDat   = FSPRD.dFsppendt
      *E300692,1 end  
        
      ELSE
        ldBeginDat = {}
        ldEndDat   = {}
      ENDIF

      *** Insert new record in the batches temp. file. ***
      INSERT INTO &lcBach_Tmp ; 
                  (CBATCHNO,CBATSTAT,CBATTYPE,CBATPYR,;
                  DBATPBEG,DBATPEND,CBATREFER,CBATDESC,;
                  CSRCMODUL,CCOMP_ID) ;
           VALUES (PADL(lcBatch,FSIZE('CBATCHNO'),'0'),;
                  'Z','Z',&lcTrDt_Tmp..cTrnpyr,;
                  ldBeginDat,ldEndDat,'On '+ldDate,;
                  'Summarized by '+lcUserNam,;
                  &lcTrDt_Tmp..cSrcmodul,&lcTrDt_Tmp..cComp_Id)

      *** Insert new record in the transactions temp. file. ***
      INSERT INTO &lcTrHd_Tmp ;
                  (CBATCHNO,CTRANNO,CTRNDESC,CTRNREFER,;
                  CTRNPYR,CTRNPPRD,CTRNSTAT,CTRNTYPE,;
                  CSRCMODUL,CSTANDARD,CSRCJRNL,CCOMP_ID);
           VALUES (PADL(lcBatch,FSIZE('CBATCHNO'),'0'),;
                  PADL(lcTrans,FSIZE('CTRANNO'),'0'),;
                  'Summarized by '+lcUserNam,;
                  'On '+ldDate,&lcTrDt_Tmp..cTrnpyr,;
                  &lcTrDt_Tmp..cTrnpprd,'Z','Z',;
                  &lcTrDt_Tmp..cSrcmodul,&lcTrDt_Tmp..cStandard,;
                  &lcTrDt_Tmp..cSrcjrnl,&lcTrDt_Tmp..cComp_Id)

    ENDIF
    SELECT (lcTrDt_Tmp)

    *** Save the values of the ordered fields. ***
    lcYPMJS = &lcTrDt_Tmp..CTRNPYR+&lcTrDt_Tmp..CTRNPPRD+;
              &lcTrDt_Tmp..CSRCMODUL+&lcTrDt_Tmp..CSRCJRNL+;
              &lcTrDt_Tmp..CSTANDARD+&lcTrDt_Tmp..CTRNSTAT

    *** Replace the batch no. & trns. no. with new ***
    *** generated codes in the entries temp. file. ***
    REPLACE CBATCHNO WITH PADL(lcBatch,FSIZE('CBATCHNO'),'0') ;
            CTRANNO  WITH PADL(lcTrans,FSIZE('CTRANNO'),'0')

  ENDSCAN

  IF lnTotRec > lnCurrRec 
    FOR lnThCount  = lnCurrRec TO lnTotRec
      =gfThermo(lnTotRec,lnThCount,'Creating summarized entries...',"")
    ENDFOR  
  ENDIF
  
  *** Set this flag to true to confirm that the temp. files ***
  *** building is done successfully.
  llBld_Tmp = .T.
  
ENDIF

*** Do you want to continue with summarization ***
*** {posted - unposted} transaction process ? ***
*** <  Yes  >  <  No  > ***
IF gfModalGen("TRM02179B00006","DIALOG",lcSumType) = 1
    
  *** Update the master files with the generated data. ***
  =lfUpd_Sum()

ENDIF

*!**************************************************************************
*!
*!      Function: lfUpd_Sum
*!
*!**************************************************************************
*
** Update the master files after the summarization process. **

FUNCTION lfUpd_Sum

*** Lock all the files that needed in this process. ***
*** If summarize posted entries,   lock : GLBATCH - GLPTRNHD - GLPTRNDT
*** If summarize unposted entries, lock : GLBATCH - GLTRNSHD - GLTRNSDT

IF gfFlock("GLBATCH,&lcHeader,&lcDetail",.T.)
  *** Update the master files with the generated data. ***

  
  *** Set the variables that hold the transactions no. & the batch no. ***
  lcTranNo  = ""
  lcBatchNo = ""

  *** Setvar. to hold total debit & credit to be replace ***
  *** in the batches or transactions temp. file.
  lnTotTrnDb = 0
  lnTotTrnCr = 0
  lnTotBatDb = 0
  lnTotBatCr = 0

  *** Set the index to the right tag in the temp. files ***
  *** to build the relations needed in this process.
  SELECT (lcTrHd_Tmp)
  SET ORDER TO TAG BATCHTRN

  SELECT (lcBach_Tmp)
  SET ORDER TO TAG BATCHNO

  SELECT (lcTrDt_Tmp)
  SET RELATION TO &lcTrDt_Tmp..CBATCHNO+&lcTrDt_Tmp..CTRANNO INTO &lcTrHd_Tmp. ADDITIVE

  SELECT (lcTrHd_Tmp)
  SET RELATION TO &lcTrHd_Tmp..CBATCHNO INTO &lcBach_Tmp). ADDITIVE
  
  *** Set variables to hold the batch key & the transactions key. ***
  lcBatKey   = ""
  lcTrnKey   = ""
  
  SELECT (lcTrDt_Tmp)
  
  lnCurrRec = 0
  lnTotRec  = RECCOUNT()
  SCAN
    lnCurrRec = lnCurrRec + 1
    =gfThermo(lnTotRec,lnCurrRec,;
              'Update the master files with summarized entries...',"")
    
    *** If the batch no. changed, that's maen it's a new ***
    *** record.
    IF lcBatKey <> &lcTrDt_Tmp..cBatchno
      *** Put in the batch key var.  the ***
      *** batch no. in the details file. ***
      lcBatKey = &lcTrDt_Tmp..cBatchno
      
      *** Create new batch no. ***

      *E300663,1 Change this lines for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBatchNo = gfSequence('BATCH',1)     && Create batch no.
      *lcBatchNo = RIGHT('000000'+ALLTRIM(STR(lcBatchNo)),6)
      lcBatchNo = gfSequence('CBATCHNO')
      *E300663,1 Change this lines [End]
      
*      lcBatchNo = PADL(gfSequence("BATCH",1),FSIZE(&lcTrDt_Tmp..CBATCHNO),"0")

      *** Collect all the batch data from the batches temp. ***
      *** file in memory variables and put the right batch  ***
      *** in its field and make all the totals equal zero.  ***
      SELECT (lcBach_Tmp)
      SCATTER MEMVAR MEMO
      m.cBatchno  = lcBatchNo
      m.nBatCnTot = 0
      m.nBatotDr  = 0
      m.nBatotCr  = 0
      m.cAdd_user = gcUser_ID
      m.dAdd_date = DATE()
      m.cAdd_time = gfGetTime()

      *** Insert new record in GLBATCH master file from ***
      *** the memory variable that hold the batch data. ***
      SELECT GLBATCH
      APPEND BLANK
      GATHER MEMVAR MEMO
      
      *** Set all the total debits & credits to zeros. ***
      lnTotBatDb = 0
      lnTotBatCr = 0

    ENDIF
    
    IF lcTrnKey <> &lcTrDt_Tmp..cTranno
      *** Put in the transaction key var.  the ***
      *** transaction no. in the details file. ***
      lcTrnKey = &lcTrDt_Tmp..cTranno
      
      *** Create new transaction no. ***

      *E300663,1 Change this lines for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcTranNo = gfSequence('TRANSACT',1)  && Create transaction no.
      *lcTranNo = RIGHT('00000000'+ALLTRIM(STR(lcTranNo)),8)
      lcTranNo = gfSequence('CTRANNO')
      *E300663,1 Change this lines [End]
      
*      lcTranNo = PADL(gfSequence("TRANSACT",1),FSIZE(&lcTrDt_Tmp..CTRANNO),"0")

      *** Collect all the transaction data from the transaction ***
      *** header  temp. file  in memory  variables and  put the ***
      *** right both batch no. and transaction no. in its field ***
      *** and make all the totals equal zero.  ***
      SELECT (lcTrHd_Tmp)
      SCATTER MEMVAR MEMO
      m.cBatchno  = lcBatchNo
      m.cTranno   = lcTranNo
      m.nTrnTotDr = 0
      m.nTrnTotCr = 0
      m.cAdd_user = gcUser_ID
      m.dAdd_date = DATE()
      m.cAdd_time = gfGetTime()

      *** Insert new  record in { GLTRNSHD OR GLPTRNHD } ***
      *** master file from the memory variable that hold ***
      *** the transaction header data. ***
      SELECT (lcHeader)
      APPEND BLANK
      GATHER MEMVAR MEMO
      
      *** Set all the total debits & credits to zeros. ***
      lnTotTrnDb = 0
      lnTotTrnCr = 0

    ENDIF
    
    *** Calculate the total debit or credit for each ***
    *** group of entries.
    DO CASE
      CASE &lcTrDt_Tmp..cDrorcr = 'D'
        lnTotTrnDb   = lnTotTrnDb + &lcTrDt_Tmp..nAmount
        lnTotBatDb   = lnTotBatDb + &lcTrDt_Tmp..nAmount
      CASE &lcTrDt_Tmp..cDrorcr = 'C'
        lnTotTrnCr   = lnTotTrnCr + &lcTrDt_Tmp..nAmount
        lnTotBatCr   = lnTotBatCr + &lcTrDt_Tmp..nAmount
    ENDCASE

    *** Collect the transaction details from the details file ***
    *** in  memory variables  and put the right batch no. and ***
    *** transaction no. 
    SELECT (lcTrDt_Tmp)
    SCATTER MEMVAR MEMO
    m.cBatchno = lcBatchNo
    m.cTranno  = lcTranNo
    m.cAdd_user = gcUser_ID
    m.dAdd_date = DATE()
    m.cAdd_time = gfGetTime()

    *** Insert new record in the { GLTRNSDT OR GLPTRNDT } ***
    *** master file from  the memvar variables  that hold ***
    *** the details data.
    SELECT (lcDetail)
    APPEND BLANK
    GATHER MEMVAR MEMO
    
    *** Put the totals in its fields in the { GLTRNSHD  OR GLPTRNHD } ***
    *** for the debit and credit detail by detail.
    SELECT (lcHeader)
    REPLACE nTrnTotDr WITH lnTotTrnDb;
            nTrnTotCr WITH lnTotTrnCr
    
    *** Put the totals in its fields in the {GLBATCH } for ***
    *** the debit and credit detail by detail.
    SELECT GLBATCH
    REPLACE nBatCnTot WITH lnTotBatDb;
            nBatotDr  WITH lnTotBatDb;
            nBatotCr  WITH lnTotBatCr
    
  ENDSCAN

  IF lnTotRec > lnCurrRec 
    FOR lnThCount  = lnCurrRec TO lnTotRec
      =gfThermo(lnTotRec,lnThCount,;
                'Update the master files with summarized entries...',"")
    ENDFOR  
  ENDIF

  *** Clear the relation between the temp. transaction detail ***
  *** and the temp. transaction header file.
  SELECT (lcTrDt_Tmp)
  SET RELATION TO

  *** Clear the relation between the temp. transaction header ***
  *** file and the temp. batch file.
  SELECT (lcTrHd_Tmp)
  SET RELATION TO
  
  *** Set relation from the transactions header master file ***
  *** into the transactions  details master file to  delete ***
  *** the right  records in  the  transactions  detail file ***
  *** according to the status 'Z' in the transaction header ***
  SELECT (lcHeader)
  SET ORDER TO TAG BATCHTRN

  SELECT (lcDetail)
  SET ORDER TO TAG BATCHTRN
  SET RELATION TO &lcDetail..cbatchno+&lcDetail..ctranno INTO &lcHeader ADDITIVE

  *** Delete all the records in the transactions details file ***
  *** if its year less than or equal the entered year and the ***
  *** type in its header record is 'Z'.
  SELECT (lcDetail)
  DELETE ALL FOR &lcDetail..cTrnpyr <= lcSmTrYer .AND. ;
                 &lcHeader..cTrntype <> 'Z'
  SET RELATION TO

  *** Delete all the records in the transactions header file if ***
  *** its year less than the entered year and its type is 'Z'.  ***
  SELECT (lcHeader)
  DELETE ALL FOR cTrnpyr <= lcSmTrYer .AND. cTrntype <> 'Z'

  *** Delete all the records in the batches file if its year  ***
  *** less than or equal the entered year and its type is 'Z' ***
  SELECT GLBATCH
  DELETE ALL FOR cBatpyr <= lcSmTrYer .AND. cBattype <> 'Z'

  *** Clean the temp. files. ***
  SELECT (lcBach_Tmp)
  ZAP
  
  SELECT (lcTrHd_Tmp)
  ZAP

  SELECT (lcTrDt_Tmp)
  ZAP

  *** Set this flag to false to confirm that updating of ***
  *** master files is done successfully.
  llBld_Tmp = .F.
    
  =gfFlock("GLBATCH,&lcHeader,&lcDetail",.F.)
  
  *** Summarize {posted or unposted} transactions process done successfully. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02190B00000","DIALOG",lcSumType)
  _CUROBJ = OBJNUM(pbCancel)
ELSE
  
  *** Summarize {posted or unposted} transactions process not completed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02191B00000","DIALOG",lcSumType)
  _CUROBJ = OBJNUM(pbProceed)
ENDIF
