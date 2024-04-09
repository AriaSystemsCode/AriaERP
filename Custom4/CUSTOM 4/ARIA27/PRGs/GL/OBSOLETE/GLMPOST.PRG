*:************************************************************************
*:
*: Procedure file: GLMPOST.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:
*:      Documented at   /  /
*:************************************************************************
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*
PARAMETERS lcCallProg


IF !USED('GLBATCH')
  SELECT 0
  USE &gcSysHome.GLBATCH ORDER TAG BATCHNO
ENDIF

IF !USED('GLTRNSHD')
  SELECT 0
  USE &gcSysHome.GLTRNSHD ORDER TAG BATCHTRN
ENDIF

IF !USED('GLTRNSDT')
  SELECT 0
  USE &gcSysHome.GLTRNSDT ORDER TAG ACCTCODE
ENDIF

IF !USED('GLACCHAR')
  SELECT 0
  USE &gcSysHome.GLACCHAR ORDER TAG ACCTCODE
ENDIF

IF !USED('GLACBALS')
  SELECT 0
  USE &gcSysHome.GLACBALS ORDER TAG ACCYRPRD
ENDIF

*E300692,1 Change file name from SYCFSPRD to FSPRD
*IF !USED('SYCFSPRD')
IF !USED('FSPRD')
*E300692,1 end
  SELECT 0
*E300692,1 Change file name and path from SYCFSPRD to FSPRD  
  *USE &gcSysHome.SYCFSPRD ORDER TAG COMFYRPRDI
  USE &gcDataDir.FSPRD ORDER TAG COMFYRPRDI
*E300692,1 end  
ENDIF

*** Get the segments sizes & positions in array laSegment ***
*** to calculate retained earnings acc. or suspense acc.  ***
DECLARE laSegment[lnAryLen,2]

lnCont    = 1
lnStrtPos = 1
SELECT GLACCODS
SCAN FOR !EMPTY(NACSSEGNO)
  laSegment [lnCont,1] = lnStrtPos
  laSegment [lnCont,2] = GLACCODS.NACSSIZE
  lnStrtPos            = lnStrtPos + GLACCODS.NACSSIZE + 1
  lnCont               = lnCont + 1 
ENDSCAN

*** Put in the account mask zeros to prepare the ***
*** account to seek in the chart of accounts.... ***
lcTmpAcCod = lcAcsMask

FOR lnCont = 1 TO ALEN(laSegment,1)
  lcTmpAcCod = STUFF(lcTmpAcCod,laSegment[lnCount,1],laSegment[lnCount,2],;
                     REPLICATE('0',laSegment[lnCount,2]))
ENDFOR

*** See if the retained earnings account major is exist ***
*** or not in the setup file, if its exist check in the ***
*** chart of accounts...
SELECT GLSETUP
IF EMPTY(GLSETUP.CSETRETMJ)
  *** You have to setup the retained earnings account ***
  *** major in the GL setup before start posting.     ***
  *** <  Ok  > ***
  =gfModalGen("QRM326B000","DIALOG")
  RETURN
ELSE
  *** Add the retained earnings account to the ***
  *** mask to seek in the chart of accounts... ***
  lcTmpAcCod = STUFF(lcTmpAcCod,laSegment[1,1],laSegment[1,2],;
                     ALLTRIM(GLSETUP.CSETRETMJ))
  SELECT GLACCHAR
  IF !SEEK(lcTmpAcCod,"GLACCHAR")
    *** The default retained earnings account not ***
    *** exist in the chart of accounts file.  You ***
    *** have to add the default retained earnings ***
    *** account before start posting.             ***
    *** <  Ok  > ***
    =gfModalGen("QRM327B000","DIALOG")
    RETURN
  ENDIF
ENDIF

*** See if  the suspense account major is exist or not ***
*** in the setup file, if its exist check in the chart ***
*** of accounts...
SELECT GLSETUP
IF EMPTY(GLSETUP.CSETSUSMJ)
  *** You have to setup the suspense account major in ***
  *** the GL setup before start posting.              ***
  *** <  Ok  > ***
  =gfModalGen("QRM328B000","DIALOG")
  RETURN
ELSE
  *** Add the suspense account to the mask to ***
  *** seek in the chart of accounts...        ***
  lcTmpAcCod = STUFF(lcTmpAcCod,laSegment[1,1],laSegment[1,2],;
                     ALLTRIM(GLSETUP.CSETSUSMJ))
  SELECT GLACCHAR
  IF !SEEK(lcTmpAcCod,"GLACCHAR")
    *** The default suspense account not exist in ***
    *** the chart of accounts file .  You have to ***
    *** add the  default suspense  account before ***
    *** start posting....                         ***
    *** <  Ok  > ***
    =gfModalGen("QRM329B000","DIALOG")
    RETURN
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvTrPoDat
*!
*!**************************************************************************
*
FUNCTION lfvTrPoDat
*** Function to Validate Transaction Posting Date. ***

IF lcCallProg <> 'Y' .AND. GLSETUP.LSETPBBBD .AND. ;
   GLTRNSHD.DTRNPDATE < GLSETUP.DSETBBDAT
  
  lcTmpStr1 = IIF(lcCallProg <> 'T',;
                  'Batch '+GLBATCH.CBATCHNO+' will not be posted',;
                  'Transaction '+GLTRNSHD.CTRANNO+' will not be posted')
  lcTmpStr2 = GLTRNSHD.CTRANNO+'|'+lcTmpStr1
  
  *** Transaction {GLTRNSHD.CTRANNO} posting date falls ***
  *** before the beginning balances date {lcTmpStr1}... ***
  *** <  Ok  > ***
  =gfModalGen("QRM333B000","DIALOG",lcTmpStr2)
  RETURN .F.
ENDIF


*E300692,1 Change file name from SYCFSPRD to FSPRD
*SELECT SYCFSPRD
SELECT FSPRD
*E300692,1 end
GO TOP
IF !EOF()
*E300692,1 Change file name from SYCFSPRD to FSPRD  
  *IF SYCFSPRD.LFSPLOCKS .AND. lcCallProg <> 'B'
  IF FSPRD.LFSPLOCKS .AND. lcCallProg <> 'B'
*E300692,1 end  
    lcTmpStr1 = GLTRNSHD.CTRNPYP+"/"+GLTRNSHD.CTRNPPRD
    lcTmpStr2 = IIF(lcCallProg <> 'T',;
                    'Batch '+GLBATCH.CBATCHNO+' will not be posted',;
                    'Transaction '+GLTRNSHD.CTRANNO+' will not be posted')
    lcTmpStr3 = lcTmpStr1+'|'+GLTRNSHD.CTRANNO+'|'+lcTmpStr2
    *** Period {lcTmpStr1} is locked.  You cannot post ***
    *** any transactions to this  period.  Transaction ***
    *** {GLTRNSHD.CTRANNO} posting  date falls in this ***
    *** period. lcTmpStr2...
    *** <  Ok  > ***
    =gfModalGen("QRM334B000","DIALOG",lcTmpStr3)
    RETURN .F.
  ENDIF
ELSE
  lcTmpStr1 = GLTRNSHD.CTRNPYP+"/"+GLTRNSHD.CTRNPPRD
  lcTmpStr2 = IIF(lcCallProg = 'B','Posting processing is terminated',;
                  IIF(lcCallProg <> 'T',;
                  'Batch '+GLBATCH.CBATCHNO+' will not be posted',;
                  'Transaction '+GLTRNSHD.CTRANNO+' will not be posted'))
  lcTmpStr3 = lcTmpStr1+'|'+lcTmpStr2
  *** Period {lcTmpStr1} not found in the fiscal year ***
  *** details.  {lcTmpStr2}.
  *** <  Ok  > ***
  =gfModalGen("QRM335B000","DIALOG",lcTmpStr3)
  RETURN .F.
ENDIF
