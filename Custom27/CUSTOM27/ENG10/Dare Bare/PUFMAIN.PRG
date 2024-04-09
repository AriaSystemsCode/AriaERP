*!**************************************************************************
*! Name      : PUFMAIN.PRG
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/24/2002
*! Purpose   : 
*! Reference : C#200376,1
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*!Modifications
*: C037345,1 ABD 01/20/2004 Made some modifications.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfToDoMbi
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/24/2002
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
*!C200376,1
FUNCTION lfToDoMbi

*B606744,1 AMH Create cursor since open the task list screen only once [Start]
CREATE CURSOR TODOMBI (X C(1))
*B606744,1 AMH [End]

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
      
      *C200413,1 AMH Look for prompet days forward and packward [Start]
      *IF cAdUsrSchd = 'N' .AND. EMPTY(dApproved) .AND.;
         BETWEEN(dRequired,gdSysDate,gdSysDate + SYUUSER.NPROMDAYS - 1)
      IF cAdUsrSchd = 'N' .AND. EMPTY(dApproved) .AND.;
         BETWEEN(dRequired,gdSysDate - SYUUSER.NPROMDAYS + 1,gdSysDate + SYUUSER.NPROMDAYS - 1)
      *C200413,1 AMH [End]
      
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
                CPO        WITH STYCRPLN.PO,;
                CSTYPTH    WITH STYCRPLN.STYLE,;
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

*!*************************************************************
*! Name      : lfUpdStyCr
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/28/2002
*! Purpose   : Update the style critical path file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfUpdStyCr()
*!*************************************************************
*!C200376,1
FUNCTION lfUpdStyCr

PRIVATE lnAlias
lnAlias = SELECT(0)
=gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPLN','SH')
=gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
SELECT STYCRPLN


IF SEEK(PADR(SUBSTR(m.Style,1,lnMajorLen),19)+'S'+m.Order)
  *C037345,1 ABD - let the update, update for the new item for type '5'. [Begin]
  *SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
  *                PADR(SUBSTR(m.Style,1,lnMajorLen),19)+'S'+m.Order;
  *            FOR CLINKTO $ '12' .AND. EMPTY(CPO)
  SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
                  PADR(SUBSTR(m.Style,1,lnMajorLen),19)+'S'+m.Order;
              FOR CLINKTO $ '125' .AND. EMPTY(CPO)

    *C037345,1 ABD - [End]
          
    *C200413,1 AMH Update approved date for create PO only [Start]
    *REPLACE CPO       WITH lcPONumber,;
    *        DAPPROVED WITH gdSysDate
    *IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
    *  REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
    *ENDIF
    REPLACE CPO WITH lcPONumber
    IF CLINKTO = '1'
      REPLACE DAPPROVED WITH gdSysDate
      IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
        REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
      ENDIF
    ENDIF
    *C200413,1 AMH [End]
    
  ENDSCAN
ENDIF
USE IN SYSCHDUL
USE IN STYCRPLN
SELECT (lnAlias)
*--end of lfUpdStyCr.

*!*************************************************************
*! Name      : lfStyCrUpd
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/28/2002
*! Purpose   : Update the style critical path file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfStyCrUpd()
*!*************************************************************
*!C200376,1
FUNCTION lfStyCrUpd

PRIVATE lnAlias
lnAlias = SELECT(0)

*C200413,1 AMH Don't link with cutpick [Start]
*=gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPLN','SH')
*=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
*=gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
*SELECT CUTPICK
*IF SEEK('2'+POSHDR.PO)
*  SCAN REST WHILE TranCd+cTktNo+Style = '2'+POSHDR.PO
*    IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+'S'+ORDER,'STYCRPLN')
*      SELECT STYCRPLN
*      SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
*                      PADR(SUBSTR(CUTPICK.STYLE,1,lnMjrWid),19)+'S'+CUTPICK.ORDER;
*                  FOR CLINKTO = '2'
*        REPLACE DAPPROVED WITH ldRcvDate
*        IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
*          REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
*        ENDIF
*      ENDSCAN
*    ENDIF
*  ENDSCAN
*ENDIF
*USE IN SYSCHDUL
*USE IN CUTPICK
*USE IN STYCRPLN
=gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPCPO','SH')
=gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
SELECT STYCRPLN
IF SEEK(POSHDR.PO)
  *C037345,1 ABD - remark the next line. [Begin]
  *SCAN REST WHILE CPO = POSHDR.PO FOR CLINKTO = '2'  
  * REPLACE DAPPROVED WITH ldRcvDate
  SCAN REST WHILE CPO = POSHDR.PO FOR CLINKTO $ '25'  
    *C037345,1 ABD - [End]
    IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
      REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
    ENDIF
  ENDSCAN
ENDIF

*C037345,1 ABD - Made some modifications. [Begin]
*- Change for dare to bare. 
SELECT STYCRPLN
SET ORDER TO STYCRPLN
llCutUsd=.T.
IF !USED('CUTPICK')
  llCutUsd=.F.
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
ENDIF  
SELECT (lcTmpLine)
lcOldOrd1 = ORDER()
lnRec_No1 = RECNO()
*-- cstytype+po+style+STR(lineno,6)+TranCd
SET ORDER TO POSLN

SELECT POSLN
lcOldord2 = ORDER()
lnRec_No2 = RECNO()
*-- cstytype+po+style+STR(lineno,6)+TranCd
SET ORDER TO POSLN

SELECT CUTPICK
IF SEEK('2'+POSHDR.PO)
  SCAN REST WHILE TranCd+cTktNo+Style = '2'+POSHDR.PO
    IF SEEK(Poshdr.cStytype + Poshdr.Po + Style + Cordline + '2',lcTmpLine) .AND. ;
       SEEK(Poshdr.cStytype + Poshdr.Po + Style + Cordline + '1','POSLN')
    
      *-- When receiving in styles against a Purchase order -and on saving the receipt, if receipt is 
      *-- updated to warehouse "PROD", i.e. POSLN.CWARECODE = "PROD" and if there is a CUTPICK record 
      *-- linking the Style Purchase order to a sales order and that sales order has a Style Critical 
      *-- Path attached and one of the operations is linked to 'Production Samples Received' - then the 
      *-- Approved date of that operation should be updated with the PO receipt date.     This should not 
      *-- significantly slow down the procedure of saving style PO receipts. If there is already an approved 
      *-- date for the operation linked to this transaction then do not overwrite i.e. Only update if 
      *-- STYCRPLN.DAPPROVED is blank
      IF  ALLTRIM(&lcTmpLine..cWareCode) = "PROD" &&  the Old Condation &lcTmpLine..cWareCode = POSLN.cWareCode
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19),'STYCRPLN')
          SELECT STYCRPLN
          SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
                          PADR(SUBSTR(CUTPICK.STYLE,1,lnMjrWid),19);
                      FOR CLINKTO = '5' .AND. CpO = Poshdr.Po
            IF EMPTY(DAPPROVED)
              REPLACE DAPPROVED WITH ldRcvDate
            ENDIF
          ENDSCAN
        ENDIF
      
      ELSE

        *-- When receiving  styles against a Purchase order -and on saving the receipt, 
        *-- if the receipt was updated to warehouse <> "PROD" i.e. POSLN.CWARECODE <> "PROD" 
        *-- then if there is a CUTPICK record linking the Style Purchase order to a sales order 
        *-- and that sales order has a Style Critical Path attached and one or more of the operations 
        *-- is linked to 'Style Purchase order receipt' - then the Approved date of that operation should 
        *-- be updated with the PO receipt date.     This should not significantly slow down the procedure 
        *-- of saving style PO receipts.  If there is already an approved date for the operation linked to 
        *-- this transaction then do not overwrite i.e. Only update if STYCRPLN.DAPPROVED is blank  
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+'S','STYCRPLN')
          SELECT STYCRPLN
          SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
                          PADR(SUBSTR(CUTPICK.STYLE,1,lnMjrWid),19);
                      FOR CLINKTO = '2' .AND. CpO = Poshdr.Po
            IF EMPTY(DAPPROVED)
              REPLACE DAPPROVED WITH ldRcvDate
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
    ENDIF  
    SELECT CUTPICK
  ENDSCAN
ENDIF

IF !llCutUsd
  USE IN CUTPICK
ENDIF

SELECT (lcTmpLine)

SET ORDER TO &lcOldOrd1
IF BETWEEN(lnRec_No1,1,RECCOUNT())
  GOTO lnRec_No1
ENDIF

SELECT POSLN
SET ORDER TO &lcOldOrd2
IF BETWEEN(lnRec_No2,1,RECCOUNT())
  GOTO lnRec_No2
ENDIF
*C037345,1 ABD - [End]

USE IN SYSCHDUL
USE IN STYCRPLN
*C200413,1 AMH [End]

SELECT (lnAlias)
*--end of lfStyCrUpd.

*!*************************************************************
*! Name      : lfStyCrMat
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/28/2002
*! Purpose   : Update the style critical path file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfStyCrMat()
*!*************************************************************
*!C200376,1
FUNCTION lfStyCrMat

PRIVATE lnAlias,lnMjrWid
lnAlias = SELECT(0)
lnMjrWid = LEN(gfItemMask('PM'))

*C200413,1 AMH Don't link with C200200 [Start]
*=gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPLN','SH')
*=gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'MAPOALO','SH')
*=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
*=gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
*SELECT MAPOALO
*IF SEEK(POFHDR.POMAT)
*  SCAN REST WHILE PoMat+Fabric+Color+Order = POFHDR.POMAT
*    IF SEEK('O'+ORDER,'ORDLINE')
*      SELECT ORDLINE
*      SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+MAPOALO.ORDER
*        IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+'S'+ORDER,'STYCRPLN')
*          SELECT STYCRPLN
*          SCAN REST WHILE Style+cStyType+Po+cCrpTmpCod+STR(nLineNo,4) =;
*                          PADR(SUBSTR(ORDLINE.STYLE,1,lnMjrWid),19)+'S'+ORDLINE.ORDER;
*                      FOR CLINKTO = '4' .AND. CPO = POFHDR.POMAT
*            REPLACE DAPPROVED WITH ldDate
*            IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
*              REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
*            ENDIF
*          ENDSCAN
*        ENDIF
*      ENDSCAN
*    ENDIF
*  ENDSCAN
*ENDIF
*USE IN SYSCHDUL
*USE IN ORDLINE
*USE IN MAPOALO
*USE IN STYCRPLN
=gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPCPO','SH')
=gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
SELECT STYCRPLN
IF SEEK(POFHDR.POMAT)
  SCAN REST WHILE CPO = POFHDR.POMAT FOR CLINKTO = '4'
    REPLACE DAPPROVED WITH ldDate
    IF !EMPTY(cSeqNumber) .AND. SEEK(cSeqNumber,'SYSCHDUL')
      REPLACE SYSCHDUL.CCOMPLETED WITH 'Y'
    ENDIF
  ENDSCAN
ENDIF
USE IN SYSCHDUL
USE IN STYCRPLN
*C200413,1 AMH [End]

SELECT (lnAlias)
*--end of lfStyCrMat.

*!*************************************************************
*! Name      : lfToDoPUF
*! Developer : Ahmed Maher (AMH)
*! Date      : 08/12/2002
*! Purpose   : Browse spicefic files for PUF 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfToDoPUF()
*!*************************************************************
*!C200376,1
FUNCTION lfToDoPUF

lcBrFields = [ccont_id:H='ID',cpo:H='Tran. ID',Cust=IIF(EMPTY(CPO),LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER"),CSTYPTH) :H="Name":22,]+;
             [contact:16,cphone:H="Phone",lcSubject=IIF(EMPTY(CPO),csubject,CCRPTHOPR):H="Subject":30,]+;
             [F=IIF(EMPTY(CPO),LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"),CTASKRESN):H="Reason",]+;
             [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
             [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
*--end of lfToDoPUF
*!*************************************************************
*! Name      : lfTODOSCH
*! Developer : Abdou Elgendy (ABD)
*! Date      : 02/11/2004 
*! Purpose   : Update with all the operation.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfTODOSCH()
*!*************************************************************
*:
FUNCTION lfTODOSCH
PRIVATE lnAlias , llOpenStyl , llOpenSyhd , llOpenOrdh , llOpenPosh , llOpenApvn , llopencust

STORE .F. TO llOpenStyl , llOpenSyhd , llOpenOrdh , llOpenPosh , llOpenApvn , llopencust
lnAlias =  SELECT(0)

CREATE CURSOR TODOMBI (X C(1))
IF !EMPTY(SYUUSER.CUSR_DCOM) .AND. !EMPTY(SYUUSER.CUSR_DMDL)
  llOpenStyl =gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPUSER','SH')
  llOpenSyhd =gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'CSEQNUM','SH')
  llOpenOrdh =gfOpenFile(gcDataDir+'ORDHDR',gcSysHome+'ORDHDR','SH')
  llOpenPosh =gfOpenFile(gcDataDir+'POSHDR',gcSysHome+'POSHDR','SH')
  llOpenApvn =gfOpenFile(gcDataDir+'APVENDOR',gcSysHome+'VENCODE','SH')
  llopencust  =gfOpenFile(gcDataDir+'CUSTOMER',gcSysHome+'CUSTOMER','SH')
  
  SELECT STYCRPLN
  IF SEEK(gcUser_ID)
    SCAN REST WHILE CUSER_ID+STYLE+CSTYTYPE+PO+CCRPTMPCOD+STR(NLINENO,4) = gcUser_ID
      IF cAdUsrSchd = 'N' .AND. EMPTY(dApproved) .AND.;
         BETWEEN(dRequired,gdSysDate - SYUUSER.NPROMDAYS + 1,gdSysDate + SYUUSER.NPROMDAYS - 1)
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
                CPO        WITH STYCRPLN.PO,;
                CSTYPTH    WITH STYCRPLN.STYLE,;
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

  IF llOpenStyl
    USE IN STYCRPLN
  ENDIF
  IF llOpenSyhd
    USE IN SYSCHDUL
  ENDIF
  IF llOpenOrdh   
    USE IN ORDHDR
  ENDIF
  IF llOpenPosh 
    USE IN POSHDR
  ENDIF
  IF llOpenApvn 
  USE IN APVENDOR
  ENDIF
  IF llopencust  
    USE IN CUSTOMER
  ENDIF
ENDIF

SELECT (lnAlias)
*--end of lfTODOSCH
*!*************************************************************
