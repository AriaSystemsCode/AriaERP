*:************************************************************************
*: Program file  : ARCHACC.PRG
*: Program desc. : Customer Account Duplication for THE18
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR2
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 06/28/2004
*: Entry #       : C120009,1  
*:************************************************************************
* Modifications
*:***************************************************************************

* Notes : 
* This program accepts Acc1 and Acc2 ,  it copies all Acc1 data in Aria database to Acc2
*: In the file "ARCUSHST" Sum the values of the two customers into one of them.

* File ObjLink : use the field "COBJLINK"  with ObjLink.COBJLNKTYP = 'A'
* File NotePad : use the field "key"       with NotePad.Type = 'A'

* If a store found in cust1 check it is found in Cust2 OR add it

* For field Poshdr.Account and Posln.Account , check it is not empty befor updating

*- The field Credit.Store  does not represent a store , it is a refrence field


DIMENSION laData[2],laUserList[1],laOpenFl[1]
laData = ' '
lcStatcOrd = ORDER('SYUSTATC')

SET REPROCESS TO 20

*- Open needed files 
=lfOpenFls()

llContinue = .T.

*- If more than one user then log off
IF llContinue .AND. !lfChkUsrs()
  llContinue = .F.
ENDIF

*- If some files can not be locked , do not continue
IF llContinue .AND. !lfChckFls()
  llContinue = .F.
ENDIF 

*- Save current keys
PUSH KEY
ON KEY LABEL ALT+C CLEAR READ
ON KEY LABEL ESC CLEAR READ

IF llContinue
  DO (gcScrDir+'AR\ARCHACC.SPR')
ENDIF  

*- restore keys
POP KEY

*- Close Opened files
=lfClose()

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2004
*:* Purpose     : Open needed files 
*:***************************************************************************
FUNCTION lfOpenFls
IF FILE(gcDataDir+'CUSTOMER'+'.DBF')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER' ,'SH')
ENDIF  
IF FILE(gcSysHome+'SYDFLFLD'+'.DBF')
  =gfOpenFile(gcSysHome+'SYDFLFLD','CFLD_NAME','SH')
ENDIF  
IF FILE(gcDataDir+'ObjLink' +'.DBF')
  =gfOpenFile(gcDataDir+'ObjLink' ,'OBJLNKTY' ,'SH')    && Key : COBJLNKTYP+COBJLINK
ENDIF  
IF FILE(gcDataDir+'NOTEPAD' +'.DBF')
  =gfOpenFile(gcDataDir+'NOTEPAD' ,'NOTEPAD'  ,'SH')    && Key : TYPE+KEY
ENDIF  
IF FILE(gcSysHome+'SYSCHDUL'+'.DBF')
  =gfOpenFile(gcSysHome+'SYSCHDUL','SCHACCT'  ,'SH')
ENDIF  
IF FILE(gcDataDir+'CONTACT' +'.DBF')
  =gfOpenFile(gcDataDir+'CONTACT' ,'CONTACT'  ,'SH')
ENDIF  
IF FILE(gcDataDir+'ARCUSHST'+'.DBF')
  =gfOpenFile(gcDataDir+'ARCUSHST','ACTHST'   ,'SH')
ENDIF  
IF FILE(gcDataDir+'STYINVJL'+'.DBF')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL' ,'SH')
ENDIF  
IF FILE(gcDataDir+'APPAYMNT.DBF')
  =gfOpenFile(gcDataDir+'APPAYMNT','TYPCLNO' ,'SH')
ENDIF  

*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfvAccount
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2004
*:* Purpose     : Valid function for account selection
*:***************************************************************************
FUNCTION lfvAccount
*C120009,3  TMI [Start] Add a second parameter for the browse
*PARAMETERS lcVar
*IF !EMPTY(&lcVar) .AND. !SEEK('M'+&lcVar,'CUSTOMER')
PARAMETERS lcVar,llBrowse
IF llBrowse .OR. (!EMPTY(&lcVar) .AND. !SEEK('M'+&lcVar,'CUSTOMER'))
*C120009,3  TMI [End  ] 
  xAccount = &lcVar
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  &lcVar = xAccount
  *C120009,3  TMI [Start] refresh lcVar field
  SHOW GET &lcVar
  *C120009,3  TMI [End  ] 
ENDIF

IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. laData[1] = laData[2]
  laData[2] =  ' '
ENDIF

lcProsStat = IIF( !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) , 'ENABLE' , 'DISABLE' )
SHOW GET pbPorceed &lcProsStat
=lfRefresh('ARCHACC')

*-- end of lfvAccount.

*:**************************************************************************
*:* Name        : lfvProceed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2004
*:* Purpose     : Move all transaction from account 1 to account 2
*:***************************************************************************
*:* Called from : ARCHACC.SPR screen
*:***************************************************************************
FUNCTION lfvProceed
PRIVATE lnFl
lcMsg = 'Are you sure to move all transactions of the account code &laData[1] to the account ' +;
        'code &laData[2] and remove the account &laData[1] completely from the system ?'
IF gfModalGen('INM00000B00012',.F.,.F.,.F.,lcMsg) <> 1
  RETURN
ENDIF

DIMENSION laStru[1,4]
laStru = ''
FOR lnFl = 1 TO ALEN(laOpenFl)
  IF ! laOpenFl[lnFl] $ 'CUSTOMER, ARCUSHST'  
    WAIT WINDOW NOWAIT 'Updating file : '+laOpenFl[lnFl]
    SELECT laOpenFl[lnFl]
    =AFIELDS(laStru)
    llStoreFnd = ( ASCAN(laStru,'STORE') > 0 )
    GO TOP
    SCAN FOR ACCOUNT == PADR(laData[1],5)      
      REPLACE ACCOUNT WITH laData[2]
      IF ! laOpenFl[lnFl] $ 'CREDIT  |ARHIST  '
      IF llStoreFnd
        =SEEK('M'+laData[2]+&laOpenFl[lnFl]..STORE,'CUSTOMER')
        SCATTER MEMVAR MEMO
        IF !EMPTY(&laOpenFl[lnFl]..STORE) .AND. !SEEK('S'+laData[2]+&laOpenFl[lnFl]..STORE,'CUSTOMER')
          M.TYPE = 'S'
          M.STORE = &laOpenFl[lnFl]..STORE
          INSERT INTO CUSTOMER FROM MEMVAR
        ENDIF
      ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDFOR

*- Update files ObjLink , NotePad files
* File ObjLink use the field : COBJLINK  with ObjLink.COBJLNKTYP = 'A'
IF USED('OBJLINK')
  WAIT WINDOW NOWAIT 'Updating file : OBJLINK'
  SELECT OBJLINK
  IF SEEK('A'+PADR(laData[2],20),'OBJLINK')
    =SEEK('A'+PADR(laData[1],20),'OBJLINK')
    DELETE REST WHILE COBJLNKTYP+COBJLINK = 'A'+PADR(laData[1],20)
  ELSE  
    =SEEK('A'+PADR(laData[1],20),'OBJLINK')
    SCAN REST WHILE COBJLNKTYP+COBJLINK = 'A'+PADR(laData[1],20)
      REPLACE COBJLINK WITH laData[2]
    ENDSCAN
  ENDIF
ENDIF

* File NotePad use the field : key       with NotePad.Type = 'A'
IF USED('NOTEPAD')
  WAIT WINDOW NOWAIT 'Updating file : NOTEPAD'
  SELECT NOTEPAD
  IF SEEK('A'+PADR(laData[2],20),'NOTEPAD')
    =SEEK('A'+PADR(laData[1],20),'NOTEPAD')
    DELETE REST WHILE TYPE+KEY = 'A'+PADR(laData[1],20)
  ELSE
    =SEEK('A'+PADR(laData[1],20),'NOTEPAD')
    SCAN REST WHILE TYPE+KEY = 'A'+PADR(laData[1],20)
      REPLACE KEY WITH laData[2]
    ENDSCAN
  ENDIF
ENDIF

*C120009,3  TMI [Start] do not update the Sycchdul file
*-* *- Syschdul file
*-* IF USED('SYSCHDUL')
*-*   WAIT WINDOW 'Updating ' + 'SYSCHDUL' +' file.' NOWAIT
*-*   SELECT SYSCHDUL
*-*   REPLACE ALL cCont_Id with PADR(laData[2],8) FOR cCont_Id = PADR(laData[1],8)
*-* ENDIF
*C120009,3  TMI [End  ] 

*C120009,3  TMI [Start] do not update the contact 
*-* *- Contact file
*-* IF USED('CONTACT')
*-*   WAIT WINDOW 'Updating ' + 'CONTACT' +' file.' NOWAIT
*-*   SELECT CONTACT
*-*   REPLACE ALL cCont_Id with PADR(laData[2],8) FOR ALLTRIM(cCont_Id) == ALLTRIM(PADR(laData[1],8))
*-* ENDIF
*C120009,3  TMI [End  ] 

*- STYINVJL File
IF USED('STYINVJL')
  SELECT STYINVJL
  WAIT WINDOW 'Updating ' + 'STYINVJL' +' file.' NOWAIT
  SCAN FOR 'Sales order' $ REFERENCE .AND. laData[1] $ REFERENCE
    REPLACE REFERENCE WITH STRTRAN(REFERENCE,'CUST# '+laData[1],'CUST# '+laData[2])
  ENDSCAN
ENDIF

*- Update files : 'ARCUSHST,  '  
IF USED('ARCUSHST')
  IF SEEK(laData[1]+gcCurrYear,'ARCUSHST')
    SELECT ARCUSHST
    WAIT WINDOW 'Updating ' + ALIAS() +' file.' NOWAIT
    =AFIELDS(laArCusHst)
    FOR lnFsYear = -1 TO 1
      IF SEEK(laData[1]+STR(VAL(gcCurrYear)+lnFsYear,4),'ARCUSHST')
        * Save all fields values for 1st Account in memory variables
        SCATT MEMVAR 
        IF SEEK(laData[2]+STR(VAL(gcCurrYear)+lnFsYear,4),'ARCUSHST')
           *- Update 2nd Account fields
           =lfRplcVls()
        ELSE
          M.ACCOUNT = laData[2]
          INSERT INTO ARCUSHST FROM MEMVAR
        ENDIF
        *- Delete 1st Account fields
        =SEEK(laData[1]+STR(VAL(gcCurrYear)+lnFsYear,4),'ARCUSHST')
        DELETE
      ENDIF
    ENDFOR 
  ENDIF
ENDIF

IF USED('APPAYMNT')
  SELECT APPAYMNT
  WAIT WINDOW 'Updating ' + ALIAS() +' file.' NOWAIT
  GO TOP
  SCAN FOR APPAYMNT.CPAYCLNO = PADR(laData[1],8)
    REPLACE CPAYCLNO WITH PADR(laData[2],8)
  ENDSCAN
ENDIF

*- Update file  : 'CUSTOMER '
SELECT CUSTOMER
DELETE FOR ACCOUNT = laData[1]

laData = ' '
SHOW GET pbPorceed DISABLE
SHOW GET laData[1]
SHOW GET laData[2]
=lfRefresh('ARCHACC')
_CUROBJ = OBJNUM(laData[1])
WAIT CLEAR
*-- end of lfvProceed.

*:**************************************************************************
*:* Name        : lfChckFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2004
*:* Purpose     : Open file with lock
*:***************************************************************************
FUNCTION lfChckFls
PRIVATE lnFl
SELECT SYDFLFLD
=SEEK('ACCOUNT ','SYDFLFLD')
lnFl = 0
SCAN REST WHILE CFLD_NAME+STR(NFLD_POS) = 'ACCOUNT '
  lcFile = ALLTRIM(SYDFLFLD.CFILE_NAM)
  IF FILE(gcDataDir+lcFile+'.DBF') .OR. FILE(gcSysHome+lcFile+'.DBF')
    IF !USED(lcFile)
      IF FILE(gcDataDir+lcFile+'.DBF')
        USE (gcDataDir+lcFile) IN 0
      ELSE
        USE (gcSysHome+lcFile) IN 0
      ENDIF
    ENDIF
    SELECT (lcFile)
    lnFl = lnFl + 1
    DIMENSION laOpenFl[lnFl]
    laOpenFl[lnFl] = ALIAS()
    
    llLock = RLOCK()
    IF !llLock
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file &lcFile can not be locked , can not proceed.')
      RETURN .F.
    ENDIF
  ENDIF
ENDSCAN

*-- end of lfChckFls.

*:**************************************************************************
*:* Name        : lfChkUsrs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2004
*:* Purpose     : Check if there is any other user connecting aria
*:***************************************************************************
FUNCTION lfChkUsrs
PRIVATE llRet,laUserList
llRet = .T.       
DIMENSION laUserList[1]

SELECT CUSER_ID FROM (gcSysHome+"SYUSTATC");
       WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION='INI'+'OLDVARS' ;
            .AND. gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUserList
IF ALEN(laUserList,1) = 1
  USE (gcSysHome+"SYUSTATC") EXCLUSIVE IN SYUSTATC
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Other users are logged in , can not proceed')    
  llRet = .F.
ENDIF

RETURN llRet
*-- end of lfChkUsrs.

*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2004
*:* Purpose     : Reopen static file and close other open files
*:***************************************************************************
FUNCTION lfClose
*- Re Open static file shared to allow other users to log in
USE (gcSysHome+"SYUSTATC") SHARED IN SYUSTATC ORDER TAG &lcStatcOrd

IF TYPE('laOpenFl') = 'C'
  FOR lnFl = 1 TO ALEN(laOpenFl,1)
    IF !EMPTY(laOpenFl[lnFl]) .AND. USED(laOpenFl[lnFl])
      UNLOCK IN (laOpenFl[lnFl])
      USE IN (laOpenFl[lnFl])
    ENDIF
  ENDFOR
ENDIF  
*-- end of lfClose.

*:**************************************************************************
*:* Name        : lfRplcVls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/22/2004
*:* Purpose     : Replace values in the file ArcustHst
*:***************************************************************************
FUNCTION lfRplcVls
PRIVATE lnI
SELECT ArCusHst
FOR lnI = 1 TO ALEN(laArCusHst,1)
  IF laArCusHst[lnI,2] = 'N'
    REPLACE &laArCusHst[lnI,1]. WITH &laArCusHst[lnI,1]. + EVAL('M.'+laArCusHst[lnI,1])
  ENDIF
ENDFOR

RETURN

*-*        REPLACE NORDAMT    WITH NORDAMT    + M.NORDAMT   
*-*                NSLSAMT    WITH NSLSAMT    + M.NSLSAMT   
*-*                NDISAMT    WITH NDISAMT    + M.NDISAMT   
*-*                NCOGSAMT   WITH NCOGSAMT   + M.NCOGSAMT  
*-*                NRETAMT    WITH NRETAMT    + M.NRETAMT   
*-*                NORDQTY    WITH NORDQTY    + M.NORDQTY   
*-*                NSLSQTY    WITH NSLSQTY    + M.NSLSQTY   
*-*                NRETQTY    WITH NRETQTY    + M.NRETQTY   
*-*                NALLOW     WITH NALLOW     + M.NALLOW    
*-*                NPAYMENT   WITH NPAYMENT   + M.NPAYMENT  
*-*                NDRADJ     WITH NDRADJ     + M.NDRADJ    
*-*                NCRADJ     WITH NCRADJ     + M.NCRADJ    
*-*                NORDAMT01  WITH NORDAMT01  + M.NORDAMT01 
*-*                NSLSQTY01  WITH NSLSQTY01  + M.NSLSQTY01 
*-*                NRETQTY01  WITH NRETQTY01  + M.NRETQTY01 
*-*                NALLOW01   WITH NALLOW01   + M.NALLOW01  
*-*                NORDQTY01  WITH NORDQTY01  + M.NORDQTY01 
*-*                NORDAMT02  WITH NORDAMT02  + M.NORDAMT02 
*-*                NORDAMT03  WITH NORDAMT03  + M.NORDAMT03 
*-*                NORDAMT04  WITH NORDAMT04  + M.NORDAMT04 
*-*                NORDAMT05  WITH NORDAMT05  + M.NORDAMT05 
*-*                NORDAMT06  WITH NORDAMT06  + M.NORDAMT06 
*-*                NORDAMT07  WITH NORDAMT07  + M.NORDAMT07 
*-*                NORDAMT08  WITH NORDAMT08  + M.NORDAMT08 
*-*                NORDAMT09  WITH NORDAMT09  + M.NORDAMT09 
*-*                NORDAMT10  WITH NORDAMT10  + M.NORDAMT10 
*-*                NORDAMT11  WITH NORDAMT11  + M.NORDAMT11 
*-*                NORDAMT12  WITH NORDAMT12  + M.NORDAMT12 
*-*                NORDAMT13  WITH NORDAMT13  + M.NORDAMT13 
*-*                NSLSAMT01  WITH NSLSAMT01  + M.NSLSAMT01 
*-*                NSLSAMT02  WITH NSLSAMT02  + M.NSLSAMT02 
*-*                NSLSAMT03  WITH NSLSAMT03  + M.NSLSAMT03 
*-*                NSLSAMT04  WITH NSLSAMT04  + M.NSLSAMT04 
*-*                NSLSAMT05  WITH NSLSAMT05  + M.NSLSAMT05 
*-*                NSLSAMT06  WITH NSLSAMT06  + M.NSLSAMT06 
*-*                NSLSAMT07  WITH NSLSAMT07  + M.NSLSAMT07 
*-*                NSLSAMT08  WITH NSLSAMT08  + M.NSLSAMT08 
*-*                NSLSAMT09  WITH NSLSAMT09  + M.NSLSAMT09 
*-*                NSLSAMT10  WITH NSLSAMT10  + M.NSLSAMT10 
*-*                NSLSAMT11  WITH NSLSAMT11  + M.NSLSAMT11 
*-*                NSLSAMT12  WITH NSLSAMT12  + M.NSLSAMT12 
*-*                NSLSAMT13  WITH NSLSAMT13  + M.NSLSAMT13 
*-*                NDISAMT01  WITH NDISAMT01  + M.NDISAMT01 
*-*                NDISAMT02  WITH NDISAMT02  + M.NDISAMT02 
*-*                NDISAMT03  WITH NDISAMT03  + M.NDISAMT03 
*-*                NDISAMT04  WITH NDISAMT04  + M.NDISAMT04 
*-*                NDISAMT05  WITH NDISAMT05  + M.NDISAMT05 
*-*                NDISAMT06  WITH NDISAMT06  + M.NDISAMT06 
*-*                NDISAMT07  WITH NDISAMT07  + M.NDISAMT07 
*-*                NDISAMT08  WITH NDISAMT08  + M.NDISAMT08 
*-*                NDISAMT09  WITH NDISAMT09  + M.NDISAMT09 
*-*                NDISAMT10  WITH NDISAMT10  + M.NDISAMT10 
*-*                NDISAMT11  WITH NDISAMT11  + M.NDISAMT11 
*-*                NDISAMT12  WITH NDISAMT12  + M.NDISAMT12 
*-*                NDISAMT13  WITH NDISAMT13  + M.NDISAMT13 
*-*                NCOGSAMT01 WITH NCOGSAMT01 + M.NCOGSAMT01
*-*                NCOGSAMT02 WITH NCOGSAMT02 + M.NCOGSAMT02
*-*                NCOGSAMT03 WITH NCOGSAMT03 + M.NCOGSAMT03
*-*                NCOGSAMT04 WITH NCOGSAMT04 + M.NCOGSAMT04
*-*                NCOGSAMT05 WITH NCOGSAMT05 + M.NCOGSAMT05
*-*                NCOGSAMT06 WITH NCOGSAMT06 + M.NCOGSAMT06
*-*                NCOGSAMT07 WITH NCOGSAMT07 + M.NCOGSAMT07
*-*                NCOGSAMT08 WITH NCOGSAMT08 + M.NCOGSAMT08
*-*                NCOGSAMT09 WITH NCOGSAMT09 + M.NCOGSAMT09
*-*                NCOGSAMT10 WITH NCOGSAMT10 + M.NCOGSAMT10
*-*                NCOGSAMT11 WITH NCOGSAMT11 + M.NCOGSAMT11
*-*                NCOGSAMT12 WITH NCOGSAMT12 + M.NCOGSAMT12
*-*                NCOGSAMT13 WITH NCOGSAMT13 + M.NCOGSAMT13
*-*                NRETAMT01  WITH NRETAMT01  + M.NRETAMT01 
*-*                NRETAMT02  WITH NRETAMT02  + M.NRETAMT02 
*-*                NRETAMT03  WITH NRETAMT03  + M.NRETAMT03 
*-*                NRETAMT04  WITH NRETAMT04  + M.NRETAMT04 
*-*                NRETAMT05  WITH NRETAMT05  + M.NRETAMT05 
*-*                NRETAMT06  WITH NRETAMT06  + M.NRETAMT06 
*-*                NRETAMT07  WITH NRETAMT07  + M.NRETAMT07 
*-*                NRETAMT08  WITH NRETAMT08  + M.NRETAMT08 
*-*                NRETAMT09  WITH NRETAMT09  + M.NRETAMT09 
*-*                NRETAMT10  WITH NRETAMT10  + M.NRETAMT10 
*-*                NRETAMT11  WITH NRETAMT11  + M.NRETAMT11 
*-*                NRETAMT12  WITH NRETAMT12  + M.NRETAMT12 
*-*                NRETAMT13  WITH NRETAMT13  + M.NRETAMT13 
*-*                NORDQTY02  WITH NORDQTY02  + M.NORDQTY02 
*-*                NORDQTY03  WITH NORDQTY03  + M.NORDQTY03 
*-*                NORDQTY04  WITH NORDQTY04  + M.NORDQTY04 
*-*                NORDQTY05  WITH NORDQTY05  + M.NORDQTY05 
*-*                NORDQTY06  WITH NORDQTY06  + M.NORDQTY06 
*-*                NORDQTY07  WITH NORDQTY07  + M.NORDQTY07 
*-*                NORDQTY08  WITH NORDQTY08  + M.NORDQTY08 
*-*                NORDQTY09  WITH NORDQTY09  + M.NORDQTY09 
*-*                NORDQTY10  WITH NORDQTY10  + M.NORDQTY10 
*-*                NORDQTY11  WITH NORDQTY11  + M.NORDQTY11 
*-*                NORDQTY12  WITH NORDQTY12  + M.NORDQTY12 
*-*                NORDQTY13  WITH NORDQTY13  + M.NORDQTY13 
*-*                NSLSQTY02  WITH NSLSQTY02  + M.NSLSQTY02 
*-*                NSLSQTY03  WITH NSLSQTY03  + M.NSLSQTY03 
*-*                NSLSQTY04  WITH NSLSQTY04  + M.NSLSQTY04 
*-*                NSLSQTY05  WITH NSLSQTY05  + M.NSLSQTY05 
*-*                NSLSQTY06  WITH NSLSQTY06  + M.NSLSQTY06 
*-*                NSLSQTY07  WITH NSLSQTY07  + M.NSLSQTY07 
*-*                NSLSQTY08  WITH NSLSQTY08  + M.NSLSQTY08 
*-*                NSLSQTY09  WITH NSLSQTY09  + M.NSLSQTY09 
*-*                NSLSQTY10  WITH NSLSQTY10  + M.NSLSQTY10 
*-*                NSLSQTY11  WITH NSLSQTY11  + M.NSLSQTY11 
*-*                NSLSQTY12  WITH NSLSQTY12  + M.NSLSQTY12 
*-*                NSLSQTY13  WITH NSLSQTY13  + M.NSLSQTY13 
*-*                NRETQTY02  WITH NRETQTY02  + M.NRETQTY02 
*-*                NRETQTY03  WITH NRETQTY03  + M.NRETQTY03 
*-*                NRETQTY04  WITH NRETQTY04  + M.NRETQTY04 
*-*                NRETQTY05  WITH NRETQTY05  + M.NRETQTY05 
*-*                NRETQTY06  WITH NRETQTY06  + M.NRETQTY06 
*-*                NRETQTY07  WITH NRETQTY07  + M.NRETQTY07 
*-*                NRETQTY08  WITH NRETQTY08  + M.NRETQTY08 
*-*                NRETQTY09  WITH NRETQTY09  + M.NRETQTY09 
*-*                NRETQTY10  WITH NRETQTY10  + M.NRETQTY10 
*-*                NRETQTY11  WITH NRETQTY11  + M.NRETQTY11 
*-*                NRETQTY12  WITH NRETQTY12  + M.NRETQTY12 
*-*                NRETQTY13  WITH NRETQTY13  + M.NRETQTY13 
*-*                NALLOW02   WITH NALLOW02   + M.NALLOW02  
*-*                NALLOW03   WITH NALLOW03   + M.NALLOW03  
*-*                NALLOW04   WITH NALLOW04   + M.NALLOW04  
*-*                NALLOW05   WITH NALLOW05   + M.NALLOW05  
*-*                NALLOW06   WITH NALLOW06   + M.NALLOW06  
*-*                NALLOW07   WITH NALLOW07   + M.NALLOW07  
*-*                NALLOW08   WITH NALLOW08   + M.NALLOW08  
*-*                NALLOW09   WITH NALLOW09   + M.NALLOW09  
*-*                NALLOW10   WITH NALLOW10   + M.NALLOW10  
*-*                NALLOW11   WITH NALLOW11   + M.NALLOW11  
*-*                NALLOW12   WITH NALLOW12   + M.NALLOW12  
*-*                NALLOW13   WITH NALLOW13   + M.NALLOW13  
*-*                NPAYMENT01 WITH NPAYMENT01 + M.NPAYMENT01
*-*                NPAYMENT02 WITH NPAYMENT02 + M.NPAYMENT02
*-*                NPAYMENT03 WITH NPAYMENT03 + M.NPAYMENT03
*-*                NPAYMENT04 WITH NPAYMENT04 + M.NPAYMENT04
*-*                NPAYMENT05 WITH NPAYMENT05 + M.NPAYMENT05
*-*                NPAYMENT06 WITH NPAYMENT06 + M.NPAYMENT06
*-*                NPAYMENT07 WITH NPAYMENT07 + M.NPAYMENT07
*-*                NPAYMENT08 WITH NPAYMENT08 + M.NPAYMENT08
*-*                NPAYMENT09 WITH NPAYMENT09 + M.NPAYMENT09
*-*                NPAYMENT10 WITH NPAYMENT10 + M.NPAYMENT10
*-*                NPAYMENT11 WITH NPAYMENT11 + M.NPAYMENT11
*-*                NPAYMENT12 WITH NPAYMENT12 + M.NPAYMENT12
*-*                NPAYMENT13 WITH NPAYMENT13 + M.NPAYMENT13
*-*                NDRADJ01   WITH NDRADJ01   + M.NDRADJ01  
*-*                NDRADJ02   WITH NDRADJ02   + M.NDRADJ02  
*-*                NDRADJ03   WITH NDRADJ03   + M.NDRADJ03  
*-*                NDRADJ04   WITH NDRADJ04   + M.NDRADJ04  
*-*                NDRADJ05   WITH NDRADJ05   + M.NDRADJ05  
*-*                NDRADJ06   WITH NDRADJ06   + M.NDRADJ06  
*-*                NDRADJ07   WITH NDRADJ07   + M.NDRADJ07  
*-*                NDRADJ08   WITH NDRADJ08   + M.NDRADJ08  
*-*                NDRADJ09   WITH NDRADJ09   + M.NDRADJ09  
*-*                NDRADJ10   WITH NDRADJ10   + M.NDRADJ10  
*-*                NDRADJ11   WITH NDRADJ11   + M.NDRADJ11  
*-*                NDRADJ12   WITH NDRADJ12   + M.NDRADJ12  
*-*                NDRADJ13   WITH NDRADJ13   + M.NDRADJ13  
*-*                NCRADJ01   WITH NCRADJ01   + M.NCRADJ01  
*-*                NCRADJ02   WITH NCRADJ02   + M.NCRADJ02  
*-*                NCRADJ03   WITH NCRADJ03   + M.NCRADJ03  
*-*                NCRADJ04   WITH NCRADJ04   + M.NCRADJ04  
*-*                NCRADJ05   WITH NCRADJ05   + M.NCRADJ05  
*-*                NCRADJ06   WITH NCRADJ06   + M.NCRADJ06  
*-*                NCRADJ07   WITH NCRADJ07   + M.NCRADJ07  
*-*                NCRADJ08   WITH NCRADJ08   + M.NCRADJ08  
*-*                NCRADJ09   WITH NCRADJ09   + M.NCRADJ09  
*-*                NCRADJ10   WITH NCRADJ10   + M.NCRADJ10  
*-*                NCRADJ11   WITH NCRADJ11   + M.NCRADJ11  
*-*                NCRADJ12   WITH NCRADJ12   + M.NCRADJ12  
*-*                NCRADJ13   WITH NCRADJ13   + M.NCRADJ13 

*-- end of lfRplcVls.