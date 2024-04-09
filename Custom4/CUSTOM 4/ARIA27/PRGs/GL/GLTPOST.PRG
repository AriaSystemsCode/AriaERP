*:************************************************************************
*:
*: Procedure file: GLTPOST.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts:
*:
*:    lfActBrow   *** To activate the browse ***
*:    lfwBrowhen  *** When browse function ***
*:    lfvApprov   *** To approve or disapprove transaction ***
*:    lfvAprOne   *** To approve or disapprove specific transaction ***
*:    lfvAprAll   *** To approve all transactions ***
*:    lfvAprNon   *** To disapprove all transactions ***
*:    lfvInvert   *** Change status for each transaction ***
*:    lfvPost     *** To post approved transactions ***
*:    lfvPrint    *** To print ***
*:    lpEscape    *** To clean the traping keys and close screens ***
*:    lpTab       *** To moves between the push buttons and browse window ***
*:    lpShiftTab  *** To moves between the push buttons and browse window ***
*:    lfvSJ       *** To Select the active source journals from list ***
*:               
*:      Documented 12/28/1993
*B601099,1 M.H 06/27/96 Hide the pbPrint by changing the comment from #OBJDISP = '!llDumyPost' TO ".F."
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*:************************************************************************
*
PARAMETERS lcReportFi
llDumyPost = IIF(TYPE('lcReportFi')='C',.T.,.F.)

*** Arrays used by mover :
DECLARE laSSorJou   [1,1]    && source array of subjournals names
DECLARE laTSorJou   [1,1]    && target array of subjournals names 

*** Arrays used to display status for each transation :
DECLARE laTranStat  [3]
DECLARE laTranType  [2]

lcTrNumber    = ''   && var. to store the active transaction in browse
lcSrcjrnl     = ''   && string to store the active subjournal(s) names
lc_ToPost     = ''
lnRecNo       = 0 

IF !gfSetup() 
  RETURN
ENDIF  


IF llDumyPost
  lcText  = 'Inclu\<de'
  lcText1 = 'Include a\<ll'
  lcText2 = 'Include \<none'
ELSE
  lcText  = '\<Approve'
  lcText1 = 'Approve a\<ll'
  lcText2 = 'Approve \<none'
ENDIF


*** the string that contaning the scope of the valid posting years  
lcFsWindow    = STR(VAL(lcCurr_yer)-1,4)+" "+lcCurr_yer+" "+;
                STR(VAL(lcCurr_yer)+1,4)

*** Create array of 1 column ,each element is the concatenation
*** of two fields (GLSUBJOR.cSrcjrnl,GLSUBJOR.cJorShDes)
SELECT cSrcjrnl+" "+cJorShDes From &gcDataDir.glSubJor ;
       INTO ARRAY laSSorJou

*** Check if there is no any subjournals in the GLSUBJOR
*** in this case display error massege "No available source journals."  
*** then exit form this program

IF ALEN(laSSorJou,1) = 1 .AND. TYPE('laSSorJou[1]') = 'L'
  laSSorJou = " "
  laSSorJou[1,1] = lcSJ_Def
ELSE
  IF ASCAN(laSSorJou,lcSJ_Def) = 0
    DIMENSION laSSorJou[ALEN(laSSorJou,1)+1,1]
    =AINS(laSSorJou,1)
    laSSorJou[1,1] = lcSJ_Def
  ENDIF  
ENDIF

*** Create array with the same number of elements in source array.
*** then copy content from source array to target array.
DECLARE laTSoJou [ALEN(laSSorJou)]
=ACOPY(laSSorJou,laTSorJou)

*** collect all source journals names in the string lcSrcjrnl
FOR lnCounter = 1 TO ALEN(laSSorJou)
  lcSrcjrnl = lcSrcjrnl + LEFT(laSSorJou[lnCounter],3) 
ENDFOR

IF llDumyPost
lc_TBrowt    = "Dummy Transaction Posting" &&  initial the window header.
ELSE  
lc_TBrowt    = "Transaction Posting"       &&  initial the window header.
ENDIF



lcScope      =  " VAL(cBatchNo) = 0 " + ;
                ".AND.   cSrcjrnl $ lcSrcjrnl "  + ;
                ".AND. ((cTrnStat $ 'UA' ) "  + ;
                ".OR.   (cStandard = 'N' .AND. cTrnStat $ 'UAO' ))"  

SELECT GLTRNSHD

*** searches the DBF (glTrnsHd) for the first record 
*** that matches a given expression.
*** --> status  (glTrnsHd.cTrnStat ) , standard (glTrnsHd.cStandard ) 

LOCATE FOR &lcScope

IF FOUND()
  lc_ToPost   = gfTempName()
  
  *** places all current on key labels on a stack in memory.
  PUSH KEY

  *** deactivate control keys for browse window 
  *** ESCAPE, TAB, Ctrl+W, Ctrl+Q, Ctrl+Enter
  ON KEY LABEL ESC        DO  lpEscape  
  ON KEY LABEL TAB        DO  lpTab 
  ON KEY LABEL RIGHTARROW DO  lpTab
  ON KEY LABEL LEFTARROW  DO  lpShiftTab
  ON KEY LABEL BACKTAB    DO  lpShiftTab 
  ON KEY LABEL Ctrl+W      lnDummi = 1
  ON KEY LABEL Ctrl+Q      lnDummi = 1
  ON KEY LABEL Ctrl+ENTER  lnDummi = 1
  
  KEYBOARD CHR(49)      && clear keyboard buffer.
  SET CONFIRM OFF


  SELECT GLTRNSHD
  *E300683,5 Call *.SPR from screens directory
  * DO GlTPost.SPR 
  DO (gcScrDir + gcWinAppl + '\GlTPost.SPR')
  *E300683,5 end  
  SET CONFIRM ON
  
  *** restores on key labels that were placed on the stack with push key. 
  POP KEY
ELSE  
  *** if there is no batches to post.
  =gfModalGen("TRM02081B00000","Dialog")      
ENDIF

glQuitting  = .T.

RELEASE WINDOW (lc_TBrowt)

IF USED(lc_ToPost)
  USE IN ALIAS (lc_ToPost)
ENDIF
ERASE &gcWorkdir.&lc_ToPost..DBF
ERASE &gcWorkdir.&lc_ToPost..CDX

SELECT GLTRNSHD
*!**************************************************************************
*!
*!      Function:  lfActBrow   
*!
*!**************************************************************************
*
FUNCTION  lfActBrow

LOCATE FOR &lcScope
=lfwBrowhen()
lcObjState = IIF(!FOUND(),"DISABLE","ENABLE")
SHOW GET pbApprov  &lcObjState
SHOW GET pbAll     &lcObjState
SHOW GET pbNone    &lcObjState
SHOW GET pbInvert  &lcObjState
SHOW GET pbPrint   &lcObjState
SHOW GET pbPost    &lcObjState

lcFieldDis = IIF(llDumyPost,;
                "Included=IIF(nTrnIndic = 1,'   û','    ')", ;
                "Approved=IIF(cTrnStat= 'A','   û','    ')") 

lcBrString = "cDummi1=IIF(lcTrNumber=cTranno,'',' '):H=' ':F:V=lfKeyPressed(),"+;
           "cTranno:H='Trans. #':8,"+;
           "cSrcjrnl:H='S/J':3,cComp_Id:H='CO':3,"+;
           "&lcFieldDis"+;
           ",dTrnPDate:H='  Date':10,"+;
           "cTrnDesc:H='   Description':40"

DO CASE
  CASE _DOS
    BROWSE FIELDS &lcBrString;
           LOCK 0;
           NOAPPEND;
           NOCLEAR  ;
           NODELETE ;
           NOMENU   ;
           NOWAIT   ;
           SAVE     ;
           FREEZ cDummi1;
           FOR   &lcScope;
           TITLE lc_TBrowt ;
           WHEN lfwBrowhen();
           WINDOW glTPost0 IN WINDOW (gcBaseWind)

  CASE _WINDOWS
    BROWSE FIELDS &lcBrString;
           LOCK 0;
           NOAPPEND;
           NOCLEAR  ;
           NODELETE ;
           NOMENU   ;
           NOWAIT   ;
           SAVE     ;
           FONT "FOXFONT", 9 STYLE 'N';
           FREEZ cDummi1;
           FOR   &lcScope;
           TITLE lc_TBrowt ;
           WHEN lfwBrowhen();
           WINDOW glTPost0 IN WINDOW (gcBaseWind)
 ENDCASE

*!**************************************************************************
*!
*!      Function:  lfwBrowhen
*!
*!**************************************************************************
*
FUNCTION  lfwBrowhen

*** check if the status for the ( cTrnStat ) 
*** and change the prompt display
*** if A -->  display 'Approved ' Else display 'Disapprove'


IF llDumyPost 

  IF nTrnIndic = 1
    SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
  ELSE
    SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
  ENDIF

ELSE

  IF cTrnStat = 'A'
    SHOW GET pbApprov,1 PROMPT '\<Disapprove'
  ELSE
    SHOW GET pbApprov,1 PROMPT '\<Approve' 
  ENDIF

ENDIF

SHOW GETS

*!**************************************************************************
*!					
*!      Function:  lfKeyPressed
*!
*!**************************************************************************
*
FUNCTION  lfKeyPressed 

IF LASTKEY()=13 
  KEYBOARD CHR(31)
  =lfvAprOne()
ENDIF  


*!**************************************************************************
*!					
*!      Function:  lfvAprOne 
*!
*!**************************************************************************
*
FUNCTION  lfvAprOne 

*** check if the status for (cTrnStat) = 'A' --> approve 
*** that is mean the user want to disapprove this record.
*** then we will check the locking status, if there is no
*** user use this record, then change the status according to 
*** the diffrent between glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance


IF llDumyPost
  IF nTrnIndic = 1
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glTrnsHd.nTrnIndic WITH 0 
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
    ENDIF
  ELSE 
    IF glTrnsHd.cTrnPYr $ lcFsWindow
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glTrnsHd.nTrnIndic WITH 1
        =gfObj_Lock(.F.) 
        SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
      ENDIF
    ELSE
      =gfModalGen("TRM02083B00000","Dialog",IIF(llDumyPost,'include','approve'))
      *** The Batch posting year is out of the posting window.    
    ENDIF  
  ENDIF  
ELSE

  IF cTrnStat = 'A' 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glTrnsHd.cTrnStat WITH ;
              IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')
      =gfAdd_Info('glTrnsHd')
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT '\<Approve' 
    ENDIF
  ELSE 
    *** that is mean the user want to approve this record.  
    *** in this case must be check if the glTrnsHd.cTrnPYr 
    *** in the scope the posting window (previous,current,next)
    *** if !valid display "The Batch posting year is out of the posting window"
    *** if valid .. check the locking status, if there is no
    *** user use this record, then change the status to 'A' --> approve

    IF glTrnsHd.cTrnPYr $ lcFsWindow
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glTrnsHd.cTrnStat WITH  'A'
        =gfAdd_Info('glTrnsHd')
        =gfObj_Lock(.F.) 
        SHOW GET pbApprov,1 PROMPT '\<Disapprove'
      ENDIF
    ELSE
      =gfModalGen("TRM02083B00000","Dialog",IIF(llDumyPost,'include','approve'))
      *** The Batch posting year is out of the posting window.    
    ENDIF  
  ENDIF  
ENDIF  
SHOW GETS


*!**************************************************************************
*!
*!      Function:  lfvAprAll 
*!
*!**************************************************************************
*
FUNCTION  lfvAprAll 

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and check if the glTrnsHd.cTrnPYr in the scope the posting 
*** window (previous,current,next), if valid check the locking 
*** status, if there is no user use this record,
*** then change the status to 'A' --> approve
lnRecNo = RecNo()

SCAN  FOR &lcScope
  IF glTrnsHd.cTrnPYr $ lcFsWindow
    IF !glTrnsHd.lLok_Stat 
      IF llDumyPost
        REPLACE glTrnsHd.nTrnIndic WITH 1
      ELSE
        REPLACE glTrnsHd.cTrnStat      WITH  'A'
        =gfAdd_Info('glTrnsHd')
      ENDIF
    ENDIF
  ENDIF  
ENDSCAN

*** refresh the browse window 
SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
=lfwBrowhen()

*!**************************************************************************
*!
*!      Function:  lfvAprNon 
*!
*!**************************************************************************
*
FUNCTION  lfvAprNon 

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RecNo()

SCAN FOR &lcScope
  IF !glTrnsHd.lLok_Stat 
    IF llDumyPost
      REPLACE glTrnsHd.nTrnIndic WITH 0
    ELSE
      REPLACE glTrnsHd.cTrnStat WITH ;
         IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')
      =gfAdd_Info('glTrnsHd')
    ENDIF  
  ENDIF  
ENDSCAN

*** refresh the browse window 
SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
=lfwBrowhen()

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RecNo()

SCAN FOR &lcScope
  IF !glTrnsHd.lLok_Stat 
    IF llDumyPost

      IF glTrnsHd.nTrnIndic = 1 
        REPLACE glTrnsHd.nTrnIndic WITH 0
      ELSE
        IF glTrnsHd.cTrnPYr $ lcFsWindow
          REPLACE glTrnsHd.nTrnIndic WITH 1
        ENDIF  
      ENDIF  

    ELSE
      IF glTrnsHd.cTrnStat = 'A'
        REPLACE glTrnsHd.cTrnStat WITH ;
                IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')    
        =gfAdd_Info('glTrnsHd')
      ELSE  
        *** that is mean the user want to approve this record.  
        *** in this case must be check if the glTrnsHd.cTrnPYr 
        *** in the scope the posting window (previous,current,next)
        *** if valid check the locking status, if there is no user 
        *** use this record, then change the status to 'A' --> approve
  
        IF glTrnsHd.cTrnPYr $ lcFsWindow
          IF !glTrnsHd.lLok_Stat 
            REPLACE glTrnsHd.cTrnStat WITH  'A' 
            =gfAdd_Info('glTrnsHd')
          ENDIF
        ENDIF  
      ENDIF  

    ENDIF  
  ENDIF  
ENDSCAN

*** refresh the browse window 
SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
=lfwBrowhen()

*!**************************************************************************
*!
*!      Function:  lfvPost 
*!
*!**************************************************************************
*
FUNCTION  lfvPost

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and if found any record with status 'A' then exit from scan loop
lnRecNo = RecNo()

IF llDumyPost
  LOCATE FOR  &lcScope .AND. glTrnsHd.nTrnIndic = 1
ELSE
  LOCATE FOR  &lcScope .AND. glTrnsHd.cTrnStat = 'A'
ENDIF

IF lnRecNo <= RECCOUNT()
  GO lnRecNo
ENDIF

IF FOUND()
  ***  Post approved transations   <Post      Cancel>.
  IF gfModalGen("TRM02085B02009","DIALOG",IIF(llDumyPost,'included','approved')) = 1 

    *** calling posting procedure
    IF llDumyPost
      SELECT cTranNo,cTrnStat FROM glTrnsHd    ;
         INTO DBF &gcWorkDir.&lc_ToPost ;
         WHERE &lcScope .AND. glTrnsHd.nTrnIndic = 1 ;
         ORDER BY cBatchNo,cTranNo
    ELSE 
      SELECT cTranNo FROM glTrnsHd    ;
         INTO DBF &gcWorkDir.&lc_ToPost ;
         WHERE &lcScope .AND. glTrnsHd.cTrnStat = 'A';
         ORDER BY cBatchNo,cTranNo
    ENDIF 

    SELECT (lc_ToPost)
    INDEX ON CTRANNO TAG CTRANNO 
    SET ORDER TO CTRANNO
    
    IF lfTBPost("Transactions",lc_ToPost,'',lcReportFi) > 0
      *WAIT " Everything is OK     " WINDOW NOWAIT

      IF llDumyPost
        SELECT (lc_ToPost)
        SET RELATION TO '000000' + cTranNo INTO GLTRNSHD

        SCAN FOR &lc_ToPost..cTrnStat = 'P'
          REPLACE glTrnsHd.nTrnIndic WITH 0
        ENDSCAN
      
        glQuitting  = .T.
        CLEAR READ
      ENDIF
    ELSE
      *WAIT " Unsuccessful posting " WINDOW NOWAIT 
    ENDIF   
    
    SELECT GLTRNSHD
    =lfActBrow()
    =lfRefresh()

    RETURN
  ELSE
    RETURN
  ENDIF
ELSE
  *** if there is no record found with 'A' status display
  *** 'No approved transations to post.'
  *** and set the current object to approve
  =gfModalGen("TRM02082B00000","DIALOG",IIF(llDumyPost,'included','approved'))   
  _CUROBJ=OBJNUM(pbApprov)
ENDIF  

*** refresh the browse window 
SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
=lfwBrowhen()

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** print function


*!**************************************************************************
*!
*!      Procedure:  lpEscape
*!
*!**************************************************************************
*
PROCEDURE lpEscape
*** restores on key labels. 
*** activate the glTpost1 window and close the screen
 ON KEY
 ACTIVATE WINDOW glTPost1
 _CUROBJ=OBJNUM(pbClose)
 KEYBOARD CHR(17)+CHR(13)

*!**************************************************************************
*!
*!      Procedure:  lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab 

*** check if the window on top is (lc_TBrowt) ---> browse window
*** if yes then change the current object to ( Approve )
IF WONTOP(lc_TBrowt)
  _CUROBJ=OBJNUM(pbApprov)
  ACTIVATE WINDOW glTPost1
ELSE
  *** else change the current object to ( Print )
*B601099,1 M.H 06/27/96 Begin.
*  IF     (!llDumyPost .AND. _CUROBJ = OBJNUM(pbPrint)) ;
    .OR. ( llDumyPost .AND. _CUROBJ = OBJNUM(pbClose))
  IF _CUROBJ = OBJNUM(pbClose)
*B601099,1 M.H 06/27/96 End.    
    *** case the current object = (Print) then activate browse 
    *** window (lc_TBrowT)
    ACTIVATE WINDOW (lc_TBrowT)
  ELSE      
    *** in this case moves the object to next object     
    _CUROBJ= _CUROBJ + 1 
  ENDIF
ENDIF 
   
*!**************************************************************************
*!
*!      Procedure:  lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab 

*** check if the window on top is (lc_TBrowt) ---> browse window
*** if yes then change the current object to ( Print )
IF WONTOP(lc_TBrowt)

  IF llDumyPost
    _CUROBJ=OBJNUM(pbClose)
  ELSE
*B601099,1 M.H 06/27/96 Begin.
*    _CUROBJ=OBJNUM(pbPrint)
    _CUROBJ=OBJNUM(pbClose)
*B601099,1 M.H 06/27/96 End.
  ENDIF

  ACTIVATE WINDOW glTPost1
ELSE
  *** check if the current object is (Approve) then activate
  *** the browse window (lc_TBrowT)


  IF _CUROBJ = OBJNUM(pbApprov)
    ACTIVATE WINDOW (lc_TBrowT)
  ELSE      
    *** in this case moves the object to previous object          
    _CUROBJ= _CUROBJ - 1 
  ENDIF
ENDIF    




*!**************************************************************************
*!
*!      Function:  lfvSJ
*!
*!**************************************************************************
*
FUNCTION  lfvSJ

*** Create array of 1 column ,each element is the concatenation
*** of two fields (GLSUBJOR.cSrcjrnl,GLSUBJOR.cJorShDes)
SELECT cSrcjrnl+" "+cJorShDes From &gcDataDir.glSubJor ;
       INTO ARRAY laSSorJou

*** Check if there is no any subjournals in the GLSUBJOR
*** in this case display error massege "No available source journals."  
*** then exit form this program

IF ALEN(laSSorJou,1) = 1 .AND. TYPE('laSSorJou[1]') = 'L'
  laSSorJou = " "
  laSSorJou[1,1] = lcSJ_Def
ELSE
  IF ASCAN(laSSorJou,lcSJ_Def) = 0
    DIMENSION laSSorJou[ALEN(laSSorJou,1)+1,1]
    =AINS(laSSorJou,1)
    laSSorJou[1,1] = lcSJ_Def
  ENDIF  
ENDIF

*** calling the global function mover to select the sobjournals
=gfMover(@laSSorJou,@laTSorJou,"Source journal",.T.)
*** init. the string variable lcSrcjrnl
lcSrcjrnl =''

*** collect all source journals names in the string lcSrcjrnl
FOR lnCounter = 1 TO ALEN(laTSorJou,1)
   lcSrcjrnl = lcSrcjrnl + LEFT(laTSorJou[lnCounter],3)
ENDFOR

*** check if there is no any subjournlas are selected
*** in this case disable all objects except (Close),(S.J.) objects.

IF ! EMPTY(lcSrcjrnl)
  *** check if there is any transaction(s) found for this subjournal(s)
  *** if there is no any transaction 
  *** display "No transactions for this source journal(s).
  *** and disable all objects except (Close),(S.J.) objects.
  LOCATE FOR &lcScope  
  IF  FOUND()
    SHOW GETS ENABLE 
  ELSE 
    =gfModalGen("TRM02084B00000","Dialog")  
    SHOW GETS DISABLE
    SHOW GET  pbClose ENABLE
    SHOW GET  pbSJ    ENABLE
  ENDIF  
ELSE   
  SHOW GETS DISABLE
  SHOW GET  pbClose ENABLE
  SHOW GET  pbSJ    ENABLE
ENDIF

LOCATE FOR &lcScope
SHOW GETS


*!**************************************************************************
*!					
*!      Function:  lfObj_Lock 
*!
*!**************************************************************************
*
FUNCTION  lfObj_Lock

IF (!llok_Stat) AND (cLok_User <> gcUser_Id)
  RETURN .T.
ELSE
  =gfModalGen("TRM02213B00000","ALART",IIF(llDumyPost,'include','approve')+'|'+'transaction')
  RETURN .F.
ENDIF
