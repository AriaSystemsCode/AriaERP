*:************************************************************************
*:
*: Procedure file: GLBGPST.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified:12/30/93
*:
*:  Procs & Fncts:
*:
*:    lfActBrow   *** To activate the browse ***
*:    lfwBrowhen  *** When browse function ***
*:    lfCheckEL   *** To put check mark in edit list field ***
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
*:    lfCalc      *** To calculate the total CR and total Dr 
*:               
*:      Documented 12/30/1993
*B601099,1 M.H 06/27/96 Hide push button print by change the #OBJDISP from "!llDumyPost" TO ".F."
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*:************************************************************************
*
PARAMETERS lcReportFi
llDumyPost = IIF(TYPE('lcReportFi')='C',.T.,.F.)

DECLARE la_TBtStat  [3] && to display status       for each transation 

la_TBtStat [1] = 'Unposted'          && Case status --> 'U'
la_TBtStat [2] = 'Out of balance'    && Case status --> 'O'
la_TBtStat [3] = 'Approved'          && Case status --> 'A'

laCtrStat      = 'DISABLE' && To disable the browse pad in the menu

lc_ToPost     = ' '
lcEditLine    = ' '    && init. edit list var.  
                                     && Say refresh function        
llprint       = .T. 
lcCurrBat     = ''     && set the current record in browse to 0

lnTotalCr     = 0.00   && total approved credit
lnTotalDr     = 0.00   && total approved debit

lnRecNo       = 0

lcScope   = "cBatType = 'B' .AND. cBatStat $ 'UOA' "

IF llDumyPost
  lc_TBrowt = "Dummy Posting beginning balances"  
ELSE  
  lc_TBrowt = "Posting beginning balances"  
ENDIF  

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


*** Check if GLSETUP.dSetBBDat between the posting window
IF Between(GLSETUP.dSetBBDat,ldPyBgDate,ldNyEnDate) 

*** Check if GLSETUP.lSetAlbBe is true,(allowed to post begining balances)
  IF !GLSETUP.lSetAlbBe 
    *** Message "Posting begining balances is not allowed.  
    *** then exit from this program
    =gfModalGen("TRM02098B00000","Dialog")      
    glQuitting  = .T.
    RETURN
  ENDIF
ELSE

  *** Message "Begining balances date is out of the posting window"
  *** then exit from this program
  =gfModalGen("TRM02097B00000","Dialog")      
  glQuitting  = .T.
  RETURN
ENDIF


SELECT GLBATCH
*** searches the DBF (glBatch) for the first record 
*** that matches a given expression (lcScope).
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

  =lfCalc()    && Totals all glBatch.nBatotCr, glBatch.nBatotDr 
               && only for batches with glBatch.cBatStat = 'A' 

  KEYBOARD CHR(49)      && clear keyboard buffer.

  SET CONFIRM OFF

  SELECT GLBATCH  
  *E300683,5 Call *.SPR from screens directory
  * DO Glbgpst.SPR 
  DO (gcScrDir + gcWinAppl + '\Glbgpst.SPR')
  *E300683,5 end   
  SET CONFIRM ON
  
  *** restores on key labels that were placed on the stack with push key. 
  POP KEY
ELSE  
  =gfModalGen("TRM02099B00000","Dialog")  && there is no batches to post.
ENDIF

glQuitting  = .T.

RELEASE WINDOW (lc_TBrowt)

IF USED(lc_ToPost)
  USE IN ALIAS (lc_ToPost)
ENDIF
ERASE &gcWorkdir.&lc_ToPost..DBF
ERASE &gcWorkdir.&lc_ToPost..CDX
SELECT GLBATCH


*!**************************************************************************
*!
*!      Function:  lfActBrow   
*!
*!**************************************************************************
*
FUNCTION  lfActBrow

LOCATE FOR &lcScope
lcObjState = IIF(!FOUND(),"DISABLE","ENABLE")
=lfwBrowhen()
SHOW GET pbApprov  &lcObjState
SHOW GET pbAll     &lcObjState
SHOW GET pbNone    &lcObjState
SHOW GET pbInvert  &lcObjState
SHOW GET pbPrint   &lcObjState
SHOW GET pbPost    &lcObjState

lcFieldDis = IIF(llDumyPost,;
                "Included=IIF(lBatInd       ,'   û','    ')", ;
                "Approved=IIF(cBatStat = 'A','   û','    ')") 


lcBrString="cDummi1=IIF(lcCurrBat=cBatchNo,'',' '):H=' ':F:V=lfKeyPressed(),"+;
           "cBatchNo:H='Batch#':6,cDummi2=lfCheckEL():H='E/L':3,"+;
           "&lcFieldDis"+;
           ",cSrcModul:H='S/M':3,"+;
           "DBatpBeg:H='  Date':10,"+;
           "CBatDesc:H='   Description':40"
 
DO CASE
  CASE _DOS
    BROWSE FIELDS &lcBrString;
           LOCK 0   ;
           NOAPPEND ;
           NOCLEAR  ;
           NODELETE ;
           NOMENU   ;
           NOWAIT   ;
           SAVE     ;
           FREEZ cDummi1;
           FOR   &lcScope;
           TITLE lc_TBrowt ;
           WHEN lfwBrowhen();
           WINDOW glBGPst0 IN WINDOW (gcBaseWind)
  CASE _WINDOWS
    BROWSE FIELDS &lcBrString;
           LOCK 0   ;
           NOAPPEND ;
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
           WINDOW glBGPst0 IN WINDOW (gcBaseWind)
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfwBrowhen
*!
*!**************************************************************************
*
FUNCTION  lfwBrowhen
*** check if the status for the ( cBatStat ) 
*** and change the prompt display
*** if A -->  display 'Approved ' Else display 'Disapprove'

IF llDumyPost 

  IF lBatInd
    SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
  ELSE
    SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
  ENDIF

ELSE

  IF cBatStat = 'A'
    SHOW GET pbApprov,1 PROMPT '\<Disapprove'
  ELSE
    SHOW GET pbApprov,1 PROMPT '\<Approve'
  ENDIF

ENDIF

SHOW GETS

*!**************************************************************************
*!
*!      Function:  lfCheckEL 
*!
*!**************************************************************************
*
FUNCTION  lfCheckEL

*** lcEditL = IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,' û',;
***           IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
***                 glBatch.cbAtelTim > glBatch.caDd_Time,' û','  '))
*** else lcEditl = ' '

IF !EMPTY(glBatch.cBatElUsr) 

  lcEditL = IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,' û',;
            IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                  glBatch.cbAtelTim > glBatch.caDd_Time,' û','  '))
ELSE
  lcEditL = '  ' 
ENDIF    

RETURN lcEditL


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

*** check if the status for (cBatStat) = 'A' --> approve 
*** that is mean the user want to disapprove this record.
*** then we will check the locking status, if there is no
*** user use this record, then change the status according to 
*** the diffrent between glBatch.nBatotdr,glBatch.nBatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
*** and subtract glBatch.nBatotCr From lnTotalCr,
***     subtract glBatch.nBatotDr From lnTotalDr

IF llDumyPost
  IF lBatInd
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .F.
      lnTotalCr = lnTotalCr - glBatch.nBatotCr
      lnTotalDr = lnTotalDr - glBatch.nBatotDr
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
    ENDIF
  ELSE 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .T.
      lnTotalCr = lnTotalCr + glBatch.nBatotCr
      lnTotalDr = lnTotalDr + glBatch.nBatotDr
      =gfObj_Lock(.F.) 
      SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
    ENDIF  
  ENDIF  

ELSE
  IF cBatStat = 'A' 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.cBatStat WITH ;
              IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      lnTotalCr = lnTotalCr - glBatch.nBatotCr
      lnTotalDr = lnTotalDr - glBatch.nBatotDr
      =gfAdd_Info('glbatch')
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT '\<Approve' 
    ENDIF
  ELSE 
    *** that is mean the user want to approve this record.  
    *** if check list is approved then if valid check the locking status,
    *** if there is no user use this record, then change the status to 'A' 
    *** and adding glBatch.nBatotCr to lnTotalCr,
    ***     adding glBatch.nBatotDr to lnTotalDr

    lcEditLine=lfCheckEL()
    llPrint = IIF (glSetup.lSetForel,;
              IIF (lcEditLine= ' û' ,.T.,.F.),.T.)
               
    IF llPrint 
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glBatch.cBatStat WITH  'A'
        lnTotalCr = lnTotalCr + glBatch.nBatotCr
        lnTotalDr = lnTotalDr + glBatch.nBatotDr
        =gfAdd_Info('glbatch')
        =gfObj_Lock(.F.) 
        SHOW GET pbApprov,1 PROMPT '\<Disapprove'
      ENDIF
    ELSE
      =gfModalGen("TRM02100B00000","Dialog")  && Printing the edit list
                                          && is a must to approve
                                          && the batch for posting.
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

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and check the locking status, if there is no user use this 
*** record, then change the status to 'A' --> approve
lnRecNo = RECNO()


IF llDumyPost 
  SCAN  FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.lBatInd WITH  .T.
    ENDIF
  ENDSCAN

ELSE

  SCAN  FOR &lcScope
    lcEditLine = lfCheckEL()
    llPrint    = IIF (glSetup.lSetForel,;
                 IIF (lcEditLine= ' û' ,.T.,.F.),.T.)
    IF llPrint 
      IF !glBatch.lLok_Stat 
        REPLACE glBatch.cBatStat      WITH  'A'
        =gfAdd_Info('glbatch')
      ENDIF
    ENDIF  
  ENDSCAN

ENDIF  

=lfCalc()    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.
             
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
*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glbatch.nBatotdr,glbatch.nbatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RECNO()

IF llDumyPost 
  SCAN  FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.lBatInd WITH  .F.
    ENDIF
  ENDSCAN

ELSE

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.cBatStat WITH ;
      IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      =gfAdd_Info('glbatch')    
    ENDIF     
  ENDSCAN
ENDIF  

=lfCalc()    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.

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
*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glBatch.nBatotdr,glBatch.nBatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
*** and subtract glBatch.nBatotCr From lnTotalCr,
***     subtract glBatch.nBatotDr From lnTotalDr
lnRecNo = RECNO()

IF llDumyPost 

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      IF glBatch.lBatInd
        REPLACE glBatch.lBatInd WITH .F.
      ELSE  
        IF !glBatch.lLok_Stat 
          REPLACE glBatch.lBatInd WITH  .T. 
        ENDIF
      ENDIF  
    ENDIF  
  ENDSCAN

ELSE

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      IF glBatch.cBatStat = 'A'
        REPLACE glBatch.cBatStat WITH ;
                IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
        =gfAdd_Info('glbatch')              
      ELSE  
        *** that is mean the user want to approve this record.  
        *** if check list is approved then if valid check the locking status,
        *** if there is no user use this record, then change the status to 'A' 

        lcEditLine=lfCheckEL()
        llPrint = IIF (glSetup.lSetForel,;
                  IIF (lcEditLine= ' û' ,.T.,.F.),.T.)
               
        IF llPrint 
          IF !glBatch.lLok_Stat 
            REPLACE glBatch.cBatStat WITH  'A' 
            =gfAdd_Info('glbatch')          
          ENDIF
        ENDIF  
      ENDIF  
    ENDIF  
  ENDSCAN
ENDIF  

=lfCalc()    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.
           
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

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and if found any record with status 'A' then exit from scan loop

lnRecNo = RECNO()

IF llDumyPost
  LOCATE FOR &lcScope .AND. glBatch.lBatInd
ELSE
  LOCATE FOR &lcScope .AND. glBatch.cBatStat = 'A'
ENDIF
  
IF lnRecNo <= RECCOUNT()
  GOTO lnRecNo
ENDIF  


IF FOUND()
  *** check if total credit approved batches = total debit approved batches
  *** if eles messaeg "Total approves batches is out of balance by XXX"+
  *** You cannot post the approved batches until it balance.
  
  IF lnTotalCr <> lnTotalDr 
    =gfModalGen("TRM02101B00000","DIALOG",;
       IIF(llDumyPost,'included','approved')+'|'+;
       LTRIM(STR(ABS(lnTotalDr-lnTotalCr),15,2))+;
       ' ' + IIF(lnTotalDr>lnTotalCr,'Dr','Cr')+'|'+;
       IIF(llDumyPost,'included','approved'))

  ELSE  
    ***  Post approved batches.  <Post>     <Cancel>
    IF gfModalGen("TRM02070B02009","DIALOG",IIF(llDumyPost,'included','approved')) = 1 
      *** calling posting procedure  

      IF llDumyPost
        SELECT cBatchNo,cBatStat FROM GLBATCH   ;
        INTO DBF &gcWorkDir.&lc_ToPost ;
        WHERE  &lcScope .AND. glBatch.lBatInd ;
        ORDER BY cBatchNo
      ELSE
        SELECT cBatchNo FROM GLBATCH   ;
        INTO DBF &gcWorkDir.&lc_ToPost ;
        WHERE  &lcScope .AND. glBatch.cBatStat = 'A';
        ORDER BY cBatchNo
      ENDIF

      SELECT (lc_ToPost)
      INDEX ON CBATCHNO TAG CBATCHNO 
      SET ORDER TO CBATCHNO     
    
      IF lfTBPost("Batch",lc_ToPost,"Beginning",lcReportFi) > 0
        *WAIT " Posting OK"  WINDOW NOWAIT
        IF llDumyPost
          SELECT (lc_ToPost)
          SET RELATION TO CBATCHNO INTO GLBATCH

          SCAN FOR &lc_ToPost..cBatchNo = 'P'
            REPLACE glBatch.lBatInd WITH .F.
          ENDSCAN
      
          glQuitting  = .T.
          CLEAR READ
        ENDIF
      ELSE
 *        WAIT " Unsuccessful posting "  WINDOW NOWAIT
      ENDIF    
      SELECT GLBATCH
      =lfCalc()   
      =lfActBrow()
      =lfRefresh()
      RETURN 
      
    ELSE
      RETURN
    ENDIF
  ENDIF  
ELSE
  *** if there is no record found with 'A' status 
  *** display "No approved batches to post."
  *** and set the current object to approve
  =gfModalGen("TRM02069B00000","DIALOG",IIF(llDumyPost,'included','approved'))
  _CUROBJ=OBJNUM(pbApprov)
ENDIF  

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function



*!**************************************************************************
*!
*!      Procedure:  lpEscape
*!
*!**************************************************************************
*
PROCEDURE lpEscape
*** restores on key labels. 
*** activate the glTpost1 window and close the screen
*ON KEY
ACTIVATE WINDOW glBGPst1
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
  ACTIVATE WINDOW glBGPst1
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
*** if yes then change the current object to ( Close )

IF WONTOP(lc_TBrowt)
  IF llDumyPost
    _CUROBJ=OBJNUM(pbClose)
  ELSE
*B601099,1 M.H 06/27/96 Begin.
*    _CUROBJ=OBJNUM(pbPrint)
    _CUROBJ=OBJNUM(pbClose)
*B601099,1 M.H 06/27/96 End.
  ENDIF
 
  ACTIVATE WINDOW glBGPst1
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
*!      Function:  lfCalc
*!
*!**************************************************************************
*
FUNCTION  lfCalc
*** calculate the totals glBatch.nBatotCr, glBatch.nBatotDr 
*** only for approved batches in scope (lcScope).

lnTotalCr = 0.00
lnTotalDr = 0.00

IF llDumyPost
  SUM  glBatch.nBatotCr, glBatch.nBatotDr ;
  FOR &lcScope .AND.  glBatch.lBatInd     ;
  TO lnTotalCr,lnTotalDr
ELSE
  SUM  glBatch.nBatotCr, glBatch.nBatotDr ;
  FOR &lcScope .AND.  glBatch.cBatStat ='A';
  TO lnTotalCr,lnTotalDr
ENDIF

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
  =gfModalGen("TRM02213B00000","ALART",IIF(llDumyPost,'include','approve')+'|'+'beginning balances batch')
  RETURN .F.
ENDIF
