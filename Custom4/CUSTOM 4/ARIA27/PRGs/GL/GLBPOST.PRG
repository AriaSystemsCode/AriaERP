*:************************************************************************
*:
*: Procedure file: GLBPOST.PRG
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
*:    lfvSM       *** To Select the active source mudals from list ***
*:               
*:      Documented 12/28/1993
*B601099,1 M.H 06/27/96 Hide push button print by change the #OBJDISP from "!llDumyPost" TO ".F."
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E301024,1 AAMER 10/15/98 Collect the source modules from SyGLTran File
*E301077,71 Walid abou el-magd (WAM) 03/02/99
*E301077,71 Move the openning of SyGLTran file and all associated processes
*E301077,71 to be after calling of gfStup() . And use gfOpenFile() to open
*E301077,71 SyGLTran file instead of USE command .
*B602645,1 WALID 03/08/99 Proper deleting of elements from the source module 
*B602645,1 				  array.
*:************************************************************************
*
PARAMETERS lcReportFi
llDumyPost = IIF(TYPE('lcReportFi')='C',.T.,.F.)

DECLARE la_TBtStat  [3] && to display status       for each transation 
DECLARE la_TBtType  [3] && to display types        for each transation 
DECLARE la_TPostYr  [4] && to display posting year for each transation

*** Arrays used by mover :
DECLARE laSApp_ID   [1] && source array of applications names
DECLARE laTApp_ID   [1] && source array of applications names
 
la_TBtStat [1] = 'Unposted'          && Case status --> 'U'
la_TBtStat [2] = 'Out of balance'    && Case status --> 'O'
la_TBtStat [3] = 'Approved'          && Case status --> 'A'

la_TBtType [1] = 'Normal'            && Case Type  ---> 'N'
la_TBtType [2] = 'Statistical'       && Case Type  ---> 'S'
la_TBtType [3] = 'Subledger'         && Case Type  ---> 'L'

la_TPostYr [1] = 'Previous'      && Case Posting year = lcCurr_yer -1
la_TPostYr [2] = 'Current'       && Case Posting year = lcCurr_yer
la_TPostYr [3] = 'Next '         && Case Posting year = lcCurr_yer +1
la_TPostYr [4] = '  '            && Case else

lcEditLine    = ' '  && init. edit list var.  
lcCurrBat     = ''   && set the current record in browse to 0
lcSrcModul    = ''   && string to store the active suorce modual(s) names
lc_ToPost     = ''

lcModName     = "General ledger"   && String to store general ladger module name

laCtrStat     = 'DISABLE' && To disable the browse pad in the menu

lnRecNo       = 0

llprint       = .T. 
*E301077,71 WALID (WAM) [BEGIN] Redefine all variabels which being moved after gfSetup()
DECLARE laTmpSMod[1,2]
laTmpSMod  = ''
lc_TBrowt  = ''
lcFsWindow =''
lcScope    =''
*E301077,71 WALID (WAM) [END..]
*** Create array of 1 column ,each element is the concatenation
*** of two fields (SYDAPPL.cApp_ID,SYDAPPL.cApp_Name)
*** except the 'SM' and 'SY'
	
*SELECT cApp_ID+" "+cApp_Name From &gcSysHome.sydAppl;
       INTO ARRAY laSApp_ID;
       WHERE cApp_ID <> 'SM' AND cApp_ID <> 'SY'		

*E301024,1 (AAMER 10/15/98)
*E301024,1 Collect the source modules from SyGLTran File (Start)
*E301024,1 Comment this part of code and collect the source modules from
*E301024,1 G/L Transaction types file SYGLTRAN
**** Added by Yasser on 3-11-95
*SELECT GLSETUP
*DECLARE laTmpSMod[1,2]
*laTmpSMod = ""
*=gfSUBSTR(glsetup.mlinkapp,@laTmpSMod,"|~")

*IF !EMPTY(laTmpSMod[1,1])
*  DECLARE laTmpSMod[ALEN(laTmpSMod,1)+1,2]
*ENDIF

*laTmpSMod[ALEN(laTmpSMod,1),1] = "GL"
*laTmpSMod[ALEN(laTmpSMod,1),2] = lcModName


*-- E301024 Collect the source modules from SyGLTran File
*-- declare llGlTrOpen variable (AAMER) 10/15/98 (Start)
*-- llGlTrOpen Variable that showes if the program opened SyGLTran file
*-- or it is opened before getting into the program

*E301077 Walid abou el-magd (WAM) 03/02/99 [BEGIN]
* the following part of code must be moved after calling gfStup()
* And use gfOpenFile() instead of USE command to open SyGLTran file .
*llGlTrOpen = .F.

*-- E301024 Declare llGlTrOpen variable (AAMER) 10/15/98 (End)

*IF !USED('SyGLTran')
*  USE (gcSysHome+'SyGLTran.DBF') IN 0 SHARED
*  llGlTrOpen = .T.
*ENDIF
  
*DECLARE laTmpSMod[1,2]
*laTmpSMod = ''
*SELECT DISTINCT SyGLTran.cSrcModul,SydAppl.cApp_Name ;
*  FROM SyGLTran,SydAppl ;
*  INTO ARRAY laTmpSMod  ;
*  WHERE SyGLTran.cSrcModul = SydAppl.cApp_ID

*IF !EMPTY(laTmpSMod[1,1])
*  SELECT SycComp
*  PRIVATE lcCurTag
*  lcCurTag = ORDER()
*  SET ORDER TO Ccomp_id
*  IF SEEK(gcAct_Comp)
*    PRIVATE lnI,lnDelCnt
*    STORE 0 TO lnI,lnDelCnt
*    FOR lnI = 1 TO ALEN(laTmpSMod,1)
*      IF !(laTmpSMod[lnI,1] $ SycComp.mModlSet)
*        lnDelCnt = lnDelCnt + 1
*        =ADEL(laTmpSMod,lnI)
*      ENDIF
*    ENDFOR
*    IF lnDelCnt > 0
*      DIMENSION laTmpSMod[ALEN(laTmpSMod,1)-lnDelCnt,2]
*    ENDIF
*  ELSE
*    STORE " " TO laTmpSMod
*  ENDIF
*  SET ORDER TO &lcCurTag
*ENDIF  
**E301024,1 Collect the source modules from SyGLTran File (End)

*DECLARE laSApp_ID[ALEN(laTmpSMod,1)]

*FOR lnSMRow = 1 TO ALEN(laTmpSMod,1)
*  laSApp_ID[lnSMRow] = ALLTRIM(laTmpSMod[lnSMRow,1])+"-"+ALLTRIM(laTmpSMod[lnSMRow,2])
*ENDFOR

**** Create array with the same number of elements in source array.
**** then copy content from source array to target array.
     
*DECLARE laTApp_ID [ALEN(laSApp_ID,1)]
*=ACOPY(laSApp_ID,laTApp_ID)


*** collect all source journals names in the string lcSorJourn
*FOR lnCounter = 1 TO ALEN(laSApp_ID)
*  lcSrcmodul = lcSrcmodul + LEFT(laSApp_ID[lnCounter],3) 
*ENDFOR


*IF llDumyPost
*  lc_TBrowt = "Dummy Batches Posting"  &&  initial the window header.
*ELSE  
*  lc_TBrowt = "Batches Posting"        &&  initial the window header.
*ENDIF
  
*** the string that contaning the scope of the valid posting years  
*lcFsWindow    = STR(VAL(lcCurr_yer)-1,4)+" "+lcCurr_yer+" "+;
                STR(VAL(lcCurr_yer)+1,4)

*lcScope       =      "((cBatType = 'N' .AND. cBatStat $ 'UA' )"  + ;
                ".OR.  (cBatType = 'L' .AND. cBatStat $ 'UA' )"  + ;
                ".OR.  (cBatType = 'S' .AND. cBatStat $ 'UAO'))" + ;
                ".AND. cSrcmodul $ lcSrcmodul" 
*E301077 Walid abou el-magd (WAM) 03/02/99 [END..]


IF !gfSetup()    
  RETURN
ENDIF  

*E301077 Walid abou el-magd (WAM) 03/02/99 [BEGIN]
llGlTrOpen = .F.

*-- E301024 Declare llGlTrOpen variable (AAMER) 10/15/98 (End)

*IF !USED('SyGLTran')
*  USE (gcSysHome+'SyGLTran.DBF') IN 0 SHARED
*  llGlTrOpen = .T.
*ENDIF
llGlTrOpen = gfOpenFile(gcSysHome+'SyGLTran','','SH')  
*DECLARE laTmpSMod[1,2]
*laTmpSMod = ''
SELECT DISTINCT SyGLTran.cSrcModul,SydAppl.cApp_Name ;
  FROM SyGLTran,SydAppl ;
  INTO ARRAY laTmpSMod  ;
  WHERE SyGLTran.cSrcModul = SydAppl.cApp_ID

IF !EMPTY(laTmpSMod[1,1])
  =gfOpenFile(gcSysHome+'SycComp','CCOMP_ID','SH')  
  SELECT SycComp
  PRIVATE lcCurTag
  lcCurTag = ORDER()
  SET ORDER TO Ccomp_id
  IF SEEK(gcAct_Comp)
    PRIVATE lnI,lnDelCnt
    STORE 0 TO lnI,lnDelCnt
    *B602645,1 begin
    *B602645,1 The following code will cause a bug as follow
    *B602645,1  assume that when lni=8 the condition is true
    *B602645,1  so due to  =ADEL(laTmpSMod,lnI) laTmpSMod[8,1] will be deleted
    *B602645,1  and laTmpSMod[9,1] will be moved to laTmpSMod[8,1]
    *B602645,1  and laTmpSMod[9,1]=.F.
    *B602645,1 so when lni=9 this condition (laTmpSMod[lnI,1] $ SycComp.mModlSet)                
    *B602645,1 will generats an ERROR Invalid function argument value , type , or count .
    *FOR lnI = 1 TO ALEN(laTmpSMod,1)
    *  IF !(laTmpSMod[lnI,1] $ SycComp.mModlSet)
    *    lnDelCnt = lnDelCnt + 1
    *    =ADEL(laTmpSMod,lnI)
    *    DIMENSION laTmpSMod[ALEN(laTmpSMod,1)-lnDelCnt,2]
    *  ENDIF
    *ENDFOR
    *IF lnDelCnt > 0
    *  DIMENSION laTmpSMod[ALEN(laTmpSMod,1)-lnDelCnt,2]
    *ENDIF
    DIMENSION laDumm(ALEN(laTmpSMod,1),1)
    FOR lnI = 1 TO ALEN(laTmpSMod,1)
      IF !(laTmpSMod[lnI,1] $ SycComp.mModlSet)
      laDumm[LNI,1] = .T.
      lnDelCnt = lnDelCnt + 1
      ENDIF
    ENDFOR
    IF lnDelCnt > 0 
      lnDelCnt =0
      FOR lnI = 1 TO ALEN(laDumm,1)
        IF laDumm[LNI,1]
          =ADEL(laTmpSMod,lnI)
          lnDelCnt = lnDelCnt+1
        ENDIF
      ENDFOR
      DIMENSION laTmpSMod[ALEN(laTmpSMod,1)-lnDelCnt,2]
    ENDIF
    *B602645,1 end
  ELSE
    STORE " " TO laTmpSMod
  ENDIF

  SET ORDER TO &lcCurTag
ENDIF  
*E301024,1 Collect the source modules from SyGLTran File (End)

DECLARE laSApp_ID[ALEN(laTmpSMod,1)]

FOR lnSMRow = 1 TO ALEN(laTmpSMod,1)
  laSApp_ID[lnSMRow] = ALLTRIM(laTmpSMod[lnSMRow,1])+"-"+ALLTRIM(laTmpSMod[lnSMRow,2])
ENDFOR

*** Create array with the same number of elements in source array.
*** then copy content from source array to target array.
     
DECLARE laTApp_ID [ALEN(laSApp_ID,1)]
=ACOPY(laSApp_ID,laTApp_ID)


*** collect all source journals names in the string lcSorJourn
FOR lnCounter = 1 TO ALEN(laSApp_ID)
  lcSrcmodul = lcSrcmodul + LEFT(laSApp_ID[lnCounter],3) 
ENDFOR


IF llDumyPost
  lc_TBrowt = "Dummy Batches Posting"  &&  initial the window header.
ELSE  
  lc_TBrowt = "Batches Posting"        &&  initial the window header.
ENDIF
  
*** the string that contaning the scope of the valid posting years  
lcFsWindow    = STR(VAL(lcCurr_yer)-1,4)+" "+lcCurr_yer+" "+;
                STR(VAL(lcCurr_yer)+1,4)

lcScope       =      "((cBatType = 'N' .AND. cBatStat $ 'UA' )"  + ;
                ".OR.  (cBatType = 'L' .AND. cBatStat $ 'UA' )"  + ;
                ".OR.  (cBatType = 'S' .AND. cBatStat $ 'UAO'))" + ;
                ".AND. cSrcmodul $ lcSrcmodul" 
*E301077 Walid abou el-magd (WAM) 03/02/99 [END..]                
IF llDumyPost
  lcText  = 'Inclu\<de'
  lcText1 = 'Include a\<ll'
  lcText2 = 'Include \<none'
ELSE
  lcText  = '\<Approve'
  lcText1 = 'Approve a\<ll'
  lcText2 = 'Approve \<none'
ENDIF

SELECT GLBATCH

*** searches the DBF (glBatch) for the first record 
*** that matches a given expression.
*** --> types  (glBatch.cBatType ) , status (glBatch.cBatStat ) 

LOCATE FOR &lcScope

IF FOUND()

  lc_ToPost   = gfTempName()

  SELECT GLBATCH   
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
  
  SELECT GLBATCH     
  *E300683,5 Call *.SPR from screens directory
  * DO Glbpost.SPR 
  DO (gcScrDir + gcWinAppl + '\Glbpost.SPR')
  *E300683,5 end   
  SET CONFIRM ON
  
  *** restores on key labels that were placed on the stack with push key. 
  POP KEY
ELSE  
  *** if there is no batches to post.
  =gfModalGen("TRM02063B00000","Dialog")      
ENDIF

glQuitting  = .T.

RELEASE WINDOW (lc_TBrowt)

*-- E301024 Collect the source modules from SyGLTran File
*-- E301024 Close SyGLTran file if it was
*-- E301024 opened by this program (AAMER) 10/15/98 (Start)
*E301077,71 WALID (WAM) [BEGIN] THIS FILE WILL BE CLOSED AUTOMATICALLY
*IF llGlTrOpen
*  USE IN SyGLTran
*ENDIF
*E301077,71 WALID (WAM) [END..]
*-- E301024 opened by this program (AAMER) 10/15/98 (End)

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
=lfwBrowhen()
lcObjState = IIF(!FOUND(),"DISABLE","ENABLE")
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
           ",cSrcmodul:H='S/M':3,cComp_ID:H='Co':3,"+;
           "DBatpBeg:H='  From':10,DBatpEnd:H='   To':10,"+;
           "CBatRefer:H='   Reference':25"

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
           WINDOW glBPost0 IN WINDOW (gcBaseWind)

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
           WINDOW glBPost0 IN WINDOW (gcBaseWind)
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

*** check if GLBATCH.cBatElUsr not empty then 
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

IF llDumyPost
  IF lBatInd
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .F.
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
    ENDIF
  ELSE 
    IF glBatch.cbatpyr $ lcFsWindow
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glBatch.lBatInd WITH .T.
        =gfObj_Lock(.F.) 
        SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
      ENDIF  
    ELSE   
      =gfModalGen("TRM02064B00000","Dialog",IIF(llDumyPost,'include','approve'))
      *** The Batch posting year is out of the posting window.    
    ENDIF  
  ENDIF  

ELSE

  IF cBatStat = 'A' 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.cBatStat WITH ;
              IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      =gfAdd_Info('glbatch')
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT '\<Approve' 
    ENDIF
  ELSE 
    *** that is mean the user want to approve this record.  
    *** in this case must be check if the glBatch.cbatpyr 
    *** in the scope the posting window (previous,current,next)
    *** if !valid display "The Batch posting year is out of the posting window"
    *** else if check list is approved then if valid check the locking status,
    *** if there is no user use this record, then change the status to 'A' 

    IF glBatch.cbatpyr $ lcFsWindow
      lcEditLine=lfCheckEL()
      llPrint = IIF (glSetup.lSetForel,IIF (lcEditLine= ' û' ,.T.,.F.),.T.)
               
      IF llPrint 
        IF lfObj_Lock() .AND. gfObj_Lock(.T.)
          REPLACE glBatch.cBatStat WITH  'A'
          =gfAdd_Info('glbatch')
          =gfObj_Lock(.F.) 
          SHOW GET pbApprov,1 PROMPT '\<Disapprove'
        ENDIF
      ELSE
        =gfModalGen("TRM02065B00000","Dialog",IIF(llDumyPost,'include','approve'))
        *** Printing the edit list is a must to approve.
      ENDIF  
    ELSE   
      =gfModalGen("TRM02064B00000","Dialog",IIF(llDumyPost,'include','approve'))     
      &&  The Batch posting year is out of the posting window.    
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
*** and check if the glBatch.cBatPYr in the scope the posting 
*** window (previous,current,next), and check list approved then
*** if valid check the locking status, if there is no user use this 
*** record, then change the status to 'A' --> approve

lnRecNo = RECNO()

IF llDumyPost 
  SCAN  FOR &lcScope
    IF glBatch.cbatpyr $ lcFsWindow
      IF !glBatch.lLok_Stat 
        REPLACE glBatch.lBatInd WITH  .T.
      ENDIF
    ENDIF  
  ENDSCAN

ELSE

  SCAN  FOR &lcScope
    IF glBatch.cbatpyr $ lcFsWindow
      lcEditLine = lfCheckEL()
      llPrint    = IIF (glSetup.lSetForel,IIF (lcEditLine= ' û' ,.T.,.F.),.T.)
      IF llPrint 
        IF !glBatch.lLok_Stat 
          REPLACE glBatch.cBatStat      WITH  'A'
          =gfAdd_Info('glbatch')
        ENDIF
      ENDIF  
    ENDIF  
  ENDSCAN
ENDIF  

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
    IF glBatch.cbatpyr $ lcFsWindow
      IF !glBatch.lLok_Stat 
        REPLACE glBatch.lBatInd WITH  .F.
      ENDIF
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
lnRecNo = RECNO()

IF llDumyPost 

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      IF glBatch.lBatInd
        REPLACE glBatch.lBatInd WITH .F.
      ELSE  
        IF glBatch.cbatpyr $ lcFsWindow
          IF !glBatch.lLok_Stat 
            REPLACE glBatch.lBatInd WITH  .T. 
          ENDIF
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
        *** in this case must be check if the glBatch.cbatpyr 
        *** in the scope the posting window (previous,current,next)
        *** if valid then if check list is approved then if valid 
        *** check the locking status,
        *** if there is no user use this record, then change the status to 'A' 

        IF glBatch.cbatpyr $ lcFsWindow
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
    ENDIF  
  ENDSCAN
ENDIF  

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
  LOCATE FOR  &lcScope .AND. glBatch.lBatInd
ELSE  
  LOCATE FOR  &lcScope .AND. glBatch.cBatStat = 'A'
ENDIF
  
IF lnRecNo <= RECCOUNT()
  GO lnRecNo
ENDIF
 
IF FOUND()
  ***  Post approved batches.
  IF gfModalGen("TRM02070B02009","DIALOG",IIF(llDumyPost,'included','approved')) = 1 
    *** calling posting procedure  

    IF llDumyPost
      SELECT cBatchNo,cBatStat FROM GLBATCH ;
      INTO DBF &gcWorkDir.&lc_ToPost        ;
      WHERE  &lcScope .AND. glBatch.lBatInd ;
      ORDER BY cBatchNo
    ELSE
      SELECT cBatchNo FROM GLBATCH    ;
      INTO DBF &gcWorkDir.&lc_ToPost  ;
      WHERE  &lcScope .AND. glBatch.cBatStat = 'A';
      ORDER BY cBatchNo
    ENDIF  
      
    SELECT (lc_ToPost)
    INDEX ON CBATCHNO TAG CBATCHNO 
    SET ORDER TO CBATCHNO      
   
      IF lfTBPost("Batch",lc_ToPost,"Nonbeginning",lcReportFi) > 0
        * WAIT " Posting OK"  WINDOW NOWAIT
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
*         WAIT " Unsuccessful posting "  WINDOW NOWAIT
      ENDIF    
      SELECT GLBATCH
      =lfActBrow()
      =lfRefresh()
      RETURN
  ELSE
    RETURN
  ENDIF
ELSE
  *** if there is no record found with 'A' status display
  *** 'No approved batches to post.'
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
ON KEY
ACTIVATE WINDOW glBPost1
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
  ACTIVATE WINDOW glBPost1
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
 
  ACTIVATE WINDOW glBPost1
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
*!      Function:  lfvSM
*!
*!**************************************************************************
*
FUNCTION  lfvSM
*** calling the global function mover to select the source modules
=gfMover(@laSApp_Id,@laTApp_Id,"Source modules",.T.)
*** init. the string variable lcSrcmodul

lcSrcmodul =''

*** collect all source modules names in the string lcSrcmodul
FOR lnCounter = 1 TO ALEN(laTApp_ID,1)
   lcSrcmodul = lcSrcmodul + LEFT(laTApp_ID[lnCounter],3)
ENDFOR

*** check if there is no any source modules are selected
*** in this case disable all objects except (Close),(S.M.) objects.

IF ! EMPTY(lcSrcmodul)
  *** check if there is any transaction(s) found for this module(s)
  *** if there is no any transaction 
  *** display "No transactions for this source module(s).
  *** and disable all objects except (Close),(S.M.) objects.

  LOCATE FOR &lcScope  
    IF  FOUND()
      SHOW GETS ENABLE 
    ELSE 
      =gfModalGen("TRM02066B00000","Dialog")  
      SHOW GETS DISABLE
      SHOW GET  pbClose ENABLE
      SHOW GET  pbSM    ENABLE
    ENDIF  
ELSE   
  SHOW GETS DISABLE
  SHOW GET  pbClose ENABLE
  SHOW GET  pbSM    ENABLE
ENDIF

*** moves the current pointer (CP) to the first record that
*** matches the specified conditions in (lcScope)
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
  =gfModalGen("TRM02213B00000","ALART",IIF(llDumyPost,'include','approve')+'|'+'batch')
  RETURN .F.
ENDIF
