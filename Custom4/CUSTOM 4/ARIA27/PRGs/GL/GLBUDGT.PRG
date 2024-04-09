*:************************************************************************
*: Procedure file: GLBUDGT.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Renee Ezzat Ashamalla
*:      Copyright (c) 
*:  Last modified: 02/08/1994
*:
*:  
*:  Called from  : GL.PRG
*:  Calls        : GLBUDGT.SPR,GLBDGAD.SPR,GLBDGVW.SPR,GLBDGRF.SPR, 
*:                   GLBDGTR.SPR
*:
*:  Procs & Fncts: lfvData_1 
*:               : lfvData_2
*:               : lfvOk
*:               : lfvCancel
*:               : lfvData_9   
*:               
*:      Documented 02/08/1994   at   :  
*:************************************************************************
*E100243,1 Reham On 06/11/95
*E100243,1 Fix the budget detail browse under windows to display all the
*E100243,1 numeric fields on the screen without swaping.
*B600927,1 RENEE 01/14/96 Windows platform:
*B600927,1                When creating budget details from actual values,
*B600927,1                the linew are always created from the current 
*B600927,1                year's values even if the year is chnged.
*B600927,1                -Update puActYear validation in GLBDGAD.SCX 
*B601568,1 Hesham El-Sheltawi 03/10/97 Dos platform
*B601568,1 in the Account Details Screen when using the differnce push
*B601568,1 button it dont work properly and when changing the differnce
*B601568,1 popup it dont display the value that was selected
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*B803699,1 SSE 10/02/2000 Fix bug of having duplicate Account code records when 
*B803699,1                returning from Accounts Details screen.
*B602252,1 KAM 09/14/2000 change the type of edit region to field object
*B602252,1                in the files glbdgvw1,glbdgrf 
*B602252,4 SSE 01/11/2001 Fix bug 'Window GLBDGVW3 is not defined' when pressing Escape.
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField
DECLARE laKeyField [4,4]           && primary key fields 

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'BDCODYR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'BDCODYR'
laKeyField[2,4] = 2

laKeyField[3,1] = 'laData[1]'
laKeyField[3,2] =.T.
laKeyField[3,3] = 'BDYRCOD'
laKeyField[3,4] = 2
laKeyField[4,1] = 'laData[2]'
laKeyField[4,2] =.F.
laKeyField[4,3] = 'BDYRCOD'
laKeyField[4,4] = 1

laDefProc [7]   = .F.           && Use local Delete procedure 
laDefProc [9]   = .F.           && Use local Save   procedure 

*** Variables declaration and/or initialization :

*** array holding the elements of Actual Years popup in 'Add budget' ***
*** 'Details' screen (ibActYears, and under DOS, puActYear under WINDOWS) 
DECLARE laTFsYears[1]

*** array holding the elements of the 'Difference' popup ('Offset Diff') ***
DECLARE laTDiff[3,2]
laTDiff[1,1] = 'Budget-Actual'
laTDiff[1,2] = 1
laTDiff[2,1] = 'Budget-Reference'
laTDiff[2,2] = 2
laTDiff[3,1] = 'Actual-Reference'
laTDiff[3,2] = 3

*** external key fields, used by the local ***
*** external key procedure.(pfExtrnKey). ***
DECLARE laFields[1]

*** Used for temporary files creation. holds the file structure. ***
DECLARE laBdDtFlds[1]



*** Temporary files and alias names 
** lc_TmpBdg  : Name of the budget details temporary file
** lc_TmpBrow : Name of a temporary file holding the
**              details of a selected account, to be
**              recopied to the temporary file 
**              (lc_TmpBdg) in case the user 'Cancels'
**              the modifications done on this account,
**              through the 'Account details ' screen.
** lc_TmpRef  : Name of the temporary file of reference column in the browse window 
** lc_TmpDet  : Name of the temporary file containing the generated details 
** lc_SrcFNam : Name of the temporary file used by GLSLACT.PRG 
** lc_SrcFile : Name of the file containing accounts to be used for details creation.
** lc_Periods : Name of cursor holding 13 periods
** lc_AliasNm : Name of an alias used when openning lc_TmpBdg AGAIN
** lcBStamp   : Audit information of current record
** lcFieldStr : The list (lsBdgDet) field string 
** lcBdHdTag  : GLBUDHD.DBF current master tag
** lcBdDtExp  :
** lcAcBalExp : Master tag name of GLACBALS file.
** lcGrDtExp  :
** puActYear  :
** lcYear     : Year variable,used for the relation.
** lcAcctCode :
** lcAccDesc  :
** lcAcctRef  : Reference account code
** lcRefDesc  : Reference account description
** lcNewRef   :
** lcNewRfDsc :
** lcOldCode  :
** lcBudCode  : Budget code
** lcOldBCode :
** lcBudYear  :
** lcOldBYear :
** lcBudDesc  :
** lcOldBDesc : 
** lcGrpCode  :
** lcOldGCode :
** lcGrpDesc  :
** lcOldGDesc :
** lcActYear  :
** lcRefAYear : Reference account actual year,
** lcOldRYear : 

*** Browse variables 

** lcBrowTitl : Browse window title difference field title on the browse.
** lcBrowFlds :
** lcBrowAmnt : Field ( or field expression ) to be used in the browse
**              window for the Actuals and Reference columns.It depends
**              on the type of account, whether account type is 'A'ssets,
**              e'Q'uity, or 'L'iability.
** lcThermo   : Thermometer function call string.                     

*** Difference popup variables

** lcDiff     : Difference popup selection ( initial )
** lcDiffFld  : Difference field combination for the 'Difference' column
**              on the browse window,initially set to option : 'Budget-Actual'

** lcRepFld   : Transferred field(s) combination for the 'Transfer' push
**              button function, initially set to option : 'Reference to budget'

** lcAccRel   :
** lcOldYear  :
** lcFirstPrd : First period in the current fiscal year
** lcActStat  :

** lnTmpRcCnt : Number of records in th list
** lnRecNum   : Number of records to be added,replaced, or merged into budget
**              details ( divided by 13 for the list ).
** lnOldRec   : Previously selected record (position).
** lnSelected :
** lnTotRec   :
** lnFisYears : Number of years available in Fascal header file, contained 
**              in the array laTFsYears and displayed in popups puActYear,
**              puRefAYear ( Windows ) or ibActYear, ibRefAYEar ( DOS ).

** rbAddDet2  :
** lnOldRbDt2 : Old value for radio button (rbAddDet2) in 'Add budget details'
**              screen
** lnOldRbTrs : Old value of radio button 'Transfer' (rbTransfer)
** lnOldVal   :
** lnThermRec :

** lsBdgDet   : List variable ( pointer ) rbRefType : "Reference type" radio 
**              button initialization found in "GLBDGRF" screen called from
**              this screen.

** rbAddDet1  :
** rbAmounts  :
** puDiff     :
** puRefAYear :
** lnDiffOpt  : Selected option from the difference popup.
** puDiff     :
** puActYear  :

** llDiff     : Offset difference check box.
** llUpdateAl : Shows if all accounts' values are to be updated without
**              individual confirmation from the user.
** llTrans    : Transfer or Cancel flag, set ( or reset ) from GLBDGTR screen.
** llRef      : OK or Cancel flag, set ( or reset ) from GLBDGRF screen.
** llAccDet   : OK or Cancel flag, set ( or reset ) from GLBDGVW screen.
** llUpdated  : Used by glbdgrf.spr                                  

** ldCurrDate : Current date,used for 'Audit' information.

*** Temporary files and alias names 

STORE ''    TO lc_TmpBdg  , lc_TmpBrow , lc_TmpRef  , lc_TmpDet  ,;
               lc_SrcFNam , lc_SrcFile , lc_Periods , lc_AliasNm ,;
               lcBStamp   , lcFieldStr , lcBdHdTag  , lcBdDtExp  ,;
               lcAcBalExp , lcGrDtExp  , puActYear  , lcAcctRef  ,;
               lcRefDesc  , lcNewRef   , lcNewRfDsc , lcOldCode  ,;
               lcBudCode  , lcOldBCode , lcBudYear  , lcOldBYear ,;
               lcBudDesc  , lcOldBDesc , lcGrpCode  , lcOldGCode ,;
               lcGrpDesc  , lcOldGDesc , lcActYear  , lcRefAYear ,;
               lcOldRYear , lcBrowFlds , lcBrowAmnt , lcThermo   ,;
               lcDiff     , lcDiffFld  , lcRepFld   , lcAccRel   ,;
               lcOldYear  , lcFirstPrd
               
STORE 1     TO lsBdgDet   , rbRefType  , rbAddDet1  , rbAmounts  ,;
               puDiff     , puRefAYear , puDiff     , lnDiffOpt  

STORE lcCurr_Yer TO lcYear, puActYear

lcAcctCode = REPLICATE ('0',lnAcsSegSz)
lcAccDesc  = SPACE(60)
lcBrowTitl = 'Details'
ldCurrDate = DATE()
lcActStat  = " "

STORE 0     TO lnTmpRcCnt , lnRecNum   , lnOldRec   , lnSelected ,;
               lnTotRec   , lnFisYears , lnOldRbDt2 , lnOldRbTrs ,;
               rbAddDet2  , lnOldVal   , lnThermRec 

STORE .F.   TO llDiff     , llUpdateAl , llTrans    , llRef      ,;
               llAccDet   , llUpdated
               

*** Setup
IF !gfSetup()
  RETURN
ENDIF

lcUserName = gcUserName

*** Check if the Types of Accounts file is empty
SELECT GLTYPES
GO TOP
IF EOF()
  *** "The types and ranges have not been setup yet. You have to 
  *** define the accounts type and ranges first."
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF
 
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields TO laData BLANK

  *** Create a name for the temporary file holding the details
  lc_TmpBdg = gfTempName()

  *** Create a name for the temporary file holding the account details
  *** of a selected account upon entering  'Account details' screen
  lc_TmpBrow = gfTempName()

  *** Create a name for the temporary file holding the reference field
  *** of the browse window IN 'Account details' screen.
  lc_TmpRef  = gfTempName()

  *** Create a name for a temporary file to hold the created 
  *** details that are to replace the old ones.
  lc_TmpDet  = gfTempName()
 
  *** Create a name for a temporary file to hold the selected
  *** account codes to be used for details creation from 
  *** 'Account details' screen
  lc_SrcFNam = gfTempName()

  *** Create a name for a temporary file alias, to be opened agian
  *** in another work area (lc_TmpBdg)
  lc_AliasNm = gfTempName()

  *** Create a name for a cursor to hold 13 periods
  lc_Periods = gfTempName()

 
  *** Prepare lcFieldStr for the list 
  lcFieldStr = "SUBSTR(&lc_TmpBdg..cAcctcode,1,lnAcsSegSz)+' '+"+;
               "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,"+;
               "&lc_TmpBdg..cAcctcode,"+;
               "GLACCHAR.cAcctcode,'ACCTCODE'),1,70-lnAcsSegSz)"
   
*E300692,1 Change file name from sycfsprd to fsprd
  *SELECT SYCFSPRD
  SELECT FSPRD
*E300692,1 end
  SET ORDER TO TAG COMFYRPRDI
*E300692,1 Change file name from sycfsprd to fsprd
  *lcFirstPrd       = IIF(SEEK(gcPrnt_Cmp+lcCurr_Yer,'SYCFSPRD'),;
                               cFspPrdId,'01')
*E300789,7 [BEGIN]  
*  lcFirstPrd       = IIF(SEEK(gcPrnt_Cmp+lcCurr_Yer,'FSPRD'),;
                               cFspPrdId,'01')
  lcFirstPrd       = IIF(SEEK(lcCurr_Yer,'FSPRD'),cFspPrdId,'01')
*E300789,7 [END..]                                 
*E300692,1 end                                                              
  
  *** Get the current tag of the Budgets header file
  SELECT GLBUDHD
  lcBdHdTag = SYS(22)
  
  SELECT GLACBALS
  *** Set order to the following tag so as to be used for the relation.
  *** Tag expression is : 'cAcctCode+cFisfYear+cFspPrdId'
  *** ( Account code + Fiscal year + Fiscal period ) 
  SET ORDER TO TAG AccYrPrd
  lcAcBalExp = SYS(14,VAL(SYS(21)))

  SELECT GLACCHAR
  SET ORDER TO TAG AcctCode
  
  SELECT GLGRPHD  
  SET ORDER TO TAG GrpCode
  
  SELECT GLGRPDT  
  SET ORDER TO TAG GrCodAcc
  lcGrDtExp = SYS(14,VAL(SYS(21)))

  SELECT GLBUDDT   
  SET ORDER TO TAG CdYrPrAcc
  lcBdDtExp = SYS(14,VAL(SYS(21)))

  *** Create lc_TmpDet, lc_TmpBdg files with the same structure
  *** as GLBUDDT file as well as four more fields as follows :
  ***  - nPercent  N( 3)    for percent value 
  ***  - cTrn      C( 1)    ' û '/'   '(space : ALT-255)
  ***  - nRecNo    N(10)    for the record number
  ***  - cStatus   C(01)    status('M'odify,'D'elete,'A'dd,'S'ame)

  =AFIELDS(laBdDtFlds)
  lnBdDtFlds = ALEN(laBdDtFlds,1)
  DIMENSION laBdDtFlds[lnBdDtFlds+4,4]

  laBdDtFlds[lnBdDtFlds+1,1] = 'nPercent'
  laBdDtFlds[lnBdDtFlds+1,2] = 'N'
  laBdDtFlds[lnBdDtFlds+1,3] = 3
  laBdDtFlds[lnBdDtFlds+1,4] = 0

  laBdDtFlds[lnBdDtFlds+2,1] = 'cTrn'
  laBdDtFlds[lnBdDtFlds+2,2] = 'C'
  *E100243,1 Make the field width 1 instead of 3.
  *laBdDtFlds[lnBdDtFlds+2,3] = 3
  laBdDtFlds[lnBdDtFlds+2,3] = 1
  laBdDtFlds[lnBdDtFlds+2,4] = 0

  laBdDtFlds[lnBdDtFlds+3,1] = 'nRecNo'
  laBdDtFlds[lnBdDtFlds+3,2] = 'N'
  laBdDtFlds[lnBdDtFlds+3,3] = 10
  laBdDtFlds[lnBdDtFlds+3,4] = 0

  laBdDtFlds[lnBdDtFlds+4,1] = 'cStatus'
  laBdDtFlds[lnBdDtFlds+4,2] = 'C'  
  laBdDtFlds[lnBdDtFlds+4,3] = 1
  laBdDtFlds[lnBdDtFlds+4,4] = 0

  CREATE TABLE &gcWorkDir.&lc_TmpDet ;
      FROM ARRAY laBdDtFlds

  CREATE TABLE &gcWorkDir.&lc_TmpBdg ;
      FROM ARRAY laBdDtFlds

  *** Prepare an array holding all years for the popups : 'Actual years'
  *** called in 2 screens ( GLBDGAD.SPR,GLBDGRF.SPR )
  laTFsYears       = " "
  
*E300692,1 Change file name AND PATH from sycfISHD to fishd
  *SELECT DISTINCT cFisFYear;
   FROM  (gcSysHome+"SYCFisHd");
  WHERE  cComp_Id +cFisFYear = gcPrnt_Cmp;
   INTO  ARRAY laTFsYears ;
  ORDER  BY cFisFYear
*E300789,7 [BEGIN]
*  SELECT DISTINCT cFisFYear;
   FROM  (gcDataDir+"FisHd");
  WHERE  cComp_Id +cFisFYear = gcPrnt_Cmp;
   INTO  ARRAY laTFsYears ;
  ORDER  BY cFisFYear

  SELECT DISTINCT cFisFYear;
   FROM  (gcDataDir+"FisHd");
   INTO  ARRAY laTFsYears ;
  ORDER  BY cFisFYear
*E300789,7 [END..]
*E300692,1 end

  lnFisYears       = _TALLY

ENDIF

lcActStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(lnTmpRcCnt=0,"DISABLE","ENABLE"))

SELECT GLBUDHD

*E300683,5 Call *.SPR from screens directory
* DO GLBUDGT.SPR 
DO (gcScrDir + gcWinAppl + '\GLBUDGT.SPR')
*E300683,5 end   


IF glQuitting
  *** Close open temporary files and cursors.  
  IF USED(lc_TmpBdg)
    USE IN ALIAS(lc_TmpBdg)
  ENDIF
  IF USED(lc_TmpBrow)
    USE IN ALIAS(lc_TmpBrow)
  ENDIF
  IF USED(lc_TmpRef)
    USE IN ALIAS(lc_TmpRef)
  ENDIF
  IF USED(lc_TmpDet)
    USE IN ALIAS(lc_TmpDet)
  ENDIF
  IF USED(lc_SrcFNam)
    USE IN ALIAS(lc_SrcFNam)
  ENDIF
  IF USED(lc_Periods)
    USE IN ALIAS(lc_Periods)
  ENDIF

  ERASE &gcWorkDir.&lc_TmpBdg..dbf  
  ERASE &gcWorkDir.&lc_TmpBdg..cdx  
  ERASE &gcWorkDir.&lc_TmpBrow..dbf  
  ERASE &gcWorkDir.&lc_TmpRef..dbf  
  ERASE &gcWorkDir.&lc_TmpRef..cdx  
  ERASE &gcWorkDir.&lc_TmpDet..dbf  
  ERASE &gcWorkDir.&lc_SrcFNam..dbf  
ENDIF 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

*** Current fiscal year is the year used for relation setting in
*** both 'Add' and 'Edit' modes
lcYear = lcCurr_Yer

DO CASE

  *** "Select" mode (laScrMode[1]=.T.)
  CASE laScrMode[1]
    SELECT (lc_TmpBdg)
    lsBdgDet = 1
    SHOW GET lsBdgDet    
    
    *** Delete old data ( if any )
    ZAP
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsBdgDet    
    ON ERROR &lcErrSett.
    
    *** Initialize variables for display
    lnTmpRcCnt = 0    && No nonempty records in the temporary file
    lnOldRec   = 0
    laData[3]  = ''   && Budget description 
    
    SHOW GET pbViewDt DISABLE
    SHOW GET pbRem    DISABLE

  *** "View" mode (laScrMode[2]=.T.) ,or "Edit" mode (laScrMode[3]=.T.)
  CASE laScrMode[2].OR.laScrMode[3] 
    lcBStamp = IIF(laScrMode[2],cAdd_User+DTOC(dAdd_Date)+;
                   cAdd_Time,lcBStamp)
   
    *** Get an approximate total number of records
    *** if in 'EDIT' mode only.
    IF laScrMode[3]
      lnTotRec = IIF(lcBStamp = cAdd_User+DTOC(dAdd_Date)+;
                     cAdd_Time,RECCOUNT(lc_TmpBdg)*13,;
                     RECCOUNT('GLBUDDT'))
      IF lnTotRec > 0 
        lnTotRec = lnTotRec + 2
      ENDIF  

      lnThermRec = 0
    ENDIF
    lcThermo      = IIF(laScrMode[2] .OR. lnTotRec = 0 ,"",;
                        ".AND. lfThermo('Collecting budget lines...')")
    lcScope       = laData[1]+laData[2]+IIF(laScrMode[2],lcFirstPrd,'')
    
    SELECT GLBUDDT 
    
    *** Recreate file with appropriate details
    *E100243,1 Make the field value 'û' instead of ' û '.
    SELECT *,100 AS nPercent,'û' AS cTrn,;
           RECNO() AS 'nRecNo',"S" AS 'cStatus';
           FROM (gcDataDir+"GLBUDDT");
           INTO DBF &gcWorkDir.&lc_TmpBdg;
            WHERE &lcBdDtExp=lcScope &lcThermo
 
    *** Create indeces only in 'Edit' mode and 'Add' mode 
    IF laScrMode[3]
      SELECT (lc_TmpBdg)

      INDEX ON cAcctCode + cBudPrd TAG ACCTCODE
      
      *B803699,1 Change the index to be on cAcctCode only [Begin]
      *INDEX ON cAcctCode + cStatus TAG U_ACCTCODE UNIQUE ADDITIVE
      INDEX ON cAcctCode TAG U_ACCTCODE UNIQUE ADDITIVE
      *B803699,1 Change the index to be on cAcctCode only [End]
      
      SET ORDER TO TAG ACCTCODE
    
      SET RELATION TO cAcctCode+lcYear+cBudPrd INTO GLACBALS ADDITIVE
             
      *** Set order to unique tag for the list
      SET ORDER TO TAG U_ACCTCODE
    
      *** Get the number of records currently in the temporary file
      lnTmpRcCnt = RECCOUNT(lc_TmpBdg)/13     
    
    ELSE  
      lnTmpRcCnt = RECCOUNT(lc_TmpBdg)
    ENDIF
    
    lcAcctCode   = cAcctCode
    lcAccDesc    = LOOKUP(GLACCHAR.cAccnldes,cAcctcode,;
                          GLACCHAR.cAcctcode,'ACCTCODE')

    lcObjState   = IIF(lnTmpRcCnt=0.OR.laScrMode[2],"DISABLE","ENABLE")
    SHOW GET pbViewDt  &lcObjState.
    SHOW GET pbRem     &lcObjState.

    lcObjState   = IIF(laScrMode[2],"DISABLE","ENABLE")
    SHOW GET pbDetail  &lcObjState.
    
    lsBdgDet     = 1
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsBdgDet     
    ON ERROR &lcErrSett.

    *** Use a large increment for the thermometer if 'S'election
    *** is already finished.
    IF laScrMode[3] .AND. lnThermRec < lnTotRec
      DO WHILE lnThermRec < lnTotRec-39
        lnThermRec = lnThermRec + 39
        =gfThermo(lnTotRec,lnThermRec,"Collecting budget lines...",'')            
      ENDDO
      =gfThermo(lnTotRec,lnTotRec,"Collecting budget lines...",'')    
    ENDIF

  *** "Add" mode (laScrMode[4]=.T.) 
  CASE laScrMode[4] 

    *** Create indeces only in 'Edit' mode and 'Add' mode 
    *** if not previously created.
    SELECT (lc_TmpBdg)

    IF EMPTY(RELATION(1,lc_TmpBdg))

      INDEX ON cAcctCode + cBudPrd TAG ACCTCODE

      *B803699,1 Change the index to be on cAcctCode only [Begin]
      *INDEX ON cAcctCode + cStatus TAG U_ACCTCODE UNIQUE ADDITIVE
      INDEX ON cAcctCode TAG U_ACCTCODE UNIQUE ADDITIVE
      *B803699,1 Change the index to be on cAcctCode only [End]

      SET ORDER TO TAG ACCTCODE
   
      SET RELATION TO cAcctCode+lcYear+cBudPrd INTO GLACBALS ADDITIVE

    ENDIF         

    *** Set order to unique tag for the list
    SET ORDER TO TAG U_ACCTCODE
     
    *** Default budget description 
    laData[3]      = SUBSTR('Created by ' + lcUserName,1,;
                     FSIZE('cBudDes','GLBUDHD'))
    *** Adjust controls ( push buttons )
    SHOW GET lsBdgDet
ENDCASE

SELECT GLBUDHD 


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*  VALID function for the get field "laData[1]",
*  corresponding to the budget code (GLBUDHD.cBudCode) field
*  It represents one part of the primary key, the other part 
*  being the budget year field (GLBUDHD.cBudYear)
*
FUNCTION lfvData_1

IF !EMPTY(laData[1])
  laData[1] = ALLTRIM(laData[1])
  laData[1] = IIF(ISDIGIT(LEFT(laData[1],1)),;
                  RIGHT("0000000"+laData[1],8),;      
                  LEFT(laData[1]+"        ",8))
  SHOW GET laData[1] 
  IF LASTKEY()= 13   
    =gfSeekRec()
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*  VALID function for the get field "laData[2]" 
*  corresponding to the budget year ( GLBUDHD.cBudYear ) field
*  It represents one part of the primary key, the other part 
*  being the budget year field (GLBUDHD.cBudYear)
*
FUNCTION lfvData_2

IF LASTKEY()=13
  IF LEFT(ALLTRIM(laData[2]),1) = '?'
    laData[1]=SPACE(8) 
    laData[2]='?'
    =gfSeekRec()
  ELSE
    IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2])
      =gfSeekRec()
    ENDIF
  ENDIF   
ENDIF

*!**************************************************************************
*!
*!      Function: lswBdgDet
*!
*!**************************************************************************
*  WHEN function for the list "lsBdgDet" containing budget 
*  details in the main screen. (GLBudgt.SPR) 
*
FUNCTION lswBdgDet

*** IF There are no records in the list,
*** prohibit selection from list.
IF  lnTmpRcCnt=0 
  RETURN .F.
ENDIF  

SELECT (lc_TmpBdg)

*** Save the currently selected record position
*lnOldRec   = RECNO()
lcAcctCode  = cAcctCode
lcAccDesc   = LOOKUP(GLACCHAR.cAccnldes,cAcctcode,;
                     GLACCHAR.cAcctcode,'ACCTCODE')
                     
SELECT GLBUDHD

*!**************************************************************************
*!
*!      Function: lsvBdgDet
*!
*!**************************************************************************
*  VALID function for the list "lsBdgDet" containing budget 
*  details in the main screen. (GLBudgt.SPR) 
*
FUNCTION lsvBdgDet


*** Enable viewing and editing an account's details only in EDIT
*** mode and ADD mode.
IF laScrMode[3].OR.laScrMode[4]
  SELECT (lc_TmpBdg)

  *** Save the currently selected record position
  lcAcctCode = cAcctCode
  lcAccDesc  = LOOKUP(GLACCHAR.cAccnldes,cAcctcode,;
                      GLACCHAR.cAcctcode,'ACCTCODE')
  =lfvViewDt()               
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDetail
*!
*!**************************************************************************
*  VALID function for push button "Detail..." (pbDetail).
*  This function handles adding details to the current budget.
*  It branches to another screen (GLBDGAD.SPR) by calling another
*  program (GLBDGAD.PRG)
*
FUNCTION lfvDetail
PRIVATE lnHDRcNum	

SELECT GLBUDHD
lnHDRcNum = RECNO()
SET ORDER TO TAG BDCODYR
SET FILTER TO cBudCode+cBudYear <> laData[1]+laData[2]

SELECT (lc_TmpDet)
ZAP

*** Select the temporary file for the whole session,then
*** go back to GLBUDHD ( budgets header file ) when returning to
*** the calling program.

SELECT (lc_TmpBdg)

SET ORDER TO TAG ACCTCODE

rbAddDet1  = 1
rbAddDet2  = 0
puActYear  = lcCurr_yer    && under WINDOWS, initialize popup
                           && with current fiscal year
lcActYear  = lcCurr_yer    && under DOS, initialize popup
                           && 'SAY' field with current
                           && fiscal year
lnOldRbDt2 = 0             && Old value for radio button 
                           && (rbAddDet2) in 'Add budget detaisl' 
                           && screen

rbAmounts  = 1
lcYear     = lcCurr_yer    && year for the relation 
lcBudCode  = ''            && budget code
lcOldBCode = ''            && old budget code
lcBudYear  = ''            && budget year
lcOldBYear = ''            && old budget year
lcBudDesc  = ''            && budget description
lcOldBDesc = ''            && old budget description
lcGrpCode  = ''            && group code
lcOldGCode = ''            && old group code
lcGrpDesc  = ''            && group description
lcOldGDesc = ''            && old group description
lc_SrcFile = 'GLACCHAR'    && name of the temporary file used by
                           && GLSLACT.PRG ( default is GLACCHAR.DBF )
lnRecNum   = 0             && number of created accounts, 
                           && (each having 13 periods) to be 
                           && added to the budget.
*E300683,5 Call *.SPR from screens directory
* DO GLBDGAD.SPR 
DO (gcScrDir + gcWinAppl + '\GLBDGAD.SPR')
*E300683,5 end   
SELECT (lc_TmpBdg)

SET ORDER TO TAG U_ACCTCODE

IF lnTmpRcCnt>0
  SHOW GET pbViewDt   ENABLE
  SHOW GET pbRem      ENABLE
  GO TOP
ENDIF  

lsBdgDet   = 1
*lnOldRec   = RECNO()   

*** Refresh objects with contents of the current record,or spaces
*** if the list is empty, to be passes to GLBDGVW program,if called
lcAcctCode  = IIF(lnTmpRcCnt=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)

*** Update list contents 
SHOW GET lsBdgDet

_CUROBJ     = OBJNUM(lsBdgDet)

SELECT GLBUDHD

SET FILTER TO

IF !EMPTY(lcBdHdTag)
  SET ORDER TO TAG (lcBdHdTag)
ELSE
  SET ORDER TO
ENDIF    

IF lnHdRcNum <= RECCOUNT('GLBUDHD')
  GO lnHdRcNum 
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*    VALID function for push button "Remove" (pbRem).
*    This function handles removal of a selected account with all its
*    corresponding details ( for 13 periods ).Note that an account
*    code cannot be found twice in the same budget. 
*
FUNCTION lfvRem

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
  SELECT (lc_TmpBdg)  

  SET ORDER TO TAG ACCTCODE

  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1);
                  FOR cAcctCode+cBudPrd=lcAcctCode

  *** Decrement number of records in list
  lnTmpRcCnt = lnTmpRcCnt-1

  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE FOR cAcctCode+cBudPrd=lcAcctCode
  
  SET ORDER TO TAG U_ACCTCODE
  GO TOP
  
  lsBdgDet   = IIF(lnTmpRcCnt=0,1,IIF(lsBdgDet=lnTmpRcCnt+1,1,lsBdgDet))

  *** Adjust controls
  lcObjState = IIF(lnTmpRcCnt=0,"DISABLE","ENABLE")

  SHOW GET pbRem      &lcObjState.
  SHOW GET pbViewDt   &lcObjState.

  *** Update list contents 
  SHOW GET lsBdgDet
  _CUROBJ = OBJNUM(lsBdgDet)
  
  SELECT GLBUDHD
ENDIF


*!**************************************************************************
*!
*!      Function: lfShowRead
*!
*!**************************************************************************
*  This screen's READ SHOW clause function
* 
FUNCTION lfShowRead

SHOW GET rbAmounts,3 DISABLE

*!**************************************************************************
*!
*!      Function: lfAddDet1
*!
*!**************************************************************************
*  Valid function for the radio button rbAddDet
* 
FUNCTION lfAddDet1

DO CASE
  CASE rbAddDet2 = 1 
    *** Save old values.
    lcOldBCode = lcBudCode   
    lcOldBYear = lcBudYear   
    lcOldBDesc = lcBudDesc

    *** Clear objects
    STORE '' TO lcBudCode, lcBudYear, lcBudDesc
  
    SHOW GET lcBudCode   DISABLE
    SHOW GET lcBudYear   DISABLE
    SHOW GET lcBudDesc   DISABLE

    *** Restore old values
    lcBudCode = lcOldBCode   
    lcBudYear = lcOldBYear   
    lcBudDesc = lcOldBDesc

    *** If previous selection from  'Amounts from' radio button is
    *** 'budgets',choose 'Actuals' as a default instead
    IF rbAmounts = 3
      rbAmounts  =  1
      SHOW GET rbAmounts,1
    
      DO CASE
        CASE _DOS
          lcActYear = lcYear 
          SHOW GET ibActYear ENABLE
        CASE _WINDOWS
          SHOW GET puActYear ENABLE
      ENDCASE
    ENDIF  
  
    SHOW GET rbAmounts,3 DISABLE
  CASE rbAddDet2 = 2
  
    *** Save old values  
    lcOldGCode = lcGrpCode 
    lcOldGDesc = lcGrpDesc 
  
    *** Clear objects
    lcGrpCode  = ''
    lcGrpDesc  = ''  
  
    SHOW GET lcGrpCode   DISABLE
    SHOW GET lcGrpDesc   DISABLE

    *** Restore old values
    lcGrpCode  = lcOldGCode 
    lcGrpDesc  = lcOldGDesc 
ENDCASE

lnOldRbDt2 = 0
rbAddDet2  = 0
SHOW GET rbAddDet2

*** Say fields are found in the DOS version of the screen only
IF _DOS
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvAddDet1
*!
*!**************************************************************************
*  Valid function for the radio button rbAddDet
* 
FUNCTION lfvAddDet1
PRIVATE lcCurrArea

IF rbAddDet2 <>0
  =lfAddDet1()
ENDIF  
IF USED(lc_SrcFNam)
  lcCurrArea         = ALIAS()
  SELECT (lc_SrcFNam)
  ZAP
  SELECT (lcCurrArea)
ENDIF  

IF rbAddDet1 = 1
  lc_SrcFile = "GLACCHAR"
ELSE
  *** Parameters passed to GLSLACT.PRG :
  *** 1:  tempfile1 name,created by GLSLACT.PRG,returns a file 
  ***     containing account type  and description,has to be erased.
  *** 2:  tempfile2 name,not created,returns a file containing
  ***     the selected accounts according to a selected criteria.
  *** 3:  tempfile3 name,returns a file containing the selected
  ***     accounts,it has to be created before calling this
  ***     function.
  *** 4:  logical number of records in the 3rd temporary file )
  *** 5:  alias name for GLTYPES.DBF if it is to be opened
  ***     from another path, other than gcDataDir.,
  ***     in another work area .  
  *** 6:  alias name for GLACCHAR.DBF if it is to be opened
  ***     from another path, other than gcDataDir.,
  ***     in another work area . 
  *** 7:  logical .T.,if third file is to be used. 
  *** 8:  .T. if account mask is required.       
  
  lc_SrcFile       = lc_SrcFNam   
  *** lc_TmpRef file name is used here as a dummy.
  *** It is not used by this module, it is overwritten
  *** somewhere else in this module
  *E300683,5 Call programs from PRGS directory
  *DO GLSLACT WITH lc_TmpRef,lc_SrcFile,'',0,'','',.F.,.T.
  DO (gcAppHome + gcWinAppl + '\GLSLACT ') WITH lc_TmpRef,lc_SrcFile,'',0,'','',.F.,.T.
  *E300683,5 end


ENDIF

*!**************************************************************************
*!
*!      Function: lfvAddDet2
*!
*!**************************************************************************
*  Valid function for the radio button rbAddDet
* 
FUNCTION lfvAddDet2
PRIVATE lcCurrArea

IF rbAddDet1 <> 0
  rbAddDet1 = 0
  SHOW GET  rbAddDet1
ENDIF

IF USED(lc_SrcFNam)
  lcCurrArea = ALIAS()
  SELECT (lc_SrcFNam)
  ZAP
  SELECT (lcCurrArea)
ENDIF  

IF lnOldRbDt2 <> rbAddDet2 
  IF rbAddDet2 = 1
 
    *** Save old values  
    lcOldGCode = lcGrpCode 
    lcOldGDesc = lcGrpDesc 
  
    *** Clear objects
    lcGrpCode  = ''
    lcGrpDesc  = ''
  ELSE
    *** Save old values.
    lcOldBCode = lcBudCode   
    lcOldBYear = lcBudYear   
    lcOldBDesc = lcBudDesc

    *** Clear the objects for showing
    STORE '' TO lcBudCode, lcBudYear, lcBudDesc

    *** If previous selection from  'Amounts from' radio button is
    *** 'budgets',choose 'Actuals' as a default instead
    IF rbAmounts = 3
      rbAmounts = 1
      SHOW GET rbAmounts,1
      DO CASE
        CASE _DOS
          lcActYear = lcYear
          SHOW GET ibActYear ENABLE
        CASE _WINDOWS
          SHOW GET puActYear ENABLE
      ENDCASE
    ENDIF  
  ENDIF     

  *** Show objects as follows :
  *** If 'Budgets', enable Budget code, Budget year, and budget
  *** description fields, snd disable the others, 
  *** else, vice versa.
  lcObjState = IIF(rbAddDet2 = 1,"ENABLE","DISABLE")
  SHOW GET lcBudCode    &lcObjState.
  SHOW GET lcBudYear    &lcObjState.
  SHOW GET lcBudDesc    &lcObjState.
  SHOW GET rbAmounts,3  &lcObjState.

  lcObjState = IIF(rbAddDet2=2,"ENABLE","DISABLE")
  SHOW GET lcGrpCode    &lcObjState.
  SHOW GET lcGrpDesc    &lcObjState.

  *** Restore variables' contents
  IF rbAddDet2 = 1
    lcGrpCode  = lcOldGCode
    lcGrpDesc  = lcOldGDesc
  ELSE
    lcBudCode  = lcOldBCode 
    lcBudYear  = lcOldBYear
    lcBudDesc  = lcOldBDesc 
  ENDIF
  lnOldrbDt2   = rbAddDet2
ENDIF

*** Say fields are found in the DOS version of the screen only
IF _DOS
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvBudCode 
*!
*!**************************************************************************
*    VALID function for the get field "lcBudCode",
*    corresponding to the budget code ( GLBUDHD.cBudCode ) field
*    It represents the first part of a key used to search for
*    a budget
*
FUNCTION lfvBudCode

lcBudCode  = ALLTRIM(lcBudCode)
lcBudCode  = IIF(ISDIGIT(LEFT(lcBudCode,1)),;
                 RIGHT("0000000"+lcBudCode,8),;      
                 LEFT(lcBudCode+"        ",8))
SHOW GET lcBudCode

IF EMPTY(lcBudCode)
  lcBudDesc = ""
  SHOW GET lcBudDesc
ELSE  
  SELECT GLBUDHD
  SET ORDER TO TAG BDCODYR
  IF EMPTY(lcBudYear)
    IF LASTKEY() = 13
      =lfvBdCdYr('lcBudCode',2)
    ENDIF   
  ELSE  
    IF LEFT(lcBudCode,1) = '?'
      lcBudCode    = ''
      SET ORDER TO TAG BDYRCOD
      =lfvBdCdYr('lcBudYear',2)
    ELSE
      =lfvBdCdYr('lcBudCode+lcBudYear',2)
    ENDIF 
  ENDIF  
  IF !EMPTY(lcBdHdTag)
    SET ORDER TO TAG (lcBdHdTag)
  ELSE
    SET ORDER TO
  ENDIF      
ENDIF    

IF WONTOP('AWDGLBDGRF') .AND. lcBudCode <> lcOldCode 

  lcNewRef   = REPLICATE ('0',lnAcsSegSz)
  lcNewRfDsc = ""
  SHOW GET lcNewRef
  SHOW GET lcNewRfDsc
ENDIF

*!**************************************************************************
*!
*!      Function: lfvBudYear
*!
*!**************************************************************************
*    VALID function for the get field "lfBudYear" 
*    corresponding to the budget year ( GLBUDHD.cBudYear ) field
*    It represents the second ( and  last ) part of a key field
*
FUNCTION lfvBudYear

IF EMPTY(lcBudYear)
  lcBudDesc = ""
  SHOW GET lcBudDesc
ELSE
  SELECT GLBUDHD
  SET ORDER TO TAG BDYRCOD
  IF EMPTY(lcBudCode)
    IF LASTKEY() = 13
      =lfvBdCdYr('lcBudYear',2)
    ENDIF  
  ELSE  
    IF LEFT(lcBudYear,1) = '?'
      lcBudYear    = ''
      SET ORDER TO TAG BDCODYR
      =lfvBdCdYr('lcBudCode',2)
    ELSE
      =lfvBdCdYr('lcBudYear+lcBudCode',2)
    ENDIF 
  ENDIF  
  IF !EMPTY(lcBdHdTag)
    SET ORDER TO TAG (lcBdHdTag)
  ELSE
    SET ORDER TO
  ENDIF      
ENDIF

IF WONTOP('AWDGLBDGRF') .AND. lcBudYear <> lcOldCode 
  
  lcNewRef   = REPLICATE ('0',lnAcsSegSz)
  lcNewRfDsc = ""
  SHOW GET lcNewRef
  SHOW GET lcNewRfDsc
ENDIF

*!**************************************************************************
*!
*!      Function: lfvBdCdYr
*!
*!**************************************************************************
*    VALID function for the get fields "lcBudCode","lcBudYear
*    corresponding to the budget code ( GLBUDHD.cBudCode, GLBUDHD.cBudYear) 
*    fields.It represents the first part of a key used to search for
*    a budget
*
FUNCTION lfvBdCdYr
PARAMETERS lcObjName,lnObjNum

DECLARE laFields[3,3]

laFields[1,1] = 'lcBudCode'
laFields[1,2] = 'cBudCode'
laFields[1,3] = "'Budget code'"

laFields[2,1] = 'lcBudYear'
laFields[2,2] = 'cBudYear'
laFields[2,3] = "'Budget year'"
  
laFields[3,1] = 'lcBudDesc'
laFields[3,2] = 'cBudDes'
laFields[3,3] = "'Budget description'"

DO lpExtrnKey WITH lcObjName,'GLBUDHD',;
                   'Select a budget',lnObjNum
SHOW GET lcBudCode
SHOW GET lcBudYear
SHOW GET lcBudDesc

*!**************************************************************************
*!
*!      Function: lfvGrpCode
*!
*!**************************************************************************
*    VALID function for the get field "lcGrpCode",
*    corresponding to the gruop code ( GLGRPHD.cGrpCode ) field
*
FUNCTION lfvGrpCode

lcGrpCode  = ALLTRIM(lcGrpCode)
lcGrpCode  = IIF(ISDIGIT(LEFT(lcGrpCode,1)),;
                 RIGHT("0000000"+lcGrpCode,8),;      
                 LEFT(lcGrpCode+"        ",8))
SHOW GET lcGrpCode

SELECT GLGRPHD

*** Validate the given group code 
IF EMPTY(lcGrpCode)
  lcGrpDesc = ""
  SHOW GET lcGrpDesc
ELSE  
  DECLARE laFields[2,3]
  laFields[1,1] = 'lcGrpCode'
  laFields[1,2] = 'cGrpCode'
  laFields[1,3] = "'Group code'"

  laFields[2,1] = 'lcGrpDesc'
  laFields[2,2] = 'cGrpLnHed'
  laFields[2,3] = "'Group header'"

  DO lpExtrnKey WITH 'lcGrpCode','GLGRPHD','Select a group',1
  
  SHOW GET lcGrpCode
  SHOW GET lcGrpDesc
ENDIF

*!**************************************************************************
*!
*!      Function: lfvAmounts
*!
*!**************************************************************************
*  Valid function for the radio button rbAmounts in GLBDGAD screen
*  ('Add details' screen)
* 
FUNCTION lfvAmounts

*** Disable budget amounts year( lcActYear ) in the Budget amounts box  
*** if initializing with zero values.
DO CASE
  CASE _DOS
    IF rbAmounts = 1
      lcActYear  = lcYear
      SHOW GET ibActYear ENABLE
    ELSE
      lcActYear  = ''
      SHOW GET ibActYear DISABLE
    ENDIF 
    =lfRefresh()   
  CASE _WINDOWS
    lcObjState   = IIF(rbAmounts = 1 ,"ENABLE","DISABLE") 
    SHOW GET puActYear &lcObjState
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvRefYear
*!
*!**************************************************************************
*  Valid function for 'Years' popups in 'Reference account' screen
*  GLDBGRF branching from GLBDGVW screen, that is called from  
*  'Account details' branching push button found in the main screen
*  
FUNCTION lfvRefYear
PRIVATE lcDspYear

DO CASE
  CASE _DOS
    *B600384,1 Reham On 06/07/95
    *B600384,1 Remark this code & stop calling lfvActYear because it was
    *B600384,1 affecting the { lcRefAYear } value twice.
    *lcDspYear = lcRefAYear 
    *=lfvActYear(3,27,@lcRefAYear,'lcRefAYear')
    
    *B600384,1 Call the gfActPop directly & refresh the returned value.
    lcRefAYear = gfActPop(3,27,8,34,'laTFsYears',1,1,@lcRefAYear)       
    =lfRefresh()
    llUpdated  = IIF(!llUpdated .AND. lcRefAYear <> lcOldYear,.T.,llUpdated)
    
    *llUpdated = IIF(!llUpdated .AND. lcRefAYear <> lcDspYear,.T.,llUpdated)
  CASE _WINDOWS
    lcRefAYear = puRefAYear
    llUpdated  = IIF(!llUpdated .AND. lcRefAYear <> lcOldYear,.T.,llUpdated)
ENDCASE

IF WONTOP('AWDGLBDGRF') .AND. lcRefAYear <> lcOldYear
  
  lcNewRef   = REPLICATE ('0',lnAcsSegSz)
  lcNewRfDsc = ""
  SHOW GET lcNewRef
  SHOW GET lcNewRfDsc
ENDIF
  
*!**************************************************************************
*!
*!      Function: lfvActYear
*!
*!**************************************************************************
*  Valid function for 'Years' popups in 'Account details' screen 
*  that branches from the branching push button 'Add details' in
*  the main screen
* 
FUNCTION lfvActYear
PARAMETERS lnRow1,lnCol1,lcRetYear,lcDispObj
*** lnRow1    : the starting row for the popup under DOS
*** lnCol1    : the starting column for the popup under DOS
*** lcRetYear : Return value from the popup, a selected year
***             from array laTFsYears
** lcDispObj  : variable to hold the selection (used under DOS)
PRIVATE lcDispYear

lcDispYear = ''

DO CASE
  CASE _DOS
    lcRetYear  = gfActPop(lnRow1,lnCol1,;
                 IIF(lnFisYears >3,lnRow1+5,lnFisYears+lnRow1+2),;
                 lnCol1+7,'laTFsYears',1,1,@lcDispYear)       
    &lcDispObj = lcDispYear
    =lfRefresh()
  CASE _WINDOWS 
    lcRetYear  = puActYear
    *B600927,1 Fill variable lcActYear with the selected year
    &lcDispObj = puActYear
    *B600927,1 end.
ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
*  Valid function for the push button 'Proceed' ( pbProceed )
*  All the files processing is done in this function.
* 
FUNCTION lfvProceed

DO CASE
  CASE rbAddDet1 = 2
    *** If no accounts are generated, this may occur for example
    *** if the user chooses to 'Cancel' a selection from the chart
    *** of accounts.
    *** Message : "No accounts selected to proceed "
    ***                      < OK >
    IF !USED(lc_SrcFile) .OR. RECCOUNT(lc_SrcFile) = 0
      =gfModalGen("TRM02071B00000","Dialog")
      _CUROBJ = OBJNUM(rbAddDet1)         
      RETURN
    ENDIF
  CASE rbAddDet2 = 1
    *** If budget code field is empty,
    *** Message : "You have to enter the budget code"
    ***  <  OK  > ***
    *** and return to budget code field.
    IF EMPTY(lcBudCode)
      =gfModalGen("TRM02026B00000","Dialog","the budget code")
      _CUROBJ = OBJNUM(lcBudCode)         
      RETURN
    ELSE
      *** If budget year field is empty,
      *** Message : "You have to enter the budget year"
      *** <  OK  > ***
      *** and return to budget year field.
      IF EMPTY(lcBudYear)
        =gfModalGen("TRM02026B00000","Dialog","the budget year")
        _CUROBJ = OBJNUM(lcBudYear)         
        RETURN
      ENDIF
    ENDIF
  CASE rbAddDet2 = 2
    *** If group code field is empty,
    *** Message : "You have to enter the group code"
    *** <  Ok  > ***
    *** and return to group code field.
    IF EMPTY(lcGrpCode)
      =gfModalGen("TRM02026B00000","Dialog","the group code")
      _CUROBJ = OBJNUM(lcGrpCode)         
      RETURN
    ENDIF
ENDCASE

*** Confirm proceeding from the user 
*** Message : "Confirm proceed option ?"
*** <  Ok  > - < Cancel > ***
IF gfModalGen("QRM02055B02011","Dialog") = 1
  *** Generate budget details according to selected criterion
  =lfGenBdDt()
  IF lnRecNum > 0 
    *** Message : "account" 
    *** <  Add  > - < Cancel > ***
    IF gfModalGen("QRM02072B02008","Dialog",ALLTRIM(STR(lnRecNum))+IIF(lnRecNum=1,"| is generated","|s are generated"))=1
  
      SELECT (lc_TmpBdg) 

      *** Check if the budget already has details.   
      IF lnTmpRcCnt > 0 
    
        *** If it is ,present the following message
        *** Message :" The budget already has details"
        *** " < Replace > , < Merge  > , < Cancel > "
        lnOption   = gfModalGen("QRM02068B02012","Dialog")
       
        DO CASE
          *** If budget details are to be replaced :
          CASE lnOption=1
            *** Reset records counter
            lnTmpRcCnt = 0 
            
            *** Number of records of the target file before replacement
            lnOldRecs  = RECCOUNT(lc_TmpBdg)
 
            *** Number of physical records in the target file 
            *** ( current file ) after replacement
            lnTotRec   = lnOldRecs + RECCOUNT(lc_TmpDet)
 
            *** Delete all current budget lines after
            *** marking all records for deletion by changing
            *** their status such that
            ***  'M'odified         -----> 'D'eleted
            ***  'A'dded    (new)   -----> 'S'ame
            ***  'S'ame (unchanged) -----> 'D'eleted    
            SELECT (lc_TmpBdg)
            REPLACE ALL cStatus WITH ;
                    SUBSTR("DSD",AT(cStatus,"MAS"),1)
            DELETE ALL
  
            *** Append the new details from (lc_TmpDet)
            APPEND FROM &gcWorkDir.&lc_TmpDet;
               FOR gfThermo(lnTotRec,RECNO()-lnOldRecs,;
                         "Replacing budget lines...",cAcctCode)             

            IF WVISIBLE('GWDTHERMO')
              =gfThermo(lnTotRec,lnTotRec,"Replacing budget lines...",' ')
            ENDIF
          
          *** If current budget details are to be merged with new ones :
          CASE lnOption=2  
             
            lnTotRec   = RECCOUNT(lc_TmpDet) 
            llUpdateAl = .F.    
            
            SELECT (lc_TmpDet)
        
            SET RELATION TO cAcctCode+cBudPrd INTO (lc_TmpBdg) ADDITIVE
            
            GO TOP

            SCAN FOR cBudPrd = lcFirstPrd
                        
              lcAcct = cAcctCode  
             
              =gfThermo(lnTotRec,RECNO(),"Merging budget lines...",;
                        cAcctCode)
              
              *** If account exists   in budget details :
              IF !EOF(lc_TmpBdg)
                *** Account ð already exists in budget ð.
                *** Do you wish to update the budget amounts for
                *** this account ?
                *** < Update > < Skip > < Update All > < Skip All > ***
                IF !llUpdateAl
                  lnUpdate = gfModalGen("QRM02067B02010","Dialog",;
                     RTRIM(cAcctCode)+'|'+RTRIM(laData[1])+'\'+laData[2])                       
                  llUpdateAl = lnUpdate = 3 .OR. lnUpdate = 4
                ENDIF
                *** Decrement records number if the record is already
                *** found in the budget details
                lnRecNum = lnRecNum - 1
               
                *** If values are to be updated : 
                IF lnUpdate = 1 .OR. lnUpdate = 3
      
                  SELECT (lc_TmpBdg)
 
                  REPLACE  REST &lc_TmpBdg..nAmount WITH &lc_TmpDet..nAmount,;
                              &lc_TmpBdg..cStatus WITH SUBSTR("MAM",;
                                 AT(&lc_TmpBdg..cStatus,"MAS"),1);
                         WHILE &lc_TmpBdg..cAcctCode=lcAcct
            
                    SELECT (lc_TmpDet)
                  ENDIF   
              ELSE
                lnCurrRec = RECNO() 
                *** Add 13 details for every account ( already found
                *** in lc_TmpDet ) to lc_TmpBdg
                SCAN FOR cAcctCode + cBudPrd = lcAcct

                  SCATTER MEMVAR 

                  INSERT INTO (lc_TmpBdg);
                    FROM MEMVAR
                ENDSCAN
                
                IF lnCurrRec <= RECCOUNT(lc_TmpDet)
                  GO lnCurrRec
                ENDIF  
              ENDIF 
            ENDSCAN     
            SET RELATION OFF INTO (lc_TmpBdg)

            IF WVISIBLE('GWDTHERMO')
              =gfThermo(lnTotRec,lnTotRec,"Merging budget lines...",' ')
            ENDIF

          CASE lnOption=3  

            SELECT (lc_TmpDet)
            ZAP
            RETURN                                                                                                                                                                                                                          
        ENDCASE
      ELSE
        *** Number of records of the target file before replacement
        lnOldRecs  = RECCOUNT(lc_TmpBdg)
 
        *** Number of physical records in the target file 
        *** ( current file ) after replacement
        lnTotRec   = lnOldRecs + RECCOUNT(lc_TmpDet)
           
        APPEND FROM &gcWorkDir.&lc_TmpDet;
          FOR gfThermo(lnTotRec,RECNO()-lnOldRecs,"Adding budget lines...",;
                      cAcctCode)             
       
        IF WVISIBLE('GWDTHERMO')
          =gfThermo(lnTotRec,lnTotRec,"Adding budget lines...",'')
        ENDIF
      ENDIF    
    ELSE
      SELECT (lc_TmpDet)
      ZAP
      RETURN
    ENDIF  
    lnTmpRcCnt       = lnTmpRcCnt + lnRecNum
    CLEAR READ
  ELSE
    *** If no accounts are generated, this may occur for example
    *** if the user chooses a budget which does not have
    *** details.
    *** No accounts selected to proceed. ***
    *** <  OK  > ***
    =gfModalGen("TRM02071B00000","Dialog")
    lnRecNum = 0     
    RETURN
  ENDIF
  lnRecNum = 0 
ELSE
  RETURN
ENDIF    

*!**************************************************************************
*!
*!      Function: lfGenBdDt
*!
*!**************************************************************************
*  Generate budget details according to selected criterion
*  All the main files processing is done in this function.
* 
FUNCTION lfGenBdDt
PRIVATE lcAmnt

*** Initialize records counter.
lnRecNum   = 0
lnThermRec = 0

DO CASE
  *** If using actual values from the balances file ( GLACBALS ) for a 
  *** certain selected year ( lcActYear ) for the amounts field :
  CASE rbAmounts = 1
      
    *** Master tag of GLACBALs has previously been set for relation setting.
    DO CASE
      *** If using all accounts,( rbAddDet1 = 1 )
      *** or a selected subset of the accounts ( rbAddDet1 = 2 )
      CASE rbAddDet1 = 1 .OR. rbAddDet1 = 2 
        *** Set a relation between the source file containing the accounts 
        *** to be used for the budget details and the balances file.

        SELECT (lc_SrcFile)
        lnTotRec   = RECCOUNT(lc_SrcFile)
        
        SET RELATION TO &lc_SrcFile..cAcctCode+lcActYear ;
                INTO GLACBALS ADDITIVE
        
        SCAN 
          =gfThermo(lnTotRec,RECNO(lc_SrcFile),"Generating budget lines...",;
                   cAcctCode)
         
          *** If the type of account is : 'A'ssets,'L'iability,e'Q'uity
          *** or statistical('Y'),use the value of field GLACBALS.nAcbClBal
          *** else use the difference between two fields as follows :
          *** GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr
          lcAmnt   = IIF(LEFT(&lc_SrcFile..cTypeCode,1) $ 'ALQY',;
                               "GLACBALS.nAcbClBal",;
                               "GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr")

          lnPeriod = 0
          lcAcct   = cAcctCode
          lnRecNum = lnRecNum + 1
          
          SELECT GLACBALS

          SCAN REST WHILE cAcctCode+cfisfyear+cFspPrdId;
                  =&lc_SrcFile..cAcctCode+lcActYear
            lnPeriod = lnPeriod + 1

            *E100243,1 Make the field value 'û' instead of ' û '.
            INSERT INTO (lc_TmpDet);
                (cBudCode,cBudYear,cBudPrd,cAcctCode,nAmount,;
                 cAdd_User,dAdd_Date,cAdd_time,nPercent,cTrn,cStatus);
                VALUES (laData[1],laData[2],GLACBALS.cFspPrdId,;
                        &lc_SrcFile..cAcctCode,&lcAmnt.,;
                        gcUser_ID,ldCurrDate,gfGetTime(),100,'û','A')
          ENDSCAN
          SELECT (lc_TmpDet)
          
          IF lnPeriod < 13
            FOR lnCount = lnPeriod+1 TO 13
              APPEND BLANK
              *E100243,1 Make the {trn} field value 'û' instead of ' û '.
              REPLACE cBudCode  WITH laData[1],;
                      cBudYear  WITH laData[2],;
                      cBudPrd   WITH RIGHT('0'+LTRIM(STR(lnCount)),2),;
                      cAcctCode WITH lcAcct,;        
                      nAmount   WITH 0.00,;
                      cAdd_User WITH gcUser_ID,;
                      dAdd_Date WITH ldCurrDate,;
                      cAdd_Time WITH gfGetTime(),;
                      nPercent  WITH 100,;
                      cTrn      WITH 'û',;
                      cStatus   WITH 'A'                                          
            ENDFOR
          ENDIF  
          SELECT (lc_SrcFile)
        ENDSCAN
                
      *** If using accounts from a selected budget 
      CASE rbAddDet2 = 1
        lnTotRec = RECCOUNT('GLBUDDT')
        *** Set a relation between the source file containing the accounts 
        *** to be used for the budget details and the balances file.
        SELECT GLBUDDT

        SET RELATION TO GLBUDDT.cAcctCode+lcActYear+GLBUDDT.cBudPrd ;
                INTO GLACBALS ADDITIVE
        
        SEEK lcBudCode+lcBudYear
                
        SCAN REST WHILE cBudCode+cBudYear=lcBudCode+lcBudYear
                
          lnRecNum = lnRecNum + 1

          =gfThermo(lnTotRec,lnRecNum,"Generating budget lines...",;
                     cAcctCode)

          *** If the type of account is : 'A'ssets,'L'iability,e'Q'uity
          *** or statistical('Y'),use the value of field GLACBALS.nAcbClBal
          *** else use the difference between two fields as follows :
          *** GLACBALS.nAcbPTDDr-GLACBALS.nAcbPTDCr
          lcAmnt   = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,;
                         GLBUDDT.cAcctCode,;
                         GLACCHAR.cAcctCode,'ACCTCODE'),1) $ 'ALQY',;
                         "GLACBALS.nAcbClBal",;
                         "GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr")
        SELECT (lc_TmpDet)
        APPEND BLANK
        *E100243,1 Make the {trn} field value 'û' instead of ' û '.
        REPLACE   cBudCode  WITH laData[1],;
                  cBudYear  WITH laData[2],;
                  cBudPrd   WITH GLBUDDT.cBudPrd,;
                  cAcctCode WITH GLBUDDT.cAcctCode,;        
                  nAmount   WITH &lcAmnt.,;
                  cAdd_User WITH gcUser_ID,;
                  dAdd_Date WITH ldCurrDate,;
                  cAdd_Time WITH gfGetTime(),;
                  nPercent  WITH 100,;
                  cTrn      WITH 'û',;
                  cStatus   WITH 'A'                                          
          SELECT GLBUDDT 
        ENDSCAN
        lnRecNum   = lnRecNum/13
      *** If using accounts from a selected 'group'
      CASE rbAddDet2 = 2
        SELECT GLGRPDT
        lnTotRec   = RECCOUNT('GLGRPDT')
 
        SET RELATION TO GLGRPDT.cAcctCode +lcActYear;
                INTO GLACBALS ADDITIVE
        
        SEEK lcGrpCode
        SCAN REST WHILE cGrpCode=lcGrpCode 
          lnRecNum = lnRecNum + 1
          
          =gfThermo(lnTotRec,lnRecNum,"Generating budget lines...",cAcctCode)

          *** If the type of account is : 'A'ssets,'L'iability,
          *** or e'Q'uity,use the value of field GLACBALS.nAcbClBal
          *** else use the difference between two fields as follows :
          *** GLACBALS.nAcbPTDDr-GLACBALS.nAcbPTDCr
          lcAmnt   = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,;
                         GLGRPDT.cAcctCode,;
                         GLACCHAR.cAcctCode,'ACCTCODE'),1) $ 'ALQY',;
                         "GLACBALS.nAcbClBal",;
                         "GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr")
          lnPeriod = 0

          SELECT GLACBALS
          SCAN REST WHILE cAcctCode+cfisFyear+cFspPrdId;
                  =GLGRPDT.cAcctCode+lcActYear
            lnPeriod = lnPeriod + 1

            *E100243,1 Make the {trn} field value 'û' instead of ' û '.
            INSERT INTO (lc_TmpDet);
                (cBudCode,cBudYear,cBudPrd,cAcctCode,nAmount,;
                 cAdd_User,dAdd_Date,cAdd_time,nPercent,cTrn,cStatus);
               VALUES (laData[1],laData[2],GLACBALS.cFspPrdId,;
                        GLGRPDT.cAcctCode,&lcAmnt.,;
                        gcUser_ID,ldCurrDate,gfGetTime(),100,'û','A')
          ENDSCAN
          IF lnPeriod < 13
            SELECT (lc_TmpDet)
            FOR lnCount = lnPeriod+1 TO 13
              APPEND BLANK
              *E100243,1 Make the {trn} field value 'û' instead of ' û '.
              REPLACE cBudCode  WITH laData[1],;
                  cBudYear  WITH laData[2],;
                  cBudPrd   WITH RIGHT('0'+LTRIM(STR(lnCount)),2),;
                  cAcctCode WITH GLGRPDT.cAcctCode,;        
                  nAmount   WITH 0.00,;
                  cAdd_User WITH gcUser_ID,;
                  dAdd_Date WITH ldCurrDate,;
                  cAdd_Time WITH gfGetTime(),;
                  nPercent  WITH 100,;
                  cTrn      WITH 'û',;
                  cStatus   WITH 'A'                                          
            ENDFOR
          ENDIF  
          SELECT GLGRPDT
        ENDSCAN
    ENDCASE  
     
    *** Set relation off in GLACBALs,and set previous tag 
    SET RELATION OFF INTO GLACBALS
    
  *** If using Zero values for details
  CASE rbAmounts = 2
    lcFSize    = REPLICATE('0',FSIZE('nAmount','GLBUDDT')-3)
    *** Create records for the accounts each having 13 periods,
    *** and initialize their amounts with zero values.
    DO CASE

      *** If using all accounts,( rbAddDet1 = 1 )
      *** or a selected subset of the accounts ( rbAddDet1 = 2 )
      CASE rbAddDet1 = 1 .OR. rbAddDet1 = 2 
        
        lnTotRec   = MAX(RECCOUNT(lc_SrcFile),14)
*        lnTotRec   = RECCOUNT(lc_SrcFile)+1
        =lfDoCursor() 
        lnRecNo = 0
        *E100243,1 Make the {trn} field value 'û' instead of ' û '.
        SELECT laData[1] AS 'cBudCode',laData[2] AS 'cBudYear',;
               &lc_Periods..cPeriod AS 'cBudPrd',;
               &lc_SrcFile..cAcctCode AS 'cAcctCode',;
               VAL(lcFSize) AS 'nAmount',gcUser_ID AS 'cAdd_User',;
               ldCurrDate AS 'dAdd_Date',gfGetTime() AS 'cAdd_Time', ;
               .f. AS 'lLok_Stat','' AS 'cLok_User',;
               {} AS 'dLok_Date','' AS 'cLok_Time',100 AS 'nPercent',;
               'û' AS cTrn, 0 AS 'nRecNo' ,'A' AS 'cStatus' ;
            FROM &lc_SrcFile.,&lc_Periods.;
            WHERE gfThermo(lnTotRec,RECNO(),"Generating budget lines...",;
                 cAcctCode);
            INTO DBF &gcWorkDir.&lc_TmpDet.
       
      CASE rbAddDet2 = 1
        lnTotRec = RECCOUNT('GLBUDDT')
        *E100243,1 Make the {trn} field value 'û' instead of ' û '.
        SELECT laData[1] AS 'cBudCode',laData[2] AS 'cBudYear',;
               cBudPrd,cAcctCode,VAL(lcFSize) AS 'nAmount',;
               gcUser_ID AS 'cAdd_User',ldCurrDate AS 'dAdd_Date',;
               gfGetTime() AS 'cAdd_Time',.f. AS 'lLok_Stat',;
               '' AS 'cLok_User',{} AS 'dLok_Date','' AS 'cLok_Time',;
               100 AS 'nPercent','û' AS cTrn,0 AS 'nRecNo' ,;
               'A' AS 'cStatus' ;
            FROM GLBUDDT;
           WHERE &lcBdDtExp = lcBudCode+lcBudYear;
               .AND. gfThermo(lnTotRec,RECNO(),;
               "Generating budget lines...",cAcctCode);
            INTO DBF &gcWorkDir.&lc_TmpDet.
      CASE rbAddDet2 = 2
        lnTotRec   = RECCOUNT('GLGRPDT')*13
        =lfDoCursor() 
        *E100243,1 Make the {trn} field value 'û' instead of ' û '.
        SELECT laData[1] AS 'cBudCode',laData[2] AS 'cBudYear',;
               &lc_Periods..cPeriod AS 'cBudPrd',;
               GLGRPDT.cAcctCode AS 'cAcctCode',;
               VAL(lcFSize) AS 'nAmount',gcUser_ID AS 'cAdd_User',;
               ldCurrDate AS 'dAdd_Date',gfGetTime() AS 'cAdd_Time',;
               .f. AS 'lLok_Stat','' AS 'cLok_User',;
               {} AS 'dLok_Date','' AS 'cLok_Time',;
               100 AS 'nPercent','û' AS cTrn,0 AS 'nRecNo' ,;
               'A' AS 'cStatus' ;
            FROM GLGRPDT,&lc_Periods;
            WHERE &lcGrDtExp=lcGrpCode;
              .AND. gfThermo(lnTotRec,RECNO(),;
               "Generating budget lines...",cAcctCode);
              INTO DBF &gcWorkDir.&lc_TmpDet.
  
    ENDCASE  
    lnRecNum       = _TALLY/13
  CASE rbAmounts = 3

    lnTotRec       = RECCOUNT('GLBUDDT')
    *** Create records for the accounts from a selected budget,using its
    *** budget amounts.
    *E100243,1 Make the {trn} field value 'û' instead of ' û '.
    SELECT laData[1] AS 'cBudCode',laData[2] AS 'cBudYear',;
           cBudPrd,cAcctCode,nAmount,gcUser_ID AS 'cAdd_User',; 
           ldCurrDate AS 'dAdd_Date',gfGetTime() AS 'cAdd_Time',;
           .f. AS 'lLok_Stat','' AS 'cLok_User',;
           {} AS 'dLok_Date','' AS 'cLok_Time',;
           100 AS 'nPercent','û' AS cTrn,0 AS 'nRecNo' ,;
           'A' AS 'cStatus' ;
       FROM GLBUDDT;
       WHERE &lcBdDtExp=lcBudCode+lcBudYear;
          .AND. gfThermo(lnTotRec,RECNO(),;
          "Generating budget lines...",cAcctCode);
       INTO DBF &gcWorkDir.&lc_TmpDet.;
       ORDER BY cAcctCode

    lnRecNum       = _TALLY/13
ENDCASE     

IF WVISIBLE('GWDTHERMO')
  =gfThermo(lnTotRec,lnTotRec,"Generating budget lines...",' ')
ENDIF

*!**************************************************************************
*!
*!      Function: lfDoCursor
*!
*!**************************************************************************
* Create a cursor with 1 field and 13 periods
*
FUNCTION lfDoCursor

CREATE CURSOR (lc_Periods) (cPeriod C(2))

FOR lnCount=1 TO 13 
  INSERT INTO (lc_Periods) ;
         VALUES (IIF(lnCount<10,'0'+STR(lnCount,1),STR(lnCount,2)))
ENDFOR

*:************************************************************************
*:
*:      Function: lfvViewDt
*:
*:************************************************************************
*  Valid function for the push buton 'View' ( pbViewDt ).
*  Also called from lsvBdgDet()  
*
FUNCTION lfvViewDt
PRIVATE lnHdRcNum	

*** Save the current record number 
lnCurrRec = RECNO(lc_TmpBdg)
lcYear    = lcCurr_Yer 

*** Account code and description are selected in the calling program.
*** Reference code is initially set to the current account code.
lcAcctRef  = lcAcctCode   && reference account code
lcRefDesc  = lcAccDesc

lcBudCode  = ''            && Budget code
lcBudYear  = ''
lcOldBCode = ''
lcOldBYear = ''
lcOldRYear = lcCurr_Yer
lcRefAYear = lcCurr_Yer   && reference year initialized to the
                          && current fiscal year.Current fiscal 
                          && year value (lcCurr_Yer) is passed from
                          && GL.prg ( the main prg for GL module )
puRefAYear = lcCurr_Yer   && reference year initialized to the
                          && current fiscal year.Current fiscal 
                          && year value (lcCurr_Yer) is passed from
                          && GL.prg ( the main prg for GL module )
*** Browse variables 
lcBrowAmnt         = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,lcAcctCode,;
                         GLACCHAR.cAcctCode,'ACCTCODE'),1) $ 'ALQY',;
                         "GLACBALS.nAcbClBal",;
                         "GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr")
                                  && field ( or field expression ) to be 
                                  && used in the browse window for the
                                  &&  Actuals and Reference columns.It
                                  &&  depends on the type of account ,
                                  && whether account type is 'A'ssets,
                                  && e'Q'uity, or 'L'iability

DO CASE
  CASE _DOS                     
    lcBrowFlds = "&lc_TmpBdg..cBudPrd:H='Pd':R:W=.F.,"+;
                 "nPercent:H=' % ':P='999':W= !EMPTY(cTrn)"+;
                 ".AND.lfwUpdtBdg(nPercent):F:V=lfvPercent(),"+;
                 "cTrn:H='Trn':P='@M û,',"+;
                 "&lc_TmpBdg..nAmount:15:H='Budget '+laData[2]"+;
                 ":F:V=lfvUpdtBdg(),"+;
                 "nActual = IIF((&lcBrowAmnt.)<>0,(&lcBrowAmnt.),0.00)"+;
                 ":15:H='Actual '+lcCurr_Yer,"+;   
                 "nRefs   = IIF(&lc_TmpRef..nAmount<>0,"+;
                 "&lc_TmpRef..nAmount,0.00):15:H='Reference '+"+;
                 "IIF(rbRefType=1,lcRefAYear,lcBudYear),"+;   
                 "nDiff   = lfGetDiffs() :14:H=lcDiff"
  CASE _WINDOWS
    *E100243,1 Add a new browse field for windows to decrease the width
    *E100243,1 of some of the numeric fields & the {trn}
    lcBrowFlds = "&lc_TmpBdg..cBudPrd:H='Pd':R:W=.F.,"+;
                 "nPercent:H=' % ':P='999':W= !EMPTY(cTrn)"+;
                 ".AND.lfwUpdtBdg(nPercent):F:V=lfvPercent(),"+;
                 "cTrn:1:H='T':P='@M û,',"+;
                 "&lc_TmpBdg..nAmount:15:H='Budget '+laData[2]"+;
                 ":F:V=lfvUpdtBdg(),"+;
                 "nActual = IIF((&lcBrowAmnt.)<>0,(&lcBrowAmnt.),0.00)"+;
                 ":13:H='Actual '+lcCurr_Yer,"+;   
                 "nRefs   = IIF(&lc_TmpRef..nAmount<>0,"+;
                 "&lc_TmpRef..nAmount,0.00):13:H='Ref. '+"+;
                 "IIF(rbRefType=1,lcRefAYear,lcBudYear),"+;   
                 "nDiff   = lfGetDiffs() :13:H=lcDiff"
ENDCASE
*** Difference popup variables
lcDiff    = 'Budget-Actual'  
                            && difference popup selection ( initial )
puDiff    =  1

lcDiffFld = '&lc_TmpBdg..nAmount-(&lcBrowAmnt.)'
                               && Difference field combination for
                               && the 'Difference' column on the browse
                               && window,initially set to option :
                               && 'Budget-Actual'
lcRepFld  = '&lc_TmpRef..nAmount'
                               && Transferred field(s) combination for
                               && the 'Transfer' push button function,
                               && initially set to option :
                               && 'Reference to budget'
lnDiffOpt = 1                  && selected option from the difference 
                               && popup
llAccDet  = .T.

*** Controls
llDiff    = .F.                && Offset difference check box
rbRefType = 1                  && "Reference type" radio button 
                               && initialization found in "GLBDGRF"
                               &&  screen called from this screen.

*** Initialize radio button 'Transfer' ( rbTransfer )  to default
*** which is the second option (' Reference to budget' )
rbTransfer = 2

*** Local Setup
SELECT GLBUDHD

lnHdRcNum = RECNO('GLBUDHD')
SET ORDER TO TAG BDCODYR

*** Store the current details for the current account.
 SELECT *;
    FROM &gcWorkDir.&lc_TmpBdg;
    INTO DBF &gcWorkDir.&lc_TmpBrow;
    WHERE cAcctcode+cBudPrd = lcAcctCode

SELECT (lc_TmpBdg)
SET ORDER TO TAG ACCTCODE

*** Create temporary file for the reference field of the browse window
=lfCreaRef()

*** Store keys :
PUSH KEY
ON KEY LABEL Ctrl+ENTER  DO lpCtrlEntr
ON KEY LABEL ESC         DO lpEscape

ON KEY LABEL TAB         DO lpTab
ON KEY LABEL BACKTAB     DO lpShiftTab

ON KEY LABEL Ctrl+Q      lnDummy = 1
ON KEY LABEL Ctrl+W      lnDummy = 1
ON KEY LABEL Ctrl+END    lnDummy = 1


*E300683,5 Call *.SPR from screens directory
* DO GLBDGVW.SPR 
DO (gcScrDir + gcWinAppl + '\GLBDGVW.SPR')
*E300683,5 end   

*B602252,4 Return all saved ON KEYS from Stack [Begin]
POP KEY
*B602252,4 Return all saved ON KEYS from Stack [End]

*** Clean up
SELECT (lc_TmpBdg)
SET RELATION OFF INTO (lc_TmpRef)

IF !llAccDet
  SELECT (lc_TmpBrow)
  SET RELATION TO cAcctCode+cBudPrd INTO (lc_TmpBdg) ADDITIVE
  REPLACE ALL &lc_TmpBdg..nAmount  WITH &lc_TmpBrow..nAmount,;
              &lc_TmpBdg..nPercent WITH &lc_TmpBrow..nPercent,;
              &lc_TmpBdg..cTrn     WITH &lc_TmpBrow..cTrn 
  SELECT (lc_TmpBdg)
  SET RELATION OFF INTO (lc_TmpBrow)
ENDIF

SET ORDER TO TAG U_ACCTCODE
IF lnCurrRec <= RECCOUNT(lc_TmpBdg)
  GO lnCurrRec  
ENDIF

SELECT GLBUDHD
IF !EMPTY(lcBdHdTag)
  SET ORDER TO TAG (lcBdHdTag)
ELSE
  SET ORDER TO
ENDIF    

IF lnHdRcNum <= RECCOUNT('GLBUDHD')
  GO lnHdRcNum 
ENDIF

SELECT(lc_TmpBdg)    && New by Mohamed 
SHOW GET lsBdgDet    && New by Mohamed 
SELECT GLBUDHD       && New by Mohamed 

*!**************************************************************************
*!
*!      Function: lfvDiff
*!
*!**************************************************************************
*  Valid function for the difference popup   
* 
FUNCTION lfvDiff
*B601568,1 Hesham El-Sheltawi (Start)
*B601568,1 New variable to be use in the popup selection
*B601568,1 and take the value if greater than 0
*PRIVATE lnDOldRec
PRIVATE lnDOldRec,lnOldDiffOpt
*B601568,1 Hesham El-Sheltawi (End)

DO CASE
  CASE _DOS
  *B601568,1 Hesham El-Sheltawi (Start)
  *B601568,1 take the value of popup selection if greater than 0
*    lnDiffOpt = gfActPop(4,54,9,75,'laTDiff',2,1,@lcDiff)  
    lnOldDiffOpt = gfActPop(4,54,9,75,'laTDiff',2,1,@lcDiff)  
    lnDiffOpt = IIF(lnOldDiffOpt = 0,lnDiffOpt,lnOldDiffOpt)
    *B601568,1 Hesham El-Sheltawi (End)
    *B601568,1 Hesham El-Sheltawi (Start)
    *B601568,1 Change the calling of the .SPR refresh function
*    =lfRefresh()  
     =lfDosRef()
    *B601568,1 Hesham El-Sheltawi (End)     
  CASE _WINDOWS
    lnDiffOpt = puDiff  
    lcDiff    = laTDiff[puDiff,1]
ENDCASE    

*** Prepare field combination to be used by the browse window
*** as well as the 'transfer'ring function, if called  
DO CASE
  CASE lnDiffOpt = 1           && Budget - Actual
    lcDiffFld    = IIF(llDiff,;
                       '(&lcBrowAmnt.)-&lc_TmpBdg..nAmount',;
                       '&lc_TmpBdg..nAmount-(&lcBrowAmnt.)')
  
  CASE lnDiffOpt = 2           && Budget - Reference
    lcDiffFld    = IIF(llDiff,;
                       '&lc_TmpRef..nAmount-&lc_TmpBdg..nAmount',; 
                       '&lc_TmpBdg..nAmount-&lc_TmpRef..nAmount')  
            
  CASE lnDiffOpt = 3           && Actual - Reference
    lcDiffFld    = IIF(llDiff,;
                       '&lc_TmpRef..nAmount-(&lcBrowAmnt.)',;
                       '(&lcBrowAmnt.)-&lc_TmpRef..nAmount')
ENDCASE 

lnDOldRec  = RECNO(lc_TmpBdg)

*** Refresh the browse window with the new Difference column.
SHOW WINDOW (lcBrowTitl) REFRESH 

SELECT (lc_TmpBdg)
IF lnDOldRec <= RECCOUNT(lc_TmpBdg) .AND. lnDOldRec <> 0
  GO lnDOldRec
ENDIF

*!**************************************************************************
*!
*!      Function: lfvOfsDiff
*!
*!**************************************************************************
*  Valid function for the push button "Difference"  
*  It inverts the sign of the difference column.
* 
FUNCTION lfvOfsDiff
PRIVATE lnDOldRec, lnMinusPos   && the 'Minus' sign ('-') position in the string

llDiff  = !llDiff
*** Swap right and left substrings of the string holding the
*** replacement fields ( lcDiffFld) in case of 'Transfer'ring data
*** from one column to another through the 'Transfer' push button.
DO CASE
  CASE lnDiffOpt = 1           && Budget - Actual
    lcDiffFld    = IIF(llDiff,;
                       '(&lcBrowAmnt.)-&lc_TmpBdg..nAmount',;
                       '&lc_TmpBdg..nAmount-(&lcBrowAmnt.)')
  
  CASE lnDiffOpt = 2           && Budget - Reference
    lcDiffFld    = IIF(llDiff,;
                       '&lc_TmpRef..nAmount-&lc_TmpBdg..nAmount',; 
                       '&lc_TmpBdg..nAmount-&lc_TmpRef..nAmount')  
            
  CASE lnDiffOpt = 3           && Actual - Reference
    lcDiffFld    = IIF(llDiff,;
                       '&lc_TmpRef..nAmount-(&lcBrowAmnt.)',;
                       '(&lcBrowAmnt.)-&lc_TmpRef..nAmount')
ENDCASE
*B601568,1 Hesham El-Sheltawi (Start)
*B601568,1 Change the Sign Variable value to '-' instead of ' - '
*B601568,1 Becuase in the diffrence array it dont have any traile
*B601568,1 blanks
*lcMinusSign = ' - '
lcMinusSign = '-'
*B601568,1 Hesham El-Sheltawi (End)    
lnMinusLen  = LEN(lcMinusSign)
  
*** Swap right and left substrings of the array elements fo the 
*** popup 'Difference'
FOR lnCount = 1 TO ALEN(laTDiff,1)
  lnMinusPos         = AT(lcMinusSign,laTDiff[lnCount,1])
  laTDiff[lnCount,1] = SUBSTR(laTDiff[lnCount,1],lnMinusPos+lnMinusLen)+;
                        lcMinusSign+LEFT(laTDiff[lnCount,1],lnMinusPos-1)         
ENDFOR

*** Swap right and left substrings of the 'Difference' column title
*** in the browse window as well as 'lcDiff' od popup 'Difference'
lcDiff  = laTDiff[lnDiffOpt,1]

DO CASE
  CASE _DOS
    *B601568,1 Hesham El-Sheltawi (Start)
    *B601568,1 Change the calling of the .SPR refresh function
*    =lfRefresh()
     =lfDosRef()
    *B601568,1 Hesham El-Sheltawi (End)    
  CASE _WINDOWS
    SHOW GET puDiff  
ENDCASE

*** Refresh the browse window with the new Difference column.
lnDOldRec = RECNO(lc_TmpBdg)

SHOW WINDOW (lcBrowTitl) REFRESH 

SELECT (lc_TmpBdg)
IF lnDOldRec <= RECCOUNT(lc_TmpBdg) .AND. lnDOldRec <> 0
  GO lnDOldRec
ENDIF

*!**************************************************************************
*!
*!      Function :  lfvTrans
*!
*!*******************************************************************************
* Valid function for push button "Transfer" ( pbTrans )
* 
FUNCTION lfvTrans
PRIVATE lnCurrRec

lnCurrRec = RECNO(lc_TmpBdg)

llTrans = .T.

*** Initialize radio button 'Transfer' ( rbTransfer )  to default
*** which is the second option (' Reference to budget' )
lnOldRbTrs = rbTransfer

PUSH KEY
ON KEY

*E300683,5 Call *.SPR from screens directory
* DO GLBDGTR.SPR 
DO (gcScrDir + gcWinAppl + '\GLBDGTR.SPR')
*E300683,5 end   
POP KEY
*** Transfer the selected fields according to selected options.

IF llTrans
  DO CASE
    
    *** If transferring from 'Actual' column to 'Budget' column 
    CASE rbTransfer  = 1
      lcRepFld = lcBrowAmnt
    
    *** If transferring from 'Reference' column to 'Budget' column 
    CASE rbTransfer  = 2
      lcRepFld = '&lc_TmpRef..nAmount'
    
    *** If transferring from 'Difference' column to 'Budget' column 
    CASE rbTransfer  = 3
      lcRepFld = lcDiffFld
  ENDCASE

  REPLACE nAmount  WITH IIF(nPercent<>100.00,;
                            (&lcRepFld.)*nPercent/100.00,&lcRepFld.),;
          cStatus  WITH SUBSTR("MAM",AT(cStatus,"MAS"),1);
          FOR cAcctCode+cBudPrd=lcAcctCode .AND. !EMPTY(cTrn)
  
  IF lnCurrRec <= RECCOUNT() .AND. lnCurrRec <> 0
    GO lnCurrRec 
  ENDIF
ELSE
  rbTransfer = lnOldRbTrs
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCanc
*!
*!**************************************************************************
* Valid function for the push button "< Cancel >", 
* Called from 'Transfer from' screen ( GLBDGTR ),
*             ' Select reference account' screen ( GLBDGRF ),
*             "Account details' screen ( GLBDGVW )
* upon pressing push button < Cancel > 
FUNCTION lfvCanc
PARAMETERS llCanc

llCanc = .F.

*!**************************************************************************
*!
*!      Function :  lfvNegNum
*!
*!*******************************************************************************
FUNCTION  lfvPercent

IF nPercent < 0
  *** Negative values are not allowed. ***
  =gfModalGen("TRM02036B00000","Dialog")  
  REPLACE nPercent WITH lnOldVal
  RETURN 
ENDIF

*!**************************************************************************
*!
*!      Function :  lfwBrow
*!
*!*******************************************************************************
*
FUNCTION  lfwBrow

BROWSE FIELDS &lcBrowFlds;
         KEY lcAcctCode ;
         SAVE;
         NOAPPEND;
         NODELETE;
         NOCLEAR;
         NOMENU;
         TITLE lcBrowTitl;
         WINDOW GLBDGVW2 IN WINDOW AWDGLBDGVW

*!**************************************************************************
*!
*!      Function: lfwUpdtBdg
*!
*!**************************************************************************
* Called by the browse command, 
*
FUNCTION lfwUpdtBdg
PARAMETERS lnCurrntVal

lnOldVal = lnCurrntVal
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfvUpdtBdg
*!
*!**************************************************************************
* Called by the browse command, 
*
FUNCTION lfvUpdtBdg

REPLACE cStatus  WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)

*!**************************************************************************
*!
*!      Function: lfGetDiffs
*!
*!**************************************************************************
* Called by the browse command, forms the 'Difference' column of the
* browse window .
*
FUNCTION lfGetDiffs

RETURN (&lcDiffFld.)

*!**************************************************************************
*!
*!      Function :  lfActRef
*!
*!******************************************************************************"
*   Activate function for screen "GLBDGRF"
* 
FUNCTION lfActRef

lcObjState = IIF(rbRefType = 1,"DISABLE","ENABLE")
SHOW GET lcBudCode &lcObjState
SHOW GET lcBudYear &lcObjState

lcObjState = IIF(rbRefType = 1,"ENABLE","DISABLE")
SHOW GET ibRefAYear &lcObjState
SHOW GET puRefAYear &lcObjState

*!**************************************************************************
*!
*!      Function :  lfvRef
*!
*!*******************************************************************************
*   Valid function for the push button "Reference",( pbRef)
*   It branches to screen "GLBDGRF"
* 
FUNCTION lfvRef
PRIVATE lnOldRType,lnROldRec

llRef      = .T.
llUpdated  = .F.     

*** The following variables are used through GLBDGRF session.
lcNewRef   = REPLICATE ('0',lnAcsSegSz)
lcNewRfDsc = SPACE(60)
lcRef_Act  = lcNewRef
lcRef_Bdg  = lcNewRef
lcRDsc_Act = lcNewRfDsc
lcRDsc_Bdg = lcNewRfDsc

*** Store old values
*** Keep the current selection to be restored if Cancelling in GLBDGRF 
*** session.
lcCncBCode  = lcBudCode
lcCncBYear  = lcBudYear
lcCncRYear  = lcRefAYear  

lnOldRType  = rbRefType
lnOldType   = 0
lnROldRec   = RECNO(lc_TmpBdg)

PUSH KEY
ON KEY

*E300683,5 Call *.SPR from screens directory
* DO GLBDGRF.SPR 
DO (gcScrDir + gcWinAppl + '\GLBDGRF.SPR')
*E300683,5 end   
POP KEY

*** If choosing to use the selected account as a reference by pressing
*** push button < OK > of the Reference account screen,
IF llRef
  SHOW GET lcAcctRef
  SHOW GET lcRefDesc
  
  *** Choose the reference field according to the final selection
  *** of the radio button ( rbRefType)

  =lfCreaRef()
  
  SELECT (lc_TmpBdg)
  IF lnROldRec <= RECCOUNT(lc_TmpBdg) .AND. lnROldRec <> 0
    GO lnROldRec
  ENDIF

  SHOW WINDOW (lcBrowTitl) REFRESH  TOP
ELSE
  lcBudCode  = IIF(lnOldRType=1,"",lcCncBCode)
  lcBudYear  = IIF(lnOldRType=1,"",lcCncBYear)
  lcRefAYear = IIF(lnOldRType=2,"",lcCncRYear)
  rbRefType  = lnOldRType
ENDIF
  
*!**************************************************************************
*!
*!      Function :  lfvRefType
*!
*!*******************************************************************************
*
FUNCTION lfvRefType


IF lnOldType <> rbRefType
  IF rbRefType = 1
    lcOldBCode = lcBudCode
    lcOldBYear = lcBudYear
    lcBudCode  = ''            && Budget code
    lcBudYear  = ''
    lcRefAYear = lcOldRYear
    lcNewRef   = lcRef_Act
    lcNewRfDsc = lcRDsc_Act 

  ELSE
    lcOldRYear = lcRefAYear
    lcBudCode  = lcOldBCode
    lcBudYear  = lcOldBYear
    lcRefAYear = ''
    lcNewRef   = lcRef_Bdg
    lcNewRfDsc = lcRDsc_Bdg 
   
  ENDIF

  =lfActRef()
  SHOW GET lcNewRef
  SHOW GET lcNewRfDsc

  IF _DOS
    =lfRefresh()   
  ENDIF
  lnOldType = rbRefType
ENDIF

*!**************************************************************************
*!
*!      Function :  lfvRefAcct
*!
*!*******************************************************************************
*
FUNCTION lfvRefAcct
PRIVATE lcTargtFil,lnRecNum 

*** This condition is true only if the account code had an old entry
*** and now it is emptied,just ignore the entry.
IF !('?' $ lcNewRef) .AND. VAL(STRTRAN(lcNewRef,'-','')) = 0 
  lcNewRef = lcOldCode
  SHOW GET lcNewRef
  RETURN 
ENDIF  

IF lfvRefBdgt()

  *** If selecting a reference account from 'Actual' values ( GLACBALS file )
  IF rbRefType = 1
      
    *** GLACBALS' master tag is already set.
    SELECT GLACCHAR
    SET RELATION TO cAcctCode+lcRefAYear INTO GLACBALS ADDITIVE
    lcTargtFil = 'GLACBALS'
     	      
  *** If selecting a reference account from a budget  
  ELSE
    *** If it's the current budget, use the latest form of its data, 
    *** which is stored in the temporary file (lc_TmpBdg)
    IF lcBudCode = laData[1] .AND. lcBudYear = laData[2]
    
  *    lnRecNum = RECNO(lc_TmpBdg)
    
      SELECT 0
      USE &gcWorkDir.&lc_TmpBdg AGAIN ALIAS &lc_AliasNm ORDER AcctCode
     
      SELECT GLACCHAR
      SET RELATION TO cAcctCode INTO (lc_AliasNm) ADDITIVE
      lcTargtFil =  lc_AliasNm
    ELSE
    
      SELECT GLACCHAR
      SET RELATION TO lcBudCode+lcBudYear+lcFirstPrd+cAcctCode INTO GLBUDDT ADDITIVE
      lcTargtFil = 'GLBUDDT'  
    ENDIF
  ENDIF

  SET FILTER TO !EOF(lcTargtFil)
  GO TOP

  DECLARE laFields[2,3]

  laFields[1,1] = 'lcNewRef'
  laFields[1,2] = 'cAcctCode'
  laFields[1,3] = "'Account code'"

  laFields[2,1] = 'lcNewRfDsc'
  laFields[2,2] = 'cAccNlDes'
  laFields[2,3] = "'Account description'"
  
  DO lpExtrnKey WITH 'lcNewRef','GLACCHAR','Select an account code',1
  
  IF rbRefType = 1
    lcRef_Act  = lcNewRef
    lcRDsc_Act = lcNewRfDsc
  ELSE
    lcRef_Bdg  = lcNewRef 
    lcRDsc_Bdg = lcNewRfDsc
  ENDIF    
   
   
  SHOW GET lcNewRef
  SHOW GET lcNewRfDsc

  SET FILTER TO 
  SET RELATION OFF INTO (lcTargtFil)

  SELECT (lc_TmpBdg)

  IF lcTargtFil = lc_AliasNm
  *  GO lnRecNum
    USE IN (lc_AliasNm)
  ENDIF    
ENDIF

*!**************************************************************************
*!
*!      Function :  lfvRefOK
*!
*!*******************************************************************************
*
FUNCTION lfvRefOk

IF VAL(STRTRAN(lcNewRef,'-','')) = 0 
  *** If any change has occured in any object (llUpdated is .T.)
  *** go to the account code field and browse
  IF llUpdated 
    IF !lfvRefBdgt() 
      RETURN
    ENDIF   
    *** You have to enter an account code. ***
    *** <  Ok  > ***
    *** and return to budget code field.
    =gfModalGen("TRM02026B00000","Dialog","an account code")
    _CUROBJ  = OBJNUM(lcNewRef)
    RETURN
  ELSE
    llRef    = .F.   
    CLEAR READ
    RETURN
  ENDIF
ENDIF
lcAcctRef  = lcNewRef
lcRefDesc  = lcNewRfDsc 
CLEAR READ
  
*!**************************************************************************
*!
*!      Function :  lfvRefBdgt
*!
*!*******************************************************************************
*
FUNCTION lfvRefBdgt

IF rbRefType = 2 
    
  *** If budget code field is empty,
  *** You have to enter the budget code. ***
  *** < OK > ***
  *** and return to budget code field.
  IF EMPTY(lcBudCode)
    =gfModalGen("TRM02026B00000","Dialog","the budget code")
    _CUROBJ  = OBJNUM(lcBudCode)         
    RETURN .F.
  ELSE
    *** If budget year field is empty,
    *** You have to enter the budget year. ***
    *** <  Ok  > ***
    *** and return to budget year field.
    IF EMPTY(lcBudYear)
      =gfModalGen("TRM02026B00000","Dialog","the budget year")
      _CUROBJ  = OBJNUM(lcBudYear)         
      RETURN .F.
    ENDIF
  ENDIF
  RETURN .T.
ENDIF  

*!**************************************************************************
*!
*!      Function :  lfCreaRef
*!
*!*******************************************************************************
*
*   Creates a temporary file holding the reference column's data 
*
FUNCTION lfCreaRef
PRIVATE lnRecNo,lcAmnt

IF rbRefType = 1
  lcAmnt  = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,lcAcctRef,;
                GLACCHAR.cAcctCode,'ACCTCODE'),1) $ 'ALQY',;
                "GLACBALS.nAcbClBal",;
                "GLACBALS.nAcbPtdDr-GLACBALS.nAcbPtdCr")
  
  SELECT cFspPrdId AS cPeriod,&lcAmnt. AS nAmount ;
      FROM GLACBALS;
      WHERE &lcAcBalExp=lcAcctRef+lcRefAYear;
      INTO DBF &gcWorkDir.&lc_TmpRef
ELSE
  
  IF lcBudCode = laData[1] .AND. lcBudYear = laData[2]

    lnRecNo = RECNO(lc_TmpBdg)
  
    SELECT cBudPrd AS cPeriod,nAmount ;
      FROM &lc_TmpBdg;
      WHERE cAcctCode+cBudPrd = lcAcctRef;
      INTO DBF &gcWorkDir.&lc_TmpRef       

    SELECT (lc_TmpBdg)
    IF lnRecNo <= RECCOUNT(lc_TmpBdg) .AND. lnRecNo <> 0
      GO lnRecNo
    ENDIF
  ELSE  
    SELECT cBudPrd AS cPeriod,nAmount ;
      FROM GLBUDDT;
      WHERE &lcBdDtExp = lcBudCode+lcBudYear;
           .AND. cAcctCode = lcAcctRef;
      INTO DBF &gcWorkDir.&lc_TmpRef       
  ENDIF
ENDIF

SELECT (lc_TmpRef)      
INDEX ON cPeriod TAG PERIOD

SELECT (lc_TmpBdg)
SET RELATION TO cBudPrd INTO (lc_TmpRef) ADDITIVE

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in daDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr

SELECT (lc_TmpBdg)
SET ORDER TO TAG ACCTCODE
GO TOP
IF EMPTY(&lc_TmpBdg..cBudCode)
  *** You cannot save budget without details. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02218B00000","DIALOG")
  llCSave = .F.
  _CUROBJ = OBJNUM(pbDetail)
  RETURN
ENDIF

SELECT GLBUDHD

*** If adding a new record,append a blank one
IF laScrMode[4]
  APPEND BLANK
  =gfObj_Lock(.T.) 
ENDIF

*** Store laData values in the current record
GATHER FROM laData fields &lcScFields.

*** Now save the data in the temporary file using the following global
*** function which performs correct translation from the temporary
*** file lc_TmpBdg,and the main file GLBUDDT
=gfTmp2Mast("GLBUDDT",lc_TmpBdg,'Saving Budget '+RTRIM(laData[1])+'\'+laData[2]+' ...')  

SELECT (lc_TmpBdg)
SET ORDER TO TAG U_ACCTCODE
 
SELECT GLBUDHD

IF laScrMode[4]
  =gfObj_Lock(.F.)
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!*******************************************************************************
*    This procedure handles deletion,instead of the global procedure.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete procedure.
*
PROCEDURE lpDelScr 

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if 
*** SET('DELETED')='ON'
IF GLBUDHD.cBudCode <> laData[1] .OR. GLBUDHD.cBudYear <> laData[2] 
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  =gfModalGen("TRM00095B00000","ALERT")
  
  *** Go back to 'Select' mode
  laScrMode    = .F.
  laScrMode[1] = .T.
  RETURN
ENDIF  

lnTotRec    = RECCOUNT(lc_TmpBdg) * 13+1
lnThermRec  = 0

*** Delete records belonging to the current budget from the master
*** file (GLBUDDT)
*** The temporary file lc_TmpBdg is zapped in 'Select' mode
SELECT GLBUDDT
DELETE FOR &lcBdDtExp. = laData[1]+laData[2] ;
         .AND. lfThermo(;
            'Deleting Budget '+RTRIM(laData[1])+'\'+laData[2]+' ...')

SELECT GLBUDHD
*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

IF lnThermRec < lnTotRec
  =gfThermo(lnTotRec,lnTotRec,;
            'Deleting Budget '+RTRIM(laData[1])+'\'+laData[2]+' ...','')
ENDIF

*** Return to "SELECT" mode
laScrMode    = .F.
laScrMode[1] = .T.

*!**************************************************************************
*!
*!      Function : lfThermo
*!
*!*******************************************************************************
*  This function calls global function gfThermo (thermometer)
*  It takes as a parameter a counter to be incremented and tested 
*  at every call. The thermometer is called at increments of 13
*  instead of 1 for faster processing.
*
FUNCTION lfThermo
PARAMETERS lcThermStr

lnThermRec = lnThermRec + 1
IF lnThermRec % 13 = 0
  =gfThermo(lnTotRec,lnThermRec,lcThermStr,'')
ENDIF  

*!**************************************************************************
*!
*!      Procedure: lpEscape
*!
*!*******************************************************************************
*  This procedure is called whenever the ESC key is pressed during
*  editing 'Account details' ('Account details screen , GLBDGVW )
*  It replaces the default ESC setting for this session .
*
PROCEDURE lpEscape

ACTIVATE WINDOW GLBdgVw3
_CUROBJ  = OBJNUM(pbCanc)
KEYBOARD '{ENTER}'

*!**************************************************************************
*!
*!      Procedure: lpCtrlEntr
*!
*!*******************************************************************************
*  This procedure is called whenever the Ctrl+ENTER key is pressed during
*  editing 'Account details' ('Account details screen , GLBDGVW )
*  It replaces the default Ctrl+ENTER setting for this session .
*
PROCEDURE lpCtrlEntr

ACTIVATE WINDOW GLBdgVw3
_CUROBJ            = OBJNUM(pbAccDetOK)
KEYBOARD '{ENTER}'

*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!*******************************************************************************
*  This procedure is called whenever the Tab key is pressed during
*  editing 'Account details' ('Account details screen , GLBDGVW )
*  It replaces the default Tab setting for this session .
*
PROCEDURE lpTab

DO CASE
  *** If current object is the last object ( ibDiff under DOS, or 
  *** puDiff under WINDOWS ) in the first ( upper ) window ( GLBDGVW1 ),
  *** activate the next window which is the browse window (lcBrowTitl) 
  CASE WONTOP('GLBDGVW1') .AND.((_DOS.AND._CUROBJ = OBJNUM(ibDiff));
      .OR. (_WINDOWS.AND._CUROBJ = OBJNUM(puDiff)))      
    ACTIVATE WINDOW (lcBrowTitl)
  
  *** If current object is the browse ( current window is the browse 
  *** window 'lcBrowTitl' ),activate the next object, which is the first
  *** object in the next window 
  CASE WONTOP(lcBrowTitl)   
    ACTIVATE WINDOW GLBdgVw3
    _CUROBJ = OBJNUM(pbAccDetOK)

  *** If current object is the last object ( pbCanc 'Cancel' ) in the 
  *** last ( lower ) window ( GLBDGVW3 ), activate the next object
  *** which is the first object in the first window (lcAccDesc) 
  CASE !(TYPE('pbCanc') $ 'UL') AND _CUROBJ = OBJNUM(pbCanc)
    ACTIVATE WINDOW GLBdgVw1
    _CUROBJ = OBJNUM(lcAccDesc)
  
  *** Otherwise, go to the next object
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!***************************************************************************
*  This procedure is called whenever the Shift+Tab key is pressed during
*  editing 'Account details' ('Account details screen , GLBDGVW )
*  It replaces the default Shift+Tab setting for this session .
*
PROCEDURE lpShiftTab

DO CASE
  *** If current object is the first object ( lcAccDesc 'Account 
  *** description' ) in the first ( upper ) window ( GLBDGVW1 ),
  *** activate the last object in the last window (GLBDGVW3)
  CASE _CUROBJ = OBJNUM(lcAccDesc)
    ACTIVATE WINDOW GLBdgVw3
    _CUROBJ  = OBJNUM(pbCanc)  

  *** If current object is the browse ( current window is the browse 
  *** window 'lcBrowTitl' ),activate the previous object, which is the 
  *** last object in the previous window (GLBdgVw1)
  CASE WONTOP(lcBrowTitl)   
    ACTIVATE WINDOW GLBdgVw1
    lcCurObj  = IIF(_DOS,"ibDiff","puDiff")
    _CUROBJ   = OBJNUM(&lcCurObj.)

  *** If current object is the first object ( pbAccDetOk 'Ok' ) in the 
  *** last ( lower ) window ( GLBDGVW3 ), activate the previous object
  *** which is the browse in the browse window (lcBrowTitl) 
  CASE WONTOP('GLBDGVW3') .AND. _CUROBJ = OBJNUM(pbAccDetOk)
    ACTIVATE WINDOW (lcBrowTitl)

  *** Otherwise, go to the previous object
  OTHERWISE
    _CUROBJ  = _CUROBJ - 1
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpExtrnKey
*!
*!***************************************************************************
PROCEDURE lpExtrnKey
PARAMETERS lcObjName,lcExtFile,lcFileTitl,lnObjNum

*** lcObjName  : search string to be used for seeking
*** lcExtFile  : current file name
*** lcFileTitl : title to be used with the browse window
*** lnObjNum   : number of fields forming the external key

*** Array laFields is formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name
*** Column 3 : the logical field name (field header to be used
***            in the browse window.

*** Save current work area alias
lcCurFile = ALIAS()
lcCurrObj = SYS(18)

*** Number of fields to be browsed is the number of rows
*** of array laFields
lnNmOfFlds = ALEN('laFields',1)

*** Prepare an array to hold the returned values according to 
*** selected fields, first initialize it.
DECLARE laRetInfo[lnNmOfFlds,1]
laRetInfo = ""

*** Transfer data to be searched for to laRetInfo

FOR lnCount = 1 TO lnObjNum
  laRetInfo[lnCount] = &laFields[lnCount,1]
ENDFOR

*** Save old values
lcSaveBrow = lcBrFields
lcSavFTitl = lcFile_Ttl
  
*** Initialize 
lcBrFields = ''
lcFlds     = ''

*** Form the browse fields strings lcBrFields, lcFlds
FOR lnCount = 1 TO lnNmOfFlds
  lcBrFields = lcBrFields+ laFields[lnCount,2]+':H='+;
               laFields[lnCount,3]+','
  lcFlds     = lcFlds + laFields[lnCount,2]+','
ENDFOR
lcBrFields   = SUBSTR(lcBrFields,1,LEN(lcBrFields)-1)
lcFlds       = SUBSTR(lcFlds,1,LEN(lcFlds)-1)
lcFile_Ttl   = lcFileTitl

SELECT (lcExtFile)

*** Search for the current value (search string) in the
*** selected file

IF SEEK(&lcObjName,lcExtFile)
  
  *** If an exact match is found,
  *** If a one key field, or a complete two key field,  
  *** fill the return information array
  *** with values as required by array 'laFields'
  IF lnObjNum = 1 .OR. ('+' $ lcObjName) 
    FOR lnCount = 1 TO lnNmOfFlds
      &laFields[lnCount,1] = &lcExtFile..&laFields[lnCount,2]
    ENDFOR
    
    *** Restore old values
    lcBrFields = lcSaveBrow
    lcFile_Ttl = lcSavFTitl

    IF !llUpdated
      llUpdated = .T.
    ENDIF       
    SELECT (lcCurFile)
    RETURN
  ELSE  
    *** Call global function gfBrows
    =gfBrows(lcObjName,lcFlds,"laRetInfo")  
  ENDIF   
ELSE
  *** else, go to the closest matching record if one is found, or
  *** go to the top of the file .
  IF RECNO(0) > 0 .AND. RECNO(0) < RECCOUNT(lcExtFile)
    GO RECNO(0)
  ELSE
    GO TOP
  ENDIF
  
  *** Call global function gfBrows
  =gfBrows(.F.,lcFlds,"laRetInfo")  
ENDIF  

*** Restore old values
lcBrFields = lcSaveBrow
lcFile_Ttl = lcSavFTitl

*** Check if the user pressed ESC or Cancelled
IF (lnObjNum = 2 .AND. &laFields[1,1] = laRetInfo[1].AND.;
      &laFields[2,1] = laRetInfo[2]) .OR.;
      (lnObjNum = 1 .AND. &laFields[1,1] = laRetInfo[1])
  &lcCurrObj = lcOldCode   
  SELECT (lcCurFile)
  RETURN
ENDIF    
    
*** Else copy returned objects to their corresponding fields
IF !llUpdated
  llUpdated = .T.
ENDIF       
FOR lnCount = 1 TO lnNmOfFlds
  &laFields[lnCount,1] = laRetInfo[lnCount]
ENDFOR
             
SELECT (lcCurFile)


