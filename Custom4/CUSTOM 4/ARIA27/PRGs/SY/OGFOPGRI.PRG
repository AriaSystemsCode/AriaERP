*:************************************************************************
*: Program file  : GFOPGRID.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B600422,4 Hesham El-Sheltawi 07/06/95 close all the report files
*B600422,4 used by the option grid because in some cases it gives
*B600422,4 to many files are opened after going back to the previous
*B600422,4 program
*B601004,1 Hesham 03/19/96
*B601004,1 check if the report form has been changed before going to
*B601004,1 the select device screen to initialize the printer driver screen
*B601004,1 with the right platform
*B601025,1 Hesham El-Sheltawi 04/10/96
*B601025,1 when opening the report files check if there is any opened files
*B601025,1 for this report before proceding with opening files
*B800551,1 Hesham 04/10/96
*B800551,1 in case of loading prefrences
*B800551,1 load default report variables for the master report
*B800551,1 of the runing one instead of the running report
*B800551,1 so if there is any master report for the running
*B800551,1 one the main variables can be defined like in case
*B800551,1 of income statment and balance sheet reports
*B601074,1 Change the checking of the report forms to make the system
*B601074,1 feel if the form is changed to reinitialize the form array
*B601074,1 if needed in case the form names is identical but in the
*B601074,1 last charecter "ex. APCASHR,APCASHR2" SO we have to change
*B601074,1 the checking from "lcOGLastForm<>&lcOGFormV" to 
*B601074,1 "!(lcOGLastForm==&lcOGFormV)"
*B601091,1 Hesham El-Sheltawi 06/04/96
*B601091,1 Remove the condition that was wrote by M.H for the
*B601091,1 CHECK PRINTING REPORT
*B800919,4 01/14/96 Hesham Use FoxFont for inlist object 
*E300399,1 Hesham El_Sheltawi 09/24/96
*E300399,1 Change the calling of the Report Application calling so it
*E300399,1 can work with the apparel system as well as with advantage
*E300667,1 Hesham El_Sheltawi 04/29/97
*E300667,1 Make The option grid screen can work with programs as well as
*E300667,1 the reports
*B601761,1 Hesham 11/06/97
*B601761,1 Close the report files that was opened by the opgrid when calling
*B601761,1 the change grid function and the open the new files
*B601915,1 Hesham 11/30/97 Disable the options push button under windows
*E300857,1  HS  04/22/98   Add a new screen (In range) to the In list
*E300857,1                 operator.
*E300857,1                 Note : These fields (mBrwFile , mBrwFields ,
*E300857,1                        mBrwFltExp , cClrRngSel , cSetFunc ,
*E300857,1                        cSetParm , cBrwSelFld) was added to
*E300857,1                        the SYREPUVR file.
*E300869,1  HS  04/22/98   Allow expressions in filter option headers.
*E300869,1  HS  04/26/98   Allow changing the operator prompt.
*B602022,1 Hesham 05/21/98
*B602022,1 change the select query to ignore the related fields of
*B602022,1 the codes in the codes popup
*E300799,4  HS  06/02/98   Pass 2 more parameters when calling OGInRange.
*E300900,1  HS  07/21/98   Add the following enhancements to the
*E300900,1                 option grid:
*E300900,1                 1) Give the user the capability to choose the
*E300900,1                    report mode.
*E300900,1                 2) Give the capability to make valid entries
*E300900,1                    from file or from array.
*E300900,1                 3) Create all the option grid popups as
*E300900,1                    array popups instead of list popups.
*E300900,1                 4) Add the suppress expression capability.
*E300900,1                 5) Use proper English in the default operator
*E300900,1                    prompts.
*E300900,1                 Note: 
*E300900,1                       I made some changes in the following
*E300900,1                       screens:
*E300900,1                                GRIDOBJ.SCX , GRIDSET.SCX
*B602079,1 RENEE 09/14/98 Reports should run from the active application
*B602079,1 				  since, their home application does not necessarily 
*B602079,1 				  exist if the report is shared, or running form 
*B602079,1 				  another module
*B801961,1  HSS  02/18/99 1) Add a new parameter to gfGenFlt() to let it
*B801961,1 				     know if it can use the filter array in the
*B801961,1 				     expression or not.
*B801961,1 				  2) If the field has a valid entries of
*B801961,1 				     type (Code):
*B801961,1 				     a) Display the code + code description in the
*B801961,1 				        case of editable codes and to sort the
*B801961,1 				        displayed values by code.
*B801961,1 				     b) Display the code description in the
*B801961,1 				        case of not editable codes and to sort the
*B801961,1 				        displayed values by code description.
*B801961,1 				  3) Add a new parameter to path the field length
*B801961,1 				     to the function OGINLIST.
*B801961,1 				     Note: If the filter option is an expression
*B801961,1 				           we will path 0 as the field length.
*B602575,1 MAB 02/23/99 Fix Sort Option popup display 
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*E300071,72 MAB 03/06/1999 Close sydfield file.
*E301170,1  MAB 03/15/1999 Rise flag detect that OG change its filter(and ask at run time variables).
*E301178,1 03/18/1999 if lfChangeGrid function called from < Reset > button, [Begin]
*E301178,1 03/18/1999 rise flag to prevent close files then open it, i.e.:Just restore defaults.
*C101459,1 Hesham 03/24/99
*C101459,1 MAKE THE OPTION GRID CONTROL THE USER DEFINED FIELDS ON PROGRAMS
*C101459,1 BASE FILES
*B802093,1  HS 04/01/99 Fix some problems in the In Range filter options
*B802093,1              with suppress expression.
*B802279,1  HS 05/18/99 Some problems in the "Advanced" option, because the
*B802279,1              function lfGetSupEx() assume that the current alias
*B802279,1              is the report variables file (SYREPUVR).
*B602931,4  HS  05/26/1999 Enhance Performance in In Range
*B602950,1  HS  05/27/1999 When the Option Grid READ CYCLE is cleared to
*B602950,1                 refresh the Option Grid or from the "Advanced"
*B602950,1                 option, Re-dimension the array (Invb) to prevent
*B602950,1                 the adding new rows to the array.
*B603065,1 Hesham 07/18/1999
*B603065,1 In numeric values check for decimal places before changing the value to a string
*E301302,1  HS  08/01/99  Give the capability to use macro substitution
*E301302,1                in the in range fields.
*B603195,1 Reham On 10/06/1999
*B603195,1 Fix the error "Index tag not found" if using descending order
*:************************************************************************

*E300667,1 Hesham El-Sheltawi (Start)
*E300693,1 Hesham El_Sheltawi (Start)
*PARAMETERS lcOGPrgName,llProgram
PARAMETERS lcOGPrgName,llProgram,lcRunDirct
*E300693,1 Hesham El_Sheltawi (End)
llProgram = IIF(TYPE('llProgram') # 'L',.F.,llProgram)
*E300693,1 Hesham El_Sheltawi (Start)
lcRunDirct = IIF(TYPE('lcRunDirct') # 'C','R',lcRunDirct)
lcOGPrtdir = IIF(LEN(lcRunDirct)>1,RIGHT(lcRunDirct,1),'')
lcRunDirct = LEFT(lcRunDirct,1)
*E300693,1 Hesham El_Sheltawi (End)
*PARAMETERS lcOGPrgName
*E300667,1 Hesham El-Sheltawi (End)

DIMENSION laFiltrExp[1,7],laRpObjDos[1,8],laRpObjWin[1,10],;
          laRpObjUni[1,8],laRpObjMac[1,10],laOGFormCnt[1],laOGSeting[1,2],;
          laOGObjType[1,3]


STORE '' TO laFiltrExp,lcOGOldEsc, lcOGEscHnd,lnOGBarNo,laRpObjDos,;
            laRpObjWin,laRpObjUni,laRpObjMac,lcOGLastForm,lcOGTmpForm,;
            lcOGFlt_ID,lcOGoldKey,lcOGPrvRun,lcOGFormV,lcOGExp
STORE .F. to llOGEscPrsd,llOGRefForm,glFirsTime,LLFromObj,llRetFlt,llJobUse,;
             llRepFlUse
STORE .T. TO llOGStyleCh,llOGSysData

*E300900,1 Add these lines to add some new variables [Begin]
*--lcRepAvlM    Variable to hold the available report modes
*--lcRepMode    Variable to hold the corrent report mode
*--lcRepModeS   Variable to hold the status of the report mode popup
*--lcRepTmpNm   Variable to hold Temp. name to open the report (FRX) with
*--lcOldRepId   Variable to hold the old report Id.

STORE '' TO lcRepAvlM , lcRepMode , lcRepModeS , lcOldRepId
lcRepTmpNm = gfTempName()
*E300900,1 Add these lines to add some new variables [End]

PRIVATE laScrMode
DIMENSION laScrMode[5]
laScrMode[3] = .T.
*******
lnObjNum=1
lnObjMax=7
lnMaxObj=0
lnScrolRow=IIF(_DOS,2,2)
lnOGStObj=0

*E300900,1 Change this line to add the suppress expression capability [Begin]
*lncount=1
lncount = 0
*E300900,1 Change this line to add the suppress expression capability [End]

*E300900,1 Add this line to add some new variables [Begin]
*-- lnLopCount   Variable to be used in the for loop that creates the
*--              objects.
*-- lnNumOfObj   Variable to be used in the for loop that creates the
*--              objects.
*-- lnVrDelObj   Variable to hold the number of that was deleted from the
*--              variable filter.
*-- llLoop       Flag to know if the current object will not appear in the
*--              option grid.
*-- llFxFrsObj   Flag to know if the first object to be displayed in the
*--              fixed filter.
*-- llVrFrsObj   Flag to know if the first object to be displayed in the
*--              variable filter.
*-- llFxLastOr   Flag to know if the last object to be displayed in the
*--              fixed filter is Or.
*-- llVrLastOr   Flag to know if the first object to be displayed in the
*--              variable filter is Or.

lnLopCount = 1
lnNumOfObj = 0
lnVrDelObj = 0
llLoop     = .F.
llFxFrsObj = .F.
llVrFrsObj = .F.
llFxLastOr = .F.
llVrLastOr = .F.
*E300900,1 Add this line to add some new variables [End]

lncolsize=25
DIMENSION laObjScrP[1,5]
lcListColor=SCHEME(6,3)
lcListColor=STRTRAN(SUBSTR(lcListColor,1,ATC('/',lcListColor)-1),'+','')+;
            SUBSTR(lcListColor,ATC('/',lcListColor))
lcXorColor=SCHEME(6,3)
lcXorColor=STRTRAN(SUBSTR(lcXorColor,ATC('/',lcXorColor)+1),'*','')+'/'+;
           STRTRAN(SUBSTR(lcXorColor,1,ATC('/',lcXorColor)-1),'+','')

******

IF !_DOS
  lcOGoldKey=SET('KEYCOMP')
  SET KEYCOMP TO DOS 
ENDIF  
lnOGSeting=1
lcOGTmpForm=gfTempName()
lcFileName=''
lcOGTmpWin=''

  lcOGOldEsc = SET('ESCAPE')
  SET ESCAPE OFF
*****************************
IF lcRunDirct $ 'RM'
  IF !USED('SYUBSRVJ')
    llJobUse = .T.
    SELECT 0
    USE &gcSysHome.SYUBSRVJ
  ELSE
    SELECT SYUBSRVJ 
  ENDIF
ENDIF
IF !USED('SYDREPRT')
  llRepFlUse =.T.
  SELECT 0
  USE &gcSysHome.SYDREPRT 
ELSE
  SELECT SYDREPRT 
ENDIF
SET ORDER TO TAG CREP_ID

lcOGRepID=PADR(ALLTRIM(lcOGPrgName),8)
lcOGManRep=lcOGRepID
IF SEEK(PADR(lcOGPrgName,8))
   lcOGPrgName=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcOGPrgName),8)))
ENDIF
***************************

lcOGCol1=SCHEME(5,2)
lcOGCol2=SCHEME(5,3)
lcOGCol3=SCHEME(5,2)
lcOGReadW=''
lcOGreadV=''
lcOGReadS=''
lcOGFltr=''
llOGFltCh = .T.
lcOGSeting=0
*lcOGSetPrc=SET('PROCEDURE')
*SET PROCEDURE TO (lcOGPrgName)
lcOGEntSet=''
lcOGEntVal=''
lcOGOldVal=''
lcOGAskOld=''  && E301170,1 Ask at run time old value.

IF lcRunDirct $ 'PM'
  IF TYPE('laOGHdFlt[1,1]') = 'C'
    =ACOPY(laOGHdFlt,gaHdFilter)
  ENDIF  
  IF TYPE('laOGFxFlt[1,1]') = 'C'  
    =ACOPY(laOGfxFlt,gaFxFilter)  
  ENDIF  
  IF TYPE('laOGVrFlt[1,1]') = 'C'    
    =ACOPY(laOGVrFlt,gaVrFilter)    
  ENDIF
ENDIF
*E300693,1 Hesham El_Sheltawi (Start)
PRIVATE ALL LIKE ??OG*
*E300693,1 Hesham El_Sheltawi (End)
DIMENSION laScrObj[1,5],laOGObjPos[1,5]

*E300857,1 Add these lines to add 2 new arrays to hold the temp. cursor names
*used by In range screen for the fixed filter and the variable filter and
* 1 new array to hold the browse fields and filter expression for all the
*In list operators (in case of using In range screen) in the option
*grid [Begin]

*B602931,4 Change this line to add 2 new columns to the array (laBrFldFlt),
*          column 4 will hold the first part - until the field that will be
*          returned by the gfrange() in the temp. cursor. - of index
*          expression of the active (controlling) index tag on the
*          "In Range" browse file and column 5 will hold the name and row
*          number of the array creating this option [Begin]
*DIMENSION laFxFltCur[1,3] , laVrFltCur[1,3] , laBrFldFlt[1,3]
DIMENSION laFxFltCur[1,3] , laVrFltCur[1,3] , laBrFldFlt[1,5]
*B602931,4 Change this line to add 2 new columns to the array [End]

laFxFltCur = ''
laVrFltCur = ''
laBrFldFlt = ''

*E300857,1 Add these lines to add 2 new arrays to hold [End]

*E300693,1 Hesham El_Sheltawi (Start)
DIMENSION InvB[1],InvE[1],laOGHdFlt[1,7],laOGFxFlt[1,7],laOGVrFlt[1,7],;
          laOGFieldH[1,1],laOGFieldN[1,1],laOGWinObj[1],laSelFile[1,2],laOpenBy[1],;
          laOGObjCnt[1]
*DIMENSION InvB[1],InvE[1],laOGHdFlt[1,7],;
          laOGFieldH[1,1],laOGFieldN[1,1],laOGWinObj[1],laSelFile[1,2],laOpenBy[1],;
          laOGObjCnt[1]
*IF lcRunDirct $ 'RB'
  DIMENSION laOGFxFlt[1,7],laOGVrFlt[1,7]
  STORE '' TO laOGFxFlt,laOGVrFlt
*ENDIF          

*E300900,1 Add these lines to add 2 new array [Begin]
*-- laOGPopAry    Array to be used to create all the popup options in the
*--               option grid except those created from array.
*-- laSupExp      Array to hold all the fields and expressions that have a
*--               suppress expression and their suppressing expressions.
DIMENSION laOGPopAry[1,1] , laSupExp[1,2]
laOGPopAry = ''
laSupExp   = ''
*E300900,1 Add these lines to add 2 new array [End]

*E300693,1 Hesham El_Sheltawi (End)          
STORE 1 TO lnWinDisp,invb,inve,inv,puSeting,puSort,lnOGVarNo
STORE .T. TO llFirstTime,lcActWind,llOGVarFlt
*E300693,1 Hesham El_Sheltawi (Start)
STORE '' TO laOGHdFlt,laOGFxFlt,laOGVrFlt,laOGFieldH,laOGFieldN,laSelFile,;
            lcOGFormArr,lcOGPlatForm
*STORE '' TO laOGHdFlt,laOGFieldH,laOGFieldN,laSelFile,;
            lcOGFormArr,lcOGPlatForm
*C101459,1 Hesham (Start)
*IF lcRunDirct $ 'PM'
*  IF TYPE('gaHdFilter[1,1]') = 'C'    
*    DIMENSION laOGHdFlt[ALEN(gaHdFilter,1),ALEN(gaHdFilter,2)]
*    =ACOPY(gaHdFilter,laOGHdFlt)
*  ENDIF  
*  IF TYPE('gaFxFilter[1,1]') = 'C'      
*    DIMENSION laOGFxFlt[ALEN(gaFxFilter,1),ALEN(gaFxFilter,2)]  
*    =ACOPY(gaFxFilter,laOGfxFlt)  
*  ENDIF  
*  IF TYPE('gaVrFilter[1,1]') = 'C'      
*    DIMENSION laOGVrFlt[ALEN(gaVrFilter,1),ALEN(gaVrFilter,2)]  
*    =ACOPY(gaVrFilter,laOGVrFlt)    
*  ENDIF  
*ENDIF

DO CASE
  CASE lcRunDirct $ 'PM'
    IF TYPE('gaHdFilter[1,1]') = 'C'    
      DIMENSION laOGHdFlt[ALEN(gaHdFilter,1),ALEN(gaHdFilter,2)]
      =ACOPY(gaHdFilter,laOGHdFlt)
    ENDIF  
    IF TYPE('gaFxFilter[1,1]') = 'C'      
      DIMENSION laOGFxFlt[ALEN(gaFxFilter,1),ALEN(gaFxFilter,2)]  
      =ACOPY(gaFxFilter,laOGfxFlt)  
    ENDIF  
    IF TYPE('gaVrFilter[1,1]') = 'C'      
      DIMENSION laOGVrFlt[ALEN(gaVrFilter,1),ALEN(gaVrFilter,2)]  
      =ACOPY(gaVrFilter,laOGVrFlt)    
    ENDIF 
  *C101459,1 if option grid called to control user define fields
  *C101459,1 then initialize the arrays from the program user defined
  *C101459,1 fields array
  CASE lcRunDirct $ 'TX'
    DIMENSION laOGHDFlt[1,7],laOGVRFlt[1,7]    
    STORE '' TO laOGHDFlt,laOGVRFlt 
    DIMENSION laOGFxFlt[ALEN(laUsrFields,1),ALEN(laUsrFields,2)]    
    =ACOPY(laUsrFields,laOGFxFlt)
    lcOGWinTitl = IIF(TYPE('lcWindTitl')='C',ALLT(lcWindTitl),'')+' User Fields'
    llOGVrFlt = .F.
    llOGFilter = .T.
    lcOGPrvRun = 'gfVldUsrFld(@laOgFxFlt)'

ENDCASE
*C101459,1 Hesham (End)

llOGBatchOk = .F.            
*IF lcRunDirct = 'RB'            
*  STORE '' TO laOGFxFlt,laOGVrFlt
*ENDIF  
*E300693,1 Hesham El_Sheltawi (End)            
*E300399,1 Hesham El-Sheltawi (Start)            
*llOGBatchOk = IIF( TYPE( 'llBatchOpt' ) = 'U', .F., llBatchOpt )
*E300399,1 Hesham El-Sheltawi (End)            
*B601004,1 Hesham (Start)            

*E300900,1 Remove these lines to give the user the capability to choose the
*E300900,1 report mode [Begin]
*=lfGetPlatFrm()
*E300900,1 Remove these lines to give the user the capability [End]

*B601004,1 Hesham (END)
lcOGExact=SET('EXACT')
SET EXACT OFF
STORE .F. TO laOpenBy,llOgTrmnat
*lnScrolRow=3
lcElmSep='|'
lcValSep='~'
lnOGMax=7
lnOGFixFl=0
lnOGVarFl=0
IF !USED('SYDREPRT')
  llRepFlUse = .T.
  SELECT 0
  USE &gcSysHome.SYDREPRT 
ELSE
  SELECT SYDREPRT 
ENDIF
SET ORDER TO TAG CREP_ID
*C101459,1 Hesham (Start)
*C101459,1 if option grid called to control user define fields
*C101459,1 then initialize the option grid title same as the
*C101459,1 program title
*lcOGWinTitl =''

lcOGWinTitl = IIF(TYPE('lcWindTitl')='C',ALLT(lcWindTitl),'')+' User Fields'
*C101459,1 Hesham (End)
lcOGOrder   =''
IF SEEK(PADR(lcOGPrgName,8))
  llOGVrFlt   = lRepVrFlt  
  llOGFilter  = !lSelect
  lcOGReadW   = ALLTRIM(cReadWhen)
  lcOGreadV   = ALLTRIM(cReadVald)  
  lcOGReadS   = ALLTRIM(cReadShow)
  lcOGWinTitl = " "+ALLTRIM(cRep_Name)+" "
  lcOGOrder   = ALLTRIM(CORDERVAR)
  lcOGOrderV  = ALLTRIM(CORDERVLD)
  lcOGFormV   = ALLTRIM(CFORMVAR)
  llOGBatchOk = LBATCHREP
  IF !EMPTY(mRepAvFld)
    RESTORE FROM MEMO mRepAvFld ADDI
  ENDIF

  IF !EMPTY(ALLTRIM(MREP_FILS))
    RESTORE FROM MEMO MREP_FILS ADDI
    =lfOpenRpFls()
  ENDIF   
ENDIF

IF !USED('SYREPUVR')
  SELECT 0
  USE &gcSysHome.SYREPUVR 
ELSE
  SELECT SYREPUVR
ENDIF

lcOgTmExact=SET('EXACT')
*SET EXACT ON
SET EXACT OFF
SET ORDER TO TAG CREPVAR

IF SEEK(PADR(lcOGPrgName,8)+'V')
  SCAN REST WHILE cRep_ID+CEXPTYPE+STR(nVarPos)=PADR(lcOGPrgName,8)+'V'
      lcOGVarName=STRTRAN(ALLTRIM(mFld_Name),CHR(13)+CHR(10),'')
      lcOGValue=STRTRAN(mData_Def,CHR(13)+CHR(10),'')
      DO CASE
        CASE cDefa_Typ='V'
          &lcOGVarName=lfTrnsStr(lcOGValue,cData_Typ)
        CASE cDefa_Typ='E'
         &lcOGVarName=&lcOGValue
      ENDCASE
      *E300693,1 Hesham El_Sheltawi (Start)      
      IF lcRunDirct $ 'PM' AND TYPE('laRepFltr[1,1]')='C' AND !EMPTY(laRepFltr[1,1])
        lnArrCol = ASCAN(laRepFltr,UPPER(ALLT(lcOGVarName)))
        IF lnArrCol<>0
           lnArrCol = ASUBSCRIPT(laRepFltr,lnArrCol,1)
           lcOGValue=STRTRAN(laRepFltr[lnArrCol,2],CHR(13)+CHR(10),'')      
           IF !EMPTY(lcOGValue)
             &lcOGVarName=&lcOGValue
           ENDIF  
        ENDIF  
      ENDIF  
      *E300693,1 Hesham El_Sheltawi (End)
  ENDSCAN
ENDIF  

*SET EXACT &lcOGTmExact
*******************
*lcOGRepID=IIF(lcOGRepID<>lcOGPrgName,PADR(lcOGPrgName,8),lcOGRepID)
*lcOGManRep=lcOGRepID
*******************
*COUNT FOR CEXPTYPE='V' AND lAskRunT AND lDispOG AND ALLTRIM(cRep_ID)=ALLTRIM(lcOGRepID) TO lnOGVariable 
=SEEK(PADR(lcOGRepID,8)+'V')
COUNT REST WHILE cRep_ID+CEXPTYPE+STR(nVarPos)=PADR(lcOGRepID,8)+'V' FOR lAskRunT AND lDispOG  TO lnOGVariable 

SELECT SYDREPRT
IF SEEK(lcOGRepID)
  llOGVrFlt   = lRepVrFlt  
  llOGFilter  = !lSelect
  lcOGReadW   = ALLTRIM(cReadWhen)
  lcOGreadV   = ALLTRIM(cReadVald)  
  lcOGReadS   = ALLTRIM(cReadShow)
  lcOGWinTitl = " "+ALLTRIM(cRep_Name)+" "
  lcOGOrder   = ALLTRIM(CORDERVAR)
  lcOGOrderV  = ALLTRIM(CORDERVLD)
  lcOGFormV   = ALLTRIM(CFORMVAR)
  llOGBatchOk = LBATCHREP  
ENDIF

*B601074,1 Hesham (Start)
*B601074,1 REMARK AND MOVE IT after getting the default pref. so the variable
*B601074,1 that hold the form will have the correct value
*B800551,1 Hesham (Start)
*B800551,1 Get the right report platform from the frx
*B800551,1 before initializing any of the layout arrays
*IF _DOS
*  IF EMPTY(&lcOGFormArr[1,1]) AND !EMPTY(lcOGFormV)
*    =gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
*    lcOGLastForm=&lcOGFormV
*  ELSE
*   IF !EMPTY(lcOGFormV)
*    lcOGLastForm=&lcOGFormV
*    llOGStyleCh =.T.
*   ENDIF 
*  ENDIF
*ENDIF  
*B800551,1 Hesham (End)
*B601074,1 Hesham (End)

IF !USED('SYURPFLT')
  SELECT 0
  USE &gcSysHome.SYURPFLT 
ELSE  
 SELECT SYURPFLT
ENDIF
SET ORDER TO TAG CREP_ID

SELECT SYURPFLT
DIMENSION laOGEntSet[1,2]
=lfGetSetting()
  
LOCATE FOR cRep_ID = lcOGRepID;
           AND cUser_ID = gcUser_ID;
           AND lDefault = .T.
*E300693,1 Hesham El_Sheltawi (Start)
*IF FOUND() 
IF FOUND() AND lcRunDirct $ 'RP'
*E300693,1 Hesham El_Sheltawi (End)
  lcOGFlt_ID  = cfltr_ID
  lcOGSeting  = RECNO() &&cfltr_ID
  lcOGSetMsg  = cFltr_des
  lcOGLastSet = lcOGSeting
  llOGSysData = .F.
  llOGFilter  = lSelect  
  lnogSeting = ASCAN(lAOGSeting,lcOGSeting)
  lnogSeting = IIF(lnogSeting<>0,ASUBSCRIPT(laOGSeting,lnogSeting,1),1)
  =lfLoadUDefV()
ELSE
 SELECT SYDREPRT
 SEEK lcOGRepID
   lcOGFlt_ID  = ''
  lcOGSeting = 0 &&'Default'
  llOGSysData = .T.
  lcOGSetMsg = 'Default'
  lcOGLastSet= lcOGSeting  
ENDIF           

IF !EMPTY(ALLTRIM(MREPHDFLT))
  RESTORE FROM MEMO MREPHDFLT ADDI
ENDIF  
*E300693,1 Hesham El_Sheltawi (Start)
*IF !EMPTY(ALLTRIM(MREPFXFLT))  
*  RESTORE FROM MEMO MREPFXFLT ADDI
*  =lfInitValue('laOGFxFlt','C')    
*ENDIF

*IF !EMPTY(ALLTRIM(MREPVRFLT)) 
*  RESTORE FROM MEMO MREPVRFLT ADDI
*    =lfInitValue('laOGVrFlt','C')          
*ENDIF  

IF lcRunDirct = 'R'
  IF !EMPTY(ALLTRIM(MREPFXFLT))  
    RESTORE FROM MEMO MREPFXFLT ADDI
    =lfInitValue('laOGFxFlt','C')    
  ENDIF

  IF !EMPTY(ALLTRIM(MREPVRFLT)) 
    RESTORE FROM MEMO MREPVRFLT ADDI
      =lfInitValue('laOGVrFlt','C')          
  ENDIF  
ENDIF

IF lcRunDirct = 'B'
  SELECT SYUBSRVJ
  IF !EMPTY(MUSRDVAR)
    RESTORE FROM MEMO MUSRDVAR ADDI
    IF TYPE('lcRpRepID')='C'
      lcOGRepID = lcRpRepID
    ENDIF  
  ENDIF  
  IF !EMPTY(ALLTRIM(MREPFXFLT))  
    RESTORE FROM MEMO MREPFXFLT ADDI
    =lfInitValue('laOGFxFlt','C')    
  ENDIF

  IF !EMPTY(ALLTRIM(MREPVRFLT)) 
    RESTORE FROM MEMO MREPVRFLT ADDI
      =lfInitValue('laOGVrFlt','C')          
  ENDIF  
ENDIF


*E300693,1 Hesham El_Sheltawi (End)
*B601091,1 Hesham (Start)
*B601091,1 get the number of ask at run time variables for the saving
*B601091,1 settings
SELECT SYREPUVR
=SEEK(PADR(lcOGRepID,8)+'V')
COUNT REST WHILE cRep_ID+CEXPTYPE+STR(nVarPos)=PADR(lcOGRepID,8)+'V' FOR lAskRunT AND lDispOG  TO lnOGVariable 
*B601091,1 Hesham (End)
SELECT SYDREPRT

IF lcRunDirct $ 'RP' AND SEEK(lcOGRepID)
  llOGVrFlt=lRepVrFlt  
  IF !EMPTY(mRepAvFld)
    RESTORE FROM MEMO mRepAvFld ADDI
  ENDIF
  IF !EMPTY(ALLTRIM(MREP_FILS))
    RESTORE FROM MEMO MREP_FILS ADDI
  ENDIF   
ENDIF

lnOGFixFl = IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGVarFl = IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
******************************* Initialize the style arrays
*B800551,1 Hesham (Start)
*B800551,1 remark all the next lines at move it up
*B800551,1 before getting the default pref.
*IF _DOS
*  IF EMPTY(&lcOGFormArr[1,1]) AND !EMPTY(lcOGFormV)
*    =gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
*    lcOGLastForm=&lcOGFormV
*  ELSE
*   IF !EMPTY(lcOGFormV)
*    lcOGLastForm=&lcOGFormV
*    llOGStyleCh =.T.
*   ENDIF 
*  ENDIF
*ENDIF  
*B800551,1 Hesham (End)

*B601074,1 Hesham (Start)
*B601074,1 check if the form variable is not empty then create the temp.
*B601074,1 FRX for the case of having a default pref.

*E300667,1 Hesham El-Sheltawi (Start)
IF !llProgram
*E300667,1 Hesham El-Sheltawi (End)
  IF EMPTY(lcOGFormV)
    lcOGTmpForm=''
  ELSE
      
      *E300900,1 Remove this line for we have canceled the Option that
      *E300900,1 change the report objects style [Begin]
      *=gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
      *E300900,1 Remove this line for we have canceled the Option [End]
      
      *E300900,1 Remove this line for the sake of performance [Begin]
      *=gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)  
      *E300900,1 Remove this line for the sake of performance [End]
      
      lcOGLastForm=&lcOGFormV    
  ENDIF
  
  *E300900,1 Add this line to get the available report modes [Begin]
  =lfRepPltFr(IIF(EMPTY(lcOGFormV) , '' , EVALUATE(lcOGFormV)))
  *E300900,1 Add this line to get the available report modes [End]
  
*E300667,1 Hesham El-Sheltawi (Start)
ENDIF
*E300667,1 Hesham El-Sheltawi (End)
*B601074,1 Hesham (End)
**********************************
IF !llOGVrFlt
  lnOGVarFl= 0
ENDIF

*E300071,72 Open Sydfield. [begin
llFieldFile = !USED('SYDFIELD')
IF llFieldFile
  USE (gcSysHome+'SYDFIELD') IN 0
ENDIF
*E300071,72 Open Sydfield. [end

*E300693,1 Hesham El_Sheltawi (Start)
*DO WHILE !llOgTrmnat
*  STORE 1 TO lnWinDisp,invb,inve,inv
*  DIMENSION laOGObjType[1,3]
*  STORE .F. TO laOGObjType
**  STORE .T. TO llFirstTime,lcActWind
*  IF (lnOGVariable+lnOGFixFl+lnOGVarFl)>0
*    DO gridhost.spr
*  ELSE
*    DO GRID.SPR  
*  ENDIF  
*  llFirstTime=.F.
*ENDDO
*C101459,1 Hesham (Start)
*IF lcRunDirct $ 'RM'
IF lcRunDirct $ 'RMTX'
*C101459,1 Hesham (End)
  DO WHILE !llOgTrmnat
    
    *B602950,1 Re-dimension the array (Invb) to prevent the adding new rows
    *          to the array when the Option Grid READ CYCLE is cleared to
    *          refresh the Option Grid or from the "Advanced" option [Begin]
    DIMENSION InvB[1]
    *B602950,1 Re-dimension the array (Invb) to prevent the adding new [End]
    
    STORE 1 TO lnWinDisp,invb,inve,inv
    DIMENSION laOGObjType[1,3]
    STORE .F. TO laOGObjType
  *  STORE .T. TO llFirstTime,lcActWind
    
    *E300900,1 Add this line to reinitialize the variables that holds
    *E300900,1 the number of elements in fixed and variable filters [Begin]
    lnOGFixFl = IIF(EMPTY(laOGFxFlt[1,1]) , 0 , ALEN(laOGFxFlt,1))
    lnOGVarFl = IIF(EMPTY(laOGVrFlt[1,1]) .OR. !llOGVrFlt , 0 ,;
                    ALEN(laOGVrFlt,1))
    *E300900,1 Add this line to reinitialize the variables [End]
    
    IF (lnOGVariable+lnOGFixFl+lnOGVarFl)>0
      DO gridhost.spr
    ELSE
      DO GRID.SPR  
    ENDIF  
    llFirstTime=.F.
  ENDDO
ELSE
  lcOGError = ON('ERROR')
  llOGErr = .f.
  ON ERROR llOGErr = .T.
  IF !EMPTY(ALLTRIM(lcOGReadW)) AND (!EMPTY(SET('PROC')) OR llProgram)
   =&lcOGReadW
  ENDIF
  IF !EMPTY(lcOGReadS) AND (!EMPTY(SET('PROC')) OR llProgram)
    =&lcOGReadS
  ENDIF
  ON ERROR &lcOGError
  IF lcRunDirct = 'B' AND lcOGPrtdir = 'V'
    =lfVViewRep()  
  ELSE
    lcOGDevice=gcDevice
    lcOGOutFile=gcOutFile
    gcOutFile = ''
    gcDevice = "PRINTER"
    llOGHeader = glHeader
    glHeader = .T.
    =lfvRunRep()
    gcDevice=lcOGDevice
    gcOutFile=lcOGOutFile
    glHeader = llOGHeader
  ENDIF  
ENDIF
*E300693,1 Hesham El_Sheltawi (End)
IF !EMPTY(lcOGTmpForm) 
  IF FILE(gcWorkDir+lcOGTmpForm+'.FRX')
    ERASE &gcWorkDir.&lcOGTmpForm..FRX
    ERASE &gcWorkDir.&lcOGTmpForm..FRT  
  ENDIF
ENDIF  
  IF !EMPTY(SET('PROC'))
    =lfClearRep() 
  ENDIF  
=lfClosRpFls()

SET EXACT &lcOGExact  
SET ESCAPE &lcOGOldEsc
*SET PROCEDURE TO &lcSetProcd
*B600422,4 closing all the reports files

*E300071,72 Close Sydfield. [begin
IF USED('SYDFIELD') And llFieldFile
  USE IN SYDFIELD
ENDIF
*E300071,72 Close Sydfield. [end

IF USED('SYDREPRT') AND llRepFlUse
  USE IN SYDREPRT 
ENDIF

IF USED('SYREPUVR')
  USE IN SYREPUVR 
ENDIF

IF USED('SYURPFLT')
  USE IN SYURPFLT 
ENDIF

IF USED('SYREPSRT')
  USE IN SYREPSRT 
ENDIF
*ymo
*E300399,1 Hesham El-Sheltawi (Start)
*E300399,1 Check if the file user privlidje is opened before
*E300399,1 Selecting it's work area
*SELECT SYDSUPRG
IF USED('SYDSUPRG')
  SELECT SYDSUPRG
ENDIF
*E300399,1 Hesham El-Sheltawi (End)

IF USED('SYUBSRVJ') AND llJobUse
    USE IN SYUBSRVJ
ENDIF


IF !_DOS
  SET KEYCOMP TO &lcOGoldKey
ENDIF  

*IF !EMPTY(GETENV('DEVELOP'))
*  ON ERROR &lcOGOldErr
*ENDIF  
IF llProgram AND llRetFlt
  
  *B801961,1 Add these lines because the filter expression use a reference
  *          to the arrays laOgHdFlt, laOgFxFlt and laOgVrFlt in the case
  *          of In List operator right now. But since these arrays are
  *          declared as private arrays in the option grid. We are going
  *          to use the arrays laHdFlt, laFxFlt and laVrFlt - which should
  *          be declared in the program that called the option grid - in the
  *          filter expression instead of the original arrays.
  DIMENSION laHdFlt[ALEN(laOgHdFlt , 1) , ALEN(laOgHdFlt , 2)] ,;
            laFxFlt[ALEN(laOgFxFlt , 1) , ALEN(laOgFxFlt , 2)] ,;
            laVrFlt[ALEN(laOgVrFlt , 1) , ALEN(laOgVrFlt , 2)]
  
  =ACOPY(laOgHdFlt , laHdFlt)        && Copy the elements of laOgHdFlt to laHdFlt
  =ACOPY(laOgFxFlt , laFxFlt)        && Copy the elements of laOgFxFlt to laFxFlt
  =ACOPY(laOgVrFlt , laVrFlt)        && Copy the elements of laOgVrFlt to laVrFlt

  *C101459,1 Hesham (Start)
  IF (lcRunDirct $ 'TX' ) 
    DIMENSION laUsrFields[ALEN(laOGFxFlt,1),ALEN(laOGFxFlt,2)]    
    =ACOPY(laOGFxFlt,laUsrFields)
  ENDIF 
  *C101459,1 Hesham (END)  
  
  *-- Replace the array laOgHdFlt with the the array laHdFlt in the
  *-- filter expression.
  lcOGExp = STRTRAN(UPPER(lcOGExp) , 'LAOGHDFLT' , 'LAHDFLT')
  
  *-- Replace the array laOgFxFlt with the the array laFxFlt in the
  *-- filter expression.
  lcOGExp = STRTRAN(lcOGExp , 'LAOGFXFLT' , 'LAFXFLT')
  
  *-- Replace the array laOgVrFlt with the the array laVrFlt in the
  *-- filter expression.
  lcOGExp = STRTRAN(lcOGExp , 'LAOGVRFLT' , 'LAVRFLT')
  *B801961,1 Add these lines because the filter expression [End]
  
  RETURN lcOGExp
ELSE
  RETURN llRetFlt  
ENDIF

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfOGShow
*SHOW GET pbOGSave DISABLE

IF ! llOGVrFlt
  SHOW GET PBADVANCED DISABLE
ENDIF
IF llProgram
  SHOW GET pbOGpreview DISABLE
  SHOW GET pboption DISABLE
ENDIF
IF llFirstTime
  llFirstTime=.f.
  =lfActvateWin(1)
ENDIF
*C101459,1 Hesham (Start)
*C101459,1 if option grid called to control user define fields
*C101459,1 then disable the objects in case of view mode and control the buttons prompt
IF lcRunDirct = 'X'
  STORE .F. TO laOGObjCnt
ENDIF
IF lcRunDirct $ 'XT'
  SHOW GET PBRUN,1 Prompt "\!\<Ok"
  IF lcRunDirct = 'X'
    SHOW GET PBOGRESET DISABLE
    SHOW GET PBRUN DISABLE
  ELSE
    SHOW GET PBOGRESET ENABLE
    SHOW GET PBRUN ENABLE
  ENDIF  
  SHOW GET PBOGSAVE,1 Prompt "\<Save Setting"  
  SHOW GET PBclose,1 Prompt "\?\<Cancel"
ENDIF
*C101459,1 Hesham (End)
IF lcOGSeting=0 OR (llOGSysData AND !EMPTY(GETENV('DEVELOP')))
  SHOW GET PbOGDel DISABLE
ELSE
  SHOW GET PbOGDel ENABLE  
ENDIF
*B601915,1 Hesham (Start) 
*B601915,1 Disable the options push button under windows
IF _WINDOWS
  SHOW GET pbOption DISABLE
ENDIF
*B601915,1 Hesham (End) 
*E300667,1 Hesham El-Sheltawi (Start)
*IF !EMPTY(lcOGReadS) AND !EMPTY(SET('PROC'))
IF !EMPTY(lcOGReadS) AND (!EMPTY(SET('PROC')) OR llProgram)
*E300667,1 Hesham El-Sheltawi (End)
 =&lcOGReadS
ENDIF

FUNCTION lfOGShowGet
PARAMETERS lcVarName
lcWOutput=WOUTPUT()
ACTIVATE WINDOW gwdGridobj SAME
DO CASE
  CASE TYPE('lcVarName')='C'
    lnVarPos=ASCAN(laOGObjType,lcVarName)
    IF lnVarPos>0
     lnVarPos=ASUBSCRIPT(laOGObjType,lnVarPos,1)
    ELSE
      RETURN
    ENDIF
  CASE TYPE('lcVarName')='L'
   lnVarPos=0
ENDCASE
IF lnVarPos>0
  IF TYPE('laOGObjType[lnVarPos,3]')='C'
    IF _DOS
      @(lnVarPos*2)-2,laObjScrP[lfObjPos('(LCOGVARNAME)'),2]  SAY &laOGObjType[lnVarPos,3] SIZE 1,25
    ELSE
      &laOGObjType[lnVarPos,2]=&laOGObjType[lnVarPos,3]    
    ENDIF
  ENDIF
  IF laOGObjCnt[lnVarPos]              
      SHOW OBJECT lnVarPos ENABLE
  ELSE
      SHOW OBJECT lnVarPos DISABLE
  ENDIF  
ELSE
  PRIVATE lnCount
  lnVarPos=lfObjPos('(LCOGVARNAME)')
  FOR lnCount=1 TO lnMaxObj
    IF TYPE('laOGObjType[lnCount,3]')='C'
      IF _DOS
        @(lnCount*2)-2,laObjScrP[lnVarPos,2]  SAY &laOGObjType[lnCount,3] SIZE 1,25
      ELSE  
        &laOGObjType[lnCount,2]=&laOGObjType[lnCount,3]
      ENDIF  
    ENDIF  
    IF laOGObjCnt[lnCount]              
       SHOW OBJECT lnCount ENABLE
    ELSE
      SHOW OBJECT lnCount DISABLE
    ENDIF    
  ENDFOR
ENDIF
IF !EMPTY(lcWOutput)
  ACTIVATE WINDOW (lcWOutput)
ENDIF
*!********************************************************************
*!
*!              Function: lfDefWin
*!
*!********************************************************************
*
FUNCTION lfDefWin
PARAMETER lnOGBar

*E300857,1 Add these lines to clear the array that hold the browse fields
*and filter expression for all the In range screens in the option grid
*and to close any temp. cursor that the user has moved it's option from
*the advance screen [Begin]

*E300857,1 IF This is the first option in the option grid
IF lnOGBar = 1
  
  *B602931,4 Change this line to add 2 new columns to the array (laBrFldFlt),
  *          column 4 will hold the first part - until the field that will
  *          be returned by the gfrange() in the temp. cursor. - of index
  *          expression of the active (controlling) index tag on the
  *          "In Range" browse file and column 5 will hold the name and row
  *          number of the array creating this option [Begin]
  *DIMENSION laBrFldFlt[1,3]
  DIMENSION laBrFldFlt[1,5]
  *B602931,4 Change this line to add 2 new columns to the array [End]
  
  laBrFldFlt = ''
  
  *E300857,1 IF There was one or more in range operators in the variable
  *filter
  IF ALEN(laVrFltCur , 1) > 1
    
    *E300857,1 FOR Loop to scan the array that hold In range operator temp.
    *cursor names for the variable filter
    FOR lnRowNumb = 2 TO ALEN(laVrFltCur , 1)
      
      *E300857,1 IF This option [The one saved in the array laVrFltCur] was
      *moved from the variable filter or if the operator was changed
      IF laVrFltCur[lnRowNumb , 1] <> 0 .AND.;
         (laVrFltCur[lnRowNumb , 1] > ALEN(laOGVrFlt , 1) .OR.;
         laOGVrFlt[laVrFltCur[lnRowNumb , 1] , 7] <> 'R')
        
        laVrFltCur[lnRowNumb , 1] = 0
        laVrFltCur[lnRowNumb , 3] = ''
        
        *E300857,1 IF The temp. cursor is opened
        IF USED(laVrFltCur[lnRowNumb , 2])
          
          USE IN (laVrFltCur[lnRowNumb , 2])
        
        ENDIF    && End of IF USED(laVrFltCur[lnRowNumb , 2])
      ENDIF    && End of IF laVrFltCur[lnRowNumb , 1] <> 0 .AND. ....
    ENDFOR    && End of FOR lnRowNumb = 2 TO ALEN(laVrFltCur , 1)
  ENDIF    && End of IF ALEN(laVrFltCur , 1) > 1
  
  *E300900,1 Add these lines to clear the array used to create all
  *E300900,1 the popup options in the option grid and to initialize some
  *E300900,1 variables for the suppress expression [Begin]
  DIMENSION laOGPopAry[1,1]
  laOGPopAry = ''
  lncount    = 0
  lnVrDelObj = 0
  llFxFrsObj = .T.
  llVrFrsObj = .T.
  llFxLastOr = .F.
  llVrLastOr = .F.
  *E300900,1 Add these lines to clear the array used to create [End]
  
ENDIF    && End of IF lnOGBar = 1
*E300857,1 Add these lines to clear the array that hold [End]

*E300900,1 Add this line to initialize some variables for the suppress
*E300900,1 expression [Begin]
llLoop     = .F.
*E300900,1 Add this line to initialize some variables [End]

IF llFirstTime
  WAIT 'Generating Option Grid...' WINDOW NOWAIT
ENDIF  
    lcDefa = ' '
    DO CASE
      CASE lnOGBar<=lnOGVariable AND lnOGVariable>0
       =lfDefVariable(lnOGBar)
      CASE lnOGBar-lnOGVariable<=lnOGFixFl AND lnOGFixFl>0
        
        *E300900,1 Change this line to add a new parameter to (lfDefVarPart)
        *E300900,1 to know if it's creating a fixed filter option or
        *E300900,1 variable filter option [Begin]
        *=lfDefVarPart(lnOGBar,lnOGBar-lnOGVariable,'laOGFxFlt') &&-lnOGVariable
        =lfDefVarPart(lnOGBar , lnOGBar-lnOGVariable , 'laOGFxFlt' , .T.)
        *E300900,1 Change this line to add a new parameter [End]
        
      CASE lnOGBar-(lnOGVariable+lnOGFixFl)<=lnOGVarFl AND lnOGVarFl>0
        
        *E300900,1 Change this line to add a new parameter to (lfDefVarPart)
        *E300900,1 to know if it's creating a fixed filter option or
        *E300900,1 variable filter option and to take in mind the removed
        *E300900,1 variable filter options [Begin]
        *=lfDefVarPart(lnOGBar,lnOGBar-(lnOGVariable+lnOGFixFl),'laOGVrFlt') &&-lnOGVariable
        =lfDefVarPart(lnOGBar ,;
                      lnOGBar - (lnOGVariable + lnOGFixFl + lnVrDelObj) ,;
                      'laOGVrFlt' , .F.)
        *E300900,1 Change this line to add a new parameter [End]
        
    ENDCASE 



*!********************************************************************
*!
*!              Function: lfActWin
*!
*!********************************************************************
*
FUNCTION lfActWin
=lfActvateWin(1)


*!********************************************************************
*!
*!              Function: lfActvateWin
*!
*!********************************************************************
*
FUNCTION lfActvateWin
PARAMETERS lnWinNo
       FOR lnOGCount= 1 TO ALEN(laOGObjCnt,1)
         IF laOGObjCnt[lnOGCount]              
           SHOW OBJECT lnOGCount ENABLE
         ELSE
           SHOW OBJECT lnOGCount DISABLE
         ENDIF
      ENDFOR                
RETURN
lnOGStart=IIF(lnWinNo>1,laOGWinObj[lnWinNo-1]+1,1)
lnOGEnd  =IIF(lnWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
              ALEN(laOGObjCnt,1),laOGWinObj[lnWinNo])
llOGCanAcc=.F.              
FOR lnOGCount= lnOGStart TO lnOGEnd
  IF laOGObjCnt[lnOGCount]              
    llOGCanAcc=.T.
    EXIT
  ENDIF
ENDFOR           
IF llOGCanAcc     
  lnWinDisp=lnWinNO
  FOR ln=1 TO CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax)
    IF ln=lnWinNo
       lnOGStart=IIF(lnWinNo>1,laOGWinObj[lnWinNo-1]+1,1)
       lnOGEnd  =IIF(lnWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
                       ALEN(laOGObjCnt,1),laOGWinObj[lnWinNo])
       FOR lnOGCount= lnOGStart TO lnOGEnd
         IF laOGObjCnt[lnOGCount]              
           SHOW OBJECT lnOGCount ENABLE
         ELSE
           SHOW OBJECT lnOGCount DISABLE
         ENDIF
       ENDFOR                
      SHOW WINDOW (laOGWinName[ln]) TOP
    ELSE
      SHOW GETS WINDOW (laOGWinName[ln]) DISABLE ONLY
    ENDIF  
  ENDFOR  
  =lfModiScrol()
ENDIF  




*!********************************************************************
*!
*!              Function: lfDefVariable
*!
*!********************************************************************
*
FUNCTION lfDefVariable
PARAMETERS lnOGObjNum
lnOGObjPos=MOD(lnOGObjNum,lnOGMax)
lnOGObjPos=IIF(lnOGObjPos=0,lnOGMax,lnOGObjPos)
lnOGChar=lnOGObjPos
lnOGObjPos=(lnOGObjPos-1)*4+1
lnOGNumObj=0
STORE '' TO lcValid,lcWhen,lcDefa,lcPict

SELECT SYREPUVR
*GO TOP

*LOCATE FOR cRep_ID=lcOGRepID AND cExpType='V' AND lAskRunT AND lDispOG;
           AND nVarPos=lnOGObjNum

LOCATE FOR cRep_ID+cexptype+STR(nVarPos)=PADR(lcOGRepID,8)+'V'+STR(lnOGObjNum) AND lAskRunT AND lDispOG

IF FOUND()
  
  *E300900,1 Add these lines to add the capability of suppress
  *E300900,1 expression [Begin]
  
  *E300900,1 If the variable will not be displayed in the option grid
  *E300900,1 (If the variable is to be suppressed)
  IF !EMPTY(mSupExpr) .AND. EVALUATE(ALLTRIM(mSupExpr))
    llLoop = .T.
    RETURN
  ENDIF    && End of IF !EMPTY(mSupExpr) .AND. EVALUATE(ALLTRIM(mSupExpr))
  lncount = lncount + 1
  *E300900,1 Add these lines to add the capability of suppress [End]
  
  *E300869,1 Change this line to allow expressions in filter option
  *headers [Begin]
  
  *lcProm=cFld_Head
  lcProm = IIF(LEFT(cFld_Head , 1) = '~' ,;
               EVALUATE(ALLTRIM(SUBSTR(cFld_Head , 2))) , cFld_Head)
  
  *E300869,1 Change this line to allow expressions in filter option [End]
  
  *E300869,1 Change this line to allow changing the operator prompt [Begin]
  
  *lcOper='Is '
  
  *E300900,1 Change this line to use proper English [Begin]
  *lcOper = IIF(EMPTY(mOperPrmp) , 'Is ' , STRTRAN(mOperPrmp , '~'))
  lcOper = IIF(EMPTY(mOperPrmp) , 'is ' , STRTRAN(mOperPrmp , '~'))
  *E300900,1 Change this line to use proper English [End]
  
  *E300869,1 Change this line to allow changing the operator prompt [End]
  
  lcOGVarName=STRTRAN(ALLTRIM(mFld_Name),CHR(13)+CHR(10),'')
  *E300667,1 Hesham El-Sheltawi (Start)
*  IF !EMPTY(SET('PROC')) AND ALIAS()<>'SYDFIELD'
  IF (!EMPTY(SET('PROC')) OR llProgram )AND ALIAS()<>'SYDFIELD'
  *E300667,1 Hesham El-Sheltawi (End)

    *BADRAN NOW
    *E301170,1 [begin
    *lcValid=IIF(EMPTY(MVAld_Str),'','Valid '+ALLTRIM(MVAld_Str))

    lcValid=IIF(EMPTY(MVAld_Str),IIF(lNotChFlt,'','Valid lfvAskVld()'),;
            'Valid ' + IIF(lNotChFlt,'','lfvAskVld() AND ') + ALLTRIM(MVAld_Str))
    *E301170,1 [end

    lcValid=STRTRAN(lcValid,'{{HPOS}}',ALLTRIM(STR(lnOGObjPos)))  
    lcWhen=IIF(ATC('WHEN ',lcValid)>0,SUBSTR(lcValid,ATC('WHEN ',lcValid)),'')
    lcValid=STRTRAN(lcValid,lcWhen,'')
  
  ENDIF  
  lcWhen=IIF(!EMPTY(lcWhen),'WHEN '+'lfWhen() AND '+STRTRAN(UPPER(lcWhen),'WHEN '),'WHEN lfWhen()')
  laOGObjType[lnCount,1]=lcOGVarName
  laOGObjType[lnCount,2]=lcOGVarName
  laogobjtype[lnCount,3]=.f.

  *lcOGVarName
  lcDefa=''
  DO CASE
    CASE cData_Typ = 'D'
      lcPict = [{  /  /  }]
      *lcValid+' '+lcWhen
         lnOGNumObj=1
         lnColSize=10
    CASE cData_Typ = 'L'
      IF COBJ_TYPE='C'
          
          *E300869,1 Change this line to allow expressions in filter
          *option headers [Begin]
          
          *lcOGPrmpt=IIF(EMPTY(mObjPrmpt),IIF(EMPTY(cfld_Head),lcOGVarName,cfld_Head),mObjPrmpt)      
          
          lcOGPrmpt = IIF(EMPTY(mObjPrmpt) , IIF(EMPTY(cfld_Head) ,;
                          lcOGVarName , IIF(LEFT(cFld_Head , 1) = '~' ,;
                          EVALUATE(ALLTRIM(SUBSTR(cFld_Head , 2))) ,;
                          cFld_Head)) , mObjPrmpt)
          
          *E300869,1 Change this line to allow expressions in filter [End]
          
          lcPict="@*C "+lcOGPrmpt
          lcOper=' '
          lnColSize=20
          *+lcValid+' '+lcWhen
       ELSE   
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))       
 
           *E300900,1 Change this line to add the capability to make valid
           *E300900,1 entries from file or from array [Begin]
           
           *lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
           *           +lcOGVarName+'",'+'"L'+'"'+','+'"E'+'"'+','+'"';
           *           +IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.');
           *           +'"'+','+;
           *           lcRowPos+','+lcColPos+;
           *           ','+lcRowPos+;
           *           ','+lcColPos+')'
           
           *E300900,1 This variable is to hold the validation of the object
           *E300900,1 in string to be used when the object is created
           *lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' , 'lfVldEntPop("');
           *            + lcOGVarName + '",' + '"L' + '"' + ',' + '"E';
           *            + IIF(EMPTY(cVldEntTyp) , 'E' , cVldEntTyp) + '"';
           *            + ',' + '"' +IIF(!EMPTY(ALLTRIM(MvEntries)) ,;
           *            ALLTRIM(MvEntries) , 'Yes' + lcElmSep + 'No' +;
           *            lcValSep + '.T.' + lcElmSep + '.F.');
           *            + '"' + ',' + lcRowPos + ',' + lcColPos + ',' +;
           *            lcRowPos + ',' + lcColPos + ')'
           lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' , 'lfVldEntPop("');
                       + lcOGVarName + '",' + '"L' + '"' + ',' + '"';
                       + IIF(EMPTY(cVldEntTyp) , 'E' , cVldEntTyp) + '"';
                       + ',' + '"' +IIF(!EMPTY(ALLTRIM(MvEntries)) ,;
                       ALLTRIM(MvEntries) , 'Yes' + lcElmSep + 'No' +;
                       lcValSep + '.T.' + lcElmSep + '.F.');
                       + '"' + ',' + lcRowPos + ',' + lcColPos + ',' +;
                       lcRowPos + ',' + lcColPos + ')'
           
           *E300900,1 Change this line to add the capability [End]
           
           
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)
         lnColSize=25
         IF !_WINDOWS           
           lcDefa=PADR(lfGetDef(lcOGVarName,'E',;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )),24)+IIF(_DOS,'','')
           lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')
           laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[','E',']+;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 

           lcPict="@*IVN "
           lcValid='VALID '+lcOGValid
           ELSE
             IF _WINDOWS
                 IF !EMPTY(INVB[1])
                   DIMENSION INVB[ALEN(INVB,1)+1]
                 ENDIF
                 lnOgBarNo=1
                
                *E300900,1 Change this line to create array popup instead
                *E300900,1 of list popup [Begin]
                
                *lcPict="@^ " + lfGetPopPrm(lcOGVarName,'E',;
                *                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                *                ),.f.,'L')
                
                *E300900,1 This variable is to hold the picture of the
                *E300900,1 object in string to be used when the object is
                *E300900,1 created
                lcPict = "@^ "
                
                *E300900,1 Change this line to create array popup [End]
                
                *E300900,1 Change this line to add the capability to make
                *E300900,1 valid entries from file or from array [Begin]
                
                *laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[','E',']+;
                *                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                *                )+[')]
                laOGObjType[lnCount,3] = [lfGetDef('] + lcOGVarName + [','];
                                         + IIF(EMPTY(cVldEntTyp) , "E" ,;
                                               cVldEntTyp);
                                         + [','] +;
                                         IIF(!EMPTY(ALLTRIM(mVEntries)) ,;
                                             ALLTRIM(mVEntries) , 'Yes' +;
                                             lcElmSep + 'No' + lcValSep +;
                                             '.T.' + lcElmSep + '.F.');
                                         + [')]
                
                *E300900,1 Change this line to add the capability [End]
                
                lcDefa=lnOgBarNo
                
                *E300900,1 Change this line to create array popup instead
                *E300900,1 of list popup
                *E300900,1 Note :
                *E300900,1        I moved the calling of lfGetPopPrm from
                *E300900,1        lcPict to lcValid because of the changes
                *E300900,1        that I made in the return value of the
                *E300900,1        function to create array popup instead
                *E300900,1        of list popup, I also changed the calling
                *E300900,1        of lfGetPopPrm to add the capability to
                *E300900,1        make valid entries from file and from
                *E300900,1        array
                
                *lcValid='VALID '+lcOGValid                 
                lcValid = lfGetPopPrm(lcOGVarName ,;
                          IIF(EMPTY(cVldEntTyp) , 'E' , cVldEntTyp);
                          , IIF(!EMPTY(ALLTRIM(mVEntries)) ,;
                                ALLTRIM(mVEntries) , 'Yes' + lcElmSep +;
                                'No' + lcValSep + '.T.' + lcElmSep + '.F.');
                          , .F. , 'L');
                          + ' VALID ' + lcOGValid
                
                *E300900,1 Change this line to create array popup [End]
                
                STORE lnOgBarNo TO INVB[ALEN(INVB,1)]                
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                                
                laOGObjType[lnCount,2]=lcOGVarName 
             ENDIF             
          ENDIF   
        ENDIF  
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
    CASE cData_Typ = 'N'
      IF COBJ_TYPE='P'
        
        *E300869,1 Change this line to allow expressions in filter option
        *headers [Begin]
        
        *lcOGPrmpt=ALLTRIM(IIF(EMPTY(mObjPrmpt),IIF(EMPTY(cfld_Head),lcOGVarName,cfld_Head),mObjPrmpt))      
        
        lcOGPrmpt = ALLTRIM(IIF(EMPTY(mObjPrmpt) , IIF(EMPTY(cfld_Head) ,;
                    lcOGVarName , IIF(LEFT(cFld_Head , 1) = '~' ,;
                    EVALUATE(ALLTRIM(SUBSTR(cFld_Head , 2))) ,;
                    cFld_Head)) , mObjPrmpt))
        
        *E300869,1 Change this line to allow expressions in filter [End]
        
          lcPict="@*HN "+lcOGPrmpt
          lnColSize=25
*          len(STRTRAN(STRTRAN(STRTRAN(lcOGPrmpt,'\<'),'\!'),'\?'))+1
          *+lcValid+' '+lcWhen
      ELSE
       DO CASE
         CASE lVldEntry OR !EMPTY(mVEntries)
         lcCodes_Fld=cCodes_Fld
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
          
          *E300900,1 Change this line to add the capability to make valid
          *E300900,1 entries from file or from array [Begin]
          
          *lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
          *           +lcOGVarName+'",'+'"N'+'"'+','+'"'+IIF(lVldEntry,'C','E')+'"'+','+'"';
          *            +IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+'"'+','+;
          *            lcRowPos+','+lcColPos+;
          *            ','+lcRowPos+;
          *            ','+lcColPos+')'
          
          *E300900,1 This variable is to hold the validation of the object
          *E300900,1 in string to be used when the object is created
          lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' , 'lfVldEntPop("');
                      + lcOGVarName + '",' + '"N' + '"' + ',' + '"';
                      + IIF(lVldEntry , 'C' , IIF(EMPTY(cVldEntTyp) ,;
                            'E' , cVldEntTyp));
                      + '"' + ',' + '"';
                      + IIF(lVldEntry .OR. cVldEntTyp = 'C' , lcCodes_Fld ,;
                            ALLTRIM(mVEntries));
                      + '"' + ',' + lcRowPos + ',' + lcColPos + ',' +;
                      lcRowPos + ',' + lcColPos + ')'
          
          *E300900,1 Change this line to add the capability [End]
          
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
         lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')         
         lcValid='Valid '+lcOGValid
         lnColSize=25
         IF !_WINDOWS  
         lcPict="@*IVN "
         lcDefa=PADR(lfGetDef(lcOGVarName,IIF(lVldEntry,'C','E'),;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))),24)+IIF(_DOS,'','')
          lcCodes_Fld=cCodes_Fld
            laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 
           ELSE
                 IF !EMPTY(INVB[1])
                   DIMENSION INVB[ALEN(INVB,1)+1]
                 ENDIF
                 
                 *E300900,1 Change this line to create array popup instead
                 *E300900,1 of list popup [Begin]
                 
                 *lcPict="@^ " + lfGetPopPrm(lcOGVarName,IIF(lVldEntry,'C','E'),;
                 *IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries)),.F.,'C')
                 
                 *E300900,1 This variable is to hold the picture of the
                 *E300900,1 object in string to be used when the object is
                 *E300900,1 created
                 lcPict = "@^ "
                 
                 *E300900,1 Change this line to create array popup [End]
                 
             *E300900,1 Change this line to add the capability to make
             *E300900,1 valid entries from file or from array [Begin]
             
             *laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
             *                   IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+[')]                
             laOGObjType[lnCount,3] = [lfGetDef('] + lcOGVarName + [','];
                                      + IIF(lVldEntry , 'C' ,;
                                            IIF(EMPTY(cVldEntTyp) , 'E' ,;
                                                cVldEntTyp));
                                      + [','] +;
                                      IIF(lVldEntry .OR. cVldEntTyp = 'C' ,;
                                          lcCodes_Fld , ALLTRIM(mVEntries));
                                      + [')]                
             
             *E300900,1 Change this line to add the capability [End]
             
             *E300900,1 Add this line to create array popup instead
             *E300900,1 of list popup
             *E300900,1 Note :
             *E300900,1        I moved the calling of lfGetPopPrm from
             *E300900,1        lcPict to lcValid because of the changes
             *E300900,1        that I made in the return value of the
             *E300900,1        function to create array popup instead
             *E300900,1        of list popup, I also changed the calling
             *E300900,1        of lfGetPopPrm to add the capability to
             *E300900,1        make valid entries from file and from
             *E300900,1        array
             lcValid = lfGetPopPrm(lcOGVarName ,;
                       IIF(lVldEntry , 'C' , IIF(EMPTY(cVldEntTyp) ,;
                                                 'E' , cVldEntTyp)) ,;
                       IIF(lVldEntry .OR. cVldEntTyp = 'C' ,;
                           lcCodes_Fld , ALLTRIM(mVEntries));
                       , .F. , 'C');
                       + ' ' + lcValid
             
             *E300900,1 Add this line to create array popup [End]
             
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                
                laOGObjType[lnCount,2]=lcOGVarName                 
                lcDefa=lnOgBarNo                
                 STORE lnOgBarNo TO INVB[ALEN(INVB,1)]              
          ENDIF 
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
         CASE !EMPTY(MVAld_Str)
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
           lnOGNumObj=1           
         OTHERWISE
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
       ENDCASE      
       ENDIF
         lnOGNumObj=1         
    CASE cData_Typ = 'C'
       DO CASE
         CASE lVldEntry OR !EMPTY(mVEntries)
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
         lcCodes_Fld=cCodes_Fld
          
          *E300900,1 Change this line to add the capability to make valid
          *E300900,1 entries from file or from array [Begin]
          
          *lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
          *           +lcOGVarName+'",'+'"C'+'"'+','+'"'+IIF(lVldEntry,'C','E')+'"'+','+'"';
          *            +IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+'"'+','+;
          *            lcRowPos+','+lcColPos+;
          *            ','+lcRowPos+;
          *            ','+lcColPos+')'
          
          *E300900,1 This variable is to hold the validation of the object
          *E300900,1 in string to be used when the object is created
          lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' , 'lfVldEntPop("');
                      + lcOGVarName + '",' + '"C' + '"' + ',' + '"';
                      + IIF(lVldEntry , 'C' , IIF(EMPTY(cVldEntTyp) , 'E' ,;
                            cVldEntTyp));
                      + '"' + ',' + '"';
                      + IIF(lVldEntry .OR. cVldEntTyp = 'C' , lcCodes_Fld ,;
                            ALLTRIM(mVEntries));
                      + '"' + ',' + lcRowPos + ',' + lcColPos + ',';
                      + lcRowPos + ',' + lcColPos + ')'
          
          *E300900,1 Change this line to add the capability [End]
          
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
         lcValid='Valid '+lcOGValid
         lnColSize=25

         IF !_WINDOWS  
            lcDefa=PADR(lfGetDef(lcOGVarName,IIF(lVldEntry,'C','E'),;
                                      IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries))),24)+IIF(_DOS,'','')

            laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 
         lcPict="@*IVN "
           ELSE
             IF !EMPTY(INVB[1])
               DIMENSION INVB[ALEN(INVB,1)+1]
             ENDIF
                 
                 *E300900,1 Change this line to create array popup instead
                 *E300900,1 of list popup [Begin]
                 
                 *lcPict="@^ " + lfGetPopPrm(lcOGVarName,IIF(lVldEntry,'C','E'),;
                 *IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries)),.F.,'C')
                 
                 *E300900,1 This variable is to hold the picture of the
                 *E300900,1 object in string to be used when the object is
                 *E300900,1 created
                 lcPict = "@^ "
                 
                 *E300900,1 Change this line to create array popup [End]
                 
             *E300900,1 Change this line to add the capability to make
             *E300900,1 valid entries from file or from array [Begin]
             
             *laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
             *                   IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+[')]                
             laOGObjType[lnCount,3] = [lfGetDef('] + lcOGVarName + [','] +;
                                      IIF(lVldEntry , 'C' ,;
                                          IIF(EMPTY(cVldEntTyp) , 'E' ,;
                                              cVldEntTyp));
                                      + [','] +;
                                      IIF(lVldEntry .OR. cVldEntTyp = 'C' ,;
                                          lcCodes_Fld , ALLTRIM(mVEntries));
                                      + [')]                
             
             *E300900,1 Change this line to add the capability [End]
             
                *E300900,1 Remove this line to move it after the calling
                *E300900,1 of lfGetPopPrm to be able to get the default
                *E300900,1 value [Begin]
                *lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                
                *E300900,1 Remove this line to move it after [End]
                
                *E300900,1 Change this line to create array popup instead
                *E300900,1 of list popup
                *E300900,1 Note :
                *E300900,1        I moved the calling of lfGetPopPrm from
                *E300900,1        lcPict to lcValid because of the changes
                *E300900,1        that I made in the return value of the
                *E300900,1        function to create array popup instead
                *E300900,1        of list popup, I also changed the calling
                *E300900,1        of lfGetPopPrm to add the capability to
                *E300900,1        make valid entries from file and from
                *E300900,1        array
                
                *lcValid='Valid '+lcOGValid
                lcValid = lfGetPopPrm(lcOGVarName ,;
                          IIF(lVldEntry , 'C' , IIF(EMPTY(cVldEntTyp) ,;
                                                    'E' , cVldEntTyp)) ,;
                          IIF(lVldEntry .OR. cVldEntTyp = 'C' ,;
                              lcCodes_Fld , ALLTRIM(mVEntries));
                          , .F. , 'C');
                          + ' Valid ' + lcOGValid
                
                *E300900,1 Add this line to create array popup [End]
                
                *E300900,1 Add this line to move it after the calling
                *E300900,1 of lfGetPopPrm to be able to get the default
                *E300900,1 value [Begin]
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                
                *E300900,1 Add this line to move it after [End]
                
                lcDefa=lnOgBarNo
                laOGObjType[lnCount,2]=lcOGVarName              
                STORE lnOGBarNo TO INVB[ALEN(INVB,1)]                            
          ENDIF 
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
         CASE !EMPTY(MVAld_Str)
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
           lnOGNumObj=1           
         OTHERWISE
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
       ENDCASE
    CASE cData_Typ='M'
       lcPict=lfOGGetPct(cData_Typ)
       lnColSize=lfGetSize(25)       
       lnOGNumObj=1       
  ENDCASE 
  lnOGNumObj=1        
ENDIF     
RETURN lnOGNumObj

*!*************************************************************************
*!
*!              Function: lfDefVarPart
*!
*!*************************************************************************
*  this function define the objects of the fixed part of the filter
FUNCTION lfDefVarPart

*E300900,1 Change this line to add a new parameter to know if the function
*E300900,1 is creating a fixed filter option or variable filter
*E300900,1 option [Begin]
*PARAMETERS lnOGObjNum,lnOGArrPos,lcOGArrName
PARAMETERS lnOGObjNum , lnOGArrPos , lcOGArrName , llFixFiltr
*E300900,1 Change this line to add a new parameter [End]

STORE '' TO lcValid,lcWhen,lcDefa,lcPict
lnOGObjPos=MOD(lnOGObjNum,lnOGMax)
lnOGObjPos=IIF(lnOGObjPos=0,lnOGMax,lnOGObjPos)
lnOGChar=lnOGObjPos
lnOGObjPos=(lnOGObjPos-1)*4+1
lnOGNumObj=0

IF &lcOGArrName[lnOGArrPos,1]='.OR.'
  
  *E300900,1 Add these lines to add the suppress expression
  *E300900,1 capability [Begin]
  
  *E300900,1 If this is the first object to be displayed in the fixed
  *E300900,1 filter or variable filter.
  IF IIF(llFixFiltr , llFxFrsObj .OR. llFxLastOr ,;
         llVrFrsObj .OR. llVrLastOr)
    
    *E300900,1 If variable filter
    IF !llFixFiltr
      =gfADel(@&lcOGArrName. , lnOGArrPos , 1)
      lnVrDelObj = lnVrDelObj + 1
    ENDIF    && End of IF !llFixFiltr
    llLoop = .T.
    RETURN
  ENDIF    && End of IF IIF(llFixFiltr , llFxFrsObj , llVrFrsObj)
  
  lncount = lncount + 1
  
  *E300900,1 If fixed filter
  IF llFixFiltr
    llFxLastOr = .T.
  ELSE    && Else [If variable filter]
    llVrLastOr = .T.
  ENDIF    && End of IF llFixFiltr
  
  *E300900,1 Add these lines to add the suppress expression [End]
  
  lcProm=''
  
  *E300900,1 Change this line to use proper English [Begin]
  *lcOper=PADC('OR',23)
  lcOper = PADC('Or' , 23)
  *E300900,1 Change this line to use proper English [Begin]
  
  lcPict="@*IVN "
  lcWhen='when lfwhen() and .f.'
  lcValid=''
  lcOGVarName='INV'
  lnOGNumObj=1       
  lcDefa=''
  RETURN lnOGNumObj
ENDIF


=lfGetRepFld(&lcOGArrName[lnOGArrPos,1])

*E300900,1 Add these lines to add the suppress expression capability [Begin]

*E300900,1 If the filter option will not be displayed
*E300900,1 (If the filter option is to be suppressed)
IF ALIAS() = 'SYREPUVR' .AND. !EMPTY(mSupExpr) ;
   .AND. EVALUATE(ALLTRIM(mSupExpr))
  
  *E300900,1 If fixed filter
  IF llFixFiltr
    DO CASE
      
      *E300900,1 Case of (In list) or (Between) operators
      CASE ALLTRIM(&lcOGArrName[lnOGArrPos,5]) = 'In List' .OR.;
           ALLTRIM(&lcOGArrName[lnOGArrPos,5]) = 'Between'
        
        lcEmptVal = ''
        
        *B802093,1 Add this line to make the In Range filter options take the cursor name again
        *          if the filter option reappeared in the grid again [Begin] 
        IF ALLTRIM(&lcOGArrName[lnOGArrPos,7]) = 'R'
          &lcOGArrName[lnOGArrPos,7] = 'V'
        ENDIF
        *B802093,1 Add this line to make the In Range filter options take the cursor name [End]
      
      *E300900,1 Case of filter option of type date
      CASE &lcOGArrName[lnOGArrPos,3] = 'D'
        lcEmptVal = {  /  /  }
      
      *E300900,1 Case of filter option of type numeric
      CASE &lcOGArrName[lnOGArrPos,3] = 'N'
        lcEmptVal = 0
      
      *E300900,1 Otherwise
      OTHERWISE
        lcEmptVal = ''
      
    ENDCASE
    
    &lcOGArrName[lnOGArrPos,6] = lcEmptVal
  ELSE    && Else [If variable filter]
    =gfADel(@&lcOGArrName. , lnOGArrPos , 1)
    lnVrDelObj = lnVrDelObj + 1
  ENDIF    && End of IF llFixFiltr
  llLoop = .T.
  RETURN
  
ELSE    && Else [If the filter option will be displayed (not to be suppressed)]
  
  *E300900,1 If fixed filter
  IF llFixFiltr
    llFxLastOr = .F.
    llFxFrsObj = .F.
    lncount    = lncount + 1
  ELSE    && Else [If variable filter]
    llVrLastOr = .F.
    llVrFrsObj = .F.
    lncount    = lncount + IIF(llFxLastOr , 0 , 1)
    llFxLastOr = .F.
  ENDIF    && End of IF llFixFiltr

ENDIF    && End of IF ALIAS() = 'SYREPUVR' .AND. ... .AND. EVALUATE(ALLTRIM(mSupExpr))

*E300900,1 Add these lines to add the suppress expression capability [End]

lcOGFieldNam=IIF(ALIAS()='SYDFIELD','cFld_Name','ALLTRIM(mFld_Name)')
lcOGSize=IIF(_WINDOWS,[FONT "MS SANS SERIF",8 STYLE "B" COLOR RGB(0,0,0,,,)],'')           

*E300869,1 Change this line to allow expressions in filter option
*headers [Begin]

*lcProm=IIF(EMPTY(ALLTRIM(cFld_Head)),&lcOGFieldNam,cFld_Head)

lcProm = IIF(EMPTY(ALLTRIM(cFld_Head)) , &lcOGFieldNam ,;
             IIF(ALIAS() = 'SYREPUVR' .AND. LEFT(cFld_Head , 1) = '~' ,;
             EVALUATE(ALLTRIM(SUBSTR(cFld_Head , 2))) , cFld_Head))

*E300869,1 Change this line to allow expressions in filter option [End]

lcDefa=''
lcOGVarName = lcOGArrName+'['+ALLTRIM(STR(lnOGArrPos))+',6]'
  *E300667,1 Hesham El-Sheltawi (Start)
  *IF !EMPTY(SET('PROC')) AND ALIAS()<>'SYDFIELD'
  IF (!EMPTY(SET('PROC')) OR llProgram) AND ALIAS()<>'SYDFIELD'  
  *E300667,1 Hesham El-Sheltawi (End)
    lcValid=IIF(EMPTY(ALLTRIM(MVAld_Str)),'','Valid '+ALLTRIM(MVAld_Str))
    lcValid=STRTRAN(lcValid,'{{HPOS}}',ALLTRIM(STR(lnOGObjPos)))  
  ENDIF    

*E300869,1 Change this line to allow changing the operator prompt [Begin]

*lcOper=IIF(&lcOGArrName[lnOGArrPos,4],'Is','Is not')+' '+IIF(&lcOGArrName[lnOGArrPos,5] ='Like','',&lcOGArrName[lnOGArrPos,5])

*E300900,1 Change this line to use proper English [Begin]

*lcOper = IIF(ALIAS() = 'SYREPUVR' .AND. !EMPTY(mOperPrmp) ;
*             .AND. lfOperPrmp(@lcOper) , lcOper ,;
*             IIF(&lcOGArrName[lnOGArrPos,4] , 'Is' , 'Is not' ) + ' ' +;
*                 IIF(&lcOGArrName[lnOGArrPos,5] = 'Like' , '' ,;
*                     &lcOGArrName[lnOGArrPos,5]))
lcOper = IIF(ALIAS() = 'SYREPUVR' .AND. !EMPTY(mOperPrmp) ;
             .AND. lfOperPrmp(@lcOper) , lcOper ,;
             IIF(&lcOGArrName[lnOGArrPos,4] , 'is' , 'is not' ) + ' ' +;
                 IIF(&lcOGArrName[lnOGArrPos,5] = 'Like' , '' ,;
                     LOWER(&lcOGArrName[lnOGArrPos,5])))

*E300900,1 Change this line to use proper English [End]

*E300869,1 Change this line to allow changing the operator prompt [End]

laOGObjType[lnCount,1]=lcOGVarName
laOGObjType[lnCount,2]=lcOGVarName

IF &lcOGArrName[lnOGArrPos,7] $ 'E' AND lcRunDirct != 'R'
  &lcOGVarName = EVAL(&lcOGArrName[lnOGArrPos,6])
  &lcOGArrName[lnOGArrPos,7] = 'V'
ENDIF

IF !INLIST(ALLTRIM(&lcOGArrName[lnOGArrPos,5]),'In List','Between')
  lcWhen=IIF(ATC('WHEN ',lcValid)>0,SUBSTR(lcValid,ATC('WHEN ',lcValid)),'')
  lcValid=STRTRAN(lcValid,lcWhen,'')
  lcWhen=IIF(!EMPTY(lcWhen),'WHEN '+'lfWhen() AND '+STRTRAN(UPPER(lcWhen),'WHEN '),'WHEN lfWhen()')                  
ELSE
  lcWhen='WHEN lfWhen()'
ENDIF  

*E300857,1 Add these lines to change the value of column 7 in the array
*lcOGArrName [laOGFxFlt & laOGVrFlt] from 'R' ('R' For range) to 
*'V' ('V' For value) if the user changed the operator from In List to
*anything else [Begin]

*E300857,1 if the value of column 7 in the array lcOGArrName is 'R' and
*the user changed the operator from In List to anything else 
IF &lcOGArrName[lnOGArrPos,7] = 'R' .AND. ;
   ALLTRIM(&lcOGArrName[lnOGArrPos,5]) <> 'In List'

  &lcOGArrName[lnOGArrPos,7] = 'V'

ENDIF    && End of IF &lcOGArrName[lnOGArrPos,7] = 'R'...
*E300857,1 Add these lines to change the value of column 7 [End]

DO CASE
  
  *E300857,1 Add these lines to handel the In List operator in the case
  *of using the gfRange() {In range screen} [Begin]
  
  *E300857,1 in case of In list operator with In range screen
  CASE &lcOGArrName[lnOGArrPos,7] = 'R'
    
    = lfRangCurs(lcOGArrName , lnOGArrPos)
    
    *B602931,4 Change this line to add 2 new columns to the array
    *        (laBrFldFlt) [Begin]
    *DIMENSION laBrFldFlt[ALEN(laBrFldFlt , 1) + 1 , 3]
    DIMENSION laBrFldFlt[ALEN(laBrFldFlt , 1) + 1 , 5]
    *B602931,4 Change this line to add 2 new columns to the array [End]
    
    lnRowNumb = ALEN(laBrFldFlt , 1)          && Variable to hold the number of the last row in the array laBrFldFlt
    lcRowNumb = ALLTRIM(STR(lnRowNumb))       && Variable to hold the number of the last row in the array laBrFldFlt as a string
    
    *E301302,1 Change these lines to give the capability to use macro
    *          substitution in the in range fields [Begin]
    *laBrFldFlt[lnRowNumb , 1] = ALLTRIM(mBrwFile)
    *laBrFldFlt[lnRowNumb , 2] = ALLTRIM(mBrwFields)
    *laBrFldFlt[lnRowNumb , 3] = ALLTRIM(mBrwFltExp)
    laBrFldFlt[lnRowNumb , 1]  = lfMacroSub(ALLTRIM(mBrwFile))
    laBrFldFlt[lnRowNumb , 2]  = lfMacroSub(ALLTRIM(mBrwFields))
    laBrFldFlt[lnRowNumb , 3]  = lfMacroSub(ALLTRIM(mBrwFltExp))
    *E301302,1 Change these lines to give the capability to use macro [End]

    *B602931,4 Add these lines to add 2 new columns to the array
    *        (laBrFldFlt) [Begin]
    
    *-- This column will hold the first part - until the field that will
    *-- be returned by the gfrange() in the temp. cursor. - of index
    *-- expression of the active (controlling) index tag on the "In Range"
    *-- browse file.
    laBrFldFlt[lnRowNumb , 4] = IIF(ALIAS() = 'SYREPUVR' .AND.;
                                    cExpType = 'E' , .F. , '')
    *-- This column will hold the name and row number of the array creating
    *-- this option.
    laBrFldFlt[lnRowNumb , 5] = ALLTRIM(lcOGArrName) + ':' +;
                                ALLTRIM(STR(lnOGArrPos))
    *B602931,4 Add these lines to add 2 new columns to the array [End]
    
    lcTmpCursr = &lcOGArrName[lnOGArrPos,6]         && Variable to hold the name of the temp. cursor that will be used be the In range screen
    
    *E300799,4 Change this line to pass 2 more parameters to
    *OGInRange [Begin]
    *lcOGVld    = 'OGInRang("' +;
    *             'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
    *             'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
    *             'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
    *             ALLTRIM(cClrRngSel) + '" , "' +;
    *             ALLTRIM(cSetFunc) + '" , "' +;
    *             ALLTRIM(cSetParm) + '" , "' +;
    *             IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
    *                 ALLTRIM(SUBSTR(mFld_Name , AT('.' , mFld_Name) + 1))) +;
    *             '" , "' + lcTmpCursr + '")'
    
    *B602931,4 Change this line to pass a new parameter to OGInRange(), the
    *          parameter will hold the name of a variable to be filled with
    *          the first part - until the field that will be returned by
    *          the gfrange() in the temp. cursor. - of index expression of
    *          the active (controlling) index tag on the "In Range" browse
    *          file [Begin]
    *lcOGVld    = 'OGInRang("' +;
    *             'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
    *             'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
    *             'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
    *             ALLTRIM(cClrRngSel) + '" , "' +;
    *             ALLTRIM(cSetFunc) + '" , "' +;
    *             ALLTRIM(cSetParm) + '" , "' +;
    *             IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
    *                 ALLTRIM(SUBSTR(mFld_Name , AT('.' , mFld_Name) + 1))) +;
    *             '" , "' + lcTmpCursr + '" , ' +;
    *             IIF(EMPTY(cIndxSrch) , '"Y"' , cIndxSrch) + ' , "' +;
    *             IIF(EMPTY(cPict_Str) , '' , EVALUATE(cPict_Str)) + '")'
    lcOGVld    = 'OGInRang("' +;
                 'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
                 'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
                 'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
                 ALLTRIM(cClrRngSel) + '" , "' +;
                 ALLTRIM(cSetFunc) + '" , "' +;
                 ALLTRIM(cSetParm) + '" , "' +;
                 IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
                     ALLTRIM(SUBSTR(mFld_Name , AT('.' , mFld_Name) + 1))) +;
                 '" , "' + lcTmpCursr + '" , ' +;
                 IIF(EMPTY(cIndxSrch) , '"Y"' , cIndxSrch) + ' , "' +;
                 IIF(EMPTY(cPict_Str) , '' , EVALUATE(cPict_Str)) +;
                 '" , "' +;
                 IIF(ALIAS() = 'SYREPUVR' .AND. cExpType = 'E' ,;
                     '' , 'laBrFldFlt[' + lcRowNumb + ',4]') + '")'
    
    *B602931,4 Change this line to pass a new parameter to OGInRange() [End]
    
    *E300799,4 Change this line to pass 2 more parameters [Begin]
    
    lcOGPrmpt   = 'In List'
    lcPict      = "@*HN " + lcOGPrmpt
    lcOGVarName = 'INV'
    lcValid     = 'VALID ' + lcOGVld
    lnColSize   = 25          
    
  *E300857,1 Add these lines to handel the In List operator [End]
  
  CASE &lcOGArrName[lnOGArrPos,7] = 'V'
    DO CASE 
      
      CASE ALLTRIM(&lcOGArrName[lnOGArrPos,5])='In List'
        
        *E300857,1 Add this if condition to add a new screen (In range) to
        *the In list operator [Begin]
        
        *E300857,1 IF We are not going to use the In range screen
        IF ALIAS() = 'SYDFIELD' .OR. EMPTY(mBrwFile) .OR.;
           (cExpType <> 'F' .AND. EMPTY(cBrwSelFld))
        
        *E300857,1 Add this if condition to add a new screen (In range) [End]
          
          lcCodes_Fld=IIF(ALIAS()='SYDFIELD',SUBSTR(&lcOGArrName[lnOGArrPos,1],ATC('.',&lcOGArrName[lnOGArrPos,1])+1),cCodes_Fld)
          lcValid = STRTRAN(STRTRAN(lcValid,'Valid ',''),'"',"'")
          
          *E300900,1 Change this line to add the capability to make valid
          *E300900,1 entries from file or from array [Begin]
          
          *lcOGVld='OGInlist(['+&lcOGArrName[lnOGArrPos,3]+'],'+;
          *         '['+lfOGGetPct(cData_Typ)+'],'+STR(lfGetSize(25))+;
          *         ',"'+lcValid+'",'+IIF(lVldEntry,'.T.','.F.')+',';
          *         +'['+IIF(lVldEntry,lcCodes_fld,mVEntries)+']'+',['+lcOGArrName+'('+ALLTRIM(STR(lnOGArrPos))+',6)])'
          
          *B801961,1 Change this line to add a new parameter to path the
          *          field length to the function OGINLIST
          *          Note: If the filter option is an expression we will
          *                path 0 as the field length.
          *lcOGVld = 'OGInlist([' + &lcOGArrName[lnOGArrPos,3] + '],' +;
          *          '[' + lfOGGetPct(cData_Typ) + '],' + STR(lfGetSize(25));
          *          + ',"' + lcValid + '","' +;
          *          IIF(lVldEntry , 'C' ,;
          *              IIF(ALIAS() <> 'SYREPUVR' .OR. EMPTY(cVldEntTyp) ,;
          *                  'E' , cVldEntTyp));
          *          + '",' + '[' +;
          *          IIF(lVldEntry .OR.;
          *              (ALIAS() = 'SYREPUVR' .AND. cVldEntTyp = 'C') ,;
          *              lcCodes_fld , mVEntries);
          *          + ']' + ',[' + lcOGArrName + '(' +;
          *          ALLTRIM(STR(lnOGArrPos)) + ',6)])'
          lcOGVld = 'OGInlist([' + &lcOGArrName[lnOGArrPos,3] + '],' +;
                    '[' + lfOGGetPct(cData_Typ) + '],' + STR(lfGetSize(25));
                    + ',"' + lcValid + '","' +;
                    IIF(lVldEntry , 'C' ,;
                        IIF(ALIAS() <> 'SYREPUVR' .OR. EMPTY(cVldEntTyp) ,;
                            'E' , cVldEntTyp));
                    + '",' + '[' +;
                    IIF(lVldEntry .OR.;
                        (ALIAS() = 'SYREPUVR' .AND. cVldEntTyp = 'C') ,;
                        lcCodes_fld , mVEntries);
                    + ']' + ',[' + lcOGArrName + '(' +;
                    ALLTRIM(STR(lnOGArrPos)) + ',6)]' + ' , ' +;
                    ALLTRIM(STR(IIF(ALIAS() = 'SYREPUVR' .AND.;
                                    cExpType = 'E' , 0 , nFld_Wdth))) + ')'
          
          *B801961,1 Change this line to add a new parameter to path [End]
          
          *E300900,1 Change this line to add the capability [End]
          
          lcOGPrmpt='In List'
          lcPict="@*HN "+lcOGPrmpt
          lcOGVarName='INV'
          lcValid='VALID '+lcOGVld
          lnColSize=25          
          
        *E300857,1 Add these lines to add a new screen (In range) to the
        *In list operator [Begin]
        
        ELSE    && Else [IF We are going to use the In range screen]
          
          *B602931,4 Change this line to add 2 new columns to the array
          *        (laBrFldFlt) [Begin]
          *DIMENSION laBrFldFlt[ALEN(laBrFldFlt , 1) + 1 , 3]
          DIMENSION laBrFldFlt[ALEN(laBrFldFlt , 1) + 1 , 5]
          *B602931,4 Change this line to add 2 new columns to the array [End]
          
          lnRowNumb = ALEN(laBrFldFlt , 1)          && Variable to hold the number of the last row in the array laBrFldFlt
          lcRowNumb = ALLTRIM(STR(lnRowNumb))       && Variable to hold the number of the last row in the array laBrFldFlt as a string
          
          *E301302,1 Change these lines to give the capability to use macro
          *		     substitution in the in range fields [Begin]
          *laBrFldFlt[lnRowNumb , 1] = ALLTRIM(mBrwFile)
          *laBrFldFlt[lnRowNumb , 2] = ALLTRIM(mBrwFields)
          *laBrFldFlt[lnRowNumb , 3] = ALLTRIM(mBrwFltExp)
          laBrFldFlt[lnRowNumb , 1] = lfMacroSub(ALLTRIM(mBrwFile))
          laBrFldFlt[lnRowNumb , 2] = lfMacroSub(ALLTRIM(mBrwFields))
          laBrFldFlt[lnRowNumb , 3] = lfMacroSub(ALLTRIM(mBrwFltExp))
          *E301302,1 Change these lines to give the capability to use [End]

          *B602931,4 Add these lines to add 2 new columns to the array
          *        (laBrFldFlt) [Begin]
          
          *-- This column will hold the first part - until the field that
          *-- will be returned by the gfrange() in the temp. cursor. - of
          *-- index expression of the active (controlling) index tag on the
          *-- "In Range" browse file.
          laBrFldFlt[lnRowNumb , 4] = IIF(ALIAS() = 'SYREPUVR' .AND.;
                                          cExpType = 'E' , .F. , '')
          *-- This column will hold the name and row number of the array creating
          *-- this option.
          laBrFldFlt[lnRowNumb , 5] = ALLTRIM(lcOGArrName) + ':' +;
                                      ALLTRIM(STR(lnOGArrPos))
          *B602931,4 Add these lines to add 2 new columns to the array [End]
          
          lcTmpCursr = lfRangCurs(lcOGArrName , lnOGArrPos)     && Variable to hold the name of the temp. cursor that will be used be the In range screen
          
          &lcOGArrName[lnOGArrPos,6] = lcTmpCursr
          &lcOGArrName[lnOGArrPos,7] = 'R'
          
          *E300799,4 Change this line to pass 2 more parameters to
          *OGInRange [Begin]
          
          *lcOGVld    = 'OGInRang("' +;
          *             'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
          *             'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
          *             'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
          *             ALLTRIM(cClrRngSel) + '" , "' +;
          *             ALLTRIM(cSetFunc) + '" , "' +;
          *             ALLTRIM(cSetParm) + '" , "' +;
          *             IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
          *                 ALLTRIM(SUBSTR(mFld_Name , AT('.' ,;
          *                 mFld_Name) + 1))) + '" , "' + lcTmpCursr + '")'
          
          *B602931,4 Change this line to pass a new parameter to
          *          OGInRange(), the parameter will hold the name of a
          *          variable to be filled with the first part - until the
          *          field that will be returned by the gfrange() in the
          *          temp. cursor. - of index expression of the active
          *          (controlling) index tag on the "In Range" browse
          *          file [Begin]
          *lcOGVld    = 'OGInRang("' +;
          *             'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
          *             'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
          *             'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
          *             ALLTRIM(cClrRngSel) + '" , "' +;
          *             ALLTRIM(cSetFunc) + '" , "' +;
          *             ALLTRIM(cSetParm) + '" , "' +;
          *             IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
          *                 ALLTRIM(SUBSTR(mFld_Name , AT('.' ,;
          *                 mFld_Name) + 1))) + '" , "' +;
          *             lcTmpCursr + '" , ' +;
          *             IIF(EMPTY(cIndxSrch) , '"Y"' , cIndxSrch) + ' , "' +;
          *             IIF(EMPTY(cPict_Str) , '' , EVALUATE(cPict_Str)) + '")'
          lcOGVld    = 'OGInRang("' +;
                       'laBrFldFlt[' + lcRowNumb + ',1]' + '" , "' +;
                       'laBrFldFlt[' + lcRowNumb + ',2]' + '" , "' +;
                       'laBrFldFlt[' + lcRowNumb + ',3]' + '" , "' +;
                       ALLTRIM(cClrRngSel) + '" , "' +;
                       ALLTRIM(cSetFunc) + '" , "' +;
                       ALLTRIM(cSetParm) + '" , "' +;
                       IIF(!EMPTY(cBrwSelFld) , ALLTRIM(cBrwSelFld) ,;
                           ALLTRIM(SUBSTR(mFld_Name , AT('.' ,;
                           mFld_Name) + 1))) + '" , "' +;
                       lcTmpCursr + '" , ' +;
                       IIF(EMPTY(cIndxSrch) , '"Y"' , cIndxSrch) + ' , "' +;
                       IIF(EMPTY(cPict_Str) , '' , EVALUATE(cPict_Str)) +;
                       '" , "' +;
                       IIF(ALIAS() = 'SYREPUVR' .AND. cExpType = 'E' ,;
                           '' , 'laBrFldFlt[' + lcRowNumb + ',4]') + '")'
          
          *B602931,4 Change this line to pass a new parameter [End]
          
          *E300799,4 Change this line to pass 2 more parameters [End]
          
          lcOGPrmpt   = 'In List'
          lcPict      = "@*HN " + lcOGPrmpt
          lcOGVarName = 'INV'
          lcValid     = 'VALID ' + lcOGVld
          lnColSize   = 25          
        ENDIF    && End of IF ALIAS() = 'SYDFIELD' ..... .AND. EMPTY(cBrwSelFld))
        *E300857,1 Add these lines to add a new screen (In range) [End]
          
      CASE ALLTRIM(&lcOGArrName[lnOGArrPos,5])='Between'
        lcValid = STRTRAN(STRTRAN(lcValid,'Valid ',''),'"',"'")
        
        *E300869,1 Change this line to allow expressions in filter option
        *headers [Begin]
        
        *lcOGVld='lfBetwen(['+IIF(EMPTY(ALLTRIM(cFld_Head)),&lcOGFieldNam,ALLTRIM(cFld_Head))+;
        *        '],'+'['+&lcOGArrName[lnOGArrPos,3]+'],'+;
        *         '['+lfOGGetPct(cData_Typ)+'],'+ALLTRIM(STR(lfGetSize(25)))+;
        *         ',"'+lcValid+'",'+'['+lcOGArrName+'('+ALLTRIM(STR(lnOGArrPos))+',6)])'
        
        lcOGVld = 'lfBetwen([' + IIF(EMPTY(ALLTRIM(cFld_Head)) ,;
                  &lcOGFieldNam ,;
                  ALLTRIM(IIF(ALIAS() = 'SYREPUVR' .AND.;
                  LEFT(cFld_Head , 1) = '~' ,;
                  EVALUATE(ALLTRIM(SUBSTR(cFld_Head , 2))) , cFld_Head))) +;
                  '],' + '[' + &lcOGArrName[lnOGArrPos,3] + '],' +;
                  '[' + lfOGGetPct(cData_Typ) + '],' +;
                  ALLTRIM(STR(lfGetSize(25))) + ',"' + lcValid + '",' +;
                  '[' + lcOGArrName + '(' + ALLTRIM(STR(lnOGArrPos)) +;
                  ',6)])'
        
        *E300869,1 Change this line to allow expressions in filter [End]
        
          lcOGPrmpt ='Between'        
          lcPict="@*HN "+lcOGPrmpt
          lcOGVarName='INV'
          lcValid='VALID '+lcOGVld
          lnColSize=25          
      OTHERWISE      
        IF ALLTRIM(&lcOGArrName[lnOGArrPos,5])='Contains'                 
          lcValid = ''
        ENDIF
        DO CASE
          CASE &lcOGArrName[lnOGArrPos,3] = 'D'
               &lcOGVarName=IIF(TYPE(lcOGVarName)='C',CTOD(&lcOGVarName),&lcOGVarName)
               lcPict=[{  /  /  }]
               lnColSize=10               
          CASE &lcOGArrName[lnOGArrPos,3] = 'L'
            lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
            lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
            
            *E300900,1 Change this line to add the capability to make valid
            *E300900,1 entries from file or from array [Begin]
            
            *lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
            *          +lcOGVarName+'",'+'"L'+'"'+','+'"E'+'"'+','+'"';
            *          +IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.');
            *          +'"'+','+;
            *          lcRowPos+','+lcColPos+;
            *          ','+lcRowPos+;
            *          ','+lcColPos+')'
            
            *E300900,1 This variable is to hold the validation of the object
            *E300900,1 in string to be used when the object is created
            lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' , 'lfVldEntPop("');
                        + lcOGVarName + '",' + '"L' + '"' + ',' + '"';
                        + IIF(ALIAS() <> 'SYREPUVR' .OR. EMPTY(cVldEntTyp);
                              , 'E' , cVldEntTyp);
                        + '"' + ',' + '"';
                        + IIF(!EMPTY(ALLTRIM(mVEntries)) ,;
                              ALLTRIM(mVEntries) , 'Yes' + lcElmSep + 'No';
                              + lcValSep + '.T.' + lcElmSep + '.F.');
                        + '"' + ',' + lcRowPos + ',' + lcColPos + ',';
                        + lcRowPos + ',' + lcColPos + ')'
            
            *E300900,1 Change this line to add the capability [End]
            
           lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
           lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')           
           lcValid='VALID '+lcOGValid           
   IF !_WINDOWS        
       lcDefa=PADR(lfGetDef(lcOGVarName,'E',;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )),24)+''
           laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[','E',']+;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 
           lcPict="@*IVN "
           lnColSize=25
      ELSE     
            IF !EMPTY(INVB[1])
               DIMENSION INVB[ALEN(INVB,1)+1]
             ENDIF
             
             *E300900,1 Change this line to create array popup instead
             *E300900,1 of list popup [Begin]
             
             *lcPict="@^ " + lfGetPopPrm(lcOGVarName,'E',;
             * IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.'),.F.,'L' )
             
             *E300900,1 This variable is to hold the picture of the object
             *E300900,1 in string to be used when the object is created
             lcPict = "@^ "
             
             *E300900,1 Change this line to create array popup [End]
             
             *E300900,1 Change this line to add the capability to make
             *E300900,1 valid entries from file or from array [Begin]
             
             *laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[','E',']+;
             *                  IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
             *                  )+[')]
             laOGObjType[lnCount,3] = [lfGetDef('] + lcOGVarName + [','] +;
                                      IIF(ALIAS() <> 'SYREPUVR' ;
                                          .OR. EMPTY(cVldEntTyp) , "E" ,;
                                          cVldEntTyp);
                                      + [','] +;
                                      IIF(!EMPTY(ALLTRIM(mVEntries)) ,;
                                          ALLTRIM(mVEntries) , 'Yes' +;
                                          lcElmSep + 'No' + lcValSep +;
                                          '.T.' + lcElmSep + '.F.');
                                      + [')]
             
             *E300900,1 Change this line to add the capability [End]
             
             *E300900,1 Add this line to create array popup instead
             *E300900,1 of list popup
             *E300900,1 Note :
             *E300900,1        I moved the calling of lfGetPopPrm from
             *E300900,1        lcPict to lcValid because of the changes
             *E300900,1        that I made in the return value of the
             *E300900,1        function to create array popup instead
             *E300900,1        of list popup, I also changed the calling
             *E300900,1        of lfGetPopPrm to add the capability to
             *E300900,1        make valid entries from file and from
             *E300900,1        array
             lcValid = lfGetPopPrm(lcOGVarName ,;
                       IIF(ALIAS() <> 'SYREPUVR' .OR. EMPTY(cVldEntTyp) ,;
                           'E' , cVldEntTyp) ,;
                       IIF(!EMPTY(ALLTRIM(mVEntries)) , ALLTRIM(mVEntries) ;
                           , 'Yes' + lcElmSep + 'No' + lcValSep + '.T.' +;
                           lcElmSep + '.F.');
                       , .F. , 'L');
                       + ' ' + lcValid
             
             *E300900,1 Add this line to create array popup instead
             
             lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'
             lnColSize=25
             lcDefa=lnOgBarNo
             laOGObjType[lnCount,2]=lcOGVarName              
             STORE lnOgBarNo TO INVB[ALEN(INVB,1)]                         
        ENDIF   
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
          CASE &lcOGArrName[lnOGArrPos,3] = 'N'
            lcPict=lfOGGetPct(cData_Typ)
            lnColSize = lfGetSize(25)
              lnOGNumObj=1               
          CASE &lcOGArrName[lnOGArrPos,3] = 'C'  
            DO CASE
              CASE lVldEntry OR !EMPTY(mVEntries)
                
                *E300900,1 Change this line to add the capability to make
                *E300900,1 valid entries from file or from array [Begin]
                
                *lcVEntries='All'+lcElmSep+SUBSTR(MVENTRIES,1,ATC(lcValSep,MVENTRIES))+;
                *           ''+lcElmSep+SUBSTR(MVENTRIES,ATC(lcValSep,MVENTRIES)+1)
                lcVEntries = IIF(ALIAS() <> 'SYREPUVR' .OR. ;
                                 EMPTY(cVldEntTyp) .OR. cVldEntTyp = 'E' ,;
                                 'All' + lcElmSep + SUBSTR(mVEntries , 1 ,;
                                 ATC(lcValSep , mVEntries)) + '' + lcElmSep;
                                 + SUBSTR(mVEntries , ATC(lcValSep ,;
                                 mVEntries) + 1) , ALLTRIM(mVEntries))
                
                *E300900,1 Change this line to add the capability [End]
                
                lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
                lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
         lcCodes_Fld=IIF(ALIAS()='SYDFIELD',SUBSTR(&lcOGArrName[lnOGArrPos,1],ATC('.',&lcOGArrName[lnOGArrPos,1])+1),cCodes_Fld)
                
                *E300900,1 Change this line to add the capability to make
                *E300900,1 valid entries from file or from array [Begin]
                
                *lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
                *           +lcOGVarName+'"'+',"C'+'"'+','+'"'+IIF(lVldEntry,'C','E')+'"'+','+'"';
                *           +IIF(lVldEntry,lcCodes_Fld,ALLTRIM(lcvEntries))+'"'+','+;
                *           lcRowPos+','+lcColPos+;
                *           ','+lcRowPos+;
                *           ','+lcColPos+')'
                
                *E300900,1 This variable is to hold the validation of the
                *E300900,1 object in string to be used when the object is
                *E300900,1 created
                lcOGValid = IIF(_WINDOWS , 'lfVldWinPop("' ,;
                                'lfVldEntPop("');
                            + lcOGVarName + '"' + ',"C' + '"' + ',' + '"';
                            + IIF(lVldEntry , 'C' ,;
                                 IIF(ALIAS() <> 'SYREPUVR' .OR.;
                                     EMPTY(cVldEntTyp) , 'E' , cVldEntTyp));
                            + '"' + ',' + '"';
                            + IIF(lVldEntry .OR. (ALIAS() = 'SYREPUVR' ;
                                                  .AND. cVldEntTyp = 'C') ,;
                                  lcCodes_Fld , ALLTRIM(lcvEntries));
                            + '"' + ',' + lcRowPos + ',' + lcColPos + ',';
                            + lcRowPos + ',' + lcColPos + ')'
                
                *E300900,1 Change this line to add the capability [End]
                
                 lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
                 lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')                 
                 lcValid='VALID '+lcOGValid                 
         IF !_WINDOWS        
             lcDefa=PADR(lfGetDef(lcOGVarName,IIF(lVldEntry,'C','E'),;
                                      IIF(lVldEntry,lcCodes_Fld,ALLTRIM(lcvEntries))),24)+IIF(_DOS,'','')
             laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(lcvEntries))+['),24)+'']
             lcOGVarName='INV'
             laOGObjType[lnCount,2]=lcOGVarName 
                                      
                 lcPict="@*IVN "
                 lnColSize=25
         ELSE
                 IF !EMPTY(INVB[1])
                   DIMENSION INVB[ALEN(INVB,1)+1]
                 ENDIF
             
             *E300900,1 Change this line to create array popup instead
             *E300900,1 of list popup [Begin]
             
             *lcPict="@^ " + lfGetPopPrm(lcOGVarName,IIF(lVldEntry,'C','E'),;
             * IIF(lVldEntry,lcCodes_Fld,ALLTRIM(lcvEntries)),.F.,'C')
             
             *E300900,1 This variable is to hold the picture of the object
             *E300900,1 in string to be used when the object is created
             lcPict = "@^ "
             
             *E300900,1 Change this line to create array popup [End]
             
             *E300900,1 Change this line to add the capability to make
             *E300900,1 valid entries from file or from array [Begin]
             
             *laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
             *                     IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+[')]              
             laOGObjType[lnCount,3] = [lfGetDef('] + lcOGVarName + [','] +;
                                      IIF(lVldEntry , 'C' ,;
                                          IIF(ALIAS() <> 'SYREPUVR' .OR.;
                                              EMPTY(cVldEntTyp) , 'E' ,;
                                              cVldEntTyp));
                                      + [','] +;
                                      IIF(lVldEntry .OR.;
                                          (ALIAS() = 'SYREPUVR' .AND.;
                                           cVldEntTyp = 'C') ,;
                                          lcCodes_Fld , ALLTRIM(mVEntries));
                                      + [')]
             
             *E300900,1 Change this line to add the capability [End]
             
             *E300900,1 Add this line to create array popup instead
             *E300900,1 of list popup
             *E300900,1 Note :
             *E300900,1        I moved the calling of lfGetPopPrm from
             *E300900,1        lcPict to lcValid because of the changes
             *E300900,1        that I made in the return value of the
             *E300900,1        function to create array popup instead
             *E300900,1        of list popup, I also changed the calling
             *E300900,1        of lfGetPopPrm to add the capability to
             *E300900,1        make valid entries from file and from
             *E300900,1        array
             lcValid = lfGetPopPrm(lcOGVarName ,;
                       IIF(lVldEntry , 'C' , IIF(ALIAS() <> 'SYREPUVR' ;
                           .OR. EMPTY(cVldEntTyp) , 'E' ,;
                           cVldEntTyp)) ,;
                       IIF(lVldEntry .OR. (ALIAS() = 'SYREPUVR' ;
                                           .AND. cVldEntTyp = 'C') ,;
                           lcCodes_Fld , ALLTRIM(lcvEntries));
                       , .F. , 'C');
                       + ' ' + lcValid
             
             *E300900,1 Add this line to create array popup [End]
             
             lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'
             lnColSize=25
             lcDefa=lnOgBarNo
             laOGObjType[lnCount,2]=lcOGVarName              
             STORE lnOgBarNo TO INVB[ALEN(INVB,1)]                                                           
           ENDIF      
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
               CASE !EMPTY(MVAld_Str)
                 lcPict=lfOGGetPct(&lcOGArrName[lnOGArrPos,3])
                 IF EMPTY(lcValid)
                   lcValid = IIF(EMPTY(SET('PROC')),'','VALID '+ALLTRIM(mVald_Str))
                   lcValid=STRTRAN(lcValid,'{{HPOS}}',ALLTRIM(STR(lnOGObjPos)))  
                   lcWhen=IIF(ATC('WHEN ',lcValid)>0,SUBSTR(lcValid,ATC('WHEN ',lcValid)),'')
                   lcValid=STRTRAN(lcValid,lcWhen,'')
                   lcWhen=IIF(!EMPTY(lcWhen),'WHEN '+'lfWhen() AND '+STRTRAN(UPPER(lcWhen),'WHEN '),'WHEN lfWhen()')                  
                 ENDIF  
                 lnColSize=lfGetSize(25)
                 lnOGNumObj=1                 
               OTHERWISE
                 lcPict=lfOGGetPct(&lcOGArrName[lnOGArrPos,3])
                 lnColSize=lfGetSize(25)
                 lnOGNumObj=1                 
             ENDCASE
          CASE &lcOGArrName[lnOGArrPos,3]='M'
                 lcPict=lfOGGetPct(&lcOGArrName[lnOGArrPos,3])
                 lnColSize=lfGetSize(25)
        ENDCASE 
      ENDCASE
  CASE  &lcOGArrName[lnOGArrPos,7] $ 'F'
    =lfGetRepFld(&lcOGArrName[lnOGArrPos,6])
      lcPict="@*IVN "
      lnColSize=lfGetSize(25)
      lcOGVarName='INV'
      lcWhen='WHEN lfWhen() AND .F.'
      lcValid=''      
  CASE  &lcOGArrName[lnOGArrPos,7] $ 'E'
    IF lcRunDirct = 'R'
      =lfGetRepFld(&lcOGArrName[lnOGArrPos,6])
      lcPict="@*IVN "      
      lcOGVarName='INV'           
      lcWhen='WHEN lfWhen() AND .F.'
      lcValid=''             
*    ELSE
*       &lcOGVarName = EVAL(&lcOGArrName[lnOGArrPos,6])
*       &lcOGArrName[lnOGArrPos,7] = 'V'
    ENDIF  
    lcOGExpToSay=&lcOGFieldNam
    lnColSize=lfGetSize(25)
  OTHERWISE
      lcPict=lfOGGetPct(&lcOGArrName[lnOGArrPos,3])
      lnColSize=lfGetSize(25)
      lcOGVarName='INV'
      lcWhen='WHEN lfWhen() AND .F.'
      lcValid=''      
ENDCASE    

RETURN lnOGNumObj




*!*************************************************************************
*!
*!              Function: lfGetPic
*!
*!*************************************************************************
*  this function to return the picture of a field using the currunt record
*  fields of the cursor file
FUNCTION lfOGGetPct
PARAMETERS cLeftType
IF EMPTY(cPict_str)
  DO CASE 
    CASE  cLeftType='C' 
*      RETURN '@! '+REPLICATE('X',nFld_Wdth)
*        wait MFLD_NAME+' '+str(nfld_wdth) WINDOW
       RETURN REPLICATE('X',nFld_Wdth)
    CASE  cLeftType='N' 
    *B603065,1 Hesham (Start)
      *RETURN REPLICATE('9',NFld_WDTH-NFld_DEC)+IIF(NFld_DEC>0,'.','');
             +IIF(NFld_DEC>0,REPLICATE('9',NFld_DEC),'')
      IF NFld_DEC>0
        RETURN REPLICATE('9',NFld_WDTH-NFld_DEC-1)+'.'+REPLICATE('9',NFld_DEC)
      ELSE
        RETURN REPLICATE('9',NFld_WDTH)
      ENDIF
    *B603065,1 Hesham (End)      
    CASE  cLeftType='M'
*      RETURN '@! '+ REPLICATE('X',165)
       RETURN REPLICATE('X',165)
  ENDCASE
ENDIF
llATSign= ATC('@',CPICT_STR)>0
llExclSign = ATC('!',CPICT_STR)>0
lcOGFunc=STRTRAN(CPICT_STR,'@')
lcOGFunc=STRTRAN(lcOGFunc,'!')
IF !EMPTY(CPICT_STR)
  lcOGFunc=EVAL(ALLTRIM(CPICT_STR))
ENDIF  

lcOGFunc=IIF(TYPE('lcOGFunc')='C',lcOGFunc,CPICT_STR)
*IIF(cLeftType $ 'CM',IIF(llAtSign,'@','')+;
                         IIF(llExclSign AND !llAtSign,'@! ',IIF(llExclSign,'@! ',''));
                          +ALLTRIM(lcOGFunc),'')
*lcPict=IIF(!INLIST(ALLTRIM(&lcOGArrName[lnOGArrPos,5]),'In List','Between'),EVAL(CPICT_STR),ALLTRIM(CPICT_STR))

lcPict=IIF(TYPE(CPICT_STR)<>'C',lcOGFunc,EVAL(CPICT_STR))
RETURN lcPict


*!*************************************************************************
*!
*!              Function: lfGetSize
*!
*!*************************************************************************
*  this function to return the size of a get field using the current record
* of the cursor file information
FUNCTION lfGetSize
PARAMETERS lnSize
IF EMPTY(NFld_WDTH)
  RETURN lnSize
ENDIF
*B800919,4 Use fixed size font for the inlist object
*RETURN MIN(IIF(_DOS OR _UNIX,nFld_Wdth,nFld_Wdth*1.33),lnSize)
RETURN MIN(IIF(_DOS OR _UNIX,nFld_Wdth,nFld_Wdth),lnSize)
*B800919,4 end


*!********************************************************************
*!
*!              Function: lfVldEntPop
*!
*!********************************************************************
*
FUNCTION lfVldEntPop
PARAMETERS lcOgVarName,lcOGDataTyp,lcOGPopType,lcOGValidEnt,lnOGRow,lnOGCol,lnOGSR,lnOGSC,lcOGEmpVl
lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lnOGSR=IIF(TYPE('lnOGSR')<>'N',lnOGRow,lnOGSR)
lnOGSC=IIF(TYPE('lnOGSC')<>'N',lnOGCol,lnOGSC)
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]
    *E300789,1 Hesham (Start)
    *SELECT cDiscrep FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal  
      
    *--MAN  
    PRIVATE llFONow  
    llFONow  = .F.
    IF !USED("CODES")
      llFONow  = .T.
    ENDIF
    SELECT cDiscrep FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CDEFCODE='N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CDEFCODE='N';
      INTO ARRAY laOGVldVal  
    IF llFONow  
      USE IN CODES
    ENDIF  
    *E300789,1 Hesham (End)     
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]      
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1,1]=lcOGEmpVl
      laOGVldVal[1,1]=lcOGEmpHD
    ENDIF  
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]      
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1]=lcOGEmpVl
      laOGVldVal[1]=lcOGEmpHD    
    ENDIF  
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount= 1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=lfTrnsStr(laOGVldVal[lnOGCount,1],lcOGDataTyp)
        ENDFOR
    ENDIF
ENDCASE
IF ALEN(laOGVldEnt,1)>1
*   lcWin=IIF(EMPTY(WPARENT()),WONTOP(),WPARENT())

* lnWlrow=WLROW(WPARENT(lcWinName))+IIF(WLROW(lcWinName)<0,WLROW(lcWinName),-WLROW(lcWinName))    
  lnWlrow=IIF(EMPTY(WPARENT(WPARENT())),1,-WLROW(WPARENT())+WLROW()+5)
  lnWlRow=IIF(lnogRow+lnWlRow+4>WROWS('GWDGRID'),lnWlRow-4,lnWlRow)
   DEFINE POPUP puOGTarget FROM lnOGRow+lnWlRow,lnOGCol TO lnOGRow+lnWlRow+IIF(_DOS,4,7),lnOGCol+30 SCROLL SHADOW IN WINDOW gwdGrid
*   IN WINDOW (lcOGWinowN)
   =lfFillPop('PUOGTARGET','laOGVldEnt')
   lnOGBarNo=0
   lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
   ON SELECTION POPUP PUOGTARGET DO lfVldEntVal with 'PUOGTARGET',PROMPT(),Bar(),lcOGVarName,lnOGRow,lnOGCol
   ACTIVATE POPUP PUOGTARGET  BAR IIF(lnOGBarNo>0,lnOGBarNo,1)   
   DEACTIVATE POPUP PUOGTARGET
   RELEASE POPUP PUOGTARGET
   IF !EMPTY(PROMPT())
     IF _DOS
       @ lnOGSR,lnOGSC SAY PADR(SUBSTR(PROMPT(),1,24),24)+IIF(_DOS,'','');
       COLOR (SCHEME(5,4))  
       SHOW OBJECT _CUROBJ      
     ELSE
       @ lnOGSR,lnOGSC SAY PADR(SUBSTR(PROMPT(),1,24),24);
       FONT "MS Sans Serif", 8;
       STYLE 'B';
       SIZE 1.144,20,0      
       SHOW OBJECT _CUROBJ
     ENDIF  
   ENDIF  
ENDIF
*IF lcOGOldVal=&lcOGVarName
*  RETURN .F.
*ENDIF

*!********************************************************************
*!
*!              Function: lfVldEntVal
*!
*!********************************************************************
*
FUNCTION lfVldEntVal
PARAMETERS lcOGPopName,lcOGSelection,lnOGBarNo,lcOGVarName,lnOGRow,lnOGCol
&lcOGVarName=laOGVldVal[lnOGBarNo]
DEACTIVATE POPUP PUOGTARGET
RELEASE POPUP PUOGTARGET




*!*************************************************************************
*!
*!              Function: lfFillPop
*!
*!*************************************************************************
*
FUNCTION lfFillPop
PARAMETERS lcPopName,lcArrName
RELEASE BAR ALL OF (lcpopname)

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

FOR lnCount=1 to ALEN(&lcArrName,1)
  IF !EMPTY(&lcArrName[lnCount,1])
    DEFINE BAR lnCount OF (lcPopName) PROMPT &lcArrName[lnCount,1]
  ENDIF
ENDFOR













*!*************************************************************************
*!
*!              Function: lfGetDef
*!
*!*************************************************************************
*
FUNCTION lfGetDef
PARAMETERS lcOGVarName,lcOGPopType,lcOGValEnt,lcOGEmpVl

lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(TYPE(lcOGVarName)<>'C',lfTrnsStr(lcOGEmpHD,TYPE(lcOGVarName)),lcOGEmpHD)
DO CASE
  CASE lcOGPopType='C'
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValEnt),10)
    DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]
    
    *B801961,1 Change these lines to display the code + code description
    *B801961,1 in the case of editable codes and to sort the displayed
    *B801961,1 values (by code if the code is editable otherwise by code
    *B801961,1 description) [Begin]
    *SELECT cDiscrep FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *  INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *  INTO ARRAY laOGVldVal    
    
    PRIVATE llIsEdtble
    llIsEdtble = gfIsEdtble(lcOGFieldName)    && Check if the code is editable
    
    *B801961,1 If the code is editable
    IF llIsEdtble
    *E300789,1 Hesham (Start)
      *SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    

      SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CDEFCODE='N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
      
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CDEFCODE='N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    ELSE    && Else [If the code is not editable]
    *E300789,1 Hesham (Start)
      *SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    

      SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt
      
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    ENDIF    && End of IF llIsEdtble
    *B801961,1 Change these lines to display the code + code description [End]
    
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]      
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1,1]=lcOGEmpVl
      laOGVldVal[1,1]=lcOGEmpHD  
    ENDIF  
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValEnt,@laOGVldFl)
    
    *E300900,1 Change this line to add the capability to open the file
    *E300900,1 from particular path
    
    *lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGFlPath = IIF('\' $ laOGVldFl[1] , '' ,;
                     IIF(LEFT(laOGVldFl[1] , 2) = 'SY' , gcSysHome ,;
                         gcDataDir))
    
    *E300900,1 Change this line to add the capability to open the file [End]
    
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)],laOGVldVal[ALEN(laOGVldVal)]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1],laOGVldVal[ALEN(laOGVldVal)+1]          
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1]=lcOGEmpVl
      laOGVldVal[1]=lcOGEmpHD    
    ENDIF  
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValEnt,1,ATC(lcValSep,lcOGValEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValEnt,ATC(lcValSep,lcOGValEnt)+1),@laOGVldVal,lcElmSep)
 *       DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]             
       lcDataTyp=TYPE(lcOGVarName)
 *      IF lcDataTyp $ 'CN'
 *        =AINS(laOgVldEnt,1)
 *        =AINS(laOgVldVal,1)  
 *        laOGVldEnt[1]=lcOGEmpVl
 *        laOGVldVal[1]=lcOGEmpHD          
 *      ENDIF  
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount=1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=IIF(TYPE('laOGVldVal[lnOGCount,1]')='C',lfTrnsStr(laOGVldVal[lnOGCount,1],lcDataTyp),;
                                       laOGVldVal[lnOGCount,1])
        ENDFOR
      ENDIF
  
  *E300900,1 Add these lines to add the capability to make valid entries
  *E300900,1 from array [Begin]
  
  *E300900,1 Case of valid entries from array
  CASE lcOGPopType = 'A'

    *E300900,1 If there is valid entries
    IF !EMPTY(lcOGValEnt)
      *-- Get the 1st array name
      lcArrayNam = SUBSTR(lcOGValEnt , 1 , ATC(lcValSep , lcOGValEnt)-1)
      DIMENSION laOGVldEnt[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldEnt)
      
      *-- Get the 2nd array name
      lcArrayNam = SUBSTR(lcOGValEnt , ATC(lcValSep,lcOGValEnt) + 1)
      DIMENSION laOGVldVal[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldVal)
    ENDIF    && End of IF !EMPTY(lcOGValEnt)
  *E300900,1 Add these lines to add the capability [End]
  
ENDCASE

lnOGBarNo=0 
lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
lcOGToRet=IIF(lnOGBarNo>0,laOGVldEnt[lnOGBarNo],'')
RETURN IIF(_DOS,lcOGToRet,lnOGBarNo)

*!********************************************************************
*!
*!              Function: lfChangeMsg
*!
*!********************************************************************
*
FUNCTION lfChangeMsg
PARAMETERS lcOGMsg1,lcOGMsg2,llOGMsg,lnOGObjPos
@ laOGObjPos[lnOGObjPos+1,1],laOGObjPos[lnOGObjPos+1,2] SAY IIF(llOGMsg,lcOGMsg1,lcOGMsg2) ;
  SIZE 1,20
RETURN  
*!********************************************************************
*!
*!              Function: lfChangeGrid
*!
*!********************************************************************
*
FUNCTION lfChangeGrid
PARAMETERS lcOGRep_ID,lnOGFltID

*E301178,1 03/18/1999 if this function called from < Reset > button, [Begin]
*E301178,1 03/18/1999 rise flag to prevent close files then open it.
PRIVATE llMustClos
llMustClos = !(ALLTRIM(lcOGRep_ID)==ALLTRIM(SydRepRt.cRep_Id))
*E301178,1 03/18/1999 if this function called from < Reset > button, [End  ]

CLEAR READ

*E301178,1 03/18/1999 if this function called from any program rather than < Reset > button, [Begin]
IF llMustClos
  *B601761,1 Hesham (Start)
  =lfClosRpFls()
  *B601761,1 Hesham (End)
ENDIF
*E301178,1 03/18/1999 if this function called from any program rather than < Reset > button, [End  ]

*B601761,1 Hesham (Start)
IF !EMPTY(SET('PROC'))
  =lfClearRep()
ENDIF  
*B601761,1 Hesham (End)

lcOGFltr=''
DIMENSION laOGHdFlt[1,7],laOGFxFlt[1,7],laOGVrFlt[1,7],INVB[1]
STORE '' TO laOGHdFlt,laOGFxFlt,laOGVrFlt,INVB

IF !USED('SYDREPRT')
  llRepFlUse = .T.
  SELECT 0
  USE &gcSysHome.SYDREPRT 
ELSE
  SELECT SYDREPRT 
ENDIF

SET ORDER TO TAG CREP_ID

IF !USED('SYURPFLT')
  SELECT 0
  USE &gcSysHome.SYURPFLT 
ELSE
  SELECT SYURPFLT 
ENDIF
SET ORDER TO TAG CREP_ID

SELECT SYURPFLT
DIMENSION laOGEntSet[1,2]
STORE '' TO laOGEntSet
IF TYPE('lnOGFltID')='N'  AND lnOGFltID<>0
      GO lnOGFltID
      lcOGFlt_ID  = cfltr_ID
      lcOGSeting=RECNO() &&cfltr_ID
      llOGSysData=lSys_Data
      lcOGSetMsg=cFltr_des
      lcOGLastSet=lcOGSeting      
      llOGFilter = lSelect      
      *--MAN
      IF llMustClos 
        =lfOpenRpFls()
      ENDIF  

      =lfLoadUDefV()
      lcOGRep_ID = lcOGRepID
  ELSE
    SELECT SYDREPRT
    lcOGFlt_ID  = ''
    lcOGSeting=0
    lcOGSetMsg='Default'
    llOGSysData=.T.
    lcOGLastSet=lcOGSeting    
    SEEK lcOGRep_ID
    llOGFilter = !lSelect
    lcOGWinTitl = " "+ALLTRIM(cRep_Name)+" "
    lcOGOrder   = ALLTRIM(CORDERVAR)
    lcOGOrderV  = ALLTRIM(CORDERVLD)    
    lcOGFormV   = ALLTRIM(CFORMVAR)    
    lcOGReadW=ALLTRIM(cReadWhen)
    lcOGreadV=ALLTRIM(cReadVald)      
    lcOGReadS=ALLTRIM(cReadShow)
    llOGVrFlt=lRepVrFlt  
    IF !EMPTY(mRepAvFld)
      RESTORE FROM MEMO mRepAvFld ADDI
    ENDIF
    IF !EMPTY(ALLTRIM(MREP_FILS))
      RESTORE FROM MEMO MREP_FILS ADDI
      *--MAN
      =lfOpenRpFls()
    ENDIF       
     *B800551,1 Hesham (Start)    
     *B800551,1 load default report variables for the master report
     *B800551,1 of the runing one instead of the running report
     *B800551,1 so if there is any master report for the running
     *B800551,1 one the main variables can be defined like in case
     *B800551,1 of income statment and balance sheet reports
    IF TYPE('lnOGFltID')<>'N'
      =lfLoadRepV(lcOGRep_ID)
    ELSE  
      =lfLoadRepV(lcOGPrgName)        
    ENDIF  
    *B800551,1 Hesham (End)
    SELECT SYDREPRT
ENDIF

IF !EMPTY(ALLTRIM(MREPHDFLT))
   RESTORE FROM MEMO MREPHDFLT ADDI
ENDIF  

IF !EMPTY(ALLTRIM(MREPFXFLT))  
  RESTORE FROM MEMO MREPFXFLT ADDI
  =lfInitValue('laOGFxFlt','C')      
ENDIF
IF !EMPTY(ALLTRIM(MREPVRFLT)) 
  RESTORE FROM MEMO MREPVRFLT ADDI
  =lfInitValue('laOGVrFlt','C')      
ENDIF  

lnogSeting = ASCAN(lAOGSeting,lcOGSeting)
lnogSeting = IIF(lnogSeting<>0,ASUBSCRIPT(laOGSeting,lnogSeting,1),1)
  
lnOGFixCnt=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGVrFCnt=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
IF !EMPTY(lcOGFormV)
  IF (EMPTY(&lcOGFormArr[1,1]) AND !EMPTY(lcOGFormV)) OR !(lcOGLastForm==&lcOGFormV)
    
    *E300900,1 Remove this line for we have canceled the Option that
    *E300900,1 change the report objects style [Begin]
    *=gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
    *E300900,1 Remove this line for we have canceled the Option [End]
    
    lcOGLastForm=&lcOGFormV
    llOGStyleCh=.T.
  ELSE
    llOGStyleCh=.T.
  ENDIF
ENDIF  
IF !llOGVrFlt
  lnOGVrFCnt=0
ENDIF
  
*E300900,1 Add this line to get the available report modes [Begin]
=lfRepPltFr(IIF(EMPTY(lcOGFormV) , '' , EVALUATE(lcOGFormV)))
*E300900,1 Add this line to get the available report modes [End]

IF !USED('SYREPUVR')
  SELECT 0
  USE &gcSysHome.SYREPUVR 
ELSE
  SELECT SYREPUVR
ENDIF
SET ORDER TO TAG CREPVAR
*GO TOP
lnOGVarCnt=0
lcOgTmExact=SET('EXACT')
SET EXACT OFF

=SEEK(PADR(lcOGRep_ID,8)+'V')
COUNT REST WHILE cRep_ID+CEXPTYPE+STR(nVarPos)=PADR(lcOGRep_ID,8)+'V' FOR lAskRunT AND lDispOG  TO lnOGVariable 

SET EXACT &lcOGTmExact
lnOGFixFl=lnOGFixCnt
lnOGVarFl=lnOGVrFCnt
lnObjNum=1
lnObjMax=7
MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
*C101459,1 Hesham (Start)
*C101459,1 if option grid called to control user define fields
*C101459,1 then initialize the arrays from the program user defined
*C101459,1 fields array
IF (lcRunDirct $ 'TX' AND EMPTY(laOGFxFlt)) OR (lcRunDirct $ 'TX' AND ALEN(laOGFxFlt) <> ALEN(laUsrFields))
   DIMENSION laOGHDFlt[1,7],laOGVRFlt[1,7] 
   STORE '' TO laOGHDFlt,laOGVRFlt 
   DIMENSION laOGFxFlt[ALEN(laUsrFields,1),ALEN(laUsrFields,2)]    
   =ACOPY(laUsrFields,laOGFxFlt)
   IF lcRunDirct='T'  AND (TYPE('lnOGFltID')#'N' OR lnOGFltID=0)
     FOR lnUsr = 1 TO ALEN(laOGFxFlt,1)
       laOGFxFlt[lnUsr,6]= ''
     ENDFOR
     =lfInitValue('laOGFXFlt','C')      
   ENDIF
   llOGVrFlt = .F.
   llOGFilter = .T.
   lcOGPrvRun = 'gfVldUsrFld(@laOgFxFlt)'
ENDIF
*C101459,1 Hesham (End)

=lfModiHScrol()

*E301178,1 03/18/1999 if this function called from any program rather than < Reset > button, [Begin]
IF llMustClos
  *B601761,1 Hesham (Start)
  *--MAN
  *=lfOpenRpFls()
  *B601761,1 Hesham (End)
ENDIF  
*E301178,1 03/18/1999 if this function called from any program rather than < Reset > button, [End  ]

lcOGRepID=lcOGRep_ID


RETURN 


*!********************************************************************
*!
*!              Function: lfGetRepFld
*!
*!********************************************************************
*
FUNCTION lfGetRepFld
PARAMETERS lcOGFldToGet

IF !USED('SYREPUVR')
  SELECT 0
  USE &gcSysHome.SYREPUVR 
ENDIF
SELECT SYREPUVR
SET ORDER TO TAG CREPVAR
IF SEEK(PADR(lcOGRepID,8))
  LOCATE REST WHILE cRep_ID+CEXPTYPE+STR(nVarPos)=PADR(lcOGRepID,8) FOR ALLT(mFld_Name)=ALLT(lcOGFldToGet)
ENDIF  
IF !FOUND()  &&SEEK(lcOGRepID+lcOGFldToGet)
  lcOGFldToGet=IIF(ATC('.',lcOGFldToGet)>0,SUBSTR(lcOGFldToGet,;
                  ATC('.',lcOGFldToGet)+1),lcOGFldToGet)
  IF !USED('SYDFIELD')
    SELECT 0
    USE &gcSysHome.sydfield 
  ENDIF
  SELECT sydfield  
  SET ORDER TO TAG CFLD_NAME
  SEEK lcOGFldToGet  
ENDIF

RETURN


************************************************
*!********************************************************************
*!
*!              Function: lfVAdvanced
*!
*!********************************************************************
*
FUNCTION lfvAdvanced
lcOGSelect=SELECT()
lcOGTempProc=SET('PROCEDURE')
SET PROCEDURE TO
=lfInitValue('laOGVrFlt','T')

*E300900,1 Add these lines to add the suppress expression capability [Begin]
DIMENSION laSaveFldH[ALEN(laOGFieldH , 1) , ALEN(laOGFieldH , 2)] ,;
          laSaveFldN[ALEN(laOGFieldN , 1) , ALEN(laOGFieldN , 2)]

=ACOPY(laOGFieldN , laSaveFldN)
*E300900,1 Add these lines to add the suppress expression capability [End]

*E300869,1 Add this line to allow expressions in filter option
*headers [Begin]

=ACOPY(laOGFieldH , laSaveFldH)

*E300900,1 Add these lines to add the suppress expression capability [Begin]

*E300900,1 If the report id have changed
IF lcOldRepId <> lcOGRepID
  lcOldRepId = lcOGRepID
  =lfGetSupEx(lcOGRepID)
ENDIF    && End of IF lcOldRepId <> lcOGRepID

*E300900,1 If there is filter options that will be suppressed
IF ALEN(laSupExp , 1) > 1
  =lfMovSupEx(@laOGFieldN , @laOGFieldH)
ENDIF    && End of IF ALEN(laSupExp , 1) > 1

*E300900,1 Add these lines to add the suppress expression capability [End]

*E300869,1 For loop to scan the array that hold the allow for filter headers
FOR lnRowNumb = 1 TO ALEN(laOGFieldH , 1)
  
  *E300869,1 If this header is an expression
  IF LEFT(laOGFieldH[lnRowNumb , 1] , 1) = '~'
    laOGFieldH[lnRowNumb , 1] = EVALUATE(ALLTRIM(SUBSTR(laOGFieldH[lnRowNumb , 1] , 2)))
  ENDIF    && End of IF LEFT(laOGFieldH[lnRowNumb , 1] , 1) = '~'
ENDFOR    && End of FOR lnRowNumb = 1 TO ALEN(laOGFieldH , 1)
*E300869,1 Add this line to allow expressions in filter option [End]

=GFBFILTR('laOGVrFlt',@laOGFieldH,@laOGFieldN,'O',lcOGRepID,'SYREPUVR')

*E300900,1 Add these lines to add the suppress expression capability [Begin]
DIMENSION laOGFieldH[ALEN(laSaveFldH , 1) , ALEN(laSaveFldH , 2)] ,;
          laOGFieldN[ALEN(laSaveFldN , 1) , ALEN(laSaveFldN , 2)]

=ACOPY(laSaveFldN , laOGFieldN)
*E300900,1 Add these lines to add the suppress expression capability [End]

*E300869,1 Add this line to allow expressions in filter option
*headers [Begin]

=ACOPY(laSaveFldH , laOGFieldH)

*E300869,1 Add this line to allow expressions in filter option [End]

=lfInitValue('laOGVrFlt','C')
*=lfInitArr('laOGVrFlt')
SET PROCEDURE TO &lcOGTempProc
SELECT(lcOGSelect)
CLEAR READ
lnOGFixCnt=lnOGFixFl
lnOGVarcnt=lnOGVariable
lnOGVrFCnt=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
IF !USED('SYREPUVR')
  SELECT 0
  USE &gcSysHome.SYREPUVR
ELSE
 SELECT SYREPUVR
ENDIF
SET ORDER TO TAG CREPVAR

*IF lnOGMaxWin<>CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)
*  IF lnOGMaxWin>CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)
*    FOR lnOGCount=CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)+1  TO lnOGMaxWin
*      RELEASE WINDOW (laOGWinName[lnOGCount])
*    ENDFOR
*    RELEASE WINDOW gwdOGscrl
*    DIMENSION laOGWinName[CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)]  
*  ELSE
*    DIMENSION laOGWinName[CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)]     
*    FOR lnOGCount=lnOGMaxWin+1 TO CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)
*      laOGWinName[lnOGCount]=SYS(2015)
*    ENDFOR    
*  ENDIF
*ENDIF
*lnOGMaxWin=CEILING((lnOGVarcnt+lnOGFixcnt+lnOGVrFCnt)/lnOGMax)
lnOGFixFl=lnOGFixCnt
lnOGVarFl=lnOGVrFCnt
lnOGVariable=lnOGVarcnt
SET EXACT OFF
lnObjNum=1
lnObjMax=7
*lnScrolRow=IIF(_DOS,1,2)
MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
=lfModiHScrol()

************************************************
*!********************************************************************
*!
*!              Function: lfInitValue
*!
*!********************************************************************
*
FUNCTION lfInitValue
PARAMETERS lcOGArrName,lcOGDir
FOR lnOGCount = 1 TO  ALEN(&lcOGArrName,1)
  IF &lcOGArrName[lnOGCount,1]<>'.OR.'
    IF &lcOGArrName[lnOGCount,7] = 'V' AND ;
      !INLIST(&lcOGArrName[lnOGCount,5],'In List','Between')
      DO CASE
        CASE &lcOGArrName[lnOGCount,3]='D'
          IF !EMPTY(&lcOGArrName[lnOGCount,6])
            &lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',CTOD(&lcOGArrName[lnOGCount,6]),;
                                            DTOC(&lcOGArrName[lnOGCount,6]))        
          ENDIF                                  
        CASE &lcOGArrName[lnOGCount,3]='N'
          *B603065,1 Hesham (Start)
          *B603065,1 check for decimal places in the value
          *&lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',VAL(&lcOGArrName[lnOGCount,6])),;
                                           STR(&lcOGArrName[lnOGCount,6]))        
          IF lcOGDir<>'C'
            IF &lcOGArrName[lnOGCount,6] - INT(&lcOGArrName[lnOGCount,6]) > 0
              lcDec = allt(str((&lcOGArrName[lnOGCount,6] - INT(&lcOGArrName[lnOGCount,6])) * 100000))
              lcDec = SUBSTR(lcDec,1,RAT('0',lcDec))
              lnDec = len(lcDec)
              &lcOGArrName[lnOGCount,6] = ALLTRIM(STR(&lcOGArrName[lnOGCount,6],10,lnDec))          
            ELSE
              &lcOGArrName[lnOGCount,6] = ALLTRIM(STR(&lcOGArrName[lnOGCount,6]))
            ENDIF  
          ELSE
            &lcOGArrName[lnOGCount,6] = VAL(&lcOGArrName[lnOGCount,6])          
          ENDIF                    
          *B603065,1 Hesham (End)             
        CASE &lcOGArrName[lnOGCount,3]='L'
          &lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',IIF(&lcOGArrName[lnOGCount,6]='.T.',.T.,.F.),;
                                         IIF(!(&lcOGArrName[lnOGCount,6]),'.F.','.T.'))
      ENDCASE
    ENDIF
  ENDIF   
ENDFOR



*!********************************************************************
*!
*!              Function: lfGetObjVal
*!
*!********************************************************************
*
FUNCTION lfGetObjVal
PARAMETERS lcValueVar,lcVarNam1,lcVarNam2,lcDataType
DIMENSION lcTemName[2]
STORE '' TO lcTemName
=gfSubStr(&lcValueVar,@lcTemName,lcElmSep)
IF ALEN(lcTemName,1)<2
 DIMENSION lcTemName[2]
 STORE '' TO lcTemName[2]
ENDIF
&lcVarNam1=lfTrnsStr(lcTemName[1],lcDataType)
&lcVarNam2=lfTrnsStr(lcTemName[2],lcDataType)


*!********************************************************************
*!
*!              Function: lfTrnsStr
*!
*!********************************************************************
*
FUNCTION lfTrnsStr
PARAMETERS lcValueStr,lcDataType,lcDirection
DO CASE
  CASE lcDataType $ 'CM'
     RETURN lcValueStr
  CASE lcDataType = 'N'
      RETURN VAL(lcValueStr)
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE




*!********************************************************************
*!
*!              Function: lfSaveUDefV
*!
*!********************************************************************
*
FUNCTION lfSaveUDefV
PARAMETERS lcOGArrName
PRIVATE lcOGSelect
lcOGSelect=SELECT()
SELECT SYREPUVR
DIMENSION &lcOGArrName[1,2] 
STORE '' TO &lcOGArrName
SELECT SYURPFLT
lcRpRepID = lcOGRepID 
SAVE  TO MEMO MUSRDVAR ALL LIKE L?RP*
SELECT(lcOGSelect)



*!********************************************************************
*!
*!              Function: lfLoadUDefV
*!
*!********************************************************************
*
FUNCTION lfLoadUDefV
PRIVATE lcOGSelect
lcOGSelect=SELECT()
SELECT SYREPUVR
SELECT SYURPFLT
IF !EMPTY(MUSRDVAR)
  RESTORE FROM MEMO MUSRDVAR ADDI
  IF TYPE('lcRpRepID')='C'
    lcOGRepID = lcRpRepID
  ENDIF  
ENDIF  

*!********************************************************************
*!
*!              Function: lfWChangeSet
*!
*!********************************************************************
*
FUNCTION lfwChangeset
lcOGLastSet=lcOGSeting



*!********************************************************************
*!
*!              Function: lfChangeSet
*!
*!********************************************************************
*
FUNCTION lfChangeSet  
*IF lcOGSeting=0 OR llOGSysData
*  SHOW GET pbOGDel DISABLE
*ELSE
*  SHOW GET pbOGDel ENABLE  
*ENDIF
IF lcOGSeting<>lcOGLastSet
  =lfChangeGrid(lcOGManRep,lcOGSeting)
ENDIF  

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfOGChOrder 
lcOGFltr=''

*!********************************************************************
*!
*!              Function: lfOGDel
*!
*!********************************************************************
*
FUNCTION lfOGDel
IF lcOGSeting<>0 &&'Default'
 IF !llOGSysData OR (llOGSysData AND !EMPTY(GETENV('DEVELOP')))
  ** WE MUST CONFIRM THE DELETION HERE
  lnOGOption=1
  IF lnOGOption = 1
    lcOGSelect=SELECT()
    SELECT SYURPFLT
    GO lcOGSeting
      =ADEL(laOGSeting,lnOGSeting)
      DIMENSION laOGSeting[ALEN(laOGSeting,1)-1,2]
      DELETE
      =lfChangeGrid(ALLTRIM(lcOGManRep))      
   ENDIF   
  ENDIF  
ENDIF

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfwDown
IF !MDOWN()
  RETURN .F.
ENDIF

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfVDown
_CUROBJ=_CUROBJ-1
KEYBOARD "{SPACEBAR}"

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfLoadRepV
PARAMETERS lcOGRepID
  IF !USED('SYREPUVR')
    SELECT 0
    USE &gcSysHome.SYREPUVR 
  ELSE
    SELECT SYREPUVR
  ENDIF
SET ORDER TO TAG CREPVAR
lcOgTmExact=SET('EXACT')
SET EXACT OFF  
 IF SEEK(PADR(lcOGRepID,8)+'V')
  SCAN REST WHILE cRep_ID+CEXPTYPE+STR(NVARPOS)=PADR(lcOGRepID,8)+'V'
      lcOGVarName=STRTRAN(ALLTRIM(mFld_Name),CHR(13)+CHR(10),'')
      lcOGValue=STRTRAN(mData_Def,CHR(13)+CHR(10),'')
      DO CASE
        CASE cDefa_Typ='V'
          &lcOGVarName=lfTrnsStr(lcOGValue,cData_Typ)
        CASE cDefa_Typ='E'
         &lcOGVarName=&lcOGValue
      ENDCASE
  ENDSCAN
ENDIF    
SET EXACT &lcOGTmExact

*!*************************************************************************
*!
*!              Function: lfvViewRep
*!
*!*************************************************************************
*
FUNCTION lfVViewRep
IF LASTKEY()=27
  KEYBOARD "P"
  RETURN
ENDIF

*E300857,1 Add this line to clear the user selection from the In Range
*screen if needed [Begin]
  =lfClrRngSel()
*E300857,1 Add this line to clear the user selection from the In Range [End]

llCanPrev=IIF(!EMPTY(lcOGPrvRun ),EVAL(lcOGPrvRun),.T.)
IF llCanPrev
llOGEscPrsd= .f.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')

*B801961,1 Change these lines to add a new parameter to gfGenFlt() to let it
*B801961,1 know if it can use the filter array in the expression or not [Begin]
*lcOGHDFlt=gfGenFlt('laOGHDFlt',llOGFilter)
*lcOGFxFlt=gfGenFlt('laOGFxFlt',llOGFilter)
*lcOGVrFlt=gfGenFlt('laOGVrFlt',llOGFilter)
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
*B801961,1 Change these lines to add a new parameter to gfGenFlt() [End]

lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))

=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')
*lcOGTmpExp=lcOGHDFlt+IIF(!EMPTY(lcOGhdFlt),''|Ÿ','')+lcOGFxFlt+;
           IIF(!EMPTY(lcOGvrflt) AND !EMPTY(lcOGFXFlt),'|Ÿ','')+lcOGVrFlt+;
           IIF(!EMPTY(lcOGhdFlt) AND !EMPTY(lcOGFXFlt),'Ÿ','')+IIF(!EMPTY(lcOGVrFlt),'Ÿ','')
           
*lcOGTmpExp=lcOGHDFlt+IIF(!EMPTY(lcOGFxFlt),'|Ÿ','')+lcOGFxFlt+;
           IIF(!EMPTY(lcOGvrflt) AND !EMPTY(lcOGFXFlt),'|Ÿ','')+lcOGVrFlt+;
           IIF(!EMPTY(lcOGFXFlt),'Ÿ','')+;
           IIF(!EMPTY(lcOGVrFlt) AND !EMPTY(lcOGFXFlt),'Ÿ','')
           
lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFXFlt) AND !EMPTY(lcOGVRFlt),lcOGFXFlt+'|Ÿ',lcOGFxFlt)+;
           lcOGVrFlt+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFXFlt)AND !EMPTY(lcOGVRFlt),'Ÿ','')
           
IF EMPTY(lcOGFxFlt) AND EMPTY(lcOGVrFlt)
  lcRpExp=lcOgHdFlt
ELSE
  lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
          ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
ENDIF          
lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)        
lcogExc=SET('EXACT')        
SET EXACT ON

*BADRAN NOW
*E301170,1
*llOGFltCh = lcogfltr<>lcrpexp        
llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        
*E301170,1

SET EXACT &lcOGExc
IF llOGFltCh
  lcOGFltr=lcRpExp
ENDIF

lcOGDevice=gcDevice
lcOGOutFile=gcOutFile
gcDevice="SCREEN"    && set printer to temprory file name
gcOutFile=gcWorkDir+gfTempName()+".TXT"      
llOGHeader = glHeader
glHeader = .T.
*SET PRINTER TO &gcOutFile
SET CONSOL OFF
lcOldProcSet = SET('PROCEDURE')
SET PROCEDURE TO &lcSetProcd
************ IF LASTFORM <> THE SELECTED FORM

IF !EMPTY(lcOGFormV)
  IF !(lcOGLastForm==&lcOGFormV)
    
    *E300900,1 Remove these lines to give the user the capability to choose
    *E300900,1 the report mode [Begin]
    *DO CASE
    *  CASE _DOS
    *    lcOGPlatForm='DOS'    
    *  CASE _WINDOWS
    *    lcOGPlatForm='WINDOWS '  
    *  CASE _MAC
    *    lcOGPlatForm='MAC'    
    *  CASE _UNIX
    *    lcOGPlatForm='UNIX '    
    *ENDCASE  
    *E300900,1 Remove these lines to give the user the capability [End]
    
    lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
    
    *E300900,1 Remove this line for we have canceled the Option that
    *E300900,1 change the report objects style [Begin]
    *=gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
    *E300900,1 Remove this line for we have canceled the Option [End]
    
    lcOGLastForm=&lcOGFormV
    llOGStyleCh=.T.
  ENDIF  

  *E300900,1 Change this line to give the user the capability to choose
  *E300900,1 the report mode [Begin]
  *IF ((llOGFltCh AND llOGRefForm) OR llOGStyleCh) AND !EMPTY(&lcOGFormArr[1,1])
  IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
  *E300900,1 Change this line to give the user the capability [End]
    
    =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  ENDIF
ENDIF  
********************************************
=lfOGSetRelat()

IF !EMPTY(lcOldProcset)
  *E300399,1 Hesham El-Sheltawi (Start)
  *DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName+'.RPR' ,.T.

  *B602079,1 Run reports application from the active application directory
  *DO (gcRepHome+LEFT(lcOGPrgName,2)+'REPORT.APP') WITH lcOGPrgName ,.T.      
  DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.
  *B602079,1 end

*E300399,1 Hesham El-Sheltawi (End)  
ELSE
  ***  COLECT DATA AND SET RELA AND DISP REPORT
  **** ADD CALLING TO THE LFDISPREP AND THE SELECT SQL IF NEEDED
  IF !llOGFilter
    lcOGRpFields = GetRpField() 
    lcOGRpFiles  = GetRpFile()
    lcOGOrdBy  = IIF(EMPTY(lcOGOrder),'',&lcOGOrder)
    SELECT &lcOGRpFields FROM &lcOGRpFiles WHERE &lcRpExp &lcOGOrdBy
    DO gfDispRe WITH EVAL('lcRpForm')
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcRpExp),'FOR ','')+lcRpExp  
  ENDIF

ENDIF  

=lfOGClrRelat()
SET PROCEDURE TO &lcOldProcset
*SET PROCEDURE TO (lcOGPrgName)
gcDevice=lcOGDevice
gcOutFile=lcOGOutFile
glHeader = llOGHeader

*badran now
llOGFltCh = .F.
ENDIF

*!*************************************************************************
*!
*!              Function: lfvRunRep
*!
*!*************************************************************************
*
FUNCTION lfvRunRep
IF LASTKEY()=27
  KEYBOARD "R"
  RETURN
ENDIF

*E300857,1 Add this line to clear the user selection from the In Range
*screen if needed [Begin]
  =lfClrRngSel()
*E300857,1 Add this line to clear the user selection from the In Range [End]

llCanPrev=IIF(!EMPTY(lcOGPrvRun ),EVAL(lcOGPrvRun),.T.)
llOGEscPrsd = .f.
lcOldProcSet = SET('PROCEDURE')
SET PROCEDURE TO
*SET PROCEDURE TO &lcOGPrgName
*B601004,1 Hesham (START)
*B601004,1 check if the report form has been changed before going to
*B601004,1 the select device screen to initialize the printer driver screen
*B601004,1 with the right platform
*E300667,1 Hesham El-Sheltawi (Start)
IF !llProgram
*E300667,1 Hesham El-Sheltawi (End)
  IF !EMPTY(lcOGFormV) AND llCanPrev
    IF !(lcOGLastForm==&lcOGFormV)
      
      *E300900,1 Remove these lines to give the user the capability to
      *E300900,1 choose the report mode [Begin]
      *=lfGetPlatFrm()
      *E300900,1 Remove these lines to give the user the capability [End]
      
      lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
      *E300900,1 Remove this line for we have canceled the Option that
      *E300900,1 change the report objects style [Begin]
      *=gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
      *E300900,1 Remove this line for we have canceled the Option [End]
      
      lcOGLastForm=&lcOGFormV
      llOGStyleCh=.T.
    ENDIF  
  ENDIF  
*E300667,1 Hesham El-Sheltawi (Start)
ENDIF
*E300667,1 Hesham El-Sheltawi (End)  
*B601004,1 Hesham (END)

** Mohamed Hassan **
*B601091,1 Hesham El-Sheltawi (Start)
*B601091,1 REMARK all the next condition for the APCHKPDD report
*IF TYPE('lcRepForm') = 'C'
*  IF _WINDOWS .AND. lcRepForm = 'APCHKPDD'
*    OGPlatForm = 'WINDOWS '
*  ENDIF
*ELSE
  *E300399,1 Hesham El-Sheltawi (Start)
*  OGPlatForm=lcOGPlatForm   && This from befor
   IF !EMPTY(lcOGFormV)
     OGPlatForm=lcOGPlatForm   && This from befor   
   ELSE
     OGPlatForm='DOS'   && This from befor   
   ENDIF
  *E300399,1 Hesham El-Sheltawi (End)  
*ENDIF
*B601091,1 Hesham El-Sheltawi (End)
** Mohamed Hassan **
*E300667,1 Hesham El-Sheltawi (Start)
*IF llCanPrev AND pSetup(.T.,llOGBatchOk)
IF llCanPrev AND (llProgram OR !(lcRunDirct $ 'MR') OR pSetup(.T.,llOGBatchOk))
  IF gcDevice = 'BATCH'
    SELECT SYUBSRVJ
    IF EMPTY(CREP_ID)
      lcRpRepID = lcOGRepID 
      SAVE  TO MEMO MUSRDVAR ALL LIKE L?RP*
      IF !EMPTY(laOGHDFLT[1,1])
        SAVE TO MEMO mRepHdFlt ALL LIKE laOGHDFLT
      ENDIF
      IF !EMPTY(laOGFxFLT[1,1])  
        =lfInitValue('laOGFxFlt','T')    
        SAVE TO MEMO mRepFxFlt ALL LIKE laOGFxFlt
        =lfInitValue('laOGFxFlt','C')      
      ENDIF  
      IF !EMPTY(laOGVrFLT[1,1])  
        =lfInitValue('laOGVrFlt','T')
        SAVE TO MEMO mRepVrFlt ALL LIKE laOGVrFlt  
       =lfInitValue('laOGVrFlt','C') 
      ENDIF  
      REPLACE CREP_ID WITH lcOGManRep,;
              cstatus WITH 'O'      
    ENDIF
    SET PROCEDURE TO &lcOldProcSet    
    RETURN
  ENDIF
*E300667,1 Hesham El-Sheltawi (End)
  lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
  lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
  lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
  lcOGSelect=SELECT()
  =lfInitValue('laOGVrFlt','T')
  =lfInitValue('laOGFXFlt','T')
  
  *B801961,1 Change these lines to add a new parameter to gfGenFlt() to let it
  *B801961,1 know if it can use the filter array in the expression or not [Begin]
  *lcOGHDFlt=gfGenFlt('laOGHDFlt',llOGFilter)
  *lcOGFxFlt=gfGenFlt('laOGFxFlt',llOGFilter)
  *lcOGVrFlt=gfGenFlt('laOGVrFlt',llOGFilter)
  lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
  lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
  lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  *B801961,1 Change these lines to add a new parameter to gfGenFlt() [End]
  
  lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
  lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
  lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
  =lfInitValue('laOGVrFlt','C')
  =lfInitValue('laOGFXFlt','C')
  lcRpExp=lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
        lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
        IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')
  lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)                
  lcogExc=SET('EXACT')        
  SET EXACT ON

  *BADRAN NOW
  *E301170,1
  *llOGFltCh = lcogfltr<>lcrpexp        
  llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        
  *E301170,1

  *E300667,1 Hesham El-Sheltawi (Start)
  IF !llProgram
  *E300667,1 Hesham El-Sheltawi (End)
    SET EXACT &lcOGExc
    IF llOGFltCh
      lcOGFltr=lcRpExp
    ENDIF
    SET CONSOL OFF
    ************ IF LASTFORM <> THE SELECTED FORM
    
    *E300900,1 Remove these lines to give the user the capability to choose
    *E300900,1 the report mode [Begin]
    *IF (_WINDOWS OR _MAC) AND (gcDevice='FILE')
    *  lcOGTemPlat=lcOGPlatForm
    *  lcOGPlatForm='DOS'
    *ENDIF
    *E300900,1 Remove these lines to give the user the capability [End]
    
    IF !EMPTY(lcOGFormV)
      
      *E300900,1 Remove these lines to give the user the capability to
      *E300900,1 choose the report mode [Begin]
      *IF !(lcOGLastForm==&lcOGFormV)
      *  DO CASE
      *    CASE _DOS
      *      lcOGPlatForm='DOS'    
      *    CASE _WINDOWS
      *      lcOGPlatForm='WINDOWS '  
      *    CASE _MAC
      *      lcOGPlatForm='MAC'    
      *    CASE _UNIX
      *      lcOGPlatForm='UNIX '    
      *  ENDCASE  
      *  lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      *
      *  =gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
      *  lcOGLastForm=&lcOGFormV
      *  llOGStyleCh=.T.
      *ENDIF  
      *E300900,1 Remove these lines to give the user the capability [End]
        
      *E300900,1 Change this line to give the user the capability to choose
      *E300900,1 the report mode [Begin]
      *IF ((llOGFltCh AND llOGRefForm) OR llOGStyleCh) AND !EMPTY(&lcOGFormArr[1,1])
      IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
      *E300900,1 Change this line to give the user the capability [End]
        
        =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
      ENDIF
    ENDIF  
    
    *E300900,1 Remove these lines to give the user the capability to choose
    *E300900,1 the report mode [Begin]
    *IF (_WINDOWS OR _MAC) AND (gcDevice='FILE')
    *  lcOGPlatForm=lcOGTemPlat
    *ENDIF
    *E300900,1 Remove these lines to give the user the capability [End]
    
  ********************************************  
  *  DO (lcOGPrgName+'.RPR') IN (gcRepHome+gcAct_Appl+'REPORT.APP')
    =lfOGSetRelat()

    IF !EMPTY(lcOldProcset)
      *E300399,1 Hesham El_Sheltawi (Start)
      *DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName+'.RPR' ,.T.
      
      *B602079,1 Run reports application from the active application directory
      *DO (gcRepHome+LEFT(lcOGPrgName,2)+'REPORT.APP') WITH lcOGPrgName ,.T.    

      DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.
      *B602079,1 end
      *E300399,1 Hesham El_Sheltawi (End)    
  
    ELSE
      ***  COLECT DATA AND SET RELA AND DISP REPORT
      **** ADD CALLING TO THE LFDISPREP AND THE SELECT SQL IF NEEDED
      IF !llOGFilter
        lcOGRpFields = GetRpField() 
        lcOGRpFiles  = GetRpFile()
        lcOGOrdBy  = IIF(EMPTY(lcOGOrder),'',&lcOGOrder)
        SELECT &lcOGRpFields FROM &lcOGRpFiles WHERE &lcRpExp &lcOGOrdBy
        DO gfDispRe WITH EVAL('lcRpForm')
      ELSE
        DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcRpExp),'FOR ','')+lcRpExp  
      ENDIF
    ENDIF  

    =lfOGClrRelat()
    SET CONSOL ON
  *E300667,1 Hesham El-Sheltawi (Start)
  ELSE
    llOgTrmnat = .T.
    llRetFlt = .T.
    lcOGExp = lcRpExp
    CLEAR READ
  ENDIF
  *E300667,1 Hesham El-Sheltawi (End)  
ENDIF  
SET PROCEDURE TO &lcOldProcSet
*badran now
llOGFltCh = .F.



FUNCTION lfChStyle
************ IF LASTFORM <> THE SELECTED FORM
IF !EMPTY(lcOGFormV)
  IF !EMPTY(&lcOGFormArr[1,1])
    IF !(lcOGLastForm==&lcOGFormV) OR llOgRefForm
      
      *E300900,1 Remove this line for we have canceled the Option that
      *E300900,1 change the report objects style [Begin]
      *=gfInstAr(EVAL(lcOGFormV),lcOGFormArr)
      *E300900,1 Remove this line for we have canceled the Option [End]
      
      lcOGLastForm=&lcOGFormV
      llOGStyleCh=.T.
    ENDIF  
    IF !llOGRefForm
      DIMENSION laOGFormCnt[ALEN(&lcOGFormArr,1)]
      STORE .T. TO laOGFormCnt
    ELSE
      =gfFrmCnt(EVAL(lcOGFormV),'laOGFormCnt')  
    ENDIF    
    llOGStyleCh = gfChRepF(lcOGFormArr,'laOGFormCnt') OR llOGStyleCh
  ENDIF  
ENDIF  



***************************************************
**    This function must be in the GQB 

***************************************************


*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION GetRpFile
PRIVATE lcOGSelected,lcOGFiles,lnOGCount
lcOGSelected=SELECT()
SELECT SYDREPRT 
IF SEEK(lcOGRepID) 
  IF !EMPTY(mRep_Fils)
    RESTORE FROM MEMO mRep_Fils ADDI
    lcOGFiles=''
    FOR lnOGCount=1 TO ALEN(LASELFILE,1)
      lcOGFiles=lcOGFiles+IIF(lnOGCount>1,',','')+;
      IIF(LEFT(LASELFILE[lnOGCount,2],2)='SY','&gcSysHome.','&gcDataDir.')+;
      laSelFile[lnOGCount,2]+IIF(laSelFile[lnOGCount,2]<>laSelFile[lnOGCount,1],;
              " "+laSelFile[lnOGCount,1],'')
    ENDFOR
  ENDIF
ELSE
  lcOGFiles=''
ENDIF
SELECT(lcOGSelected)
RETURN lcOGFiles

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION GetRpField
PRIVATE lcOGSelected,lcOGFiles,lnOGCount
lcOGSelected=SELECT()
SELECT SYDREPRT 
IF SEEK(lcOGRepID) 
  IF !EMPTY(mRepField)
    RESTORE FROM MEMO mRepField ADDI
    lcOGFiles=''
    FOR lnOGCount=1 TO ALEN(LASELFIELDS,1)
      lcOGFiles=lcOGFiles+IIF(lnOGCount>1,',','')+;
      laSelFields[lnOGCount,1]
      IF TYPE('laSelFields[lnOGCount,2]')='C'
        lcOGFiles=lcOGFiles+IIF(!EMPTY(laSelFields[lnOGCount,2]),;
               ' AS "'+ALLTRIM(laSelFields[lnOGCount,2])+'"','')
      ENDIF         
    ENDFOR
  ENDIF
ELSE
  lcOGFiles=''
ENDIF
SELECT(lcOGSelected)
RETURN lcOGFiles






*!*************************************************************************
*!
*!              Function: lfOpenRpFls
*!
*!*************************************************************************
*
FUNCTION lfOpenRpFls
DIMENSION laOpenBy[ALEN(laSelFile,1)]
STORE .F. TO laOpenBy

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

FOR lnCount = 1 TO ALEN(laSelFile,1)
  *B601025,1 Hesham (Start)
  *B601025,1 change the condition to check if there is any opened
  *B601025,1 files for this report to prevend any error in the RG
  *B601025,1 saving
*  IF !USED(laSelFile[lnCount,1])  
*MAN Added AND FILE(IIF(LEFT(laSelFile[lnCount,2],2)='SY',gcSysHome,gcDataDir)+laSelFile[lnCount,2]+'.DBF')
  IF !EMPTY(laSelFile[lnCount,1]) AND !USED(laSelFile[lnCount,1]) AND ;
     FILE(IIF(LEFT(laSelFile[lnCount,2],2)='SY',gcSysHome,gcDataDir)+laSelFile[lnCount,2]+'.DBF')
  *B601025,1 Hesham (End)  
    laOpenBy[lnCount]=.T.
    SELECT 0
    lcSysHome=IIF(LEFT(laSelFile[lnCount,2],2)='SY',gcSysHome,gcDataDir)
    USE &lcSysHome.&laSelFile[lnCount,2] ALIAS &laSelFile[lnCount,1] AGAIN
  ENDIF  
  *E000000,1 Hesham (Start)
  *E000000,1 if the report file has a selected order then
  *E000000,1 set the order of the file
  IF USED(laSelFile[lnCount,1]) AND !EMPTY(laSelFile[lnCount,3])
    *B603195,1 Reham On 10/06/1999   *** Begin ***
    *B603195,1 Fix the error "Index tag not found" if using descending order
    *SET ORDER TO TAG (laSelFile[lnCount,3]) IN laSelFile[lnCount,1]
    SET ORDER TO TAG &laSelFile[lnCount,3] IN laSelFile[lnCount,1]
    *B603195,1 Reham On 10/06/1999   *** End   ***
  ENDIF
  *E000000,1 Hesham (End)
ENDFOR

*!*************************************************************************
*!
*!              Function: lfClosRpFls
*!
*!*************************************************************************
*
FUNCTION lfClosRpFls

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

FOR lnCount = 1 TO ALEN(laSelFile,1)
  IF laOpenBy[lnCount]
    USE IN &laSelFile[lnCount,1]
  ENDIF
ENDFOR

*E300857,1 Add these lines to close the range operator temp. cursors [Begin]

*E300857,1 IF The option grid is not called from a program
IF !llProgram

  *E300857,1 FOR Loop to scan the array that hold the temp. cursor names used
  *by In range screen for the fixed filter
  FOR lnCount = 1 TO ALEN(laFxFltCur , 1)
    
    *E300857,1 IF The temp. cursor is opened
    IF USED(laFxFltCur[lnCount , 2])
      USE IN laFxFltCur[lnCount , 2]
    ENDIF    && End of IF USED(laFxFltCur[lnCount , 2])
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laFxFltCur , 1)
  
  *E300857,1 FOR Loop to scan the array that hold the temp. cursor names used
  *by In range screen for the variable filter
  FOR lnCount = 1 TO ALEN(laVrFltCur , 1)
    
    *E300857,1 IF The temp. cursor is opened
    IF USED(laVrFltCur[lnCount , 2])
      USE IN laVrFltCur[lnCount , 2]
    ENDIF    && End of IF USED(laVrFltCur[lnCount , 2])
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laVrFltCur , 1)
  
ENDIF    && End of IF !llProgram

*E300857,1 Add these lines to close the range operator temp. cursors [End]

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfGetLastObj
PARAMETERS lnOGWinNo
lnOGStart=IIF(lnOGWinNo>1,laOGWinObj[lnOGWinNo-1]+1,1)
lnOGEnd  =IIF(lnOGWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
              ALEN(laOGObjCnt,1),laOGWinObj[lnOGWinNo])
lnOGCanAcc=0              
FOR lnOGCount= lnOGEnd TO lnOGStart STEP -1
  IF laOGObjCnt[lnOGCount]              
    lnOGCanAcc=lnOGCount
    EXIT
  ENDIF
ENDFOR 
RETURN lnOGCanAcc          

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfGetOrder
PARAMETERS lcOGSortID

lcOGSelected=SELECT()
IF !USED('SYREPSRT')
  SELECT 0
  USE &gcSysHome.SYREPSRT 
ELSE
  SELECT SYREPSRT
ENDIF
SET ORDER TO TAG CREP_ID
lcOGSort=''
IF !EMPTY(lcOGSortID)
  IF SEEK(lcOGRepID+lcOGSortID)
    lcOGSort=mOrderStr
  ENDIF
ENDIF
SELECT(lcOGSelected)
RETURN lcOGSort

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfTrapEsc
PARAMETERS llOgSwitch,lnOGThrStart
lnOGThrStart = IIF(TYPE('lnOGThrStart')<>'N',0,lnOGThrStart)
IF llOGSwitch
  SET ESCAPE ON
  lcOGEscHnd = ON('ESCAPE')
*  lcOGEscPrm = [DO lfOGEscHnd WITH ] +STR(lnOGThrStart)
  lcOGEscPrm = [DO lfOGEscHnd ] 
  ON ESCAPE &lcOGEscPrm
ELSE
  SET ESCAPE OFF
  ON ESCAPE &lcOGEscHnd  
ENDIF  






*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfOGEscHnd
PARAMETERS lnOGThrStart
 ON ESCAPE
 IF !llOGEscPrsd AND gfModalGen("QRM00114B00023","ALERT") = 1
*     lnCount=lnOGThrStart
*     lcOGEscPrm = [DO lfOGEscHnd WITH ] +STR(lnOGThrStart)
     lcOGEscPrm = [DO lfOGEscHnd] 
*    IF WEXIST("gwdThermo")
*      RELEASE WINDOW gwdThermo
*   ENDIF     
    ON ESCAPE &lcOGEscPrm
    RETRY
 ELSE
   IF !llOGEscPrsd
     lcOGFltr=''
     llOGEscPrsd=.T.
      KEYBOARD "{ESCAPE}"
*     lcOGEscPrm = [DO lfOGEscHnd WITH ] +STR(lnOGThrStart)
      lcOGEscPrm = [DO lfOGEscHnd] 
     ON ESCAPE &lcOGEscPrm      
*   ELSE
*     ON ESCAPE   
*     SET ESCAPE OFF
   ENDIF  
    lcPrgToRet=IIF(!EMPTY(lcOGPrgName),'TO '+lcOGPrgName,'.T.')
    RETURN &lcPrgToRet
 ENDIF





*!********************************************************************
*!
*!              Function: lfGetPopPrm
*!
*!********************************************************************
*
FUNCTION lfGetPopPrm
PARAMETERS lcOgVarName,lcOGPopType,lcOGValidEnt,lcOGEmpVl,lcOGDataTyp,lcOGPopN

lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
*lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 

    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    
    *B801961,1 Change these lines to display the code + code description
    *B801961,1 in the case of editable codes and to sort the displayed
    *B801961,1 values (by code if the code is editable otherwise by code
    *B801961,1 description) [Begin]
    *SELECT cDiscrep FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *        AND crltfield='N';
    *  INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *        AND crltfield='N';      
    *  INTO ARRAY laOGVldVal    
    
    PRIVATE llIsEdtble
    llIsEdtble = gfIsEdtble(lcOGFieldName)    && Check if the code is editable

    *--MAN  
    PRIVATE llFONow  
    llFONow  = .F.
    IF !USED("CODES")
      llFONow  = .T.
    ENDIF
    
    *B801961,1 If the code is editable
    IF llIsEdtble
    *E300789,1 Hesham (Start)
      *SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND crltfield='N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND crltfield='N';      
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    

      SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND crltfield='N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
      
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND crltfield='N';      
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    
        

    *E300789,1 Hesham (End)
    ELSE    && Else [If the code is not editable]
    *E300789,1 Hesham (Start)
      *SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND crltfield='N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND crltfield='N';      
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    


      SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND crltfield='N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt
      
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND crltfield='N';      
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    ENDIF    && End of IF llIsEdtble
    *B801961,1 Change these lines to display the code + code description [End]
    IF llFONow  
      USE IN CODES
    ENDIF  
 
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    
    *E300900,1 Change this line to add the capability to open the file
    *E300900,1 from particular path
    
    *lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGFlPath = IIF('\' $ laOGVldFl[1] , '' ,;
                     IIF(LEFT(laOGVldFl[1] , 2) = 'SY' , gcSysHome ,;
                         gcDataDir))
    
    *E300900,1 Change this line to add the capability to open the file [End]
    
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD    
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
    ENDIF
  
  *E300900,1 Add these lines to add the capability to make valid entries
  *E300900,1 from array [Begin]
  
  *E300900,1 Case of valid entries from array
  CASE lcOGPopType = 'A'
    
    *E300900,1 If there is valid entries
    IF !EMPTY(lcOGValidEnt)
      *-- Get the 1st array name
      lcArrayNam = SUBSTR(lcOGValidEnt , 1 , ATC(lcValSep , lcOGValidEnt)-1)
      DIMENSION laOGVldEnt[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldEnt)
      
      *-- Get the 2nd array name
      lcArrayNam = SUBSTR(lcOGValidEnt , ATC(lcValSep,lcOGValidEnt) + 1)
      DIMENSION laOGVldVal[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldVal)
    ENDIF    && End of IF !EMPTY(lcOGValidEnt)
  *E300900,1 Add these lines to add the capability [End]
  
ENDCASE
IF !EMPTY(laOGVldEnt[1,1])
  lcOGPrompt=''
  
  *E300900,1 Change these lines to make all the popups in the option grid
  *E300900,1 from array [Begin]
  
  *FOR lnOGCount = 1 TO ALEN(laOGVldEnt,1)
  *  lcOGPrompt=lcOGPrompt+IIF(lnOGCount=1,'',';')+laOGVldEnt[lnOGCount,1]
  *ENDFOR
  
  *-- Variable to hold the name of the array that will be used to create
  *-- the popup
  lcArrayNam = IIF(lcOGPopType <> 'A' , 'laOGPopAry' ,;
                   SUBSTR(lcOGValidEnt , 1 , ATC(lcValSep , lcOGValidEnt)-1))
  
  *-- Variable to hold the number of the array element that will be used
  *-- as the first element of the popup in string
  lcFirstElm = IIF(lcOGPopType = 'A' , '1' ,;
                   ALLTRIM(STR(ALEN(laOGPopAry , 1) + 1)))
  
  *-- Variable to hold the number of array elements that will be used
  *-- to create ths popup
  lcNmbOfElm = ALLTRIM(STR(ALEN(laOGVldEnt , 1)))
  
  *E300900,1 If the valid entries is not from array
  IF lcOGPopType <> 'A'
    
    *E300900,1 For loop to scan the array that hold the valid entries
    *E300900,1 descriptions that will be displayed to the user
    FOR lnOGCount = 1 TO ALEN(laOGVldEnt , 1)

      *B602575,1 global Sort by Popup is still List Popup, not from array. [Begin
      *B602575,1 Add Check for type of popup name parameter, to do old adv 
      *B602575,1 code if it's List popup otherwise do new code done by HSS.
      *B602575,1 Comment out the following line of code and doing it inside 
      *B602575,1 check for type block.
  
      *DIMENSION laOGPopAry[ALEN(laOGPopAry , 1) + 1 , 1]
      *laOGPopAry[ALEN(laOGPopAry , 1) , 1] = laOGVldEnt[lnOGCount , 1]

      *B602575,1 if it's list popup.
      IF TYPE('lcOGPopN')<>'L'
        lcOGPrompt=lcOGPrompt+IIF(lnOGCount=1,'',';')+laOGVldEnt[lnOGCount,1]
      
      ELSE  &&B602575,1 else it's array popup
        
        DIMENSION laOGPopAry[ALEN(laOGPopAry , 1) + 1 , 1]
        laOGPopAry[ALEN(laOGPopAry , 1) , 1] = laOGVldEnt[lnOGCount , 1]

      ENDIF  &&B602575,1 end if it's list popup.
      *B602575,1 global Sort by Popup is still List Popup, not from array. [End..

    ENDFOR    && End of FOR lnOGCount = 1 TO ALEN(laOGVldEnt , 1)
  ENDIF    && End of IF lcOGPopType <> 'A'
  
  *B602575,1 if it's array popup prompt must be a null string. [Begin
  *B602575,1 thus comment out the following line of code and doing it
  *B602575,1 inside check for empty of prompt variable.
  
  *lcOGPrompt = 'FROM ' + lcArrayNam + ' RANGE ' + lcFirstElm + ', ';
  *             + lcNmbOfElm
  
  IF EMPTY(lcOGPrompt)
    lcOGPrompt = 'FROM ' + lcArrayNam + ' RANGE ' + lcFirstElm + ', ';
                 + lcNmbOfElm
  ENDIF                 
  *B602575,1 if it's array popup prompt must be a null string. [End..
  
  *E300900,1 Change these lines to make all the popups [End]
  
ENDIF
 lnOGBarNo=0
 lnOGBarNo = ASCAN(laOGVldVal,GetString(&lcOGVarName,TYPE(lcOGVarName)),1)
 IF TYPE('lcOGPopN')<>'L'
   &lcOGPopN = MAX(lnOGBarNo,1)
 ENDIF
RETURN lcOGPrompt


*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfVldWinPop
PARAMETERS lcOgVarName,lcOGDataTyp,lcOGPopType,lcOGValidEnt,lnOGRow,lnOGCol,lnOGSR,lnOGSC,lcOGEmpVl
lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lnOGSR=IIF(TYPE('lnOGSR')<>'N',lnOGRow,lnOGSR)
lnOGSC=IIF(TYPE('lnOGSC')<>'N',lnOGCol,lnOGSC)
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
*MAN Flag to chek if the codes file was already open
llCodIsOpen = USED("CODES")

DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
*B602022,1 Hesham (Start)
*B602022,1 change the select query to ignore the related fields of
*B602022,1 the codes 
*    SELECT cDiscrep FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt


    *B801961,1 Change these lines to display the code + code description
    *B801961,1 in the case of editable codes and to sort the displayed
    *B801961,1 values (by code if the code is editable otherwise by code
    *B801961,1 description) [Begin]
    *SELECT cDiscrep FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *  AND CRLTFIELD = 'N';
    *  INTO ARRAY laOGVldEnt
    
    PRIVATE llIsEdtble
    llIsEdtble = gfIsEdtble(lcOGFieldName)    && Check if the code is editable
    
    *B801961,1 If the code is editable
    IF llIsEdtble
    *E300789,1 Hesham (Start)
      *SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt

      SELECT cCode_No + ' - ' + cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt

    *E300789,1 Hesham (Start)
    ELSE    && Else [If the code is not editable]
    *E300789,1 Hesham (Start)
      *SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt

      SELECT cDiscrep FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt

    *E300789,1 Hesham (End)
    ENDIF    && End of IF llIsEdtble
    *B801961,1 Change these lines to display the code + code description [End]
      
*    SELECT cCode_No FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal    

    *B801961,1 Change these lines to display the code + code description
    *B801961,1 in the case of editable codes and to sort the displayed
    *B801961,1 values (by code if the code is editable otherwise by code
    *B801961,1 description) [Begin]
    *SELECT cCode_No FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
    *  AND CRLTFIELD = 'N' ;     
    *  INTO ARRAY laOGVldVal    
    
    *B801961,1 If the code is editable
    IF llIsEdtble
    *E300789,1 Hesham (Start)
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND CRLTFIELD = 'N' ;     
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    

      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N' ;     
       ORDER BY cCode_No;
        INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    ELSE    && Else [If the code is not editable]
    *E300789,1 Hesham (Start)
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
             AND CRLTFIELD = 'N' ;     
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    

      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N' ;     
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    ENDIF    && End of IF llIsEdtble
    *B801961,1 Change these lines to display the code + code description [End]
    
*B602022,1 Hesham (End)      
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    
    *E300900,1 Change this line to add the capability to open the file
    *E300900,1 from particular path
    
    *lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGFlPath = IIF('\' $ laOGVldFl[1] , '' ,;
                     IIF(LEFT(laOGVldFl[1] , 2) = 'SY' , gcSysHome ,;
                         gcDataDir))
    
    *E300900,1 Change this line to add the capability to open the file [End]
    
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD    
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount= 1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=lfTrnsStr(laOGVldVal[lnOGCount,1],lcOGDataTyp)
        ENDFOR
    ENDIF
  
  *E300900,1 Add these lines to add the capability to make valid entries
  *E300900,1 from array [Begin]
  
  *E300900,1 Case of valid entries from array
  CASE lcOGPopType = 'A'
    
    *E300900,1 If there is valid entries
    IF !EMPTY(lcOGValidEnt)  
      *-- Get the 1st array name
      lcArrayNam = SUBSTR(lcOGValidEnt , 1 , ATC(lcValSep , lcOGValidEnt)-1)
      DIMENSION laOGVldEnt[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldEnt)
      
      *-- Get the 2nd array name
      lcArrayNam = SUBSTR(lcOGValidEnt , ATC(lcValSep,lcOGValidEnt) + 1)
      DIMENSION laOGVldVal[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
      =ACOPY(&lcArrayNam , laOGVldVal)
    ENDIF    && End of IF !EMPTY(lcOGValidEnt)  
  *E300900,1 Add these lines to add the capability [End]
  
ENDCASE

IF !llCodIsOpen AND USED("CODES")   
  USE IN CODES
ENDIF
lnOGBarNo=0
lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
IF LASTKEY()=13
 IF lnOGBarNo=EVAL(VARREAD())
   RETURN .F.
 ELSE
   &lcOGVarName=laOGVldVal[EVAL(VARREAD()),1]  
 ENDIF
ELSE
 lcOGVar = VARREAD()
 &lcOGVar= MAX(lnOGBarNo,1)
 _CUROBJ=_CUROBJ+1
 RETURN .F.
ENDIF  



*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfSaveTDefV
PARAMETERS lcOGArrName
PRIVATE lcOGSelect
lcOGSelect=SELECT()
SELECT SYREPUVR
DIMENSION &lcOGArrName[1,2] 
STORE '' TO &lcOGArrName
*SELECT STRTRAN(ALLTRIM(mFld_Name),CHR(13)+CHR(10),''),'' ;
  FROM &gcSysHome.SYREPUVR;
  WHERE cRep_ID = lcOGRepID;
   AND cExpType = 'V';
   AND lAskRunT = .T.;
   INTO ARRAY &lcOGArrName
SELECT mFld_Name,'' ;
  FROM &gcSysHome.SYREPUVR;
  WHERE cRep_ID = PADR(lcOGRepID,8);
   AND cExpType = 'V';
   AND lAskRunT = .T.;
   INTO ARRAY &lcOGArrName   

IF lnOGVariable>0
  DIMENSION &lcOGArrName[lnOGVariable,2]    
  FOR lnOGCount= 1 TO lnOGVariable
    lcOGVarName = &lcOGArrName[lnOGCount,1]
    lcOGValue= &lcOGVarName
    &lcOGArrName[lnOGCount,2] = lcOGValue
  ENDFOR  
ENDIF  
SELECT(lcOGSelect)



*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfLoadTDefV
lcOGArrName = 'laOGUDefVr'
IF !EMPTY(mUsrDVar)
  RESTORE FROM MEMO mUsrDVar ADDI
  FOR lnOGCount=1 TO ALEN(&lcOGArrName,1)
    IF !EMPTY(&lcOGArrName[lnOGCount,1])
      lcOGVarName=&lcOGArrName[lnOGCount,1]
      lcOGValue=&lcOGArrName[lnOGCount,2]
      &lcOGVarName=lcOGValue
    ENDIF
  ENDFOR
  lnOGVariable=ALEN(laOGUdefVr,1)
ELSE
  lnOGVariable=0  
ENDIF  



FUNCTION lfGetSetting
lcOGSelect=SELECT()
SELECT SYURPFLT
DIMENSION laOGSeting[1,2]

*SELECT Cfltr_Des,CFLTR_ID;
   FROM &gcSysHome.SYURPFLT;
   WHERE cRep_ID+cUser_ID+CFLTR_ID = lcOGRepID;
   AND CUSER_ID=GCUSER_ID;
   INTO ARRAY laOGSeting
   
SELECT Cfltr_Des,RECNO();
   FROM &gcSysHome.SYURPFLT;
   WHERE cRep_ID+cUser_ID+CFLTR_ID = PADR(lcOGMANREP,8);
   AND (CUSER_ID=GCUSER_ID;
   OR lSys_Data); 
   INTO ARRAY laOGSeting
      
IF !EMPTY(laOGSeting[1,2])   
  DIMENSION laOGSeting[ALEN(laOGSeting,1)+1,2]   
  =AINS(laOGSeting,1)
ENDIF  
laOGSeting[1,1]='Default'
laOGSeting[1,2]=0
SELECT (lcOGSelect)


FUNCTION getString
PARAMETERS lcValue,lcDataType
DO CASE
  CASE lcDataType $ 'CM'
      RETURN lcValue
  CASE lcDataType = 'N'
      RETURN ALLTRIM(STR(lcValue))
  CASE lcDataType = 'L'
    RETURN IIF(lcValue=.T.,'.T.','.F.')
  CASE lcDataType = 'D'    
    RETURN CTOD(lcValue)
ENDCASE  







************************ SCROLL CONTROLING
****************************************

FUNCTION lfWhen

*E301170,1 MAB
lcOGAskOld = EVALUATE(SYS(18))

IF BETWEEN(_CUROBJ-lnOGStObj,lnObjNum,lnObjMax) 
  LLFromObj= .F.
  RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
ELSE
  IF INLIST(LASTKEY(),5,4,19,24) AND !LLFromObj
      DO CASE
      CASE lnObjMax<_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.               
*          MOVE WINDOW gwdgridobj BY -2,0
          lnObjMax=_CUROBJ-lnOGStObj        
          lnObjNum=lnObjMax-6
          MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
          =lfModiHScrol()   
       ENDIF  
       RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
      CASE lnObjNum>_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.                     
*          MOVE WINDOW gwdgridobj BY 2,0  
          lnObjNum=_CUROBJ-lnOGStObj
          lnObjMax=lnObjNum+6
          MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
          =lfModiHScrol()   
        ENDIF  
       RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
    ENDCASE
  ELSE
    RETURN .F.  
  ENDIF 
ENDIF

FUNCTION lfPageUp
IF LASTKEY()=18 
  lnOldObjN=lnObjNum
  lnObjNum=IIF((lnObjNum-6)<=1,1,lnObjNum-6)
  IF lnObjNum<>lnOldObjN
    MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
    lnObjMax=lnObjNum+6
    =lfModiHScrol()   
    _CUROBJ=lnObjNum+lnOGStObj
  ENDIF
ELSE
   LLFromObj= .T.    
ENDIF  
RETURN .F.

FUNCTION lfPageDn
IF LASTKEY()=3
  lnOldObjN=lnObjNum
  lnObjNum=IIF((lnObjNum+6)>=lnMaxObj-6,lnMaxObj-6,lnObjNum+6)
  IF lnObjNum<>lnOldObjN  
    MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
    lnObjMax=lnObjNum+6
    =lfModiHScrol()   
    _CUROBJ=lnObjNum+lnOGStObj
  ENDIF  
ELSE
 LLFromObj= .T.      
ENDIF  
RETURN .F.
*!********************************************************************
*!
*!              Function: lfScrollup
*!
*!********************************************************************
*
FUNCTION lfScrollUP
lnObjNo1=lfObjPos('IBSCRUP')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnObjNum>1 AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnObjNum = lnObjNum-1
  lnObjMax=lnObjNum+6
   MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
   =lfModiHScrol()   
ENDDO
_CUROBJ=lnObjNum+lnOGStObj




*!********************************************************************
*!
*!              Function: lfScrollDn
*!
*!********************************************************************
*
FUNCTION lfScrollDn
lnObjNo1=lfObjPos('IBSCRDN')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnObjNum+6<lnMaxObj AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnObjNum = lnObjNum+1
  lnObjMax=lnObjNum+6
   MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
   =lfModiHScrol()
ENDDO
_CUROBJ=lnObjNum+lnOGStObj




*!********************************************************************
*!
*!              Function: lfScrollVer
*!
*!********************************************************************
*
FUNCTION lfScrollVer
lnObjNo1=lfObjPos('IBSCRHBAR')
IF !MDOWN() AND !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL("gwdOGscrl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
lnCurCol=MROW("gwdOGscrl")
lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                 ((lnMaxObj-6)))),1)

DO CASE 
  CASE IIF(_DOS,lnCurCol<>lnScrolRow,lnWCurPos<>lnObjNum)
    lnSign=IIF(lnWCurPos>lnObjNum,1,-1)                 
    DO WHILE MDOWN() 
      IF BETWEEN(MCOL("gwdOGscrl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]) AND;
         BETWEEN(MROW("gwdOGscrl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3])
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
        lnSign2=IIF(lnWCurPos>lnObjNum,1,-1)                             
       IF lnSign=lnSign2
         lnObjNum=lnObjNum+(lnSign*IIF(ABS(lnWCurPos-lnObjNum)>1,1,ABS(lnWCurPos-lnObjNum)))
         lnObjMax=lnObjNum+6                
         MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0
         =lfModiHScrol()        
        ENDIF  
     ENDIF  
       =INKEY(.01,'HM')
   ENDDO
 OTHERWISE
    lnTmpCol=lnScrolROW
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
    =lfVerCurs()                                
    DO WHILE MDOWN() 
      IF (BETWEEN(MROW("gwdOGscrl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]))
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
         =lfVerCurs()                                
         =INKEY(.01,'HM')         
     ENDIF  
       =INKEY(.01,'HM')     
   ENDDO    
     IF lnObjNum<>lnWcurPos
       lnObjNum=lnWCurPos
       lnObjMax=lnObjNum+6       
       lnScrolRow=lnTmpCol
       MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0
       =lfModiHScrol()    
     ENDIF      
ENDCASE   
_CUROBJ=lnObjNum+lnOGStObj




*********************************************************************

*!********************************************************************
*!
*!              Function: lfModiHScrol
*!
*!********************************************************************
*
FUNCTION lfModiHScrol
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7))*(lnObjNum-1)+laObjScrP[lnObjPos,1]))
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7)-1)*(lnObjNum-1)+laObjScrP[lnObjPos,1])))
                 
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] CLEAR TO lnPos+1.000+2*0.077,laObjScrP[lnObjPos,4] &&-0.088         

         
        @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,2];
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	
                  
         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow,laObjScrP[lnObjPos,4]-0.500+3*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	         
         
         @ lnScrolRow+1.00,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)
         
         @ lnScrolRow,laObjScrP[lnObjPos,4]-0.500+3*0.077 TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500+3*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)		  
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        


*!*************************************************************************
*!
*!              Function: lfObjPos
*!
*!*************************************************************************
*  This function to return an object position in the array that defined in
* the screen layout 'setup' to hold the position of the screen objects and
* the names to and this function is to return the row # that have the data
* of this object
FUNCTION lfObjPos
PARAMETERS lcOGObjNam
PRIVATE lnCount
llOGFound=.f.
FOR lnCount=1 TO ALEN(laObjScrP,1)
  IF UPPER(laObjScrP[lnCount,5])=UPPER(lcOGObjNam)
    llOGFound=.t.
    EXIT  
  ENDIF
ENDFOR
RETURN IIF(llOGFound,lnCount,0)

         
         
         
*!********************************************************************
*!
*!              Function: lfVerCurs
*!
*!********************************************************************
*
FUNCTION lfVerCurs
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7))*(lnObjNum-1)+laObjScrP[lnObjPos,1]))

       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF

     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,1])))
                 
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME

         @ lnPos,laObjScrP[lnObjPos,2] TO lnPos+1.00+2*0.077,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(192,192,192,,,,)         	
         	

         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00+2*0.077,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(255,255,255,,,,)         	    	
         	
         	


       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        
         

FUNCTION lfOGSetRelat

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

*E300399,1 Hesham El-Sheltawi (Start)
*E300399,1 Check if there is any files opened with the report
IF !EMPTY(laSelFile[1,1])
*E300399,1 Hesham El-Sheltawi (End)
FOR lnCount = 1 TO ALEN(laSelFile,1)
  *-- MAN START
  IF !USED(laSelFile[lnCount,1])
     LOOP
  ENDIF 
  *-- MAN END  
  SELECT (laSelFile[lnCount,1])
  IF !EMPTY(laSelFile[lnCount,3])
    IF ATC(' DESC',laSelFile[lnCount,3])>0
       SET ORDER TO TAG &laSelFile[lnCount,3]
    ELSE
       SET ORDER TO TAG &laSelFile[lnCount,3] ASCENDING  
    ENDIF   
  ELSE
*    SET ORDER TO 
  ENDIF  
  SET FILTER TO &laSelFile[lnCount,6]
ENDFOR
FOR lnCount = 1 TO ALEN(laSelFile,1)
  *-- MAN START
  IF !USED(laSelFile[lnCount,1])
     LOOP
  ENDIF 
  *-- MAN END  
  SELECT (laSelFile[lnCount,1])
  lcRelation=STRTRAN(laSelFile[lnCount,4],CHR(255),',')
  IF !EMPTY(lcRelation)
    SET RELATION TO &lcRelation ADDI
  ELSE
    SET RELATION TO  
  ENDIF  
ENDFOR
FOR lnCount = 1 TO ALEN(laSelFile,1)
  *-- MAN START
  IF !USED(laSelFile[lnCount,1])
     LOOP
  ENDIF 
  *-- MAN END  

  SELECT (laSelFile[lnCount,1])
  IF !EMPTY(laSelFile[lnCount,5])
    SET SKIP TO (STRTRAN(laSelFile[lnCount,5],CHR(255)))
  ELSE
    SET SKIP TO  
  ENDIF  
ENDFOR
*-- MAN START ADDED If Condition
IF USED(laSelFile[1,1])
  SELECT (laSelFile[1,1])
  GO  TOP
ENDIF 
  *-- MAN END  

*E300399,1 Hesham El-Sheltawi (Start)
ENDIF
*E300399,1 Hesham El-Sheltawi (End)

FUNCTION lfOGClrRelat

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

*E300399,1 Hesham El-Sheltawi (Start)
*E300399,1 Check if there is any files opened with the report
IF !EMPTY(laSelFile[1,1])
*E300399,1 Hesham El-Sheltawi (End)
FOR lnCount = 1 TO ALEN(laSelFile,1)
  *-- MAN START
  IF !USED(laSelFile[lnCount,1])
     LOOP
  ENDIF 
  *-- MAN End
  SELECT (laSelFile[lnCount,1])
*  SET ORDER TO 
  SET FILTER TO
  SET RELATION TO 
  SET SKIP TO
ENDFOR
*E300399,1 Hesham El-Sheltawi (Start)
ENDIF
*E300399,1 Hesham El-Sheltawi (End)

*B601004,1 Hesham (Start)            
FUNCTION lfGetPlatFrm
DO CASE
  CASE _DOS
    lcOGPlatForm='DOS'    
  CASE _WINDOWS
    lcOGPlatForm='WINDOWS '  
  CASE _MAC
    lcOGPlatForm='MAC'    
  CASE _UNIX
    lcOGPlatForm='UNIX '    
ENDCASE  
lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
*B601004,1 Hesham (END)            

*!*************************************************************
*! Name      : lfRangCurs
*! Developer : Haytham El_Sheltawi
*! Date      : 04/19/1998
*! Purpose   : Function to get temp. name to be used for the 
*!             In range operator cursor
*!*************************************************************
*! Called from : lfDefVarPart()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) Option grid array name [laOGVrFlt or laOGFxFlt]
*!                     2) Option grid array row number
*!*************************************************************
*! Return      : The temp. name
*!*************************************************************
*E300857,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfRangCurs

PARAMETERS lcOGArayN , lnOGArayR

PRIVATE lcArayName , lcReturn , lnArayElm , lnArayRow

lcReturn  = ''          && Variable to hold the returned value

*-- Variable to hold the array name that hold the temp. cursor names
*-- for the current filter option
lcArayName = IIF('FX' $ UPPER(lcOGArayN) , 'laFxFltCur' , 'laVrFltCur')

*-- Variable to hold the number of the element holding the current filter
*-- option row number
lnArayElm = ASCAN(&lcArayName , lnOGArayR)

*-- If there is no array element for this filter option or if column 7 in
*-- the array lcOGArrName [laOGFxFlt & laOGVrFlt] is 'R' ('R' For range)
*-- [column 7 is 'R' if this is a saved filter or if the user used the 
*-- advance screen but did not change the current filter option]
IF lnArayElm = 0 .OR. &lcOGArayN[lnOGArayR , 7] = 'R'
  
  *-- If there is no row for the current filter option in the array
  *-- lcArayName
  IF lnArayElm = 0
    
    *-- Look if there is unused cursor temp. name
    lnArayElm = ASCAN(&lcArayName , 0)
    
    *-- If we did not found any unused cursor temp. name
    IF lnArayElm = 0
      
      *-- Get a new cursor temp. name or use the one saved in the array
      *-- lcOGArayN if column 7 is 'R'
      lcReturn = IIF(&lcOGArayN[lnOGArayR , 7] = 'R' ,;
                     &lcOGArayN[lnOGArayR , 6] , gfTempName())
      
      DIMENSION &lcArayName[ALEN(&lcArayName , 1) + 1 , 3]
      &lcArayName[ALEN(&lcArayName , 1) , 1] = lnOGArayR
      &lcArayName[ALEN(&lcArayName , 1) , 2] = lcReturn
      &lcArayName[ALEN(&lcArayName , 1) , 3] = SYREPUVR.cClrRngSel
    ELSE    && Else [If we did found unused cursor temp. name]
      lnArayRow = ASUBSCRIPT(&lcArayName , lnArayElm , 1)
      lcReturn  = &lcArayName[ALEN(&lcArayName , 1) , 2]
      
      &lcArayName[ALEN(&lcArayName , 1) , 1] = lnOGArayR
      &lcArayName[ALEN(&lcArayName , 1) , 3] = SYREPUVR.cClrRngSel
    ENDIF    && End of IF lnArayElm = 0
  ENDIF    && End of IF lnArayElm = 0
ELSE    && Else [If there is no array element for this filter option and column 7 <> 'R']
  lnArayRow = ASUBSCRIPT(&lcArayName , lnArayElm , 1)
  lcReturn  = &lcArayName[lnArayRow , 2]
  
  *-- If the temp. cursor is open
  IF USED(lcReturn)
    USE IN (lcReturn)
  ENDIF    && End of IF USED(lcReturn)
ENDIF    && End of lnArayElm = 0 .OR. &lcOGArayN[lnOGArayR , 7] = 'R'

RETURN lcReturn

*!*************************************************************
*! Name      : lfClrRngSel
*! Developer : Haytham El_Sheltawi
*! Date      : 04/21/1998
*! Purpose   : Function to clear the user selection from the
*!             In Range screen(s) if needed
*!*************************************************************
*! Called from : lfVViewRep() , lfvRunRep()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300857,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfClrRngSel

*E300900,1 Add this line [Begin]
PRIVATE lnCount
*E300900,1 Add this line [End]

*-- For loop to scan the array that hold the temp. cursor names used
*-- by In range screen for the fixed filter
FOR lnCount = 1 TO ALEN(laFxFltCur , 1)
  
  *-- IF The selection is to be cleared
  IF !EMPTY(laFxFltCur[lnCount , 3]) .AND. EVALUATE(laFxFltCur[lnCount , 3])
    
    &laFxFltCur[lnCount , 3] = .F.
    
    *-- If the temp. cursor is opened
    IF USED(laFxFltCur[lnCount , 2])
      USE IN laFxFltCur[lnCount , 2]
    ENDIF    && End of IF USED(laFxFltCur[lnCount , 2])
    
  ENDIF    && End of IF !EMPTY(laFxFltCur[lnCount , 3]) .AND. .......
ENDFOR    && End of FOR lnCount = 1 TO ALEN(laFxFltCur , 1)

*-- For loop to scan the array that hold the temp. cursor names used
*-- by In range screen for the variable filter
FOR lnCount = 1 TO ALEN(laVrFltCur , 1)
  
  *-- If the selection is to be cleared and the temp. cursor is opened
  IF !EMPTY(laVrFltCur[lnCount , 3]) .AND. EVALUATE(laVrFltCur[lnCount , 3])
    
    &laVrFltCur[lnCount , 3] = .F.
    
    *-- If the temp. cursor is opened
    IF USED(laVrFltCur[lnCount , 2])
      USE IN laVrFltCur[lnCount , 2]
    ENDIF    && End of IF USED(laVrFltCur[lnCount , 2])
    
  ENDIF    && End of IF !EMPTY(laVrFltCur[lnCount , 3]) .AND. .......
ENDFOR    && End of FOR lnCount = 1 TO ALEN(laVrFltCur , 1)

*!*************************************************************
*! Name      : lfOperPrmp
*! Developer : Haytham El_Sheltawi
*! Date      : 04/26/1998
*! Purpose   : Function to get the operator prompt if there is any
*!*************************************************************
*! Called from : lfDefVarPart()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : operator prompt variable (passed by reference)
*!*************************************************************
*! Return      : 1) .T. if there is a prompt for the current
*!                  filter option and operator.
*!               2) .F. Otherwise.
*!*************************************************************
*E300869,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfOperPrmp

PARAMETERS lcPrompt

PRIVATE lcOperStr , lcPrompStr , llReturn , lnParts , lcOperPrmp ,;
        lnOperPos , lnSerPos

lnParts = OCCURS('~' , mOperPrmp)           && Variable to hold the number of times the character '~' occurs in the field mOperPrmp

*-- If there is no operator in the option grid array (lcOGArrName)
*-- or if the operator prompt do not exist in the field mOperPrmp
IF EMPTY(&lcOGArrName[lnOGArrPos,5]) ;
   .OR. lnParts < IIF(&lcOGArrName[lnOGArrPos,4] , 1 , 2)
  
  RETURN .F.
ENDIF    && End of IF EMPTY(&lcOGArrName[lnOGArrPos,5]).....

*-- Variable to hold the Operators and operator prompts string
lcOperPrmp = IIF(lnParts < 3 , mOperPrmp ,;
                 SUBSTR(mOperPrmp , 1 , AT('~' , mOperPrmp , 3) - 1))

llReturn = .F.                    && Variable to hold the returned value

*-- Variable to hold the Operators string
lcOperStr = SUBSTR(lcOperPrmp , 1 , AT('~' , lcOperPrmp) - 1)

*-- Variable to hold the position of the current operator in the operators
*-- string
lnOperPos = AT(UPPER(ALLTRIM(&lcOGArrName[lnOGArrPos,5])) , UPPER(lcOperStr))

*-- If the current operator exist in the operators string
IF lnOperPos <> 0
  lnSerPos   = OCCURS('|' , SUBSTR(lcOperStr , 1 , lnOperPos - 1))
  
  *-- Variable to hold the operator prompts string
  lcPrompStr   = SUBSTR(lcOperPrmp , AT('~' , lcOperPrmp) + 1)
  
  *-- If operator is 'Is' + < Operator >
  IF &lcOGArrName[lnOGArrPos,4]
    lcPrompStr = IIF(AT('~' , lcPrompStr) = 0 , lcPrompStr ,;
                   SUBSTR(lcPrompStr , 1 , AT('~' , lcPrompStr) - 1))
    
  *-- Else, If operator is 'Is Not' + < Operator >
  ELSE    
    lcPrompStr = SUBSTR(lcPrompStr , AT('~' , lcPrompStr) + 1)
  ENDIF    && End of IF &lcOGArrName[lnOGArrPos,4]
  
  *-- If there is a prompt for the current operator
  IF OCCURS('|' , lcPrompStr) >= lnSerPos
    llReturn  = .T.
    lcPrompt  = SUBSTR(lcPrompStr , IIF(lnSerPos = 0 , 1 ,;
                       AT('|' , lcPrompStr , lnSerPos) + 1))
    
    lcPrompt  = IIF(AT('|' , lcPrompt) = 0 , lcPrompt ,;
                    SUBSTR(lcPrompt , 1 , AT('|' , lcPrompt) - 1))
    
  ENDIF    && End of IF OCCURS('|' , lcPrompStr) >= lnSerPos
ENDIF    && End of IF lnOperPos <> 0

RETURN llReturn

*!*************************************************************
*! Name      : lfRepPltFr
*! Developer : Haytham El_Sheltawi
*! Date      : 07/08/1998
*! Purpose   : Function to get the report -FRX- available platforms
*!*************************************************************
*! Called from : GFOPGRID.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Report form name (FRX name)
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300900,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfRepPltFr

PARAMETERS lcRepFrmNm

PRIVATE lcFile

*-- If there is no report form name (FRX name)
IF TYPE('lcRepFrmNm') <> 'C' .OR. EMPTY(lcRepFrmNm)
  lcRepAvlM = 'T'
ELSE    && Else [If there is a report form name (FRX name)]
  
  *-- Get the file name including the path
  DO CASE
    CASE FILE(gcRepHome + lcRepFrmNm + '.FRX')
      lcFile = gcRepHome + lcRepFrmNm + '.FRX'
    CASE FILE(gcRepHome + gcAct_Appl + '\' + lcRepFrmNm + '.FRX')
      lcFile = gcRepHome + gcAct_Appl + '\' + lcRepFrmNm + '.FRX'
    CASE FILE(gcRepHome + lcRepFrmNm + '.LBX')
      lcFile = gcRepHome + lcRepFrmNm + '.LBX'
    CASE FILE(gcRepHome + gcAct_Appl + '\' + lcRepFrmNm + '.LBX')
      lcFile = gcRepHome + gcAct_Appl + '\' + lcRepFrmNm + '.LBX'
    OTHERWISE
      lcFile = ''
  ENDCASE

  *-- If the file (FRX) dose not exist
  IF EMPTY(lcFile)
    lcRepAvlM = 'T'
  ELSE    && Else [If the file (FRX) dose not exist]
    
    *-- Open the FRX as a table and get the available platforms
    USE (lcFile) IN 0 AGAIN ALIAS (lcRepTmpNm)
    GO TOP IN (lcRepTmpNm)
    lcRepAvlM = IIF(&lcRepTmpNm..Platform = 'WINDOWS' , 'G' , 'T')
    GO BOTTOM IN (lcRepTmpNm)
    lcRepAvlM = IIF(IIF(&lcRepTmpNm..Platform = 'WINDOWS' , 'G' , 'T') =;
                     lcRepAvlM , lcRepAvlM , 'B')
    
    USE IN (lcRepTmpNm)
  ENDIF    && End of IF EMPTY(lcFile)
ENDIF    && End of IF TYPE('lcRepFrmNm') <> 'C' .OR. EMPTY(lcRepFrmNm)

lcOGPlatForm = IIF(lcRepAvlM $ 'BG' , 'WINDOWS ' , 'DOS')
lcRepMode    = IIF(lcRepAvlM $ 'BG' , 'Graphics' , 'Text')
lcRepModeS   = IIF(lcRepAvlM = 'B' , 'ENABLE' , 'DISABLE')
SHOW GET lcRepMode &lcRepModeS


*!*************************************************************
*! Name      : lfvRepMode
*! Developer : Haytham El_Sheltawi
*! Date      : 07/12/1998
*! Purpose   : Valid function for the Mode popup
*!*************************************************************
*! Called from : Mode popup [GRIDSET.SCX]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300900,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfvRepMode

lcOGPlatForm = IIF(lcRepMode = 'Graphics' , 'WINDOWS ' , 'DOS')

*!*************************************************************
*! Name      : lfGetSupEx
*! Developer : Haytham El_Sheltawi
*! Date      : 07/19/1998
*! Purpose   : Function to get all the fields and expressions
*!             that have a suppress expression and their
*!             suppressing expressions.
*!*************************************************************
*! Called from : lfvAdvanced()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Report Id
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300900,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfGetSupEx

PARAMETERS lcReportId

PRIVATE laFiles
DIMENSION laSupExp[1,2]
laSupExp = ''

*-- If there is a report Id
IF TYPE('lcReportId') = 'C' .AND. !EMPTY(lcReportId)
  DIMENSION laFiles[1,8]
  laFiles[1,1] = 'SYREPUVR'
  laFiles[1,2] = 'CREPVAR'
  laFiles[1,3] = .T.
  =gfChkFile(.T. , @laFiles)
  
  *B802279,1 Add this line to make sure that the report variables file
  *          (SYREPUVR) is the current alias [Begin]
  SELECT SYREPUVR
  *B802279,1 Add this line to make sure that the report variables file [End]
  
  *-- If there is records for filter options of type field for this report
  IF SEEK(PADR(lcReportId , 8) + 'F')
    
    *-- Scan loop to scan the filter options of type field for this report
    *-- that have suppress expression
    SCAN REST;
        WHILE cRep_Id + cExpType = PADR(lcReportId , 8) + 'F';
          FOR !EMPTY(mSupExpr)
      
      DIMENSION laSupExp[ALEN(laSupExp , 1) + 1 , 2]
      laSupExp[ALEN(laSupExp , 1) , 1] = mFld_Name
      laSupExp[ALEN(laSupExp , 1) , 2] = ALLTRIM(mSupExpr)
    ENDSCAN    && End of SCAN REST WHILE cRep_Id + cExpType = ... FOR !EMPTY(mSupExpr)
  ENDIF    && End of IF SEEK(PADR(lcReportId , 8) + 'F')
  
  *-- If there is records for filter options of type expression for
  *-- this report
  IF SEEK(PADR(lcReportId , 8) + 'E')
    
    *-- Scan loop to scan the filter options of type expression for this
    *-- report that have suppress expression
    SCAN REST;
        WHILE cRep_Id + cExpType = PADR(lcReportId , 8) + 'E';
          FOR !EMPTY(mSupExpr)
      
      DIMENSION laSupExp[ALEN(laSupExp , 1) + 1 , 2]
      laSupExp[ALEN(laSupExp , 1) , 1] = ALLTRIM(mFld_Name)
      laSupExp[ALEN(laSupExp , 1) , 2] = ALLTRIM(mSupExpr)
    ENDSCAN    && End of SCAN REST WHILE cRep_Id + cExpType = ... FOR !EMPTY(mSupExpr)
  ENDIF    && End of IF SEEK(PADR(lcReportId , 8) + 'E')
  =gfChkFile(.F. , @laFiles)
ENDIF    && End of IF TYPE('lcReportId') = 'C' .AND. !EMPTY(lcReportId)

*!*************************************************************
*! Name      : lfMovSupEx
*! Developer : Haytham El_Sheltawi
*! Date      : 07/19/1998
*! Purpose   : Function to delete any row with suppress
*!             expression = .T. from the Advanced arrays.
*!*************************************************************
*! Called from : lfvAdvanced()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) Reference to the array that holds the
*!                        fields and expressions.
*!                     2) Reference to the array that holds the
*!                        the fields and expressions headers.
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300900,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfMovSupEx

PARAMETERS laFields , laFildHedr

PRIVATE lnRowCount , lnCount , lnDeleted , lnElmNumb

*-- Variable to hold the number of rows in the array (laFields)
lnRowCount = ALEN(laFields , 1)

lnDeleted  = 0

*-- For loop to scan the array (laFields)
FOR lnCount = 1 TO lnRowCount
  
  *-- Search for the filter option in the suppressed options array
  lnElmNumb = ASCAN(laSupExp , laFields[lnCount - lnDeleted , 1])
  lnElmNumb = IIF(lnElmNumb = 0 , 0 , ASUBSCRIPT(laSupExp , lnElmNumb , 1))
  
  *-- If this filter option is to be suppressed
  IF lnElmNumb <> 0 .AND. EVALUATE(laSupExp[lnElmNumb , 2])
    =gfADel(@laFields , lnCount - lnDeleted , 1)
    =gfADel(@laFildHedr , lnCount - lnDeleted , 1)
    lnDeleted = lnDeleted + 1
  ENDIF    && End of IF lnElmNumb <> 0 .AND. EVALUATE(laSupExp[lnElmNumb , 2])
ENDFOR    && End of FOR lnCount = 1 TO lnRowCount



**********
*E301170,1
FUNCTION lfvAskVld
IF !llOGFltCh
  PRIVATE lcCurrObjVl
  lcCurrObjVl = EVALUATE(SYS(18))
  llOGFltCh = !(lcCurrObjVl == lcOGAskOld)
ENDIF
RETURN .T.


*!*************************************************************
*! Name      : lfMacroSub
*! Developer : Haytham El_Sheltawi
*! Date      : 08/11/1999
*! Purpose   : Function to perform the macro substitution within
*!             any giving string.
*!*************************************************************
*! Called from : lfDefVarPart()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) lcString, Any string with macros
*!                        that need to be substituted.
*!*************************************************************
*! Return      : The string after performing the macro substitution.
*!*************************************************************
*E301302,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfMacroSub

PARAMETERS lcString

PRIVATE lcReturStr , lnNoOfMacr , lnCount , lnMacroEnd , lcMacroStr

*-- Get the number of occurrences of the macro character "&" in the string
lnNoOfMacr = OCCURS('&' , lcString)

*-- If the string has no macro character "&" return the string as it is.
IF lnNoOfMacr = 0
  RETURN lcString
ENDIF    && End of IF lnNoOfMacr = 0

lcReturStr = ''        && Variable to hold the returned string

*-- For loop to perform the necessary macro substitution
FOR lnCount = 1 TO lnNoOfMacr
  
  *-- Add the part of the string that is ahead of the macro character "&"
  *-- to the returned string
  lcReturStr = lcReturStr + SUBSTR(lcString , 1 , AT('&' , lcString) - 1)
  
  *-- Remove the part of the string that is ahead of the macro character "&"
  *-- from the original string
  lcString   = SUBSTR(lcString , AT('&' , lcString))
  
  *-- Get the end position of the macro
  lnMacroEnd = MIN(IIF(AT('.' , lcString) = 0 ,;
                       LEN(lcString) , AT('.' , lcString)) ,;
                   IIF(AT(' ' , lcString) = 0 ,;
                       LEN(lcString) , AT(' ' , lcString) - 1) ,;
                   IIF(AT('&' , lcString , 2) = 0 ,;
                       LEN(lcString) , AT('&' , lcString , 2) - 1))
  
  *-- Get the part of the string that belongs to the macro substitution
  lcMacroStr = SUBSTR(lcString , 1 , lnMacroEnd)
  
  *-- Remove the part of the string that belongs to the macro substitution
  *-- from the original string
  lcString   = SUBSTR(lcString , lnMacroEnd + 1)
  
  *-- Perform the macro substitution and add the result to the returned
  *-- string
  lcReturStr = lcReturStr + [&lcMacroStr]
ENDFOR    && End of FOR lnCount = 1 TO lnNoOfMacr

*-- Add any string left from the original string that doesn't have a
*-- macro character "&"
lcReturStr = lcReturStr + lcString

RETURN lcReturStr
