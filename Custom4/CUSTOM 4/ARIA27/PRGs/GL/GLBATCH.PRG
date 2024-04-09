*:************************************************************************
*:
*: Procedure file: GLBATCH.PRG
*:
*:         System: GL System
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified: 3/27/94
*:
*:  Procs & Fncts:
*:
*:    PROCEDURE lpShow       ***                                     ***
*:    FUNCTION  lfvData_1    ***                                     ***
*:    FUNCTION  lfvData_7    ***                                     ***
*:    FUNCTION  lfwPostyr    ***                                     ***
*:    FUNCTION  lfvPostyr    ***                                     ***
*:    FUNCTION  lfvSDatPrd   ***                                     ***
*:    FUNCTION  lfvEDatPrd   ***                                     ***
*:    FUNCTION  lfFillAry1   ***                                     ***
*:    FUNCTION  lfFillAry2   ***                                     ***
*:    FUNCTION  lfwStaDate   ***                                     ***
*:    FUNCTION  lfvStaDate   ***                                     ***
*:    FUNCTION  lfwEndDate   ***                                     ***
*:    FUNCTION  lfvEndDate   ***                                     ***
*:    FUNCTION  lfwStaPrd    ***                                     ***
*:    FUNCTION  lfvStaPrd    ***                                     ***
*:    FUNCTION  lfwEndPrd    ***                                     ***
*:    FUNCTION  lfvEndPrd    ***                                     ***
*:    FUNCTION  lfRevShow    ***                                     ***
*:    FUNCTION  lfvReview    ***                                     ***
*:    FUNCTION  lfvPrint     ***                                     ***
*:    FUNCTION  lfvPost      ***                                     ***
*:    FUNCTION  lfvHold      ***                                     ***
*:    FUNCTION  lfvBatType   ***                                     ***
*:    FUNCTION  lfvNew       ***                                     ***
*:    FUNCTION  lfvTrans     ***                                     ***
*:    FUNCTION  lfvOK        ***                                     ***
*:    PROCEDURE lpDelScr     ***                                     ***
*:    PROCEDURE lpSavScr     ***                                     ***
*:    PROCEDURE lfCheckRec   ***                                     ***
*:    FUNCTION  gfCpEdit     ***                                     ***
*:               
*:      Documented 1/26/1994
*:************************************************************************
*B600486,1 Reham On 06/21/95
*B600486,1 Control the array elemets that hold the control pannel button
*B600486,1 status for <Delete> button "laCtrStat[8]" & for <Edit> button
*B600486,1 "laCtrStat[7]" to control the the edit & delete Bars in the
*B600486,1 record pad.
*B601099,1 M.H 06/27/96 Hide push button print.
*B601480,1 HS  09/12/96 Fix the browse button of the Key Field 
*B601480,1              disable and enable status
*B601514,1 M.H 01/16/97 Fix the changing of the popup if the user run onther screen.
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,5 AHMED & RENEE 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,71 Walid (WAM) 03/03/99 Open GLTYPES & GLACCHAR
*          With gfOpenFile() , Move any calling for gfModalGen() to be after
*          gfSetup() , Comment SELECT GLBATCH just befor calling gfSetup()
*          as gfSetup() return the base file selected .
*         
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*B602733,1 RENEE 04/15/99. 1. Start/End dates of batch were empty.
*B602733,1				   2. Start/End period popups were not working.
*
*B602859,1 AMM 05/02/99 Fix the bug of setting period to 01 if changed.
*B601514,4 Walid (WAM) on 05/05/99 Take Subledger type into consideration .
*B607000,1 WAB 03/03/2003  Fix the bug of Cance.bmp not found
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laCtrStat

DECLARE laKeyField[1,4] , laTstatus[7,1]  , laFisYear[3,1]  , laSelect[1,1]  ,;
        laAceptRec[1]   , la_TPostYr[5] 

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'BATCHNO'
laKeyField[1,4] = 1

laDefProc [7]  = .F.            && Use local Delete procedure
laDefProc [9]  = .F.            && Use local Save   procedure


*B601514,4 Add Subledger type to the array [Begin]
*DECLARE laBatType[2,2]
DECLARE laBatType[3,2]
laBatType[1,1] = 'Normal'
laBatType[1,2] = 'N'
laBatType[2,1] = 'Statistical'
laBatType[2,2] = 'S'

laBatType[3,1] = 'Subledger' &&-- added due to B601514,1
laBatType[3,2] = 'L'         &&-- added due to B601514,1
*B601514,4 Add Subledger type to the array [End..]

** lcYear        :  Holds the selected batch year.
** lcDelMesag    :  Holds the prompt name insted of delete "void" 
** lcPrompt      :
** lcOldStPrd    :  Holds the old start period.
** lcOldEnPrd    :  Holds the old end   period.
** lcStartPrd    :  Holds the     start period.
** lcEndPrd      :  Holds the     end   period.
** lcOldYear     :  Holds the old selected batch year.
** lcDumYear     :
** lcDumStart    :
** lcDummyEnd    :
** lcStatus      :  Holds the batch status.
** lcBalncid     :  Holds the batch balacne id 'CR' or 'DR'
*B601514,4 Just add Subledger to the comment .

** lcBatType     :  Holds the current batch type Normal,Statistical,Subledger

** lcScope       :  Holds the filter expression.
** lcBStamp      :  Holds the batch stamp (user+date+time).
** lcPeriodLock  :  Holds the locked period number.
** lcObjStatus   :
** laSelect      :  Initialize the array laSelect
** laFisYear     :  Initialize the array laFisYear
** laTstatus     :  Initialize the array laTstatus

** llOK          :  OK Push Button pressed at Preview screen
** llPeriodLock  :  .T. if the period if locked.
** llglobShow    :
** llBrowse      :  To say coming from browse or not.

** lnCentury     :  In case of CENTURY='ON' lnCentury=2 Else =0
** lncbHold      :  Holds the batch hold status.
** lnBalance     :  Holds the diff. between total CR and Total DR.
** rbCriteria    :  Holds the selection of criteria (review screen)

** rbType        :  Holds the selection of review mode Transations or transations detailas. 

** rbSort        :  Holds the selection of sort mode (date,trns. no.)
** puActtype     :

** ldOldStDat    :  Holds old batch start date.
** ldOldEnDat    :  Holds old batch end   date.


lcYear          =  SPACE(4)
lcDelMesag      =  "void"
lcPrompt        = IIF(_DOS,"PROMPT '\<Void'",'')
lnBalance       = 0.00
rbCriteria      = 3

STORE " "    TO lcOldStPrd , lcOldEnPrd   , lcStartPrd  , lcEndPrd   ,;
                lcOldYear  , lcDumYear    , lcDumStart  , lcDummyEnd ,;
                lcStatus   , lcBalncid    , lcBatType   , lcScope    ,;
                lcBStamp   , lcPeriodLock , lcObjStatus , laSelect   ,;
                laFisYear  , laTstatus

STORE .F.    TO llOK       , llPeriodLock , llglobShow  , llBrowse

STORE 0      TO lnCentury  , lncbHold

STORE 1      TO rbType     , rbSort       , puActtype   , puBatType

STORE {}     TO ldOldStDat , ldOldEnDat

la_TPostYr [1]  = 'Previous'      
la_TPostYr [2]  = 'Current'       
la_TPostYr [3]  = 'Next '         
la_TPostYr [4]  = 'History'            
la_TPostYr [5]  = ''    && to initialize the year field in the select mode.
*E301077,71 [BEGIN]
*SELECT GLTYPES
*GO TOP
*IF EOF()
*  *** The types and ranges have not ***
*  *** been setup yet.  You have to  ***
*  *** define the accounts type and ranges first. ***
*  *** < Ok > ***
*  =gfModalGen("TRM02038B00000","DIALOG")
*  glQuitting  = .T.  
*  RETURN 
*ENDIF

*** check if the chart of accounts is created.
*SELECT GLACCHAR
*LOCATE
*IF EOF()
*  *** The chart of accounts is empty. You have to create. ***
*  *** the chart of accounts first...
*  *** <  Ok  > ***
*  =gfModalGen("TRM02215B00000","DIALOG")
*  glQuitting = .T.
*  RETURN
*ENDIF

*SELECT GLBATCH
*E301077,71 [END..]

IF !gfSetup()    
  RETURN
ENDIF  

*E301077,71 [BEGIN]
=gfOpenFile(gcDataDir+'GLACCHAR',gcDataDir+'Acctcode','SH')
=gfOpenFile(gcDataDir+'GLTYPES' ,gcDataDir+'Typecode','SH')
*SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  *** The chart of accounts is empty. You have to create. ***
  *** the chart of accounts first...
  *** <  Ok  > ***
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF
*E301077,71 [END..]
llDoLocal  = .T.
lcLoclShow = "lfCheckRec"


laFisYear[1,1]  = STR(lnCurr_yer-1,4) + ' - Previous'
laFisYear[2,1]  = STR(lnCurr_yer  ,4) + ' - Current'
laFisYear[3,1]  = STR(lnCurr_yer+1,4) + ' - Next'

llDate  = IIF(LEFT(GLSETUP.cSetPcnt,1)=='D',.T.,.F.)
                                              && check if the program
                                              && working with periods 
                                              && format or dates format.

IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ELSE
  IF laScrMode[2] .OR. laScrMode[3]
    && in case of return to the program after activate the any other 
    && program, Reread the batch status, batch Credit, Batch Debit.
    laData[2]  = glbatch.cbatstat
    laData[10] = glbatch.nbatotcr
    laData[11] = glbatch.nbatotdr          
    lnBalance = ABS(laData[11] - laData[10])
    lcBalncID = IIF(laData[11] > laData[10],"Cr",;
                IIF(laData[11] < laData[10],"Dr",""))

    IF laData[2] = 'U' .AND. lncbHold = 1
      cbHold = 1
      laData[2] = 'H'
    ELSE  
      cbHold = 0
    ENDIF

    lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
    lncbHold = cbHold 

*B601514,4 [Begin] 
    *B601514,1 M.H Begin.
    *IF _WINDOWS .OR. _MAC
    *  puBatType = IIF(laData[3] = 'N',1,2)
    *ENDIF
    *B601514,1 M.H End.
    IF _WINDOWS .OR. _MAC
      puBatType = IIF(laData[3] = 'N',1,IIF(laData[3] = 'S',2,3))
    ENDIF
*B601514,4 [End..] 
  ENDIF
ENDIF

SELECT GLBATCH
&& initialize the filter to glbatch
lcScope    = "cBatType $ 'NSL' .AND. cBatStat $ 'EOUHAP'"


SET FILTER TO
SET FILTER TO &lcScope

****
lcNewStat   = IIF(laScrMode[1] .OR. laScrMode[2],"ENABLE","DISABLE")
lcTrnStat   = IIF((laScrMode[2] .AND. laData[2] <> 'E') .OR. laScrMode[3],"ENABLE","DISABLE")
lcPrnStat   = IIF(laScrMode[2] .AND. laData[2]<> 'E',"ENABLE","DISABLE")
lcRevStat   = IIF((laScrMode[2] .OR. laScrMode[3]) .AND. laData[2]<> 'E',"ENABLE","DISABLE")
lcPosStat   = IIF(laScrMode[2] .AND. ;
                 (laData[2] ='U' .OR. (laData[2]='O' .AND. laData[3]='S')),"ENABLE","DISABLE")
lcHldStat   = IIF(laScrMode[3] .AND. laData[2] $ 'UH',"ENABLE","DISABLE" )
lcTypStat   = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[2] = 'E' ,"ENABLE","DISABLE")
lcColPair   = IIF(laScrMode[1] .OR. laScrMode[2] ,SCHEME(1,10),SCHEME(1,2))
lcObjStatus = IIF(laScrMode[2] .AND. laData[2] $ 'EOUZH',"ENABLE","DISABLE")
laCtrStat[7]= lcObjStatus
laCtrStat[8]= lcObjStatus
****

*E300683,5 Call *.SPR from screens directory
* DO Glbatch.SPR 
DO (gcScrDir + gcWinAppl + '\Glbatch.SPR')
*E300683,5 end   
SELECT GLBATCH

SELECT GLBATCH
SET FILTER TO

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

SELECT GLBATCH

IF laScrMode[2] AND lcBStamp <> DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
  && hold the audit information in lcBStamp from the current record.
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
ENDIF  

=lfCheckRec()  && check the current batch status.

DO CASE 
  CASE laScrMode [1]        && ----->  Select Mode
    llPeriodLock = .F.
    lcDumYear  = ''
    lcDumStart = ''
    lcDummyEnd = ''
    lcBStamp   = '' 
    lcStatus   = ''
    lcBatType  = laBatType[1,1] &&-- Default value .
    puBatType  = 1
    lcYear     = ''
    lcStartPrd = ''
    lcEndPrd   = ''
    lnBalance  = 0.00
    lcBalncID  = ''
    cbHold     = 0
    lncbHold   = cbHold 

    SHOW GET laData[1]   ENABLE

    *B601480,1 Enable the browse button of the Key Field [Begin] 
    SHOW GET ibBrowKey   ENABLE
    *B601480,1 Enable the browse button of the Key Field [End] 

    SHOW GET pbNew       ENABLE
    SHOW GET ibBatType   DISABLE
    SHOW GET puBatType   DISABLE
    SHOW GET laData[4]   DISABLE
    SHOW GET ibPostYr    DISABLE  
    SHOW GET pbPostYr    DISABLE        
    SHOW GET lcYear      DISABLE
    SHOW GET lcStartPrd  DISABLE
    SHOW GET laData[5]   DISABLE
    SHOW GET ibStartDt   DISABLE
    SHOW GET pbStartDt   DISABLE 
    SHOW GET lcEndPrd    DISABLE
    SHOW GET laData[6]   DISABLE
    SHOW GET ibEndDt     DISABLE
    SHOW GET pbEndDt     DISABLE      
    SHOW GET laData[11]  DISABLE  
    SHOW GET laData[10]  DISABLE
    SHOW GET lnBalance   DISABLE
    SHOW GET lcBalncId   DISABLE
    SHOW GET laData[7]   DISABLE
    SHOW GET cbHold      DISABLE
    SHOW GET laData[8]   DISABLE
    SHOW GET laData[9]   DISABLE
    SHOW GET pbTrans     DISABLE
    SHOW GET pbReview    DISABLE
    SHOW GET pbPrint     DISABLE
    SHOW GET pbPost      DISABLE
    SHOW GET pbDlt,1 &lcPrompt DISABLE
    lcObjStatus = 'DISABLE'
   
    *B600486,1 Control the status of the delete bar in the record pad.
    laCtrStat[8] = "DISABLE"
    
*************************************************

  CASE laScrMode[2] .OR. laScrMode[3]   && ----> View or Edit Mode
    *** IF RECORD CHANGED AND NOT VIEW MODE

    lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
    cbHold    = IIF(laData[2]='H',1,0)
    lncbHold  = cbHold 
    lnBalance = ABS(laData[11] - laData[10])
    lcBalncID = IIF(laData[11] > laData[10],"Cr",;
                IIF(laData[11] < laData[10],"Dr",""))

    lcYear    = la_TPostYr[IIF(laData[4]     =    lcCurr_yer   ,2,;
                        IIF(VAL(laData[4])=VAL(lcCurr_yer)-1,1,;
                        IIF(VAL(laData[4])=VAL(lcCurr_yer)+1,3,4)))]

    lnStartPrd = gfPeriod(laData[5],gcPrnt_Cmp)
    lcStartPrd = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
    lnEndPrd   = gfPeriod(laData[6],gcPrnt_Cmp)
    lcEndPrd   = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
    lcDumStart = lcStartPrd
    lcDummyEnd = lcEndPrd
    lcDumYear  = laData[4]

    *B601415,2 control the popup in case of view,edit [Begin]  
    *lcBatType  = IIF(laData[3]='N',laBatType[1,1],laBatType[2,1]) 
    *puBatType  = IIF(laData[3]='N',1,2)    
    lcBatType  = IIF(laData[3]='N',laBatType[1,1],IIF(laData[3]='S',laBatType[2,1],laBatType[3,1])) 
    puBatType  = IIF(laData[3]='N',1,IIF(laData[3]='S',2,3))     

*    IF laData[3]='L' &&-- commented due to B601415,4
*      lcBatType  = "Subledger"    commented due to B601415,4
*B601514,1 M.H Begin.
*      puBatType = "Subledger"
*      puBatType = 2  &&-- commented due to B601415,4
*B601514,1 M.H End.
*    ENDIF            &&-- commented due to B601415,4
    *B601415,2 control the popup in case of view,edit [End..]  

    *** View mode
    IF laScrMode[2]
      lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 

      SHOW GET laData[1]   DISABLE

      *B601480,1 Disable the browse button of the Key Field [Begin] 
      SHOW GET ibBrowKey   DISABLE
      *B601480,1 Disable the browse button of the Key Field [End] 

      SHOW GET pbNew       ENABLE
      SHOW GET ibBatType   DISABLE
      SHOW GET puBatType   DISABLE
      SHOW GET laData[4]   DISABLE
      SHOW GET ibPostYr    DISABLE  
      SHOW GET pbPostYr    DISABLE        
      SHOW GET lcYear      DISABLE
      SHOW GET lcStartPrd  DISABLE
      SHOW GET laData[5]   DISABLE
      SHOW GET ibStartDt   DISABLE
      SHOW GET pbStartDt   DISABLE 
      SHOW GET lcEndPrd    DISABLE
      SHOW GET laData[6]   DISABLE
      SHOW GET ibEndDt     DISABLE
      SHOW GET pbEndDt     DISABLE      
      SHOW GET laData[11]  DISABLE  
      SHOW GET laData[10]  DISABLE
      SHOW GET lnBalance   DISABLE
      SHOW GET lcBalncId   DISABLE
      SHOW GET laData[7]   DISABLE
      SHOW GET cbHold      DISABLE
      SHOW GET laData[8]   DISABLE
      SHOW GET laData[9]   DISABLE

     
      
      *** If empty batch dont go to transaction program
      IF laData[2]= "E" 
        SHOW GET pbTrans   DISABLE
        SHOW GET pbPrint   DISABLE
      ELSE
        SHOW GET pbTrans   ENABLE
        SHOW GET pbPrint   ENABLE
      ENDIF      


      IF laData[2] $ 'PA'
        lcObjStatus = 'DISABLE'
        SHOW GET pbDlt,1 &lcPrompt DISABLE
        *B600486,1 Control the status of the delete bar in the record pad.
        laCtrStat[8] = "DISABLE"
        
        SHOW GET pbCopy DISABLE
        SHOW GET pbEdt  DISABLE
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "DISABLE"
        
        SHOW GET pbPost DISABLE
      ELSE  
        lcObjStatus = 'ENABLE'
        SHOW GET pbDlt,1 &lcPrompt ENABLE
        *B600486,1 Control the status of the delete bar in the record pad.
        laCtrStat[8] = "ENABLE"

        SHOW GET pbCopy ENABLE
        SHOW GET pbEdt  ENABLE
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "ENABLE"
        
        IF (laData[2]= "U") .OR. (laData[2]= "O" .AND. laData[3]= "S")
          SHOW GET pbPost  ENABLE
        ELSE  
          SHOW GET pbPost  DISABLE    
        ENDIF
      ENDIF    

      IF laData[3]='L'
        SHOW GET pbEdt  DISABLE
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "DISABLE"
      ENDIF

*********************************************

    ELSE       && ----->  Edit mode
      SHOW GET laData[1]   DISABLE

      *B601480,1 Disable the browse button of the Key Field [Begin] 
      SHOW GET ibBrowKey   DISABLE
      *B601480,1 Disable the browse button of the Key Field [End] 

      SHOW GET pbNew       DISABLE

      IF laData[2] = "E" 
        SHOW GET ibBatType   ENABLE
        SHOW GET puBatType   ENABLE    
        SHOW GET laData[4]   ENABLE
        SHOW GET ibPostYr    ENABLE
        SHOW GET pbPostYr    ENABLE        
        SHOW GET lcYear      ENABLE        
        SHOW GET ibStartDt   ENABLE
        SHOW GET pbStartDt   ENABLE 
        SHOW GET ibEndDt     ENABLE
        SHOW GET pbEndDt     ENABLE  

        *** check if the program working with periods format or dates format.
        IF llDate 
          SHOW GET laData[5]  ENABLE
          SHOW GET laData[6]  ENABLE
        ELSE
          SHOW GET laData[5]  DISABLE
          SHOW GET laData[6]  DISABLE
          SHOW GET lcStartPrd ENABLE         
          SHOW GET lcEndPrd   ENABLE         
        ENDIF
      ELSE
        SHOW GET ibBatType   DISABLE
        SHOW GET puBatType   DISABLE    
        SHOW GET laData[4]   DISABLE
        SHOW GET ibPostYr    DISABLE
        SHOW GET pbPostYr    DISABLE        
        SHOW GET lcYear      DISABLE        
        SHOW GET ibStartDt   DISABLE
        SHOW GET pbStartDt   DISABLE 
        SHOW GET ibEndDt     DISABLE
        SHOW GET pbEndDt     DISABLE  
        SHOW GET laData[5]   DISABLE
        SHOW GET laData[6]   DISABLE
        SHOW GET lcStartPrd  DISABLE         
        SHOW GET lcEndPrd    DISABLE         
      ENDIF  

      SHOW GET laData[11]  ENABLE
      SHOW GET laData[10]  ENABLE
      SHOW GET lnBalance   ENABLE
      SHOW GET lcBalncId   ENABLE
      SHOW GET laData[7]   ENABLE

      *** If batch is unposted or holded enable the hold check box
      IF laData[2] $ 'UH' 
        SHOW GET cbHold  ENABLE  
      ELSE  
        SHOW GET cbHold  DISABLE
      ENDIF
      lncbHold   = cbHold
      
      SHOW GET laData[8]   ENABLE
      SHOW GET laData[9]   ENABLE
      SHOW GET pbTrans     ENABLE
      SHOW GET pbPrint     DISABLE
      SHOW GET pbPost      DISABLE
      SHOW GET pbDlt,1 &lcPrompt DISABLE
      lcObjStatus = 'DISABLE'
      *B600486,1 Control the status of the delete bar in the record pad.
      laCtrStat[8] = "DISABLE"
    ENDIF

    *** If empty batch dont go to review or print
    IF laData[2]= "E" 
      SHOW GET pbReview  DISABLE
    ELSE
      SHOW GET pbReview  ENABLE    
    ENDIF  

**************************************************************

  CASE laScrMode [4]       && ----->  Add Mode
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *SELECT  SYCFSPRD
    SELECT  FSPRD
    *E300692,1 end
    *SEEK (gcAct_Comp+lcCurr_Yer+lcCurr_Prd)
    SET ORDER TO COMFYRPRDI
    *B602733,1 Do not seek company ID
    *SEEK (gcPrnt_Cmp+lcCurr_Yer+lcCurr_Prd)
    SEEK (lcCurr_Yer+lcCurr_Prd)
    *B602733,1 end
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *laData[5] = SYCFSPRD.dFsPpBgDt
    *laData[6] = SYCFSPRD.dFsPpEnDt
    laData[5] = FSPRD.dFsPpBgDt
    laData[6] = FSPRD.dFsPpEnDt
  *E300692,1 end
    SELECT GLBATCH
    
    lcStartPrd = lcCurr_Prd
    lcEndPrd   = lcCurr_Prd
    lcDumStart = lcStartPrd
    lcDummyEnd = lcEndPrd
    laData[1]  = ''
    laData[2]  = 'E'
    
    laData[3]  = laBatType[1,2] &&-- Default value in add mode
    lcBatType  = laBatType[1,1] &&-- Default value in add mode
   
    lcStatus   = laTStatus[1]
    puBatType  = 1
    laData[4]  = lcCurr_Yer
    lcDumYear  = laData[4]
    lcYear     = la_TPostYr [2]
    laData[11] = 0.00
    laData[10] = 0.00
    lnBalance  = 0.00
    lcBalncID  = ''
    laData[7]  = 0.00
    cbHold     = 0
    laData[8]  = "On " + IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                         LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))

    laData[9]  = SUBSTR("Created by " + LOOKUP(SYUUSER.cUsr_Name,gcUser_Id,SYUUSER.cUser_Id),1,40)

    laData[12] = gcAct_Appl
    laData[13] = gcAct_Comp
    

    SHOW GET laData[1]   DISABLE

    *B601480,1 Disable the browse button of the Key Field [Begin] 
    SHOW GET ibBrowKey   DISABLE
    *B601480,1 Disable the browse button of the Key Field [End] 

    SHOW GET pbNew       DISABLE
    SHOW GET ibBatType   ENABLE
    SHOW GET puBatType   ENABLE    
    SHOW GET laData[4]   ENABLE
    SHOW GET ibPostYr    ENABLE
    SHOW GET pbPostYr    ENABLE        
    SHOW GET lcYear      ENABLE        
    SHOW GET lcStartPrd  ENABLE
    SHOW GET ibStartDt   ENABLE
    SHOW GET pbStartDt   ENABLE 
    SHOW GET lcEndPrd    ENABLE
    SHOW GET ibEndDt     ENABLE
    SHOW GET pbEndDt     ENABLE  
    
    IF llDate 
      SHOW GET laData[5]  ENABLE
      SHOW GET laData[6]  ENABLE
    ELSE
      SHOW GET laData[5]  DISABLE
      SHOW GET laData[6]  DISABLE
    ENDIF

    SHOW GET laData[11]  ENABLE
    SHOW GET laData[10]  ENABLE
    SHOW GET lnBalance   ENABLE
    SHOW GET lcBalncId   ENABLE
    SHOW GET laData[7]   ENABLE
    SHOW GET cbHold      DISABLE
    SHOW GET laData[8]   ENABLE
    SHOW GET laData[9]   ENABLE
    SHOW GET pbTrans     DISABLE
    SHOW GET pbReview    DISABLE
    SHOW GET pbPrint     DISABLE
    SHOW GET pbPost      DISABLE
    lncbHold   = cbHold
    SHOW GET pbDlt,1 &lcPrompt DISABLE
    lcObjStatus = 'DISABLE'    
    *B600486,1 Control the status of the delete bar in the record pad.
    laCtrStat[8] = "DISABLE"
ENDCASE

lcColPair= IIF(laScrMode[1] .OR. laScrMode[2] ,SCHEME(1,10),SCHEME(1,2))
SELECT GLBATCH


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()= 13)

  IF LEFT(laData[1],1) <> '?' .AND. !llBrowse
    laData[1] = RIGHT("000000"+ALLTRIM(laData[1]),6)
    SHOW GET laData[1]
  ENDIF  
  
  IF RECNO() <= RECCOUNT() .AND. ! EOF()
    GOTO RECNO()  
  ENDIF  
  =gfSeekRec()

  IF laScrMode[4]    && add mode
   laData[1] = "      "
   laData[2] = 'E'
   SHOW GET laData[1]
  ELSE
    lnStartPrd = gfPeriod(laData[5],gcPrnt_Cmp)
    lcStartPrd = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
    lnEndPrd   = gfPeriod(laData[6],gcPrnt_Cmp)
    lcEndPrd   = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
    SHOW GET lcStartPrd DISABLE             
    SHOW GET lcEndPrd   DISABLE                
  ENDIF
ENDIF

llBrowse = .F. 

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7
*** check the value of audit total

IF SIGN(laData[7]) = -1
  * Negative values are not allowed.
  * < OK >
  =gfModalGen("TRM02036B00000","DIALOG")
  laData[7] = 0.00
  _CUROBJ = OBJNUM(laData[7])
ENDIF 

*!**************************************************************************
*!
*!      Function: lfwPostyr
*!
*!**************************************************************************
*
FUNCTION lfwPostyr

lcOldYear  = laData[4]

*!**************************************************************************
*!
*!      Function: lfvPostyr
*!
*!**************************************************************************
*
FUNCTION lfvPostyr

IF  LASTKEY() <> 13 .AND. _CUROBJ = OBJNUM(laData[4])
  laData[4] = lcOldYear
  RETURN
ENDIF

lcOYear = laData[4]

IF _CUROBJ = OBJNUM(laData[4])  && in case current object is year field.
  *** check if the entered year falls in the posting window.
  IF (ASCAN(laFisYear,laData[4]) = 0) .OR. (EMPTY(laData[4]))
    *** in case the enterd year not falls in the posting window
    *** or ladata[4] is empty, in this case activate popup.
    DO CASE
      CASE _DOS
        laData[4] =SUBSTR(gfActPop(4,17,9 ,35,'laFisYear',1,1,@lcDumYear),1,4)
      CASE _WINDOWS   
        laData[4] =SUBSTR(gfActPop(12.63,12.27,16.58,30.58,'laFisYear',1,1,@lcDumYear),1,4)
    ENDCASE
    
    IF lcOYear <> laData[4]
      lcYear = la_TPostYr[IIF(laData[4]     =    lcCurr_yer   ,2,;
                          IIF(VAL(laData[4])=VAL(lcCurr_yer)-1,1,;
                          IIF(VAL(laData[4])=VAL(lcCurr_yer)+1,3,4)))]
    ENDIF   
  ELSE
    lcDumYear = laFisYear[ASCAN(laFisYear,laData[4])]  
  ENDIF

  *** check if the program working with periods format or dates format.
  IF llDate 
    _CUROBJ = OBJNUM(laData[5])
  ELSE
    _CUROBJ = OBJNUM(lcStartPrd)
  ENDIF  

ELSE    && in case current object is popup field.
  DO CASE
    CASE _DOS
      laData[4] =SUBSTR(gfActPop(4,17,9 ,35,'laFisYear',1,1,@lcDumYear),1,4)
    CASE _WINDOWS  
      laData[4] =SUBSTR(gfActPop(12.63,12.27,16.58,30.58,'laFisYear',1,1,@lcDumYear),1,4)
  ENDCASE
  IF lcOYear <> laData[4]
    lcYear = la_TPostYr[IIF(laData[4]     =    lcCurr_yer   ,2,;
                        IIF(VAL(laData[4])=VAL(lcCurr_yer)-1,1,;
                        IIF(VAL(laData[4])=VAL(lcCurr_yer)+1,3,4)))]
  ENDIF
ENDIF

SHOW GET lcYear 
SHOW GET laData[4]

*** check if the program working with periods format or dates format.
IF llDate
  lcStartPrd  =  "01"
ELSE

  ***  Check if the lcStartPrd falls in the selected year.
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *SELECT  SYCFSPRD
  SELECT  FSPRD
  *E300692,1 end
  *SEEK (gcAct_Comp+laData[4]+lcStartPrd)
  SET ORDER TO TAG COMFYRPRDI
  *B602733,1 Do not seek company ID
  *SEEK (gcPrnt_Cmp+laData[4]+lcStartPrd)
  SEEK (laData[4]+lcStartPrd)
  *B602733,1 end
  IF FOUND()
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *laData[5] = SYCFSPRD.dFsPpBgDt
    laData[5] = FSPRD.dFsPpBgDt
  *E300692,1 end
    SHOW GET laData[5]
  ELSE
    * Period ð does not exist in fiscal year ð .
    * < OK >
    =gfModalGen("TRM02156B00000","DIALOG",lcStartPrd+'|'+laData[4])
    IF _DOS
      _CUROBJ = OBJNUM(ibStartDt)    
      KEYBOARD '{ENTER}'
    ENDIF  

    IF _WINDOWS
      _CUROBJ = OBJNUM(pbStartDt)    
      KEYBOARD '{ENTER}'
    ENDIF  

    SELECT GLBATCH
    RETURN
  ENDIF
    
  ***  Check if the lcEndPrd falls in the selected year.    
  *SEEK (gcAct_Comp+laData[4]+lcEndPrd)  
  *B602733,1 Do not seek company ID
  *SEEK (gcPrnt_Cmp+laData[4]+lcEndPrd)
  SEEK (laData[4]+lcEndPrd)
  *B602733,1 end
  IF FOUND()
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *laData[6] = SYCFSPRD.dFsPpEnDt
    laData[6] = FSPRD.dFsPpEnDt
  *E300692,1 end
    SHOW GET laData[6]
  ELSE
    * Period ð does not exist in fiscal year ð .
    * < OK >
    =gfModalGen("TRM02156B00000","DIALOG",lcEndPrd+'|'+laData[4])
    _CUROBJ = OBJNUM(lcEndPrd)    
    SELECT GLBATCH
    RETURN
  ENDIF
ENDIF  

SELECT GLBATCH


*!**************************************************************************
*!
*!      Function: lfvSDatPrd
*!
*!**************************************************************************
*
FUNCTION lfvSDatPrd

*** check if the program working with periods format or dates format.
IF llDate 
  =lfvStaDate()
ELSE
  =lfvStaPrd()
ENDIF


*!**************************************************************************
*!
*!      Function: lfvEDatPrd
*!
*!**************************************************************************
*
FUNCTION lfvEDatPrd

*** check if the program working with periods format or dates format.
IF llDate 
  =lfvEndDate()
ELSE
  =lfvEndPrd()
ENDIF


*!**************************************************************************
*!
*!      Function: lfFillAry1
*!
*!**************************************************************************
*
FUNCTION lfFillAry1

lnCentury = IIF(SET("CENTURY")='ON',2,0)

*** select all periods id, begin date, end date, lock status
*** into array laselect
*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcSYSHOME.SYCFSPRD.DBF;
    wHERE Ccomp_id + Cfisfyear = gcAct_Comp + laData[4] ;
    ORDER BY SYCFSPRD.Cfspprdid;

*E300692,1 Change file name and path from sycfsprd to fsprd
*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcSysHome.sycFSPRD.DBF;
    WHERE Ccomp_id + Cfisfyear = gcPrnt_Cmp + laData[4] ;
    ORDER BY sycFSPRD.Cfspprdid;
    INTO  ARRAY laSelect
*E300789,7 [BEGIN]
*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE Ccomp_id + Cfisfyear = gcPrnt_Cmp + laData[4] ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY laSelect
SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE  Cfisfyear =  laData[4] ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY laSelect
*E300789,7 [END..]        
*E300692,1 end
*!**************************************************************************
*!
*!      Function: lfFillAry2
*!
*!**************************************************************************
*
FUNCTION lfFillAry2

lnCentury = IIF(SET("CENTURY")='ON',2,0)

*** select all periods id, begin date, end date, lock status
*** where cFspPrdId >= lcStartPrd into array laselect 

*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcSYSHOME.SYCFSPRD.DBF;
    WHERE Ccomp_id + Cfisfyear = gcAct_Comp + laData[4] ;
    .AND. cFspPrdId >= lcStartPrd ;
    ORDER BY SYCFSPRD.Cfspprdid;
    INTO  ARRAY laSelect

*E300692,1 Change file name AND PATH from sycfspro to fspro
*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcSYSHOME.SYCFSPRD.DBF;
    WHERE Ccomp_id + Cfisfyear = gcPrnt_Cmp + laData[4] ;
    .AND. cFspPrdId >= lcStartPrd ;
    ORDER BY SYCFSPRD.Cfspprdid;
    INTO  ARRAY laSelect

*E300789,7 [BEGIN]    
*SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE Ccomp_id + Cfisfyear = gcPrnt_Cmp + laData[4] ;
    .AND. cFspPrdId >= lcStartPrd ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY laSelect
SELECT Cfspprdid+" ³ "+Cfisfyear+" ³ "+DTOC(Dfsppbgdt)+" ³ "+;
    DTOC(dfsppendt)+" ³ "+ IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE  Cfisfyear =  laData[4] ;
    .AND. cFspPrdId >= lcStartPrd ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY laSelect
*E300789,7 [END..]    
*E300692,1 end    


*!**************************************************************************
*!
*!      Function: lfwStaDate
*!
*!**************************************************************************
*
FUNCTION lfwStaDate
ldOldStDat  = laData[5]

*!**************************************************************************
*!
*!      Function: lfvStaDate
*!
*!**************************************************************************
*
FUNCTION lfvStaDate

*B602859,1 AMM define the variable as private
PRIVATE lcSExact
*B602859,1 AMM END

IF  LASTKEY() <> 13 .AND. _CUROBJ = OBJNUM(laData[5])
  laData[5] = ldOldStDat
  RETURN
ENDIF

*B602859,1 AMM start, Set exact off
lcSExact = SET('EXACT')
SET EXACT OFF
*B602859,1 AMM END

=lfFillAry1()

IF LEN(lcDumStart) < 4   && first time 
  lcDumStart = laSelect[1]
ENDIF

IF _CUROBJ = OBJNUM(laData[5])   && check if the current object is 
                                 && start date get filed.

  *** check if the start date falls between the start date of the first
  *** period and the end date of the last period.

  IF !BETWEEN(laData[5],;
     CTOD(SUBSTR(laSelect[1],13,8+lnCentury)),;
     CTOD(SUBSTR(laSelect[ALEN(laSelect)],24+lnCentury,8+lnCentury)))

    *** Activate popup list
    DO CASE
      CASE _DOS
        =gfActPop(5,19,7+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDumStart)
      CASE _WINDOWS  
        =gfActPop(15.27,12.27,16.27+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDumStart)
    ENDCASE

    lnPosition = ASCAN(laSelect,lcDumStart)  && in case press Esc
                                             && and previous value is invalid
    IF lnPosition = 0 
      lcDumStart = laSelect[1]
    ENDIF

    laData[5]  = CTOD(SUBSTR(lcDumStart,13,8+lnCentury))
    lcStartPrd = SUBSTR(lcDumStart,1 ,2 )
  ELSE 
    lnStartPrd  =gfPeriod(laData[5],gcPrnt_Cmp)
    lcStartPrd  = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
    lcDumStart = laSelect[ASCAN(laSelect,lcStartPrd)]
  ENDIF

  _CUROBJ = OBJNUM(laData[6])

ELSE
  *** Activate popup list
  DO CASE
    CASE _DOS
       =gfActPop(5,19,7+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDumStart)
    CASE _WINDOWS  
       =gfActPop(15.27,12.27,16.27+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDumStart)
  ENDCASE

  lnPosition = ASCAN(laSelect,lcDumStart)  && in case press Esc
                                           && and previous value is invalid
  IF lnPosition = 0 
    lcDumStart = laSelect[1]
  ENDIF

  laData[5]  = CTOD(SUBSTR(lcDumStart,13,8+lnCentury))
  lcStartPrd = SUBSTR(lcDumStart,1 ,2 )
ENDIF    
*B602859,1 AMM start, restore old value of exact
SET EXACT &lcSExact
*B602859,1 AMM end

SHOW GET laData[5]   ENABLE


*!**************************************************************************
*!
*!      Function: lfwEndDate
*!
*!**************************************************************************
*
FUNCTION lfwEndDate
ldOldEnDat   = laData[6]


*!**************************************************************************
*!
*!      Function: lfvEndDate
*!
*!**************************************************************************
*
FUNCTION lfvEndDate
*B602859,1 AMM define the variable as private
PRIVATE lcSExact
*B602859,1 AMM END

IF  LASTKEY() <> 13 .AND. _CUROBJ = OBJNUM(laData[6]) 
  laData[6] = ldOldEnDat
  RETURN
ENDIF

*B602859,1 AMM start, Set exact off
lcSExact = SET('EXACT')
SET EXACT OFF
*B602859,1 AMM END

=lfFillAry2()

IF LEN(lcDummyEnd) < 4  && first time
  lcDummyEnd = laSelect[1]
ENDIF
  
IF _CUROBJ = OBJNUM(laData[6])
  *** check if the end date falls between the start date of the first
  *** period (start date) and the end date of the last period.
  IF !BETWEEN(laData[6],;
    CTOD(SUBSTR(laSelect[1],13,8+lnCentury)),;
    CTOD(SUBSTR(laSelect[ALEN(laSelect)],24+lnCentury,8+lnCentury)))  

    *** Activate popup list
    DO CASE
      CASE _DOS
        =gfActPop(6,19,8+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
      CASE _WINDOWS  
        =gfActPop(17.72,12.27,18.72+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
    ENDCASE

    lnPosition = ASCAN(laSelect,lcDummyEnd)  && in case press Esc
                          					 && and previous value is invalid
    IF lnPosition = 0 
      lcDummyEnd = laSelect[1]
    ENDIF
    
    laData[6]  = CTOD(SUBSTR(lcDummyEnd,24+lnCentury,8+lnCentury))
    lcEndPrd   = SUBSTR(lcDummyEnd,1,2)
  ELSE
    lnEndPrd =gfPeriod(laData[6],gcPrnt_Cmp)
    lcEndPrd = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
    lcDummyEnd = laSelect[ASCAN(laSelect,lcEndPrd)]
  ENDIF
  _CUROBJ = OBJNUM(laData[7])

ELSE

  *** Activate popup list
  DO CASE
    CASE _DOS
      =gfActPop(6,19,8+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
    CASE _WINDOWS  
      =gfActPop(17.72,12.27,18.72+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
  ENDCASE

  lnPosition = ASCAN(laSelect,lcDummyEnd)  && in case press Esc
                                           && and previous value is invalid
  IF lnPosition = 0 
    lcDummyEnd = laSelect[1]
  ENDIF

  laData[6]  = CTOD(SUBSTR(lcDummyEnd,24+lnCentury,8+lnCentury))
  lcEndPrd   = SUBSTR(lcDummyEnd,1,2)
ENDIF  

*B602859,1 AMM start, restore old value of exact setting
SET EXACT &lcSExact
*B602859,1 AMM end

SHOW GET laData[6]   ENABLE


*!**************************************************************************
*!
*!      Function: lfwStaPrd
*!
*!**************************************************************************
*
FUNCTION lfwStaPrd

lcOldStPrd = lcStartPrd


*!**************************************************************************
*!
*!      Function: lfvStaPrd
*!
*!**************************************************************************
*
FUNCTION lfvStaPrd

*B602859,1 AMM define the variable as private
PRIVATE lcSExact
*B602859,1 AMM end
IF  LASTKEY() <> 13 .AND. _CUROBJ = OBJNUM(lcStartPrd)
  lcStartPrd = lcOldStPrd
  RETURN
ENDIF

=lfFillAry1()

IF _CUROBJ = OBJNUM(lcStartPrd)
  lcStartPrd = RIGHT("0"+ALLTRIM(lcStartPrd),2)

  *** check if the start period falls between the first
  *** period and the last period.

  IF !BETWEEN(lcStartPrd,SUBSTR(laSelect[1],1,2),;
                         SUBSTR(laSelect[ALEN(laSelect)],1,2))
    *** Activate popup list
    DO CASE
      CASE _DOS
        =gfActPop(5,19,7+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDumStart)
      CASE _WINDOWS  
        =gfActPop(15.27,12.27,16.27+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDumStart)
    ENDCASE
  
    lcStartPrd = SUBSTR(lcDumStart,1 ,2 )
  ENDIF

    _CUROBJ = OBJNUM(lcEndPrd)
ELSE

  *** Activate popup list
  DO CASE
    CASE _DOS
      =gfActPop(5,19,7+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDumStart)
    CASE _WINDOWS  
      =gfActPop(15.27,12.27,16.27+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDumStart)
  ENDCASE

  lcStartPrd = SUBSTR(lcDumStart,1 ,2 )
 
ENDIF    

*B602859,1 AMM start, Set exact off
lcSExact = SET('EXACT')
SET EXACT OFF
*B602859,1 AMM END
lnPosition = ASCAN(laSelect,lcStartPrd)
*B602859,1 AMM start, restore old value of exact
SET EXACT &lcSExact
*B602859,1 AMM end
IF lnPosition = 0
  lcDumStart = laSelect[1]
  lcStartPrd = SUBSTR(lcDumStart,1 ,2 )
ELSE
  lcDumStart = laSelect[lnPosition]
ENDIF

laData[5]  = CTOD(SUBSTR(lcDumStart,13,8+lnCentury))

SHOW GET laData[5]   DISABLE
SHOW GET lcStartPrd  ENABLE

*!**************************************************************************
*!
*!      Function: lfwEndPrd
*!
*!**************************************************************************
*
FUNCTION lfwEndPrd

lcOldEnPrd  = lcEndPrd


*!**************************************************************************
*!
*!      Function: lfvEndPrd
*!
*!**************************************************************************
*
FUNCTION lfvEndPrd

*B602859,1 AMM Define the variable as private
PRIVATE lcSExact
*B602859,1 AMM end

IF  LASTKEY() <> 13 .AND. _CUROBJ = OBJNUM(lcEndPrd)
  lcEndPrd = lcOldEnPrd 
  RETURN
ENDIF

=lfFillAry2()
  
IF _CUROBJ = OBJNUM(lcEndPrd)
  lcEndPrd = RIGHT("0"+ALLTRIM(lcEndPrd),2)

  *** check if the end period falls between the first
  *** period ( start period value ) and the last period.

  IF !BETWEEN(lcEndPrd,SUBSTR(laSelect[1],1,2),;
                       SUBSTR(laSelect[ALEN(laSelect)],1,2))  

  *** Activate popup list
  DO CASE
    CASE _DOS
      =gfActPop(6,19,8+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
    CASE _WINDOWS  
      =gfActPop(17.72,12.27,18.72+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
  ENDCASE

  lcEndPrd   = SUBSTR(lcDummyEnd,1,2)

  ENDIF
    _CUROBJ = OBJNUM(laData[7])
ELSE

  *** Activate popup list
  DO CASE
    CASE _DOS
      =gfActPop(6,19,8+ALEN(laSelect),64+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
    CASE _WINDOWS  
      =gfActPop(17.72,12.27,18.72+(ALEN(laSelect)*1.1),56.5+(lnCentury*2),'laSelect',1,1,@lcDummyEnd)
  ENDCASE

  lcEndPrd   = SUBSTR(lcDummyEnd,1,2)
ENDIF  
*B602859,1 AMM start, set exact off
lcSExact = SET('EXACT')
SET EXACT OFF
*B602859,1 AMM END
lnPosition = ASCAN(laSelect,lcEndPrd)
*B602859,1 AMM start, restore old value of set exact
SET EXACT &lcSExact
*B602859,1 AMM end

IF lnPosition = 0
  lcDummyEnd = laSelect[1]
  lcEndPrd   = SUBSTR(lcDummyEnd,1 ,2 )
ELSE
  lcDummyEnd = laSelect[lnPosition]
ENDIF

laData[6]  = CTOD(SUBSTR(lcDummyEnd,24+lnCentury,8+lnCentury))

SHOW GET lcEndPrd    ENABLE
SHOW GET laData[6]   DISABLE


*!**************************************************************************
*!
*!      Function: lfRevShow
*!
*!**************************************************************************
*
FUNCTION lfRevShow

*** check if the batch type is 'N' ----> normal and the batch status $ 'PA'
*** 'Posted' , 'Approved' in this case disable the 'Out Of Balance' ,'All'

IF laData[3] = 'N' .AND. laData[2] $ 'PA'
  SHOW GET rbCriteria,2 DISABLE
  SHOW GET rbCriteria,3 DISABLE
  rbCriteria = 1
ENDIF

*** check if the batch type is 'S' ----> statistical and the batch 
*** status = 'P' ---> 'posted' in this case disable the 'Out Of Balance' 
*** ,'All'

IF laData[3] = 'S' .AND. laData[2] $ 'P'
  SHOW GET rbCriteria,2 DISABLE
  SHOW GET rbCriteria,3 DISABLE
  rbCriteria = 1
ENDIF


*!**************************************************************************
*!
*!      Function: lfvReview
*!
*!**************************************************************************
*
FUNCTION lfvReview
DECLARE laTdata [1]

IF ! lfCheckRec()   && check the current batch status.
  RETURN
ENDIF  

laTdata  = " "
llOk     = .F.

*E300683,5 Call *.SPR from screens directory
* DO GLReview.SPR 
DO (gcScrDir + gcWinAppl + '\GLReview.SPR')
*E300683,5 end   
IF llOK                       && Push Button OK has been Pressed

  llOk      = .F.
  lcBFields = lcBrfields
  lcOldTit  = lcFile_Ttl

  SELECT GLTRNSHD
  SET FILTER TO

  *** set the filter according to selected criteria.
  IF rbSort = 1
    SET ORDER TO TAG BATCHTRN
  ELSE
    SET ORDER TO TAG POSTDATE
  ENDIF

  IF laData[2] $ 'PA'
    SET FILTER TO  cTrnStat <> 'V' .AND. cBatchNo = laData[1]
  ELSE
    DO CASE
      CASE rbCriteria = 1
        SET FILTER TO cTrnStat = 'U'  .AND. cBatchNo = laData[1]
     
      CASE rbCriteria = 2
        SET FILTER TO cTrnStat = 'O'  .AND. cBatchNo = laData[1]
 
      CASE rbCriteria = 3
        SET FILTER TO cTrnStat $ 'UO' .AND. cBatchNo = laData[1]
    ENDCASE
  ENDIF

  laAceptRec[1] = ""

* "cStat=laTStatus [AT(cTrnStat,'EOUPZHA')]" +

  IF rbType  = 1 
    lcFile_Ttl = 'Transactions'
    lcBrfields = "cTranNo:H='Trans. No.',"                   +;
                 "dTrnPDate:H='Trans. Date',"                +;
                 "cTrnStat:H='Status':15,"                   +;
                 "nTrnTotDr:H='Total Debit',"                +;
                 "nTrnTotCr:H='Total Credit',"               +;
                 "nBal=ABS(nTrnTotCr-nTrnTotDr):H='Balance'"

    =gfBrows(.F.,"cTranNo","laAceptRec")
  ELSE

    SET RELATION TO gltrnshd.cbatchno+gltrnshd.cTranNo INTO GLTRNSDT ADDITIVE

    lcFile_Ttl = 'Transactions details'
    lcBrfields = "GLTRNSHD.cTranno:H='Transaction #',GLTRNSDT.cAcctcode:H=lcAcsegDes,"   +;
             "nDebit =IIF(GLTRNSDT.cDrorcr='D',GLTRNSDT.nAmount,0):H='Debit ':15," +;
             "nCredit=IIF(GLTRNSDT.cDrorcr='C',GLTRNSDT.nAmount,0):H='Credit':15," +;
             "cDesc=LOOKUP(GLACCHAR.cAccnldes,GLTRNSDT.cAcctcode," +;
             "GLACCHAR.cAcctcode,'ACCTCODE'):H='Description'"

    SET SKIP TO GLTRNSDT
    =gfBrows(.F.,"cTranNo","laAceptRec")    
    SET RELATION TO
  ENDIF    


  SET FILTER TO
  SELECT GLBATCH 
  lcBrfields = lcBFields 
  lcFile_Ttl = lcOldTit 
  
  *** if there is a record selected from browse window.
  *** calling transaction program.
  
  IF !EMPTY(laAceptRec)
    lcMode   = IIF(laScrMode[2],'T','F')
    lcTrOrEn = IIF(rbType = 1,'T','E') 
    lnTrnHdRec = STR(RECNO('GLTRNSHD'))
    lnTrnDtRec = STR(RECNO('GLTRNSDT'))

    *E300683,5 Add parameters to gpDoprog
    *DO gpDoProg WITH 'AWRGLTRANS WITH "&laData[1]","&lnTrnHdRec","&lnTrnDtRec","&lcMode","&lcTrOrEn"'," ",'GL'
    DO gpDoProg WITH 'AWRGLTRANS', .F., 'GL', ;
      "'" + laData[1] + "','" + lnTrnHdRec + "','"  + lnTrnDtRec + ;
      "','" + lcMode    + "','" + lcTrOrEn   + "'"
    *E300683,5 end
  ENDIF
ENDIF
   
    
*!**************************************************************************
*!
*!      Function: lfvPrint
*!
*!**************************************************************************
*
FUNCTION lfvPrint

IF ! lfCheckRec()    && check the current batch status.
  RETURN
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost

IF ! lfCheckRec()    && check the current batch status.
  RETURN
ENDIF  

*** check if the batch year falls in posting window.

lcExactSet = SET("EXACT")
SET EXACT OFF

IF ASCAN(laFisYear,glBatch.cbatpyr) <> 0
  
  *** check if the user print the batch after last updating.
  IF !EMPTY(glBatch.cBatElUsr) 
    llEditList= IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
                IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                      glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
  ELSE
    llEditList = .F. 
  ENDIF    

  *** check the force printing flag in glsetup
  llPrint = IIF (glSetup.lSetForel,;
            IIF (llEditList,.T.,.F.),.T.)
            
  IF llPrint 
    * Are you sure you want to post this ð ?
    * <Post> <No>
    IF gfModalGen("TRM02149B02009","DIALOG",'batch') = 1 
      IF ! lfCheckRec()    && check the current batch status.
        SET EXACT &lcExactSet
        RETURN
      ENDIF  
      
      SELECT GLBATCH 
      IF gfObj_Lock(.T.)     && check the lock status.
        IF ! lfCheckRec()    && check the current batch status.
          SET EXACT &lcExactSet
          RETURN
        ENDIF  
        *** calling posting program.
        IF lfTBPost('BATCH','','NonBeginning') <> 0 
          SCATTER FIELDS &lcScFields MEMO TO laData 
          *** update the audit information in the variable lcBStamp.
          lcBStamp      = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
          laScrMode     = .F.
          laScrMode [2] = .T.   
          SHOW GETS   
        ENDIF
        =gfObj_Lock(.F.)  
      ENDIF

    ENDIF
  ELSE
    * Printing the edit list is required to post this batch.
    * < OK > 
    =gfModalGen("TRM02148B00000","Dialog")  &&  Printing the edit list
                                        &&  is a must to post.
  ENDIF  
ELSE   
  * The ð posting year is out of the posting window.  You cannot post this ð.
  * < OK >
  =gfModalGen("TRM02147B00000","Dialog",'batch|batch') 
                                        &&  The Batch posting year is
                                        &&  out of the posting window.    
ENDIF  

SET EXACT &lcExactSet


*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
FUNCTION lfvHold
lncbHold = cbHold 

IF cbHold = 1
  laData[2]  = 'H'
  lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
 ELSE
  laData[2]  = 'U'
  lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
ENDIF

DO lfRefresh    && refresh says filed on the screen
  

*!**************************************************************************
*!
*!      Function: lfvBatType
*!
*!**************************************************************************
*
FUNCTION lfvBatType

*** select batch type ( normal or statistical )
*** select batch type ( normal or statistical or Subledger )
DO CASE
  CASE _DOS 
    laData[3] =gfActPop(1,57,5,71,'laBatType',2,1,@lcBatType)
  CASE _WINDOWS

    *B601514,4 we have to modify this line since Statistical & Subledger [Begin] 
    *          will both give 'S'
    *laData[3] = LEFT(laBatType[puBatType,1],1)
    laData[3] = IIF(puBatType=1,'N',IIF(puBatType=2,'S','L')) 
    *B601514,4 we have to modify this line since Statistical & Subledger [End..]     

ENDCASE

*B601514,4 [Begin]
*lcBatType = IIF(laData[3]='N',laBatType[1,1],laBatType[2,1])
lcBatType = IIF(laData[3]='N',laBatType[1,1],IIF(laData[3]='S',laBatType[2,1],laBatType[3,1]))
*B601514,4 [End..]

DO lfRefresh    && refresh says filed on the screen

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

*** to add new batch number

IF laScrMode [2]
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF  

laData[1]    = "" 
laScrMode    = .F.
laScrMode[4] = .T.
SHOW GETS

  
*!**************************************************************************
*!
*!      Function: lfvTrans
*!
*!**************************************************************************
*
FUNCTION lfvTrans

*** branch to the transation screen
*** in case of the batch status was not changed or batch start/end dates
*** was not changed.

*** check year , start (date or period) and end (date or period)
IF laData[4] <> glBatch.cBatpYr  .OR. ;
   laData[5] <> glBatch.dBatpBeg .OR. ;
   laData[6] <> glBatch.dBatpEnd 

  * Batch Start/End date is changed, you have to save first..!
  * < OK >
  =gfModalGen("TRM02180B00000","DIALOG")
  _CUROBJ=OBJNUM(pbSav)
  RETURN
ENDIF   

*** check batch type 

IF laData[3] <> glBatch.cBatType 
  * Batch type was changed, you have to save first..!
  * < OK >  
  =gfModalGen("TRM02185B00000","DIALOG")
  _CUROBJ=OBJNUM(pbSav)
  RETURN
ENDIF   

*** check the batch posting boundaries if any period was locked.
IF laData[2] = "E" .AND. llPeriodLock
  *** Batch posting boundaries has period ð locked. 
  *** You cannot add any transactions until you change posting 
  *** boundaries or unlock this period.
  * < OK >  
  =gfModalGen("TRM02205B00000","DIALOG",lcPeriodLock)
  RETURN
ENDIF

IF !lfCheckRec()    && check the current batch status.
  RETURN
ENDIF  

lcMode  = IIF(laScrMode[2],'T','F')
lnTrnHdRec = STR(RECNO('GLTRNSHD'))
lnTrnDtRec = STR(RECNO('GLTRNSDT'))

*E300683,5 Add parameters to gpDoprog
*DO gpDoProg WITH 'AWRGLTRANS WITH "&laData[1]","&lnTrnHdRec","&lnTrnDtRec","&lcMode"'," ",'GL'
DO gpDoProg WITH 'AWRGLTRANS', .F., 'GL', ;
   "'" + laData[1] + "','" + lnTrnHdRec + "','"  + lnTrnDtRec + ;
   "','" + lcMode    + "'"
*E300683,5 end

*!**************************************************************************
*!
*!      Function: lfvOK
*!
*!**************************************************************************

FUNCTION lfvOK
llOk = .T.            && Push Button OK has been Pressed at Preview screen

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

*** local delete procedure to void the current batch.
*** void current batch. (logical deletion)

IF lfCheckRec()    && check the current batch status.
  laScrMode = .F.
  laScrMode [1] = .T.  
  REPLACE GLBATCH.cBatStat WITH 'V'
  =gfAdd_Info('GLBATCH')
  =gfObj_Lock(.F.) 
  
   
   SELECT GLTRNSHD
   SET ORDER TO TAG BATCHTRN
   SELECT GLBATCH 
   SET RELATION TO glbatch.cbatchno INTO GLTRNSHD ADDITIVE
   SELECT GLTRNSHD
 
   SCAN REST WHILE glbatch.cbatchno = GLTRNSHD.cBatchNo
     REPLACE GLTRNSHD.cTrnStat WITH 'V'
     =gfAdd_Info('GLTRNSHD')
   ENDSCAN
 
   SELECT GLBATCH   
   SET RELATION TO
ENDIF  

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*

PROCEDURE lpSavScr

*** local procedure to save the current batch

*E300692,1 Change file name from sycfishd to fishd
*SELECT SYCFISHD
SELECT FISHD
*E300692,1 end
*SEEK (gcAct_Comp+laData[4])
*B602733,1 Do not seek company ID
*SEEK (gcPrnt_Cmp+laData[4])
SEEK (laData[4])
*B602733,1 end
SELECT GLBATCH



*** check the start date (ladata[5]) falls in the selected year (ladata[4])
*E300692,1 Change file name from sycfishd to fishd
*IF !BETWEEN(laData[5],SYCFISHD.dFisBgDat,SYCFISHD.dFisEnDat)
IF !BETWEEN(laData[5],FISHD.dFisBgDat,FISHD.dFisEnDat)
*E300692,1 end
  * Start ð must fall in selected year.
  * < OK >  
  =gfModalGen("TRM02153B00000","DIALOG",IIF(llDate,'date','period'))

  *** check if the program working with periods format or dates format.
  IF llDate 
    _CUROBJ=OBJNUM(laData[5])
  ELSE
    _CUROBJ=OBJNUM(lcStartPrd)
  ENDIF  

  llcSave = .F.
  SELECT GLBATCH
  RETURN
ENDIF



*** check the end date ladata[6] falls in the selected year (ladata[4])
*E300692,1 Change file name from sycfishd to fishd
*IF !BETWEEN(laData[6],SYCFISHD.dFisBgDat,SYCFISHD.dFisEnDat)
IF !BETWEEN(laData[6],FISHD.dFisBgDat,FISHD.dFisEnDat)
*E300692,1 end
  * End ð must fall in selected year.
  * < OK >  
  =gfModalGen("TRM02154B00000","DIALOG",IIF(llDate,'date','period'))

  *** check if the program working with periods format or dates format.
  IF llDate 
    _CUROBJ=OBJNUM(laData[6])
  ELSE
    _CUROBJ=OBJNUM(lcEndPrd)
  ENDIF  
 
  llcSave = .F.  
  SELECT GLBATCH
  RETURN
ENDIF



*** check if the start date (ladata[4]) greaater 
*** than the end date (ladata[6])
IF laData[5] > laData[6]

  *** check if the program working with periods format or dates format.
  IF llDate 
    _CUROBJ=OBJNUM(laData[5])
    * Beginning date should be less than the end date.
    * < OK >    
    =gfModalGen("TRM02009B00000","DIALOG")
  ELSE
    _CUROBJ=OBJNUM(lcStartPrd)
    * Beginning period should be less than the end period.
    * < OK >    
    =gfModalGen("TRM02155B00000","DIALOG")
  ENDIF  

  llcSave = .F.  
  SELECT GLBATCH
  RETURN
ENDIF



*** check if the audit total not equal the transations amounts.
IF laData[7] <> laData[10] AND laData[7] <> laData[11] AND laData[2] <>'E'
  * The transaction amounts do not equal the Audit total.
  * < Proceed >  < Cancel >
  IF gfModalGen("TRM02168B00012","DIALOG") = 2
    llcSave = .F.  
    _CUROBJ=OBJNUM(laData[7])
    SELECT GLBATCH
    RETURN
  ENDIF 
ENDIF



*** check the batch posting boundaries if any period was locked.
*E300692,1 Change file name from sycfsprd to fsprd
*SELECT  SYCFSPRD
SELECT  FSPRD
*E300692,1 end
SET ORDER TO COMFYRPRDI

*SEEK (gcAct_Comp+laData[4]+lcStartPrd)
*B602733,1 Do not seek company ID
*SEEK (gcPrnt_Cmp+laData[4]+lcStartPrd)
SEEK (laData[4]+lcStartPrd)
*B602733,1 end
llPerLock = .F.
*E300789,7 [BEGIN]
*SCAN REST FOR cComp_ID   = gcAct_Comp .AND. ;
              cFisfYear  = laData[4]  .AND. ;
              cFspPrdID <= lcEndPrd

*  IF lFspLocks  
*    llPerLock = .T.
*    EXIT
*  ENDIF  
*ENDSCAN 
SCAN REST FOR cFisfYear  = laData[4]  .AND. ;
              cFspPrdID <= lcEndPrd

  IF lFspLocks  
    llPerLock = .T.
    EXIT
  ENDIF  
ENDSCAN 
*E300789,7 [END..]
IF llPerLock
  * Period ð is manually locked.
  * < OK >
  =gfModalGen("TRM02157B00000","DIALOG",cFspPrdID+'-'+laData[4])
  llcSave = .F.  
  IF cFspPrdID = lcStartPrd
    *** check if the program working with periods format or dates format.
    IF llDate 
      _CUROBJ=OBJNUM(laData[5])
    ELSE
      _CUROBJ=OBJNUM(lcStartPrd)
    ENDIF 
  ELSE
    *** check if the program working with periods format or dates format.
    IF llDate 
      _CUROBJ=OBJNUM(laData[6])
    ELSE
      _CUROBJ=OBJNUM(lcEndPrd)
    ENDIF 
  ENDIF   
  SELECT GLBATCH
  RETURN
ENDIF  
SELECT GLBATCH



*** check if the start date ladata[5] 
*** falls before the beginning balance date from glSetup.dSetBBDat.
*** and check the status flag of glSetup.dSetBBDat
IF ladata[5] < glSetup.dSetBBDat
  IF glSetup.lSetPBBBd 
    * ð date falls before the beginning balance date ð.
    * < OK >
    =gfModalGen("TRM02159B00000","Dialog","Start|"+DTOC(glSetup.dSetBBDat))
  ELSE
    * ð date falls before the beginning balance date ð.
    * Posting before the beginning balance date is not allowed.
    * < OK >    
    =gfModalGen("TRM02202B00000","Dialog","Start|"+DTOC(glSetup.dSetBBDat))
  
    *** check if the program working with periods format or dates format.
    IF llDate 
      _CUROBJ=OBJNUM(laData[5])
    ELSE
      _CUROBJ=OBJNUM(lcStartPrd)
    ENDIF  
    llcSave = .F.  
    RETURN
  ENDIF  
ENDIF 



*** check if the difference between the strat date and end date
*** falls in the boundaries posting window (GLSETUP.nSetBatPw).
IF (VAL(lcEndPrd)-VAL(lcStartPrd))+1 > GLSETUP.nSetBatPw
  * The batch posting window cannot exceed ð periods.
  * < OK >  
  =gfModalGen("TRM02152B00000","DIALOG",STR(GLSETUP.nSetBatPw))

  *** check if the program working with periods format or dates format.
  IF llDate 
    _CUROBJ=OBJNUM(laData[6])
  ELSE
    _CUROBJ=OBJNUM(lcEndPrd)
  ENDIF 

  llcSave = .F.  
  SELECT GLBATCH
  RETURN
ENDIF



*** check if the user is forced to enter the audit total filed 
*** in case of ladata[7] is empty.
IF glSetUp.lSetCnTot .AND. EMPTY(laData[7])
  * You have to enter the audit total.
  * < OK >  
  =gfModalGen("TRM02146B00000","DIALOG")
   llcSave = .F.
  _CUROBJ = OBJNUM(laData[7])
  SELECT GLBATCH
  RETURN 
ENDIF

SELECT GLBATCH


IF laScrMode[4]     && Add Mode  - New Batch sequence.
  APPEND BLANK

  *E300663,1 Change this line for the changes we have 
  *          made to (gfSequence) [Begin]
  *laData[1]  = RIGHT("000000"+ALLTRIM(STR(gfSequence("BATCH",1))),6)
  laData[1]  = gfSequence('CBATCHNO')
  *E300663,1 Change this line [End]
  
  SHOW GET laData[1] 
  GATHER FROM laData FIELDS &lcScFields 
  =gfAdd_Info('GLBATCH')
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  =gfObj_Lock(.T.)     
  * Batch saved with number ð.  
  * Do you want to add transactions to this batch now ? 
  * < Yes > < No >
  IF gfModalGen("TRM02162B00006","DIALOG",laData[1]) = 1
    laScrMode     = .F.
    laScrMode [3] = .T.
    SHOW GETS
    llCSave = .F.
    *** update the audit information in the variable lcStamp
    lcStamp = glBatch.cAdd_User+DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
    lcMode  = IIF(laScrMode[2],'T','F')
    *** branching to the transation screen.
    *E300683,5 Add parameters to gpDoprog
    *DO gpDoProg WITH 'AWRGLTRANS WITH "&laData[1]","&lcMode"'," ",'GL'
    DO gpDoProg WITH 'AWRGLTRANS', .F., 'GL', ;
          "'" + laData[1] + "','" + lcMode  + "'"
    *E300683,5 end
  ENDIF
ELSE      &&  Edit mode  - Old batch number.
  GATHER FROM laData FIELDS &lcScFields 
  =gfAdd_Info('GLBATCH')
  =gfObj_Lock(.F.)     
ENDIF

SELECT GLBATCH

*!**************************************************************************
*!
*!      Procedure: lfCheckRec
*!
*!**************************************************************************
*

PROCEDURE lfCheckRec

lcLoclShow = "lpShow"
SHOW GET pbDlt,1 &lcPrompt &lcObjStatus
*B600486,1 Control the status of the delete bar in the record pad.
laCtrStat[8] = lcObjStatus


*** function to check the batch status.
IF ! laScrMode[2] 
  RETURN
ENDIF  

IF EMPTY(laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

*** check if the displayed batch is removed (voided) by another workstation.
IF laData[1] <> glbatch.cBatchNo
  * This ð was ð by another user .
  * < Yes > < No >
  =gfModalGen("TRM02187B00000","DIALOG",;
     "batch"+'|'+"voided")
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  SHOW GETS   
  RETURN .F.
ENDIF


IF ( lcBStamp = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time ) ;
   .AND. ( laData[2] = glbatch.cbatstat )
  RETURN .T.
ENDIF

lcAdd_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time = glBatch.cAdd_Time
lcAdd_Date = dtoc(glBatch.dAdd_Date)
lcPos_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cPostUser,SYUUSER.cUser_Id))
lcPos_Time = glBatch.cPostTime
lcPos_Date = dtoc(glBatch.dPostDate)

=gfObj_Lock(.F.) 

IF glbatch.cbatstat = 'V' 
  * This ð was ð by user ð at ð.
  * < Ok > 
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"voided"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  SHOW GETS   
  RETURN .F.
ENDIF


*** check if the displayed batch was approved by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'A'
  * This ð was ð by user ð at ð.
  * < Ok > 
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"approved"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  laScrMode = .F.
  laScrMode [3] = .T.     
  SHOW GETS 
  RETURN .F.
ENDIF


*** check if the displayed batch was posted by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'P'
  * This ð was ð by user ð at ð.
  * < Ok >   
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"posted"+'|'+lcPos_User+'|'+lcPos_Date+' '+lcPos_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  laScrMode = .F.
  laScrMode [3] = .T.   
  SHOW GETS 
  RETURN .F.
ENDIF


*** in case the batch status was changed.
IF glbatch.cbatstat $ 'HOEU'
  * ð status has been changed by user ð at ð.
  * < Ok >   
  =gfModalGen("TRM02181B00000","DIALOG",;
  "batch"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  SHOW GETS 
  RETURN .F.

ENDIF


*!**************************************************************************
*!
*!      FUNCTION: gfCpEdit
*!
*!**************************************************************************
*

FUNCTION gfCpEdit

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

llPeriodLock = .F.
lcPeriodLock = " "

*E300692,1 Change file name from sycfsprd to fsprd
*SELECT  SYCFSPRD
SELECT  FSPRD
*E300692,1 end
SET ORDER TO COMFYRPRDI
*SEEK (gcAct_Comp+laData[4]+lcStartPrd)
*B602733,1 Do not seek company ID
*SEEK (gcPrnt_Cmp+laData[4]+lcStartPrd)
SEEK (laData[4]+lcStartPrd)
*B602733,1 end
*E300789,7 [BEGIN]
*SCAN REST FOR cComp_ID   = gcAct_Comp .AND. ;
              cFisfYear  = laData[4]  .AND. ;
              cFspPrdID <= lcEndPrd

*  IF lFspLocks  
*    llPeriodLock = .T.
*    lcPeriodLock = cFspPrdID+'-'+laData[4]
*    EXIT
*  ENDIF  
*ENDSCAN 
SCAN REST FOR cFisfYear  = laData[4]  .AND. ;
              cFspPrdID <= lcEndPrd

  IF lFspLocks  
    llPeriodLock = .T.
    lcPeriodLock = cFspPrdID+'-'+laData[4]
    EXIT
  ENDIF  
ENDSCAN 
*E300789,7 [END..]
SELECT GLBATCH

IF laData[2] <> "E" .AND. llPeriodLock
  * Batch posting boundaries has period ð locked. 
  * You cannot edit this batch until you unlock this period.
  * < Ok >   
  =gfModalGen("TRM02198B00000","DIALOG",lcPeriodLock)
  RETURN
ENDIF  

IF glbatch.cbatstat $ 'APV'
  SHOW GETS
ELSE
  *B607000,1 WAB (Start)
  *DO gfCpEdit IN ARIA27
  DO gfCpEdit IN AAS_WIN.EXE
  *B607000,1 WAB (End)
ENDIF


