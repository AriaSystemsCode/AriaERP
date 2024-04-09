*:************************************************************************
*:
*: Procedure file: SMFSPRD.PRG 
*:                 (Fiscal periods)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
*: Last modified :
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*B602011,1 RENEE 05/04/98. Fix : Data overflow
*B602215,1 MAN   11/07/98. Fiscal periods are sometimes deleted
*B801967,1 AMM   02/22/99 Fix the bug of 'Subscript out of bounds' when adding periods manually.
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602658,1  HS   03/10/99 Fix some bugs that has appeared because of the
*B602658,1                remove of the field cComp_ID from the files
*B602658,1                FISHD , FSPRD.
*B602799,1  HS   04/15/99 Prevent the dimension of the array "laPriod"
*B602799,1                from depending on the fields order in the file
*B602799,1                "FISPRD" because this array is used to create
*B602799,1                most of the Get Fields in the Periods screen
*B602799,1                (SMFSPRD.SPR).
*B602860,1 AMM 05/03/99 Fix the bug of erasing periods when saving after editing them.
*B803179,1 RAMY 05/03/2000 Fix updating the temp file that holds the periods upon closing the screen
*B603824,1 RAMY 08/29/2000 Fix the bug of "Subscript out if bounds" in case we enter the 
*                          program from close year program
*B119864,1 MHM 10/06/2003 FIX bug of wrong date in case of close year before ending regular
*B608692,1 TMI 09/14/2008 Fix a bug that changing the end of the period does not saved (T20080801.0004   ) 
*:************************************************************************
PARAMETERS llCloseYear
*E300692,1 Store current alias
PRIVATE lnCurAlias
lnCurAlias = SELECT()
*E300692,1 end

EXTERNAL ARRAY laData,laScrMode,laDefProc,laPriod,laCompany

DECLARE laPriod[13,16]        && Array to hold all fields of 13 record


*B602658,1 Change this line for we have removed the field cComp_ID [Begin]
*DECLARE laStruc[1,18]         && Array of 18 colum to be use in append rest
DECLARE laStruc[1,17]         && Array of 18 colum to be use in append rest
*B602658,1 Change this line for we have removed the field cComp_ID [End]

*B602799,1 Add this line to get the fields that will be scattered in the
*          array "laPriod" to prevent the array dimension from depending
*          on the fields order in the file "FISPRD" [Begin]

*-- lcSctFilds    Variable to hold the fields that will be
*--               scattered in - and gathered from - the array "laPriod"
*B602860,1 AMM Add the two fields  nRecNo, cStatus
*lcSctFilds = 'cFisFYear , cFspPrdID , cFspPDesc , dFspPBgDt , ' +;
             'dFspPEnDt , nFspPartN , lFspLockS , lFspClsdS , ' +;
             'cAdd_User , dAdd_Date , cAdd_Time , lLok_Stat , ' +;
             'dLok_Date , cLok_User , cLok_Time'
lcSctFilds = 'cFisFYear , cFspPrdID , cFspPDesc , dFspPBgDt , ' +;
             'dFspPEnDt , nFspPartN , lFspLockS , lFspClsdS , ' +;
             'cAdd_User , dAdd_Date , cAdd_Time , lLok_Stat , ' +;
             'dLok_Date , cLok_User , cLok_Time , nRecNo    , ' +;
             'cStatus'
*B602860,1 AMM end
*B602799,1 Add this line to get the fields that will be scattered [End]


llCloseYear   = IIF(TYPE('llCloseYear')='U',.F.,llCloseYear)

IF llCloseYear



  *B603824,1 RAMY The laData array is no longer used in this program but we fill it manual 
  *               cuase the program use it in many cases[start]
  DECLARE laData[7]
  laData[1] = lcComp_ID
  laData[2] = &lcTmpFisHD..cfisnoprd
  laData[4] = &lcTmpFisHD..cfisnoprd
  laData[5] = syccomp.cCurr_Prd
  laData[6] = &lcTmpFisHD..dfisbgdat
  laData[7] = &lcTmpFisHD..dfisendat
  *B603824,1 RAMY [end]

  *B602658,1 Change this line for we have removed the field ccomp_id [Begin]
  *lcCompId   = &lcTmpFisHD..ccomp_id
  lcCompId   = ' '
  *B602658,1 Change this line for we have removed the field ccomp_id [End]
  
  lcFiscal   = &lcTmpFisHD..cfisfyear
  ldFisEnd   = &lcTmpFisHD..dfisendat
  lcFisNoPrd = &lcTmpFisHD..cfisnoprd
ELSE
  lcTmpFisHD = ' '
  lcCompId   = ' '
  lcFiscal   = ' '
  ldFisEnd   = {}
  lcFisNoPrd = ' '
ENDIF

laStruc        = ' '

*B602658,1 Change these lines for we have removed the field cComp_ID [Begin]
*laStruc [1,1 ] = IIF(llCloseYear,lcCompId,laData[1]) 
*laStruc [1,2 ] = IIF(llCloseYear,lcFiscal,laData[2])
*laStruc [1,18] = 'A'       && Default status for all records is Add
laStruc [1,1 ] = IIF(llCloseYear,lcFiscal,laData[2])
laStruc [1,17] = 'A'       && Default status for all records is Add
*B602658,1 Change these lines for we have removed the field cComp_ID [End]

DECLARE laObjStat [14,1]   && Array to hold status of each row ena/dis 

*** IF view mode, DISABLE all objects
IF llCloseYear
  laObjStat = IIF(laScrMode[2],"DISABLE","ENABLE")
ELSE
  laObjStat = IIF(laScrMode[2],"DISABLE","ENABLE")
ENDIF  

*** Open only periods added in home screen
IF llCloseYear
  FOR lnCount = (VAL(lcFisNoPrd)+1) TO 14
    laObjStat [lnCount] = "DISABLE"
  ENDFOR
ELSE
  FOR lnCount = (VAL(laData[4]) + 1) TO 14
    laObjStat [lnCount] = "DISABLE"
  ENDFOR
ENDIF

*** If periods was not accepted before
IF !llAcptPrd
  *** Create the default periods for fiscal year ð ? ***
  *** <  Yes  >  - <  No  > ***
  IF  gfModalGen("QRM00076B00006","DIALOG",IIF(llCloseYear,lcFiscal,IIF(llCloseYear,lcFiscal,laData[2]))) = 1
    llNewPrds = .T.   && Flag to creat new periods
    llDefPrds = .T.   && Flag to creat defaulte periods
  ELSE
    llNewPrds = .T.   && Flag to creat new periods
    llDefPrds = .F.   && Flag not creat defaulte periods 
  ENDIF
ENDIF


IF !llCloseYear
  llUpdate  = .F.       && Flag to tell if modification done 
ELSE
  llUpdate = .T.
ENDIF  

*** IF entering for first time creat temp file from master

IF lcCompFis <> IIF(llCloseYear,lcCompId+lcFiscal,laData[1]+laData[2])

  *** If the comp id and fiscal year was changed get them 
  *** and reselect from master file if there any old records
  *** or creat blank temp file by select command
  lcCompFis = IIF(llCloseYear,lcCompId+lcFiscal,laData[1]+IIF(llCloseYear,lcFiscal,laData[2]))

*E300692,1 Change file name and path from SYCFSPRD to FSPRD  
  *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (gcSysHome+"SYCFSPRD") ;
   WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = ;
         IIF(llCloseYear,lcCompId+lcFiscal,;
             laData[1]+IIF(llCloseYear,lcFiscal,laData[2]));
    INTO DBF (gcWorkDir+lc_TempPR)
  *E300789,1 Hesham (Start)  
  *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (lcDataDir+"FSPRD") ;
   WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = ;
         IIF(llCloseYear,lcCompId+lcFiscal,;
             laData[1]+IIF(llCloseYear,lcFiscal,laData[2]));
    INTO DBF (gcWorkDir+lc_TempPR)
  SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (lcDataDir+"FSPRD") ;
   WHERE CFISFYEAR+CFSPPRDID = ;
         IIF(llCloseYear,lcFiscal,;
             IIF(llCloseYear,lcFiscal,laData[2]));
    INTO DBF (gcWorkDir+lc_TempPR)

  *E300789,1 Hesham (End)  
 *E300692,1 end   
    
ENDIF

*B803179,1 RAMY add this lines to make this variables shown by the screen [start]
lnPeriod = 1
lcOldVal = ''
*B803179,1 RAMY [end]

SELECT(lc_TempPR)

*B803179,1 RAMY add index to the temp file used to seek on the year + period Id. [start]
INDEX ON CFISFYEAR+CFSPPRDID TAG lc_TempPR OF (gcWorkDir+lc_TempPR)
*B803179,1 RAMY [end]

*** If creation of new periods is required, in case adding fiscal year 
*** for a company or modifying existing periods
IF llNewPrds 
  llNewPrds = .F.

  *** Mark old records in temp file with delete marker if collected from 
  *** the master file or with 'S' marker if was added to the same record
  REPLACE ALL cStatus WITH IIF(AT(cStatus,"ASM") > 0,;
                               SUBSTR("SDD",AT(cStatus,"ASM"),1),"S")
  DELETE ALL

  *** If creat default periods of the year is required
  IF llDefPrds
    llDefPrds = .F.
    llUpdate  = .T.          && Change update flag to accept the default

    *** Call Add periods funtion with parameters: 
    *** Company ID, Fscal year, No of periods,Currant period,Begin date,
    *** End Date,Temp file name
    
    IF llCloseYear
      SELECT(lcTmpFisHD)
      =lfAddPrds(laCompany[puCompany,2],cFisfyear,VAL(cfisnoprd),VAL(ccurr_prd),dfisbgdat,dfisendat,"&gcWorkDir.&lc_TempPR")
      SELECT(lc_TempPR)
    ELSE
      =lfAddPrds(laData[1],laData[2],VAL(laData[4]),VAL(laData[5]),laData[6],laData[7],"&gcWorkDir.&lc_TempPR")
    ENDIF  
  *B801967,1 AMM Prepare the temporary file to accept data by adding records 
  *B801967,1 AMM with compID,FS year, FS period.
  ELSE
    FOR lnCount = 1 TO VAL(laData[4])
      
      *B602658,1 Change this line for we have removed the field
      *          ccomp_id [Begin]
      *INSERT INTO &gcWorkDir.&lc_TempPR (cComp_ID,cFisFyear,cFspprdid,cStatus) ;
      *       VALUES (laData[1],laData[2],RIGHT("0"+ALLTRIM(STR(lnCount)),2),'A')
      INSERT INTO &gcWorkDir.&lc_TempPR (cFisFyear,cFspprdid,cStatus) ;
             VALUES (laData[2],RIGHT("0"+ALLTRIM(STR(lnCount)),2),'A')
      *B602658,1 Change this line for we have removed the field [End]
      
    ENDFOR
  *B801967,1 AMM end
  ENDIF
ENDIF

*** IF records of temp file less than 13 period, Make it 13 so the 
*** array will be declared with the right dimention

COUNT TO lnRecords
FOR lnCount = lnRecords TO 12
  APPEND FROM ARRAY laStruc 
ENDFOR  

*** Creat screen array from temp file

*B602658,1 Change this line for we have removed the field cComp_ID from the
*          file but we need to keep the array dimension as it is so we are
*          going to add a dummy field as the first field in the SELCT SQL
*          fields [Begin]
*SELECT * FROM (gcWorkDir+lc_TempPR) INTO ARRAY laPriod

*B602799,1 Change this line to get only the fields that will be needed in
*          the array "laPriod" to prevent the array dimension from
*          depending on the fields order in the file "FISPRD" [Begin]
*SELECT SPACE(1) , * FROM (gcWorkDir+lc_TempPR) INTO ARRAY laPriod
SELECT SPACE(1) , &lcSctFilds FROM (gcWorkDir+lc_TempPR) INTO ARRAY laPriod

*B119864,1 MHM 10/06/2003 [Start]
IF llCloseYear
  lcCurAlias = ALIAS()
  SELECT(lcTmpFisHD)
  IF !BETWEEN(laPriod[1,6] - dfisbgdat,27,31)
    SELECT(lc_TempPR)
    DELETE ALL
    pack
    SELECT(lcTmpFisHD)
    =lfAddPrds(laCompany[puCompany,2],cFisfyear,VAL(cfisnoprd),VAL(ccurr_prd),dfisbgdat,dfisendat,"&gcWorkDir.&lc_TempPR")
    SELECT(lc_TempPR)
    COUNT TO lnRecords
    FOR lnCount = lnRecords TO 12
      APPEND FROM ARRAY laStruc 
    ENDFOR  
    SELECT SPACE(1) , &lcSctFilds FROM (gcWorkDir+lc_TempPR) INTO ARRAY laPriod
  ENDIF  
  SELECT(lcCurAlias)
ENDIF
*B119864,1 MHM 10/06/2003 [End]

*B602799,1 Change this line to get only the fields that will be needed [End]

*B602658,1 Change this line for we have removed the field cComp_ID [End]

*** Marke all array elements with add marker so if accpted by ok
*** all records will be added to the master file

FOR lnCount = 1 TO 13
  *B602215,1 Start Fix the Status field position 
  *laPriod[lnCount,18] = 'A'
  laPriod[lnCount,MIN(19,ALEN(laPriod,2))] = 'A'
  *B602215,1 end
ENDFOR

*** Begin of first period and End of last period has to be defaulted
*** From home fiscal screen and not to be modified.
IF llCloseYear
  lcCurAlias = ALIAS()
  SELECT(lcTmpFisHD)
  laPriod[1,5] = dfisbgdat
  laPriod[VAL(cfisnoprd),6] = dfisendat
  SELECT(lcCurAlias)
ELSE
  laPriod[1,5]              = laData[6]
  laPriod[VAL(laData[4]),6] = laData[7]
ENDIF

*B803179 RAMY Add another array holds the temp file data to use it in case the user cancle changes [START]
=ACOPY(laPriod , laTmpPrd)
*B803179 RAMY [END]


*E300683,4 Call *.SPR from screens directory
* DO SMFSPRD.SPR 
DO (gcScrDir + gcWinAppl + '\SMFSPRD.SPR')
*E300683,4 end    
*E300692,1 Change file name from SYCFISHD to 
*SELECT SYCFISHD 
SELECT (lnCurAlias)
*E300692,1 end

*!**************************************************************************
*!
*!      Function: lfvDateRng
*!
*!**************************************************************************
* Function to validate all end date fields
*
FUNCTION lfvDateRng

*** Get the period No. from the name of the object detected by the SYS(18)
*** Function
lnPeriod = INT(VAL(SUBSTR(SYS(18),AT('(',SYS(18))+1,AT(',',SYS(18))-AT('(',SYS(18))-1)))

*** The end date has to be not empty to be validated so the user can leave
*** the screen whenever he wants by cancel buttun or even leave the field

IF !EMPTY(EVALUATE(SYS(18)))
  DO CASE 
    *** User is not permited to skip end date without adding
    CASE EMPTY(laPriod[lnPeriod,5])
      *** Periods End Date must be enterd sequentialy..!
      =gfModalGen("TRM00078B00000","DIALOG",STR(lnPeriod))
      laPriod[lnPeriod,6] = {}
      SHOW GET (SYS(18))   
      *** Look for the first empty end date to select
      FOR lnCount = 1 TO 13
        IF EMPTY(laPriod[lnCount,6])
          EXIT
        ENDIF
      ENDFOR
      _CUROBJ = OBJNUM(laPriod[lnCount,6])

    *** Periods days not less than one day
    CASE laPriod[lnPeriod,6] <= laPriod[lnPeriod,5]
      *** End date of period ð can not ***
      *** be less than begin date ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00072B00000","DIALOG",ALLTRIM(STR(lnPeriod)))
      laPriod[lnPeriod,6] ={}
      _CUROBJ = OBJNUM(laPriod[lnPeriod,6])
      SHOW GET (SYS(18))   

    *** End Date has to be within the fiscal year. ***
    CASE laPriod[lnPeriod,6] >= IIF(llCloseYear,ldFisEnd,laData[7])
      *** End date of period ð can not be grater ***
      *** than end date of the year ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00073B00000","DIALOG",STR(lnPeriod))
      laPriod[lnPeriod,6] ={}
      _CUROBJ = OBJNUM(laPriod[lnPeriod,6])
      SHOW GET (SYS(18))   

    *** Begin Date of next period is one day affter the end date
    OTHERWISE
      IF llCloseYear 
        laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,ldFisEnd)
      ELSE
        laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,laData[7])
      ENDIF
      *B608692,1 TMI 09/14/2008 [Start] Fix a bug that changing the end of the period does not saved
      laPriod[lnPeriod+1,18] = 'M'
      *B608692,1 TMI 09/14/2008 [ End ] Fix a bug that changing the end of the period does not saved
      SHOW GETS
  ENDCASE
ELSE
  laPriod[lnPeriod+1,5] = {}
  SHOW GETS
ENDIF

*** IF the user use UP or DOWN arrow move to the right object
DO CASE 
  *** Up arrow 
  CASE LASTKEY() = 5
    _CUROBJ = _CUROBJ - 5
    *** Down arrow
  CASE LASTKEY() = 24
    _CUROBJ = _CUROBJ + 5
ENDCASE 

*B803179,1 Ramy -> Update end date fields.
IF laPriod[lnPeriod,6] <> lcOldVal
  laPriod[lnPeriod,18] = "M"
ENDIF
*B803179,1 Ramy

*!**************************************************************************
*!
*!      Function: lfvPartRng
*!
*!**************************************************************************
* This function is called from all the part fields
* Parts has to be from 1 to 6
*
FUNCTION lfvPartRng

IF EVALUATE(SYS(18)) < 1 .OR. EVALUATE(SYS(18)) > 6
  =gfModalGen("TRM00071B00000","DIALOG")
  lnOnjName  = SYS(18) 
  &lnOnjName = 1
  _CUROBJ = OBJNUM(&lnOnjName)
  SHOW GET (SYS(18)) 
ENDIF

*B803179,1 Ramy - Update part range
lnPeriod = INT(VAL(SUBSTR(SYS(18),AT('(',SYS(18))+1,AT(',',SYS(18))-AT('(',SYS(18))-1)))
IF laPriod[lnPeriod,7] <> lcOldVal
  laPriod[lnPeriod,18] = "M"
ENDIF
*B803179,1 Ramy

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

*** All End date of periods has to be filled
FOR lnCount = 1 TO IIF(llCloseYear,VAL(lcFisNoPrd),VAL(laData[4]))
  IF EMPTY(laPriod[lnCount,5]) .OR. EMPTY(laPriod[lnCount,6])
    =gfModalGen("TRM00075B00000","DIALOG",IIF(llCloseYear,lcFiscal,laData[2]))  
    RETURN
  ENDIF
ENDFOR

*** Check if all end dates is enterd with the right validation
*** If not go to the wrong date to be reenterd

FOR lnCount = 1 TO IIF(llCloseYear,VAL(lcFisNoPrd),VAL(laData[4]))
  IF laPriod[lnCount,6] < laPriod[lnCount,5]
    *** End date of period ð can not be less than begin date ..!
    =gfModalGen("TRM00072B00000","DIALOG",STR(lnCount))
    laPriod[lnCount,6] = {}
    _CUROBJ = OBJNUM(laPriod[lnCount,6])
    lcObjName = "laPriod["+ALLTRIM(STR(lnCount))+",6]"
    SHOW GET &lcObjName
    RETURN 
  ENDIF
ENDFOR

*** Redimention the array with right legnth so the no of records to 
*** add is same as NO. of periods
IF llCloseYear 
  *B602011,1 Adjust column dimension
  *DECLARE laPriod[VAL(lcFisNoPrd),18] 
  DECLARE laPriod[VAL(lcFisNoPrd), ALEN(laPriod,2)] 
  *B602011,1 end
ELSE 
  *B602011,1 Adjust column dimension
  *DECLARE laPriod[VAL(laData[4]),18]
  DECLARE laPriod[VAL(laData[4]),ALEN(laPriod,2)]
  *B602011,1 end
ENDIF  

*B803179,1 Ramy Change the way of saving the periods in the period temp. file.
SELECT (lc_TempPR)

*** Delete all old records in temp file before adding the new ones
*** Affter changing the marker from SAME to DELETE and form
*** ADD to SAME
*REPLACE ALL cStatus WITH IIF(AT(cStatus,"ASM") > 0,;
*                             SUBSTR("SDD",AT(cStatus,"ASM"),1),"S")
*DELETE ALL

*B602658,1 Add this line to remove the dummy field [the first column] from
*          the array [Begin]
*=ADEL(laPriod , 1 , 2)
*B602658,1 Add this line to remove the dummy field [the first column] [End]

*** Append from tow dimention array will add all the (No. of periods)
*** to the temp file in one step 

*B602799,1 Change this line to gather only the fields that's stored in
*          the array
*APPEND FROM ARRAY laPriod

*APPEND FROM ARRAY laPriod FIELDS &lcSctFilds
*B602799,1 Change this line to gather only the fields that's stored [End]

=ADEL(laPriod , 1 , 2)

FOR lnCnt = 1 TO ALEN(laPriod , 1)
  IF laPriod[lnCnt,17] = 'M'
    SELECT (lc_TempPR)
    =SEEK(laPriod[lnCnt , 1] + laPriod[lnCnt , 2])
    REPLACE cFsppDesc WITH laPriod[lnCnt , 3] ;
            dFsppEndt WITH laPriod[lnCnt , 5] ;
            nFspPartN WITH laPriod[lnCnt , 6] ;
            lFspLocks WITH laPriod[lnCnt , 7] ;
            lFspClsds WITH laPriod[lnCnt , 8] ;
            cStatus   WITH IIF(cStatus $ 'SM' , 'M' , cStatus)
    *B608692,1 TMI 09/14/2008 [Start] Update the dFsppBGdt field
    REPLACE dFsppBGdt WITH laPriod[lnCnt , 4]
    *B608692,1 TMI 09/14/2008 [ End ] Update the dFsppBGdt field
  ENDIF
ENDFOR

*-Ram
DELETE ALL FOR cFspprdid = " "
*-Ram
*B803179,1 Ramy

*** Change the accept flag so if the screen reenterd, Dont miss the data
llAcptPrd = .T.

*** This flag to say not swap the old value.
llOldFlg  = .F.

*** Change the update periods flag so in the save of home screen
*** move recoreds to master file in case of editing old recorde
llUpdPr = .T.

llPeriod = .T.

=gfUpdate()

*** Terminat the screen. ***
CLEAR READ

*:****************************************************************************
*! Name      : lfwOldVal
*! Developer : RAMY MABROUK (RAM)
*! Date      : 05/03/2000
*! Purpose   : To get the field old value
*:****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfwOldVal()
*:****************************************************************************
*B803179,1
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*:****************************************************************************
*! Name      : lfvDesc
*! Developer : RAMY MABROUK (RAM)
*! Date      : 05/03/2000
*! Purpose   : The descreption filed valid function
*:****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfvDesc()
*:****************************************************************************
*B803179,1
FUNCTION lfvDesc

*B803179,1 Ramy - Update description field
lnPeriod = INT(VAL(SUBSTR(SYS(18),AT('(',SYS(18))+1,AT(',',SYS(18))-AT('(',SYS(18))-1)))
IF laPriod[lnPeriod,4] <> lcOldVal
  laPriod[lnPeriod,18] = "M"
ENDIF

*:****************************************************************************
*! Name      : lfvLock
*! Developer : RAMY MABROUK (RAM)
*! Date      : 05/03/2000
*! Purpose   : The lock filed valid function
*:****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfvLock()
*:****************************************************************************
*B803179,1
FUNCTION lfvLock

*B803179,1 Ramy - Update lock fields
lnPeriod = INT(VAL(SUBSTR(SYS(18),AT('(',SYS(18))+1,AT(',',SYS(18))-AT('(',SYS(18))-1)))
IF laPriod[lnPeriod,8] <> lcOldVal
  laPriod[lnPeriod,18] = "M"
ENDIF


*:****************************************************************************
*! Name      : lfvClsPrd
*! Developer : RAMY MABROUK (RAM)
*! Date      : 05/03/2000
*! Purpose   : The close Period filed valid function
*:****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfvClsPrd()
*:****************************************************************************
*B803179,1
FUNCTION lfvClsPrd

*B803179,1 Ramy - Update clode fields.
lnPeriod = INT(VAL(SUBSTR(SYS(18),AT('(',SYS(18))+1,AT(',',SYS(18))-AT('(',SYS(18))-1)))
IF laPriod[lnPeriod,9] <> lcOldVal
  laPriod[lnPeriod,18] = "M"
ENDIF

*:****************************************************************************
*! Name      : lfvClose
*! Developer : RAMY MABROUK (RAM)
*! Date      : 05/03/2000
*! Purpose   : The close button valid function
*:****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfvClose()
*:****************************************************************************
*B803179,1
FUNCTION lfvClose

=ACOPY(laTmpPrd , laPriod)
llOldFlg = .T.
