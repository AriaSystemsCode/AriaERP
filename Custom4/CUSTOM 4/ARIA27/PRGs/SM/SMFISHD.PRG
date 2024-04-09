*:************************************************************************
*:
*: Procedure file: SMFISHD.PRG 
*:                 (Fiscal year called from closing)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:************************************************************************
*: Last modified :
*:************************************************************************
*B601152,1 M.H 07/07/96 Fixing duplicating the balances.
*B601543,1 M.H 01/16/97 Fix the bug of deleting the rest of the period file.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300692,5 RENEE 07/01/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,5 				   SYCFSPRD, SYCFSHLD
*B602193,1 AMM 02/22/99 Default the fiscal years descriptions
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602658,5 AMM 03/14/99 Fix some bugs in the program.
*B803570,1 RAMY 08/13/2000 This bug fixing is done in the screen generator
*B605667,1 SSE 03/28/2002 Fix bug of adding new records for all customers in ArCusHst file 
*B605667,1                And also new records for all styles in IcStyHst file.
***************************************************************************

EXTERNAL ARRAY laScrMode,laDefProc,laCompany

DECLARE laPriod[13,16]  && Array to hold all fields of 13 record.
DECLARE laStruc[1,18]   && Array of 18 colum to be use in append rest.

lcOldNoPrd   = ' '      && Variable to keep old value of no. of periods.
lcFisDes     = ' '      && Variable to keep 'Perivious or Current or Next'.
lcFldNm      = ' '      && Variable to hold text in one of the messages.
lcYear       = ' '      && Variable to hold the prompt of the popup.
lcCompFis    = ' '      && Variable to hold the active company + the fiscal year.
lcNoOfPrd    = ' '      && Variable to hold the no of periods.
lcFisYear    = ' '      && Variable to hold the fiscal year.

laPriod      = ' '      && Array intialization
laStruc      = ' '      && Array intialization

llOldFlg     = .F.      && Flag to know that return cancel from lfAcptPrd
llNewPrds    = .F.      && Flag to be used if creat periods for ;
                           first time or when chang periods or dates.
llDefPrds    = .F.      && Flag to be used if creat default ;
                           periods is required.
llAcptPrd    = .F.      && Flag to tell if the periods of the year ;
                           is accepted before for the same record.
llUpdPr      = .F.      && Flag to know is there any modification in;
                           periods.
llPeriod     = .F.      && Variable to indicate if the no of periods change.
llFrstEndDat = .T.      && Flag to assign the end date of the closing year;
                           for the first time entering the screen.
llUpdEndDate = .F.      && Flag indicate if there is any change happen in;
                           the end date of the closing year.

ldOldEnd     = {}       && Var to keep old value of End date
ldEndYearDat = {}       && This variable is assigned from the fuction of the;
                           begin date
ldOldEndDate = {}       && Variable to hold the old date of the next year(New Current)
*B601152,1 M.H. Initialize the variables that hold the current year end date 
ldDatChange  = laCompany[puCompany,7]
ldOldEndDate = laCompany[puCompany,7]
*B601152,1 M.H. End

*** Assign random names to the variables to use in openning the temp files.

lcTmpCPrd    = gfTempName() && Temp file to hold the Current period records
lcTmpNPrd    = gfTempName() && Temp file to hold the Next period records
lcTmpNHold   = gfTempName() && Temp file to hold the Next period records

*E300692,5 Change file name from SYCFISHD to FISHD
*SELECT SYCFISHD
SELECT (lcFisHd)
*E300692,5 end
*** Locate in the fiscal header file for the current company.

*E300692,5 Change file name from SYCFISHD to FISHD
*LOCATE FOR SYCFISHD.cComp_Id  = laCompany[puCompany,2];
       AND SYCFISHD.cFisYStat = 'C'
*E300789,1 Hesham (Start)
*LOCATE FOR &lcFisHd..cComp_Id  = laCompany[puCompany,2];
       AND &lcFisHd..cFisYStat = 'C'
LOCATE FOR &lcFisHd..cFisYStat = 'C'
*E300789,1 Hesham (End)       
*E300692,5 end       
       
       
IF FOUND()
  *B602193,1 AMM Get fiscal Year Description
  DIMENSION laFisDesc[3,3]
  STORE SPACE(0) TO laFisDesc
  laFisDesc[1,1] = &lcFisHd..CFISLHEAD
  laFisDesc[1,2] = &lcFisHd..CFISSHEAD
  laFisDesc[1,3] = &lcFisHd..CFISNONWD
  *B602193,1 AMM  end
  
  *** Assign the fiscal year to this variable to use in lfBeginDat().
  *E300692,5 Change file name from SYCFISHD to FISHD  
  *lcFisYear   = STR(INT(VAL(SYCFISHD.CFISFYEAR)),4)
  lcFisYear   = STR(INT(VAL(&lcFisHd..CFISFYEAR)),4)
  *E300692,5 end  
  SKIP +1
  *B602193,1 AMM Get desription for current and next years
  laFisDesc[2,1] = &lcFisHd..CFISLHEAD
  laFisDesc[2,2] = &lcFisHd..CFISSHEAD
  laFisDesc[2,3] = &lcFisHd..CFISNONWD
  laFisDesc[3,1] = 'Fis. year '+ ALLTRIM(STR(INT(VAL(&lcFisHd..CFISFYEAR)+1)))
  laFisDesc[3,2] = laFisDesc[3,1]
  laFisDesc[3,3] = &lcFisHd..CFISNONWD
  *B602193,1 AMM end

  *E300692,5 Change file name from SYCFISHD to FISHD    
  *lcNoOfPrd   = SYCFISHD.CFISNOPRD  && Variable to hold the old no of;
                                       periods of the new current year.
                                       
  *ldOldNxtDat = SYCFISHD.DFISENDAT  && Variable to hold the old end date;
                                       of the new current year.

  lcNoOfPrd   = &lcFisHd..CFISNOPRD  && Variable to hold the old no of;
                                       periods of the new current year.
                                       
  ldOldNxtDat = &lcFisHd..DFISENDAT  && Variable to hold the old end date;
                                       of the new current year.
 *E300692,5 end                                      
                                       
  SKIP -1

  *** In this loop we are going to change the following :
  *** 'C'urrent -> 'P'revioce.
  *** 'N'ext    -> 'C'urrent.
  *** New year  -> 'N'ext.
  
  FOR lnCount = 1 TO 3
  *E300692,5 Change file name from SYCFISHD to FISHD     
    *INSERT INTO (gcWorkDir+lcTmpFisHD);
           (CCOMP_ID,CFISFYEAR,CFISYSTAT,CFISNOPRD,CFISLHEAD,;
           CFISSHEAD,CFISNONWD,DFISBGDAT,DFISENDAT,CCURR_PRD);
     VALUES(SYCFISHD.CCOMP_ID,lcFisYear,;
           IIF(lnCount = 1,'P',IIF(lnCount = 2,'C','N')),;
           IIF(lnCount = 1,laCompany[puCompany,3],lcNoOfPrd),;
           SYCFISHD.CFISLHEAD,SYCFISHD.CFISSHEAD,;
           SYCFISHD.CFISNONWD,lfBeginDat(lnCount),ldEndYearDat,;
           IIF(lnCount = 1,laCompany[puCompany,3],'01'))
           
    *B602193,1 AMM Add the right description for each year.
    *INSERT INTO (gcWorkDir+lcTmpFisHD);
           (CCOMP_ID,CFISFYEAR,CFISYSTAT,CFISNOPRD,CFISLHEAD,;
           CFISSHEAD,CFISNONWD,DFISBGDAT,DFISENDAT,CCURR_PRD);
     VALUES(&lcFisHd..CCOMP_ID,lcFisYear,;
           IIF(lnCount = 1,'P',IIF(lnCount = 2,'C','N')),;
           IIF(lnCount = 1,laCompany[puCompany,3],lcNoOfPrd),;
           &lcFisHd..CFISLHEAD,&lcFisHd..CFISSHEAD,;
           &lcFisHd..CFISNONWD,lfBeginDat(lnCount),ldEndYearDat,;
           IIF(lnCount = 1,laCompany[puCompany,3],'01'))
    *E300789,1 Hesham (Start)
    *INSERT INTO (gcWorkDir+lcTmpFisHD);
           (CCOMP_ID,CFISFYEAR,CFISYSTAT,CFISNOPRD,CFISLHEAD,;
           CFISSHEAD,CFISNONWD,DFISBGDAT,DFISENDAT,CCURR_PRD);
     VALUES(&lcFisHd..CCOMP_ID,lcFisYear,;
           IIF(lnCount = 1,'P',IIF(lnCount = 2,'C','N')),;
           IIF(lnCount = 1,laCompany[puCompany,3],lcNoOfPrd),;
           laFisDesc[lnCount,1],laFisDesc[lnCount,2],;
           laFisDesc[lnCount,3],lfBeginDat(lnCount),ldEndYearDat,;
           IIF(lnCount = 1,laCompany[puCompany,3],'01'))

    INSERT INTO (gcWorkDir+lcTmpFisHD);
           (CFISFYEAR,CFISYSTAT,CFISNOPRD,CFISLHEAD,;
           CFISSHEAD,CFISNONWD,DFISBGDAT,DFISENDAT,CCURR_PRD);
     VALUES(lcFisYear,;
           IIF(lnCount = 1,'P',IIF(lnCount = 2,'C','N')),;
           IIF(lnCount = 1,laCompany[puCompany,3],lcNoOfPrd),;
           laFisDesc[lnCount,1],laFisDesc[lnCount,2],;
           laFisDesc[lnCount,3],lfBeginDat(lnCount),ldEndYearDat,;
           IIF(lnCount = 1,laCompany[puCompany,3],'01'))

    *E300789,1 Hesham (End)
    *B602193,1 AMM end
    
    IF lnCount = 1
      *** Add 1 to the fiscal year variable to be the year of the
      *** new 'C'urrent.
      *E300692,5 Change file name from SYCFISHD to FISHD        
      *lcFisYear = STR(INT(VAL(SYCFISHD.CFISFYEAR)+1),4)
      lcFisYear = STR(INT(VAL(&lcFisHd..CFISFYEAR)+1),4)
      *E300692,5 end  
    ELSE
      *** Add 1 to the new 'C'urrent year to be the new 'N'ext.
      lcFisYear = STR(INT(VAL(lcFisYear)+1),4)
    ENDIF       
  ENDFOR
ENDIF       

SELECT(lcTmpFisHD)

IF RECCOUNT() > 0
  *** Go to the new 'C'urrent year to be the defaule in the
  *** begin of the screen
  GO 2
  *** Assign the variable of the PUSH BUTTON with next + the new next year.
  lcYear    = "Ne\<xt"+' '+ALLTRIM(STR(VAL(CFISFYEAR)+1))
  lcFisDes  = "Current"
  *E300789,1 Hesham (Start)
  *lcCompFis = CCOMP_ID+CFISFYEAR
  lcCompFis = CFISFYEAR
  *E300789,1 Hesham (End)
*E300692,5 Change file name and path from SYCFSPRD to FSPRD          
  *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (gcSysHome+"SYCFSPRD");
   WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = lcCompFis;
    INTO DBF (gcWorkDir+lc_TempPR)
  *E300789,1 Hesham (Start)
  *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (lcDataDir+"FSPRD");
   WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = lcCompFis;
    INTO DBF (gcWorkDir+lc_TempPR)

  SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
    FROM (lcDataDir+"FSPRD");
   WHERE CFISFYEAR+CFSPPRDID = lcCompFis;
    INTO DBF (gcWorkDir+lc_TempPR)

  *E300789,1 Hesham (Start)
*E300692,5 end    
    

  SELECT(lc_TempPR)
  COPY STRUCTURE TO &gcWorkDir.&lcTmpCPrd
  COPY STRUCTURE TO &gcWorkDir.&lcTmpNPrd

  SELECT 0
  USE &gcWorkDir.&lcTmpCPrd EXCLUSIVE

  SELECT 0
  USE &gcWorkDir.&lcTmpNPrd EXCLUSIVE
ENDIF  

SELECT(lcTmpFisHD)

*E300683,4 Call *.SPR from screens directory
* DO SmfisHd.SPR 
DO (gcScrDir + gcWinAppl + '\SmfisHd.SPR')
*E300683,4 end   

glQuitting = .T.

*!**************************************************************************
*!
*!      Function: lfwNoOfPer
*!
*!**************************************************************************
*
FUNCTION lfwNoOfPer

lcOldNoPrd = &lcTmpFisHD..cfisnoprd

*!**************************************************************************
*!
*!      Function: lfvNoOfPer
*!
*!**************************************************************************
*
FUNCTION lfvNoOfPer

lcFldNm  = 'No. of periods'

lnNoPrd  = INT(VAL(&lcTmpFisHD..cfisnoprd))
lnCurPrd = INT(VAL(&lcTmpFisHD..ccurr_prd))

IF lnCurPrd <> 0 .AND. lnNoPrd < lnCurPrd .AND. lnNoPrd <> 0
  =gfModalGen("QRM00062B00000","DIALOG")
  REPLACE &lcTmpFisHD..cfisnoprd WITH lcOldNoPrd
  _CUROBJ = OBJNUM(cfisnoprd)
ELSE
  IF lnNoPrd <= 0 .OR. lnNoPrd > 13
    =gfModalGen("QRM00063B00000","DIALOG",'13')
    REPLACE &lcTmpFisHD..cfisnoprd WITH lcOldNoPrd
    _CUROBJ = OBJNUM(cfisnoprd)
  ELSE
    REPLACE &lcTmpFisHD..cfisnoprd WITH PADL(lnNoPrd,2,'0')
    IF lcOldNoPrd <> &lcTmpFisHD..cfisnoprd
    *E300789,1 Hesham (Start)
      *lcKeyExp = laCompany[puCompany,2]+&lcTmpFisHD..CFISFYEAR
      lcKeyExp = &lcTmpFisHD..CFISFYEAR
    *E300789,1 Hesham (End) 
    *E300692,5 Change file name and path from SYCFSPRD to FSPRD            
      *SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM &gcSysHome.SYCFSPRD;
       WHERE SYCFSPRD.CCOMP_ID+SYCFSPRD.CFISFYEAR = lcKeyExp;
        INTO DBF &gcWorkDir.&lc_TempPR
      *E300789,1 Hesham (Start)     
      *SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM &lcDataDir.FSPRD;
       WHERE CCOMP_ID+CFISFYEAR = lcKeyExp;
        INTO DBF &gcWorkDir.&lc_TempPR
      SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM &lcDataDir.FSPRD;
       WHERE CFISFYEAR = lcKeyExp;
        INTO DBF &gcWorkDir.&lc_TempPR

      *E300789,1 Hesham (End)  
     *E300692,5 end   
        
      llAcptPrd = .T.  
      =lfvAcptPrd()      && To know if accept to change periods or not.
      IF llOldFlg
        llOldFlg = .F.
        REPLACE &lcTmpFisHD..cfisnoprd WITH lcOldNoPrd
      ENDIF
    ENDIF  
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfwEndDate
*!
*!**************************************************************************
*
FUNCTION lfwEndDate

ldOldEnd = &lcTmpFisHD..dfisendat

IF llFrstEndDat .AND. &lcTmpFisHD..CFISYSTAT = 'C'
  ldOldEndDate = &lcTmpFisHD..dfisendat
  llFrstEndDat = .F.
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvEndDate
*!
*!**************************************************************************
*
FUNCTION lfvEndDate

lcFldNm = 'End date'

SELECT(lcTmpFisHD)

DO CASE
  CASE dfisendat < laCompany[puCompany,7] .AND. CFISYSTAT = 'C'
    =gfModalGen("QRM00116B00000","DIALOG",cfisfyear)
    REPLACE dfisendat WITH ldOldEnd
    _CUROBJ = OBJNUM(dfisendat)
  
  CASE YEAR(dfisendat) <> VAL(cfisfyear)
    =gfModalGen("QRM00068B00000","DIALOG",cfisfyear)
    REPLACE dfisendat WITH ldOldEnd
    _CUROBJ = OBJNUM(dfisendat)

  CASE dfisendat <= dfisbgdat
    =gfModalGen("QRM00069B00000","DIALOG")
    REPLACE &lcTmpFisHD..dfisendat WITH ldOldEnd
    _CUROBJ = OBJNUM(dfisendat)

  CASE GOMONTH(dfisbgdat,12) < dfisendat .AND. !EMPTY(dfisbgdat)
    =gfModalGen("QRM00070B00000","DIALOG")
    REPLACE &lcTmpFisHD..dfisendat WITH ldOldEnd
    _CUROBJ = OBJNUM(dfisendat)
  
  OTHERWISE
    IF ldOldEnd <> dfisendat
    *E300789,1 Hesham (Start)
      *lcKeyExp = laCompany[puCompany,2]+&lcTmpFisHD..CFISFYEAR
      lcKeyExp = &lcTmpFisHD..CFISFYEAR
    *E300789,1 Hesham (End)  
    *E300692,5 Change file name and path from SYCFSPRD to FSPRD              
      *SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM (gcSysHome+"SYCFSPRD");
       WHERE SYCFSPRD.CCOMP_ID+SYCFSPRD.CFISFYEAR = lcKeyExp;
        INTO DBF (gcWorkDir+lc_TempPR)
      *E300789,1 Hesham (Start)  
      *SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM (lcDataDir+"FSPRD");
       WHERE CCOMP_ID+CFISFYEAR = lcKeyExp;
        INTO DBF (gcWorkDir+lc_TempPR)
      SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus' ;
        FROM (lcDataDir+"FSPRD");
       WHERE CFISFYEAR = lcKeyExp;
        INTO DBF (gcWorkDir+lc_TempPR)
        
       *E300789,1 Hesham (End)
    *E300692,5 end    
        
      llAcptPrd = .T.  
      =lfvAcptPrd()  && To know if accept to change periods or not
      IF llOldFlg
        llOldFlg  = .F.
        REPLACE &lcTmpFisHD..dfisendat WITH ldOldEnd
      ELSE
        llUpdEndDate = .T.
      ENDIF
    ENDIF
ENDCASE

IF &lcTmpFisHD..CFISYSTAT = 'C'
  IF ldOldEndDate = &lcTmpFisHD..dfisendat
   llDatChange = IIF(llDatChange,llDatChange,.F.)
  ELSE
    llDatChange = .T.
    ldDatChange = &lcTmpFisHD..dfisendat
  ENDIF  
ENDIF  

ldEndYearDat = &lcTmpFisHD..dfisendat + 1
lcNoOfPrd    = &lcTmpFisHD..cFisNoPrd    

*!**************************************************************************
*!
*!      Function: lfvAcptPrd
*!
*!**************************************************************************
*
FUNCTION lfvAcptPrd

IF llAcptPrd
  lnOption =gfModalGen("QRM00074B00018","ALERT",lcFldNm)
  DO CASE
    CASE lnOption = 1               && Reenter Periods
      llNewPrds   = .T.
      llDefPrds   = .F.
      _CUROBJ     = OBJNUM(pbPeriod)
      KEYBOARD "{ENTER}"

    CASE lnOption = 2               && Cancel
      llOldFlg    = .T.
      llPeriod    = .F.

    CASE lnOption = 3               && Accept Default
      llNewPrds   = .T.
      llDefPrds   = .T.
      _CUROBJ     = OBJNUM(pbPeriod)
      KEYBOARD "{ENTER}"
  ENDCASE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvPeriod
*!
*!**************************************************************************
*
FUNCTION lfvPeriod

llPeriod  = .F.

IF &lcTmpFisHD..CFISYSTAT = 'C'
  IF RECCOUNT(lcTmpCPrd) > 0
    SELECT *;
      FROM (gcWorkDir+lcTmpCPrd);
      INTO DBF (gcWorkDir+lc_TempPR)
    *B602658,5 AMM Remove the company ID from the expression.
    *lcCompFis = &lcTmpFisHD..CCOMP_ID+&lcTmpFisHD..CFISFYEAR  
    lcCompFis = &lcTmpFisHD..CFISFYEAR  
    *B602658,5 AMM end
  ELSE
    llAcptPrd = .T.
    llDefPrds = .T.
    SELECT(lc_TempPR)
  ENDIF
ELSE
  IF RECCOUNT(lcTmpNPrd) > 0
    SELECT *;
      FROM (gcWorkDir+lcTmpNPrd);
      INTO DBF (gcWorkDir+lc_TempPR)
    *B602658,5 AMM Remove the company ID from the program.
    *lcCompFis = &lcTmpFisHD..CCOMP_ID+&lcTmpFisHD..CFISFYEAR
    lcCompFis = &lcTmpFisHD..CFISFYEAR
    *B602658,5 AMM end
  ELSE
    llAcptPrd = .F.
    SELECT(lc_TempPR)
  ENDIF
ENDIF

*E300683,4 Call programs from PRGS directory
*DO SMFSPRD WITH .T.
DO (gcAppHome + gcWinAppl + '\SMFSPRD ') WITH .T. 
*E300683,4 end
SELECT(lcTmpFisHD)

IF &lcTmpFisHD..CFISYSTAT = 'C' .AND. llPeriod
  SELECT *;
    FROM &gcWorkDir.&lc_TempPR;
    INTO DBF &gcWorkDir.&lcTmpCPrd
ENDIF

IF &lcTmpFisHD..CFISYSTAT = 'N' .AND. llPeriod
  SELECT *;
    FROM (gcWorkDir+lc_TempPR);
    INTO DBF (gcWorkDir+lcTmpNPrd)
ENDIF

SELECT(lcTmpFisHD)

*!**************************************************************************
*!
*!      Function: lfvYear
*!
*!**************************************************************************
*
FUNCTION lfvYear

SELECT(lcTmpFisHD)
IF CFISYSTAT = 'C'
  ldEndYearDat  = &lcTmpFisHD..dfisendat + 1
  lcFisDes   = "Next"
  SKIP + 1
  IF llUpdEndDate 
  *E300692,5 Change file name from SYCFISHD to FISHD                
    *REPLACE &lcTmpFisHD..dfisbgdat WITH ldEndYearDat ,;
            &lcTmpFisHD..dfisendat WITH GOMONTH(ldEndYearDat-1,INT(VAL(SYCFISHD.CFISNOPRD)))
            
    REPLACE &lcTmpFisHD..dfisbgdat WITH ldEndYearDat ,;
            &lcTmpFisHD..dfisendat WITH GOMONTH(ldEndYearDat-1,INT(VAL(&lcFisHd..CFISNOPRD)))
   *E300692,5 end         
            
    llUpdEndDate = .F.        
  ENDIF  
  lcYear   = "C\<urrent"+' '+ALLTRIM(STR(VAL(CFISFYEAR)-1))
  SHOW GET pbYear,1 PROMPT lcYear
ELSE
  lcFisDes = "Current"
  SKIP - 1
  lcYear   = "Ne\<xt"+' '+ALLTRIM(STR(VAL(CFISFYEAR)+1))
  SHOW GET pbYear,1 PROMPT lcYear
ENDIF

SHOW GETS

*!**************************************************************************
*!
*!      Function: lfvNote
*!
*!**************************************************************************
*
FUNCTION lfvNote

laScrMode    = .F.
laScrMode[3] = .T.
=gfZoom('mfiscomnt')

*!**************************************************************************
*!
*!      Function: lfBeginDat
*!
*!**************************************************************************
*
FUNCTION lfBeginDat
PARAMETERS lnCount

DO CASE
  CASE lnCount = 1
  *E300692,5 Change file name from SYCFISHD to FISHD                  
    *ldBegDate = SYCFISHD.DFISBGDAT
    ldBegDate = &lcFisHd..DFISBGDAT
  *E300692,5 end  
    IF lcClosStat $ 'IN'
    *E300692,5 Change file name from SYCFISHD to FISHD                    
      *ldEndYearDat = SYCFISHD.DFISENDAT
      ldEndYearDat = &lcFisHd..DFISENDAT
    *E300692,5 end  
    ELSE
      ldEndYearDat = laCompany[puCompany,10] 
    ENDIF  
  CASE lnCount = 2
    IF lcClosStat $ 'IN'
    *E300692,5 Change file name from SYCFISHD to FISHD                      
      *ldBegDate = SYCFISHD.DFISENDAT + 1
      *ldEndYearDat = GOMONTH(SYCFISHD.DFISENDAT,12) 
      ldBegDate = &lcFisHd..DFISENDAT + 1
      ldEndYearDat = GOMONTH(&lcFisHd..DFISENDAT,12) 
    *E300692,5 end  
      
    ELSE
      ldBegDate = laCompany[puCompany,10] + 1
      ldEndYearDat = GOMONTH(laCompany[puCompany,10],12)
    ENDIF  
  CASE lnCount = 3
    IF lcClosStat $ 'IN'
    *E300692,5 Change file name from SYCFISHD to FISHD                        
      *ldBegDate = GOMONTH(SYCFISHD.DFISENDAT+1,12)
      *ldEndYearDat = GOMONTH(SYCFISHD.DFISENDAT,24) 
      ldBegDate = GOMONTH(&lcFisHd..DFISENDAT+1,12)
      ldEndYearDat = GOMONTH(&lcFisHd..DFISENDAT,24) 
    *E300692,5  
      
    ELSE
      ldBegDate = GOMONTH(laCompany[puCompany,10]+1,12)
      ldEndYearDat = GOMONTH(laCompany[puCompany,10],24)
    ENDIF  
ENDCASE  
RETURN(ldBegDate)

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

lcFiscalY = STR(INT(VAL(laCompany[puCompany,4])+1),4)

SELECT(lcTmpFisHD)

IF cFisYStat = 'C' .AND. llDatChange
  IF RECCOUNT(lcTmpCPrd) = 0
    SELECT(lcTmpFisHD)
    DO WHILE !llPeriod
      llAcptPrd = .T.
      llPeriod  = .T.
      =lfvAcptPrd()  && To know if accept to change periods or not
      IF llPeriod
        =lfvPeriod()
      ENDIF  
    ENDDO  
  ENDIF
  ldEndYearDat  = &lcTmpFisHD..dfisendat + 1
  SKIP + 1
*E300692,5 Change file name from SYCFISHD to FISHD                          
  *REPLACE dfisbgdat WITH ldEndYearDat,;
          dfisendat WITH GOMONTH(ldEndYearDat-1,INT(VAL(SYCFISHD.CFISNOPRD)))
          
  REPLACE dfisbgdat WITH ldEndYearDat,;
          dfisendat WITH GOMONTH(ldEndYearDat-1,INT(VAL(&lcFisHd..CFISNOPRD)))
*E300692,5        
          
  llPeriod = .F.
  DO WHILE !llPeriod        
    llAcptPrd = .T.        
    llPeriod  = .T.
    =lfvPeriod()
  ENDDO  
ELSE
  IF RECCOUNT(lcTmpCPrd) = 0
    SELECT(lcTmpFisHD)
    IF cFisYStat <> 'C'
      SKIP - 1
    ENDIF  
    *B602658,5 AMM Remove the company ID from the expression
    *lcCompFis = CCOMP_ID + CFISFYEAR
    lcCompFis = CFISFYEAR
    *B602658,5 AMM end
  
  *E300692,5 Change file name from SYCFSPRD to FSPRD                            
    *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
      FROM (gcSysHome+"SYCFSPRD");
     WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = lcCompFis;
      INTO DBF (gcWorkDir+lcTmpCPrd)
    *E300789,1 Hesham (Start)      
    *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
      FROM (lcDataDir+"FSPRD");
     WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = lcCompFis;
      INTO DBF (gcWorkDir+lcTmpCPrd)
    SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
      FROM (lcDataDir+"FSPRD");
     WHERE CFISFYEAR+CFSPPRDID = lcCompFis;
      INTO DBF (gcWorkDir+lcTmpCPrd)

    *E300789,1 Hesham (End)   
   *E300692,5 end   
      
  ENDIF

  IF RECCOUNT(lcTmpNPrd) = 0
    SELECT(lcTmpFisHD)
    IF cFisYStat = 'C'
      SKIP + 1
    ENDIF  
    llPeriod = .F.
    DO WHILE !llPeriod
      llPeriod  = .T.
      llAcptPrd = .T.        
      =lfvPeriod()
    ENDDO  
  ENDIF
ENDIF  

lcTmpCompair = gfTempName()

IF RECCOUNT(lcTmpCPrd) > 0
*E300692,5 Change file name and path from SYCFSPRD to FSPRD                              
  *SELECT SYCFSPRD.CCOMP_ID,SYCFSPRD.CFISFYEAR,;
         SYCFSPRD.CFSPPRDID,SYCFSPRD.DFSPPBGDT,;
         SYCFSPRD.DFSPPENDT,&lcTmpCPrd..DFSPPBGDT,;
         &lcTmpCPrd..DFSPPENDT,;
         IIF(DTOS(sycfsprd.dfsppbgdt)+DTOS(sycfsprd.dfsppendt) =;
             DTOS(&lcTmpCPrd..dfsppbgdt)+DTOS(&lcTmpCPrd..dfsppendt);
             ,.T.,.F.) AS 'lStatus';
    FROM (gcSysHome+"SYCFSPRD"),(gcWorkDir+lcTmpCPrd);
   WHERE sycfsprd.ccomp_id + sycfsprd.cfisfyear + sycfsprd.cfspprdid =;
         &lcTmpCPrd..ccomp_id + &lcTmpCPrd..cfisfyear+ &lcTmpCPrd..cfspprdid;
   GROUP BY SYCFSPRD.CFSPPRDID;
    INTO CURSOR &lcTmpCompair
  *E300789,1 Hesham (Start)    
  *SELECT &lcFsPrd..CCOMP_ID,&lcFsPrd..CFISFYEAR,;
         &lcFsPrd..CFSPPRDID,&lcFsPrd..DFSPPBGDT,;
         &lcFsPrd..DFSPPENDT,&lcTmpCPrd..DFSPPBGDT,;
         &lcTmpCPrd..DFSPPENDT,;
         IIF(DTOS(&lcFsPrd..dfsppbgdt)+DTOS(&lcFsPrd..dfsppendt) =;
             DTOS(&lcTmpCPrd..dfsppbgdt)+DTOS(&lcTmpCPrd..dfsppendt);
             ,.T.,.F.) AS 'lStatus';
    FROM (lcFSPRD),(gcWorkDir+lcTmpCPrd);
   WHERE &lcfsprd..ccomp_id + &lcfsprd..cfisfyear + &lcfsprd..cfspprdid =;
         &lcTmpCPrd..ccomp_id + &lcTmpCPrd..cfisfyear+ &lcTmpCPrd..cfspprdid;
   GROUP BY &lcFSPRD..CFSPPRDID;
    INTO CURSOR &lcTmpCompair

  SELECT &lcFsPrd..CFISFYEAR,&lcFsPrd..CFSPPRDID,&lcFsPrd..DFSPPBGDT,;
         &lcFsPrd..DFSPPENDT,&lcTmpCPrd..DFSPPBGDT,;
         &lcTmpCPrd..DFSPPENDT,;
         IIF(DTOS(&lcFsPrd..dfsppbgdt)+DTOS(&lcFsPrd..dfsppendt) =;
             DTOS(&lcTmpCPrd..dfsppbgdt)+DTOS(&lcTmpCPrd..dfsppendt);
             ,.T.,.F.) AS 'lStatus';
    FROM (lcFSPRD),(gcWorkDir+lcTmpCPrd);
   WHERE  &lcfsprd..cfisfyear + &lcfsprd..cfspprdid =;
          &lcTmpCPrd..cfisfyear+ &lcTmpCPrd..cfspprdid;
   GROUP BY &lcFSPRD..CFSPPRDID;
    INTO CURSOR &lcTmpCompair

  *E300789,1 Hesham (End)
  
  *E300692,5 end  

  SELECT(lcTmpCompair)
  
  LOCATE FOR !lStatus
  IF FOUND()
    llPrdChange = .T.
  ENDIF
ENDIF

*E300692,5 Change file name from SYCFSPRD to FSPRD
*SELECT SYCFSPRD
SELECT (lcFsPrd)
*E300692,5 end
SET ORDER TO TAG COMFYRPRDI

IF laCompany[puCompany,7] <> laCompany[puCompany,10]
*E300789,1 Hesham (Start)
  *=SEEK(laCompany[puCompany,2]+laCompany[puCompany,4]+laCompany[puCompany,3])
  =SEEK(laCompany[puCompany,4]+laCompany[puCompany,3])
*E300789,1 Hesham (End)  
*B601543,1 M.H Begin.
*  DELETE REST FOR sycfsprd.ccomp_id  = laCompany[puCompany,2];
              AND sycfsprd.dfsppendt > laCompany[puCompany,10]
*E300692,5 Change file name from SYCFSPRD to FSPRD  
  *DELETE REST WHILE sycfsprd.ccomp_id  = laCompany[puCompany,2];
                FOR sycfsprd.dfsppendt > laCompany[puCompany,10]
*E300789,1 Hesham (Start)                
  *DELETE REST WHILE ccomp_id  = laCompany[puCompany,2];
                FOR dfsppendt > laCompany[puCompany,10]
  DELETE REST FOR dfsppendt > laCompany[puCompany,10]

*E300789,1 Hesham (End)
 *E300692,5 end               
                
*B601543,1 M.H End.
ELSE
  lcNewNext = STR(INT(VAL(laCompany[puCompany,4])+1),4)
  *E300789,1 Hesham (Start)
  *=SEEK(laCompany[puCompany,2]+lcNewNext+'01')
  =SEEK(lcNewNext+'01')
  *E300789,1 Hesham (End)
*B601543,1 M.H Begin.
*  DELETE REST
*E300692,5 Change file name from SYCFSPRD to FSPRD    
  *DELETE REST WHILE sycfsprd.ccomp_id = laCompany[puCompany,2]
  *E300789,1 Hesham (Start)  
  *DELETE REST WHILE ccomp_id = laCompany[puCompany,2]
  DELETE REST 
  *E300789,1 Hesham (End)
*E300692,5 end  
**B601543,1 M.H End.
ENDIF

IF RECCOUNT(lcTmpCPrd) > 0
  SELECT(lcTmpCPrd)   
  REPLACE ALL CADD_USER WITH gcUser_Id,;
              DADD_DATE WITH ldTodayDate ,;
              CADD_TIME WITH gfGetTime()
*E300692,5 Change file name from SYCFSPRD to FSPRD      
  *SELECT SYCFSPRD
  SELECT (lcFsPrd)
*E300692,5 end  
  APPEND FROM &gcWorkDir.&lcTmpCPrd
ENDIF  

IF RECCOUNT(lcTmpNPrd) > 0
  SELECT(lcTmpNPrd)
  REPLACE ALL CADD_USER WITH gcUser_Id,;
              DADD_DATE WITH ldTodayDate ,;
              CADD_TIME WITH gfGetTime()
*E300692,5 Change file name from SYCFSPRD to FSPRD        
  *SELECT SYCFSPRD
  SELECT (lcFsPrd)
  *E300692,5 end
  APPEND FROM &gcWorkDir.&lcTmpNPrd
ENDIF

*E300692,5 Change file name and path from SYCFSHLD to FSHLD        
*SELECT SYCFSHLD.CCOMP_ID,;
       STR(INT(VAL(CFISFYEAR)+1),4) AS 'CFISFYEAR',;
       GOMONTH(DFSHHDATE,12) AS 'DFSHHDATE',;
       SYCFSHLD.CFSHHSTAT,SYCFSHLD.CFSHHDESC,;
       gcUser_Id AS 'CADD_USER',ldTodayDate AS 'DADD_DATE',;
       gfGetTime() AS 'CADD_TIME',SYCFSHLD.LLOK_STAT,;
       SYCFSHLD.DLOK_DATE,SYCFSHLD.CLOK_USER,;
       SYCFSHLD.CLOK_TIME;
  FROM (gcSysHome+"SYCFSHLD");
 WHERE SYCFSHLD.CCOMP_ID = laCompany[puCompany,2];
   AND SYCFSHLD.CFISFYEAR = lcFiscalY;
  INTO DBF (gcWorkDir+lcTmpNHold)
 
*E300789,1 Hesham (Start) 
*SELECT CCOMP_ID,;
       STR(INT(VAL(CFISFYEAR)+1),4) AS 'CFISFYEAR',;
       GOMONTH(DFSHHDATE,12) AS 'DFSHHDATE',;
       CFSHHSTAT,CFSHHDESC,;
       gcUser_Id AS 'CADD_USER',ldTodayDate AS 'DADD_DATE',;
       gfGetTime() AS 'CADD_TIME',LLOK_STAT,;
       DLOK_DATE,CLOK_USER,;
       CLOK_TIME;
  FROM (lcDataDir+"FSHLD");
 WHERE CCOMP_ID = laCompany[puCompany,2];
   AND CFISFYEAR = lcFiscalY;
  INTO DBF (gcWorkDir+lcTmpNHold)
  
*B602658,5 AMM Remove the company ID from the where condition
*SELECT STR(INT(VAL(CFISFYEAR)+1),4) AS 'CFISFYEAR',;
       GOMONTH(DFSHHDATE,12) AS 'DFSHHDATE',;
       CFSHHSTAT,CFSHHDESC,;
       gcUser_Id AS 'CADD_USER',ldTodayDate AS 'DADD_DATE',;
       gfGetTime() AS 'CADD_TIME',LLOK_STAT,;
       DLOK_DATE,CLOK_USER,;
       CLOK_TIME;
  FROM (lcDataDir+"FSHLD");
 WHERE CCOMP_ID = laCompany[puCompany,2];
   AND CFISFYEAR = lcFiscalY;
  INTO DBF (gcWorkDir+lcTmpNHold)
SELECT STR(INT(VAL(CFISFYEAR)+1),4) AS 'CFISFYEAR',;
       GOMONTH(DFSHHDATE,12) AS 'DFSHHDATE',;
       CFSHHSTAT,CFSHHDESC,;
       gcUser_Id AS 'CADD_USER',ldTodayDate AS 'DADD_DATE',;
       gfGetTime() AS 'CADD_TIME',LLOK_STAT,;
       DLOK_DATE,CLOK_USER,;
       CLOK_TIME;
  FROM (lcDataDir+"FSHLD");
 WHERE CFISFYEAR = lcFiscalY;
  INTO DBF (gcWorkDir+lcTmpNHold)
*B602658,5 AMM end
*E300789,1 Hesham (Start)
*E300692,5 end  
  

IF RECCOUNT(lcTmpNHold) > 0
*E300692,5 Change file namefrom SYCFSHLD to FSHLD   
  *SELECT SYCFSHLD
  SELECT (lcFsHld)
*E300692,5 end  
  APPEND FROM (gcWorkDir+lcTmpNHold)
ENDIF

SELECT(lcTmpFisHD)
REPLACE ALL CADD_USER WITH gcUser_Id,;
            DADD_DATE WITH ldTodayDate,;
            CADD_TIME WITH gfGetTime()

*E300692,5 Change file namefrom SYCFishd to FiSHD   
*SELECT SYCFISHD
SELECT (lcFisHd)
*E300692,5 end
SET ORDER TO TAG COMPFYEAR
lcOldYear = STR(INT(VAL(laCompany[puCompany,4])-1),4)
*E300789,1 Hesham (Start)
*=SEEK(laCompany[puCompany,2]+lcOldYear)
=SEEK(lcOldYear)
*E300789,1 Hesham (End)
REPLACE cFisYStat WITH 'H'
*E300789,1 Hesham (Start)
*DELETE REST FOR cComp_Id = laCompany[puCompany,2];
            .AND. cFisfYear <> lcOldYear
DELETE REST FOR cFisfYear <> lcOldYear
*E300789,1 Hesham (End)
APPEND FROM (gcWorkDir+lcTmpFisHD)

*B605667,1 Add new year records for all customers and styles. [Begin]
LOCATE FOR cFisYStat = "N"
IF FOUND()
  PRIVATE llOpnCust , llOpnStyle , llOpnCsHst , llOpnIcHst , llOpnVend , llOpnVnHst , ;
          lcNewYear , lcStyTitle
  STORE .F. TO llOpnCust , llOpnStyle , llOpnCsHst , llOpnIcHst , llOpnVend , llOpnVnHst
  lcNewYear = cFisFYear 

  IF !USED('Customer')
    llOpnCust = .T.
    USE (gcDataDir+'Customer.DBF') IN 0 SHARE ORDER TAG Customer
  ENDIF

  IF SEEK('M','Customer')
    IF !USED('ArCusHst')
      llOpnCsHst = .T.
      USE (gcDataDir+'ArCusHst.DBF') IN 0 SHARE ORDER TAG ActHst
    ENDIF

    SELECT Customer
    SCAN REST WHILE Type + Account + Store = 'M'      
      IF !SEEK(Account + lcNewYear,'ArCusHst')
        WAIT WINDOW 'Adding a record for Account : ' + Account + ' for year : ' + lcNewYear + ' in Customer history file.' NOWAIT
        INSERT INTO ArCusHst (Account , cFisFYear , cAdd_User , dAdd_Date , cAdd_Time) ;
               VALUES (Customer.Account , lcNewYear , gcUser_Id , gdSysDate , gfGetTime())
      ENDIF
    ENDSCAN
  
    IF llOpnCsHst
      USE IN ArCusHst
    ENDIF
  ENDIF  

  IF llOpnCust
    USE IN Customer
  ENDIF

  IF !USED('Style')
    llOpnStyle = .T.
    USE (gcDataDir+'Style.DBF') IN 0 SHARE
  ENDIF

  SELECT Style
  LOCATE
  IF !EOF('Style')
    lcStyTitle = gfItemMask('HI')
    IF !USED('IcStyHst')
      llOpnStHst = .T.
      USE (gcDataDir+'IcStyHst.DBF') IN 0 SHARE ORDER TAG StyHst
    ENDIF
    
    SELECT Style
    SCAN
      IF !SEEK(Style + lcNewYear,'IcStyHst')
        WAIT WINDOW 'Adding a record for ' + lcStyTitle + " : " + Style + ' for year : ' + lcNewYear + ' in Style history file.' NOWAIT
        INSERT INTO IcStyHst (Style , cFisFYear , cAdd_User , dAdd_Date , cAdd_Time) ;
               VALUES (Style.Style , lcNewYear , gcUser_Id , gdSysDate , gfGetTime())
      ENDIF
    ENDSCAN

    IF llOpnStHst
      USE IN IcStyHst
    ENDIF
  ENDIF
  
  IF llOpnStyle
    USE IN Style
  ENDIF

  IF !USED('ApVendor')
    llOpnVend = .T.
    USE (gcDataDir+'ApVendor.DBF') IN 0 SHARE
  ENDIF

  SELECT ApVendor
  LOCATE
  IF !EOF('ApVendor')
    IF !USED('ApVenHst')
      llOpnVnHst = .T.
      USE (gcDataDir+'ApVenHst.DBF') IN 0 SHARE ORDER TAG VendYear
    ENDIF
    
    SELECT ApVendor
    SCAN
      IF !SEEK(cVendCode + lcNewYear,'ApVenHst')
        WAIT WINDOW 'Adding a record for Vendor code : ' + cVendCode + ' for year : ' + lcNewYear + ' in Style history file.' NOWAIT
        INSERT INTO ApVenHst (cVendCode , cFisFYear , cAdd_User , dAdd_Date , cAdd_Time) ;
               VALUES (ApVendor.cVendCode , lcNewYear , gcUser_Id , gdSysDate , gfGetTime())
      ENDIF
    ENDSCAN

    IF llOpnVnHst
      USE IN ApVenHst
    ENDIF
  ENDIF
  
  IF llOpnVend
    USE IN ApVendor
  ENDIF

  WAIT CLEAR
ENDIF  
*B605667,1 Add new year records for all customers and styles. [End]

IF USED(lcTmpCompair)
  USE IN &lcTmpCompair
ENDIF

IF USED(lcTmpNHold)
  USE IN &lcTmpNHold
ENDIF
ERASE &gcWorkDir.&lcTmpNHold+'.DBF'

IF USED(lcTmpNPrd)
  USE IN &lcTmpNPrd
ENDIF
ERASE &gcWorkDir.&lcTmpNPrd+'.DBF'
ERASE &gcWorkDir.&lcTmpNPrd+'.CDX'
ERASE &gcWorkDir.&lcTmpNPrd+'.FPT'

IF USED(lcTmpCPrd)
  USE IN &lcTmpCPrd
ENDIF
ERASE &gcWorkDir.&lcTmpCPrd+'.DBF'
ERASE &gcWorkDir.&lcTmpCPrd+'.CDX'
ERASE &gcWorkDir.&lcTmpCPrd+'.FPT'

SELECT(lcTmpFisHD)
ZAP

SELECT(lc_TempPR)
ZAP

CLEAR READ
