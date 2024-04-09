*:************************************************************************
*: Program file  : SMWFSYER.PRG  N#000185
*: Program desc. : FISCAL YEAR
*:         System: Aria advantage series
*:         Module: SYSTEM MANAGER
*:      Developer: 
*:           Date: 
*:************************************************************************
*: Calls : 
*:************************************************************************
*: Passed Parameters  : 
*:********************************************************************
*: Modification :
*:B#804025,1 SSH 25/03/2001 Incorrect inital values for the long and shot
*:B#804025,1 SSH 25/03/2001 discreption,and initiate the period with current
*:B#804025,1 SSH 25/03/2001 Month
*:************************************************************************
PARAMETERS pcComp_ID

EXTERNAL ARRAY laData
DECLARE laTStat[4,1],laTYears[4]
DECLARE laHolDates[1,1]
lcOldDat = SET("DATE")
SET DATE "DMY"
lcDataDir  = ''       && Var to hold the company data path
lcToSetFlt = ''
lcOldCurPr = ''       && Var to keep old value of Current period
lcFFScrHlp = ""       && Helping text variable.
lcFisDes   = ''       && Var to keep 'Perivious or Current or Next'
lcFldNm    = ''       && Var to hold text in one of the messages
lc_TempHd  = ''       && Var to hold name of Holidays temp. file
lcYear     = STR(YEAR(DATE()),4)
laData[5]  = PADL(MONTH(DATE()),2,'0')
laData[6]  = CTOD("01\01\"+PADR(lcYear,4))
laData[7]  = CTOD("31\12\"+PADR(lcYear,4))
laData[8]  = ""
laData[9]  = ""
llUpdHd    = .F.      && Flag to know is there any modification in Holidays
llUpdPr    = .F.      && Flag to know is there any modification in Periods

llOldFlg   = .F.      && Flag to know that return cancel from lfAcptPrd

llNewPrds = .F.       && Flag to be used if creat periods for 
                      && first time or when chang periods or dates.
llDefPrds = .F.       && Flag to be used if creat default 
                      && periods is required.
llAcptPrd = .F.       && Flag to tell if the periods of the year 
                      && is accepted before for the same record.

ibComp     = 1
lcComp     = " "
STORE 0 TO cbDay_1,cbDay_2,cbDay_3,cbDay_4,cbDay_5,cbDay_6,cbDay_7
lcScFields = 'CCOMP_ID,SYCFISHD.CFISFYEAR,SYCFISHD.CFISYSTAT,SYCFISHD.CFISNOPRD,SYWCOMP.CCURR_PRD,SYCFISHD.DFISBGDAT,SYCFISHD.DFISENDAT,SYCFISHD.CFISSHEAD,SYCFISHD.CFISLHEAD,SYCFISHD.CFISNONWD,SYCFISHD.MFISCOMNT'

IF !WEXIST(gcBaseWind)
  lc_TempHd = gfTempName()    && Temporary file of the holiday
ENDIF
lcModal    = "MODAL"
IF SEEK(pcComp_ID,"SYWCOMP")
  laData[1] = pcComp_ID
  =lfOpenCmpData()
  SELECT SYCFISHD
  LOCATE FOR cFisyStat ='C'
  SCATTER FIELDS &lcScFields MEMO TO laData
  *:B#804025,1 SSH 25/03/2001 Incorrect inital values for the long and shot
  *:B#804025,1 SSH 25/03/2001 discreption
  IF ALLTRIM(laData[8]) == "Fis. year"
    laData[8]  = SUBSTR(laData[8],1,10)+" "+lcYear
  ENDIF
  IF ALLTRIM(laData[9]) == "Fis. year"
    laData[9]  = laData[8]
  ENDIF
  *:B#804025,1 SSH [End]
  lcComp = SYWCOMP.cComp_Id+" - "+SYWCOMP.cCom_Name
ENDIF
*** Set the non working days for each company ***
FOR lnCount = 1 TO 7
  lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(ALLTRIM(STR(lnCount)) $ laData[10],1,0)
ENDFOR

DO (gcScrDir + gcWinAppl + '\SmWfsyer.SPR')
SELECT SYWCOMP
SET FILTER TO
*** If exit from this program, erasing all the temp. files ***
IF USED(lc_TempHd)
  USE IN ALIAS(lc_TempHd)
ENDIF
ERASE (gcWorkDir+lc_TempHd+".DBF")
ERASE (gcWorkDir+lc_TempHd+".FPT")
ERASE (gcWorkDir+lc_TempHd+".CDX")
IF USED('SYCFISHD')
  USE IN SYCFISHD
ENDIF

IF USED('SYCFSPRD')
  USE IN SYCFSPRD
ENDIF

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for lfvData_2.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvData_2()
*!*************************************************************
*!
FUNCTION lfvData_2
 
IF !EMPTY(laData[2])
  laData[1] = IIF(EMPTY(laData[1]),pcComp_ID,laData[1])
  
  IF !BETWEEN(VAL(laData[2]),1900,9998)
    laData[2] = SPACE(4)
    _CUROBJ   = OBJNUM(laData[2])
    RETURN
  ELSE
    IF SEEK(ALLTRIM(laData[2]))
      SCATTER FIELDS &lcScFields MEMO TO laData
      *:B#804025,1 SSH 25/03/2001 Incorrect inital values for the long and shot
      *:B#804025,1 SSH 25/03/2001 discreption
      IF ALLTRIM(laData[8]) == "Fis. year"
        laData[8]  = SUBSTR(laData[8],1,10)+" "+lcYear
      ENDIF
      IF ALLTRIM(laData[9]) == "Fis. year"
        laData[9]  = laData[8]
      ENDIF
      SHOW GETS
      *:B#804025,1 SSH [End]
    ENDIF
    SELECT SYWCOMP
    =SEEK(pcComp_ID)
    REPLACE cCurr_Yer WITH laData[2],; 
            cCurr_Prd WITH laData[5]
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfvData_5
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for lfvData_5.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvData_5()
*!*************************************************************
*!
FUNCTION lfvData_5

IF LASTKEY() = 27
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF

lnNoPrd  = VAL(laData[4])
lnCurPrd = VAL(laData[5])

IF lnCurPrd <= 0 .OR. lnCurPrd > lnNoPrd
  *** Period value must be between 1 and ð...! ***
  *** <  Ok  > ***
  =gfModalGen("QRM00063B00000","DIALOG",laData[4])
  laData[5] = lcOldCurPr
  _CUROBJ = OBJNUM (laData[5])
ELSE 
  laData[5] = RIGHT("0"+ALLTRIM(laData[5]),2)
ENDIF

SELECT SYCFSPRD
GO TOP
SCAN
  REPLACE lFspclsds WITH IIF(VAL(cFspprdid) < VAL(laData[5]),.T.,.F.)
ENDSCAN
SELECT SYWCOMP
=SEEK(pcComp_ID)
REPLACE cCurr_Yer WITH laData[2],; 
        cCurr_Prd WITH laData[5]
SELECT SYCFSPRD
SHOW GET laData[5] 

*!*************************************************************
*! Name      : lfvAddDay
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for lfvData_5.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvAddDay()
*!*************************************************************
*!
FUNCTION lfvAddDay

laData[10] = IIF(EVALUATE(SYS(18)) = 1,;
               ALLTRIM(laData[10])+RIGHT(SYS(18),1),;
               STUFF(ALLTRIM(laData[10]),AT(RIGHT(SYS(18),1),ALLTRIM(laData[10])),1,''))

laData[10] = ALLTRIM(laData[10])
SELECT SYCFISHD
LOCATE FOR cFisyStat ='C'
REPLACE CFISNONWD WITH laData[10]

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for lfvData_5.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvCancel()
*!*************************************************************
*!
FUNCTION lfvCancel

*!*************************************************************
*! Name      : lfvChcDate
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function to check holidayes.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvChcDate()
*!*************************************************************
*!
FUNCTION lfvChcDate
*** To know if there is any holidays not ***
*** within the start & end date..        ***

IF !EMPTY(laHolDates)
  SELECT (lc_TempHd)
  FOR lnCnt = 1 TO ALEN(laHolDates,1)
    IF !BETWEEN(laHolDates[lnCnt],laData[6],laData[7])
      IF SEEK(laData[1]+laData[2]+DTOS(laHolDates[lnCnt]))
        lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)   && Delete
        REPLACE cStatus WITH lcStatus
        DELETE
        llUpdHd   = .T.
        laHolDates[lnCnt] = {}
      ENDIF
    ENDIF
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfOpenCmpData
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : function to open all the data files from sysdir..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfOpenCmpData()
*!*************************************************************
*!
FUNCTION lfOpenCmpData

IF !EMPTY(laData[1])
  lcDataDir = ALLT(LOOKUP(SYWCOMP.CCOM_DDIR,laData[1],SYWCOMP.ccomp_id,'Ccomp_id'))
  lcDataDir = gfGetDataDir(lcDataDir)
  IF USED('SYCFISHD')
    USE IN SYCFISHD
  ENDIF
  USE (lcDataDir+'SYWFSHD') IN 0 AGAIN ALIAS SYCFISHD ORDER TAG COMPFYEAR
  IF USED('SYCFSPRD')
    USE IN SYCFSPRD
  ENDIF
  USE (lcDataDir+'sywfspd') IN 0 AGAIN ALIAS SYCFSPRD ORDER TAG Comfyrprdi
  SELECT SYCFISHD
ENDIF

*!*************************************************************
*! Name      : lfvYearPop
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for year poup..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvYearPop()
*!*************************************************************
*!
FUNCTION lfvYearPop


PRIVATE lnOldAls
lnOldAls = SELECT(0)
*:B#804025,1 SSH 25/03/2001 Initiate the year
*laData[2] = STR(lcYear,4)
laData[2] = lcYear
*:B#804025,1 SSH [End]

SELECT SYWCOMP
=SEEK(pcComp_ID)
REPLACE cCurr_Yer WITH laData[2],; 
        cCurr_Prd WITH laData[5]
SHOW GET laData[5] ENABLE
FLUSH
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvBack
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function previous buttopn..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvBack()
*!*************************************************************
*!
FUNCTION lfvBack
=lfvSavFis()
lcNextActn = 'B'
SET DATE &lcOldDat
=gfCPClose()

*!*************************************************************
*! Name      : lfvFNext
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function next buttopn..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvFNext()
*!*************************************************************
*!
FUNCTION lfvFNext

=lfvYearPop()
=lfvSavFis()
lcNextActn = 'N'
=gfCPClose()

*!*************************************************************
*! Name      : lfChkSess
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : function to check if select year before in the same session..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfChkSess()
*!*************************************************************
*!
FUNCTION lfChkSess

PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF SEEK(pcComp_ID,"SYWCOMP")
  laData[1] = pcComp_ID
  =lfOpenCmpData()  
  SELECT SYCFISHD
  LOCATE FOR cFisyStat = "C"
  IF FOUND()
    lcYear = VAL(SYWCOMP.cCurr_yer)
    lcYear = SYWCOMP.cCurr_yer
    laData[10] = CFISNONWD
    SCATTER FIELDS &lcScFields MEMO TO laData
    lcComp = SYWCOMP.cComp_Id+" - "+SYWCOMP.cCom_Name
    *:B#804025,1 SSH 25/03/2001 Incorrect inital Year and period
    IF EMPTY(lcYear) .OR. ALLTRIM(lcYear) = '0'
      lcYear     = STR(YEAR(DATE()),4)
    ENDIF
    SELECT SYWCOMP
    GO TOP
    laData[5]  = cCurr_Prd
    IF EMPTY(laData[5])
      laData[5]  = PADL(MONTH(DATE()),2,'0')
    ENDIF
    SELECT SYCFISHD
    laData[6]  = CTOD("01\01\"+PADR(lcYear,4))
    laData[7]  = CTOD("31\12\"+PADR(lcYear,4))
    IF ALLTRIM(laData[8]) == "Fis. year"
      laData[8]  = SUBSTR(laData[8],1,10)+" "+lcYear
    ENDIF
    IF ALLTRIM(laData[9]) == "Fis. year"
      laData[9]  = laData[8]
    ENDIF
    *:B#804025,1 SSH 25/03/2001 [END]
  ENDIF
ENDIF
*** Set the non working days for each company ***
FOR lnCount = 1 TO 7
  lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(ALLTRIM(STR(lnCount)) $ laData[10],1,0)
ENDFOR

SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfSave
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : function to save..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfSave()
*!*************************************************************
*!
FUNCTION lfSave

SELECT SYCFISHD
LOCATE FOR cFisyStat ='C'
REPLACE cFisLHead WITH laData[9],;
        cFisSHead WITH laData[8]
        
       
FUNCTION lfvNext

=lfvYearPop()
lcNextActn = 'N'
SET DATE &lcOldDat
=gfCPClose()

FUNCTION lfvResu

=lfvYearPop()
lcNextActn = 'U'
SET DATE &lcOldDat
=gfCPClose()

FUNCTION lfvYear
*:B#804025,1 SSH 25/03/2001 Incorect start and end persiond date
laData[6] = CTOD("01\01\"+PADR(lcYear,4))
laData[7] = CTOD("31\12\"+PADR(lcYear,4))
IF ALLTRIM(laData[8]) == "Fis. year"
  laData[8]  = SUBSTR(laData[8],1,10)+" "+lcYear
ENDIF
IF ALLTRIM(laData[9]) == "Fis. year"
  laData[9]  = laData[8]
ENDIF
*:B#804025,1 SSH 25/03/2001 [END]
SHOW GET laData[8]
SHOW GET laData[9]
=lfRefresh()

FUNCTION lfvCurPer

SELECT SYWCOMP
REPLACE cCurr_Prd WITH laData[5],;
        cCurr_Yer WITH lcYear

FUNCTION lfvSavFis
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT SYCFISHD  && Header
LOCATE FOR cFisyStat = "C"
REPLACE cFisfYear WITH lcYear    ,;
        dFisBgDat WITH laData[6] ,;
        dFisEnDat WITH laData[7] ,;
        cFislHead WITH laData[9],;
        cFissHead WITH laData[8]
FLUSH
SELECT SYWCOMP
=SEEK(pcComp_ID)
REPLACE cCurr_Yer WITH laData[2],; 
        cCurr_Prd WITH laData[5]
SELECT(lnOldAls)