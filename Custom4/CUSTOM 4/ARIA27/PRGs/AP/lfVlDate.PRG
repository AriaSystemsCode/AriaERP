*!***********************************************************************
*!
*! This function is to validate dates from the periods file.
*!
*! Parameters :
*!
*! 1- The company that you want to validate from its periods.
*!
*! 2- Logical parameter to accept the date in a locked period.
*!
*!***********************************************************************
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301148,1 AMM 02/28/99 Enhance opening and closing files
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
**************************************************************************
PARAMETERS lcCompany,lcCrPrd,lcCrYer,ldDateTVal,llLockPer   && Parameters

PRIVATE llOpenPrd,lcSavOrdPr,llValidDate,lcExactStat        && Private variables

** If type of this variable is undefine so we have assign them as private
IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
  PRIVATE lcCurYear,lcCurPrd,lLockStat 

  ** Variable to hold the year of the period.
  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  ** Varibale to hold the period No.
  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
ENDIF

llLockPer  = IIF(llLockPer,llLockPer,.F.)

llValidDate= .F.      && Variable to return with from the function.

lcCurYear  = ' '      && Variable to hold the current year.
lcCurPrd   = ' '      && Varibale to hold the current period.
lcExactStat= ' '      && Variable to hold the SET EXACT status.
lLockStat  = .F.      && Variable to hold the lock stat of the record. 

lcSavSelct = ALIAS()  && Variable to save the currently selected file.

llOpenPrd  = .F.      && Variable to indicate that the there is a file;
                         open by the function.

*MAN     
PRIVATE llOpAPS, llOpcomp                     
*E301148,1 AMM Open files

llOpcomp = gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
llOpAPS  = gfOpenFile(gcDataDir+'APSETUP','','SH')
*E301148,1 AMM end

** Check if the company parameter if defined or not. 
lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

*-- I'm not sure where the following variables are defined.
lcCompYer  = lcCurr_Yer
lcCompPrd  = lcCurr_Prd



** Check if the company parameter if defined or not. 
lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

*lcCompYer  = ALLTRIM(LOOKUP(SYCCOMP.CCURR_YER,lcCompany,SYCCOMP.CCOMP_ID,"CCOMP_ID"))
*lcCompPrd  = IIF(!EMPTY(lcCompYer),SYCCOMP.CCURR_PRD,lcCompPrd)

lcCompYer  = lcCurr_Yer
lcCompPrd  = lcCurr_Prd

*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*IF !USED('SYCFSPRD')   && Check if the period file is open or not.
IF !USED('FSPRD')   && Check if the period file is open or not.
  *E300692,1 end
  llOpenPrd = .T.      && Indicates that the file is open by the function.
  SELECT 0             && Select an empty work area.
  ** Use the file and assign the index.
  *E300692,1 CHANGE FILE NAME AND PATH FROM SYCFSPRD TO FSPRD
  *USE &gcSysHome.sycfsprd ORDER TAG COMFYRPRDI 
  *E301098,1 Hesham (Start)
  *lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'), SYCCOMP.cCom_DDir, gcDataDir))
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'), gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  *E301098,1 Hesham (End)
  USE &lcDataDir.fsprd ORDER TAG COMFYRPRDI 
  *E300692,1 end
ELSE
  SELECT FSPRD      && The file is open so we are going to select
  lcSavOrdPr = SYS(22) && Save the file order
  SET ORDER TO TAG COMFYRPRDI   && Change the order
ENDIF  
IF USED('SYCCOMP') .AND. llOpcomp
  =gfCloseFile('SYCCOMP')
ENDIF
IF USED('APSETUP') .AND. llOpAPS
  =gfCloseFile('APSETUP')
ENDIF

** Variable to hold the date to validate.
ldDate = IIF(TYPE('ldDateTVal') <> 'D',EVALUATE(SYS(18)),ldDateTVal)

lcExactStat = SET('EXACT')
SET EXACT OFF
*E300789,4  AMM Adjust to fit the new structure
*SEEK lcCompany   && Seek for the selected company
GO TOP
*E300789,4  AMM end

SET EXACT &lcExactStat

** We are going to locate for the date between the begin and the
** end of the period of the company and the period is equal the
** locking parameter.
*E300789,4  AMM Adjust to fit the new structure
*LOCATE REST WHILE cComp_Id = lcCompany;
              FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
*E300789,4  AMM end
IF FOUND() .AND. BETWEEN(ldDate,ldPyBgDate,ldNyEnDate)

  llLockStat = lFspLocks  && Assign the variable with the period lock stat.
  lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.  
  lcCurPrd   = cFspprdid  && Assign the variable with the period no.
  lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.  
  lcCrPrd    = cFspprdid  && Assign the variable with the period no.
  llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
  llValidDate= .T.        && Assign the variable with .T. 
  
  ** We are going to close the files opend by the function.
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *IF USED('SYCFSPRD') .AND. llOpenPrd
  IF USED('FSPRD') .AND. llOpenPrd
  *E300692,1 end  
    llOpenPrd = .F. 
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD    
    *USE IN SYCFSPRD 
    USE IN FSPRD 
    *E300692,1 end   
  ELSE
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD  
    *SELECT SYCFSPRD
    SELECT FSPRD
    *E300692,1 end
    SET ORDER TO &lcSavOrdPr
  ENDIF  

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF  
ELSE
  ** We are going to close the files opend by the function.
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *IF USED('SYCFSPRD') .AND. llOpenPrd
  IF USED('FSPRD') .AND. llOpenPrd
  *E300692,1 end  
    llOpenPrd = .F. 
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD  
    *USE IN SYCFSPRD 
    USE IN FSPRD
  *E300692,1 end   
  ELSE
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD  
    *SELECT SYCFSPRD
    SELECT FSPRD
  *E300692,1 end
    SET ORDER TO &lcSavOrdPr
  ENDIF  

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)  
  ENDIF
ENDIF

RETURN llValidDate
