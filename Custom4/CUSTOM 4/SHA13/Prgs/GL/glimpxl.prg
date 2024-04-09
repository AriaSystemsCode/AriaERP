************************************************************************************************************************
*: Program File  : GLIMPXL.PRG
*: Program Desc. : Convert a custome program to import Excel sheet to update GL files
*: System : Aria4xp R12
*: Module : GL
*  06/13/2013
*: Developer : TMI - Tarek Mohammed Ibrahim
*: Original Tracking # : C201310.122  [T20110118.0014 ] ( a27 - before R13 in A4 )
*: Convert  Tracking # : C201582.exe  [T20130606.0005 ]
************************************************************************************************************************
*Modifications:
*B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012]
*B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006]
*B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001]
*B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001]
*B611261,1 MMT 02/01/2017 Issue#1: Error while using custom Import GL entries program[P20170117.0044]
*C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044]
*C201994,1 MHM 04/02/2017 Custom import journal entries, not update the correct transaction period/year in gltrnsdt table[T20170316.0073].
*C202004,1 MHM 04/18/2017 Custom import journal entries, not update the correct transaction/reverse period/year in gltrnshd table[T20170316.0073].
*B611625,1 Es 07/24/2018 Modify this issue ->"Custom Import GL  Entries program hangs with some Excel files like the attached one" [T20180720.0004]
************************************************************************************************************************
*!**!**!**!**
*!*	steps *!*
*!*
*!*	import
*!*	validate
*!*	Update
*!**!**!**!**

lcRunScx = oAriaApplication.clientscreenhome+"gl\glimprxl.scx"
*B611261,1 MMT 02/01/2017 Issue#1: Error while using custom Import GL entries program[P20170117.0044][Start]
*DO FORM (lcRunScx)
=GFCALLFORM("glimprxl",'GL')
*B611261,1 MMT 02/01/2017 Issue#1: Error while using custom Import GL entries program[P20170117.0044][End]
*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/13/2013
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.Ariaform1.CAPTION = 'Import Excel files to update GL entries'

SET MULTILOCKS ON
*- Open files
=lfOpenFls()
loFormSet.ADDPROPERTY('lcCr', CHR(13) )
loFormSet.ADDPROPERTY('lcNxtBatch','')
loFormSet.ADDPROPERTY('lcErrLog','')

loFormSet.ADDPROPERTY('lcTmpGLXLS', gfTempName())

loFormSet.Ariaform1.edtErrorlist.CONTROLSOURCE = 'Thisformset.lcErrLog'
loFormSet.Ariaform1.REFRESH()

*****************************************************************************************************************************
*
*   FUNCTION lfImport
*   06/13/2013
*
*****************************************************************************************************************************
FUNCTION lfImport
PARAMETERS loFormSet

lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')
  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'File already has been selected, Select again?')<>1
    RETURN
  ENDIF
ENDIF

*- Be sure to close the imported file
IF USED(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
ENDIF


SET SAFETY OFF
CD (oAriaApplication.clienta4path)
IF FILE(oAriaApplication.clientprogramhome+'GL\GLIMPVXL.MEM')
  RESTORE FROM (oAriaApplication.clientprogramhome+'GL\GLIMPVXL.MEM') ADDITIVE
  *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][Start]
  IF DIRECTORY(lcopndir)
    *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][End]
    CD (lcopndir)
    *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][Start]
  ENDIF
  *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][End]
ENDIF
llcontinue = .T.
*C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
*lcgetdir = GETFILE('xls')
lcgetdir = GETFILE('XLSX')
*C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
IF EMPTY(lcgetdir)
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
  *MESSAGEBOX('No file is selected', 0, 'Aria27')
  MESSAGEBOX('No file is selected', 0, 'Aria4XP')
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
  llcontinue = .F.
ELSE
  lcopndir = ADDBS(JUSTPATH(lcgetdir))
  SAVE TO (oAriaApplication.clientprogramhome+'GL\GLIMPVXL.MEM') ALL LIKE lcopndir*
ENDIF
IF llcontinue
  CD (oAriaApplication.WorkDir)
  SELECT 0
  ON ERROR llcontinue = .F.
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
  *IMPORT FROM (lcgetdir) TYPE XL5
  CREATE CURSOR 'TmpXlsx' (A C(24),B C(65),C C(40),D C(20),E C(20),F C(20),G C(20),H C(10),I C(20))
  oExcelSheet = CREATEOBJECT('Excel.Application')
  z=oExcelSheet.Workbooks.OPEN (lcgetdir)
  s = z.Sheets [1]
  *B611625,1 Es 07/24/2018 Modify this issue ->"Custom Import GL  Entries program hangs with some Excel files like the attached one" [Start]
   *lnRow = s.UsedRange.Rows.Count
   lnRow  = s.Cells(S.Rows.Count, "A").End(-4162).Row
   *B611625,1 Es 07/24/2018 Modify this issue ->"Custom Import GL  Entries program hangs with some Excel files like the attached one" [End]
  
  FOR lnY = 1 TO lnRow
    SELECT 'TmpXlsx'
    APPEND BLANK
    FOR lnX = 1 TO 9
      IF !ISNULL(s.Cells(lnY , lnX ).VALUE)
        REPLACE (FIELD(lnX)) WITH IIF(TYPE('s.Cells(lnY , lnX ).Value')='N',ALLTRIM(STR(s.Cells(lnY,lnX).VALUE,20,IIF(INLIST(lnX,1,2,3,4,8,9),0,2))),IIF(TYPE('s.Cells(lnY , lnX ).Value')='T',DTOC(TTOD(s.Cells(lnY,lnX).VALUE)),s.Cells(lnY , lnX ).VALUE))
      ENDIF
    ENDFOR
  ENDFOR
  oExcelSheet = NULL
  SELECT 'TmpXlsx'
  DELETE FOR EMPTY(ALLTRIM(A)) AND EMPTY(ALLTRIM(B))
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
  ON ERROR
  IF llcontinue
    COPY TO (oAriaApplication.WorkDir+lcTmpGLXLS)
    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
    *!*	      lcdbf = DBF()
    *!*	      USE
    *!*	      ERASE (lcdbf)
    USE IN 'TmpXlsx'
    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
  ELSE
    MESSAGEBOX('Invalid file format', 0, 'Aria4xp')
  ENDIF
ENDIF
*- End of lfImport.

*!*************************************************************
*! Name      : lfValidate
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : Validate the selected file
*!*************************************************************
FUNCTION lfValidate
PARAMETERS loFormSet

lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')

  loFormSet.lcErrLog = ''

  IF !USED(lcTmpGLXLS)
    USE (oAriaApplication.WorkDir+lcTmpGLXLS) IN 0
  ENDIF

  SELECT (lcTmpGLXLS)

  *-Account # (column A) is valid in the chart of accounts.
  LOCATE FOR !EMPTY(B)
  SKIP
  SCAN REST FOR !EMPTY(A)
    IF !SEEK( PADR(ALLT(A),24) ,'GLACCHAR')
      =lfErrLog(loFormSet,'ACC',ALLT(A))
    ENDIF
    *B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012][Start]
    IF "/" $ D
      REPLACE D WITH STR(CTOD(D)-{^1899-12-30})
    ENDIF
    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
    IF "/" $ I
      REPLACE I WITH STR(CTOD(I)-{^1899-12-30})
    ENDIF
    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
    *B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012][End]
    *B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006][Start]
    IF "." $ E OR "." $ F OR "." $ G
      IF "." $ E
        lnDotLoc = ATC(".",E)
        lnAfterDot = SUBSTR(E,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(E,1,lnDotLoc)
          REPLACE E WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
        ENDIF
      ENDIF

      IF "." $ F
        lnDotLoc = ATC(".",F)
        lnAfterDot = SUBSTR(F,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(F,1,lnDotLoc)
          REPLACE F WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
        ENDIF
      ENDIF

      IF "." $ G
        lnDotLoc = ATC(".",G)
        lnAfterDot = SUBSTR(G,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(G,1,lnDotLoc)
          REPLACE G WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
        ENDIF
      ENDIF
    ENDIF
    *B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006][End]
  ENDSCAN

  *-Each transaction's total Debit = total Credit for the related set of entries.2
  =SEEK('CTRANNO   ','SEQUENCE')
  loFormSet.lcNxtBatch = PADL(SEQUENCE.NSEQ_NO,8,'0')
  SELECT (lcTmpGLXLS)
  LOCATE
  LOCATE REST FOR !EMPTY(B)
  SKIP
  DO WHILE !EOF()
    LOCATE REST FOR !EMPTY(B)
    lnBalance = 0
    lcBal = ''
    SCAN REST WHILE !EMPTY(B)
      lcBal = A + ',' + B + ',' + E + ',' + F + loFormSet.lcCr
      *B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001][Start]
      *lnBalance = lnBalance + VAL(E) - VAL(F)
      lnBalance = lnBalance + ROUND(VAL(E),2) - ROUND(VAL(F),2)
      *B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001][End]
    ENDSCAN
    IF lnBalance <> 0
      =lfErrLog(loFormSet,'BAL',lcBal)
    ENDIF
  ENDDO

  *- Enable/Disable buttons
  IF EMPTY(loFormSet.lcErrLog)
    WITH loFormSet.Ariaform1
      .pbImport.ENABLED = .F.
      .pbVld.ENABLED = .F.
      .pbUpd.ENABLED = .T.
    ENDWITH
  ELSE
    loFormSet.lcErrLog = 'Error List ' + loFormSet.lcCr  + ;
      loFormSet.lcErrLog
  ENDIF

  loFormSet.Ariaform1.REFRESH()

ELSE

  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No file has been selected.')

ENDIF

*!*************************************************************
*! Name      : lfVlDate
*! Developer : Mohamed hamdy mohamed (MHM)
*! Date      : 4/2/2017
*! Purpose   : To Validate dates from the periods file.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION LFVLDATE
PARAMETERS LCCOMPANY, LCCRPRD, LCCRYER, LDDATETVAL, LLLOCKPER


PRIVATE LLOPENPRD, LCSAVORDPR, LLVALIDDATE, LCEXACTSTAT

IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
  PRIVATE LCCURYEAR, LCCURPRD, LLOCKSTAT

  LCCRYER    = IIF(TYPE('lcCrYer') <> 'C',' ',LCCRYER)

  LCCRPRD    = IIF(TYPE('lcCrPrd') <> 'C',' ',LCCRPRD)
ENDIF

LLLOCKPER  = IIF(LLLOCKPER,LLLOCKPER,.F.)

LLVALIDDATE= .F.      && Variable to return with from the function.

LCCURYEAR  = ' '      && Variable to hold the current year.
LCCURPRD   = ' '      && Varibale to hold the current period.
LCEXACTSTAT= ' '      && Variable to hold the SET EXACT status.
LLOCKSTAT  = .F.      && Variable to hold the lock stat of the record.

LCSAVSELCT = ALIAS()  && Variable to save the currently selected file.

LLOPENPRD  = .F.      && Variable to indicate that the there is a file;
&& OPEN BY the FUNCTION.



*PRIVATE llOpAPS, llOpcomp

*llOpcomp = gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','Ccomp_id','SH')  &&lower
*llOpAPS  = gfOpenFile(oAriaApplication.DataDir+'APSETUP','','SH')  &&lower

LCCOMPANY  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,LCCOMPANY)

LCCOMPYER  = oAriaApplication.CURRENTYEAR
LCCOMPPRD  = oAriaApplication.CURRENTPERIOD



LCCOMPANY  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,LCCOMPANY)


LCCOMPYER  = oAriaApplication.CURRENTYEAR
LCCOMPPRD  = oAriaApplication.CURRENTPERIOD

IF !USED('FSPRD')   && Check if the period file is open or not.
  LLOPENPRD = .T.      && Indicates that the file is open by the function.
  SELECT 0             && Select an empty work area.
  *lcDataDir = ALLTRIM(IIF(SEEK(oAriaApplication.PrntCompanyID, 'SYCCOMP'), gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), oAriaApplication.DataDir))  &&lower  &&lower


  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
  *USE &lcDataDir.fsprd ORDER TAG COMFYRPRDI
  =gfOpenTable('fsprd','COMFYRPRDI','SH')
  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]

ELSE
  SELECT FSPRD      && The file is open so we are going to select

  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
  *!*	  lcSavOrdPr = SYS(22) && Save the file order
  *!*	  SET ORDER TO TAG COMFYRPRDI   && Change the order
  LCSAVORDPR = ORDER() && Save the file order
  =GFSETORDER('COMFYRPRDI')
  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
ENDIF
*IF USED('SYCCOMP') .AND. llOpcomp
*=gfCloseFile('SYCCOMP')
*ENDIF
*IF USED('APSETUP') .AND. llOpAPS
*=gfCloseFile('APSETUP')
*ENDIF

IF TYPE('ldDateTVal') <> 'D'
  LDDATE = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
ELSE
  LDDATE = LDDATETVAL
ENDIF
*ldDate = ldDateTVal

LCEXACTSTAT = SET('EXACT')
SET EXACT OFF
GO TOP

SET EXACT &LCEXACTSTAT


LOCATE REST FOR BETWEEN(LDDATE,DFSPPBGDT,DFSPPENDT)
IF FOUND()
  LLLOCKSTAT = LFSPLOCKS  && Assign the variable with the period lock stat.
  LCCURYEAR  = CFISFYEAR  && Assign the variable with fiscal year of the period.
  LCCURPRD   = CFSPPRDID  && Assign the variable with the period no.
  LCCRYER    = CFISFYEAR  && Assign the variable with fiscal year of the period.
  LCCRPRD    = CFSPPRDID  && Assign the variable with the period no.
  LLLOCKPER  = LFSPLOCKS  && Assign the variable with the period lock stat.
  LLVALIDDATE= .T.        && Assign the variable with .T.

  IF USED('FSPRD') .AND. LLOPENPRD
    LLOPENPRD = .F.
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *USE IN FSPRD
    =gfCloseTable('FSPRD')
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[END]
  ELSE
    SELECT FSPRD
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *SET ORDER TO &lcSavOrdPr
    =GFSETORDER(LCSAVORDPR)
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ENDIF

  IF !EMPTY(LCSAVSELCT)
    SELECT(LCSAVSELCT)
  ENDIF
ELSE
  IF USED('FSPRD') .AND. LLOPENPRD
    LLOPENPRD = .F.
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *USE IN FSPRD
    =gfCloseTable('FSPRD')
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ELSE
    SELECT FSPRD
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *SET ORDER TO &lcSavOrdPr
    =GFSETORDER(LCSAVORDPR)
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ENDIF

  IF !EMPTY(LCSAVSELCT)
    SELECT(LCSAVSELCT)
  ENDIF
ENDIF

RETURN LLVALIDDATE

*--end of lfVlDate



*********************************************************************************************************
*
*   FUNCTION lfUpdate
*   06/13/2013
*   TMI - TAREK MOHAMMED IBRAHIM
*   Update the GL files
*
*********************************************************************************************************
FUNCTION lfUpdate
PARAMETERS loFormSet
PRIVATE lnSlct
lnSlct = SELECT(0)
SET STEP ON
lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')
  lcCent = SET('CENTU')
  SET CENTU ON

  STORE '' TO lcCrFrm,lcCrTo
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
  lcRevDate = ""
  *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]
  SELECT (lcTmpGLXLS)
  LOCATE
  LOCATE REST FOR !EMPTY(B)
  SKIP
  LOCATE REST FOR !EMPTY(B)

  DO WHILE !EOF(lcTmpGLXLS)
    SELECT (lcTmpGLXLS)
    LOCATE REST FOR !EMPTY(B)
    STORE 0 TO lnDebit,lnCrdit
    *C202004 ,MHM 04/12/2017 Custom import journal entries [Begin]
    LCPSTYER = ""
    LCPSTPRD = ""
    *C202004 ,MHM 04/12/2017 Custom import journal entries [End]

    SCAN REST WHILE !EMPTY(B)
      lnDebit = lnDebit + VAL(&lcTmpGLXLS..E)
      lnCrdit = lnCrdit + VAL(&lcTmpGLXLS..F)
      *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]
      IF !EMPTY(&lcTmpGLXLS..I)
        lcRevDate =  &lcTmpGLXLS..I
      ENDIF
      *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]

      *C202004,1 MHM 04/18/2017 Custom import journal entries, not update the correct transaction/reverse period/year in gltrnshd table[T20170316.0073][Begin]
      *!*	      SELECT GLTRNSDT
      *!*	      APPEND BLANK
      *!*	      REPLACE CBATCHNO  WITH '000000';
      *!*	        CTRANNO   WITH loFormSet.lcNxtBatch ;
      *!*	        CACCTCODE WITH &lcTmpGLXLS..A ;
      *!*	        CTRDTEXP  WITH &lcTmpGLXLS..C ;
      *!*	        CDRORCR   WITH IIF(!EMPTY(&lcTmpGLXLS..E),'D','C') ;
      *!*	        NAMOUNT   WITH IIF(CDRORCR='D',VAL(&lcTmpGLXLS..E),VAL(&lcTmpGLXLS..F)) ;
      *!*	        DTRNPDATE WITH {^1899-12-30} + INT(VAL(&lcTmpGLXLS..D)) ;
      *!*	        CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
      *!*	        CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
      *!*	        CADD_USER WITH oAriaApplication.USER_ID ;
      *!*	        CADD_TIME WITH TIME() ;
      *!*	        DADD_DATE WITH DATE() ;
      *!*	        COWNER    WITH 'imported'

      lDTRNPDATE =  {^1899-12-30} + INT(VAL(&lcTmpGLXLS..D))
      =LFVLDATE(oAriaApplication.PRNTCOMPANYID , @LCPSTPRD , @LCPSTYER , lDTRNPDATE )
      SELECT GLTRNSDT
      APPEND BLANK
      REPLACE CBATCHNO  WITH '000000';
        CTRANNO   WITH loFormSet.lcNxtBatch ;
        CACCTCODE WITH &lcTmpGLXLS..A ;
        CTRDTEXP  WITH &lcTmpGLXLS..C ;
        CDRORCR   WITH IIF(!EMPTY(&lcTmpGLXLS..E),'D','C') ;
        NAMOUNT   WITH IIF(CDRORCR='D',VAL(&lcTmpGLXLS..E),VAL(&lcTmpGLXLS..F)) ;
        DTRNPDATE WITH {^1899-12-30} + INT(VAL(&lcTmpGLXLS..D)) ;
        CTRNPYR   WITH LCPSTYER ;
        CTRNPPRD  WITH LCPSTPRD ;
        CADD_USER WITH oAriaApplication.USER_ID ;
        CADD_TIME WITH TIME() ;
        DADD_DATE WITH DATE() ;
        COWNER    WITH 'imported'
      *C202004,1 MHM 04/18/2017 Custom import journal entries, not update the correct transaction/reverse period/year in gltrnshd table[T20170316.0073][End]

    ENDSCAN

    SELECT GLTRNSHD
    APPEND BLANK

    *C201994 ,MHM 04/02/2017 Custom import journal entries [Begin]

    *!*	    *REPLACE CBATCHNO  WITH '000000' ;
    *!*	    CTRANNO   WITH loFormSet.lcNxtBatch ;
    *!*	    CTRNDESC  WITH 'Created by Aria Systems from Excel' ;
    *!*	    CTRNREFER WITH 'On ' + DTOC(DATE()) ;
    *!*	    DTRNPDATE WITH GLTRNSDT.DTRNPDATE ;
    *!*	    CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
    *!*	    CTRNPYR   WITH LCCRYER ;
    *!*	    CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
    *!*	    CTRNSTAT  WITH 'U' ;
    *!*	    CTRNTYPE  WITH 'N' ;
    *!*	    CTRNREVER WITH 'N' ;
    *!*	    NTRNTOTDR WITH lnDebit ;
    *!*	    NTRNTOTCR WITH lnCrdit ;
    *!*	    CSRCMODUL WITH 'GL';
    *!*	    CSTANDARD WITH 'Y' ;
    *!*	    CSRCJRNL  WITH IIF(!EMPTY(&lcTmpGLXLS..H),ALLT(&lcTmpGLXLS..H),'GJ') ;
    *!*	    CADD_USER WITH oAriaApplication.USER_ID ;
    *!*	    CADD_TIME WITH TIME() ;
    *!*	    DADD_DATE WITH DATE() ;
    *!*	    COWNER    WITH 'imported'


    REPLACE CBATCHNO  WITH '000000' ;
      CTRANNO   WITH loFormSet.lcNxtBatch ;
      CTRNDESC  WITH 'Created by Aria Systems from Excel' ;
      CTRNREFER WITH 'On ' + DTOC(DATE()) ;
      DTRNPDATE WITH GLTRNSDT.DTRNPDATE ;
      CTRNPYR   WITH LCPSTYER ;
      CTRNPPRD  WITH LCPSTPRD ;
      CTRNSTAT  WITH 'U' ;
      CTRNTYPE  WITH 'N' ;
      CTRNREVER WITH 'N' ;
      NTRNTOTDR WITH lnDebit ;
      NTRNTOTCR WITH lnCrdit ;
      CSRCMODUL WITH 'GL';
      CSTANDARD WITH 'Y' ;
      CSRCJRNL  WITH IIF(!EMPTY(&lcTmpGLXLS..H),ALLT(&lcTmpGLXLS..H),'GJ') ;
      CADD_USER WITH oAriaApplication.USER_ID ;
      CADD_TIME WITH TIME() ;
      DADD_DATE WITH DATE() ;
      COWNER    WITH 'imported'

    *C201994 ,MHM 04/02/2017 Custom import journal entries [END]

    *- update the CCOMP_ID field
    REPLACE CCOMP_ID  WITH oAriaApplication.activecompanyid

    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][Start]




    IF !EMPTY(lcRevDate)
      *C202004,1 MHM 04/18/2017 Custom import journal entries, not update the correct transaction/reverse period/year in gltrnshd table[T20170316.0073] [Begin]
      *!*	      REPLACE CTRNREVER WITH 'Y',;
      *!*	        DTRNREVDT WITH {^1899-12-30} + INT(VAL(lcRevDate)),;
      *!*	        CTRNREVPR WITH PADL(MONTH(DTRNREVDT),2,'0'),;
      *!*	        CTRNREVYR WITH STR(YEAR(DTRNREVDT),4)

      lDTRNPDATE =  {^1899-12-30} + INT(VAL(lcRevDate))
      =LFVLDATE(oAriaApplication.PRNTCOMPANYID , @LCPSTPRD , @LCPSTYER , lDTRNPDATE )
      REPLACE CTRNREVER WITH 'Y',;
        DTRNREVDT WITH lDTRNPDATE,;
        CTRNREVPR WITH LCPSTPRD,;
        CTRNREVYR WITH LCPSTYER
      *C202004,1 MHM 04/18/2017 Custom import journal entries, not update the correct transaction/reverse period/year in gltrnshd table[T20170316.0073] [End]

    ENDIF


    *C201968,1 MMT 03/13/2017 Issue#2: Change the custom GL entries import program to add reverse date[P20170117.0044][End]

    lcCrFrm = IIF(EMPTY(lcCrFrm),loFormSet.lcNxtBatch,lcCrFrm)
    lcCrTo  = loFormSet.lcNxtBatch

    *- the loFormSet.lcNxtBatch should not take a decimal value
    loFormSet.lcNxtBatch = PADL(INT(VAL(loFormSet.lcNxtBatch))+ 1,8,'0')

    SELECT (lcTmpGLXLS)
    LOCATE REST FOR !EMPTY(B)
  ENDDO

  SELECT SEQUENCE
  REPLACE NSEQ_NO   WITH VAL(loFormSet.lcNxtBatch)

  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File has been imported, transactions created from ' + lcCrFrm + ' to ' + lcCrTo)

  *- finalizing / erase / close
  lcDbf = DBF(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
  ERASE (lcDbf)

  *- update
  SELECT SEQUENCE
  =gfTableUpdate()

  SELECT GLTRNSDT
  =gfTableUpdate()

  SELECT GLTRNSHD
  =gfTableUpdate()


  *- close
  =gfCloseTable('SEQUENCE')
  =gfCloseTable('GLTRNSDT')
  =gfCloseTable('GLTRNSHD')
  =gfCloseTable('GLACCHAR')


  *- restore century setting
  SET CENTU ON &lcCent
  SELECT (lnSlct)
  CLEAR READ

ELSE

  =gfModalGen("INM000000B00000",.F.,.F.,.F.,"No imported file to update")

ENDIF

SELECT (lnSlct)


*!*************************************************************
*! Name      : lfErrLog
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : Create an error log, if any
*!*************************************************************
FUNCTION lfErrLog
PARAMETERS loFormSet,lcRes,lcVal
PRIVATE lcMsg

DO CASE
CASE lcRes = 'ACC'
  lcMsg = ' Account is not found in the Chart of Accounts.'
  loFormSet.lcErrLog = loFormSet.lcErrLog + lcVal + lcMsg + loFormSet.lcCr

CASE lcRes = 'BAL'
  lcMsg = ' Debit/Credits are Not balanced.'
  loFormSet.lcErrLog = loFormSet.lcErrLog + lcMsg + loFormSet.lcCr + ;
    lcVal + loFormSet.lcCr
ENDCASE

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : * Open the needed file for the import and update to complete
*!*************************************************************
FUNCTION lfOpenFls

gcDataDir = oAriaApplication.DataDir
=gfOpenTable(gcDataDir+'SEQUENCE','CSEQ_TYPE','SH')
=gfOpenTable(gcDataDir+'GLTRNSDT','BATCHTRN','SH')
=gfOpenTable(gcDataDir+'GLTRNSHD','BATCHTRN','SH')
=gfOpenTable(gcDataDir+'GLACCHAR','ACCTCODE','SH')
