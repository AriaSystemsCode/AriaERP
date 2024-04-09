*!*****************************************************************************************
*! Name      : GfDispRe
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/31/2002 01:03:00 PM
*! Purpose   : Display Report Functionality
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!E038338,1 SMM 09/06/2004 Use gfDispRe to display Crystal Reports
*!N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer
*! B127195,1 MAH 04/10/2005 Fix Print Text Format problem.
*! B128092,1 SMM 05/24/2005 Clear Tag1, TAG2 in preview
*! B128052,1 MAH 06/05/2005 Takes a long time to log in.
*! B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF
*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print an export
*! B608287,1 MMT 09/25/2007 Fix bug of Printing on PDF Before printing to printer[T20070531.0011]
*! B608190,1 WLD 07/31/2007 Printing Selection Criteria Setting Report in case of Crystal report
*! B608311,1 MMT 10/11/2007 fix bug of not able to print legal paper size reports[T20071008.0013]
*! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels               [T20071016.0010]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Task:T20081225.0022]
*! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[T20080422.0042]
*! E302857,1 HES 02/10/2011 Avoid 'X:\Aria4xp\SRVRPTS' Fixed Path [T20110206.0017]
*! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [T20071022.0153]
*! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report
*!*****************************************************************************************


FUNCTION GFDISPRE
PARAMETERS LCOGRPRTNAM,LCCRITERIA,LLENDJOB,LCRPRTLBL,LLPRNTTOFILE
PRIVATE LCOGRPRTNAM,LCCRITERIA,LLENDJOB,LCRPRTLBL,LLPRNTTOFILE    &&


*! T20080115.0013 26,Feb 2008 Change the date format to British to pass int to SQL in case SQL
IF TYPE("oAriaApplication.cActCompDateFormat") = 'C' .AND. ;
    !EMPTY(OARIAAPPLICATION.CACTCOMPDATEFORMAT)
  LCTEMP = OARIAAPPLICATION.CACTCOMPDATEFORMAT
  SET DATE &LCTEMP.
ENDIF
*! T20080115.0013 END

*! B128052,1 MAH 06/05/2005 [BEGIN]
= GFADDPERFLOG("OPTIONGRID", "PRINT", "END", "")
*! B128052,1 MAH 06/05/2005 [END]

*-- Used variables in this session.
PRIVATE LCTMPPRINTERSET,;            && Save Old Printer Setup
LCSAVDFDIR,;                 && Save Current directory.
LCFULLSET,;                  && Save FullPath setting
GCOUTFILE,;                  && Output File
LCOGWHILE,;
  LCOGFOR,;
  LNDSRECNO,;
  LCOGSCOPE

*-- Save current default directory
LCFULLSET  = SET("Fullpath")
SET FULLPATH ON
LCSAVDFDIR = FULLPATH(SET('DEFAULT'))

*wael
*-- Reset the error handler.
LCOLDERRHND = ON('ERROR')
LCSETCUR = SET('CURSOR')
*wael

*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [Start]
LOCAL LCOLDCENTURY
LCOLDCENTURY = SET("Century" )
SET CENTURY OFF
*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [End]

*! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [START]

*B040214,1 AMH Define variable to restore the current device [Start]
LOCAL LCOLDDEVICE
LCOLDDEVICE = OARIAAPPLICATION.GCDEVICE
*B040214,1 AMH [End]

LOCAL LCPDFPATH
LOOGSCROLL.LLPREVPDF = .F.
IF (OARIAAPPLICATION.GCDEVICE="SCREEN" .OR. LOOGSCROLL.LLPREVPDF).AND. !LOOGSCROLL.LLCRYSTAL .AND. LOOGSCROLL.LUSEPDFVIEWER
  OARIAAPPLICATION.GCDEVICE="FILE"
  LCPDFPATH = LOOGSCROLL.GFTEMPNAME()
  LOOGSCROLL.CTEXTREPTYPE = "PDF"
  LCPDFPATH = OARIAAPPLICATION.WORKDIR + LCPDFPATH + ".PDF"
  OARIAAPPLICATION.GCOUTFILE = LCPDFPATH
  LOOGSCROLL.LLPREVPDF = .T.
ENDIF
*! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [END]

*B040214,1 AMH Using PDF to print graphic reports [Start]

*B608287,1 MMT Fix bug of Printing on PDF Before printing to printer[Start]
*!*	loOGScroll.llPrintPDF = .F.
*!*	IF oAriaApplication.gcDevice="PRINTER" .AND. !LoOGScroll.llCrystal .AND. lcOGPlatForm = 'WINDOW'
*!*	  oAriaApplication.gcDevice="FILE"
*!*	  lcPDFPath = loOGScroll.gfTempName()
*!*	  loOGScroll.cTextRepType = "PDF"
*!*	  lcPDFPath = oAriaApplication.WorkDir + lcPDFPath + ".PDF"
*!*	  oAriaApplication.gcOutFile = lcPDFPath
*!*	  loOGScroll.llPrintPDF = .T.
*!*	ENDIF
*B608287,1 MMT Fix bug of Printing on PDF Before printing to printer[End]

*B040214,1 AMH [End]

*-- Clear the printer setting.
SET PRINTER TO DEFAULT
SET DEVICE TO SCREEN

*-- Initialize required variables and validate parameters ... BEGIN
GCOUTFILE = OARIAAPPLICATION.GCOUTFILE
LCRPRTLBL = IIF(TYPE('lcRprtLbl') $ 'UL','R',LCRPRTLBL)

LLPRNTTOFILE = IIF(TYPE('llPrntToFile')='L',LLPRNTTOFILE,.F.)
LCCRITERIA   = IIF(TYPE('lcCriteria')='C' AND !EMPTY(LCCRITERIA),LCCRITERIA,'')
LCOGRPRTNAM = IIF(TYPE('lcOGTmpForm')<>'C' OR EMPTY(LCOGTMPFORM),LCOGRPRTNAM,OARIAAPPLICATION.WORKDIR+LCOGTMPFORM)

*! E038338,1 SMM Use gfDispRe to display Crystal Reports [START]
IF LOOGSCROLL.LLCRYSTAL
  *B608190,1 WLD 07/31/2007 Printing Selection Criteria Setting Report in case of Crystal report [Begin]
  LOOGSCROLL.LLCRYSTAL = .F.
  *Printing Selection Criteria Setting Report in a Portrait page
  LCDEFAULTORNT = LOOGSCROLL.CCRORIENTATION
  LOOGSCROLL.CCRORIENTATION = 'P'
  LFPRINTCRITRIA()
  LOOGSCROLL.LLCRYSTAL = .T.
  LOOGSCROLL.CCRORIENTATION = LCDEFAULTORNT
  *B608190,1 WLD 07/31/2007 Printing Selection Criteria Setting Report in case of Crystal report [End]

  LOOGSCROLL.MDOCRYSTALREPORT()
  LOOGSCROLL.OREPORT  = NULL
  LOOGSCROLL.OCRYSTAL = NULL
  LOOGSCROLL.LLOGFLTCH = .F.

  *AMH Last Day [Start]
  SET PRINTER TO
  SET CURSOR &LCSETCUR.
  ON ERROR &LCOLDERRHND.
  SET DEFAULT TO (LCSAVDFDIR)
  SET FULLPATH &LCFULLSET.
  SET CENTURY &LCOLDCENTURY
  SET DEVICE TO SCREEN
  *AMH Last Day [End]

  RETURN .T.
ENDIF
*! E038338,1 SMM Use gfDispRe to display Crystal Reports [END]

*-- Get report name.
IF RAT('\',LCOGRPRTNAM)=0

  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  *lcOGRprtNam = IIF(FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') ;
  .OR. FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX'),;
  oAriaApplication.ReportHome+lcOGRprtNam,oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam)
  *B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[Start]
  *!*	   IF (oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX'))) OR ;
  *!*	      FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist( oAriaApplication.ReportHome +lcOGRprtNam + oAriaApplication.ActiveModuleID+'\'+'.FRX') OR ;
  *!*	      FileExist( oAriaApplication.ReportHome +lcOGRprtNam + oAriaApplication.ActiveModuleID+'\'+'.LBX')

  *!*	    IF oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX'))
  *!*
  *!*
  *!*	      lcOGRprtNam = IIF(FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	                        FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX'),oAriaApplication.clientreporthome+ lcOGRprtNam,;
  *!*	                        oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam)
  *!*	    ELSE
  *!*	      lcOGRprtNam = IIF(FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') ;
  *!*						  .OR. FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX'),;
  *!*						  oAriaApplication.ReportHome+lcOGRprtNam,oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam)
  *!*	    ENDIF
  *!*	  ENDIF
  LCSRVRPTS = STRTRAN(UPPER(OARIAAPPLICATION.REPORTHOME),'REPORTS','SRVRPTS')

  *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN
  *!*	 IF (oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX')))OR ;
  *!*	      FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist( oAriaApplication.ReportHome  + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist( oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\' +lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist( lcSrvRpts  +lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(lcSrvRpts +lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist(lcSrvRpts + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam +'.FRX') OR ;
  *!*	      FileExist( lcSrvRpts  + oAriaApplication.ActiveModuleID+'\' +lcOGRprtNam+'.LBX')
  *!*
  *!*	    IF oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.LBX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
  *!*	      FileExist('X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX'))
  *!*
  *!*		  IF FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	                        FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
  *!*	                        FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.FRX') OR ;
  *!*						    FileExist('X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam+'.LBX')
  *!*
  *!*	        IF  FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
  *!*	                        FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX')
  *!*
  *!*	           lcOGRprtNam = oAriaApplication.clientreporthome+ lcOGRprtNam
  *!*	        ELSE
  *!*	          lcOGRprtNam = 'X:\Aria4xp\SRVRPTS\'+ lcOGRprtNam
  *!*	        ENDIF

  IF (OARIAAPPLICATION.MULTIINST AND (FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.LBX')))OR ;
      FILEEXIST(OARIAAPPLICATION.REPORTHOME + LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.REPORTHOME + LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(OARIAAPPLICATION.REPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(OARIAAPPLICATION.REPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\' +LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(LCSRVRPTS + LCOGRPRTNAM+'.FRX') OR ;
      FILEEXIST(LCSRVRPTS + LCOGRPRTNAM+'.LBX') OR ;
      FILEEXIST(LCSRVRPTS + OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCOGRPRTNAM +'.FRX') OR ;
      FILEEXIST(LCSRVRPTS + OARIAAPPLICATION.ACTIVEMODULEID+'\' +LCOGRPRTNAM+'.LBX')

    IF OARIAAPPLICATION.MULTIINST AND (FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.FRX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME + LCOGRPRTNAM+'.LBX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.FRX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.LBX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.FRX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.LBX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.FRX') OR ;
        FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.LBX'))

      IF FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.FRX') OR ;
          FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME + LCOGRPRTNAM+'.LBX') OR ;
          FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.FRX') OR ;
          FILEEXIST(OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM+'.LBX')

        IF  FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.FRX') OR ;
            FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+ LCOGRPRTNAM+'.LBX')

          LCOGRPRTNAM = OARIAAPPLICATION.CLIENTREPORTHOME + LCOGRPRTNAM
        ELSE
          LCOGRPRTNAM = OARIAAPPLICATION.CLIENTSRVREPORTHOME + LCOGRPRTNAM
        ENDIF
        *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END

      ELSE
        IF FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.FRX') OR ;
            FILEEXIST(OARIAAPPLICATION.CLIENTREPORTHOME+OARIAAPPLICATION.ACTIVEMODULEID+'\'+ LCOGRPRTNAM+'.LBX')
          LCOGRPRTNAM = OARIAAPPLICATION.CLIENTREPORTHOME+OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCOGRPRTNAM
        ELSE
          *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN
          *!*	           lcOGRprtNam = 'X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID
          
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
          *LCOGRPRTNAM = OARIAAPPLICATION.CLIENTSRVREPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID
          LCOGRPRTNAM = OARIAAPPLICATION.CLIENTSRVREPORTHOME + ADDBS(OARIAAPPLICATION.ACTIVEMODULEID)
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]
                    
          *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END
        ENDIF
      ENDIF
    ELSE
      IF FILEEXIST( OARIAAPPLICATION.REPORTHOME +LCOGRPRTNAM+'.FRX') .OR.;
          FILEEXIST(OARIAAPPLICATION.REPORTHOME+LCOGRPRTNAM+'.LBX') OR ;
          FILEEXIST( LCSRVRPTS  +LCOGRPRTNAM+'.FRX') OR ;
          FILEEXIST(LCSRVRPTS +LCOGRPRTNAM+'.LBX')

        IF FILEEXIST( OARIAAPPLICATION.REPORTHOME +LCOGRPRTNAM+'.FRX') .OR.;
            FILEEXIST(OARIAAPPLICATION.REPORTHOME+LCOGRPRTNAM+'.LBX')
          LCOGRPRTNAM =   OARIAAPPLICATION.REPORTHOME +LCOGRPRTNAM
        ELSE
          LCOGRPRTNAM = LCSRVRPTS +LCOGRPRTNAM
        ENDIF
      ELSE
        IF FILEEXIST( OARIAAPPLICATION.REPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCOGRPRTNAM +'.FRX') OR ;
            FILEEXIST( OARIAAPPLICATION.REPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCOGRPRTNAM +'.LBX')
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
          *LCOGRPRTNAM = OARIAAPPLICATION.REPORTHOME + OARIAAPPLICATION.ACTIVEMODULEID+LCOGRPRTNAM
          LCOGRPRTNAM = OARIAAPPLICATION.REPORTHOME + ADDBS(OARIAAPPLICATION.ACTIVEMODULEID)+LCOGRPRTNAM          
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]
        ELSE
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
          *LCOGRPRTNAM = LCSRVRPTS + OARIAAPPLICATION.ACTIVEMODULEID +LCOGRPRTNAM
          LCOGRPRTNAM = LCSRVRPTS + ADDBS(OARIAAPPLICATION.ACTIVEMODULEID) +LCOGRPRTNAM          
          *! B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[End]
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
ENDIF

STORE '' TO LCOGWHILE,LCOGFOR
LNDSRECNO = IIF(RECNO()>RECCOUNT(),0,RECNO())

*-- Get the Scope.
LCOGSCOPE = LFGETSCOPE('lcCriteria')
LLWHILEFRST=(ATC('WHILE ',LCCRITERIA)<ATC('FOR ',LCCRITERIA)) && AND ATC('WHILE ',lcCriteria)>0

*-- While condition.
IF ATC('WHILE ',UPPER(LCCRITERIA))>0
  LCOGWHILE=IIF(LLWHILEFRST,SUBSTR(LCCRITERIA,1,ATC('FOR ',LCCRITERIA)-1),SUBSTR(LCCRITERIA,ATC('WHILE ',LCCRITERIA)))
ENDIF

*-- For Condition.
IF ATC('FOR ',UPPER(LCCRITERIA))>0
  LCOGFOR  =IIF(LLWHILEFRST,SUBSTR(LCCRITERIA,ATC('FOR ',LCCRITERIA)-1),SUBSTR(LCCRITERIA,ATC('FOR ',LCCRITERIA)-1,ATC(LCOGWHILE,LCCRITERIA)-ATC('FOR ',LCCRITERIA)))
ENDIF

LLCANDISP = (RECCOUNT() > 0)

IF !EMPTY(LCCRITERIA) OR !EMPTY(LCOGSCOPE)
  IF EMPTY(LCOGSCOPE)
    LOCATE   && Locate is faster than GoTop when the table filtered.
  ENDIF
  LCLOCTCOND = LCOGSCOPE+' '+LCOGFOR
  LOCATE &LCLOCTCOND.
  LLCANDISP = FOUND()
ENDIF

IF LNDSRECNO <> 0
  GO LNDSRECNO
ENDIF

IF !LLCANDISP  && No Records to display message.
  =GFMODALGEN("INM00052B00000","DIALOG")

  *AMH Last Day [Start]
  SET PRINTER TO
  SET CURSOR &LCSETCUR.
  ON ERROR &LCOLDERRHND.
  SET DEFAULT TO (LCSAVDFDIR)
  SET FULLPATH &LCFULLSET.
  SET CENTURY &LCOLDCENTURY
  SET DEVICE TO SCREEN
  *AMH Last Day [End]

  RETURN
ENDIF

*-- Reset the error handler.
LCOLDERRHND = ON('ERROR')
ON ERROR

*-- Now we should print the user selection criteria ..... BEGIN
*-- If o/p the printing and user want to print the criteria.

*B040220,1 AMH Preview the user selection criteria report [Start]
*IF (TYPE("llProceedPrn") = "U") AND;
(ALLTRIM(oAriaApplication.gcDevice) != "SCREEN") AND;
loOGScroll.llRpPrUSel
IF (TYPE("llProceedPrn") = "U") AND LOOGSCROLL.LLRPPRUSEL
  *B040220,1 AMH [End]

  PRIVATE LLPROCEEDPRN
  LLPROCEEDPRN = .F.
  ON ERROR &LCOLDERRHND
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  PRIVATE LLOLDPREPDF
  LLOLDPREPDF = LOOGSCROLL.LLPREVPDF

  *B040214,1 AMH Save the value of llPrintPDF since it will changes when calling PrintCriteria method [Start]
  PRIVATE LLOLDPRINTPDF
  LLOLDPRINTPDF = LOOGSCROLL.LLPRINTPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)
  LOOGSCROLL.PRINTCRITERIA()  && Print user selection criteria.

  *B040214,1 AMH Restore the llPrintPDF value [Start]
  LOOGSCROLL.LLPRINTPDF = LLOLDPRINTPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  LOOGSCROLL.LLPREVPDF = LLOLDPREPDF
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)

  IF !LLPROCEEDPRN

    *AMH Last Day [Start]
    SET PRINTER TO
    SET CURSOR &LCSETCUR.
    ON ERROR &LCOLDERRHND.
    SET DEFAULT TO (LCSAVDFDIR)
    SET FULLPATH &LCFULLSET.
    SET CENTURY &LCOLDCENTURY
    SET DEVICE TO SCREEN
    *AMH Last Day [End]

    RETURN .T.
  ENDIF
  RELEASE LLPROCEEDPRN
ENDIF
*-- Now we should print the user selection criteria ..... END


LCPRNTERROR = ""
*-- Set printer and lan escape sequences ... BEGIN
DO CASE
CASE OARIAAPPLICATION.GCDEVICE = "PRINTER"
  IF OARIAAPPLICATION.GLPRNT_LAN                && if it is a lan option
    LCPRINTSET = '\\SPOOLER\P='+ALLTRIM(STR(OARIAAPPLICATION.GNPRNT_NO))+'\S='+ALLTRIM(OARIAAPPLICATION.GCSERV_NAM);
      +'\Q='+ALLTRIM(OARIAAPPLICATION.GCQUEU_NAM);
      +IIF(_PCOPIES>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
      +IIF(OARIAAPPLICATION.GLBANER,'\B='+OARIAAPPLICATION.GCBANER_H,'\N')
  ELSE
    LCPRINTSET = OARIAAPPLICATION.GCPRNT_PORT
  ENDIF
  SET PRINTER TO

  *-- If preview or print to text file.
CASE OARIAAPPLICATION.GCDEVICE="SCREEN" OR;
    (OARIAAPPLICATION.GCDEVICE="FILE" AND (EMPTY(LOOGSCROLL.CTEXTREPTYPE) OR LOOGSCROLL.CTEXTREPTYPE="HTM"))

  IF LOOGSCROLL.CTEXTREPTYPE="HTM"
    GCOUTFILE = STRTRAN(GCOUTFILE,".HTM",".TXT")
  ENDIF

  IF LOOGSCROLL.LADDITIVE
    SET PRINTER TO (GCOUTFILE) ADDITIVE
  ELSE
    SET PRINTER TO (GCOUTFILE)
  ENDIF

ENDCASE
*-- Set printer and lan escape sequences ... END

LCSETCUR = SET('CURSOR')
SET CURSOR OFF

LCCRITERIA=IIF(EMPTY(LCOGSCOPE),'ALL',LCOGSCOPE)+' '+IIF(EMPTY(LCOGWHILE),"",LCOGWHILE+[])+;
  ' '+LCOGFOR

DO CASE
CASE OARIAAPPLICATION.GCDEVICE = "SCREEN"    && Preview report.

  *-- IF Windows .FRX or .LBX
  *! B128092,1 SMM 05/24/2005 Clear Tag1, TAG2 in preview  [START]
  IF LCRPRTLBL <> 'L'
    LNALIAS = SELECT()
    LCX = LCOGRPRTNAM + ".frx"
    USE (LCX) ALIAS TMPREP IN 0
    SELECT TMPREP
    LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(LOOGSCROLL.LCOGPLATFORM))
    IF FOUND()
      REPLACE TAG2 WITH '',TAG  WITH '',EXPR WITH ''
      LCEXPR = IIF(LOOGSCROLL.CCRORIENTATION='L','ORIENTATION=1','ORIENTATION=0')
      LCEXPR = LCEXPR + CHR(13)
      *MAH
      *	  lcExpr = lcExpr + IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1')
      LCEXPR = LCEXPR + IIF(ALLTRIM(UPPER(LOOGSCROLL.CCRPAPERSIZE)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(LOOGSCROLL.CCRPAPERSIZE ='A4','PAPERSIZE=9','PAPERSIZE=1'))
      REPLACE EXPR WITH LCEXPR
    ENDIF
    USE IN TMPREP
    SELECT(LNALIAS)
    *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[Start]
  ELSE
    LNALIAS = SELECT()
    LCX = LCOGRPRTNAM + ".LBX"
    USE (LCX) ALIAS TMPREP IN 0
    SELECT TMPREP
    LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(LOOGSCROLL.LCOGPLATFORM))
    IF FOUND()
      REPLACE TAG2 WITH '',TAG  WITH '',EXPR WITH ''
      LCEXPR = IIF(LOOGSCROLL.CCRORIENTATION='L','ORIENTATION=1','ORIENTATION=0')
      LCEXPR = LCEXPR + CHR(13)
      LCEXPR = LCEXPR + IIF(ALLTRIM(UPPER(LOOGSCROLL.CCRPAPERSIZE)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(LOOGSCROLL.CCRPAPERSIZE ='A4','PAPERSIZE=9','PAPERSIZE=1'))
      REPLACE EXPR WITH LCEXPR
    ENDIF
    USE IN TMPREP
    SELECT(LNALIAS)
    *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[End]
  ENDIF
  *! B128092,1 SMM 05/24/2005 Clear Tag1, TAG2 in preview  [END]

  LOOGSCROLL.PARENT.HIDE
  IF LCOGPLATFORM = 'WINDOW' OR LCOGPLATFORM = 'MAC'
    LOCAL LNSCALEMODE, LCWINDNAME, LCICONFILE, LCPREVIEWTITLE
    LCPREVIEWTITLE = IIF(TYPE('lcOGWinTitl')='C',LCOGWINTITL,'Report') + " Preview"
    LNSCALEMODE = _SCREEN.SCALEMODE

    _SCREEN.SCALEMODE = 0    && Foxel.
    LCWINDNAME = GFTEMPNAME()
    LCICONFILE = _SCREEN.ICON

    DEFINE WINDOW (LCWINDNAME) FROM 0,0 TO _SCREEN.HEIGHT - 2, _SCREEN.WIDTH ;
      FONT "Courier New",9 TITLE (LCPREVIEWTITLE);
      SYSTEM ICON FILE (LCICONFILE)

    _SCREEN.SCALEMODE = LNSCALEMODE

    IF OARIAAPPLICATION.GLHEADER
      IF LCRPRTLBL = 'L'
        *LABEL FORM &lcOGRprtNam. PREVIEW  WINDOW (lcWindName) &lcCriteria.
        LABEL FORM (LCOGRPRTNAM) PREVIEW &LCCRITERIA.
      ELSE
        *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [Start]
        *REPORT FORM (lcOGRprtNam) PREVIEW WINDOW (lcWindName) &lcCriteria.
        REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT PREVIEW WINDOW (LCWINDNAME) &LCCRITERIA.
        *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [End]
      ENDIF
    ELSE
      IF LCRPRTLBL = 'L'
        *LABEL FORM &lcOGRprtNam.  PREVIEW  WINDOW (lcWindName) PLAIN &lcCriteria.
        LABEL FORM (LCOGRPRTNAM)  PREVIEW PLAIN &LCCRITERIA.
      ELSE
        *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [Start]
        *REPORT FORM (lcOGRprtNam)  PREVIEW  WINDOW (lcWindName) PLAIN &lcCriteria.
        REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT PREVIEW  WINDOW (LCWINDNAME) PLAIN &LCCRITERIA.
        *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [End]
      ENDIF
    ENDIF
    RELEASE  WINDOW (LCWINDNAME)

  ELSE  && ELSE DOS .FRX, .LBX

    *-- Reset printer drivers ...
    LCTMPPRINTERSET = _PDSETUP
    _PDRIVER = ''
    _PDSETUP = ''

    *--         lcCriteria = IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+;
    *--           IIF(EMPTY(lcOGWhile),;
    *--           [ WHILE &lcPrntError. IIF(inkey()<>27,IIF(_PAGENO <> lnPPage,lfDispPage(),.T.),;
    *--           gfModalGen("QRM00114B00023","ALERT") = 1) ],;
    *--           lcOGWhile+[ AND IIF(CheckInKey(),IIF(_PAGENO<>lnPPage,lfDispPage(),.T.),;
    *--           gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
    *--           ' '+lcOGFor

    *-- Set the criteria.
    IF EMPTY(LCOGSCOPE)
      LCOGSCOPE = "ALL"
    ENDIF
    IF EMPTY(LCOGWHILE)
      LCOGWHILE = [ WHILE &lcPrntError. ]
    ELSE
      LCOGWHILE = LCOGWHILE + " AND "
    ENDIF

    LCCRITERIA = LCOGSCOPE + " " + LCOGWHILE + [CheckInKey()] + ' '+LCOGFOR

    _PAGENO = 1
    PRIVATE LODISPLAYPAGE,;    && Reference to the display page screen.
    LNPPAGE, LLCONTINUEPRINT

    LLCONTINUEPRINT = .T.
    LNPPAGE = 0
    LOCAL LCCLASSDIR
    LCCLASSDIR = ADDBS(OARIAAPPLICATION.CLASSDIR)

    PUSH KEY
    ON KEY
    LODISPLAYPAGE = NEWOBJECT("TextPrintStatus",LCCLASSDIR+"OptionGrid.vcx","",;
      OARIAAPPLICATION.GLHEADER, LCRPRTLBL, LCOGRPRTNAM, LCCRITERIA)
    LODISPLAYPAGE.SHOW()
    POP KEY

    IF LLCONTINUEPRINT AND !LLENDJOB
      =GFENDPRN()
    ENDIF

    _PDSETUP = LCTMPPRINTERSET    && Restore driver.
  ENDIF
  LOOGSCROLL.PARENT.SHOW
CASE OARIAAPPLICATION.GCDEVICE = "PRINTER"    && Print to printer.
  * Get the Enviroment variable called ARIAPRN contents
  * and if we were in DOS then we are in case of forcing
  * printing the report to file to ignore any errors while
  * updating some of the data files
  LCPRNTTOOL = GETENV('ARIAPRN')

  * redirect printing under windows to temprory file
  * and then copy this temprory file to the local port selected
  * port from the control pannel you can link the port to any
  * network queue you want

  IF LCOGPLATFORM='DOS' OR LCOGPLATFORM='UNIX'

    *-- DOS .FRX, .LBX
    OARIAAPPLICATION.GCOUTFILE = OARIAAPPLICATION.WORKDIR + GFTEMPNAME() + ".TXT"
    GCOUTFILE = OARIAAPPLICATION.GCOUTFILE
    SET PRINTER TO (GCOUTFILE)

    *-- Eject Page before printing ...
    IF _PEJECT $ "BOTH,BEFORE"
      LCADDV=_PADVANCE
      _PADVANCE="FORMFEED"
      EJECT
      _PADVANCE=LCADDV
    ENDIF

    *-- Print to File ... BEGIN
    PRINTJOB
      IF OARIAAPPLICATION.GLHEADER
        IF LCRPRTLBL = 'L'
          LABEL FORM (LCOGRPRTNAM) TO PRINTER NOCONSOLE &LCCRITERIA.
        ELSE
          REPORT FORM (LCOGRPRTNAM) TO PRINTER  NOCONSOLE NOEJECT &LCCRITERIA.
        ENDIF
      ELSE
        IF LCRPRTLBL = 'L'
          LABEL FORM (LCOGRPRTNAM) TO PRINTER NOCONSOLE &LCCRITERIA.
        ELSE
          REPORT FORM (LCOGRPRTNAM) TO PRINTER  NOEJECT NOCONSOLE PLAIN &LCCRITERIA.
        ENDIF
      ENDIF
    ENDPRINTJOB
    *-- Print to File ... END

    *-- Eject Page after printing ...
    IF _PEJECT $ "BOTH,BEFORE"
      LCADDV=_PADVANCE
      _PADVANCE="FORMFEED"
      EJECT
      _PADVANCE=LCADDV
    ENDIF

    SET PRINTER TO

    *-- set printer to local port
    *SET PRINTER TO (oAriaApplication.gcPrnt_Port)
    =SETPRINTERTO(OARIAAPPLICATION.GCPRNT_PORT)

    LLERROR=.F.
    LCONERR=ON('ERROR')
    ON ERROR LLERROR=.T.
    LCSETSAFE=SET('SAFE')
    SET SAFE OFF

    *-- copy temprory file to local port to print it
    * save the printer setup variable
    * and then clear it to Ignore the error
    * handler of the Genpd from working
    * and give the handler to the windows
    LCPDSETUP = _PDSETUP

    *! B127195,1 MAH 04/10/2005 [BEGIN]
    *-- _PDSETUP = ''
    *! B127195,1 MAH 04/10/2005 [END]

    *-- Detect printer status...
    DO WHILE SYS(13)='OFFLINE'
      IF GFMODALGEN("QRM00054B00015",'ALERT') = 2
        LLERROR=.T.
        EXIT
      ENDIF
    ENDDO

    IF FILEEXIST(GCOUTFILE) AND !LLERROR
      * Get the value of the output port in another variable
      GCLOCLPRT = OARIAAPPLICATION.GCPRNT_PORT

      *   ADDING NEW WAY OF PRINTING
      *-- IF ENVIROMENT VARIABLE FOR PRINTING IS DEFINED
      *-- THEN PRINT BY USING THE TECH. ??? ELSE USE THE COPY COMMAND
      *-- Take care that we are setting the printer to the port in which
      *-- we want to print to before starting printing
      DO CASE
        *Change the contition by using the variable
        * lcPrntTool instead of GETENV() so in DOS
        * we force the program to go through this situation.
      CASE LCPRNTTOOL = '???' OR LCPRNTTOOL = '??' OR LCPRNTTOOL = '?'
        LCTMPCUR = GFTEMPNAME() && temprory cursor
        LCWORKA = SELECT()      && curr. work area
        CREATE CURSOR &LCTMPCUR. (MPRNFILE M) && create temp. cursor
        APPEND BLANK
        *-- append memo with the output file
        APPEND MEMO MPRNFILE FROM (GCOUTFILE)
        *-- clear the printer port then set the device to printer with the
        *-- port needed to print.
        SET PRINTER TO
        * Take care of the lan printing so in case
        * DOS printing we will print to the spool if
        * needed but in windows there is no Lan option at all.
        IF OARIAAPPLICATION.GLPRNT_LAN                && if it is a lan option

          LCPRINTSET = '\\SPOOLER\P='+ALLTRIM(STR(OARIAAPPLICATION.GNPRNT_NO))+'\S='+ALLTRIM(OARIAAPPLICATION.GCSERV_NAM);
            +'\Q='+ALLTRIM(OARIAAPPLICATION.GCQUEU_NAM);
            +IIF(_PCOPIES>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
            +IIF(OARIAAPPLICATION.GLBANER,'\B='+OARIAAPPLICATION.GCBANER_H,'\N')
          SET PRINTER TO (LCPRINTSET)
        ELSE
          SET PRINTER TO (GCLOCLPRT)
        ENDIF

        SET PRINTER ON
        LNMEMOWID = SET('MEMOW')
        LCCONSSET = SET('CONS')
        LLWRAPSTAT = _WRAP
        LNINDENT = _INDENT
        LNRMARGIN = _RMARGIN
        LNLMARGIN = _LMARGIN
        SET CONS OFF
        SET MEMOW TO 65
        _WRAP = .T.
        _RMARGIN = 255
        _LMARGIN = 0
        _INDENT = 2

        *-- reset the printing cursor then start printing the memo field (Number of copies)
        FOR LNNOCOPY = 1 TO _PCOPIES
          &LCPRNTTOOL. MPRNFILE FUNCTION 'V253'
        ENDFOR
        *-- reset the device to screen and clear the printer buffer to make
        *-- the printer start printing.
        SET PRINTER OFF
        SET PRINTER TO
        SET CONS &LCCONSSET.
        SET MEMOW TO LNMEMOWID
        _WRAP = LLWRAPSTAT
        _INDENT = LNINDENT
        _RMARGIN = LNRMARGIN
        _LMARGIN = LNLMARGIN

        *-- close the temp. cursor and select the curr. work area
        USE IN &LCTMPCUR.
        SELECT (LCWORKA)

        * change the contition by using the variable
        * lcPrntTool instead of GETENV() so in DOS
        * we force the program to go through this situation.
      CASE LCPRNTTOOL="COPY"
        SET PRINTER TO (GCLOCLPRT)
        FOR LNNOCOPY = 1 TO _PCOPIES
          !COPY &GCOUTFILE. &GCLOCLPRT.    && DOS Copy to port.
        ENDFOR

      CASE LCPRNTTOOL="LIB"
        *--               IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE")
        *--                 SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
        *--                 lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPrt)
        *--                 lcOrgPrnt = PRTWIN(lcWinPrt)
        *--                 FOR lnNoCopy = 1 TO _PCOPIES
        *--                   =TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
        *--                 ENDFOR
        *--                 = PRTWIN(lcOrgPrnt)
        *--                 RELEASE LIBRARY TXTSPOOL.FLL
        *--               ENDIF

      CASE LCPRNTTOOL = 'TYPE'
        LCHEADSET = SET('HEADING')
        LNMEMOWID = SET('MEMOW')
        LCCONSSET = SET('CONS')
        LLWRAPSTAT = _WRAP
        LNRMARGIN = _RMARGIN
        LNLMARGIN = _LMARGIN
        SET MEMOW TO 255
        _WRAP = .F.
        _RMARGIN = 255
        _LMARGIN = 0
        SET HEADING OFF
        SET CONS OFF
        * Clear the Printer Driver that installed
        * to be used by Genpd
        LCTMPPRINTERSET =_PDSETUP
        _PDRIVER=''
        _PDSETUP=''
        SET PRINTER TO (OARIAAPPLICATION.GCPRNT_PORT)
        FOR LNNOCOPY = 1 TO _PCOPIES
          TYPE (GCOUTFILE) TO PRINTER
        ENDFOR
        SET PRINTER TO
        SET HEADING &LCHEADSET.
        SET CONS &LCCONSSET.
        SET MEMOW TO LNMEMOWID
        * Restore the Printer Driver that was in use by Genpd
        _PDSETUP = LCTMPPRINTERSET
        _WRAP = LLWRAPSTAT
        _RMARGIN = LNRMARGIN
        _LMARGIN = LNLMARGIN

      OTHERWISE    && Normal printing.
        FOR LNNOCOPY = 1 TO _PCOPIES
          COPY FILE (GCOUTFILE) TO (OARIAAPPLICATION.GCPRNT_PORT)
        ENDFOR

      ENDCASE
    ENDIF
    ON ERROR &LCONERR.
    SET SAFE &LCSETSAFE.

    _PDSETUP = LCPDSETUP    && Restore driver.

    *--delete temprory file
    ERASE (GCOUTFILE)

  ELSE  && Windows .FRX
    *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [Start]
    IF LCRPRTLBL <> 'L'
      *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [END]
      LOCAL LCEXPR
      LNALIAS = SELECT()
      LCX = LCOGRPRTNAM + ".frx"
      USE (LCX) ALIAS TMPREP IN 0
      SELECT TMPREP
      LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(LOOGSCROLL.LCOGPLATFORM))
      IF FOUND()
        REPLACE TAG2 WITH '',TAG  WITH ''
        *! B608311,1 MMT 10/11/2007 fix bug of not able to print legal paper size reports[Start]
        *REPLACE EXPR WITH ''
        LCEXPR = IIF(LOOGSCROLL.CCRORIENTATION='L','ORIENTATION=1','ORIENTATION=0')
        LCEXPR = LCEXPR + CHR(13)
        LCEXPR = LCEXPR + IIF(ALLTRIM(UPPER(LOOGSCROLL.CCRPAPERSIZE)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(LOOGSCROLL.CCRPAPERSIZE ='A4','PAPERSIZE=9','PAPERSIZE=1'))
        REPLACE EXPR WITH LCEXPR
        *! B608311,1 MMT 10/11/2007 fix bug of not able to print legal paper size reports[End]
      ENDIF
      USE IN TMPREP
      SELECT(LNALIAS)
      *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [Start]
    ELSE
      LNALIAS = SELECT()
      LCX = LCOGRPRTNAM + ".LBX"
      USE (LCX) ALIAS TMPREP IN 0
      SELECT TMPREP
      LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(LOOGSCROLL.LCOGPLATFORM))
      IF FOUND()
        REPLACE TAG2 WITH '',TAG  WITH '',EXPR WITH ''
        LCEXPR = IIF(LOOGSCROLL.CCRORIENTATION='L','ORIENTATION=1','ORIENTATION=0')
        LCEXPR = LCEXPR + CHR(13)
        LCEXPR = LCEXPR + IIF(ALLTRIM(UPPER(LOOGSCROLL.CCRPAPERSIZE)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(LOOGSCROLL.CCRPAPERSIZE ='A4','PAPERSIZE=9','PAPERSIZE=1'))
        REPLACE EXPR WITH LCEXPR
      ENDIF
      USE IN TMPREP
      SELECT(LNALIAS)
    ENDIF
    *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [END]



    *-- No Special handling.
    IF _PEJECT = 'NONE'
    ENDIF

    PRINTJOB
      IF OARIAAPPLICATION.GLHEADER
        IF LCRPRTLBL = 'L'
          LABEL FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOCONSOLE &LCCRITERIA.
        ELSE
          IF _PEJECT = 'NONE'
            REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOCONSOLE NOEJECT &LCCRITERIA.
          ELSE
            REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOCONSOLE &LCCRITERIA.
          ENDIF

        ENDIF

      ELSE

        IF LCRPRTLBL = 'L'
          LABEL FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOCONSOLE &LCCRITERIA.

        ELSE

          IF _PEJECT = 'NONE'
            REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOEJECT NOCONSOLE PLAIN &LCCRITERIA.
          ELSE
            REPORT FORM (LCOGRPRTNAM) TO PRINTER PROMPT NOCONSOLE PLAIN &LCCRITERIA.
          ENDIF

        ENDIF
      ENDIF
    ENDPRINTJOB
  ENDIF

CASE OARIAAPPLICATION.GCDEVICE = "FILE"    && Print to File.
  LOOGSCROLL.PRINTTOFILE(LCOGRPRTNAM,LCCRITERIA)

  *! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [START]
  IF LOOGSCROLL.LLPREVPDF .AND. FILE(LCPDFPATH)
    GFPDFVIEWER (LOOGSCROLL.LCOGWINTITL,LCPDFPATH)
    *B040161,1 AMH Restore the original device [Start]

    *B040214,1 AMH Restore the current device [Start]
    *oAriaApplication.gcDevice="SCREEN"
    OARIAAPPLICATION.GCDEVICE=LCOLDDEVICE
    *B040214,1 AMH [End]

    *B040161,1 AMH [End]
  ENDIF
  *! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [END]

  *B040214,1 AMH Printing the report using PDF [START]
  IF LOOGSCROLL.LLPRINTPDF .AND. FILE(LCPDFPATH)
    LOOGSCROLL.MPRINTPDF(LCPDFPATH)
    OARIAAPPLICATION.GCDEVICE = LCOLDDEVICE
  ENDIF
  *B040214,1 AMH [END]
ENDCASE

SET PRINTER TO
SET CURSOR &LCSETCUR.


* return the error handler setting to the
* previous routine
ON ERROR &LCOLDERRHND.

*Restore the original default directory
SET DEFAULT TO (LCSAVDFDIR)
SET FULLPATH &LCFULLSET.

*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [Start]
SET CENTURY &LCOLDCENTURY
*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [End]

SET DEVICE TO SCREEN
*-- end of the GfDispRe program.

*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************


*!*****************************************************************************************
*! Name      : lfGetScope
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/31/2002 02:38:25 PM
*! Purpose   :
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION LFGETSCOPE
LPARAMETERS LCOGCRITERIA

*ON ERROR do lperr with ERROR(), LINENO(), PROGRAM(), MESSAGE(), MESSAGE(1)

LOCAL LCOGEXACT, LCOGSCOPE, LCOGREC, LCOGRCCONT
LCOGEXACT = SET('EXACT')
SET EXACT ON

LCOGSCOPE = ''
&LCOGCRITERIA. = LFCONDUPPER(&LCOGCRITERIA.)  && Get Condition Upper.
&LCOGCRITERIA. = IIF(LEFT(&LCOGCRITERIA.,1)=' ','',' ')+;
  &LCOGCRITERIA. + IIF(RIGHT(&LCOGCRITERIA.,1)=' ','',' ')

LCCRITERIA = &LCOGCRITERIA.
LCOGSCOPE  = LCOGSCOPE + IIF(&LCOGCRITERIA.<>STRTRAN(&LCOGCRITERIA.,' ALL '),'ALL ','')

&LCOGCRITERIA. = STRTRAN(&LCOGCRITERIA.,' ALL ' , ' ')
&LCOGCRITERIA. = IIF(LEFT(&LCOGCRITERIA.,1)=' ','',' ')+;
  &LCOGCRITERIA. + IIF(RIGHT(&LCOGCRITERIA.,1)=' ','',' ')
LCOGSCOPE = LCOGSCOPE + IIF(&LCOGCRITERIA.<>STRTRAN(&LCOGCRITERIA.,' REST '),'REST ','')

&LCOGCRITERIA. = STRTRAN(&LCOGCRITERIA. , ' REST ' , ' ')

&LCOGCRITERIA. = IIF(LEFT(&LCOGCRITERIA.,1)=' ','',' ')+;
  &LCOGCRITERIA. + IIF(RIGHT(&LCOGCRITERIA.,1)=' ','',' ')

IF ATC(' RECORD ',&LCOGCRITERIA.) > 0
  LCOGREC = ALLTRIM(SUBSTR(&LCOGCRITERIA.,ATC(' RECORD ',&LCOGCRITERIA.)+8))
  LCOGREC = LCOGREC+' '
  LCOGRCCONT = SUBSTR(LCOGREC,1,ATC(' ',LCOGREC)-1)
  LCOGSCOPE  = LCOGSCOPE + 'RECORD ' + LCOGRCCONT + ' '
  LCOGREC    = SUBSTR(&LCOGCRITERIA.,ATC(' RECORD ',&LCOGCRITERIA.),;
    (ATC(' RECORD ',&LCOGCRITERIA)+LEN(LCOGRCCONT)+8)-ATC(' RECORD ',&LCOGCRITERIA.))
  &LCOGCRITERIA. = STRTRAN(&LCOGCRITERIA.,LCOGREC)
ENDIF

&LCOGCRITERIA. = IIF(LEFT(&LCOGCRITERIA.,1)=' ','',' ')+;
  &LCOGCRITERIA. + IIF(RIGHT(&LCOGCRITERIA.,1)=' ','',' ')

IF ATC(' NEXT ',&LCOGCRITERIA.)>0
  LCOGREC = ALLTRIM(SUBSTR(&LCOGCRITERIA.,ATC(' NEXT ',&LCOGCRITERIA.)+6))
  LCOGREC = LCOGREC + ' '
  LCOGRCCONT = SUBSTR(LCOGREC,1,ATC(' ',LCOGREC)-1)
  LCOGSCOPE = LCOGSCOPE + IIF(!EMPTY(LCOGREC),'NEXT '+LCOGRCCONT,'')
  LCOGREC = SUBSTR(&LCOGCRITERIA.,ATC(' NEXT ',&LCOGCRITERIA.),(ATC(' NEXT ',&LCOGCRITERIA.)+LEN(LCOGRCCONT)+6)-ATC(' NEXT ',&LCOGCRITERIA.))
  &LCOGCRITERIA. = STRTRAN(&LCOGCRITERIA.,LCOGREC)
ENDIF
SET EXACT &LCOGEXACT.
RETURN LCOGSCOPE
ENDFUNC
*-- end of lfGetScope.

*!*****************************************************************************************
*! Name      : lfCondUpper
*! Date      : 10/31/2002 03:18:27 PM
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION LFCONDUPPER
LPARAMETERS LCOGCRITERIA
LOCAL LNOGCOUNT, LLTOUP, LCOGUPCRIT
LLTOUP     = .T.
LCOGUPCRIT = ''
LNOGCOUNT  = 0

FOR LNOGCOUNT = 1 TO LEN(LCOGCRITERIA)
  IF SUBSTR(LCOGCRITERIA,LNOGCOUNT,1) $ ['"]
    LLTOUP=!LLTOUP
  ENDIF
  LCOGUPCRIT = LCOGUPCRIT + IIF(LLTOUP,UPPER(SUBSTR(LCOGCRITERIA,LNOGCOUNT,1)),;
    SUBSTR(LCOGCRITERIA,LNOGCOUNT,1))
ENDFOR
RETURN LCOGUPCRIT
ENDFUNC
*-- end of lfCondUpper.

*!*****************************************************************************************
*! Name      : lfDispPage
*! Date      : 10/31/2002 03:18:27 PM
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION LFDISPPAGE
LODISPLAYPAGE.REFRESHPAGE()
LNPPAGE = _PAGENO
ENDFUNC
*-- end of lfDispPage.

FUNCTION CHECKINKEY
IF !LLCONTINUEPRINT
  RETURN .F.
ENDIF

IF INKEY() = 27
  IF GFMODALGEN("QRM00114B00023","ALERT") = 1
    LLCONTINUEPRINT = .T.
  ELSE
    LLCONTINUEPRINT = .F.
  ENDIF
ELSE
  IF _PAGENO != LNPPAGE
    LFDISPPAGE()
  ENDIF
  LLCONTINUEPRINT = .T.
ENDIF
RETURN LLCONTINUEPRINT
ENDFUNC

*!*****************************************************************************************
*! Name      : SetProceed
*! Date      : 06/22/2003 03:18:27 PM
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
PROCEDURE SETPROCEED
LLPROCEEDPRN = .T.
RETURN ""
ENDPROC && end of SetProceed.
*!*****************************************************************************************
*! Name      : lfPrintCritria
*! Developer : WLD - Walid Hamed
*! Date      : 07/31/2007
*! Entry no. : B608190,1
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION LFPRINTCRITRIA

LCOLDERRHND = ON('ERROR')
ON ERROR
*-- Now we should print the user selection criteria ..... BEGIN
*-- If o/p the printing and user want to print the criteria.

*B040220,1 AMH Preview the user selection criteria report [Start]
*IF (TYPE("llProceedPrn") = "U") AND;
(ALLTRIM(oAriaApplication.gcDevice) != "SCREEN") AND;
loOGScroll.llRpPrUSel
IF (TYPE("llProceedPrn") = "U") AND LOOGSCROLL.LLRPPRUSEL
  *B040220,1 AMH [End]

  PRIVATE LLPROCEEDPRN
  LLPROCEEDPRN = .F.
  ON ERROR &LCOLDERRHND
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  PRIVATE LLOLDPREPDF
  LLOLDPREPDF = LOOGSCROLL.LLPREVPDF

  *B040214,1 AMH Save the value of llPrintPDF since it will changes when calling PrintCriteria method [Start]
  PRIVATE LLOLDPRINTPDF
  LLOLDPRINTPDF = LOOGSCROLL.LLPRINTPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)
  LOOGSCROLL.PRINTCRITERIA()  && Print user selection criteria.

  *B040214,1 AMH Restore the llPrintPDF value [Start]
  LOOGSCROLL.LLPRINTPDF = LLOLDPRINTPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  LOOGSCROLL.LLPREVPDF = LLOLDPREPDF
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)

  IF !LLPROCEEDPRN
    *AMH Last Day [Start]
    SET PRINTER TO
    SET CURSOR &LCSETCUR.
    ON ERROR &LCOLDERRHND.
    SET DEFAULT TO (LCSAVDFDIR)
    SET FULLPATH &LCFULLSET.
    SET CENTURY &LCOLDCENTURY
    SET DEVICE TO SCREEN
    *AMH Last Day [End]
    RETURN .T.
  ENDIF
  RELEASE LLPROCEEDPRN
ENDIF
*-- Now we should print the user selection criteria ..... END
