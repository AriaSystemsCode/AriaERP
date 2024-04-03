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
*! E612378,1 MMT 05/10/2021 Aria5 new UX
*!*****************************************************************************************


FUNCTION gfDispRe
PARAMETERS lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile
PRIVATE lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile    &&

*! T20080115.0013 26,Feb 2008 Change the date format to British to pass int to SQL in case SQL
IF TYPE("oAriaApplication.cActCompDateFormat") = 'C' .AND. ;
   !EMPTY(oAriaApplication.cActCompDateFormat)
  lcTemp = oAriaApplication.cActCompDateFormat
  SET DATE &lcTemp.
ENDIF
*! T20080115.0013 END

*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfAddPerfLog("OPTIONGRID", "PRINT", "END", "")
*! B128052,1 MAH 06/05/2005 [END]

*-- Used variables in this session.
PRIVATE lcTmpPrinterSet,;            && Save Old Printer Setup
        lcSavDfDir,;                 && Save Current directory.
        lcFullSet,;                  && Save FullPath setting
        gcOutFile,;                  && Output File
        lcOGWhile,;
        lcOGFor,;
        lnDsRecNo,;
        lcOGScope

*-- Save current default directory
lcFullSet  = SET("Fullpath")
SET FULLPATH ON
lcSavDfDir = FULLPATH(SET('DEFAULT'))

*wael
*-- Reset the error handler.
lcOldErrHnd = ON('ERROR')
lcSetCur = SET('CURSOR')
*wael

*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [Start]
LOCAL lcOldCentury
lcOldCentury = SET("Century" )
SET CENTURY OFF
*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [End]

*! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [START]

*B040214,1 AMH Define variable to restore the current device [Start]
LOCAL lcOldDevice
lcOldDevice = oAriaApplication.gcDevice
*B040214,1 AMH [End]

LOCAL lcPDFPath
loOGScroll.llPrevPDF = .F.
IF (oAriaApplication.gcDevice="SCREEN" .OR. loOGScroll.llPrevPDF).AND. !LoOGScroll.llCrystal .AND. LoOGScroll.lUsePDFViewer
  oAriaApplication.gcDevice="FILE"
  lcPDFPath = loOGScroll.gfTempName()
  loOGScroll.cTextRepType = "PDF"	
  lcPDFPath = oAriaApplication.WorkDir + lcPDFPath + ".PDF"
  oAriaApplication.gcOutFile = lcPDFPath
  loOGScroll.llPrevPDF = .T.
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
gcOutFile = oAriaApplication.gcOutFile
lcRprtLbl = IIF(TYPE('lcRprtLbl') $ 'UL','R',lcRprtLbl)

llPrntToFile = IIF(TYPE('llPrntToFile')='L',llPrntToFile,.F.)
lcCriteria   = IIF(TYPE('lcCriteria')='C' AND !EMPTY(lcCriteria),lcCriteria,'')
lcOGRprtNam = IIF(TYPE('lcOGTmpForm')<>'C' OR EMPTY(lcOGTmpForm),lcOGRprtNam,oAriaApplication.WorkDir+lcOGTmpForm)

*! E038338,1 SMM Use gfDispRe to display Crystal Reports [START]
IF LoOGScroll.llCrystal
  *B608190,1 WLD 07/31/2007 Printing Selection Criteria Setting Report in case of Crystal report [Begin]
  LoOGScroll.llCrystal = .F.
  *Printing Selection Criteria Setting Report in a Portrait page
  lcDefaultOrnt = loOGScroll.cCRorientation
  loOGScroll.cCRorientation = 'P'
  lfPrintCritria()
  LoOGScroll.llCrystal = .T.
  loOGScroll.cCRorientation = lcDefaultOrnt
  *B608190,1 WLD 07/31/2007 Printing Selection Criteria Setting Report in case of Crystal report [End]

  loOGScroll.mDoCrystalReport()
  loOGScroll.oReport  = NULL
  loOGScroll.oCrystal = NULL
  loOGScroll.llOGFltCh = .F.

  *AMH Last Day [Start]
  SET PRINTER TO
  SET CURSOR &lcSetCur.
  ON ERROR &lcOldErrHnd.
  SET DEFAULT TO (lcSavDfDir)
  SET FULLPATH &lcFullSet.
  SET CENTURY &lcOldCentury
  SET DEVICE TO SCREEN
  *AMH Last Day [End]

  RETURN .T.
ENDIF
*! E038338,1 SMM Use gfDispRe to display Crystal Reports [END]

*-- Get report name.
IF RAT('\',lcOGRprtNam)=0

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
 lcSrvRpts = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')

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

 IF (oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX')))OR ;
      FileExist(oAriaApplication.ReportHome + lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.ReportHome + lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\' +lcOGRprtNam+'.LBX') OR ;
      FileExist(lcSrvRpts + lcOGRprtNam+'.FRX') OR ;
      FileExist(lcSrvRpts + lcOGRprtNam+'.LBX') OR ;
      FileExist(lcSrvRpts + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam +'.FRX') OR ;
      FileExist(lcSrvRpts + oAriaApplication.ActiveModuleID+'\' +lcOGRprtNam+'.LBX')

    IF oAriaApplication.MULTIINST AND (FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.clientreporthome + lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.clientreporthome + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.clientreporthome + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.LBX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
      FileExist(oAriaApplication.CLIENTSRVREPORTHOME + oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX'))

	  IF FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
                        FileExist(oAriaApplication.clientreporthome + lcOGRprtNam+'.LBX') OR ;
                        FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.FRX') OR ;
					    FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam+'.LBX')
					
        IF  FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.FRX') OR ;
                        FileExist(oAriaApplication.clientreporthome+ lcOGRprtNam+'.LBX')

           lcOGRprtNam = oAriaApplication.clientreporthome + lcOGRprtNam
        ELSE
          lcOGRprtNam = oAriaApplication.CLIENTSRVREPORTHOME + lcOGRprtNam
        ENDIF
 *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END

      ELSE
        IF FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.FRX') OR ;
	       FileExist(oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+ lcOGRprtNam+'.LBX')
          lcOGRprtNam = oAriaApplication.clientreporthome+oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam
        ELSE
           *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN
*!*	           lcOGRprtNam = 'X:\Aria4xp\SRVRPTS\'+oAriaApplication.ActiveModuleID
           *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
           *lcOGRprtNam = oAriaApplication.CLIENTSRVREPORTHOME + oAriaApplication.ActiveModuleID
           lcOGRprtNam = oAriaApplication.CLIENTSRVREPORTHOME + ADDBS(oAriaApplication.ActiveModuleID)
           *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]
           *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END
        ENDIF
      ENDIF
    ELSE
      IF FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') .OR.;
         FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX') OR ;
         FileExist( lcSrvRpts  +lcOGRprtNam+'.FRX') OR ;
    	 FileExist(lcSrvRpts +lcOGRprtNam+'.LBX')
		
		IF FileExist( oAriaApplication.ReportHome +lcOGRprtNam+'.FRX') .OR.;
            FileExist(oAriaApplication.ReportHome+lcOGRprtNam+'.LBX')
           lcOGRprtNam =   oAriaApplication.ReportHome +lcOGRprtNam
        ELSE
          lcOGRprtNam = lcSrvRpts +lcOGRprtNam
        ENDIF
	  ELSE
	    IF FileExist( oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam +'.FRX') OR ;
   		   FileExist( oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\'+lcOGRprtNam +'.LBX')
   		  *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
  	      *lcOGRprtNam = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+lcOGRprtNam   		  
  	      lcOGRprtNam = oAriaApplication.ReportHome + ADDBS(oAriaApplication.ActiveModuleID) +lcOGRprtNam
          *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]  	      
	    ELSE
   	       *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [Begin]
	       *lcOGRprtNam = lcSrvRpts + oAriaApplication.ActiveModuleID +lcOGRprtNam
	       lcOGRprtNam = lcSrvRpts + ADDBS(oAriaApplication.ActiveModuleID) +lcOGRprtNam	       
           *!* B610409,1 HIA 07/03/13 T20130627.0025 - Cannot view / print financial report [End]
	    ENDIF
	  ENDIF 				
    ENDIF
  ENDIF
*B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[End]
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
ENDIF

STORE '' TO lcOGWhile,lcOGFor
lnDsRecNo = IIF(RECNO()>RECCOUNT(),0,RECNO())

*-- Get the Scope.
lcOGScope = lfGetScope('lcCriteria')
llWhileFrst=(ATC('WHILE ',lcCriteria)<ATC('FOR ',lcCriteria)) && AND ATC('WHILE ',lcCriteria)>0

*-- While condition.
IF ATC('WHILE ',UPPER(lcCriteria))>0
  lcOGWhile=IIF(llWhileFrst,SUBSTR(lcCriteria,1,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('WHILE ',lcCriteria)))
ENDIF

*-- For Condition.
IF ATC('FOR ',UPPER(lcCriteria))>0
  lcOGFor  =IIF(llWhileFrst,SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1,ATC(lcOGWhile,lcCriteria)-ATC('FOR ',lcCriteria)))
ENDIF

llCanDisp = (RECCOUNT() > 0)

IF !EMPTY(lcCriteria) OR !EMPTY(lcOGScope)
  IF EMPTY(lcOGScope)
    LOCATE   && Locate is faster than GoTop when the table filtered.
  ENDIF
  lcLoctCond = lcOGScope+' '+lcOGFor
  LOCATE &lcLoctCond.
  llCanDisp = FOUND()
ENDIF

IF lnDsRecNo <> 0
  GO lnDsRecNo
ENDIF

IF !llCanDisp  && No Records to display message.
  =gfModalGen("INM00052B00000","DIALOG")

  *AMH Last Day [Start]
  SET PRINTER TO
  SET CURSOR &lcSetCur.
  ON ERROR &lcOldErrHnd.
  SET DEFAULT TO (lcSavDfDir)
  SET FULLPATH &lcFullSet.
  SET CENTURY &lcOldCentury
  SET DEVICE TO SCREEN
  *AMH Last Day [End]

  RETURN
ENDIF

*-- Reset the error handler.
lcOldErrHnd = ON('ERROR')
ON ERROR

*-- Now we should print the user selection criteria ..... BEGIN
*-- If o/p the printing and user want to print the criteria.

*B040220,1 AMH Preview the user selection criteria report [Start]
*IF (TYPE("llProceedPrn") = "U") AND;
   (ALLTRIM(oAriaApplication.gcDevice) != "SCREEN") AND;
  loOGScroll.llRpPrUSel
IF (TYPE("llProceedPrn") = "U") AND loOGScroll.llRpPrUSel
*B040220,1 AMH [End]

  PRIVATE llProceedPrn
  llProceedPrn = .F.
  ON ERROR &lcOldErrHnd
 *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  PRIVATE llOldPrePDF
  llOldPrePDF = loOGScroll.llPrevPDF

  *B040214,1 AMH Save the value of llPrintPDF since it will changes when calling PrintCriteria method [Start]
  PRIVATE llOldPrintPDF
  llOldPrintPDF = loOGScroll.llPrintPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)
  loOGScroll.PrintCriteria()  && Print user selection criteria.

  *B040214,1 AMH Restore the llPrintPDF value [Start]
  loOGScroll.llPrintPDF = llOldPrintPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  loOGScroll.llPrevPDF = llOldPrePDF
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)

  IF !llProceedPrn

    *AMH Last Day [Start]
    SET PRINTER TO
    SET CURSOR &lcSetCur.
    ON ERROR &lcOldErrHnd.
    SET DEFAULT TO (lcSavDfDir)
    SET FULLPATH &lcFullSet.
    SET CENTURY &lcOldCentury
    SET DEVICE TO SCREEN
    *AMH Last Day [End]

    RETURN .T.
  ENDIF
  RELEASE llProceedPrn
ENDIF
*-- Now we should print the user selection criteria ..... END


lcPrntError = ""
*-- Set printer and lan escape sequences ... BEGIN
DO CASE
  CASE oAriaApplication.gcDevice = "PRINTER"
    IF oAriaApplication.glPrnt_Lan                && if it is a lan option
      lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(oAriaApplication.gnPrnt_No))+'\S='+ALLTRIM(oAriaApplication.gcServ_Nam);
        +'\Q='+ALLTRIM(oAriaApplication.gcQueu_nam);
        +IIF(_PCOPIES>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
        +IIF(oAriaApplication.glBaner,'\B='+oAriaApplication.gcBaner_H,'\N')
    ELSE
      lcPrintSet = oAriaApplication.gcPrnt_Port
    ENDIF
    SET PRINTER TO

  *-- If preview or print to text file.
  CASE oAriaApplication.gcDevice="SCREEN" OR;
      (oAriaApplication.gcDevice="FILE" AND (EMPTY(loOGScroll.cTextRepType) OR loOGScroll.cTextRepType="HTM"))

    IF loOGScroll.cTextRepType="HTM"
      gcOutFile = STRTRAN(gcOutFile,".HTM",".TXT")
    ENDIF

    IF loOGScroll.lAdditive
      SET PRINTER TO (gcOutFile) ADDITIVE
    ELSE
      SET PRINTER TO (gcOutFile)
    ENDIF

ENDCASE
*-- Set printer and lan escape sequences ... END

lcSetCur = SET('CURSOR')
SET CURSOR OFF

lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),"",lcOGWhile+[])+;
  ' '+lcOGFor

DO CASE
  CASE oAriaApplication.gcDevice = "SCREEN"    && Preview report.

    *-- IF Windows .FRX or .LBX
    *! B128092,1 SMM 05/24/2005 Clear Tag1, TAG2 in preview  [START]
    IF lcRprtLbl <> 'L'
	lnAlias = SELECT()
	lcx = lcOGRprtNam + ".frx"
	USE (lcX) ALIAS TmpRep IN 0
    SELECT tmprep
    LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(loOGScroll.lcOGPlatform))
    IF FOUND()
      REPLACE TAG2 WITH '',tag  WITH '',EXPR WITH ''
 	 lcExpr = IIF(loOGScroll.cCRorientation='L','ORIENTATION=1','ORIENTATION=0')  	
 	 lcExpr = lcExpr + CHR(13)
 	 *MAH
 *	  lcExpr = lcExpr + IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1')  	
      lcExpr = lcExpr + IIF(ALLTRIM(UPPER(loOGScroll.cCRPapersize)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1'))
 	 REPLACE EXPR WITH lcExpr
 	 *BEYONDPRO_FRX
      =FRX_Globalize()  && BEYONDPRO_FRX
      *BEYONDPRO_FRX
    ENDIF
	USE IN TmpRep	
    SELECT(lnAlias)
    *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[Start]
    ELSE
      lnAlias = SELECT()
      lcx = lcOGRprtNam + ".LBX"
      USE (lcX) ALIAS TmpRep IN 0
      SELECT tmprep
      LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(loOGScroll.lcOGPlatform))
      IF FOUND()
        REPLACE TAG2 WITH '',tag  WITH '',EXPR WITH ''
        lcExpr = IIF(loOGScroll.cCRorientation='L','ORIENTATION=1','ORIENTATION=0')
        lcExpr = lcExpr + CHR(13)
        lcExpr = lcExpr + IIF(ALLTRIM(UPPER(loOGScroll.cCRPapersize)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1'))
        REPLACE EXPR WITH lcExpr
   	 *BEYONDPRO_FRX
        =FRX_Globalize()  && BEYONDPRO_FRX        
   	 *BEYONDPRO_FRX         	     
      ENDIF
      USE IN TmpRep
      SELECT(lnAlias)
    *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[End]
    ENDIF
    *! B128092,1 SMM 05/24/2005 Clear Tag1, TAG2 in preview  [END]
    *E612378,1 MMT 05/10/2021 Aria5 new UX[Start]
    IF oAriaApplication.Context = 5 OR oAriaApplication.Context = 54 OR oAriaApplication.llNewInterFace
       loOGScroll.Parent.Parent.Parent.Hide()
    ELSE 
    *E612378,1 MMT 05/10/2021 Aria5 new UX[End]
      loOGScroll.Parent.Hide
    *E612378,1 MMT 05/10/2021 Aria5 new UX[Start]
    ENDIF
    *E612378,1 MMT 05/10/2021 Aria5 new UX[End]
    
    IF lcOGPlatForm = 'WINDOW' OR lcOGPlatForm = 'MAC'
      LOCAL lnScaleMode, lcWindName, lcIconFile, lcPreviewTitle
      lcPreviewTitle = IIF(TYPE('lcOGWinTitl')='C',lcOGWinTitl,'Report') + " Preview"
      lnScaleMode = _Screen.ScaleMode

      _Screen.ScaleMode = 0    && Foxel.
      lcWindName = gfTempName()
      lcIconFile = _Screen.Icon

      DEFINE WINDOW (lcWindName) FROM 0,0 TO _Screen.Height - 2, _Screen.Width ;
       FONT "Courier New",9 TITLE (lcPreviewTitle);
        SYSTEM ICON FILE (lcIconFile)

      _screen.ScaleMode = lnScaleMode

      IF oAriaApplication.glHeader
        IF lcRprtLbl = 'L'
          *LABEL FORM &lcOGRprtNam. PREVIEW  WINDOW (lcWindName) &lcCriteria.
          LABEL FORM (lcOGRprtNam) PREVIEW &lcCriteria.
        ELSE
          *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [Start]
          *REPORT FORM (lcOGRprtNam) PREVIEW WINDOW (lcWindName) &lcCriteria.
          REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT PREVIEW WINDOW (lcWindName) &lcCriteria.
          *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [End]
        ENDIF
      ELSE
        IF lcRprtLbl = 'L'
          *LABEL FORM &lcOGRprtNam.  PREVIEW  WINDOW (lcWindName) PLAIN &lcCriteria.
          LABEL FORM (lcOGRprtNam)  PREVIEW PLAIN &lcCriteria.
        ELSE
          *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [Start]
          *REPORT FORM (lcOGRprtNam)  PREVIEW  WINDOW (lcWindName) PLAIN &lcCriteria.
          REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT PREVIEW  WINDOW (lcWindName) PLAIN &lcCriteria.
          *! E302948,1 SAB 07/27/2011 Show select printer dialog when press print from report preview [End]
        ENDIF
      ENDIF
      RELEASE  WINDOW (lcWindName)

    ELSE  && ELSE DOS .FRX, .LBX

      *-- Reset printer drivers ...
      lcTmpPrinterSet = _PDSETUP
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
      IF EMPTY(lcOGScope)
        lcOGScope = "ALL"
      ENDIF
      IF EMPTY(lcOGWhile)
        lcOGWhile = [ WHILE &lcPrntError. ]
      ELSE
        lcOGWhile = lcOGWhile + " AND "
      ENDIF

      lcCriteria = lcOGScope + " " + lcOGWhile + [CheckInKey()] + ' '+lcOGFor

      _PAGENO = 1
      PRIVATE loDisplayPage,;    && Reference to the display page screen.
              lnPPage, llContinuePrint

      llContinuePrint = .T.
      lnPPage = 0
      LOCAL lcClassDir
      lcClassDir = ADDBS(oAriaApplication.ClassDir)

      PUSH KEY
      ON KEY
      loDisplayPage = NEWOBJECT("TextPrintStatus",lcClassDir+"OptionGrid.vcx","",;
        oAriaApplication.glHeader, lcRprtLbl, lcOGRprtNam, lcCriteria)
      loDisplayPage.Show()
      POP KEY

      IF llContinuePrint AND !llEndJob
        =gfEndPrn()
      ENDIF

      _PDSETUP = lcTmpPrinterSet    && Restore driver.
    ENDIF
    *E612378,1 MMT 05/10/2021 Aria5 new UX[Start]
    IF oAriaApplication.Context = 5 OR oAriaApplication.Context = 54 OR oAriaApplication.llNewInterFace
       loOGScroll.Parent.Parent.Parent.Show()
    ELSE 
    *E612378,1 MMT 05/10/2021 Aria5 new UX[End]
      loOGScroll.Parent.SHOW
    *E612378,1 MMT 05/10/2021 Aria5 new UX[Start]
    ENDIF
    *E612378,1 MMT 05/10/2021 Aria5 new UX[End]
    
  CASE oAriaApplication.gcDevice = "PRINTER"    && Print to printer.
    * Get the Enviroment variable called ARIAPRN contents
    * and if we were in DOS then we are in case of forcing
    * printing the report to file to ignore any errors while
    * updating some of the data files
    lcPrntTool = GETENV('ARIAPRN')

    * redirect printing under windows to temprory file
    * and then copy this temprory file to the local port selected
    * port from the control pannel you can link the port to any
    * network queue you want

    IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'

      *-- DOS .FRX, .LBX
      oAriaApplication.gcOutFile = oAriaApplication.WorkDir + gfTempName() + ".TXT"
      gcOutFile = oAriaApplication.gcOutFile
      SET PRINTER TO (gcOutFile)

      *-- Eject Page before printing ...
      IF _PEJECT $ "BOTH,BEFORE"
        lcAddv=_PADVANCE
        _PADVANCE="FORMFEED"
        EJECT
        _PADVANCE=lcAddv
      ENDIF

      *-- Print to File ... BEGIN
      PRINTJOB
        IF oAriaApplication.glHeader
          IF lcRprtLbl = 'L'
            LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria.
          ELSE
            REPORT FORM (lcOGRprtNam) TO PRINTER  NOCONSOLE NOEJECT &lcCriteria.
          ENDIF
        ELSE
          IF lcRprtLbl = 'L'
            LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria.
          ELSE
            REPORT FORM (lcOGRprtNam) TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria.
          ENDIF
        ENDIF
      ENDPRINTJOB
      *-- Print to File ... END

      *-- Eject Page after printing ...
      IF _PEJECT $ "BOTH,BEFORE"
        lcAddv=_PADVANCE
        _PADVANCE="FORMFEED"
        EJECT
        _PADVANCE=lcAddv
      ENDIF

      SET PRINTER TO

      *-- set printer to local port
      *SET PRINTER TO (oAriaApplication.gcPrnt_Port)
      =SetPrinterTo(oAriaApplication.gcPrnt_Port)

      llError=.F.
      lcOnErr=ON('ERROR')
      ON ERROR llError=.T.
      lcSetSafe=SET('SAFE')
      SET SAFE OFF

      *-- copy temprory file to local port to print it
      * save the printer setup variable
      * and then clear it to Ignore the error
      * handler of the Genpd from working
      * and give the handler to the windows
      lcPDSetup = _PDSETUP

      *! B127195,1 MAH 04/10/2005 [BEGIN]
      *-- _PDSETUP = ''
      *! B127195,1 MAH 04/10/2005 [END]

      *-- Detect printer status...
      DO WHILE SYS(13)='OFFLINE'
        IF gfModalGen("QRM00054B00015",'ALERT') = 2
          llError=.T.
          EXIT
        ENDIF
      ENDDO

      IF FileExist(gcOutFile) AND !llError
        * Get the value of the output port in another variable
        gcLoclPrt = oAriaApplication.gcPrnt_Port

        *   ADDING NEW WAY OF PRINTING
        *-- IF ENVIROMENT VARIABLE FOR PRINTING IS DEFINED
        *-- THEN PRINT BY USING THE TECH. ??? ELSE USE THE COPY COMMAND
        *-- Take care that we are setting the printer to the port in which
        *-- we want to print to before starting printing
        DO CASE
          *Change the contition by using the variable
          * lcPrntTool instead of GETENV() so in DOS
          * we force the program to go through this situation.
          CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
            lcTmpCur = gfTempName() && temprory cursor
            lcWorkA = SELECT()      && curr. work area
            CREATE CURSOR &lcTmpCur. (mprnfile M) && create temp. cursor
            APPEND BLANK
            *-- append memo with the output file
            APPEND MEMO mprnfile FROM (gcOutFile)
            *-- clear the printer port then set the device to printer with the
            *-- port needed to print.
            SET PRINTER TO
            * Take care of the lan printing so in case
            * DOS printing we will print to the spool if
            * needed but in windows there is no Lan option at all.
            IF oAriaApplication.glPrnt_Lan                && if it is a lan option

              lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(oAriaApplication.gnPrnt_No))+'\S='+ALLTRIM(oAriaApplication.gcServ_Nam);
                +'\Q='+ALLTRIM(oAriaApplication.gcQueu_nam);
                +IIF(_PCOPIES>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                +IIF(oAriaApplication.glBaner,'\B='+oAriaApplication.gcBaner_H,'\N')
              SET PRINTER TO (lcPrintSet)
            ELSE
              SET PRINTER TO (gcLoclPrt)
            ENDIF

            SET PRINTER ON
            lnMemoWid = SET('MEMOW')
            lcConsSet = SET('CONS')
            llWrapStat = _WRAP
            lnIndent = _INDENT
            lnRMargin = _RMARGIN
            lnLMargin = _LMARGIN
            SET CONS OFF
            SET MEMOW TO 65
            _WRAP = .T.
            _RMARGIN = 255
            _LMARGIN = 0
            _INDENT = 2

            *-- reset the printing cursor then start printing the memo field (Number of copies)
            FOR lnNoCopy = 1 TO _PCOPIES
              &lcPrntTool. mprnfile FUNCTION 'V253'
            ENDFOR
            *-- reset the device to screen and clear the printer buffer to make
            *-- the printer start printing.
            SET PRINTER OFF
            SET PRINTER TO
            SET CONS &lcConsSet.
            SET MEMOW TO lnMemoWid
            _WRAP = llWrapStat
            _INDENT = lnIndent
            _RMARGIN = lnRMargin
            _LMARGIN = lnLMargin

            *-- close the temp. cursor and select the curr. work area
            USE IN &lcTmpCur.
            SELECT (lcWorkA)

          * change the contition by using the variable
          * lcPrntTool instead of GETENV() so in DOS
          * we force the program to go through this situation.
          CASE lcPrntTool="COPY"
            SET PRINTER TO (gcLoclPrt)
            FOR lnNoCopy = 1 TO _PCOPIES
              !COPY &gcOutFile. &gcLoclPrt.    && DOS Copy to port.
            ENDFOR

          CASE lcPrntTool="LIB"
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

          CASE lcPrntTool = 'TYPE'
            lcHeadSet = SET('HEADING')
            lnMemoWid = SET('MEMOW')
            lcConsSet = SET('CONS')
            llWrapStat = _WRAP
            lnRMargin = _RMARGIN
            lnLMargin = _LMARGIN
            SET MEMOW TO 255
            _WRAP = .F.
            _RMARGIN = 255
            _LMARGIN = 0
            SET HEADING OFF
            SET CONS OFF
            * Clear the Printer Driver that installed
            * to be used by Genpd
            lcTmpPrinterSet =_PDSETUP
            _PDRIVER=''
            _PDSETUP=''
            SET PRINTER TO (oAriaApplication.gcPrnt_Port)
            FOR lnNoCopy = 1 TO _PCOPIES
              TYPE (gcOutFile) TO PRINTER
            ENDFOR
            SET PRINTER TO
            SET HEADING &lcHeadSet.
            SET CONS &lcConsSet.
            SET MEMOW TO lnMemoWid
            * Restore the Printer Driver that was in use by Genpd
            _PDSETUP = lcTmpPrinterSet
            _WRAP = llWrapStat
            _RMARGIN = lnRMargin
            _LMARGIN = lnLMargin

          OTHERWISE    && Normal printing.
            FOR lnNoCopy = 1 TO _PCOPIES
              COPY FILE (gcOutFile) TO (oAriaApplication.gcPrnt_Port)
            ENDFOR

        ENDCASE
      ENDIF
      ON ERROR &lcOnErr.
      SET SAFE &lcSetSafe.

      _PDSETUP = lcPDSetup    && Restore driver.

      *--delete temprory file
      ERASE (gcOutFile)

    ELSE  && Windows .FRX
      *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [Start]
      IF lcRprtLbl <> 'L'
      *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [END]
     	  LOCAL lcExpr
    	  lnAlias = SELECT()
    	  lcx = lcOGRprtNam + ".frx"
    	  USE (lcX) ALIAS TmpRep IN 0
    	  SELECT tmprep
        LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(loOGScroll.lcOGPlatform))
        IF FOUND()
          REPLACE TAG2 WITH '',tag  WITH ''
          *! B608311,1 MMT 10/11/2007 fix bug of not able to print legal paper size reports[Start]
   	      *REPLACE EXPR WITH ''
        	lcExpr = IIF(loOGScroll.cCRorientation='L','ORIENTATION=1','ORIENTATION=0')  	
     	    lcExpr = lcExpr + CHR(13)
          lcExpr = lcExpr + IIF(ALLTRIM(UPPER(loOGScroll.cCRPapersize)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1'))
 	        REPLACE EXPR WITH lcExpr
 	        *BEYONDPRO_FRX 
 	        =FRX_Globalize()  && BEYONDPRO_FRX
    	     *BEYONDPRO_FRX         	   
     	    *! B608311,1 MMT 10/11/2007 fix bug of not able to print legal paper size reports[End]
        ENDIF
  	    USE IN TmpRep	
        SELECT(lnAlias)
    *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [Start]
    ELSE
      lnAlias = SELECT()
      lcx = lcOGRprtNam + ".LBX"
      USE (lcX) ALIAS TmpRep IN 0
      SELECT tmprep
      LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(loOGScroll.lcOGPlatform))
      IF FOUND()
        REPLACE TAG2 WITH '',tag  WITH '',EXPR WITH ''
        lcExpr = IIF(loOGScroll.cCRorientation='L','ORIENTATION=1','ORIENTATION=0')
        lcExpr = lcExpr + CHR(13)
        lcExpr = lcExpr + IIF(ALLTRIM(UPPER(loOGScroll.cCRPapersize)) == UPPER('Legal'), 'PAPERSIZE=5', IIF(loOGScroll.cCRPapersize ='A4','PAPERSIZE=9','PAPERSIZE=1'))
        REPLACE EXPR WITH lcExpr
        *BEYONDPRO_FRX
        =FRX_Globalize()  && BEYONDPRO_FRX
        *BEYONDPRO_FRX         	          
      ENDIF
      USE IN TmpRep
      SELECT(lnAlias)
      ENDIF
      *B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [END]



      *-- No Special handling.
      IF _PEJECT = 'NONE'
      ENDIF

      PRINTJOB
        IF oAriaApplication.glHeader
          IF lcRprtLbl = 'L'
            LABEL FORM (lcOGRprtNam) TO PRINTER PROMPT NOCONSOLE &lcCriteria.
          ELSE
            IF _PEJECT = 'NONE'
              REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT NOCONSOLE NOEJECT &lcCriteria.
            ELSE
              REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT NOCONSOLE &lcCriteria.
            ENDIF

          ENDIF

        ELSE

          IF lcRprtLbl = 'L'
            LABEL FORM (lcOGRprtNam) TO PRINTER PROMPT NOCONSOLE &lcCriteria.

          ELSE

            IF _PEJECT = 'NONE'
              REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT NOEJECT NOCONSOLE PLAIN &lcCriteria.
            ELSE
              REPORT FORM (lcOGRprtNam) TO PRINTER PROMPT NOCONSOLE PLAIN &lcCriteria.
            ENDIF

          ENDIF
        ENDIF
      ENDPRINTJOB
    ENDIF

  CASE oAriaApplication.gcDevice = "FILE"    && Print to File.
    loOGScroll.PrintToFile(lcOGRprtNam,lcCriteria)

	*! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [START]
    IF loOGScroll.llPrevPDF .AND. FILE(lcPDFPath)
      gfPDFViewer (loOGScroll.lcOGWinTitl,lcPDFPath)
      *B040161,1 AMH Restore the original device [Start]

      *B040214,1 AMH Restore the current device [Start]
      *oAriaApplication.gcDevice="SCREEN"
      oAriaApplication.gcDevice=lcOldDevice
      *B040214,1 AMH [End]

      *B040161,1 AMH [End]
	ENDIF
	*! N038424,1 SMM 10/17/2004 Preview the report on PDF Viewer [END]

	*B040214,1 AMH Printing the report using PDF [START]
    IF loOGScroll.llPrintPDF .AND. FILE(lcPDFPath)
      loOGScroll.mPrintPDF(lcPDFPath)
      oAriaApplication.gcDevice = lcOldDevice
	ENDIF
	*B040214,1 AMH [END]
ENDCASE

SET PRINTER TO
SET CURSOR &lcSetCur.


* return the error handler setting to the
* previous routine
ON ERROR &lcOldErrHnd.

*Restore the original default directory
SET DEFAULT TO (lcSavDfDir)
SET FULLPATH &lcFullSet.

*! B129093,1 MMT 07/21/2005 make SET CENTURY  always off in report preview and print [Start]
SET CENTURY &lcOldCentury
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
FUNCTION lfGetScope
  LPARAMETERS lcOGCriteria

  *ON ERROR do lperr with ERROR(), LINENO(), PROGRAM(), MESSAGE(), MESSAGE(1)

  LOCAL lcOGExact, lcOGScope, lcOGRec, lcOGRcCont
  lcOGExact = SET('EXACT')
  SET EXACT ON

  lcOGScope = ''
  &lcOGCriteria. = lfCondUpper(&lcOGCriteria.)  && Get Condition Upper.
  &lcOGCriteria. = IIF(LEFT(&lcOGCriteria.,1)=' ','',' ')+;
    &lcOGCriteria. + IIF(RIGHT(&lcOGCriteria.,1)=' ','',' ')

  lcCriteria = &lcOGCriteria.
  lcOGScope  = lcOGScope + IIF(&lcOGCriteria.<>STRTRAN(&lcOGCriteria.,' ALL '),'ALL ','')

  &lcOGCriteria. = STRTRAN(&lcOGCriteria.,' ALL ' , ' ')
  &lcOGCriteria. = IIF(LEFT(&lcOGCriteria.,1)=' ','',' ')+;
    &lcOGCriteria. + IIF(RIGHT(&lcOGCriteria.,1)=' ','',' ')
  lcOGScope = lcOGScope + IIF(&lcOGCriteria.<>STRTRAN(&lcOGCriteria.,' REST '),'REST ','')

  &lcOGCriteria. = STRTRAN(&lcOGCriteria. , ' REST ' , ' ')

  &lcOGCriteria. = IIF(LEFT(&lcOGCriteria.,1)=' ','',' ')+;
    &lcOGCriteria. + IIF(RIGHT(&lcOGCriteria.,1)=' ','',' ')

  IF ATC(' RECORD ',&lcOGCriteria.) > 0
    lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria.,ATC(' RECORD ',&lcOGCriteria.)+8))
    lcOGRec = lcOGRec+' '
    lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
    lcOGScope  = lcOGScope + 'RECORD ' + lcOGRcCont + ' '
    lcOGRec    = SUBSTR(&lcOGCriteria.,ATC(' RECORD ',&lcOGCriteria.),;
      (ATC(' RECORD ',&lcOGCriteria)+LEN(lcOGRcCont)+8)-ATC(' RECORD ',&lcOGCriteria.))
    &lcOGCriteria. = STRTRAN(&lcOGCriteria.,lcOGRec)
  ENDIF

  &lcOGCriteria. = IIF(LEFT(&lcOGCriteria.,1)=' ','',' ')+;
    &lcOGCriteria. + IIF(RIGHT(&lcOGCriteria.,1)=' ','',' ')

  IF ATC(' NEXT ',&lcOGCriteria.)>0
    lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria.,ATC(' NEXT ',&lcOGCriteria.)+6))
    lcOGRec = lcOGRec + ' '
    lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
    lcOGScope = lcOGScope + IIF(!EMPTY(lcOGRec),'NEXT '+lcOGRcCont,'')
    lcOGRec = SUBSTR(&lcOGCriteria.,ATC(' NEXT ',&lcOGCriteria.),(ATC(' NEXT ',&lcOGCriteria.)+LEN(lcOGRcCont)+6)-ATC(' NEXT ',&lcOGCriteria.))
    &lcOGCriteria. = STRTRAN(&lcOGCriteria.,lcOGRec)
  ENDIF
  SET EXACT &lcOGExact.
  RETURN lcOGScope
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
FUNCTION lfCondUpper
  LPARAMETERS lcOGCriteria
  LOCAL lnOGCount, llToUp, lcOGUpCrit
  llToUp     = .T.
  lcOGUpCrit = ''
  lnOGCount  = 0

  FOR lnOGCount = 1 TO LEN(lcOGCriteria)
    IF SUBSTR(lcOGCriteria,lnOGCount,1) $ ['"]
      llToUp=!llToUp
    ENDIF
    lcOGUpCrit = lcOGUpCrit + IIF(llToUp,UPPER(SUBSTR(lcOGCriteria,lnOGCount,1)),;
      SUBSTR(lcOGCriteria,lnOGCount,1))
  ENDFOR
  RETURN lcOGUpCrit
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
FUNCTION lfDispPage
  loDisplayPage.RefreshPage()
  lnPPage = _PAGENO
ENDFUNC
*-- end of lfDispPage.

FUNCTION CheckInKey
  IF !llContinuePrint
    RETURN .F.
  ENDIF

  IF INKEY() = 27
    IF gfModalGen("QRM00114B00023","ALERT") = 1
      llContinuePrint = .T.
    ELSE
      llContinuePrint = .F.
    ENDIF
  ELSE
    IF _PAGENO != lnPPage
      lfDispPage()
    ENDIF
    llContinuePrint = .T.
  ENDIF
  RETURN llContinuePrint
ENDFUNC

*!*****************************************************************************************
*! Name      : SetProceed
*! Date      : 06/22/2003 03:18:27 PM
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
PROCEDURE SetProceed
  llProceedPrn = .T.
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
FUNCTION lfPrintCritria

lcOldErrHnd = ON('ERROR')
ON ERROR
*-- Now we should print the user selection criteria ..... BEGIN
*-- If o/p the printing and user want to print the criteria.

*B040220,1 AMH Preview the user selection criteria report [Start]
*IF (TYPE("llProceedPrn") = "U") AND;
   (ALLTRIM(oAriaApplication.gcDevice) != "SCREEN") AND;
  loOGScroll.llRpPrUSel
IF (TYPE("llProceedPrn") = "U") AND loOGScroll.llRpPrUSel
*B040220,1 AMH [End]

  PRIVATE llProceedPrn
  llProceedPrn = .F.
  ON ERROR &lcOldErrHnd
 *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  PRIVATE llOldPrePDF
  llOldPrePDF = loOGScroll.llPrevPDF

  *B040214,1 AMH Save the value of llPrintPDF since it will changes when calling PrintCriteria method [Start]
  PRIVATE llOldPrintPDF
  llOldPrintPDF = loOGScroll.llPrintPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)
  loOGScroll.PrintCriteria()  && Print user selection criteria.

  *B040214,1 AMH Restore the llPrintPDF value [Start]
  loOGScroll.llPrintPDF = llOldPrintPDF
  *B040214,1 AMH [End]

  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (Start)
  loOGScroll.llPrevPDF = llOldPrePDF
  *B039444,1 SSH 07/05/2005 Fixed Previwing blank report in case of PDF  (End)

  IF !llProceedPrn
    *AMH Last Day [Start]
    SET PRINTER TO
    SET CURSOR &lcSetCur.
    ON ERROR &lcOldErrHnd.
    SET DEFAULT TO (lcSavDfDir)
    SET FULLPATH &lcFullSet.
    SET CENTURY &lcOldCentury
    SET DEVICE TO SCREEN
    *AMH Last Day [End]
    RETURN .T.
  ENDIF
  RELEASE llProceedPrn
ENDIF
*-- Now we should print the user selection criteria ..... END
*-- BEYONDPRO_FRX
PROCEDURE FRX_Globalize
  IF ((VARTYPE(oAriaApplication.oActiveLang) <> "O") OR (oAriaApplication.oActiveLang.cLang_ID = 'EN'))
    RETURN 
  ENDIF

  LOCAL lnLayoutWidth, llMirror, lcReportXML_Alias
  lcReportXML_Alias = oAriaApplication.GetReportXML_Alias()
  
  lnLayoutWidth = 0
  llMirror = oAriaApplication.oActiveLang.lIs_RTL
  
  IF llMirror  && Compute report width
    *-- loOGScroll.lcOGPlatform && Is DOS or WINDOWS
    *-- loOGScroll.cCRorientation='L' && Is Landescape
    *-- loOGScroll.cCRPapersize='LEGAL', 'A4'
    IF (UPPER(ALLTRIM(loOGScroll.lcOGPlatform)) == 'DOS')
      LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 .AND. ALLTRIM(UPPER(PLATFORM)) = ALLTRIM(UPPER(loOGScroll.lcOGPlatform)) .AND. Width > 0
      IF FOUND()
        lnLayoutWidth = Width
      ENDIF 
    ELSE  && Windows
      IF loOGScroll.cCRorientation='L'  && Landescape
        DO CASE
          CASE UPPER(ALLTRIM(loOGScroll.cCRPapersize))='LEGAL'
            lnLayoutWidth  = 136666
          CASE UPPER(ALLTRIM(loOGScroll.cCRPapersize))='A4'
            lnLayoutWidth  = 112983
          OTHERWISE 
            lnLayoutWidth  = 106666
        ENDCASE 
      ELSE  
        DO CASE
          CASE UPPER(ALLTRIM(loOGScroll.cCRPapersize))='LEGAL'
            lnLayoutWidth  = 81666
          CASE UPPER(ALLTRIM(loOGScroll.cCRPapersize))='A4'
            lnLayoutWidth  = 79333
          OTHERWISE 
            lnLayoutWidth  = 80266
        ENDCASE 
      ENDIF 
    ENDIF
  ENDIF   
  
  LOCATE  && Point first record .......................... 
  IF llMirror THEN
    LOCAL newPos  &&, lnCount
    lnCount = 0
    SCAN FOR INLIST(OBJTYPE,5,6,7,8,17)
      IF INLIST(OBJTYPE,5) AND !EMPTY(lcReportXML_Alias)
        *lnCount = lnCount + 1
        =FRX_Translate(llMirror) &&, lnCount)
      ENDIF 
      
      *-- Mirror current control .............................. BEGIN
      newPos = lnLayoutWidth - (width + hpos)
      REPLACE hpos WITH newPos
      *-- Mirror current control .............................. END

    ENDSCAN 
  
  ELSE  && Translate only 
  
    IF !EMPTY(lcReportXML_Alias)
      SCAN FOR INLIST(OBJTYPE,5)
        =FRX_Translate(llMirror)
      ENDSCAN 
    ENDIF
    
  ENDIF 
  
ENDPROC 

*-- BEYONDPRO_FRX
PROCEDURE FRX_Translate
  LPARAMETERS llMirror  &&, lnCount
  
  *IF lnCount <= 5
    *_screen.Visible = .T.
    *SET STEP ON 
  *ENDIF
  
  *IF (UPPER(ALLTRIM(expr)) == '"G"')
  *  RETURN 
  *ENDIF 
  
  LOCAL lcLangString, lcFontFace, lnFontSize
  IF !EMPTY(uniqueID) 
    IF llMirror
      IF EMPTY(picture)
        REPLACE picture WITH '"@J"'  && Text Alignment Right
      ENDIF
      REPLACE mode WITH 3 && Alignment Right To Left 
    ENDIF  

    lcFontFace = oAriaApplication.oActiveLang.ReportFontName
    lnFontSize = oAriaApplication.oActiveLang.ReportFontSize
    
    IF OBJTYPE = 8
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *lcLangString = "oAriaApplication.GetReportString('" + ALLTRIM(uniqueID) + "', " + ALLTRIM(expr) +  ", .T.)"
      lcLangString = "oAriaApplication.GetReportString('" + ALLTRIM(uniqueID) + "'," +ALLTRIM(strtr(expr,CHR(13),'')) +  ", .T.)"
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      REPLACE expr WITH lcLangString
    ELSE
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *lcLangString = "oAriaApplication.GetReportString('" + ALLTRIM(uniqueID) + "', " + ALLTRIM(expr) +  ", .F.)"      
      lcLangString = "oAriaApplication.GetReportString('" + ALLTRIM(uniqueID) + "'," + ALLTRIM(strtr(expr,CHR(13),''))+  ", .F.)"
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
      REPLACE expr WITH lcLangString, OBJTYPE WITH 8       
    ENDIF 
    *_screen.Visible = .T.
    *SET STEP ON 

    IF !EMPTY(lcFontFace)
      REPLACE FontFace WITH lcFontFace, FontSize WITH lnFontSize
    ENDIF 

 ENDIF   
ENDPROC  

