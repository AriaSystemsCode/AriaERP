*!**************************************************************************
*!
*!      Function: gfDispRep
*!         Function to Perform Reports
*!**************************************************************************
*
*E300254,1 redirect printing under windows to temprory file
*E300254,1 and then copy this temprory file to the local port selected
*E300254,1 port from the control pannel you can link the port to any
*E300254,1 network queue you want
*E300341,1 M.H 02/08/96 Add a new Parametter to identifi if its a report or label
*B601090,1 Hesham El-Sheltawi 06/03/96
*B601090,1 Changing the error handler to be set to the fox error handler
*B601090,1 so if any error occures while printing the user can retry
*B601090,1 normaly to continue printing from the point the error occures in
*B601091,1 Hesham El-Sheltawi 06/04/96
*B601091,1 Adding new logical parameter to be used in printing the report
*B601091,1 first to temprory file then copy the file to printer to ignore
*B601091,1 any errors can happen while printing because some reports we
*B601091,1 are updating some files "Ex. CHECK PRINT REPORT" so we must
*B601091,1 finish the report even there was an error with the printer
*E300399,1 Hesham El-Sheltawi 09/29/96
*E300399,1 if windows printing Clear the Printer Driver that installed
*E300399,1 to be used by Genpd in the "TYPE" Printing because it
*E300399,1 reuse the printer driver again
*B801058,4 HS 12/08/97 Add the AND to the WHILE condition if the report 
*B801058,4             exprition got a WHILE clauses in the case of Windows
*B602903,1 MAN_NY 05/14/99 Attempt to speed up reports display
*B802386,1 HDM    07/14/1999 Fix bug of removing 'ALL' if criteria icludes 'ALL' eg season='FALL'
*B602787,1 HESHAM 07/15/99 Fix Page eject problem at printing
*B802847,1 Hesham 02/10/2000
*B802847,1 Make the @ say & dos FRXs Print Multiple copies
*B604137,1 AMM 01/21/2001 fix the bug of changing the default directory with some printer drivers
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*************************************************************************************************
*FUNCTION gfDispRep

PARAMETERS  lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile


*E300341,1 M.H Begin.
lcRprtLbl = IIF(TYPE('lcRprtLbl') $ 'UL','R',lcRprtLbl)
*E300341,1 M.H End.
*B602772,1 Hesham (Start)
*B602772,1 clear setting of device and printer
SET PRINTER TO 
SET DEVICE TO SCREEN
*B602772,1 Hesham (End)

*B601091,1 Hesham El-Sheltawi (Start)
llPrntToFile = IIF(TYPE('llPrntToFile')='L',llPrntToFile,.F.)
IF llPrntToFile AND _DOS AND gcDevice <> "PRINTER"
  llPrntToFile = .F.
ENDIF
*B601091,1 Hesham El-Sheltawi (End)

lcCriteria=IIF(TYPE('lcCriteria')='C' AND !EMPTY(lcCriteria),lcCriteria,'')

*B604137,1 AMM define new variable
*PRIVATE lcTempPr,lnOGCount,lnOGTotal,llQuiting
PRIVATE lcTempPr,lnOGCount,lnOGTotal,llQuiting,lcSavDfDir
*B604137,1 AMM end
llQuiting=.F.
lcOGRprtNam=IIF(TYPE('lcOGTmpForm')<>'C' OR EMPTY(lcOGTmpForm),lcOGRprtNam,gcWorkDir+lcOGTmpForm)

*B604137,1 AMM get current default directory
lcSavDfDir = SET ('DEFAULT')
*B604137,1 AMM end
IF RAT('\',lcOGRprtNam)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *Renee
  *lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX'),gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)

  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX') ;
  				   .OR. FILE(gcRepHome+lcOGRprtNam+'.LBX'),;
  				   gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)
  if (GCMULTIINST AND FILE(gcCRepHome   +lcOGRprtNam+'.FRX')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome   +lcOGRprtNam+'.LBX')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome   +gcAct_Appl+'\'+lcOGRprtNam+'.FRX')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome   +gcAct_Appl+'\'+lcOGRprtNam+'.LBX'))  

    lcOGRprtNam= IIF(FILE(gcCRepHome   +lcOGRprtNam+'.FRX') ;
  				   .OR. FILE(gcCRepHome   +lcOGRprtNam+'.LBX'),;
  				   gcCRepHome   +lcOGRprtNam,gcCRepHome   +gcAct_Appl+'\'+lcOGRprtNam)
  
  ELSE   
    lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX') ;
  				   .OR. FILE(gcRepHome+lcOGRprtNam+'.LBX'),;
  				   gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)
  ENDIF 
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  SET FULL &lcFullSet
ENDIF  
STORE '' TO lcOGWhile,lcOGFor
**&gcRepHome.
lnDsRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())

lcOGScope=lfGETSCOPE('lcCriteria')
llWhileFrst=(ATC('WHILE ',lcCriteria)<ATC('FOR ',lcCriteria)) && AND ATC('WHILE ',lcCriteria)>0

IF ATC('WHILE ',UPPER(lcCriteria))>0
  lcOGWhile=IIF(llWhileFrst,SUBSTR(lcCriteria,1,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('WHILE ',lcCriteria)))
ENDIF  

IF ATC('FOR ',UPPER(lcCriteria))>0
  lcOGFor  =IIF(llWhileFrst,SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1,ATC(lcOGWhile,lcCriteria)-ATC('FOR ',lcCriteria)))
ENDIF  

llCanDisp = RECCOUNT()>0

IF !EMPTY(lcCriteria) OR !EMPTY(lcOGScope)
   IF EMPTY(lcOGScope)
     GO TOP
   ENDIF 
  lcLoctCond = lcOGScope+' '+lcOGFor 
  LOCATE &lcLoctCond
  llCanDisp = FOUND()
ENDIF

IF lnDsRecNo<>0
  GO lnDsRecNo
ENDIF

IF !llCanDisp
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF
*B601090,1 Hesham El-Sheltawi (Start)
*B601090,1 clear the error handler traping to the fox default
*B601090,1 so if any error occures while printing the user can
*B601090,1 select retry and continue printing from the same point
*B601090,1 the error occures
lcOldErrHnd = ON('ERROR')
ON ERROR
lcPrntError = IIF(_DOS AND gcDevice="PRINTER" ,[lfChckPrtStat() AND ],'')
*B601090,1 Hesham El-Sheltawi (End)
DO CASE
  CASE gcDevice="PRINTER" 
    IF glPrnt_Lan                && if it is a lan option

      lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                      +'\Q='+ALLTRIM(gcQueu_nam);
                      +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                      +IIF(glBaner,'\B='+gcBaner_H,'\N')

    ELSE
      lcPrintSet = gcPrnt_Port               
    ENDIF       
    IF _DOS
      SET PRINTER TO &lcPrintSet
    ELSE
      SET PRINTER TO
    ENDIF  
    
  CASE gcDevice="SCREEN" OR gcDevice="FILE"
    SET PRINTER TO &gcOutFile
ENDCASE

lcSetCur=SET('CURSOR')
SET CURSOR OFF
*B601091,1 Hesham El-Sheltawi (Start)
*B601091,1 Change the if condition to if platform is dos and we dont
*B601091,1 have to print to file
* IF _DOS 
IF _DOS AND !llPrntToFile
*B601091,1 Hesham El-Sheltawi (End)
  IF gcDevice="PRINTER" AND SYS(13) = "READY"
    IF _PEJECT $ "BOTH,BEFORE"
       lcAddv=_PADVANCE
       _PADVANCE="FORMFEED"
       EJECT
       _PADVANCE=lcAddv  
     ENDIF
  ENDIF   

  IF gcDevice='SCREEN'
    lcTempPr=_PDSETUP
    _PDRIVER=''    
    _PDSETUP=''
  ENDIF

  PRIVATE lnOGTotal,lnOGCount,lcOGSetEsc

  lnOGTotal=RECCOUNT()    
  lnOGCount=1
*IIF(inkey()<>27,.T.,gfModalGen("QRM00114B00023","ALERT") = 1)  
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1)])+;
             ' '+lcOGFOR
*             IIF(EMPTY(lcOGFOR),[ FOR  lfRpThermo(lnOGTotal,"lnOGCount","Generating Report")],lcOGFor)+;
             [ AND lfRpThermo(lnOGTotal,"lnOGCount","Generating Report")])
  _PAGENO=1             
  lcTempWin=SYS(2015)             
  lnPPage=0
  DEFINE WINDOW (lcTempWin) FROM 1,1 TO 5,30 DOUBLE COLOR SCHEME 5 SHADOW
  ACTIVATE WINDOW (lcTempWin) NOSHOW
  MOVE WINDOW (lcTempWin) CENTER
  @ 0,2 SAY 'Printing Page 1' SIZE 1,20
  @ 2,2 SAY "Press [ESC] to abort" SIZE 1,20
  ACTIVATE WINDOW (lcTempWin) TOP
  IF glHeader           
*E300341,1 M.H Begin.
    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
    ENDIF
*E300341,1 M.H End.
  ELSE
*E300341,1 M.H Begin.
    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE PLAIN NOEJECT  &lcCriteria    
    ENDIF
*E300341,1 M.H End.
  ENDIF  

  RELEASE WINDOW (lcTempWin)


  IF gcDevice='SCREEN'
    _PDSETUP=lcTempPr
  ENDIF
  
  &&lcReport      
  IF gcDevice="PRINTER" AND SYS(13) = "READY"
    IF _PEJECT $ "BOTH,AFTER"
       lcAddv=_PADVANCE
       _PADVANCE="FORMFEED"
       EJECT
       _PADVANCE=lcAddv  
     ENDIF
  ENDIF 
  IF !llEndJob
    =gfEndPrn()
  ENDIF
*** Windows
ELSE
  *IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) 
  * AND IIF(inkey()<>27,.T.,gfModalGen("QRM00114B00023","ALERT") = 1)
  *B602903,1 If empty(lcOGWhile), do not include a WHILE condition in lcCriteria
  *lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE .T.],lcOGWhile+[])+;
             ' '+lcOGFor
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),"",lcOGWhile+[])+;
             ' '+lcOGFor
  *B602903,1 end         

  DO CASE
    CASE gcDevice = "SCREEN"
      IF lcOGPlatForm='WINDOW' OR lcOGPlatForm='MAC'
        IF glHeader           
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam PREVIEW &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam PREVIEW &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ELSE
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL  FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ELSE
            REPORT FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ENDIF
*E300341,1 M.H End.        
        ENDIF  
      ELSE
        lcTempPr=_PDSETUP
        _PDRIVER=''    
        _PDSETUP=''
        
        *B801058,4 Change this line to add the AND to the WHILE condition 
        *          if the report exprition got a WHILE clauses
        *lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
        *     ' '+lcOGFor      
        lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
             ' '+lcOGFor      
        *B801058,4 Change this line [End]
        
       _PAGENO=1             
       lcTempWin=SYS(2015)             
       lnPPage=0
       DEFINE WINDOW (lcTempWin) FROM 1,1 TO 13,30  COLOR RGB(,,,192,192,192);
       FONT 'MS SANS SERIF',8 STYLE "B" FLOAT ;
			NOCLOSE ;
			NOMINIMIZE ;
			SYSTEM 

       ACTIVATE WINDOW (lcTempWin) NOSHOW
       MOVE WINDOW (lcTempWin) CENTER
       @ 3,8 SAY 'Printing Page 1' SIZE 1,20
       @ 6,8 SAY "Press [ESC] to abort" SIZE 1,20
       ACTIVATE WINDOW (lcTempWin) TOP             
       
        IF glHeader           
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ELSE
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER PLAIN NOEJECT  &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ENDIF            
        RELEASE WINDOW (lcTempWin)
        IF !llEndJob
          =gfEndPrn()        
        ENDIF  
        _PDSETUP =lcTempPr               
      ENDIF  

    CASE gcDevice = "PRINTER"
      *B601091,1 Hesham El-Sheltawi (Start)
      *B601091,1 Get the Enviroment variable called ARIAPRN contents
      *B601091,1 and if we were in DOS then we are in case of forcing
      *B601091,1 printing the report to file to ignore any errors while
      *B601091,1 updating some of the data files
*      lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),"??")
      lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))
      *B601091,1 Hesham El-Sheltawi (End)  
      *E300254,1 redirect printing under windows to temprory file
      *E300254,1 and then copy this temprory file to the local port selected
      *E300254,1 port from the control pannel you can link the port to any
      *E300254,1 network queue you want
      IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
        gcOutFile=gcWorkDir+gfTempName()+".TXT"
        SET PRINTER TO (gcOutFile)

        
        *B602787,1 Hesham (Start)
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
       *B602787,1 Hesham (End)
        PRINTJOB
          IF glHeader           
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
            ENDIF
*E300341,1 M.H End.
          ELSE
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
            ENDIF
*E300341,1 M.H End.  
          ENDIF  
        ENDPRINTJOB

        *B602787,1 Hesham (Start)
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
       *B602787,1 Hesham (End)
        
        SET PRINTER TO
        *-- set printer to local port
        SET PRINTER TO (gcPrnt_Port)
        llError=.F.
        lcOnErr=ON('ERROR')
        ON ERROR llError=.T.
        lcSetSafe=SET('SAFE')
        SET SAFE OFF
        *-- copy temprory file to local port to print it
       *B601091,1 Hesham El-Sheltawi (Start)
       *B601091,1 save the printer setup variable
       *B601091,1 and then clear it to Ignore the error
       *B601091,1 handler of the Genpd from working
       *B601091,1 and give the handler to the windows
       lcPDSetup = _PDSETUP
       _PDSETUP = ''
       *B601091,1 Hesham El-Sheltawi (End)    
        
        DO WHILE SYS(13)='OFFLINE'
          IF gfModalGen("QRM00054B00015",'ALERT') = 2
            llError=.T.
            EXIT
          ENDIF
        ENDDO 
        IF FILE(gcOutFile) AND llError=.F.
          *B600951,1 Get the value of the output port in another variable
          gcLoclPrt = gcPrnt_Port
          *B600951,1 ORABY 02/28/96
          *B600951,1 ADDING NEW WAY OF PRINTING
          *-- IF ENVIROMENT VARIABLE FOR PRINTING IS DEFINED
          *-- THEN PRINT BY USING THE TECH. ??? ELSE USE THE COPY COMMAND
          *-- Take care that we are setting the printer to the port in which
          *-- we want to print to before starting printing
          DO CASE
            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
*            CASE GETENV('ARIAPRN')='???'
            *B601091,1 Hesham El-Sheltawi (End)
              lcTmpCur = gfTempName() && temprory cursor
              lcWorkA = SELECT()      && curr. work area
              CREATE CURS &lcTmpCur (mprnfile M) && create temp. cursor
              APPEND BLANK 
              *-- append memo with the output file
              APPEND MEMO mprnfile FROM (gcOutFile) 
              *-- clear the printer port then set the device to printer with the 
              *-- port needed to print.
              SET PRINTER TO 
*              SET DEVICE TO PRINTER
              *B601091,1 Hesham El-Sheltawi (Start)
              *B601091,1 Take care of the lan printing so in case
              *B601091,1 DOS printing we will print to the spool if
              *B601091,1 needed but in windows there is no Lan option
              *B601091,1 at all
              IF glPrnt_Lan                && if it is a lan option

                 lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                        +'\Q='+ALLTRIM(gcQueu_nam);
                        +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                        +IIF(glBaner,'\B='+gcBaner_H,'\N')
                 SET PRINTER TO &lcPrintSet        
               ELSE
                 SET PRINTER TO (gcLoclPrt)
               ENDIF       
              *B601091,1 Hesham El-Sheltawi (End)
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
              *-- reset the printing cursor then start printing the memo field
*              @ 0,0 SAY ""
*              ??? MPRNFILE
              *ZiZo
*              PRINTJOB
*              _PLENGTH = 42
              *B802847,1 Hesham (Start)
              *&lcPrntTool MPRNFILE FUNCTION 'V253'
              FOR lnNoCopy = 1 TO _PCOPIES
                &lcPrntTool MPRNFILE FUNCTION 'V253'
              ENDFOR
              *B802847,1 Hesham (eND)
*              ENDPRINTJOB
              *-- reset the device to screen and clear the printer buffer to make
              *-- the printer start printing.
*              SET DEVICE TO SCREEN
              SET PRINTER OFF
              SET PRINTER TO
              SET CONS &lcConsSet
              SET MEMOW TO lnMemoWid
              _WRAP = llWrapStat
              _INDENT = lnIndent 
              _RMARGIN = lnRMargin 
              _LMARGIN = lnLMargin 
              
              *-- close the temp. cursor and select the curr. work area
              USE IN &lcTmpCur
              SELECT (lcWorkA)          

            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool="COPY"
*            CASE UPPER(GETENV('ARIAPRN'))="COPY"
              SET PRINTER TO (gcLoclPRT)  
              *B802847,1 Hesham (Start)
              *!COPY &gcOutFile &gcLoclPRT
              FOR lnNoCopy = 1 TO _PCOPIES
               !COPY &gcOutFile &gcLoclPRT
              ENDFOR
              *B802847,1 Hesham (End)
             
            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool="LIB"
*            CASE UPPER(GETENV('ARIAPRN'))="LIB"
              IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE") 
                SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
                lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPRT)
                lcOrgPrnt = PRTWIN(lcWinPrt)
                *B802847,1 Hesham (Start)
                *=TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
                FOR lnNoCopy = 1 TO _PCOPIES
                  =TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
                ENDFOR
                *B802847,1 Hesham (End)  
                = PRTWIN(lcOrgPrnt)
                RELEASE LIBRARY TXTSPOOL.FLL
              ENDIF
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
              *E300399,1 Hesham El-Sheltawi (Start)
              *E300399,1 Clear the Printer Driver that installed
              *E300399,1 to be used by Genpd
              lcTempPr=_PDSETUP
              _PDRIVER=''    
              _PDSETUP=''              
              *E300399,1 Hesham El-Sheltawi (End)              
              SET PRINTER TO (gcPrnt_Port)
              *B802847,1 Hesham (Start)
              *TYPE (gcOutFile) TO PRINTER
              FOR lnNoCopy = 1 TO _PCOPIES
                TYPE (gcOutFile) TO PRINTER
              ENDFOR
              *B802847,1 Hesham (End)
              SET PRINTER TO 
              SET HEADING &lcHeadSet
              SET CONS &lcConsSet
              SET MEMOW TO lnMemoWid
              *E300399,1 Hesham El-Sheltawi (Start)
              *E300399,1 Restore the Printer Driver that was in use
              *E300399,1 by Genpd
              _PDSETUP = lcTempPr
              *E300399,1 Hesham El-Sheltawi (End)              
              _WRAP = llWrapStat
              _RMARGIN = lnRMargin 
              _LMARGIN = lnLMargin 
            OTHERWISE
              *B802847,1 Hesham (Start)
              *COPY FILE (gcOutFile) TO (gcPrnt_Port)
              FOR lnNoCopy = 1 TO _PCOPIES
                COPY FILE (gcOutFile) TO (gcPrnt_Port)          
              ENDFOR  
              *B802847,1 Hesham (End)
          ENDCASE
    
        ENDIF
        ON ERROR &lcOnErr
        SET SAFE &lcSetSafe    
        *--delete temprory file
       *B601091,1 Hesham El-Sheltawi (Start)
       *B601091,1 Restore the printer driver that was installed
       _PDSETUP = lcPDSetup 
       *B601091,1 Hesham El-Sheltawi (End)    
        
        ERASE (gcOutFile)
      ELSE 
        *B602787,1 Hesham (Start)
        IF _PEJECT = 'NONE'
        ENDIF
       *B602787,1 Hesham (End)
      
        PRINTJOB
          IF glHeader           
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              *B602787,1 Hesham (Start)            
              *REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE &lcCriteria 
              ENDIF
             *B602787,1 Hesham (End)
              
            ENDIF
*E300341,1 M.H End.  
          ELSE
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              
              *B602787,1 Hesham (Start)            
              *REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE PLAIN &lcCriteria     
              ENDIF
             *B602787,1 Hesham (End)
              
            ENDIF
*E300341,1 M.H End.  
          ENDIF  
        ENDPRINTJOB  
      ENDIF 
    CASE gcDevice = "FILE"

      IF glHeader           
*E300341,1 M.H Begin.
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
        ENDIF
*E300341,1 M.H End.  
      ELSE
*E300341,1 M.H Begin.
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
        ENDIF
*E300341,1 M.H End.  
      ENDIF  
  ENDCASE  
ENDIF
SET PRINTER TO
SET CURSOR &lcSetCur
*B601090,1 Hesham El-Sheltawi (Start)
*B601090,1 return the error handler setting to the 
*B601090,1 previous routine
ON ERROR &lcOldErrHnd
*B601090,1 Hesham El-Sheltawi (End)
*B604137,1 AMM set the original default directory
SET DEFAULT TO &lcSavDFDir
RELEASE lcSavDFDir
SET DEVICE TO SCREEN
*B604137,1 AMM end

*!**************************************************************************
*!
*!      Function: lfCondUpper
*!         
*!**************************************************************************
*
FUNCTION lfCondUpper
PARAMETERS lcOGCriteria
llToUp=.T.
lcOGUpCrit=''
FOR lnOGCount = 1 TO LEN(lcOGCriteria)
 IF SUBSTR(lcOGCriteria,lnOGCount,1) $ ['"]
   llToUp=!llToUp 
 ENDIF
   lcOGUpCrit=lcOGUpCrit+IIF(llToUp,UPPER(SUBSTR(lcOGCriteria,lnOGCount,1)),;
                            SUBSTR(lcOGCriteria,lnOGCount,1))
ENDFOR
RETURN lcOGupCrit

*!**************************************************************************
*!
*!      Function: lfGetScope
*!         
*!**************************************************************************
*
FUNCTION lfGetScope
PARAMETERS lcOGCriteria
PRIVATE lcOGExact
lcOGExact=SET('EXACT')
SET EXACT ON
lcOGScope = ''
&lcOGCriteria=lfCondUpper(&lcOGCriteria)
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
lcCriteria = &lcOGCriteria
lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' ALL '),'ALL ','')

*HDM 802386,1 [Start]
*&lcOGCriteria = STRTRAN(&lcOGCriteria,'ALL ')
&lcOGCriteria = STRTRAN(&lcOGCriteria,' ALL ' , ' ')
*HDM 802386,1[End]

&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' REST '),'REST ','')

*HDM 802386,1[Start]
*&lcOGCriteria = STRTRAN(&lcOGCriteria,'REST ')
&lcOGCriteria = STRTRAN(&lcOGCriteria , ' REST ' , ' ')
*HDM 802386,1[End]

&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
IF ATC(' RECORD ',&lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria)+8))
  lcOGRec = lcOGRec+' '
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + 'RECORD '+lcOGRcCont+' '
  lcOGRec = SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria),(ATC(' RECORD ',&lcOGCriteria)+LEN(lcOGrcCont)+8)-ATC(' RECORD ',&lcOGCriteria))
  &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
ENDIF  
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
IF ATC(' NEXT ',&lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria)+6))
  lcOGRec = lcOGRec+' '
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + IIF(!EMPTY(lcOGRec),'NEXT '+lcOGRcCont,'')
  lcOGRec = SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria),(ATC(' NEXT ',&lcOGCriteria)+LEN(lcOGrcCont)+6)-ATC(' NEXT ',&lcOGCriteria))
  &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
ENDIF  
SET EXACT &lcOGExact
RETURN lcOGScope

*!**************************************************************************
*!
*!      Function: lfGetcope1
*!       
*!**************************************************************************
*
FUNCTION lfGetcope1
PARAMETERS lcOGCriteria
PRIVATE lcOGExact
lcOGExact=SET('EXACT')
SET EXACT ON
lcOGScope = ''
lcCriteria=lfCondUpper(lcOGCriteria)
lcOGScope = lcOGScope + IIF(lcOGCriteria<>STRTRAN(lcOGCriteria,'ALL'),'ALL ','')
lcOGScope = lcOGScope + IIF(lcOGCriteria<>STRTRAN(lcOGCriteria,'REST'),'REST ','')

IF ATC('RECORD ',lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(lcOGCriteria,ATC('RECORD ',lcOGCriteria)+7))
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + 'RECORD '+lcOGRcCont+' '
ENDIF  

IF ATC('NEXT ',lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(lcOGCriteria,ATC('NEXT ',lcOGCriteria)+5))
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + IIF(!EMPTY(lcOGRec),'NEXT '+lcOGRcCont,'')
ENDIF  

SET EXACT &lcOGExact
RETURN lcOGScope

*!**************************************************************************
*!
*!      Function: lfRpThermo
*!       
*!**************************************************************************
*
FUNCTION lfRpThermo
PARAMETERS lnRpTotal,lnRpCount,lcMessage 

WAIT SUBSTR(SPACE(LEN(lcMessage))+lcMessage+SPACE(LEN(lcMessage)),MOD(&lnRpCount,LEN(lcMessage)*2),LEN(lcMessage));
     WINDOW NOWAIT 
&lnRpCount=&lnRpCount+1      
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfQuitReport
*!       
*!**************************************************************************
*
FUNCTION lfQuitReport
PARAMETERS lnMRow,lnMcol

RETURN IIF(LASTKEY()=27,.F.,!(BETWEEN(lnMRow,2,2) AND BETWEEN(lnMCol,2,12)))

*!**************************************************************************
*!
*!      Function: lfDispPage
*!       
*!**************************************************************************
*
FUNCTION lfDispPage
PRIVATE lcOutWin

lcOutWin=WONTOP()
ACTIVATE WINDOW (lcTempWin) SAME

@ IIF(_DOS,0,3),IIF(_DOS,2,8) SAY 'Printing Page '+ALLTRIM(STR(_PAGENO)) SIZE 1,20

lnPPage=_PAGENO

IF !EMPTY(lcOutWin)
  ACTIVATE WINDOW (lcOutWin)
ENDIF



FUNCTION lfChckPrtStat
RETURN (SET('PRINTER')='ON')