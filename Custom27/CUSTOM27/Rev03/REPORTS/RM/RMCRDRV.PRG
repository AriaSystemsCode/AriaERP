*:***************************************************************************
*: Program File       : RMCRDRV.PRG
*: Program desc.      : CUSTOMIZED DAILY RETURN REGISTER FOR REVUE.
*: Date               : 12/31/2001
*: System             : ARIA ADVANTAGE SERIES
*: Module             : RETURN MERCHANDISE (RM)
*: Developer          : RANIA ABDEL RAZIK (RAE)			
*: Tracking Job Number: C102505
*:***************************************************************************
*: Calls              : 						
*:    Functions       : lfvCrTemF()  , lfvColData() , lfvArray()   ,   
*:                      lfvComp()    , lfvDateRng() , lfvpbOk() , 
*:                      lfNoRecord() , lfDoFRX()    , gfOpenFile() ,
*:                      gfCodDes()   ,
*:    Procedures      : lfDispRe()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example            : DO RMCRDRV
*:***************************************************************************

llRnFrmOg = IIF(TYPE('llRnFrmOg')='U',.F.,llRnFrmOg)
*-- Running as separeate file case 
IF !llRNFrmOG
  RESTORE FROM FULLPATH("")+"MFILE.MEM" ADDITIVE
  ldRpFDate = DATE()
  ldRpSDate = DATE()
  *RESTORE FROM D:\ARIA27\MFILE.MEM ADDITIVE
  llRnFrmOg = .F.
  lcOgTmpForm = ''
  IF !USED('SYURPFLT')
    =gfOpenFile(gcSysHome+'SYURPFLT','Crep_id','SH')
  ENDIF
  SELECT SYURPFLT
  IF SEEK('RMCRDRUV')
    LOCATE REST WHILE Crep_id = 'RMCRDRUV' FOR SYURPFLT.ldefault .AND. SYURPFLT.laccbyall
    IF FOUND()
      RESTORE FROM MEMO MUSRDVAR ADDITIVE
    ENDIF
  ENDIF
  IF USED('SYURPFLT')
    USE IN SYURPFLT
  ENDIF
ENDIF

*-- Running from the ARIA27 case
*ldRpFDate = IIF(EMPTY(ldRpFDate),DATE(),ldRpFDate)
*ldRpSDate = IIF(EMPTY(ldRpFDate),DATE(),ldRpFDate)
lcStTime = DATE()
=lfvCrTemF()
=lfvColData()
=lfDoFRX()

*:***************************************************************************
*: Name              : lfvCrTemF
*: Developer         : Rania Abdel Razik (RAE)
*: Date              : 12/31/2001
*: Purpose           : Function to create temporary file to collect the data 
*:                     that will be printed.
*:***************************************************************************
*: Called from       : RMCRDRV.PRG
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvCrTemF()
*:***************************************************************************

FUNCTION lfvCrTemF

*-- Create temporary file to collect the data

CREATE CURSOR (lcTempFile) (cCompID C(2)     , Ccom_name C(30) , Season C(6)      ,;
                            Crmemo C(6)      , Style C(19)     , Totqty N(4)      ,;
                            Price N(6,2)     , CtermDesc C(30) , ReasonDesc C(30) ,;
                            cRepCode  C(3)   , cRepName c(24)  , Exten N(9,2))                           
                                                     
INDEX ON cCompId + cRepCode + Crmemo TAG (lcTempFile)

*:***************************************************************************
*: Name              : lfvColData
*: Developer         : Rania Abdel Razik 
*: Date              : 12/31/2001
*: Purpose           : Function to collect the data that will be printed.           
*:***************************************************************************
*: Called from       : RMCRDRV.PRG
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvColData()
*:***************************************************************************

FUNCTION lfvColData

PRIVATE lnCount, lcCompID, lcComDir

DIMENSION laTemp[1]
IF EMPTY(laRpCompT)
  =ACOPY(laRpCompS, laTemp)
ELSE
  =ACOPY(laRpCompT, laTemp)
ENDIF

FOR lnCount=1 TO ALEN(laTemp)
  lcCompID = laTemp[lnCount]
  lcCompID = SUBSTR(lcCompID,1,2)
  M.cCompID= ALLTRIM(lcCompID)
    
  IF !USED('SYCCOMP')
    =gfOpenFile(gcSysHome+'SYCCOMP', 'Ccomp_id','SH')
  ENDIF
  SELECT SYCCOMP
  SET ORDER TO TAG Ccomp_id
  IF SEEK (lcCompID,'SYCCOMP')
    lcComDir = ALLTRIM(SYCCOMP.Ccom_ddir)
  ENDIF
 
  IF !USED('RETLINE')
    =gfOpenFile(lcComDir +'RETLINE', 'RETLINES','SH')
  ENDIF
  IF !USED('RETHDR')
    =gfOpenFile(lcComDir +'RETHDR', 'RETHDR','SH')
  ENDIF
  IF !USED('STYLE')
    =gfOpenFile(lcComDir +'STYLE', 'STYLE','SH')
  ENDIF
 
  SELECT RETLINE
  SET RELATION TO Retline.crmemo INTO Rethdr ADDITIVE
  SET RELATION TO Retline.style INTO Style ADDITIVE
  SCAN FOR BETWEEN(cRDate,ldRpFDate,ldRpSDate) .AND. RETHDR.STATUS <> "V"
    m.Ccom_name  = SYCCOMP.Ccom_name
    m.season     = STYLE.SEASON
    m.CtermDesc  = gfCodDes(RETHDR.Ctermcode,PADR('CTERMCODE',10))
    m.ReasonDesc = gfCodDes(RETLINE.Reason,PADR('REASON',10))
    m.cRepCode   = IIF(!EMPTY(RETHDR.Salesrep1),RETHDR.Salesrep1,RETHDR.Salesrep2)
    =gfOpenFile(lcComDir +'SALESREP', 'SALESREP','SH')
    m.cRepName   = IIF(SEEK(m.cRepCode,'SALESREP') , SALESREP.Name , SPACE(24))
    USE
    m.crmemo     = RETLINE.crmemo
    m.Style      = RETLINE.Style
    m.Totqty     = RETLINE.Totqty
    m.Price      = RETLINE.Price
    m.Exten      = m.Totqty * m.Price
    INSERT INTO (lcTempFile) FROM MEMVAR
  ENDSCAN
  
  IF USED('RETLINE')
    USE IN RETLINE
  ENDIF
  IF USED('RETHDR')
    USE IN RETHDR
  ENDIF
  IF USED('STYLE')
    USE IN STYLE
  ENDIF
  IF !llRNFrmOG
    IF USED('SYCCOMP')
      USE IN SYCCOMP
    ENDIF
  ENDIF
ENDFOR

*:***************************************************************************
*: Name              : lfvArray
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to initiate the target and sourece arrays
*:                     in Option Grid
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvArray()
*:***************************************************************************

FUNCTION lfvArray

DIMENSION laRpCompS[1],laRpCompT[1]
STORE '' TO laRpCompS , laRpCompT

SELECT SYCCOMP
SELECT DISTINCT Ccomp_id+' - '+Ccom_name ;
       FROM SYCCOMP INTO ARRAY laRpCompS

*:***************************************************************************
*: Name              : lfvComp
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to call the Mover Window with list ogf all 
*:                     the companies
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvComp()
*:***************************************************************************

FUNCTION lfvComp

=gfMover(@laRpCompS,@laRpCompT,'Companies',.T.,'')

*:***************************************************************************
*: Name              : lfvDateRng
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to call the Date range Window 
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvDateRng()
*:***************************************************************************

FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
ldFrom = IIF(EMPTY(ldRpFDate),DATE(),ldRpFDate)
ldTo   = IIF(EMPTY(ldRpSDate),DATE(),ldRpSDate)
lcTitle='DATE'
DO DateRng.Spx

*:***************************************************************************
*: Name              : lfvpbOk
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Valid Function of OK object
*:***************************************************************************
*: Called from       : DateRng.Spx
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvpbOk()
*:***************************************************************************

FUNCTION lfvpbOk

ldRpFDate = ldFrom
ldRpSDate = ldTo
CLEAR READ

*:***************************************************************************
*: Name              : lfNoRecord
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to check on the date in the records
*:***************************************************************************
*: Called from       : RMCRDRV , lfDoFRX()
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfNoRecord()
*:***************************************************************************

FUNCTION lfNoRecord

GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  IF llRnFrmOG
    =gfModalGen('TRM00052B00000','DIALOG' )
  ENDIF
  SET DEVICE TO SCREEN
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*:***************************************************************************
*: Name              : lfDoFRX
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to do the report
*:***************************************************************************
*: Called from       : RMCRDRV 
*:***************************************************************************
*: Calls             : lfNoRecord() , lfDispRe()
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfDoFRX()
*:***************************************************************************

FUNCTION lfDoFRX
SELECT (lcTempFile)
LOCATE
IF lfNoRecord()
  RETURN
ELSE
IF !llRnFrmOg
  gcDevice = "PRINTER"
ENDIF
  DO lfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + lcRpExp
  IF llRnFrmOg
    SAVE TO FULLPATH("")+"MFILE.MEM"
  ENDIF
ENDIF

*:***************************************************************************
*: Name              : lfDispRe
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to display the Option Grid
*:***************************************************************************
*: Called from       : RMCRDRV , lfDoFRX()
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfDispRe()
*:***************************************************************************

PROCEDURE lfDispRe

PARAMETERS  lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile

lcRprtLbl = IIF(TYPE('lcRprtLbl') $ 'UL','R',lcRprtLbl)
SET PRINTER TO 
SET DEVICE TO SCREEN

llPrntToFile = IIF(TYPE('llPrntToFile')='L',llPrntToFile,.F.)
IF llPrntToFile AND _DOS AND gcDevice <> "PRINTER"
  llPrntToFile = .F.
ENDIF

lcCriteria=IIF(TYPE('lcCriteria')='C' AND !EMPTY(lcCriteria),lcCriteria,'')

PRIVATE lcTempPr,lnOGCount,lnOGTotal,llQuiting,lcSavDfDir

llQuiting=.F.

lcOGRprtNam=IIF(TYPE('lcOGTmpForm')<>'C' OR EMPTY(lcOGTmpForm),lcOGRprtNam,gcWorkDir+lcOGTmpForm)
                                                   
lcSavDfDir = SET ('DEFAULT')
IF RAT('\',lcOGRprtNam)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX') ;
  				   .OR. FILE(gcRepHome+lcOGRprtNam+'.LBX'),;
  				   gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)
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
lcOldErrHnd = ON('ERROR')
ON ERROR
lcPrntError = IIF(_DOS AND gcDevice="PRINTER" ,[lfChckPrtStat() AND ],'')

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
IF _DOS AND !llPrntToFile
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
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1)])+;
             ' '+lcOGFOR
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

    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
    ENDIF
  ELSE
    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE PLAIN NOEJECT  &lcCriteria    
    ENDIF
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
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),"",lcOGWhile+[])+;
             ' '+lcOGFor
  DO CASE
    CASE gcDevice = "SCREEN"
      IF lcOGPlatForm='WINDOW' OR lcOGPlatForm='MAC'
        IF glHeader           
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam PREVIEW &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam PREVIEW &lcCriteria
          ENDIF
        ELSE
          IF lcRprtLbl = 'L'
            LABEL  FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ELSE
            REPORT FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ENDIF
        ENDIF  
      ELSE
        lcTempPr=_PDSETUP
        _PDRIVER=''    
        _PDSETUP=''
        lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
             ' '+lcOGFor              
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
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
          ENDIF
        ELSE
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER PLAIN NOEJECT  &lcCriteria
          ENDIF
        ENDIF            
        RELEASE WINDOW (lcTempWin)
        IF !llEndJob
          =gfEndPrn()        
        ENDIF  
        _PDSETUP =lcTempPr               
      ENDIF  

    CASE gcDevice = "PRINTER"
      lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))
      IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
        gcOutFile=gcWorkDir+gfTempName()+".TXT"
        SET PRINTER TO (gcOutFile)        
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
        PRINTJOB
          IF glHeader           
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
            ENDIF
          ELSE
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
            ENDIF
          ENDIF  
        ENDPRINTJOB
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
        
        SET PRINTER TO
        *-- set printer to local port
        SET PRINTER TO (gcPrnt_Port)
        llError=.F.
        lcOnErr=ON('ERROR')
        ON ERROR llError=.T.
        lcSetSafe=SET('SAFE')
        SET SAFE OFF
        *-- copy temprory file to local port to print it
       lcPDSetup = _PDSETUP
       _PDSETUP = ''
        
        DO WHILE SYS(13)='OFFLINE'
          IF gfModalGen("QRM00054B00015",'ALERT') = 2
            llError=.T.
            EXIT
          ENDIF
        ENDDO 
        IF FILE(gcOutFile) AND llError=.F.
          *B600951,1 Get the value of the output port in another variable
          gcLoclPrt = gcPrnt_Port
          *-- IF ENVIROMENT VARIABLE FOR PRINTING IS DEFINED
          *-- THEN PRINT BY USING THE TECH. ??? ELSE USE THE COPY COMMAND
          *-- Take care that we are setting the printer to the port in which
          *-- we want to print to before starting printing
          DO CASE
            CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
              lcTmpCur = gfTempName() && temprory cursor
              lcWorkA = SELECT()      && curr. work area
              CREATE CURS &lcTmpCur (mprnfile M) && create temp. cursor
              APPEND BLANK 
              *-- append memo with the output file
              APPEND MEMO mprnfile FROM (gcOutFile) 
              *-- clear the printer port then set the device to printer with the 
              *-- port needed to print.
              SET PRINTER TO 
              IF glPrnt_Lan                && if it is a lan option

                 lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                        +'\Q='+ALLTRIM(gcQueu_nam);
                        +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                        +IIF(glBaner,'\B='+gcBaner_H,'\N')
                 SET PRINTER TO &lcPrintSet        
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
              *-- reset the printing cursor then start printing the memo field
              *&lcPrntTool MPRNFILE FUNCTION 'V253'
              FOR lnNoCopy = 1 TO _PCOPIES
                &lcPrntTool MPRNFILE FUNCTION 'V253'
              ENDFOR
              *-- reset the device to screen and clear the printer buffer to make
              *-- the printer start printing.
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

            CASE lcPrntTool="COPY"
              SET PRINTER TO (gcLoclPRT)  
              *!COPY &gcOutFile &gcLoclPRT
              FOR lnNoCopy = 1 TO _PCOPIES
               !COPY &gcOutFile &gcLoclPRT
              ENDFOR
             
            CASE lcPrntTool="LIB"
              IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE") 
                SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
                lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPRT)
                lcOrgPrnt = PRTWIN(lcWinPrt)
                *=TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
                FOR lnNoCopy = 1 TO _PCOPIES
                  =TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
                ENDFOR
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
              lcTempPr=_PDSETUP
              _PDRIVER=''    
              _PDSETUP=''              
              SET PRINTER TO (gcPrnt_Port)
              *TYPE (gcOutFile) TO PRINTER
              FOR lnNoCopy = 1 TO _PCOPIES
                TYPE (gcOutFile) TO PRINTER
              ENDFOR
              SET PRINTER TO 
              SET HEADING &lcHeadSet
              SET CONS &lcConsSet
              SET MEMOW TO lnMemoWid
              _PDSETUP = lcTempPr
              _WRAP = llWrapStat
              _RMARGIN = lnRMargin 
              _LMARGIN = lnLMargin 
            OTHERWISE
              *COPY FILE (gcOutFile) TO (gcPrnt_Port)
              FOR lnNoCopy = 1 TO _PCOPIES
                COPY FILE (gcOutFile) TO (gcPrnt_Port)          
              ENDFOR  
          ENDCASE
    
        ENDIF
        ON ERROR &lcOnErr
        SET SAFE &lcSetSafe    
        *--delete temprory file
        _PDSETUP = lcPDSetup 
        
        ERASE (gcOutFile)
      ELSE 
        IF _PEJECT = 'NONE'
        ENDIF      
        PRINTJOB
          IF glHeader           
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE &lcCriteria 
              ENDIF
              
            ENDIF  
          ELSE
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE PLAIN &lcCriteria     
              ENDIF
              
            ENDIF  
          ENDIF  
        ENDPRINTJOB  
      ENDIF 
    CASE gcDevice = "FILE"

      IF glHeader           
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
        ENDIF
      ELSE
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
        ENDIF
      ENDIF  
  ENDCASE  
ENDIF
SET PRINTER TO
SET CURSOR &lcSetCur
ON ERROR &lcOldErrHnd
SET DEFAULT TO &lcSavDFDir
RELEASE lcSavDFDir
SET DEVICE TO SCREEN

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

&lcOGCriteria = STRTRAN(&lcOGCriteria,' ALL ' , ' ')

&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' REST '),'REST ','')

&lcOGCriteria = STRTRAN(&lcOGCriteria , ' REST ' , ' ')

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
*!**************************************************************************
*!
*!      Function: gfEndPrn
*!       
*!**************************************************************************

FUNCTION gfEndPrn

IF _DOS OR lcOGPlatForm='DOS'
  DO CASE
    CASE gcDevice="PRINTER"
      SET PRINTER TO
    CASE gcDevice="FILE"
      SET PRINTER TO    
    CASE gcDevice="SCREEN"
      lcMemWidth=SET('MEMOWIDTH')
      SET MEMOWIDTH TO 230
      SET PRINTER TO 
   IF LASTKEY()<>27
      lcOGWinTitl=IIF(TYPE('lcOGWinTitl')='C',lcOGWinTitl,'Report')
      DO GETFILE WITH gcOutFile ,lcOGWinTitl
   ENDIF      
     SET MEMOWIDTH TO lcMemWidth
     ERASE &gcOutFile.
  ENDCASE  
ENDIF  
*:***************************************************************************
*: Name              : gfOpenFile
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to open the files with a specific index
*:***************************************************************************
*: Called from       : RMCRDRV , lfvColData()
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = gfOpenFile()
*:***************************************************************************

FUNCTION gfOpenFile

PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
PRIVATE MODE,lcFileName,lcPath,llReturnVal,lcMsg,lcSetExact

PRIVATE lcMacroSub
lcMacroSub=""

lcFileName  = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,RAT('\',nfile)+1),NFILE)
lcOpenMode  = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
lcOrderTag  = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')

PRIVATE llOpen 
llOpen = .F.
IF TYPE('lcAliasNam') <> 'C'
  lcAliasNam = lcFilename
ENDIF

lcMsg = 'Opening '+NFILE+IIF(EMPTY(lcIndex),'', ' Index Tag '+lcOrderTag)+'....'
lcMsg = PROPER(lcMsg)
lnCurRow = ROW()
lnCurCol = COL()
IF 'SCREEN' $ SYS(101)
  DO CASE
    CASE _DOS OR _UNIX
      lcActWin = WONTOP()
      ACTIVATE SCREEN
      @ 24,00 SAY gfMsgStr(lcMsg) COLOR &gcMsgClr
      IF !EMPTY(lcActWin)
        ACTIVATE WINDOW (lcActWin) 
      ENDIF  
    CASE _WINDOWS OR _MAC
      SET MESSAGE TO lcMsg
  ENDCASE
ENDIF

llReturnVal = .T.
lcFPathSt   = SET('FULLPATH')
SET FULLPATH ON
IF USED(lcFilename)
  lcOpenMode = "SHARED"
  *-- if the file is used and it is from the same data directory
  IF DBF(lcFilename) == ALLTRIM(STRTRAN(UPPER(nFile), ".DBF") + ".DBF")
    *-- if forced open is desired
    IF llForceOp
      lcAliasNam  = gfTempName()
      lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &lcMacroSub   
      llOpen = .T.
      IF !EMPTY(lcOrderTag)
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF    &&IF !EMPTY(lcOrderTag)
    ELSE
      *-- if forced open is not desired
      llReturnVal = .F.
      lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
      *-- if there is no tag is desired to set order to
      IF EMPTY(lcOrderTag)
        SET ORDER TO 0 IN (lcAliasNam)
      ELSE
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF   &&IF EMPTY(lcOrderTag)
    ENDIF     &&IF llForceOp
  ELSE
    *-- if the file is used but not from the same data directory
    lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),gfTempName(),lcAliasNam)
    lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
    &lcMacroSub

    llOpen = .T.
    IF !EMPTY(lcOrderTag)
      SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
    ENDIF  &&IF !EMPTY(lcOrderTag)
  ENDIF   &&IF DBF(lcFilename) == .......
ELSE
  *-- if the file is not used
  lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
  lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
  &lcMacroSub

  llOpen = .T.
  IF !EMPTY(lcOrderTag)
    SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
  ENDIF    &&IF !EMPTY(lcOrderTag)
ENDIF    &&IF IF USED(lcFilename)
SELECT (lcAliasNam)
SET FULLPATH &lcFPathSt

*-- MAN Commented out the following block .... to the end of the function and changed the code

IF !UPPER(gcWorkDir) $ UPPER(NFILE)
  lcErrOn = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.
  IF TYPE('laFileName')<>'U'
    lcSetExact = SET('Exact')
    SET EXACT ON
    FOR lnFilePos = 1 TO ALEN(laFileName,1)
      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
        EXIT
      ENDIF
    ENDFOR
    IF lnFilePos > ALEN(laFileName,1)
      lnFilePos = 0
    ENDIF

    IF lnFilePos = 0
      IF !EMPTY(laFileName[1,1])
        DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
      ELSE
        DIME laFileName[1,4] 
      ENDIF
      laFileName[ALEN(laFileName,1),1] = lcAliasNam
      laFileName[ALEN(laFileName,1),2] = lcOrderTag
      laFileName[ALEN(laFileName,1),3] = NFILE
      laFileName[ALEN(laFileName,1),4] = lcFilename
      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
        IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
          EXIT
        ENDIF
      ENDFOR
      IF lnFileElm > ALEN(gaMnu_Fl,1)
        lnFileElm = 0
      ENDIF
      IF lnFileElm > 0 
         lnRowNo = lnFileElm 
         gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
      ELSE
         DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
         =AINS(gaMnu_Fl,1)
         gaMnu_Fl[1,1] = lcAliasNam
         gaMnu_Fl[1,2] = lcOrderTag
         gaMnu_Fl[1,3] = SELECT(0)
         gaMnu_Fl[1,4] = 1
         gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
         gaMnu_Fl[1,6] = " "
      ENDIF
    ENDIF
    SET EXACT &lcSetExact
  ENDIF
  ON ERROR &lcErrOn
ENDIF  
IF 'SCREEN' $ SYS(101)
  DO CASE
    CASE _DOS OR _UNIX
      lcActWin = WONTOP()
      ACTIVATE SCREEN
      @ 24,00 SAY gfMsgStr("") COLOR &gcMsgClr
      IF !EMPTY(lcActWin)
        ACTIVATE WINDOW (lcActWin)
      ENDIF  
      @ lnCurRow,lnCurCol SAY ""
    CASE _WINDOWS OR _MAC
      SET MESSAGE TO ""
  ENDCASE
ENDIF
RETURN llReturnVal

*:***************************************************************************
*: Name              : gfCodDes
*: Developer         : Rania Abdel Razik 
*: Date              : 01/01/2002
*: Purpose           : Function to get the code of Terms and Return Reasons
*:***************************************************************************
*: Called from       : lfvColData()
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = gfCodDes()
*:***************************************************************************

FUNCTION gfCodDes
PARAMETERS lcCodeVal, lcFldName , llChkEdit

PRIVATE lcSavOrder,lcReturnVal,lcSavSelct

llRetArray = (TYPE("lcCodeVal[1,1]") = "C")
IF !llRetArray AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !llRetArray
  lcCodeVal   = PADR(UPPER(lcCodeVal),6)
  lcFldName   = PADR(UPPER(lcFldName),10)
ENDIF  
lcReturnVal = ""
lcSavSelct  = ALIAS()      && Variable to save the currently selected file.

llUseCodes = .F.
IF !USED("CODES")
  USE (lcComDir+"Codes") IN 0
  llUseCodes = .T.
ENDIF

SELECT CODES               && Select CODES file
lcSavOrder = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order
*-- if pass array of codes.

IF llRetArray
  PRIVATE lnArrLen , lnCodeLen
  lnCodeLen = 6
  lnArrLen  = 0
  FOR lnArrLen = 1 TO ALEN(lcCodeVal,1)
    lcCodeVal[lnArrLen,1] = PADR(UPPER(lcCodeVal[lnArrLen,1]),6)
    lcCodeVal[lnArrLen,2] = PADR(UPPER(lcCodeVal[lnArrLen,2]),10)
    IF EMPTY(lcCodeVal[lnArrLen,1]) .OR. lcCodeVal[lnArrLen,1] = "*"
      lcCurFlt = FILTER()
      lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO
      *MAN Changed lcCodeVal to lcCodeVal[lnArrLen,1] Start
      IF !SEEK(SPACE(1)+LEFT(lcCodeVal[lnArrLen,1],1))
        APPEND BLANK
        REPLACE cFld_Name  WITH IIF(lcCodeVal[lnArrLen,1] = '*','ALL','N/A') ;
                cCode_No   WITH IIF(lcCodeVal[lnArrLen,1] = '*','*','')      ;
                cDiscrep   WITH IIF(lcCodeVal[lnArrLen,1] = '*','All','N/A') ;
                cRltField  WITH 'N'
      ENDIF  
      lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      SET FILTER TO &lcCurFlt.
      IF BETWEEN(lnFltRec,1,RECCOUNT())
        GO lnFltRec
      ENDIF    

    ELSE
  
      IF SEEK('N' + lcCodeVal[lnArrLen,1] + "N" + lcCodeVal[lnArrLen,2])  
        lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      ELSE
        lcCodeVal[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF  

    ENDIF

    IF !EMPTY(lcCodeVal[lnArrLen,3]) AND gfIsEdtble(ALLTRIM(lcCodeVal[lnArrLen,2]) , @lnCodeLen)
       lcCodeVal[lnArrLen,3] = PADR(lcCodeVal[lnArrLen,1],lnCodeLen) + '-' + lcCodeVal[lnArrLen,3]
    ENDIF

  ENDFOR
ELSE && Pass one code only

  IF EMPTY(lcCodeVal) .OR. lcCodeVal = "*"
    lcCurFlt = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    IF !SEEK(SPACE(1)+LEFT(lcCodeVal,1))
      APPEND BLANK
      REPLACE cFld_Name  WITH IIF(lcCodeVal = '*','ALL','N/A') ;
              cCode_No   WITH IIF(lcCodeVal = '*','*','')      ;
              cDiscrep   WITH IIF(lcCodeVal = '*','All','N/A') ;
              cRltField  WITH 'N'
    ENDIF  
    lcReturnVal = CODES.cDiscrep
    SET FILTER TO &lcCurFlt.
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
  ELSE
    IF SEEK('N' + lcCodeVal + "N" + lcFldName)  
      lcReturnVal = CODES.cDiscrep
    ELSE
      lcReturnVal = ''       && In case of this code record is deleted
    ENDIF  
  ENDIF  
  PRIVATE lnCodeLen
  lnCodeLen = 6
  IF llChkEdit AND !EMPTY(lcReturnVal) AND gfIsEdtble(ALLTRIM(lcFldName) , @lnCodeLen)
    lcReturnVal = PADR(lcCodeVal,lnCodeLen) + '-' + lcReturnVal
  ENDIF
ENDIF

SET ORDER TO &lcSavOrder
IF llUseCodes
  USE IN Codes
ENDIF
SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
RETURN lcReturnVal
