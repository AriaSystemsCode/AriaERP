*:*************************************************************
*: Program file  : Arpinvls.Prg
*: Program desc. : Invoice Form (Lawrence St.)   
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Account Receivable - [AR]
*: Developer     : Abdou Elgendy -  [ABD]
*: Date          : 12/01/2003
*: Tracking Job Number : 119040,1
*:*************************************************************
*: Calls         : FUNCTIONS  : lfEvalSegs,lfCustInfo,lfGetScale 
*:               :            : lfvOK     , lfPrnt_Rep , lfvSetup
*:               :            : lfEndGroup,
*:               -----------------------------------------------
*:               : PROCEDURE  : ..
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*:B119040.
*:*************************************************************
*:Modifications  : Change the invoice form from @ SAY to FRX.
*:*************************************************************
*:
*-- Start Declaration Variable.
STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst
DIMENSION laScales[4]
STORE ''  TO laScales

STORE .F. TO llPrint
STORE 1   TO ln1stCopy, ln2ndCopy
STORE ''  TO lc1stPort, lc2ndPort
STORE ''  TO lc1PrntNam , lc2PrntNam

*-- Evaluate NonMajor Type and variables.
= lfEvalSegs ()

*-- End Declaration Variable.

*-- Close Temporary file [Begin.]
IF USED(lcDumFile)
  USE IN (lcDumFile)
ENDIF  
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  
*-- Close Temporary file [End.]


llWareHous = (ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS'))) = 'Y')
lcWhName = lcCompName

IF llWareHous
   =gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
ENDIF

SELECT INVHDR
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
lcRpExp = "INVOICE = '' AND " + lcRpExp
COPY ALL FOR &lcRpExp TO &gcWorkDir.&lcDumFile
IF !USED(lcDumFile)
  *-- there is record in SYREPUVR file for gfTempName called lcDumFile
  *-- this will be used to carry the INVOICE HEADER TEMP. File
  = gfOpenFile('&gcWorkDir.&lcDumFile',' ','EX')
ENDIF  


SELECT (lcDumFile)
SET RELATION TO INVOICE INTO INVHDR ADDITIVE
SELECT INVHDR

IF llWareHous
   SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE
ENDIF

IF RECCOUNT(lcDumFile) = 0
  *--No records to display.
  llNoRec = .T.
  = gfModalGen('TRM00052B00000','DIALOG' )
  IF USED(lcDumFile)
    USE IN (lcDumFile)
  ENDIF
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
  RETURN
ENDIF
 
IF VARREAD() = "PBRUN"
   SET DEVICE TO SCREEN
   DO (gcScrDir+'AL\ALSELPRT.SPX')
   IF llPrint
      IF ln1stCopy > 0
        *-- Print the Frx 
         =lfPrnt_Rep('ARPINVLS','FOR ' + lcRpExp,ln1stCopy,lc1stPort)
      ENDIF

      IF ln2ndCopy > 0
        *-- Print the Frx 
         =lfPrnt_Rep('ARPINVLS','FOR ' + lcRpExp,ln2ndCopy,lc2ndPort)
      ENDIF

      SET DEVICE TO SCREEN
      llarpinv = .F.
   ELSE
    SET DEVICE TO SCREEN
    llarpinv = .F.
    RETURN
  ENDIF
ELSE
  llarpinv = .T.
ENDIF

*-- End Of Code.
*:*************************************************************
*: Name        : lfEvalSegs
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose     : Evaluate NonMajor Type and variables.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfEvalSegs

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*-- End OF lfEvalSegs.
*:*************************************************************
*: Name        : lfCustInfo
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose   : To get the customer addresses.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   :  =lfCustInfo()
*:*************************************************************
*:
FUNCTION lfCustInfo
PARAMETER lcReturn

PRIVATE lcAlias,lnCount

lcReturn = .T.
lcAlias = SELECT(0)

lcSolTName = CUSTOMER.BTName
llEndGroup = .F.
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else
  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  laShipTo[1] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF

=lfAdrShift('laShipTo')

*-- Get the Scale For the current  styles.
=lfGetScale()

SELECT (lcAlias)

RETURN ''
*- End OF lfCustInfo.
*:***********************************************************************
*: Name        : lfGetScale
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose     : To print the scale
*:***********************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   :  =lfGetScale()
*:***********************************************************************
*:
FUNCTION lfGetScale
PRIVATE lnAlias, lnInvLRecN

lnAlias = SELECT(0)

DIMENSION laScales[4,9]
laScales = ' '

llFrstTime = .T.
lnCounter  = 1

lcOldInvce = InvHdr.Invoice
lcInvLine = ''
llOpenInvL =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH',@lcInvLine,.T.)

SELECT (lcInvLine)
lnInvLRecN = RECNO()
=SEEK(InvHdr.Invoice)
SCAN REST WHILE Invoice+STR(LineNo,6) = Invhdr.invoice
   IF llFrstTime
      laScales[1,1] = SCALE
      =SEEK('S'+Scale,'Scale')
      FOR lnStrtScal = 2 TO 9
        lcSize = STR(lnStrtScal - 1,1)
        laScales[1,lnStrtScal] = Scale.sz&lcSize
      ENDFOR
      llFrstTime = .F.
   ELSE
      IF lnCounter < 4
         IF ASCAN(laScales,SCALE)=0
            lnCounter = lnCounter + 1
            laScales[lnCounter,1] = Scale
            =SEEK('S'+Scale,'Scale')
            FOR lnStrtScal = 2 TO 9
              lcSize = STR(lnStrtScal - 1,1)
              laScales[lnCounter,lnStrtScal] = Scale.sz&lcSize
            ENDFOR
         ENDIF
      ENDIF
   ENDIF
ENDSCAN

IF BETWEEN(lnInvLRecN,1,RECCOUNT())
  GOTO lnInvLRecN IN (lcInvLine)
ENDIF


IF llOpenInvL
  USE IN (lcInvLine)
ENDIF
SELECT(lnAlias)

*-- End OF lfGetScale.
*:*************************************************************
*: Name        : lfvOK
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose     : The valid function for OK button
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   :  =lfvOK()
*:*************************************************************
*:
FUNCTION lfvOK

lc1stPort = Pu1stPort
lc2ndPort = Pu2ndPort
llPrint = .T.

*-- End OF lfvOK.
*:*************************************************************
*: Name        : lfPrnt_Rep
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose     : Valid Function For Printer Driver Check Box
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*:
FUNCTION lfPrnt_Rep
PARAMETERS  lcOGRprtNam,lcCriteria,ln_No_Cops,lcSend_Prt

*-- Number of Copies.
_PCOPIES = ln_No_Cops

lcRprtLbl = IIF(TYPE('lcRprtLbl') $ 'UL','R',lcRprtLbl)
SET PRINTER TO 
SET DEVICE TO SCREEN
llPrntToFile = IIF(TYPE('llPrntToFile')='L',llPrntToFile,.F.)

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

*-- the Port

lcPrintSet  = gcPrnt_Port
gcPrnt_Port = lcSend_Prt
SET PRINTER TO

lcSetCur=SET('CURSOR')
SET CURSOR OFF
lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),"",lcOGWhile+[])+;
           ' '+lcOGFor
lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))
gcOutFile=gcWorkDir+gfTempName()+".TXT"
SET PRINTER TO (gcOutFile)
PRINTJOB
REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria
ENDPRINTJOB

SET PRINTER TO
*-- set printer to local port
SET PRINTER TO (gcPrnt_Port)
llError=.F.
lcOnErr=ON('ERROR')
ON ERROR llError=.T.
lcSetSafe=SET('SAFE')
SET SAFE OFF

lcPDSetup = _PDSETUP
_PDSETUP = ''


IF FILE(gcOutFile) AND llError=.F.
  gcLoclPrt = gcPrnt_Port
  FOR lnNoCopy = 1 TO _PCOPIES - 1
    COPY FILE (gcOutFile) TO (gcPrnt_Port)          
  ENDFOR  
  
ENDIF

ON ERROR &lcOnErr
SET SAFE &lcSetSafe    
_PDSETUP = lcPDSetup 
ERASE (gcOutFile)

SET PRINTER TO
SET CURSOR &lcSetCur
ON ERROR &lcOldErrHnd
SET DEFAULT TO &lcSavDFDir
RELEASE lcSavDFDir
SET DEVICE TO SCREEN


gcPrnt_Port = lcPrintSet

RETURN

*-- End OF lfPrnt_Rep.
*:*************************************************************
*: Name        : lfvSetup
*: Developer   : Abdou ElGendi
*: Date        : 12/02/2003
*: Purpose     : Valid Function For Printer Driver Check Box
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*:
FUNCTION lfvSetup
PARAMETER lcSendParm

IF TYPE('lcSendParm') = "U"
  RETURN
ENDIF

lcResouce = SET('RESOU')
SET RESOU ON

IF _WINDOWS
  IF TYPE('OGPlatForm')='C' AND (OGPlatForm='DOS' OR OGPlatForm='UNIX')
    lcTPrint = _PDSETUP
    _PDSETUP = "?"
    IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
      rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
      lcServ_Nam = gcServ_Nam
      lcQueu_Nam = gcQueu_Nam
    ENDIF
    lcPrnt_Port = gcPrnt_Port
    lcWinPrint = IIF(!EMPTY(_PDSETUP),_pdsetup,'None')
    lc&lcSendParm.PrntNam = lcWinPrint
    SHOW GETS
  ELSE
    =SYS(1037)
  ENDIF
ELSE
  lcTPrint = _PDSETUP
  _PDSETUP = "?"
  IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
    rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
    lcServ_Nam = gcServ_Nam
    lcQueu_Nam = gcQueu_Nam
  ENDIF
  SHOW GETS
ENDIF  
SET RESOU &lcResouce

*-- End OF lfvSetup.
*:*************************************************************
*: Name      : lfEndGroup
*: Developer   : Abdou ElGendi
*: Date        : 12/02/2003
*: Purpose   : Function to Update the End of Group flag and to update
*:             the PrtFlag field in the INVHDR file if the divice is not
*:             Screen
*:*************************************************************
*: Called from : ARPINVA.FRX
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : ''
*:*************************************************************
*
FUNCTION lfEndGroup
PARAMETER lcReturn

lcReturn = .T.

llEndGroup = .T.

IF VARREAD() = "PBRUN"
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
ENDIF    && End of IF

RETURN ''

*-- End Of lfEndGroup.
*:*************************************************************