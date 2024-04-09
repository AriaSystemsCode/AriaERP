*:****************************************************************************************
*: Program file  : POBRCDRS.PRG
*: Program desc. : Print Bar Codes for receiving P/Os. (FOR CUSTOMER RSH10)
*: System        : Aria Apparel System (A27).
*: Module        : Purchase Orders (PO).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200145,1
*: Date          : 01/08/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:               :            : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#200145,1          : 
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*:B604461,1 KHM 05/07/2001 Printing a text file instead of the FRX.
*:B604495,1 ABD 05/21/2001 fix Bug that the report save wrong size & 
*:B604495,1 ABD            Duplicate the styles.
*:B604645,1 AMH 07/09/2001 fix Bug Duplicate UPC numbers.
*:****************************************************************************************

*-- Declaration variables.

*-- End Declaration variables.

*B604461,1 KHM 05/07/2001 (Begin) Creating a temporary file to hold the
*B604461,1                printed styles.
lcTmpLabe = gfTempName()
CREATE CURSOR (lcTmpLabe) (cCode C(20), cStyle C(20), sDesc C(23),;
                            SizeDesc C(5), cPrice C(5))
INDEX ON cCode+SizeDesc TAG (lcTmpLabe) OF (lcTmpLabe)
*B604461,1 KHM 05/07/2001 (End)

*- Temp File hold all styles or Upc that will print 
*-- this temp file hold 3 items Per recored.
*- open style UPC file order style+size
= gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')

*B604645,1 AMH Open styluupc with styupcn index [Start]
lcAliac = SPACE(0)
= gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYUPCN','SH',@lcAliac,.T.)
*B604645,1 AMH [End]

SELECT (lcTmpBars)
GO TOP
SCAN
  *-- Scan on all file and get the upc number from first field in the style upc file
  *-- if you didn't find generate it.
  I = 1   && number of style per line.
  FOR I = 1 TO 3
    lcCurstyle = IIF(I = 1,'',STR(I-1,1))
    IF EMPTY(CCode&lcCurstyle)
      *-- get out from the for loop.
      EXIT
    ELSE
      IF SEEK (CCode&lcCurstyle,'STYLEUPC')
        REPLACE cstyle&lcCurstyle WITH STYLEUPC.Cupcnum1
      ELSE
        *-- Add this style in style upc
        =lfAdStyUpc()
      ENDIF

      *B604461,1 KHM 05/07/2001 (Begin) Adding the records to the new
      *B604461,1                temporary file.
      INSERT INTO (lcTmpLabe) (cCode, cStyle,sDesc ,SizeDesc, cPrice);
             VALUES(&lcTmpBars..CCode&lcCurstyle,;
                    &lcTmpBars..cStyle&lcCurstyle,;
                    &lcTmpBars..sDesc&lcCurstyle,;
                    &lcTmpBars..SizeDesc&lcCurstyle,;
                    &lcTmpBars..cPrice&lcCurstyle)
      *B604461,1 KHM 05/07/2001 (End)

    ENDIF
  ENDFOR
ENDSCAN

*B604645,1 AMH close styluupc with styupcn index [Start]
IF USED(lcAliac)
  USE IN (lcAliac)
ENDIF
*B604645,1 AMH [End]

*B604461,1 KHM 05/07/2001 (Begin) Check if the user is printing to the printer
*B604461,1                then create a text file and print it.
IF VARREAD() = "PBRUN"
  SELECT (lcTmpLabe)
  =lfSendTxt()
  llPrntFrm = .F.
ENDIF
*B604461,1 KHM 05/07/2001 (End)

*-- END Option Program.
*:*************************************************************
*: Name      : lfAdStyUpc
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/08/2001
*: Purpose   : add function that add upc number in style upc file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfAdStyUpc()
*:*************************************************************
*
FUNCTION lfAdStyUpc

SELECT STYLEUPC
APPEND BLANK
        
*604495 ABD - Add the Variable that hold the style Postion in 
*604495 ABD - the temp file. [Begin]
*REPLACE STYLE     WITH LEFT (&lcTmpBars..CCode,19)        ,;
*        SIZE      WITH ALLTRIM(RIGHT(&lcTmpBars..CCode&lcCurstyle,2))
REPLACE STYLE     WITH LEFT (&lcTmpBars..CCode&lcCurstyle,19)        ,;
        SIZE      WITH ALLTRIM(RIGHT(&lcTmpBars..CCode&lcCurstyle,1))
*604495 ABD - [End]


*-- the UPC must be equal 5 digits.
*B604645,1 AMH Open styluupc with styupcn index [Start]
*lcUpcNum = SYS(2007,STYLE+SIZE)
*lnUpcNum = LEN(lcUpcNum)

*IF lnUpcNum <  6
*  FOR X = lnUpcNum To 5
*   lcUpcNum = lcUpcNum +'0'
*  ENDFOR 
*ENDIF
lcUpcNum = gfsequence('cUpcElNo')
DO WHILE .T.
  IF SEEK(lcUpcNum,lcAliac)
    lcUpcNum = gfsequence('cUpcElNo')
  ELSE
    EXIT
  ENDIF
ENDDO
*B604645,1 AMH [End]

REPLACE cupcnum1  WITH lcUpcNum

*-- add the new UPC number to the printable file.
SELECT (lcTmpBars)
REPLACE cstyle&lcCurstyle WITH STYLEUPC.Cupcnum1

*-- End Of lfAdStyUpc.
*:*************************************************************


*:*************************************************************
*: Name      : lfSendTxt
*: Developer : Khalid Mohi El-Din
*: Date      : 05/07/2001
*: Purpose   : Create and Print the text file.
*:*************************************************************
*: Example   : =lfSendTxt()
*:*************************************************************
*B604461,1 KHM 05/07/2001 Function to create the text file and print it.
*:*************************************************************
FUNCTION lfSendTxt

*-- Get the selected port
lcPortName = ''
DO CASE
    CASE lcRpPort = "L"
      lcPortName = "LPT1"
    CASE lcRpPort = "P"
      lcPortName = "LPT2"
    CASE lcRpPort = "C"
      lcPortName = "COM1"
    CASE lcRpPort = "M"
      lcPortName = "COM2"
ENDCASE

*-- Create text file
lcTxtFile = gcWorkDir+"UPCLBL.TXT"
llOpen = .F.
IF !USED('SycUpcLb')
  = gfOpenFile(gcSysHome+'SycUpcLb','Upc_lbl','SH')
  llOpen = .T.
ENDIF


*-- Printing the label's lines information
SELECT (lcTmpLabe)
LOCATE
DO WHILE !EOF()

  *-- The information to be sent to the label's lines for each style/size
  lcStyle    = cCode             && Style code
  lcStySize  = cCode + SizeDesc
  lcUpcCode  = ALLTRIM(cStyle)
  lcSizeDesc = ALLTRIM(SizeDesc)
  lcRetPrice = cPrice
  lcStyDesc  = sDesc
  lcCompany  = ALLTRIM(gcCom_Name)
  lnQuantity = 0  

  *-- Create the text file
  lnFileHnd = FCREATE("&lcTxtFile",0)
  *-- Print the label's header information.
  SELECT SycUpcLb
  SEEK 'NIK'+'H'
  SCAN WHILE cVer+cEdiType+STR(NSEQ,4) = 'NIK'+"H"
    STORE Data TO lcData
    =FPUTS(lnFileHnd,&lcData)
  ENDSCAN	

  =FPUTS(lnFileHnd,'   ')

  *-- No. of lables for each style/size
  SELECT (lcTmpLabe)
  SCAN REST WHILE cCode+SizeDesc = lcStySize
    lnQuantity = lnQuantity + 1
  ENDSCAN 

  *-- Print the label's line information.
  SELECT SycUpcLb
  SEEK 'NIK'+'L'
  SCAN WHILE cVer+cEdiType+STR(NSEQ,4) = 'NIK'+'L'
    STORE Data TO lcData
    =FPUTS(lnFileHnd,&lcData)
  ENDSCAN	
  *=FPUTS(lnFileHnd,'   ')

  
  *-- Close the text file and print it.
  =FCLOSE(lnFileHnd)
  ! TYPE &lcTxtFile> &lcPortName

  *lnWaitSec = MAX(lnQuantity,2)
  lnWaitSec = MAX(lnQuantity*(3/2),3)
  WAIT WINDOW TIMEOUT lnWaitSec

  SELECT (lcTmpLabe)
ENDDO

*-- Close the handler of the text file
SET DEVICE TO SCREEN

IF llOpen AND USED('SycUpcLb')
  USE IN SycUpcLb
ENDIF
SELECT (lcTmpBars)