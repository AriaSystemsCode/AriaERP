************************************************************************
*: Program file  : DOR300.PRG
*: Program desc. : Print Wall-Mart Orders and Display 
*:
*:         System: Aria Apparel System
*:      Developer: NAD - Nader Anis Mitry
*: Ref           : C101901     
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Modifications      :
*:**********************************************************************

=gfOpenFile(gcDataDir+'EDICODES',gcDataDir+'EDICODES','SH')
IF EMPTY(lcEdiFile)
  =gfModalGen(.F.,.F.,.F.,.F.,'There is no EDI file selected. Cannot proceed.')
  RETURN
ENDIF
=gfOpenFile(gcDataDir+'SEGFIELD',gcDataDir+'SEGFIELD','SH')
lnInFile = FOPEN(lcEdiFile)
IF lnInFile < 0 
  =gfModalGen(.F.,.F.,.F.,.F.,'CANT OPEN THE INPUT FILE.')
  RETURN
ENDIF
lcRptFile= gcWorkDir +'WLMRPT.TXT'
lnRptFile = FCREATE(lcRptFile)
=lfAddChr13()
DECLARE xFields(30)
lcSegID = SPACE(3)
SELECT SEGFIELD
STORE '' TO lcFieldSep,lcLineSep
WAIT WINDOW 'Reading Incoming File...' NOWAIT
DO WHILE !FEOF(lnInFile)
  lcSegID = lfGetSeg()
  =SEEK(lcSegID)
  SCAN REST WHILE SEGID+STR(NSEQ,4) = lcSegID
    IF EVAL(F_Cond)
      lcData = EVAL(ALLTRIM(Data))
      =FPUTS(lnRptFile,lcData)
    ENDIF
  ENDSCAN
ENDDO
=FCLOSE(lnInFile)
=FCLOSE(lnRptFile)
=lfPrint()

*!*************************************************************
*! Name      : Chr2Date
*! Developer : Wael Aly Mohamed
*! Date      : 07/20/1999
*! Purpose   : Convert date to form MM/DD/YY
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcDate : Date
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =Chr2Date('DDMMYY')
*!*************************************************************
FUNCTION Chr2Date
PARAMETERS lcDate
IF LEN(lcDate)=6
  RETURN(SUBSTR(lcDate,3,2)+'/'+SUBSTR(lcDate,5,2)+'/'+SUBSTR(lcDate,1,2))
ELSE
  RETURN(SUBSTR(lcDate,5,2)+'/'+SUBSTR(lcDate,7,2)+'/'+SUBSTR(lcDate,1,4))
ENDIF

*!*************************************************************
*! Name      : GetCode
*! Developer : Wael Aly Mohamed
*! Date      : 07/20/1999
*! Purpose   : Return Description of VICS code
*!*************************************************************
*! Calls     : lcSegID  : Sgement ID
*!             lnFldPos : Field number
*!             lcFldVal : Code value
*!*************************************************************
*! Parameters: lcDate : Date
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =GetCode('REF',1,'DP')
*!*************************************************************
FUNCTION GetCode
PARAMETERS lcSegID,lnFldPos,lcFldVal

=SEEK(PADR(lcSegID,3)+STR(lnFldPos,2)+PADR(lcFldVal,6),'EDICODES')
RETURN(ALLTRIM(EDICODES.Desc1))

***************************************************************
**
** FUNCTION TO GET RECORD FORM THE P.O. FILE AND
** PUT THE DATA ELEMENTS INTO ARRAY
**
****************************************************************
FUNCTION lfGetSeg
PRIVATE lcSegLine,lcSegID,lnFields,lnStrtPos,lnEndPos

STORE '' TO lcSegLine,lcSegID,xFields
DO WHILE !FEOF(lnInFile)
  lcSegLine  = FGETS(lnInFile)
  lcFieldSep = IIF(LEFT(lcSegLine,3)='ISA',SUBSTR(lcSegLine,4,1),lcFieldSep)
  lcLineSep  = IIF(LEFT(lcSegLine,3)='ISA',IIF(LEN(SUBSTR(lcSegLine,;
               AT(lcFieldSep,lcSegLine,16)+1))=1,'',RIGHT(lcSegLine,1)),lcLineSep)
  lcSegLine  = IIF(EMPTY(lcLineSep),lcSegLine,SUBSTR(lcSegLine,1,LEN(lcSegLine)-LEN(lcLineSep)))
  IF EMPTY(lcSegLine)
    LOOP
  ENDIF
  lcSegID = SUBSTR(lcSegLine,1,AT(lcFieldSep,lcSegLine,1)-1)
  IF SEEK(PADR(lcSegID,3),'SEGFIELD')
    lnFields= OCCURS(lcFieldSep,lcSegLine)
    FOR lnCount = 1 TO  lnFields
      lnStrtPos = AT(lcFieldSep,lcSegLine,lnCount)+1				
      lnEndPos  = IIF(lnCount < lnFields, AT(lcFieldSep,lcSegLine,lnCount+1 ), LEN(lcSegLine)+1)
      IF lnEndPos > lnStrtPos
        xFields(lnCount) = ALLTRIM(SUBSTR(lcSegLine,lnStrtPos,lnEndPos-lnStrtPos))
      ENDIF
    ENDFOR
    EXIT
  ENDIF
ENDDO
RETURN PADR(lcSegID,3)

*!*************************************************************
*! Name      : lfAddChr13
*! Developer : Wael Aly Mohamed
*! Date      : 07/20/1999
*! Purpose   : Convert a Variable length records text file has a line 
*              separator to a variavle lenght lenght records test file with
*              a cariag return at the end of each line.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfAddChr13()
*!*************************************************************
PROCEDURE lfAddChr13
PRIVATE lnStartPos, lnEndPos, lnSegLen, lcSegLine, ;
        lcToBeCont, lnCount,  lnTempPtr,lcTempLine

*-- Create a temporary text file has the same name of the incoming EDI file
*-- with an extension TMP
lcTempFile = SUBSTR(lcEdiFile, 1, AT('.', lcEdiFile))+'TMP'
lnTempPtr  = FCREATE(lcTempFile)
lcToBeCont = ''
MLineSep   = ''
MField_Sep = ''
DO WHILE !FEOF(lnInFile)
  lcSegLine = FGETS(lnInFile,256)
  lcSegLine = STRTRAN(lcSegLine,CHR(10),'')
  lcSegLine = STRTRAN(lcSegLine,CHR(13),'')
  IF SUBSTR(lcSegLine,1,3)='ISA'
    MField_Sep = SUBSTR(lcSegLine,4,1)
    MLineSep   = SUBSTR(lcSegLine,AT('GS'+MField_Sep,lcSegLine,1)-1,1)
  ENDIF
  lnSegLen  = LEN(lcSegLine)
  STORE 1 TO lnStartPos, lnEndPos, lnCount
  IF lnSegLen > 0 
    DO WHILE .T.
      lnEndPos   = AT(MLineSep, lcSegLine , lnCount)
      lcTempLine = SUBSTR(lcSegLine, lnStartPos, lnEndPos-lnStartPos)
      IF lnEndPos = 0
        lcToBeCont = lcToBeCont + ;
                     SUBSTR(lcSegLine,lnStartPos,lnSegLen+1-lnStartPos)
        IF lnCount <> 1
          EXIT
        ENDIF
      ENDIF
      IF lnCount = 1
        lcTempLine = lcToBeCont+ lcTempLine
        lcToBeCont = ''        
      ENDIF
      lcTempLine = lcTempLine + MLineSep
      = FPUTS(lnTempPtr,LTRIM(lcTempLine))
      IF lnCount = 1 AND lnEndPos = 0
        EXIT
      ENDIF
      lnCount    = lnCount  + 1
      lnStartPos = lnEndPos + 1
    ENDDO
  ENDIF
ENDDO
=FCLOSE(lnInFile)  
=FCLOSE(lnTempPtr)
lnInFile = FOPEN(lcTempFile)

*!*************************************************************
*! Name      : lfPrint
*! Developer : Wael Aly Mohamed
*! Date      : 07/20/1999
*! Purpose   : Print Report
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfPrint()
*!*************************************************************
FUNCTION lfPrint

CHOICE = 'D'
R_WIDTH = 'N'
XREPORT = 'DOR300'
MFILE =FOPEN(gcWorkDir+'WLMRPT.TXT')
IF XREPORT='ABORT'
  RETURN
ENDIF
SET DEVICE TO PRINT
ROW=56
PAGENO = 0
XTITLE = SPACE(30)
R_TITLE= 'Wall-Mart Received Purchase Orders'
DO WHILE !FEOF(MFILE)
  XLINE = FGETS(MFILE)
  IF LEFT(XLINE,1)='+'
    XTITLE = SUBSTR(XLINE,2)
    LOOP
  ENDIF  
  IF LEFT(XLINE,3)='PO#' OR ROW = 56
    XTITLE = IIF(LEFT(XLINE,1)='+',SUBSTR(XLINE,2),XTITLE)
    PAGENO = PAGENO+1
    DO RPT_HDR WITH XREPORT,XTITLE,'N'
    ROW = 5
  ENDIF
  ROW =ROW+1
  @ ROW ,01 SAY XLINE 
ENDDO
DO ENDREPORT
SET DEVICE TO SCREEN
=FCLOSE(MFILE)
ERASE (gcWorkDir+'WLMRPT.TXT')
RETURN

*!*************************************************************
*! Name      : lfSegHdr
*! Developer : Wael Aly Mohamed
*! Date      : 07/20/1999
*! Purpose   : Print Segment Header
*!*************************************************************
*! Calls     : lcHeader : Header
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfSegHdr()
*!*************************************************************
FUNCTION lfSegHdr
PARAMETERS lcHeader
PRIVATE lnHdrlen
lcHeader = IIF(EMPTY(lcHeader),'',' '+ALLTRIM(lcHeader)+' ')
lnHdrlen = INT((80-LEN(lcHeader))/2)
RETURN(REPL('-',lnHdrlen)+lcHeader+REPL('-',80-lnHdrlen-LEN(lcHeader)))

*!*************************************************************
*! Name      : lfGetPath
*! Developer : Nader Anis
*! Date      : 06/14/2000
*! Purpose   :  
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfGetPath()
*!*************************************************************
FUNCTION lfGetPath 
lcFile= GETFILE('*.*','Select EDI Inbound File.')
lcEdiFile=IIF(EMPTY(lcFile),lcEdiFile,lcFile)
*!*************************************************************
*! Name      : lfClearRep
*! Developer : Nader Anis
*! Date      : 06/14/2000
*! Purpose   : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

IF USED('EDICODES')
  USE IN  EDICODES
ENDIF

IF USED('SEGFIELD')
  USE IN  SEGFIELD
ENDIF  