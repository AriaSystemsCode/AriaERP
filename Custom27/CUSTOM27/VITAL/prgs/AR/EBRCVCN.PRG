*!*************************************************************
*! Name : EBRCVCN
*! Auth : Wael Aly MOhamed
*! Date : 12/31/1999
*!*************************************************************
*! Synopsis : Read inbound order confirmation text file, Update master
*!            files and generate & display a report.
*!*************************************************************
*! Calls : 
*!         Procedures : lpConPrint, lpAddDetail
*!*************************************************************
*! Modifications : None.
*!*************************************************************
*B606721,1 SSE 01/16/2002 Fix bug of converting Complete order to open.
*!*************************************************************

lcMessage=''
*-- Get inbound order confirmation text file name
lcInFile = GETFILE('','Select inbound order confirmation file')
IF EMPTY(lcInFile)
  RETURN
ENDIF
*-- Open the text file
lnOrdCon = FOPEN(lcInFile)
IF lnOrdCon < 0
  WAIT 'Cannot open the inbound order confirmation file.' WINDOW
  RETURN
ENDIF

*-- Open Customer and order header files
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'CITTRNLN',gcDataDir+'CITTRNLN','SH')

*-- Create transaction temp. file
lcOrdCon = gfTempName()
COPY STRUCTURE TO (gcWorkDir+lcOrdCon)
=gfOpenFile(gcWorkDir+lcOrdCon,'','EX')
INDEX ON Account+Order TAG (lcOrdCon)
*-- Read details records and save data into the temp. file
DO WHILE !FEOF(lnOrdCon)
  lcSegLine = FGETS(lnOrdCon)
  IF LEFT(lcSegLine,1) = 'O'
    =lfAddDetail()
  ENDIF
ENDDO 
=FCLOSE(lnOrdCon)

*-- Update master files
=lfUpdatMFile()
lcDispDesc = ''
*-- Print a confirmation report
SELECT (lcOrdCon)
SET RELATION TO cFacCode+Order INTO CITTRNLN
SET RELATION TO 'M'+Account INTO CUSTOMER ADDITIVE
SET RELATION TO 'O'+Order INTO OrdHdr ADDITIVE
IF pSetup(.T.)
  DO gfDispRep WITH (gcWinAppl+'\EBORDCN.FRX')
ENDIF

*!*************************************************************
*! Name : lpAddDetail
*! Auth : Wael Aly MOhamed
*! Date : 12/31/1999
*!*************************************************************
*! Synopsis : Update the temp. file with data element read from the text
*!            order confirmation file.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddDetail
PRIVATE ldDate, lcOrder

*-- Get order number as sent to R&R
lcOrder = SUBSTR(lcSegLine,44,6)
*-- Get customer code from order header file.
=SEEK('O'+lcOrder,'OrdHdr')
SELECT (lcOrdCon)
APPEND BLANK
REPLACE cFacCode  WITH OrdHdr.cFacCode            ,;
        Account   WITH OrdHdr.Account             ,;
        Order     WITH lcOrder                    ,;
        Approval  WITH SUBSTR(lcSegLine,53,4)     ,;
        Status    WITH SUBSTR(lcSegLine,57,1)     ,;
        cOrdDisp  WITH SUBSTR(lcSegLine,58,1)     ,;
        ApprAmt   WITH VAL(SUBSTR(lcSegLine,59,7)),;
        cOrdHDRes WITH SUBSTR(lcSegLine,66,23)

*!*************************************************************
*! Name : lfUpdatMFile
*! Auth : Wael Aly MOhamed
*! Date : 12/31/1999
*!*************************************************************
*! Synopsis : Update master order header file with approved, Hold and 
*!            Decline information. Update customer file with Available
*!            credit.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfUpdatMFile
PRIVATE lcAccount, lnOpenAmt

WAIT 'Update master files...' WINDOW NOWAIT
SET ORDER TO TAG ORDACCT IN OrdHdr
SELECT (lcOrdCon)
GO TOP
DO WHILE !EOF()
  lcAccount = Account
  SCAN WHILE Account+Order = lcAccount
    DO CASE
      CASE Status = 'A'
        *-- Approved orders
        SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          
          *B606721,1 Check if order is complete. [Begin]
          *REPLACE STATUS   WITH 'O'                ,;
          *        APPRAMT  WITH &lcOrdCon..ApprAmt ,; 
          *        APPROVAL WITH &lcOrdCon..Approval
          REPLACE STATUS   WITH IIF(Status="C","C","O")  ,;
                  APPRAMT  WITH &lcOrdCon..ApprAmt ,; 
                  APPROVAL WITH &lcOrdCon..Approval
          *B606721,1 Check if order is complete. [End]
                  
          UNLOCK
        ENDIF  
      CASE Status = 'D'
        *-- Declined orders
        SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          = RLOCK()
          REPLACE STATUS    WITH 'H'        ,;
                  APPRAMT   WITH  0         ,;
                  APPROVAL  WITH 'DECLINED' ,;
                  DECL_CODE WITH ''         ,;
                  DECL_DATE WITH gdSysDate
          UNLOCK
        ENDIF  
      CASE Status = 'H'
        *-- Hold orders
        SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          REPLACE STATUS   WITH 'H' ,;
                  APPRAMT  WITH 0   ,;
                  APPROVAL WITH &lcOrdCon..Approval
          UNLOCK
        ENDIF
      OTHERWISE
        LOOP
    ENDCASE  
    SELECT CitTrnLn
    IF SEEK(OrdHdr.cFacCode+OrdHdr.Order)
      REPLACE Approval  WITH &lcOrdCon..Approval ,;
              ApprAmt   WITH &lcOrdCon..ApprAmt  ,;
              Status    WITH &lcOrdCon..Status   ,;
              cOrdDisp  WITH &lcOrdCon..cOrdDisp ,;
              cOrdHDRes WITH &lcOrdCon..cOrdHDRes
    ENDIF
  ENDSCAN
  WAIT 'COMPUTING AVAILABLE CREDIT...' WINDOW NOWAIT
  *-- Sum all open amount for this customer
  SELECT OrdHdr
  =SEEK(lcAccount+'O')
  lnOpenAmt = 0.00
  SUM REST OPENAMT TO lnOpenAmt FOR Status='O' WHILE Account+cOrdType+Order = lcAccount+'O'
    
  *-- Update the customer available credit
  SELECT Customer
  IF SEEK('M'+lcAccount)
    REPLACE NEXT 1 CrAvail WITH CrLimit - (TotAge+lnOpenAmt)
    IF CrAvail > CrLimit
      REPLACE NEXT 1 CrAvail WITH CrLimit
    ENDIF
  ENDIF 
  SELECT (lcOrdCon)
ENDDO
SET ORDER TO TAG OrdHdr IN OrdHdr
WAIT CLEAR


*!*************************************************************
*! Name      : lfStatus
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Get order status and Disposition Description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfStatus()
*!*************************************************************
FUNCTION lfStatus
PRIVATE lcStatus
STORE '' TO lcStatus,lcDispDesc,lcReason
DO CASE
  CASE CitTrnLn.Status = 'A'
    lcStatus = 'APPROVED'
  CASE CitTrnLn.Status = 'H'
    lcStatus = 'HOLD'
  CASE CitTrnLn.Status = 'D'
    lcStatus = 'DECLINED'
ENDCASE
lcDispDesc = CitTrnLn.cOrdHDRes
RETURN (lcStatus)