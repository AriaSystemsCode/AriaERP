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
lcMessage = ''
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
  *CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [Begin]
  *IF SUBSTR(lcSegLine,185,1) = '1'
   IF SUBSTR(lcSegLine,16,2) = '10'
   *-- Record type at position 16. Detail record has a type of '10'
  *CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [End]

    =lfAddDetail()
  ENDIF
ENDDO 
=FCLOSE(lnOrdCon)

*-- Update master files
=lfUpdatMFile()

*-- Print a confirmation report
STORE '' TO lcDispDesc,lcReason
SELECT (lcOrdCon)
IF pSetup(.T.)
  DO gfDispRep WITH (gcWinAppl+'\EBORDCN.FRX')
ENDIF
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
SELECT (lcOrdCon)
GO TOP
DO WHILE !EOF()
  lcAccount = Account
  SCAN WHILE Account+Order = lcAccount
   *CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [Begin]
    Select OrdHdr
    Set Order To Tag OrdAcct
    DO CASE
      *CASE INLIST(&lcOrdCon..cOrdDisp,'APPR','AAPP')
      CASE INLIST(&lcOrdCon..cOrdDisp,'AA','AC','AD','AX' )
        *-- Approved orders
        *SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          REPLACE STATUS     WITH 'O'                 ,;
                  APPRAMT    WITH &lcOrdCon..ApprAmt  ,; 
                  APPROVAL   WITH &lcOrdCon..Approval
          UNLOCK
        ENDIF  
      
      *CASE &lcOrdCon..cOrdDisp = 'DECL'
      CASE InList(&lcOrdCon..cOrdDisp , 'DA' , 'DR')
        *-- Declined orders
        *SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          = RLOCK()
          REPLACE STATUS     WITH 'H'         ,;
                  APPRAMT    WITH  0          ,;
                  APPROVAL   WITH 'DECLINED'  ,;
                  DECL_CODE  WITH ''          ,;
                  DECL_DATE  WITH gdSysDate
          UNLOCK
        ENDIF  
        
      *CASE &lcOrdCon..cOrdDisp = 'HOLD'
      CASE &lcOrdCon..cOrdDisp = 'HC'
        *-- Hold orders
        *SELECT OrdHdr
        IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          REPLACE STATUS   WITH 'H' ,;
                  APPRAMT  WITH 0   ,;
                  APPROVAL WITH &lcOrdCon..Approval
                  
          UNLOCK
        ENDIF
    ENDCASE  
    Set Order To Tag OrdHdr
   *CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [End]
   
    DO CASE 
      *CASE INLIST(&lcOrdCon..cOrdDisp,'AAPP','APPR','CLSB','DECL','CANC')
      CASE INLIST(&lcOrdCon..cOrdDisp,'AA','AC','AD','AX','CLSB','DA','DR','CI','CR','CZ')
        lcStatus = 'F'
      *CASE INLIST(&lcOrdCon..cOrdDisp,'RFRD','HOLD','RCHK','GRSB')
      CASE INLIST(&lcOrdCon..cOrdDisp,'RFRD','HC','RCHK','GRSB')
        lcStatus = 'N'
      OTHERWISE
        lcStatus = ' '
    ENDCASE    
        
    SELECT CitTrnLn
    IF SEEK(OrdHdr.cFacCode+OrdHdr.Order+DTOS(&lcOrdCon..dDate))
      REPLACE Approval   WITH &lcOrdCon..Approval   ,;
              Status     WITH lcStatus              ,;
              cOrdDisp   WITH &lcOrdCon..cOrdDisp   ,;
              cOrdHDRes  WITH &lcOrdCon..cOrdHDRes  ,;
              cGrantName WITH &lcOrdCon..cGrantName ,;
              cComment   WITH &lcOrdCon..cComment
    ENDIF
  ENDSCAN
  WAIT 'COMPUTING AVAILABLE CREDIT...' WINDOW NOWAIT
  *-- Sum all open amount for this customer
  SELECT OrdHdr
  SET ORDER TO TAG ORDACCT
  =SEEK(lcAccount+'O')
  lnOpenAmt = 0.00
  SUM REST OPENAMT TO lnOpenAmt FOR Status='O' WHILE Account+cOrdType+Order = lcAccount+'O'
  SET ORDER TO TAG ORDHDR
    
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
WAIT CLEAR

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
*CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [Begin]
*!*	*-- Get order send date
*!*	ldDate = CTOD(SUBSTR(lcSegLine,136,2) +'/' + SUBSTR(lcSegLine,138,2) +'/' + ;
*!*	         PADL(YEAR(gdSysDate),2)+SUBSTR(lcSegLine,140,2) )
*!*	*-- Get order number as sent to CIT
*!*	lcOrder = SUBSTR(lcSegLine,70,6)
*!*	*-- Get customer code from order header file.
*!*	=SEEK('O'+lcOrder,'OrdHdr')
*!*	SELECT (lcOrdCon)
*!*	APPEND BLANK
*!*	REPLACE cFacCode   WITH OrdHdr.cFacCode          ,;
*!*	        Account    WITH OrdHdr.Account           ,;
*!*	        Order      WITH lcOrder                  ,;
*!*	        Approval   WITH SUBSTR(lcSegLine,60,10)  ,;
*!*	        cOrdDisp   WITH SUBSTR(lcSegLine,84,4)   ,;
*!*	        cOrdHDRes  WITH SUBSTR(lcSegLine,88,24)  ,;
*!*	        cGrantName WITH SUBSTR(lcSegLine,112,24) ,;
*!*	        cComment   WITH SUBSTR(lcSegLine,188,26) ,;
*!*	        dDate      WITH ldDate                   ,;
*!*	        ApprAmt    WITH VAL(SUBSTR(lcSegLine,170,7))
*-------------------------------------------------------------------        
*-- Get order send date
ldDate = CTOD(SUBSTR(lcSegLine,130,2) +'/' + SUBSTR(lcSegLine,132,2) +'/' + ;
         PADL(YEAR(gdSysDate),2)+SUBSTR(lcSegLine,134,2) )
         
*-- Get order number as sent to CIT
lcOrder = SUBSTR(lcSegLine,70,6)

*-- Get customer code from order header file.
= SEEK('O'+lcOrder,'OrdHdr')
SELECT (lcOrdCon)
APPEND BLANK
*!*	REPLACE cFacCode   WITH OrdHdr.cFacCode          ,; 
*!*	        Account    WITH OrdHdr.Account           ,;
*!*	        Order      WITH lcOrder                  ,;
*!*	        Approval   WITH SUBSTR(lcSegLine,60,10)  ,;
*!*	        cOrdDisp   WITH SUBSTR(lcSegLine,84,4) ,;
*!*	        cOrdHDRes  WITH SUBSTR(lcSegLine,88,24)  ,;
*!*	        cGrantName WITH SUBSTR(lcSegLine,112,24) ,;
*!*	        cComment   WITH SUBSTR(lcSegLine,188,26) ,;
*!*	        dDate      WITH ldDate                   ,;
*!*	        ApprAmt    WITH VAL(SUBSTR(lcSegLine,110,8))
         
REPLACE cFacCode   WITH OrdHdr.cFacCode          ,; 
        Account    WITH OrdHdr.Account           ,;
        Order      WITH lcOrder                  ,;
        Approval   WITH SUBSTR(lcSegLine,161,3)  ,;
        cOrdDisp   WITH SUBSTR(lcSegLine,118,2) ,;
        cOrdHDRes  WITH SUBSTR(lcSegLine,120,2)  ,;
        cGrantName WITH "" ,;
        cComment   WITH "" ,;
        dDate      WITH ldDate                   ,;
        ApprAmt    WITH VAL(SUBSTR(lcSegLine,110,8))         
*CP200288 Hassan Modify The Mapping To TYCO Partner 02/03/2002 [End]        
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
*!*	  CASE &lcOrdCon..cOrdDisp = 'APPR'
*!*	    lcStatus    = 'FINAL'
*!*	    lcDispDesc  = 'Automatic approval by a credit grantor'
*!*	  CASE &lcOrdCon..cOrdDisp = 'AAPP'
*!*	    lcStatus    = 'FINAL'        
*!*	    lcDispDesc  = 'Automatic approval by the system'
*!*	  CASE &lcOrdCon..cOrdDisp = 'CLSB'
*!*	    lcStatus    = 'FINAL'        
*!*	    lcDispDesc  = 'Client should re-submit thet order at a later date'
*!*	  CASE &lcOrdCon..cOrdDisp = 'DECL'
*!*	    lcReason    = 'DECLINE REASON => ' + cOrdHDRes
*!*	    lcStatus    = 'FINAL'        
*!*	    lcDispDesc  = 'Declined'
*!*	  CASE &lcOrdCon..cOrdDisp = 'CANC'
*!*	    lcStatus    = 'FINAL'        
*!*	    lcDispDesc  = 'Cancelled'
*!*	  CASE &lcOrdCon..cOrdDisp = 'RFRD'
*!*	    lcStatus    = 'NON-FINAL'
*!*	    lcDispDesc  = 'Automatic referral to a credit grantor for a manual decision'
*!*	  CASE &lcOrdCon..cOrdDisp = 'HOLD'
*!*	    lcReason    = 'HOLD REASON    => ' + cOrdHDRes
*!*	    lcStatus    = 'NON-FINAL'
*!*	    lcDispDesc  = 'The order is being held by the credit grantor'
*!*	  CASE &lcOrdCon..cOrdDisp = 'RCHK'
*!*	    lcStatus    = 'NON-FINAL'        
*!*	    lcDispDesc  = 'The order is to be rechecked before shipment'
*!*	  CASE &lcOrdCon..cOrdDisp = 'GRSB'
*!*	    lcStatus    = 'NON-FINAL'
*!*	    lcDispDesc  = 'The credit grantor will re-submit the order'
  CASE &lcOrdCon..cOrdDisp = 'AA'
    lcStatus    = 'FINAL'
    lcDispDesc  = 'Approval automatically'
  CASE &lcOrdCon..cOrdDisp = 'AC'
    lcStatus    = 'FINAL'
    lcDispDesc  = 'Approved'
  CASE &lcOrdCon..cOrdDisp = 'AD'
    lcStatus    = 'FINAL'
    lcDispDesc  = 'Approved For reduced amount'
  CASE &lcOrdCon..cOrdDisp = 'AX'
    lcStatus    = 'FINAL'
    lcDispDesc  = 'Split Risk approval'
  CASE InList(&lcOrdCon..cOrdDisp,'DA','DR' )
    lcReason    = 'DECLINE REASON => ' + cOrdHDRes
    lcStatus    = 'FINAL'        
    lcDispDesc  = 'Declined'
  CASE InList(&lcOrdCon..cOrdDisp ,'CI','CR','CZ')
    lcStatus    = 'FINAL'        
    lcDispDesc  = 'Cancelled'
  CASE &lcOrdCon..cOrdDisp = 'HC'
    lcReason    = 'HOLD REASON    => ' + cOrdHDRes
    lcStatus    = 'NON-FINAL'
    lcDispDesc  = 'Credit officer hold'
  CASE InList(&lcOrdCon..cOrdDisp ,'EX','EW')
    lcStatus    = 'FINAL'        
    lcDispDesc  = 'Expired'
ENDCASE

RETURN (lcStatus)