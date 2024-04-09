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
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[T20100825.0016]
*! C201294,2 MMT 01/18/2011 Fix error appear while receiving file at SHA13[T20100825.0016]
*!*************************************************************

lcMessage=''

*-- Get inbound order confirmation text file name
lcInFile = GETFILE('','Select inbound order confirmation file')
IF EMPTY(lcInFile)
  =lfCleanPrg()
  RETURN
ENDIF
*-- Open the text file
lnOrdCon = FOPEN(lcInFile)
IF lnOrdCon < 0
  WAIT 'Cannot open the inbound order confirmation file.' WINDOW
  =lfCleanPrg()
  RETURN
ENDIF

*-- Open Customer and order header files
*!*	=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
*!*	=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
*!*	=gfOpenFile(gcDataDir+'CITTRNLN',gcDataDir+'CITTRNLN','SH')
=gfOpenTABLE(OARIAAPPLICATION.DATADIR +'CUSTOMER',OARIAAPPLICATION.DATADIR+'CUSTOMER','SH')
=gfOpenTAble(OARIAAPPLICATION.DATADIR+'ORDHDR',OARIAAPPLICATION.DATADIR+'ORDHDR','SH')
=gfOpenTABLE(OARIAAPPLICATION.DATADIR+'CITTRNLN',OARIAAPPLICATION.DATADIR+'CITTRNLN','SH')

*-- Create transaction temp. file
SELECT CITTRNLN
lcOrdCon = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+lcOrdCon) (cFacCode C(6), BatchNo C(2), dDate D,;
              Account C(5), Order C(6), Status C(1), ApprAmt N(14,2), Approval C(10),;
              cOrdDisp C(4), cOrdHDRes C(30), cGrantName C(30), cComment C(30),;
              FactAcc C(6), btName C(30)) 
*COPY STRUCTURE TO (oAriaApplication.WorkDir+lcOrdCon)
*=gfOpenTABLE(oAriaApplication.WorkDir+lcOrdCon,'','EX')
SELECT (lcOrdCon)
INDEX ON Account+Order TAG (lcOrdCon)

*-- Read details records and save data into the temp. file
DO WHILE !FEOF(lnOrdCon)
  lcSegLine = FGETS(lnOrdCon)
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]
  *IF SUBSTR(lcSegLine,185,1) = '1'
  IF SUBSTR(lcSegLine,16,2) = '10'
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]
    *-- Record type at position 185. Detail record has a type of '1'
    =lfAddDetail()
  ENDIF
ENDDO 
=FCLOSE(lnOrdCon)

*-- Update master files
=lfUpdatMFile()

*-- Print a confirmation report
STORE '' TO lcDispDesc,lcReason

=lfPrint()

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

*khm
SELECT OrdHdr
gfSetOrder('ORDACCT')
*khm

WAIT 'Update master files...' WINDOW NOWAIT
SELECT (lcOrdCon)
GO TOP
DO WHILE !EOF()
  lcAccount = Account
  SCAN REST WHILE Account+Order = lcAccount
    DO CASE
      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]
      *CASE INLIST(&lcOrdCon..cOrdDisp,'APPR','AAPP')
      CASE INLIST(&lcOrdCon..cOrdDisp,'AA','AC','AD','AX')
      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]
        *-- Approved orders
        SELECT OrdHdr
        *khm
        *IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
        *khm
          =RLOCK()
          *khm
          *REPLACE STATUS     WITH 'O'                 ,;
                  APPRAMT    WITH &lcOrdCon..ApprAmt  ,; 
                  APPROVAL   WITH &lcOrdCon..Approval
          gfREPLACE ([STATUS     WITH 'O'             ,]+;
                     [APPRAMT    WITH &lcOrdCon..ApprAmt,]+; 
                     [APPROVAL   WITH &lcOrdCon..Approval])
          *khm
          UNLOCK
        ENDIF  
      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]  
      *CASE &lcOrdCon..cOrdDisp = 'DECL'
      CASE INLIST(&lcOrdCon..cOrdDisp ,'DA','DR','CI','CR','CZ')
      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[ENd]      
        *-- Declined orders
        SELECT OrdHdr
        *khm
        *IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
        *khm
          = RLOCK()
          *khm
          *REPLACE STATUS     WITH 'H'         ,;
                  APPRAMT    WITH  0          ,;
                  APPROVAL   WITH 'DECLINED'  ,;
                  DECL_CODE  WITH ''          ,;
                  DECL_DATE  WITH gdSysDate
          *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]  
          *! C201294,2 MMT 01/18/2011 Fix error appear while receiving file at SHA13[Start]
          *IF CASE INLIST(&lcOrdCon..cOrdDisp ,'DA','DR')        
          IF INLIST(&lcOrdCon..cOrdDisp ,'DA','DR')        
          *! C201294,2 MMT 01/18/2011 Fix error appear while receiving file at SHA13[End]
	      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]  
            gfREPLACE ([STATUS     WITH 'H'       ,]+;
                     [APPRAMT    WITH  0        ,]+;
                     [APPROVAL   WITH 'DECLINED',]+;
                     [DECL_CODE  WITH ''        ,]+;
                     [DECL_DATE  WITH oAriaApplication.SystemDate])
	      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]  
          ELSE
            gfREPLACE ([STATUS     WITH 'H'       ,]+;
                     [APPRAMT    WITH  0        ,]+;
                     [DECL_CODE  WITH ''        ,]+;
                     [DECL_DATE  WITH oAriaApplication.SystemDate])
          ENDIF
	      *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]  

          *khm
          UNLOCK
        ENDIF  
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]          
      *CASE &lcOrdCon..cOrdDisp = 'HOLD'
      CASE &lcOrdCon..cOrdDisp = 'HC' OR EMPTY(&lcOrdCon..cOrdDisp)      
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[ENd]        
        *-- Hold orders
        SELECT OrdHdr
        *khm
        *IF SEEK(lcAccount+'O'+&lcOrdCon..Order)
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
        *khm
          =RLOCK()
          *khm
          *REPLACE STATUS   WITH 'H' ,;
                  APPRAMT  WITH 0   ,;
                  APPROVAL WITH &lcOrdCon..Approval
          gfREPLACE ([STATUS   WITH 'H' ,]+;
                     [APPRAMT  WITH 0   ,]+;
                     [APPROVAL WITH &lcOrdCon..Approval])
                  
          *khm
          UNLOCK
        ENDIF
    ENDCASE  
    DO CASE 
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]              
      *CASE INLIST(&lcOrdCon..cOrdDisp,'APPR','AAPP','CLSB','DECL','CANC')
      CASE INLIST(&lcOrdCon..cOrdDisp,'AA','AC','AD','AX','DA','DR','CI','CR','CZ')      
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]                
        lcStatus = 'F'
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                  
      *CASE INLIST(&lcOrdCon..cOrdDisp,'RFRD','HOLD','RCHK','GRSB')
      CASE &lcOrdCon..cOrdDisp = 'HC' OR EMPTY(&lcOrdCon..cOrdDisp)      
  	*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]                
        lcStatus = 'N'
      OTHERWISE
        lcStatus = ' '
    ENDCASE    
        
    SELECT CitTrnLn
    *khm
    *IF SEEK(OrdHdr.cFacCode+OrdHdr.Order+DTOS(&lcOrdCon..dDate))
    IF gfSEEK(OrdHdr.cFacCode+OrdHdr.Order+DTOS(&lcOrdCon..dDate))
    *khm  
      *khm
      *REPLACE Approval   WITH &lcOrdCon..Approval   ,;
              Status     WITH lcStatus              ,;
              cOrdDisp   WITH &lcOrdCon..cOrdDisp   ,;
              cOrdHDRes  WITH &lcOrdCon..cOrdHDRes  ,;
              cGrantName WITH &lcOrdCon..cGrantName ,;
              cComment   WITH &lcOrdCon..cComment
      gfREPLACE ([Approval   WITH &lcOrdCon..Approval   ,]+;
                 [Status     WITH lcStatus              ,]+;
                 [cOrdDisp   WITH &lcOrdCon..cOrdDisp   ,]+;
                 [cOrdHDRes  WITH &lcOrdCon..cOrdHDRes  ,]+;
                 [cGrantName WITH &lcOrdCon..cGrantName ,]+;
                 [cComment   WITH &lcOrdCon..cComment   ,]+;
                 [ApprAmt    WITH &lcOrdCon..ApprAmt])

    ENDIF
  ENDSCAN
  WAIT 'COMPUTING AVAILABLE CREDIT...' WINDOW NOWAIT
  *-- Sum all open amount for this customer
  SELECT OrdHdr
  *khm
  *SET ORDER TO TAG ORDACCT
  *=SEEK(lcAccount+'O')
  =gfSEEK(lcAccount+'O')
  *khm
  lnOpenAmt = 0.00
  SUM REST OPENAMT TO lnOpenAmt FOR Status='O' WHILE Account+cOrdType+Order = lcAccount+'O'
  *khm
  *SET ORDER TO TAG ORDHDR
  *khm
    
  *-- Update the customer available credit
  SELECT Customer
  *khm
  *IF SEEK('M'+lcAccount)
  IF gfSEEK('M'+lcAccount)
  *khm
    *khm
    *REPLACE NEXT 1 CrAvail WITH CrLimit - (TotAge+lnOpenAmt)
    gfREPLACE ([NEXT 1 CrAvail WITH CrLimit - (TotAge+lnOpenAmt)])
    
    IF CrAvail > CrLimit
      *khm
      *REPLACE NEXT 1 CrAvail WITH CrLimit
      gfREPLACE ([NEXT 1 CrAvail WITH CrLimit])
      *khm
      
    ENDIF
  ENDIF 
  SELECT (lcOrdCon)
ENDDO
*khm
SELECT OrdHdr
gfSetOrder('ORDHDR')
*khm

*khm
*-- To physically update the OrdHdr, Customer and CitTrnLn
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   WAIT WINDOW 'Saving process has not been completed successfully.'
   RETURN .F.
ENDIF
LOCAL lnUpdated, lnAryLen
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
    lnUpdated=lnCounter
    EXIT
  ENDIF
NEXT

IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  WAIT WINDOW 'Saving process has not been completed successfully.'
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  RETURN .T.
ENDIF
*khm
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

*-- Get order send date
ldDate = CTOD(SUBSTR(lcSegLine,136,2) +'/' + SUBSTR(lcSegLine,138,2) +'/' + ;
         PADL(YEAR(oAriaApplication.SystemDate),2)+SUBSTR(lcSegLine,140,2) )
*-- Get order number as sent to CIT
lcOrder = SUBSTR(lcSegLine,70,6)
*-- Get customer code from order header file.
*khm
*=SEEK('O'+lcOrder,'OrdHdr')
=gfSEEK('O'+lcOrder,'OrdHdr')
=gfSeek('M'+OrdHdr.Account,'Customer')
*khm
SELECT (lcOrdCon)
APPEND BLANK
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                  
*REPLACE cFacCode   WITH OrdHdr.cFacCode          ,;
        Account    WITH OrdHdr.Account           ,;
        Order      WITH lcOrder                  ,;
        Approval   WITH SUBSTR(lcSegLine,60,10)  ,;
        cOrdDisp   WITH SUBSTR(lcSegLine,84,4)   ,;
        cOrdHDRes  WITH SUBSTR(lcSegLine,88,24)  ,;
        cGrantName WITH SUBSTR(lcSegLine,112,24) ,;
        cComment   WITH SUBSTR(lcSegLine,188,26) ,;
        dDate      WITH ldDate                   ,;
        ApprAmt    WITH VAL(SUBSTR(lcSegLine,170,7)),;
        FactAcc    WITH Customer.cFacCode,;
        btName     WITH Customer.btName
REPLACE cFacCode   WITH OrdHdr.cFacCode          ,;
        Account    WITH OrdHdr.Account           ,;
        Order      WITH lcOrder                  ,;
        Approval   WITH SUBSTR(lcSegLine,92,10)  ,;
        cOrdDisp   WITH SUBSTR(lcSegLine,118,2)   ,;
        cOrdHDRes  WITH lfGetReason(SUBSTR(lcSegLine,120,2)),;
        cComment   WITH lfGetAction(SUBSTR(lcSegLine,118,2)) ,;
        dDate      WITH ldDate                   ,;
        ApprAmt    WITH VAL(SUBSTR(lcSegLine,110,8)),;
        FactAcc    WITH Customer.cFacCode,;
        btName     WITH Customer.btName
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]                          
        
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
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                  
  *CASE &lcOrdCon..cOrdDisp = 'APPR'
  CASE INLIST(&lcOrdCon..cOrdDisp , 'AA','AC')
    lcDispDesc  = 'Approved'
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[End]                    
    lcStatus    = 'FINAL'
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                      
    *lcDispDesc  = 'Automatic approval by a credit grantor'
  *CASE &lcOrdCon..cOrdDisp = 'AAPP'
  CASE &lcOrdCon..cOrdDisp = 'AD'
    lcDispDesc  = 'Approved for reduced amount'
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[END]                    
    lcStatus    = 'FINAL'        
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                          
  *  lcDispDesc  = 'Automatic approval by the system'
  * CASE &lcOrdCon..cOrdDisp = 'CLSB'
   CASE &lcOrdCon..cOrdDisp = 'AX'    
     lcDispDesc  = 'Split Risk approved'
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[END]                        
    lcStatus    = 'FINAL'        
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                        
  *  lcDispDesc  = 'Client should re-submit thet order at a later date'
  *CASE &lcOrdCon..cOrdDisp = 'DECL'
  *lcReason    = 'DECLINE REASON => ' + cOrdHDRes
  CASE INLIST(&lcOrdCon..cOrdDisp , 'DA','DR')        
    lcReason    = 'DECLINE REASON => ' +  lfGetReason(cOrdHDRes)
  *! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[END]                          
    lcStatus    = 'FINAL'        
    lcDispDesc  = 'Declined'
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]                          
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
 CASE &lcOrdCon..cOrdDisp = 'HC'
    lcDispDesc  = 'Credit officer hold' 
	lcStatus    = 'NON-FINAL'    
    lcReason    = 'HOLD REASON    => ' + lfGetReason(cOrdHDRes)
 CASE &lcOrdCon..cOrdDisp = 'CI'       
     lcDispDesc  = 'Cancelled' 
	 lcStatus    = 'FINAL'
 CASE &lcOrdCon..cOrdDisp = 'CR'       
     lcDispDesc  = "Cancelled by credit officer per client's request" 
 	lcStatus    = 'FINAL'     
 CASE &lcOrdCon..cOrdDisp = 'CZ'            
    lcDispDesc  = "Cancelled by client via on-line system" 
    lcStatus    = 'FINAL'
 CASE EMPTY(&lcOrdCon..cOrdDisp) 
  lcDispDesc  = "Pending" 
  lcStatus    = 'NON-FINAL'    
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[END]                          
ENDCASE          
RETURN (lcStatus)        

*!*************************************************************
*! Name      : lfPrint
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 08/26/2006
*! Purpose   : To print the report
*!*************************************************************
FUNCTION lfPrint
PRIVATE loOGScroll

*-- Create a Dummy Object of the Option Grid...
LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
lnDataSess = SET("Datasession")
lcDevice   = oAriaApplication.gcDevice
oAriaApplication.gcDevice = 'PRINTER'

lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = 	''
loOgScroll.lcOGLastForm  = 'EBORDCN'
loOGScroll.llPrintPDF = .F.
loogScroll.cCROrientation = 'L'
LoOGScroll.llCrystal = .T.

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''
SELECT (lcOrdCon)
LOCATE

*SET RELATION TO 'M'+Account INTO Customer

DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcOrdCon + ".DBF"
SELECT(lcOrdCon)
USE 
=gfDispRe('EBORDCN')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.
LOogsCROLL =  .NULL.

=gfCloseTable('CUSTOMER')
=gfCloseTable('CITTRNLN')
=gfCloseTable('ORDHDR')
=lfCleanPrg()

*!*************************************************************
*! Name      : FileExist
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 08/26/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION FileExist
LPARAMETERS cFileToCheckFor

RETURN loOGScroll.FileExist(cFileToCheckFor)

*!*************************************************************
*! Name      : lfcleanPrg
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 08/26/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
FUNCTION lfCleanPrg

lcBaseWind='AAREBRCVCN'
oariaapplication.SETHlpfl(oariaapplication.activemoduleid)

SELECT syuStatc
IF SEEK ('WIN'+ALLTRIM(lcBaseWind)+oariaapplication.User_ID+oariaapplication.Station) 
  *** Save the name of module from which the window was activated
  SCATTER MEMVAR MEMO BLANK
  GATHER  MEMVAR MEMO
  DELETE 
  =TABLEUPDATE(.T.)
ENDIF   

*** Clear the temp record in static file
IF !USED('SYDSUPRG')
  SELECT 0 
  USE (oariaapplication.SysPath+'SYDSUPRG')
ELSE
  SELECT SYDSUPRG
ENDIF

SET ORDER TO TAG PRGCOMP

*** Check if the program was single user clear the locking
IF SEEK(SUBSTR(lcBaseWind,4)+oariaapplication.ActiveCompanyID,'SYDSUPRG')
  UNLOCK IN SYDSUPRG
ENDIF

glNoLog    = .F.


*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[Start]
*!*************************************************************
*! Name      : lfGetReason
*! Developer : Mariam Mazhar
*! Date      : 12/09/2010
*! Purpose   : To return the reason desc.
*!*************************************************************
FUNCTION lfGetReason
LPARAMETERS lcReasonCode
lcReasonDesc = ''
DO CASE 
  CASE lcReasonCode = 'AF' 
     lcReasonDesc = 'AWAITING FINANCIAL STATEMENTS'  
CASE lcReasonCode ='AI' 
     lcReasonDesc = 'AVAILABLE INFO UNSATISFACTORY'  
CASE lcReasonCode ='AW' 
     lcReasonDesc = 'AWAITING DOCUMENTATION'         
CASE lcReasonCode ='CC' 
     lcReasonDesc = 'CONTACTING CUSTOMER'            
CASE lcReasonCode ='CL' 
     lcReasonDesc = 'CREDIT LOSS'                    
CASE lcReasonCode ='CM' 
     lcReasonDesc = 'CREDIT OFFICER DECISION'        
CASE lcReasonCode ='CS' 
     lcReasonDesc = 'CUST REFUSES SUFFICIENT INFO'   
CASE lcReasonCode ='DS' 
     lcReasonDesc = 'UNSATISFACTORY INFO'            
CASE lcReasonCode ='FG' 
     lcReasonDesc = 'FACTOR GUARANTED'               
CASE lcReasonCode ='FR' 
     lcReasonDesc = 'FINANCIAL STATEMENT REFUSED'   
CASE lcReasonCode ='HR' 
     lcReasonDesc = 'HIGH RISK COUNTRY'              
CASE lcReasonCode ='II' 
     lcReasonDesc = 'INSUFFICIENT INFORMATION'       
CASE lcReasonCode ='IU' 
     lcReasonDesc = 'INFO UNSATISFACTORY'            
CASE lcReasonCode ='LF' 
     lcReasonDesc = 'LINE FULL'                      
CASE lcReasonCode ='LI' 
     lcReasonDesc = 'LINE INCREASE'                  
CASE lcReasonCode ='LS' 
     lcReasonDesc = 'LOSSES SUSTAINED'               
CASE lcReasonCode ='MT' 
     lcReasonDesc = 'MAXIMUM TERMS OF 30 DAYS'       
CASE lcReasonCode ='NP' 
     lcReasonDesc = 'NO PHONE LISTING'               
CASE lcReasonCode ='NR' 
     lcReasonDesc = 'NO REPLY TO REQUEST FOR INFO'   
CASE lcReasonCode ='PA' 
     lcReasonDesc = 'PARTIAL APPROVAL'              
CASE lcReasonCode ='PC' 
     lcReasonDesc = 'PLACED FOR COLLECTION'          
CASE lcReasonCode ='PD' 
     lcReasonDesc = 'PAST DUE CONDITION'             
CASE lcReasonCode ='PY' 
     lcReasonDesc = 'PAST DUE YOUR INVOICE'          
CASE lcReasonCode ='RB' 
     lcReasonDesc = 'CONTACTING REFERENCES/BANK'     
CASE lcReasonCode ='RR' 
     lcReasonDesc = 'DUPLICATE REQUEST'              
CASE lcReasonCode ='RT' 
     lcReasonDesc = 'REFER TO CREDIT LINE'           
CASE lcReasonCode ='SA' 
     lcReasonDesc = 'SINGLE APPROVAL'                
CASE lcReasonCode ='SI' 
     lcReasonDesc = 'SENDING CREDIT APPLICATION'     
CASE lcReasonCode ='SP' 
     lcReasonDesc = 'SLOW PAY EXPERIENCE'            
CASE lcReasonCode ='SU' 
     lcReasonDesc = 'ACCOUNT SUBJECT TO SURCHARGE'   
CASE lcReasonCode ='TU' 
     lcReasonDesc = 'TERMS UNACCEPTABLE'             
CASE lcReasonCode ='UE' 
     lcReasonDesc = 'UNSATISFACTORY PREV EXPERIENCE' 
CASE lcReasonCode ='UF' 
     lcReasonDesc = 'UNBALANCED FINANCIAL CONDITION' 
CASE lcReasonCode ='UP' 
     lcReasonDesc = 'UPDATING INFORMATION'           
CASE lcReasonCode ='US' 
     lcReasonDesc = 'OUTSIDE US-EXPORT RIDER NEEDED' 
CASE lcReasonCode ='WD' 
     lcReasonDesc = 'CREDIT WITHDRAWN'               
  CASE lcReasonCode ='WX' 
     lcReasonDesc = 'ALL PRIOR APPROVALS WITHDRAWN'  
ENDCASE 
RETURN lcReasonDesc 
*!*************************************************************
*! Name      : lfGetAction
*! Developer : Mariam Mazhar
*! Date      : 12/09/2010
*! Purpose   : To return the action desc.
*!*************************************************************
FUNCTION lfGetAction
LPARAMETERS lcActionId
lcRetAct  =''
DO CASE 
  CASE lcActionId ='AA' 
    lcRetAct  ='Approved'
  CASE lcActionId ='AC'	
    lcRetAct  ='Approved'
  
  CASE lcActionId ='AD'	
    lcRetAct  ='Approved for reduced amount'
    
  CASE lcActionId = 'AX'
	lcRetAct  ='Split Risk approved'

  CASE lcActionId ='DA'
  	lcRetAct  ='Declined'
	
  CASE lcActionId ="DR"
    lcRetAct  ='Declined'
	
  CASE lcActionId ='HC'
	lcRetAct  ='Credit officer hold'

  CASE lcActionId ="CI"
	lcRetAct  ='Cancelled'
	
  CASE lcActionId ='CR'
    lcRetAct  ="Cancelled by credit officer per client's request"
    
  CASE lcActionId ='CZ'	
    lcRetAct  ='Cancelled by client via on-line system'
    

  CASE EMPTY(lcActionId)
     lcRetAct  ='the request is PENDING'
ENDCASE 
RETURN lcRetAct  
*! C201294,1 MMT 12/09/2010 Modify program to use the new guidlines[END]