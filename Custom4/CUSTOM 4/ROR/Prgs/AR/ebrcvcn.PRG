*!*************************************************************
*! Name : EBRCVCN
*! Auth : Waleed Hamed (WLD)
*! Date : 11/21/2006
*!*************************************************************
*! Synopsis : Read inbound order confirmation text file, Update master
*!            files and generate & display a report.
*!*************************************************************
*! Calls : 
*!         Procedures : lfCleanPrg, lpAddDetail,lfUpdatMFile,lfPrint
*!*************************************************************
*! Modifications : None.
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
=gfOpenTABLE(OARIAAPPLICATION.DATADIR +'CUSTOMER',OARIAAPPLICATION.DATADIR+'CUSTOMER','SH')
=gfOpenTAble(OARIAAPPLICATION.DATADIR+'ORDHDR',OARIAAPPLICATION.DATADIR+'ORDHDR','SH')
=gfOpenTABLE(OARIAAPPLICATION.DATADIR+'CITTRNLN',OARIAAPPLICATION.DATADIR+'CITTRNLN','SH')

*-- Create transaction temp. file
SELECT CITTRNLN
lcOrdCon = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+lcOrdCon) (cFacCode C(6), BatchNo C(2), dDate D,;
              Account C(5), Order C(6), Status C(1), ApprAmt N(14,2), Approval C(10),;
              cOrdDisp C(4), cOrdHDRes C(30), cGrantName C(30), cComment C(30),;
              FactAcc C(6), btName C(30),Start date,Complete date,BookAmt N(14,2)) 

SELECT (lcOrdCon)
INDEX ON Account+Order TAG (lcOrdCon)

*-- Read details records and save data into the temp. file
DO WHILE !FEOF(lnOrdCon)
  lcSegLine = FGETS(lnOrdCon)
  *wld
  *IF SUBSTR(lcSegLine,185,1) = '1'
  IF LEFT(lcSegLine,1) = 'O'
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
*! Auth : Waleed Hamed (WLD)
*! Date : 11/21/2006
*!*************************************************************
*! Synopsis : Update master order header file with approved, Hold and 
*!            Decline information. Update customer file with Available
*!            credit.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfUpdatMFile
PRIVATE lcAccount, lnOpenAmt

SELECT OrdHdr
gfSetOrder('ORDACCT')

WAIT 'Update master files...' WINDOW NOWAIT
SELECT (lcOrdCon)
GO TOP
DO WHILE !EOF()
  lcAccount = Account
  SCAN REST WHILE Account+Order = lcAccount
    DO CASE
      CASE Status = 'A'
        *-- Approved orders
        SELECT OrdHdr
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          gfREPLACE ([STATUS     WITH IIF(Status="C","C","O")   ,]+;
                     [APPRAMT    WITH &lcOrdCon..ApprAmt,]+; 
                     [APPROVAL   WITH &lcOrdCon..Approval])
          UNLOCK
        ENDIF  
      CASE Status = 'D'
        *-- Declined orders
        SELECT OrdHdr
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
          = RLOCK()
          gfREPLACE ([STATUS     WITH 'H'       ,]+;
                     [APPRAMT    WITH  0        ,]+;
                     [APPROVAL   WITH 'DECLINED',]+;
                     [DECL_CODE  WITH ''        ,]+;
                     [DECL_DATE  WITH oAriaApplication.SystemDate])
          UNLOCK
        ENDIF  
      CASE Status = 'H'
        *-- Hold orders
        SELECT OrdHdr
        IF gfSEEK(lcAccount+'O'+&lcOrdCon..Order)
          =RLOCK()
          gfREPLACE ([STATUS   WITH 'H' ,]+;
                     [APPRAMT  WITH 0   ,]+;
                     [APPROVAL WITH &lcOrdCon..Approval])
          UNLOCK
        ENDIF
      *wld
      OTHERWISE
        LOOP
    ENDCASE  
    SELECT CitTrnLn
    IF gfSEEK(OrdHdr.cFacCode+OrdHdr.Order)
      gfREPLACE ([Approval  WITH &lcOrdCon..Approval    ,]+;
                 [ApprAmt   WITH &lcOrdCon..ApprAmt     ,]+;
                 [Status    WITH &lcOrdCon..Status      ,]+;
                 [cOrdDisp  WITH &lcOrdCon..cOrdDisp    ,]+;
                 [cOrdHDRes WITH &lcOrdCon..cOrdHDRes])
    ENDIF
  ENDSCAN
  WAIT 'COMPUTING AVAILABLE CREDIT...' WINDOW NOWAIT
  *-- Sum all open amount for this customer
  SELECT OrdHdr
  =gfSEEK(lcAccount+'O')
  lnOpenAmt = 0.00
  SUM REST OPENAMT TO lnOpenAmt FOR Status='O' WHILE Account+cOrdType+Order = lcAccount+'O'
 
  *-- Update the customer available credit
  SELECT Customer
  IF gfSEEK('M'+lcAccount)
    gfREPLACE ([NEXT 1 CrAvail WITH CrLimit - (TotAge+lnOpenAmt)])
    
    IF CrAvail > CrLimit
      gfREPLACE ([NEXT 1 CrAvail WITH CrLimit])
    ENDIF
  ENDIF 
  SELECT (lcOrdCon)
ENDDO
SELECT OrdHdr
gfSetOrder('ORDHDR')

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

WAIT CLEAR

*!*************************************************************
*! Name : lpAddDetail
*! Auth : Waleed Hamed (WLD)
*! Date : 11/21/2006
*!*************************************************************
*! Synopsis : Update the temp. file with data element read from the text
*!            order confirmation file.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddDetail
PRIVATE ldDate, lcOrder

*-- Get order send date
*-- Get order number as sent to CIT ROR
&&lcOrder = SUBSTR(lcSegLine,70,6)
lcOrder = SUBSTR(lcSegLine,44,6)
*-- Get customer code from order header file.
=gfSEEK('O'+lcOrder,'OrdHdr')
=gfSeek('M'+OrdHdr.Account,'Customer')
=gfSEEK(OrdHdr.cFacCode+OrdHdr.Order,'CitTrnLn')

SELECT (lcOrdCon)
APPEND BLANK
REPLACE cFacCode  WITH OrdHdr.cFacCode            ,;
        Account   WITH OrdHdr.Account             ,;
        Order     WITH lcOrder                    ,;
        Approval  WITH SUBSTR(lcSegLine,53,4)     ,;
        Status    WITH SUBSTR(lcSegLine,57,1)     ,;
        cOrdDisp  WITH SUBSTR(lcSegLine,58,1)     ,;
        ApprAmt   WITH VAL(SUBSTR(lcSegLine,59,7)),;
        cOrdHDRes WITH SUBSTR(lcSegLine,66,23)    ,;
        Start     WITH OrdHdr.Start               ,;
        Complete  WITH OrdHdr.Complete            ,;
        BookAmt   WITH OrdHdr.BookAmt             ,;
        FactAcc   WITH Customer.FactAcct          ,;
        btName    WITH Customer.btName            ,;
        dDate     WITH CitTrnLn.dDate
        
*!*	*!*************************************************************
*!*	*! Name      : lfStatus
*!*	*! Developer : WAM
*!*	*! Date      : 12/30/1999
*!*	*! Purpose   : Get order status and Disposition Description
*!*	*!*************************************************************
*!*	*! Calls     : None
*!*	*!*************************************************************
*!*	*! Passed Parameters  :  None
*!*	*!*************************************************************
*!*	*! Returns            :  None
*!*	*!*************************************************************
*!*	*! Example            :  =lfStatus()
*!*	*!*************************************************************
*!*	FUNCTION lfStatus
*!*	STORE '' TO lcStatus,lcDispDesc,lcReason
*!*	DO CASE
*!*	  CASE CitTrnLn.Status = 'A'
*!*	    lcStatus = 'APPROVED'
*!*	  CASE CitTrnLn.Status = 'H'
*!*	    lcStatus = 'HOLD'
*!*	  CASE CitTrnLn.Status = 'D'
*!*	    lcStatus = 'DECLINED'
*!*	ENDCASE
*!*	lcDispDesc = CitTrnLn.cOrdHDRes
*!*	RETURN (lcStatus)
&&&&& code moved to the report  
*!*************************************************************
*! Name      : lfPrint
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/21/2006
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
loogScroll.cCROrientation = 'P'
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
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/21/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION FileExist
LPARAMETERS cFileToCheckFor

RETURN loOGScroll.FileExist(cFileToCheckFor)

*!*************************************************************
*! Name      : lfcleanPrg
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
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
