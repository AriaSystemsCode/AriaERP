*:***************************************************************************
*: Program file  : ARGRSPAN.PRG
*: Program desc. : CUSTOMIZED GROSS PROFIT FOR REVUE.
*: Date          : 08/12/2008
*: System        : Aria Advantage Series.4XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job Number: C201040[T20080422.0026]
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)



oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

oAriaEnvironment.Report.gcAct_Appl = "AR"

oariaenvironment.activeModuleID = 'AR'


PUBLIC gcAct_Appl 
gcAct_Appl = "AR"


IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

lcOGTmpForm  = oAriaEnvironment.Cursors.GetCursorTempName()

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
IF llFrstTmAp
  STORE 0 TO lnClrLnAp , lnClrPosAp , lnStyLnAp
  DECLARE laItemSeg[1]
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.ItemMask.Do(@laItemSeg)
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfItemMask(@laItemSeg)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLnAp  = LEN(laItemSeg[lnCount,3])
      lnClrPosAp = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    lnStyLnAp  = LEN(oAriaEnvironment.ItemMask.Do('PM'))
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    lnStyLnAp  = LEN(gfItemMask('PM'))
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  llFrstTmAp = .F.
ENDIF
               
=lfCreatTmp()
=lfcolctDat()


IF lcRPSortBy = "C"
  lcRpForm = IIF(lcRPSumDet = "S" , 'ARGRSPCS' , 'ARGRSPCD')
ELSE
  lcRpForm = 'ARGRSPST'
ENDIF

*: C201040,2 MMT 10/01/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') <> 'C'
  =gfCrtFrm(lcRpForm,'',llOGRefForm)
  =lfRepPltFr(lcRpForm)
ENDIF   
*: C201040,2 MMT 10/01/2009 call request builder fxp to collect data[End]

SELECT (lclinesAn)
LOCATE

IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "D"
    IF lcRPBasdOn = "B"
      STORE 0 TO lnUntSold , lnAmount
    ENDIF
    lcExpPrnt = SPACE(0)
    lcExpPrnt = &lclinesAn..Account
    lcPrnSson = &lclinesAn..Account + &lclinesAn..Season
    SCAN
      IF lcExpPrnt # &lclinesAn..Account OR lcPrnSson # (&lclinesAn..Account + &lclinesAn..Season)
        IF lcExpPrnt # &lclinesAn..Account
          lcExpPrnt = &lclinesAn..Account
          SKIP - 1
          REPLACE &lclinesAn..llPrntDet WITH .T.
          SKIP
        ENDIF
        IF lcPrnSson # (&lclinesAn..Account + &lclinesAn..Season)
          lcPrnSson = &lclinesAn..Account + &lclinesAn..Season
          SKIP - 1
          REPLACE &lclinesAn..llPrntSum WITH .T.
          SKIP
        ENDIF     
        IF &lclinesAn..Account = "ZZZZZ"
          EXIT
        ENDIF
      ENDIF
    ENDSCAN
    IF !llRPRecap
      GOTO BOTTOM
      REPLACE &lclinesAn..llPrntDet WITH .T. ,;
              &lclinesAn..llPrntSum WITH .T.
    ENDIF
  ENDIF
ENDIF

LOCATE
REPLACE ALL &lclinesAn..Season WITH SPACE(0) FOR &lclinesAn..ACCOUNT = "ZZZZZ"
IF llRPRecap
  GOTO BOTTOM
  IF &lclinesAn..Account = "ZZZZZ"
    REPLACE &lclinesAn..llPrntDet WITH .T.
  ENDIF
ENDIF
LOCATE

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
oAriaEnvironment.report.OGLastForm = lcRpForm

loProgress.Percent = 0.9
loProgress.Description = "Printing Report..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

PRIVATE loProxy
loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

IF loProxy.GetRequest(lcRequestID).Status = 3
  oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

  loProgress.Percent = 1.0
  loProgress.Description = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)
ENDIF
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  DO gfdispre WITH EVALUATE('lcRpForm')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]



*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

IF USED(lclinesAn)
  USE IN (lclinesAn)
ENDIF 

DIMENSION laFileStruct[24,4]

laFileStruct[1,1] = 'Account'
laFileStruct[1,2] = 'C'
laFileStruct[1,3] = 5
laFileStruct[1,4] = 0


laFileStruct[2,1] = 'Btname'
laFileStruct[2,2] = 'C'
laFileStruct[2,3] = 30
laFileStruct[2,4] = 0

laFileStruct[3,1] = 'SeasonDesc'
laFileStruct[3,2] = 'C'
laFileStruct[3,3] = 30
laFileStruct[3,4] = 0


laFileStruct[4,1] = 'UntSold'
laFileStruct[4,2] = 'N'
laFileStruct[4,3] = 8
laFileStruct[4,4] = 0

laFileStruct[5,1] = 'PriceSty'
laFileStruct[5,2] = 'N'
laFileStruct[5,3] = 11
laFileStruct[5,4] = 4

laFileStruct[6,1] = 'Amount'
laFileStruct[6,2] = 'N'
laFileStruct[6,3] = 11
laFileStruct[6,4] = 4

laFileStruct[7,1] = 'UntRtrn'
laFileStruct[7,2] = 'N'
laFileStruct[7,3] = 5
laFileStruct[7,4] = 0

laFileStruct[8,1] = 'AmntRtrn'
laFileStruct[8,2] = 'N'
laFileStruct[8,3] = 11
laFileStruct[8,4] = 4

laFileStruct[9,1] = 'Dscont'
laFileStruct[9,2] = 'N'
laFileStruct[9,3] = 10
laFileStruct[9,4] = 4

laFileStruct[10,1] = 'AmntCost'
laFileStruct[10,2] = 'N'
laFileStruct[10,3] = 13
laFileStruct[10,4] = 4

laFileStruct[11,1] = 'NetSals'
laFileStruct[11,2] = 'N'
laFileStruct[11,3] = 13
laFileStruct[11,4] = 4

laFileStruct[12,1] = 'NetUntSold'
laFileStruct[12,2] = 'N'
laFileStruct[12,3] = 8
laFileStruct[12,4] = 0

laFileStruct[13,1] = 'Cost'
laFileStruct[13,2] = 'N'
laFileStruct[13,3] = 17
laFileStruct[13,4] = 8



laFileStruct[15,1] = 'AmntPrft'
laFileStruct[15,2] = 'N'
laFileStruct[15,3] = 13
laFileStruct[15,4] = 4

laFileStruct[16,1] = 'StyDesc'
laFileStruct[16,2] = 'C'
laFileStruct[16,3] = 30
laFileStruct[16,4] = 0

laFileStruct[17,1] = 'Season'
laFileStruct[17,2] = 'C'
laFileStruct[17,3] = 6
laFileStruct[17,4] = 0

laFileStruct[18,1] = 'llCsSum'
laFileStruct[18,2] = 'L'
laFileStruct[18,3] = 1
laFileStruct[18,4] = 0

laFileStruct[19,1] = 'llPrntDet'
laFileStruct[19,2] = 'L'
laFileStruct[19,3] = 1
laFileStruct[19,4] = 0
 

laFileStruct[20,1] = 'llPrntSum'
laFileStruct[20,2] = 'L'
laFileStruct[20,3] = 1
laFileStruct[20,4] = 0


laFileStruct[21,1] = 'ShpCostD'
laFileStruct[21,2] = 'N'
laFileStruct[21,3] = 13
laFileStruct[21,4] = 4

laFileStruct[22,1] = 'OpnCostD'
laFileStruct[22,2] = 'N'
laFileStruct[22,3] = 13
laFileStruct[22,4] = 4


laFileStruct[23,1] = 'OpnQty'
laFileStruct[23,2] = 'N'
laFileStruct[23,3] = 6
laFileStruct[23,4] = 0

laFileStruct[24,1] = 'ShpQty'
laFileStruct[24,2] = 'N'
laFileStruct[24,3] = 6
laFileStruct[24,4] = 0


laFileStruct[14,1] = 'Style'
laFileStruct[14,2] = 'C'
laFileStruct[14,3] = lnStyLnAp+lnClrLnAp+1
laFileStruct[14,4] = 0


*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "S"
    =oAriaEnvironment.Cursors.createcursor(lclinesAn,@laFileStruct,"Account + Season",lclinesAn,.f.)
  ELSE
    =oAriaEnvironment.Cursors.createcursor(lclinesAn,@laFileStruct,"Account + Season + Style",lclinesAn,.f.)
  ENDIF
ELSE
   =oAriaEnvironment.Cursors.createcursor(lclinesAn,@laFileStruct,"Style + STR(PriceSty,9,2)",lclinesAn,.f.)
ENDIF
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  IF lcRPSortBy = "C"                  && Customer
    IF lcRPSumDet = "S"
      =gfCrtTmp(lclinesAn,@laFileStruct,"Account + Season",lclinesAn,.f.)
    ELSE
      =gfCrtTmp(lclinesAn,@laFileStruct,"Account + Season + Style",lclinesAn,.f.)
    ENDIF
  ELSE
     =gfCrtTmp(lclinesAn,@laFileStruct,"Style + STR(PriceSty,9,2)",lclinesAn,.f.)
  ENDIF
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function to Collect the date from the credit and 
*!           : the debit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.OpenTable('ORDHDR','ORDACCT')
=oAriaEnvironment.remotetableaccess.OpenTable('CUSTOMER','CUSTOMER')
=oAriaEnvironment.remotetableaccess.OpenTable('INVLINE','INVLINE')
=oAriaEnvironment.remotetableaccess.OpenTable('STYLE','STYLE')
=oAriaEnvironment.remotetableaccess.OpenTable('ORDLINE','ORDLINE')
=oAriaEnvironment.remotetableaccess.OpenTable('INVHDR','INVHDRA')
=oAriaEnvironment.remotetableaccess.OpenTable('RETHDR','RETHDRA')
=oAriaEnvironment.remotetableaccess.OpenTable('RETLINE','RETLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
=gfOpenTable('ORDHDR','ORDACCT')
=gfOpenTable('CUSTOMER','CUSTOMER')
=gfOpenTable('INVLINE','INVLINE')
=gfOpenTable('STYLE','STYLE')
=gfOpenTable('ORDLINE','ORDLINE')
=gfOpenTable('INVHDR','INVHDRA')
=gfOpenTable('RETHDR','RETHDRA')
=gfOpenTable('RETLINE','RETLINE')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

lcAlias = ALIAS()
STORE SPACE(0) TO lnCostVal , lnInvCost
STORE SPACE(0) TO lcDescSesn

lnRetCost = 0


llAccSele = .F.
lcAccFile = ''
IF lnAcctPos > 0
  IF !EMPTY(laogFxflt[lnAcctPos,6]) AND USED(laogFxflt[lnAcctPos,6])
    SELECT (laogFxflt[lnAcctPos,6])
    LOCATE 
    IF !EOF()
      llAccSele = .T.
      lcAccFile = laogFxflt[lnAcctPos,6]
    ENDIF 
  ENDIF 
ENDIF 

ldStartComp = {}
ldEndComp = {}
IF lnOrdHdPos > 0
  IF !EMPTY(laogFxflt[lnOrdHdPos ,6])
    ldStartComp = CTOD(SUBSTR(laOGFxFlt[lnOrdHdPos ,6],1,10))
    ldEndComp = CTOD(SUBSTR(laOGFxFlt[lnOrdHdPos ,6],12,21))
  ENDIF 
ENDIF 

ldStartInv = {}
ldEndInv = {}
IF lnInvHdPos > 0
  IF !EMPTY(laogFxflt[lnInvHdPos ,6])
    ldStartInv = CTOD(SUBSTR(laOGFxFlt[lnInvHdPos ,6],1,10))
    ldEndInv = CTOD(SUBSTR(laOGFxFlt[lnInvHdPos ,6],12,21))
  ENDIF 
ENDIF 

llSelectStyle = .F.
lcStyFile = ''
lnStyPos = ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR')
IF lnStyPos > 0
  lnStyPos  = ASUBSCRIPT(laOGFxFlt,lnStyPos,1)
  IF !EMPTY(laOGFxFlt[lnStyPos  ,6]) AND USED(laOGFxFlt[lnStyPos  ,6])
    SELECT (laOGFxFlt[lnStyPos  ,6])
    LOCATE 
    IF !EOF()
      llSelectStyle = .T.
      lcStyFile = laOGFxFlt[lnStyPos  ,6]
    ENDIF 
  ENDIF 
ENDIF





*--Case Customer Summary
IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "S"                && Customer / Summary.

    *--First option : Open quantities.
    IF lcRPBasdOn = "O"              && Case Open quantities.
      =lfOpnQnt()
    ENDIF

    *--2nd option : Shipped quantities.
    IF lcRPBasdOn = "S"              && Case Shipped quantities.    
      =lfInvQnt()
      =lfRetQnt()
    ENDIF

    *--3rd option : Booked quantities.
    IF lcRPBasdOn = "B"              && Case Booked quantities.
      =lfBokQnt()
    ENDIF

  ELSE                               && && Customer / Detail.

    *--First option : Open quantities.
    IF lcRPBasdOn = "O"              && Case Open quantities.
      =lfOpnDet()
    ENDIF

    *--2nd option : Shipped quantities.
    IF lcRPBasdOn = "S"              && Case Shipped quantities.
      =lfInvDet()
      =lfRetDet()
    ENDIF

    *--3rd option : Booked quantities.
    IF lcRPBasdOn = "B"              && Case Booked quantities.
      =lfBokDet()
    ENDIF
  ENDIF

ELSE                                 && Style

  *--First option : Open quantities.
  IF lcRPBasdOn = "O"              && Case Open quantities.
    =lfOpnSty()
  ENDIF

  *--2nd option : Shipped quantities.
  IF lcRPBasdOn = "S"              && Case Shipped quantities.
    =lfInvSty()
    =lfRetSty()
  ENDIF

  *--3rd option : Booked quantities.
  IF lcRPBasdOn = "B"              && Case Booked quantities.
    =lfBokSty()
  ENDIF

ENDIF
SELECT (lcAlias)


*!*************************************************************
*! Name      : lfOpnQnt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Open Quntities in summery.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnQnt()
*!*************************************************************
FUNCTION lfOpnQnt
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
lnAccNt = 0
COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  SELECT CUSTOMER
  =gfSEEK('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)

 lcAccount = CUSTOMER.ACCOUNT
   
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 IF TYPE('lcXMLFileName') = 'C'
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
 
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 ENDIF 
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]



  SELECT ORDHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
    
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
     =gfSeek(lcAccount)  
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
  SCAN REST WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH" AND;
             IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDHDR.COMPLETE,ldStartComp ,ldEndComp ),.T.)

    SELECT (lclinesAn)
    IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON)
      REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
      REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                          ,;
              &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                       ,;
              &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDHDR.OPENAMT)/100) ,;
              &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
              
      IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)

        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
        REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
                &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)

      ENDIF
    ELSE
      APPEND BLANK
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
        lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
      
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
      REPLACE &lclinesAn..OpnCostD   WITH lfTotCost() 
      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
              &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
              &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
              &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
              &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
              &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.

          REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
          REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
                  &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
                  &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        

        ELSE
          APPEND BLANK
          
         
          REPLACE &lclinesAn..OpnCostD WITH lfTotCost() 
          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
                  &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
                  &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
                  &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
                  &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..llCsSum    WITH .T.                                ,;
                  &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
         
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN


SELECT (lcAlias)


*!***************************************************************************
*! Name      : lfTotCost
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect total cost * Units.
*!***************************************************************************
*! Called from : lfOpnQnt()
*!***************************************************************************
*! Example     : =lfTotCost()
*!***************************************************************************
*
FUNCTION lfTotCost
PRIVATE lcAlias , lcKeyOrd

lcAlias = ALIAS()
lnCostVal = 0
SELECT ORDLINE
lcKeyOrd = EVAL(KEY())
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(ORDHDR.CORDTYPE + ORDHDR.ORDER,'ORDLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER,'ORDLINE')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = ORDHDR.CORDTYPE + ORDHDR.ORDER
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  *IF oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.STYLE , 'STYLE')
  IF (TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.STYLE , 'STYLE')) OR gfSEEK(ORDLINE.STYLE , 'STYLE')
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    IF STYLE.MAKE
      lnCostVal = lnCostVal + ((STYLE.NMCOST1 + STYLE.NMCOST2 + STYLE.NMCOST3 + STYLE.NMCOST4 + STYLE.NMCOST5 +STYLE.NMCOST6+STYLE.NMCOST7) * OrdLine.TotQty)
    ELSE
      lnCostVal = lnCostVal + ((STYLE.NICOST1 + STYLE.NICOST2 + STYLE.NICOST3 + STYLE.NICOST4 + STYLE.NICOST5+STYLE.NICOST6+STYLE.NICOST7) * OrdLine.TotQty)
    ENDIF
  ENDIF
ENDSCAN
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcKeyOrd,'ORDLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEk(lcKeyOrd,'ORDLINE')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SELECT (lcAlias)
RETURN lnCostVal
*-- End of lfTotCost.
*!*************************************************************
*! Name      : lfInvQnt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the invoice Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvQnt()
*!*************************************************************
FUNCTION lfInvQnt
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT INVHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
     gfSeek(lcAccount)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
  SCAN REST WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND  IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(INVHDR.INVDATE,ldStartInv ,ldEndInv ),.T.)
    SELECT (lclinesAn)
    IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON)
   
      REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                              ,;
              &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                           ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
              &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                              ,;
              &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()                             ,;
              &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
              
      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
        
        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)

      ENDIF
    ELSE
      APPEND BLANK
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

      lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')

     *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
     ELSE
       lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
     ENDIF 
     *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
              &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
              &lclinesAn..UntSold    WITH INVHDR.SHIP                            ,;
              &lclinesAn..Amount     WITH INVHDR.SHIPAMT                         ,;
              &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
              &lclinesAn..ShpQty     WITH INVHDR.SHIP                            ,;
              &lclinesAn..ShpCostD   WITH lfInvCost()                            ,;
              &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
     
      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)

        ELSE
          APPEND BLANK
          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                          ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON                                    ,;
                  &lclinesAn..UntSold    WITH INVHDR.SHIP                                      ,;
                  &lclinesAn..Amount     WITH INVHDR.SHIPAMT                                   ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                                       ,;
                  &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                  &lclinesAn..llCsSum    WITH .T.                                              ,;
                  &lclinesAn..ShpQty     WITH &lclinesAn..ShpQty  + INVHDR.SHIP                ,;
                  &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + lnInvCost                 ,;
                  &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN


SELECT (lcAlias)
*!*************************************************************
*! Name      : lfInvCost
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the style cost from invline.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvCost()
*!*************************************************************
FUNCTION lfInvCost
PRIVATE lcAlias , lcKeyInv

lcAlias = ALIAS()
lnInvCost = 0
SELECT INVLINE
lcKeyInv = EVAL(KEY())
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(INVHDR.INVOICE ,'INVLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEK(INVHDR.INVOICE ,'INVLINE')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
  lnInvCost = lnInvCost + (InvLine.TotQty * InvLine.Cost)
ENDSCAN
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcKeyInv,'INVLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
 =GFSEEK(lcKeyInv,'INVLINE')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SELECT (lcAlias)
RETURN lnInvCost
*--End of lfInvCost.
*!*************************************************************
*! Name      : lfRetQnt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the return Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetQnt()
*!*************************************************************
FUNCTION lfRetQnt
PRIVATE lcAlias

lcAlias = ALIAS()

SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
lnAccNt = 0
COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR  IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)

  lcAccount = CUSTOMER.ACCOUNT
  
  
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 IF TYPE('lcXMLFileName') = 'C'
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 ENDIF 
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
 SELECT RETHDR
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 IF TYPE('lcXMLFileName') = 'C'
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
 ELSE
   gfSeek(lcAccount)  
 ENDIF 
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
  SCAN REST WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND  IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETHDR.CRDATE,ldStartInv ,ldEndInv ),.T.)
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.    
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + '_zzzzz')
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
                  
        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = 'Ret. Without Inv.'
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH '_zzzzz'         ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES    ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ELSE
            APPEND BLANK
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH '_zzzzz'        ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1 ,;
                    &lclinesAn..llCsSum    WITH .T.

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ENDIF
        ENDIF
      ENDIF
    ELSE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.ACCOUNT + RETHDR.INVOICE,'INVHDR')        
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        =gfSeek(RETHDR.ACCOUNT + RETHDR.INVOICE,'INVHDR')   
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON)
        
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          
        ENDIF
      ELSE
        APPEND BLANK
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        ELSE
          lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
        ENDIF 
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          ELSE
            APPEND BLANK

            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH INVHDR.SEASON   ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1,;
                    &lclinesAn..llCsSum    WITH .T.
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN
SELECT (lcAlias)
*--End of lfRetQnt.
*!***************************************************************************
*! Name      : lfRetCost
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the style cost from RetLine.
*!***************************************************************************
*! Example   : =lfRetCost()
*!***************************************************************************
FUNCTION lfRetCost
PRIVATE lcAlias , lcKeyRet

lcAlias = ALIAS()
lnRetCost = 0
SELECT RetLine
lcKeyRet = EVAL(KEY())
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord(RetHdr.CrMemo ,'RetLine')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEK(RetHdr.CrMemo ,'RetLine')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RetHdr.CrMemo
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF
  lnRetCost = lnRetCost + (RetLine.TotQty * RetLine.Cost)
ENDSCAN
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcKeyRet,'RetLine')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEk(lcKeyRet,'RetLine')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SELECT (lcAlias)
RETURN lnRetCost
*--End of lfRetCost.

*!*************************************************************
*! Name      : lfBokQnt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Book Quntities in summery.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokQnt()
*!*************************************************************
FUNCTION lfBokQnt
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord("M")
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek("M")
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   ENDIF 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT ORDHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)  
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC" AND ;
             IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDHDR.COMPLETE,ldStartComp ,ldEndComp ),.T.)

    SELECT (lclinesAn)
    IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON)
      
      REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + OrdHdr.Open                          ,;
              &lclinesAn..Amount   WITH &lclinesAn..Amount   + OrdHdr.OpenAmt                       ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((OrdHdr.Disc * OrdHdr.OpenAmt)/100) ,;
              &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + OrdHdr.OPEN                          ,;
              &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost()                          ,;
              &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)

        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.Open                        ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.OpenAMT                     ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
                &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lnCostVal                          ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

                
      ENDIF
    ELSE
      APPEND BLANK
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
              &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
              &lclinesAn..UntSold    WITH ORDHDR.Open                        ,;
              &lclinesAn..Amount     WITH ORDHDR.OpenAMT                     ,;
              &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
              &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
              &lclinesAn..OpnCostD   WITH lfTotCost()                        ,;
              &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.
        
          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.Open                        ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.OpenAMT                     ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
                  &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lnCostVal                          ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
           
        ELSE
          APPEND BLANK

          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
                  &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
                  &lclinesAn..UntSold    WITH ORDHDR.Open                        ,;
                  &lclinesAn..Amount     WITH ORDHDR.OpenAMT                     ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
                  &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..llCsSum    WITH .T.                                ,;
                  &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
                  &lclinesAn..OpnCostD   WITH lnCostVal                          ,;
                  &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT CUSTOMER



*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord("M")
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek("M")
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


  
  SELECT INVHDR
   
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)    
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(INVHDR.INVDATE,ldStartInv ,ldEndInv ),.T.)
    SELECT (lclinesAn)
    IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON)
      
      REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
              &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
              &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()            ,;
              &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                            ,;
              &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
         REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                            ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                          ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
 
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
 
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
     lnPerCent = RECNO()/lnAccNt 
     loProgress.Percent = lnPerCent * 0.9
     loProgress.Description = "Collecting Data For Account:"+lcAccount 
     loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT RETHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND IIF(!EMPTY(ldEndInv),BETWEEN(RETHDR.CRDATE,ldStartInv ,ldEndInv ),.T.)
     IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.       
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + '_zzzzz')
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = 'Ret. Without Inv.'
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH '_zzzzz'         ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt    ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ELSE
            APPEND BLANK
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH '_zzzzz'        ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1,;
                    &lclinesAn..llCsSum    WITH .T.

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        ENDIF
      ENDIF
    ELSE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.ACCOUNT + RETHDR.INVOICE,'INVHDR')        
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        gfSeek(RETHDR.ACCOUNT + RETHDR.INVOICE,'INVHDR')  
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON)

        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfOpnDet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Open Quntities in Detail.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnDet()
*!*************************************************************
FUNCTION lfOpnDet

PRIVATE lcAlias
lcAlias = ALIAS()
SELECT CUSTOMER
*!*	SET RELATION TO CUSTOMER.ACCOUNT INTO ORDHDR ADDITIVE
*!*	SELECT ORDHDR
*!*	SET RELATION TO ORDHDR.CORDTYPE + ORDHDR.ORDER INTO ORDLINE ADDITIVE
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
lnAccNt = 0
COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
SELECT CUSTOMER
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  SELECT CUSTOMER
  =gfSeek('M')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
     lnPerCent = RECNO()/lnAccNt 
     loProgress.Percent = lnPerCent * 0.9
     loProgress.Description = "Collecting Data For Account:"+lcAccount 
     loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT ORDHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  SCAN REST WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH" AND IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDHDR.COMPLETE,ldStartComp ,ldEndComp ),.T.)
   
    SELECT ORDLINE
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(ORDHDR.CORDTYPE + ORDHDR.ORDER)
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      =gfSEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER)
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    SCAN REST WHILE CORDTYPE + ORDER =  ORDHDR.CORDTYPE + ORDHDR.ORDER
      SELECT (lclinesAn)
      IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON + ORDLINE.STYLE)
        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
        REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                                       ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE                       ,;
                &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        
        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)
         
          REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
          REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                  &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                  &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                  &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)

        ENDIF
      ELSE
        APPEND BLANK
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        
        lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
        
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        ELSE
          lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
        ENDIF 
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        
        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                    ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                     ,;
                &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
                &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
                &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
                &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                &lclinesAn..llCsSum    WITH .F.                                 ,;
                &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        
        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + ORDHDR.SEASON)     &&Case the account change and the same season.
            REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
            REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                    &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE      ,;
                    &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100)  ,;
                    &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
                                          
          ELSE
            APPEND BLANK            

            REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                             ,;
                    &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
                    &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
                    &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
                    &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                    &lclinesAn..llCsSum    WITH .T.                                 ,;
                    &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
                    
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfDetCost
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the style cost in detail.
*!*************************************************************
*! Called from : lfOpnDet()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDetCost()
*!*************************************************************
FUNCTION lfDetCost
PRIVATE lcAlias , lcKeyOrd

lcAlias = ALIAS()
lnCostVal = 0
SELECT ORDLINE
lcKeyOrd = EVAL(KEY())
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*IF oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.STYLE , 'STYLE')
IF (TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.STYLE , 'STYLE')) OR ;
   gfSEEK(ORDLINE.STYLE , 'STYLE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  IF STYLE.MAKE
    lnCostVal = lnCostVal + STYLE.NMCOST1 + STYLE.NMCOST2 + STYLE.NMCOST3 + STYLE.NMCOST4 + STYLE.NMCOST5 +STYLE.NMCOST6 + STYLE.NMCOST7
  ELSE
    lnCostVal = lnCostVal + STYLE.NICOST1 + STYLE.NICOST2 + STYLE.NICOST3 + STYLE.NICOST4 + STYLE.NICOST5 +STYLE.NICOST6 + STYLE.NICOST7
  ENDIF
ENDIF
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcKeyOrd,'ORDLINE')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSEEk(lcKeyOrd,'ORDLINE')
ENDIF   
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
SELECT (lcAlias)
RETURN lnCostVal

*!*************************************************************
*! Name      : lfInvDet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the invoice Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvDet()
*!*************************************************************
FUNCTION lfInvDet
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   ENDIF 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT INVHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount) 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   ELSE
      =gfSeek(lcAccount) 
   ENDIF 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(INVHDR.INVDATE,ldStartInv ,ldEndInv ),.T.)
    SELECT INVLINE
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(INVHDR.INVOICE)
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      =gfSEEK(INVHDR.INVOICE)
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End] 
    SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
      SELECT (lclinesAn)
      IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON + INVLINE.STYLE)
        
        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVLINE.TOTQTY                   ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                   ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        
        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                    ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE    ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        ENDIF
      ELSE
        APPEND BLANK
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
          lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        ELSE
          lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
        ENDIF 
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
                &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
                &lclinesAn..Style      WITH INVLINE.STYLE                          ,;
                &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
                &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
                &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..llCsSum    WITH .F.                                    ,;
                &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
                &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
                &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        
        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.
            REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                         ,;
                    &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE         ,;
                    &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                    &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                        ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)      ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
                    
          ELSE
            APPEND BLANK
            
 
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                ,;
                    &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
                    &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
                    &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
                    &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                    &lclinesAn..llCsSum    WITH .T.                                    ,;
                    &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
                    &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
                    &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            
           ENDIF
        ENDIF
      ENDIF
     ENDSCAN
  ENDSCAN
ENDSCAN

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfRetDet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the return Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetDet()
*!*************************************************************
FUNCTION lfRetDet
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
lnAccNt = 0
COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR  IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT RETHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETHDR.CRDATE,ldStartInv ,ldEndInv ),.T.)
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has NO invoice.
      SELECT RETLINE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.CRMEMO)
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        =gfSEEK(RETHDR.CRMEMO)
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO
        
        IF cRet_TrnCD = '4'
          LOOP
        ENDIF

        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + '_zzzzz' + RETLINE.STYLE)                
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price)   ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
                  
          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          ENDIF
        ELSE
          APPEND BLANK
          lcDescSesn = 'Ret. Without Inv.'
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH '_zzzzz'         ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn   WITH RetLine.TotQty * RetLine.Gros_Price ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          REPLACE &lclinesAn..Style WITH RetLine.Style
          
          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH '_zzzzz'         ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH RetLine.TotQty * RetLine.Gros_Price ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1,;
                      &lclinesAn..llCsSum    WITH .T.

              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
        = oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.ACCOUNT + RETHDR.Invoice,'INVHDR')
        SELECT RETLINE
        =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.CRMEMO)
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        = gfSeek(RETHDR.ACCOUNT + RETHDR.Invoice,'INVHDR')
        SELECT RETLINE
        =gfSEEK(RETHDR.CRMEMO)
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
      
        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON + RETLINE.STYLE)

          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
                  
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
            
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ENDIF
        ELSE
          APPEND BLANK
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
          IF TYPE('lcXMLFileName') = 'C'
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
          lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
          ELSE
            lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
          ENDIF 
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty    ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1

          REPLACE &lclinesAn..Style WITH RetLine.Style
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)


          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

            ELSE
              APPEND BLANK
              
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT  * -1 ,;
                      &lclinesAn..llCsSum    WITH .T.
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)


            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDSCAN


SELECT (lcAlias)

*!*************************************************************
*! Name      : lfBokDet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Book Quntities in Detail.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokDet()
*!*************************************************************
FUNCTION lfBokDet
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


  
  SELECT ORDHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)  
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC" AND IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDHDR.COMPLETE,ldStartComp ,ldEndComp ),.T.)
    SELECT ORDLINE
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(ORDHDR.CORDTYPE + ORDHDR.ORDER)
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
     =gfSEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER)    
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    SCAN REST WHILE CORDTYPE + ORDER =  ORDHDR.CORDTYPE + ORDHDR.ORDER
      SELECT (lclinesAn)
      IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON + ORDLINE.STYLE)
        

        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + (ORDLINE.TOTQTY * ORDLINE.PRICE)  ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
                &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)       ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))


        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)
          

          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount  + (ORDLINE.TOTQTY * ORDLINE.PRICE) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                  &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty  + ORDLINE.TOTQTY                       ,;
                  &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)        ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))


        ENDIF
      ELSE
        APPEND BLANK
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
          lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
        ELSE
          lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
        ENDIF 
        *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                     ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                      ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
                &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
                &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
                &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
                &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
                &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                &lclinesAn..llCsSum    WITH .F.                                  ,;
                &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
                &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
                &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))


        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.
          
            REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
                    &lclinesAn..Amount   WITH &lclinesAn..Amount   + (ORDLINE.TOTQTY * ORDLINE.PRICE)  ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100),;
                    &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
                    &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)         ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            
          ELSE
            APPEND BLANK
            
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                              ,;
                    &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
                    &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
                    &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
                    &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
                    &lclinesAn..llCsSum    WITH .T.                                  ,;
                    &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
                    &lclinesAn..OpnCostD   WITH lnCostVal * ORDLINE.TOTQTY           ,;
                    &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN

*--Section to collect the shipped quantity.
SELECT CUSTOMER


*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
  lnAccNt = 0
  COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
  =oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)
  lcAccount = CUSTOMER.ACCOUNT
  
  
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
     lnPerCent = RECNO()/lnAccNt 
     loProgress.Percent = lnPerCent * 0.9
     loProgress.Description = "Collecting Data For Account:"+lcAccount 
     loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   ENDIF 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  SELECT INVHDR
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)  
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SCAN REST WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv ),BETWEEN(INVHDR.INVDATE,ldStartInv ,ldEndInv ),.T.)

    SELECT INVLINE
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    =oAriaEnvironment.remotetableaccess.SeekRecord(INVHDR.INVOICE)
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      =gfSeek(INVHDR.INVOICE)
    ENDIF  
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
      SELECT (lclinesAn)
      IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON + INVLINE.STYLE)

        REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
                &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100) ,;
                  &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                               ,;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN

*RETHDR.ACCOUNT + RETHDR.INVOICE 

SELECT CUSTOMER


*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

=oAriaEnvironment.remotetableaccess.SeekRecord('M')
lnAccNt = 0
COUNT REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.) TO lnAccNt
=oAriaEnvironment.remotetableaccess.SeekRecord('M')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('M')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

SCAN REST WHILE TYPE + ACCOUNT + STORE ='M' FOR IIF(llAccSele,SEEK(ACCOUNT ,lcAccFile),.T.)

  lcAccount = CUSTOMER.ACCOUNT
  
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

   lnPerCent = RECNO()/lnAccNt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+lcAccount 
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]



  
  SELECT RETHDR
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccount)
  
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfSeek(lcAccount)
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
  SCAN REST WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETHDR.CRDATE,ldStartInv ,ldEndInv ),.T.)

    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
      SELECT RETLINE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.CRMEMO)
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        =gfSeek(RETHDR.CRMEMO)
      ENDIF 
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
      
        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + '_zzzzz' + RETLINE.STYLE)
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
                  
          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        ELSE
          APPEND BLANK
          lcDescSesn = 'Ret. Without Inv.'
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH '_zzzzz'         ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          REPLACE &lclinesAn..Style WITH RetLine.Style

          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH '_zzzzz'         ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1,;
                      &lclinesAn..llCsSum    WITH .T.

              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.ACCOUNT + RETHDR.INVOICE ,'INVHDR')
      SELECT RETLINE
      =oAriaEnvironment.remotetableaccess.SeekRecord(RETHDR.CRMEMO)
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
      ELSE
        =gfSeek(RETHDR.ACCOUNT + RETHDR.INVOICE ,'INVHDR')
        SELECT RETLINE
        =gfSeek(RETHDR.CRMEMO)
      ENDIF
      *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        IF cRet_TrnCD = '4'
          LOOP
        ENDIF

        SELECT (lclinesAn)
        
        IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON + RETLINE.STYLE)
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
            
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        

        ELSE
          APPEND BLANK
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
          IF TYPE('lcXMLFileName') = 'C'
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
            lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
          ELSE
            lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
          ENDIF 
          *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]            
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty    ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1
          REPLACE &lclinesAn..Style    WITH RetLine.Style
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT  * -1 ,;
                      &lclinesAn..llCsSum    WITH .T.
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDSCAN

SELECT (lcAlias)

*--End of lfBokDet.
*!*************************************************************
*! Name      : lfOpnSty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Open Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnSty()
*!*************************************************************
FUNCTION lfOpnSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT Ordline
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
oAriaEnvironment.remotetableaccess.Setorderto('Ordlines')
oAriaEnvironment.remotetableaccess.SeekRecord('')

lnCntOrd = 0
STORE RECCOUNT() TO lnCntOrd
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  gfSetOrder('Ordlines')
  gfSeek('')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*!*  SCAN FOR IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDLINE.COMPLETE,ldStartComp ,ldEndComp ),.T.) AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR') AND ORDHDR.STATUS $ "OH" ; 
*!*     AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
SCAN FOR IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDLINE.COMPLETE,ldStartComp ,ldEndComp ),.T.);
   AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR')) OR gfSeek(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR')) AND ORDHDR.STATUS $ "OH" ; 
   AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Style,'Style','Style')) OR gfSeek(ORDLINE.Style,'Style','Style')) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  IF MOD(RECNO(),CEILING(lnCntOrd/10)) = 0
    lnPerCent = RECNO()/lnCntOrd
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = "Collecting Data For Order:"+Ordline.Order
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  ENDIF   

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
  SELECT (lclinesAn)
  IF SEEK(ORDLINE.STYLE + STR(ORDLINE.GROS_PRICE,9,2))

    REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
    REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                          ,;
            &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE     ,;
            &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
            &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD

  ELSE
    APPEND BLANK
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty) ,;
            &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
            &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)              ,;
            &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
            &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
            &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
            &lclinesAn..PriceSty   WITH ORDLINE.PRICE                  ,;
            &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)

    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
  ENDIF
ENDSCAN

SELECT ORDLINE

SELECT (lcAlias)

*--End of lfOpnSty.
*!*************************************************************
*! Name      : lfInvSty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the invoice Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvSty()
*!*************************************************************
FUNCTION lfInvSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVLINE
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('')
  lnCntOrd = 0
  STORE RECCOUNT() TO lnCntOrd
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  SELECT INVLINE
  =gfSeek('')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]  
*!*  SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv ),BETWEEN(INVLINE.INVDATE,ldStartInv ,ldEndInv ),.T.) AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Invoice,'INVHDR','INVHDR') AND  INVHDR.STATUS <> "V" AND ;
*!*    oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv ),BETWEEN(INVLINE.INVDATE,ldStartInv ,ldEndInv ),.T.) AND;
  ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Invoice,'INVHDR','INVHDR')) OR  gfSeek(INVLINE.Invoice,'INVHDR','INVHDR')) AND  INVHDR.STATUS <> "V" AND ;
  ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Style,'Style','Style')) OR ;
   gfSeek(INVLINE.Style,'Style','Style') ) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
  IF MOD(RECNO(),CEILING(lnCntOrd/10)) = 0
    lnPerCent = RECNO()/lnCntOrd
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = "Collecting Data For Invoice:"+INVLINE.Invoice
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  ENDIF   
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  
  SELECT (lclinesAn)
  IF SEEK(PADR(SUBSTR(INVLINE.STYLE,1,lnStyLnAp),19) + STR(INVLINE.PRICE,9,2))
    REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                          ,;
            &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE     ,;
            &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
            &lclinesAn..Cost    WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
  ELSE
    APPEND BLANK
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    REPLACE &lclinesAn..Style      WITH SUBSTR(INVLINE.STYLE,1,lnStyLnAp)  ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)              ,;
            &lclinesAn..UntSold    WITH INVLINE.TOTQTY                      ,;
            &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE ,;
            &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100) ,;
            &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST  ,;
            &lclinesAn..PriceSty   WITH INVLINE.PRICE                  ,;
            &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)

  ENDIF
ENDSCAN

SELECT INVLINE
SELECT (lcAlias)

*--End of lfInvSty.
*!*************************************************************
*! Name      : lfRetSty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the return Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetSty()
*!*************************************************************
FUNCTION lfRetSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT RETLINE

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
=oAriaEnvironment.remotetableaccess.SeekRecord('')

lnCntOrd = 0
STORE RECCOUNT() TO lnCntOrd
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*!*  SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETLINE.CRDATE,ldStartInv ,ldEndInv ),.T.)  AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.crmemo,'RetHdr','RetHdr') AND RetHdr.Status <> 'V';
*!*      AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*!*    =oAriaEnvironment.remotetableaccess.SeekRecord(Rethdr.Invoice,'INVHDR','INVHDR')
SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETLINE.CRDATE,ldStartInv ,ldEndInv ),.T.)  AND ;
    ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.crmemo,'RetHdr','RetHdr')) OR gfSeek(RETLINE.crmemo,'RetHdr','RetHdr')) AND RetHdr.Status <> 'V';
    AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.Style,'Style','Style')) OR gfSeek(RETLINE.Style,'Style','Style')) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
     =oAriaEnvironment.remotetableaccess.SeekRecord(Rethdr.Invoice,'INVHDR','INVHDR') 
   ELSE 
     =gfSeek(Rethdr.Invoice,'INVHDR','INVHDR')
   ENDIF   
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]  
  
  
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  IF MOD(RECNO(),CEILING(lnCntOrd/10)) = 0
    lnPerCent = RECNO()/lnCntOrd
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = "Collecting Data For Credit Memo:"+RETLINE.crmemo
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  ENDIF   
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  
  
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF


  SELECT (lclinesAn)

  IF SEEK(PADR(SUBSTR(RETLINE.STYLE,1,lnStyLnAp),19) + STR(RETLINE.GROS_PRICE,9,2))  
    REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
            &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold-&lclinesAn..UntRtrn=0,1,&lclinesAn..UntSold-&lclinesAn..UntRtrn)

  ELSE
    APPEND BLANK
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    
    REPLACE &lclinesAn..Style      WITH SUBSTR(RETLINE.STYLE,1,lnStyLnAp)  ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH IIF(EMPTY(RetLine.Desc),LEFT(Style.Desc1,30),LEFT(RETLINE.DESC,30)),;
            &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
            &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
            &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 * -1,;
            &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold-&lclinesAn..UntRtrn=0,1,&lclinesAn..UntSold-&lclinesAn..UntRtrn)

  ENDIF
ENDSCAN

SELECT RETLINE
SELECT (lcAlias)

*--End of lfRetSty.
*!*************************************************************
*! Name      : lfBokSty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to collect the Book Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokSty()
*!*************************************************************
FUNCTION lfBokSty
PRIVATE lcAlias

lcAlias = ALIAS()

*--Section of making the relations.
SELECT ORDLINE
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.remotetableaccess.SeekRecord('')
  lnCntOrd = 0
  STORE RECCOUNT() TO lnCntOrd
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  gfSeek('')  
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*!*  SCAN FOR IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDLINE.COMPLETE,ldStartComp ,ldEndComp ),.T.) AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR') AND ORDHDR.STATUS $ "OH" ; 
*!*     AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
SCAN FOR IIF(!EMPTY(ldStartComp) AND !EMPTY(ldEndComp),BETWEEN(ORDLINE.COMPLETE,ldStartComp ,ldEndComp ),.T.) AND;
   ((TYPE('lcXMLFileName') = 'C' AND;
    oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR')) OR;
    gfSeek(ORDLINE.Cordtype+ORDLINE.Order,'ORDHDR','ORDHDR')) AND ORDHDR.STATUS $ "OH" ; 
   AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.Style,'Style','Style')) OR;
   gfSeek(ORDLINE.Style,'Style','Style')) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    IF MOD(RECNO(),CEILING(lnCntOrd/10)) = 0
      lnPerCent = RECNO()/lnCntOrd
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description = "Collecting Data For Order:"+ORDLINE.Order
      loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF   
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

  SELECT (lclinesAn)
  IF SEEK(ORDLINE.STYLE + STR(ORDLINE.GROS_PRICE,9,2))
    REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                        ,;
            &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDLINE.TOTQTY * ORDLINE.PRICE   ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
            &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                        ,;
            &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)        ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))


    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
  ELSE
    APPEND BLANK
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      lcDescSesn = oAriaEnvironment.codes.getcodedescription(ORDHDR.SEASON,'SEASON')
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
   ELSE
     lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
   ENDIF 
   *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    REPLACE &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
            &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
            &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)               ,;
            &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
            &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
            &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
            &lclinesAn..PriceSty   WITH ORDLINE.PRICE                   ,;
            &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
            &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
            &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))


    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
  ENDIF
ENDSCAN


SELECT INVLINE
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
 =gfSeek('')
ENDIF 
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]


*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*!*  SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv ),BETWEEN(INVLINE.INVDATE,ldStartInv ,ldEndInv ),.T.) AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Invoice,'INVHDR','INVHDR') AND  INVHDR.STATUS <> "V" AND ;
*!*    oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv ),BETWEEN(INVLINE.INVDATE,ldStartInv ,ldEndInv ),.T.) AND;
     ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Invoice,'INVHDR','INVHDR')) ;
     OR gfSeek(INVLINE.Invoice,'INVHDR','INVHDR')) AND  INVHDR.STATUS <> "V" AND ;
    ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(INVLINE.Style,'Style','Style')) OR;
     gfSeek(INVLINE.Style,'Style','Style')) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  SELECT (lclinesAn)
  IF SEEK(INVLINE.STYLE + STR(INVLINE.GROS_PRICE,9,2))   
  
    REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
            &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
            &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
            &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE                                  ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))            

  ELSE
    APPEND BLANK
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
    REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                        ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                        ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
            &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)               ,;
            &lclinesAn..UntSold    WITH INVLINE.TOTQTY                       ,;
            &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE       ,;
            &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..PriceSty   WITH INVLINE.PRICE                   ,;
            &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                       ,;
            &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + (INVLINE.COST * INVLINE.TOTQTY)        ,;
            &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
  ENDIF
ENDSCAN

SELECT RETLINE
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
 *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('')
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
ELSE
  =gfSeek('')
ENDIF
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]

*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
*!*  SCAN FOR IIF(!EMPTY(ldStartInv ) AND !EMPTY(ldEndInv ),BETWEEN(RETLINE.CRDATE,ldStartInv ,ldEndInv ),.T.)  AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.crmemo,'RetHdr','RetHdr') AND RetHdr.Status <> 'V';
*!*      AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.Style,'Style','Style') AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
*!*    =oAriaEnvironment.remotetableaccess.SeekRecord(Rethdr.Invoice,'INVHDR','INVHDR')
SCAN FOR IIF(!EMPTY(ldStartInv) AND !EMPTY(ldEndInv),BETWEEN(RETLINE.CRDATE,ldStartInv ,ldEndInv ),.T.) ;
    AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.crmemo,'RetHdr','RetHdr')) OR gfSeek(RETLINE.crmemo,'RetHdr','RetHdr')) AND RetHdr.Status <> 'V';
    AND ((TYPE('lcXMLFileName') = 'C' AND oAriaEnvironment.remotetableaccess.SeekRecord(RETLINE.Style,'Style','Style')) OR gfSeek(RETLINE.Style,'Style','Style')) AND IIF(llSelectStyle ,SEEK(Style.cStyMajor,lcStyFile),.T.)
  IF TYPE('lcXMLFileName') = 'C'  
    =oAriaEnvironment.remotetableaccess.SeekRecord(Rethdr.Invoice,'INVHDR','INVHDR')
  ELSE
    =gfSeek(Rethdr.Invoice,'INVHDR','INVHDR')
  ENDIF   
*: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF
  SELECT (lclinesAn)
  IF SEEK(RETLINE.STYLE + STR(RETLINE.GROS_PRICE,9,2))

    REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
            &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..ShpQty WITH &lclinesAn..ShpQty - RetLine.TOTQTY                      ,;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
  ELSE
    APPEND BLANK
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[End]
      lcDescSesn = oAriaEnvironment.codes.getcodedescription(INVHDR.SEASON,'SEASON')
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    ENDIF 
    *: C201040,1 MMT 09/06/2009 call request builder fxp to collect data[Start]
    
    REPLACE &lclinesAn..Style      WITH RETLINE.STYLE                       ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH IIF(EMPTY(RetLine.Desc),LEFT(Style.Desc1,30),LEFT(RETLINE.DESC,30)) ,;
            &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
            &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
            &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 * -1,;
            &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty - RetLine.TOTQTY ,;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
  
  ENDIF
ENDSCAN

SELECT RETLINE
SET RELATION TO

SELECT (lcAlias)

*--End of lfBokSty.

