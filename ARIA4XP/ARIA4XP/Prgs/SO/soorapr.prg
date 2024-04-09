*!*****************************************************************************************
*! Name      : SOORAPR.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2005 
*! Purpose   : Sales order Approval Screen
*! Entry no. : N037513 - Sales order Approval Screen
*******************************************************************************************
*Modifications:
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111]
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111]
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032]
*!*****************************************************************************************
#INCLUDE R:\ARIA4XP\SCREENS\SO\SOORAPR.H


DO FORM (oAriaApplication.ScreenHome+"\SO\SOORAPR.SCX") 

RETURN


*Defining the Business class 
DEFINE CLASS Order_Approval AS Custom 
  lcTempCust = ""
  loFormSet = .F.
  loForm = .F.
  llEnableOrdDet =.F.
  llEnableOrdRea =.F. 
  llEnableOrdStat =.F. 
  llAppEna =.F. 
  llRecEna =.F. 
  llSelectEna  =.F. 
  llSelectnOneEna =.F. 
  llSelectAllEna =.F. 
  llSelectIvertEna =.F. 
  lcSelectCap = ""
  lcSycIntInfo = ""
  loOrdHdrSec = ""
  lcCodes = ""
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  lcOrders = ""
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
  lcScopeCustomer = ""
  lcScopeOrder = ""
  llFormScope = .F.
  lcApprovalNo = ''
  *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2005 
*! Purpose   : init function of th from 
*!*************************************************************
*!
FUNCTION lfInit
  LPARAMETERS loFrm
  SET MULTILOCKS ON
  This.loForm    = loFrm
  This.loFormSet = loFrm.Parent
  This.lcCodes   = gfTempName()
  This.lfopenfile('Customer','Customer')
  This.lfopenfile('OrdHdr','OrdHdr')
  This.lfopenfile('Debit','Debit')
  This.lfopenfile('Credit','Credit')
  *--Codes File problem 
  This.lfopenfile('Codes','Codes')
  IF !THIS.Codes.llNative
    lcCodeSql = "SELECT cDefCode, cFld_Name, cCode_No, cDiscrep, cRltd_Nam, cRltField, cRltd_Vlu "+ ;
            " FROM Codes (INDEX=CCODE_NO) " + ;
            " WHERE cDefCode = 'N' AND (cFld_Name = 'SEASON' OR cFld_Name = 'TRANCODE')"
    THIS.Codes.Sqlrun(lcCodeSql,This.lcCodes,.T.)            
    lnBuffering = CURSORGETPROP("Buffering",This.lcCodes)
    =CURSORSETPROP("Buffering",3,This.lcCodes)
    SELECT(This.lcCodes)
    INDEX on  CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM TAG CCODE_NO
    SET ORDER TO CCODE_NO
  ENDIF 
*--
WITH This.loFormSet
  IF !This.Customer.llNative
    .cbrowsetabledbengine   = "SQL"
  ELSE
    .cbrowsetabledbengine   = 'NATIVE'  
  ENDIF 
  .DataEnvironment.InitialSelectedAlias = 'Customer'
ENDWITH 
*--


  *Open the ordhdr file with another alias to be used in calculating the avialable credit value
  This.loOrdHdrSec = CREATEOBJECT("RemoteTable",'OrdHdr','OrdHdr','OrdHdr_x',This.loFormSet.DataSessionID)

	*!*	*C102556,1 (Begin) Is this Scope Greek.
	*!*	*--llScopeGrk IS USED IN SCREEN
	*!*	llScopeGrk = (ASCAN(This.loFormSet.laEvntTrig, PADR('HOLDRES',10)) <> 0)
	*!*	STORE .F. TO llEanabled,llPopMover,llClearReas
	*!*	*C102556,1 (End)
	
This.lcTempCust = gfTempName()
This.lcSycIntInfo = gfTempName()

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
THIS.lcOrders = gfTempName()	
SELECT Ordhdr
DIMENSION laOrdStruct[1]
AFIELDS(laOrdStruct)
=gfCrtTmp(THIS.lcOrders,@laOrdStruct,"ACCOUNT+ORDER",THIS.lcOrders,.T.)
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
SELECT(THIS.lcOrders)
INDEX ON ORDER+ACCOUNT TAG 'ORDACCT' ADDITIVE
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
This.lcScopeCustomer = gfTempName()
This.lcScopeOrder = gfTempName()
=gfCrtTmp(THIS.lcScopeOrder ,@laOrdStruct,"ACCOUNT+ORDER",THIS.lcScopeOrder ,.T.)
DIMENSION laScpCustomer[1,4]
laScpCustomer[1,1] = "Account"
laScpCustomer[1,2] = 'C'
laScpCustomer[1,3] = 5
laScpCustomer[1,4] = 0
=gfCrtTmp(THIS.lcScopeCustomer ,@laScpCustomer,"ACCOUNT",THIS.lcScopeCustomer ,.T.)
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]
*--Array to make the temp file which will be used in hold customers
DIMENSION laFileStru[9, 18]

laFileStru[1 ,1] = 'ACCOUNT'
laFileStru[1 ,2] = 'C'
laFileStru[1 ,3] = 5
laFileStru[1,4] = 0

laFileStru[2,1] = 'Btname'
laFileStru[2,2] = 'c'
laFileStru[2,3] = 30
laFileStru[2,4] = 0

laFileStru[3,1] = 'STATUS'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 1
laFileStru[3,4] = 0

laFileStru[4,1] = 'CRLIMIT'
laFileStru[4,2] = 'N'
laFileStru[4,3] = 11
laFileStru[4,4] = 0
  
laFileStru[5,1] = 'CRAVAIL'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 12
laFileStru[5,4] = 2


laFileStru[6,1] = 'NETBAL'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 14
laFileStru[6,4] = 2
  

laFileStru[7,1] = 'CFACCODE'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 6
laFileStru[7,4] = 0
  
laFileStru[8,1] = 'FACTACCT'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 10
laFileStru[8,4] = 0
  

laFileStru[9,1] = 'LLSEL'
laFileStru[9,2] = 'L'
laFileStru[9,3] = 1
laFileStru[9,4] = 0
  
FOR lnLoop = 1 TO  9
  STORE ' ' TO  laFileStru[lnLoop,7],laFileStru[lnLoop,8],;
                laFileStru[lnLoop,9],laFileStru[lnLoop,10],;
                laFileStru[lnLoop,11],laFileStru[lnLoop,12],;
                laFileStru[lnLoop,13],laFileStru[lnLoop,14],;
                laFileStru[lnLoop,15],laFileStru[lnLoop,16]
  STORE 0 TO    laFileStru[lnLoop,17] ,laFileStru[lnLoop,18]
ENDFOR   
  
=gfCrtTmp(THIS.lcTempCust,@laFileStru,"ACCOUNT",'ACCOUNT',.T.)


* This.Customer.SqlRun("SELECT ACCOUNT,BTNAME,STATUS,CRLIMIT,CRAVAIL,NETBAL,CFACCODE,FACTACCT,LHASNOTES AS LLSEL FROM CUSTOMER WHERE TYPE = 'M' ORDER BY ACCOUNT",THIS.lcTempCust)
   This.Customer.Seek('M')
   SELECT CUSTOMER
*   LOCATE 
   M.LLSEL = .F.
   SCAN REST WHILE TYPE+ACCOUNT+STORE = 'M'
	   SCATTER MEMO MEMVAR 
	   INSERT INTO (THIS.lcTempCust) FROM MEMVAR  
   ENDSCAN 

*!*	INSERT INTO (THIS.lcTempCust)  SELECT ACCOUNT,BTNAME,STATUS,CRLIMIT,CRAVAIL,NETBAL,CFACCODE,FACTACCT,LHASNOTES AS LLSEL FROM CUSTOMER WHERE type ="M"
*!*	SELECT(THIS.lcTempCust)  
*!*	REPLACE ALL LLSEL WITH .F.
*!*	   lnBuffering = CURSORGETPROP("Buffering",This.lcTempCust)
*!*	   =CURSORSETPROP("Buffering",3,This.lcTempCust)
*!*	   SELECT(This.lcTempCust)
*!*	   INDEX on ACCOUNT TAG ACCOUNT
*!*	   SET ORDER TO ACCOUNT 

SELECT(This.lcTempCust)
LOCATE 
IF !EOF()
  This.llSelectEna  =.T. 
  This.llSelectnOneEna =.F. 
  This.llSelectAllEna =.T. 
  This.llSelectIvertEna =.T. 
  This.llAppEna =.F. 
  This.llRecEna =.F. 
ELSE
  This.llSelectEna  =.F. 
  This.llSelectnOneEna =.F. 
  This.llSelectAllEna =.F. 
  This.llSelectIvertEna =.F. 
  This.llAppEna =.F. 
  This.llRecEna =.F. 
ENDIF 
ENDFUNC 
*!*****************************************************************
*! Name      : lfvCustomer
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2005 
*! Purpose   : open Customer screen
*!*****************************************************************
FUNCTION lfvCustomer
lcExeKeyValue = "'"+CUSTOMER.ACCOUNT+"',''"
oAriaApplication.DoProgram("AWRARCUST",lcExeKeyValue,.F.,'')
*!*************************************************************
*! Name      : lfOrders
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2005 
*! Purpose   : Inquire Customer Orders
*!*************************************************************
FUNCTION lfOrders
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*IF !EOF('ORDHDR')
IF !EOF(THIS.lcOrders)
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  *lcOrder = "'"+Ordhdr.cOrdType+"','"+Ordhdr+'.ORDER'+"'"
  lcOrder = "'"+EVALUATE(THIS.lcOrders+'.cOrdType')+"','"+EVALUATE(THIS.lcOrders+'.ORDER')+"'"
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  oAriaApplication.DoProgram("AWRSOORD", lcOrder,.F.,'SO')
ENDIF  
*!*****************************************************************************************
*! Name      : lfOpenFile
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/02/2005 
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile, lcTag
LOCAL lcProp
  lcFile = JUSTSTEM(lcFile)
  lcTag = JUSTSTEM(lcTag)
  lcProp = lcFile
  IF !PEMSTATUS(This,lcProp,5)
    This.addproperty(lcProp)
  ENDIF
  lcProp = 'This.'+lcProp

  IF TYPE(lcProp)<>'O'
    &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,This.loFormSet.DataSessionID)
  ELSE
    &lcProp..SetOrder(lcTag)
  ENDIF
ENDFUNC 
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2005 
*! Purpose   : function to control source
*!*************************************************************
FUNCTION lfAddControlSource
  WITH This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = This.lcTempCust
    .cSeekIndex= 'ACCOUNT'
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl	 = "AriaCheckBox1"
    .column1.ControlSource 	 ='ThisFormSet.loBusObj.lfgetValueLogic()'          
    .column2.ControlSource   = This.lcTempCust+ '.ACCOUNT'
    .Column2.CurrentControl	 = "gridcolumn1"
    .column3.ControlSource 	 = This.lcTempCust+ '.BTNAME'
    .Column3.CurrentControl	 = "gridcolumn1"
    .column4.ControlSource   = 'ThisFormSet.loBusObj.lfgetValueStatus()'
    .Column4.CurrentControl	 = "gridcolumn1"
    .column5.ControlSource   = This.lcTempCust+ '.NETBAL'
    .Column5.CurrentControl	 = "gridcolumn1"
    .column6.ControlSource   = This.lcTempCust+ '.CRLIMIT'
    .Column6.CurrentControl	 = "gridcolumn1"
    .column7.ControlSource   = This.lcTempCust+ '.CrAvail'
    .Column7.CurrentControl	 = "gridcolumn1"
    .column8.ControlSource   = This.lcTempCust+ '.cFacCode'
    .Column8.CurrentControl	 = "gridcolumn1"
    .column9.ControlSource   = This.lcTempCust+ '.FactAcct'
    .Column9.CurrentControl	 = "gridcolumn1"
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T. 
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
    .AfterRowColChange ()

  ENDWITH 
   
    WITH This.loFormSet.ariaform1.pgfOrders.pgOrders.grdOrders
    .RecordSource = ""
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    *.RecordSource = 'Ordhdr'
    .RecordSource = THIS.lcOrders
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    
    .columnCount = 8
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    *.cSeekIndex= 'ORDACCT'
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
    *.cSeekIndex= THIS.lcOrders
    .cSeekIndex=  'ORDACCT'
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    
    .columns(1).header1.caption = LANG_Order
    .columns(2).header1.caption = LANG_Status
    .columns(3).header1.caption = LANG_Season
    .columns(4).header1.caption = LANG_Factor
    .columns(5).header1.caption = LANG_Complete
    .columns(6).header1.caption = LANG_OpenAmnt
    .columns(7).header1.caption = LANG_AprNum
    .columns(8).header1.caption = LANG_AppAmnt
    
    .columns(1).Width = 75
    .columns(2).Width = 75
    .columns(3).Width = 139
    .columns(4).Width = 75
    .columns(5).Width = 75
    .columns(6).Width = 75
    .columns(7).Width = 75
    .columns(8).Width = 75
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(1).ControlSource = THIS.lcOrders+'.ORDER'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(1).CurrentControl	 = "gridcolumn1"
    .Columns(1).gridcolumn1.Visible = .T.
    .Columns(1).ReadOnly = .T.

    .columns(2).ControlSource = 'Thisformset.loBusObj.lfGetValueOrdStat()'
    .Columns(2).CurrentControl	 = "gridcolumn1"    
    .Columns(2).gridcolumn1.Visible = .T.
    .Columns(2).ReadOnly = .T.

    .columns(3).ControlSource = 'Thisformset.loBusObj.lfGetSeason()'
    .Columns(3).CurrentControl	 = "gridcolumn1"    
    .Columns(3).gridcolumn1.Visible = .T.
    .Columns(3).ReadOnly = .T.
	*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(4).ControlSource = THIS.lcOrders+'.cFacCode'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(4).CurrentControl	 = "gridcolumn1"
    .Columns(4).gridcolumn1.Visible = .T.
    .Columns(4).ReadOnly = .T.
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(5).ControlSource = THIS.lcOrders+'.COMPLETE'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(5).CurrentControl	 = "gridcolumn1"
    .Columns(5).gridcolumn1.Visible = .T.
    .Columns(5).ReadOnly = .T.

	*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(6).ControlSource = THIS.lcOrders+'.OpenAmt'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(6).CurrentControl	 = "gridcolumn1"    
    .Columns(6).gridcolumn1.Visible = .T.
    .Columns(6).ReadOnly = .T.
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(7).ControlSource = THIS.lcOrders+'.Approval'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(7).CurrentControl	 = "gridcolumn1"    
    .Columns(7).gridcolumn1.Visible = .T.
    .Columns(7).ReadOnly = .T.
    
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    .columns(8).ControlSource = THIS.lcOrders+'.ApprAmt'
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
    .Columns(8).CurrentControl	 = "gridcolumn1"
    .Columns(8).gridcolumn1.Visible = .T.
    .Columns(8).ReadOnly = .T.

    .SETALL('ReadOnly',.T.,'COLUMN')
    .Enabled = .T. 
    .refresh()
    .AfterRowColChange ()           
  ENDWITH 
ENDFUNC 
*!*************************************************************
*! Name      : lfGetValueOrdStat
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/28/2005
*! Purpose   : Function to fill check box in grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*!
FUNCTION lfGetValueOrdStat
  PRIVATE lcRetValStat
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  lcRetValStat = IIF(EVALUATE(THIS.lcOrders+'.Status')='O','Open','Hold')
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  RETURN lcRetValStat
ENDFUNC 
*!*************************************************************
*! Name      : lfgetValueStatus
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/28/2005
*! Purpose   : Function to status field in grid
*!*************************************************************
FUNCTION lfgetValueStatus
  PRIVATE lcRetVal
  lcStatus = EVALUATE(This.lcTempCust+ '.Status')
  lcRetVal = IIF(lcStatus ='P','Potential',IIF(lcStatus='A','Active',IIF(lcStatus='H','Hold','Cancelled')))
  RETURN lcRetVal
ENDFUNC
*!*************************************************************
*! Name      : lfAfterRowCol
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/16/2005
*! Purpose   : Function to status field in grid
*!*************************************************************
FUNCTION lfAfterRowCol
  =THIS.CUSTOMER.SEEK('M'+EVALUATE(This.lcTempCust+'.Account'))

  This.loFormSet.ariaform1.kbAccounts.keytextbox.Value = Customer.Account
  This.loFormSet.ariaform1.txtAccDesc.Value = Customer.btName
  This.loFormSet.ariaform1.txtAddr1.Value  = Customer.cAddress32 
  This.loFormSet.ariaform1.txtAdd2.Value =   Customer.cAddress42
  lcCustomerCode = Customer.cCont_Cod2
  lcSqlCommand="SELECT SycInt.cPart3Lab,SycInt.cPart4Lab FROM SycInt WHERE SycInt.CCONT_CODE = '"+lcCustomerCode +"'"
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",This.lcSycIntInfo ,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult = 1
	lcSycInt = This.lcSycIntInfo 
    This.loFormSet.ariaform1.lblAddPar1.Caption = &lcSycInt..cPart3Lab
    This.loFormSet.ariaform1.lblAddPar2.Caption = &lcSycInt..cPart4Lab
  ENDIF   
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  *This.lfGetOrders() 
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
ENDFUNC 
*!*************************************************************
*! Name      : lfGetOrders
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/16/2005
*! Purpose   : Function to status field in grid
*!*************************************************************
FUNCTION lfGetOrders
  * WAIT WINDOW "Collecting data for account " + Customer.Account + " ...!" NOWAIT
  
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  SELECT(THIS.lcOrders)
  ZAP 
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
  lcAccount = Customer.Account
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
*!*	  This.lfopenfile('OrdHdr','Ordbulk')
*!*	  This.OrdHdr.SEEK( Customer.Account +'O')
*!*	  SELECT ORDHDR 
*!*	  SUM REST OpenAmt TO   This.loFormSet.ariaform1.pgfOrders.pgOrders.txtOpenOrd.Value ;
*!*	  WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account +'O' FOR cOrdType='O'
*!*	  
*!*	  This.OrdHdr.SEEK(Customer.Account+'H')
*!*	  SELECT ORDHDR
*!*	  SUM REST OpenAmt TO   This.loFormSet.ariaform1.pgfOrders.pgOrders.txtHoldOrd.Value ;
*!*	  WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account +'H' FOR cOrdType='O'
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]  
  
  This.lfopenfile('OrdHdr','Ordacct')
  SELECT ORDHDR
  This.ordhdr.Seek(Customer.Account+'O') 
  
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  STORE 0 TO lnTotHold,lnTotOpen
  SELECT ORDHDR
  SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = Customer.Account+'O' FOR INLIST(Status,'H','O')
    *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
    IF This.llFormScope AND !SEEK(Ordhdr.Account+Ordhdr.Order,This.lcScopeOrder)
      LOOP 
    ENDIF
    *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]

    SCATTER MEMO MEMVAR  
    IF m.Status = 'H'
     lnTotHold = lnTotHold + m.OpenAmt
    ELSE
     lnTotOpen = lnTotOpen + m.OpenAmt
    ENDIF 
    INSERT INTO (THIS.lcOrders) FROM MEMVAR  
  ENDSCAN 
  *  This.lfaddOrderControl()
  SELECT(THIS.lcOrders)
  LOCATE 
  WITH This.loFormSet.ariaform1.pgfOrders.pgOrders
    .txtOpenOrd.Value = lnTotOpen
    .txtHoldOrd.Value = lnTotHold
    .grdOrders.AfterRowColChange ()
    .grdOrders.Refresh()
  ENDWITH
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

 ENDFUNC  

*!*************************************************************
*! Name      : lfGetSeason
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/16/2005
*! Purpose   : Function to get season desc.
*!*************************************************************
FUNCTION lfGetSeason
  PRIVATE lcRetSeason
  *lcRetSeason = IIF(SEEK('N'+'SEASON    '+ORDHDR.SEASON,This.lcCodes),EVALUATE(This.lcCodes+'.CDISCREP'),"")
  IF !THIS.Codes.llNative
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    lcRetSeason = IIF(SEEK('N'+'SEASON    '+EVALUATE(THIS.lcOrders+'.SEASON'),This.lcCodes),EVALUATE(This.lcCodes+'.CDISCREP'),"")
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  ELSE 
    lcRetSeason =gfCodDes(EVALUATE(THIS.lcOrders+'.SEASON'),'SEASON')
  ENDIF 
  RETURN lcRetSeason 
ENDFUNC 

*!*************************************************************
*! Name      : lfAfterOrderChng
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/16/2005
*! Purpose   : Function to refresh order grid
*!*************************************************************
FUNCTION lfAfterOrderChng
lcCurrAlias = ALIAS()
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*IF EOF('OrdHdr') 
IF EOF(THIS.lcOrders) 
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
  This.llEnableOrdDet =.F.
  This.llEnableOrdRea =.F. 
  This.llEnableOrdStat =.F. 
  This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value  = 0
  This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppNum.Value  = ''
  This.loFormSet.ariaform1.pgfOrders.pgOrders.cboStatus.Value = ''
ENDIF 

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value  = EVALUATE(THIS.lcOrders+'.ApprAmt')
This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppNum.Value  = EVALUATE(THIS.lcOrders+'.Approval')
This.loFormSet.ariaform1.pgfOrders.pgOrders.cboStatus.Value = IIF(EVALUATE(THIS.lcOrders+'.Status')='O',1,2)
*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
This.loFormSet.lnApprAmnt = EVALUATE(THIS.lcOrders+'.ApprAmt')
This.loFormSet.lnApprNum  = EVALUATE(THIS.lcOrders+'.Approval')
*This.loFormSet.llStatus   = IIF(EVALUATE(THIS.lcOrders+'.Status')='O',1,2)
*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]
IF EVALUATE(THIS.lcOrders+'.Status') = 'O'
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

  This.llEnableOrdStat = .T.
  This.llEnableOrdDet = .T.
  This.llEnableOrdRea = .F.
  This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value ="      "
  
  *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
  This.loFormSet.lcReason = "      "
  This.loFormSet.AriaForm1.pgfOrders.pgOrders.cmdOk.Enabled     = lfApprChanged(This.loFormSet)
  This.loFormSet.AriaForm1.pgfOrders.pgOrders.cmdCancel.Enabled = lfApprChanged(This.loFormSet)
  *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]

*!*	  *C102556,1 (Begin) Is this Scope Greek.
*!*	  IF llScopeGrk
*!*	    STORE .F. TO llEanabled,llPopMover,llClearReas
*!*	    llReturn = gfDoTriger('SOORAPR',PADR('HOLDRES',10))
*!*	  ENDIF
*!*	  *C102556,1 (End)
ENDIF

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
IF EVALUATE(THIS.lcOrders+'.Status') = 'H'
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

  This.llEnableOrdDet = .F.
  This.llEnableOrdRea = .T.
  This.llEnableOrdStat = .T.
  
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value = EVALUATE(THIS.lcOrders+'.DECL_CODE')
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
  *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
  This.loFormSet.lcReason = EVALUATE(THIS.lcOrders+'.DECL_CODE')
  This.loFormSet.AriaForm1.pgfOrders.pgOrders.cmdOk.Enabled     = lfApprChanged(This.loFormSet)
  This.loFormSet.AriaForm1.pgfOrders.pgOrders.cmdCancel.Enabled = lfApprChanged(This.loFormSet)
  *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]
  
  *C102556,1 (Begin) Is this Scope Greek.
*!*	  IF llScopeGrk
*!*	    STORE .F. TO llPopMover,llClearReas
*!*	    llEanabled = .T.
*!*	    llReturn = gfDoTriger('SOORAPR',PADR('HOLDRES',10))
*!*	  ENDIF
  *C102556,1 (End)
ENDIF

SELECT (lcCurrAlias)
ENDFUNC 
*!*************************************************************
*! Name      : lfvStatus
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/17/2005
*! Purpose   : Approve/Decline order
*!*************************************************************
*! Calls     : gfGetExSin,gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvStatus()
*!*************************************************************
FUNCTION lfvStatus
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
LPARAMETERS llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]

PRIVATE lcUntSin, lcExRSin,lcStatus 

lnStatus = This.loFormSet.ariaform1.pgfOrders.pgOrders.cboStatus.Value

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF !llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[END]
IF (lnStatus = 1 .AND. EVALUATE(THIS.lcOrders+'.STATUS')= 'O') .OR. (lnStatus = 2 .AND. EVALUATE(THIS.lcOrders+'.STATUS')='H')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  RETURN
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
IF (lnStatus = 1 .AND. EVALUATE(THIS.lcOrders+'.STATUS')='H').AND. (CUSTOMER.STATUS $'HX')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  This.loFormSet.ariaform1.pgfOrders.pgOrders.cboStatus.Value = 2
  lnStatus = 2
  lcStatus=IIF (CUSTOMER.STATUS = 'H','Hold','Cancelled')
  
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  = gfModalGen("TRM32083",'DIALOG', EVALUATE(THIS.lcOrders+'.ACCOUNT')+"|"+lcStatus)
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
  This.llEnableOrdStat = .T.
  RETURN
ENDIF

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
This.Ordhdr.Seek(Customer.ACCOUNT + 'O' + EVALUATE(THIS.lcOrders+'.Order'),	'ORDACCT')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

lcUntSin = ' '
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
lcExRSin = gfGetExSin(@lcUntSin,EVALUATE(THIS.lcOrders+'.cCurrCode'))
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

IF lnStatus = 1
  

  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
  *lnOpen = ORDHDR.OPENAMT &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
  lnOpen = EVALUATE(THIS.lcOrders+'.OPENAMT') &lcExRSin EVALUATE(THIS.lcOrders+'.nExRate') &lcUntSin EVALUATE(THIS.lcOrders+'.nCurrUnit')
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  IF llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    SELECT Customer
    *REPLACE CrAvail WITH CrAvail - lnOpen
    THIS.CUSTOMER.REPLACE('CrAvail WITH CrAvail - lnOpen')
    REPLACE CrAvail WITH CrAvail - lnOpen
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ELSE
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    SELECT(This.lcTempCust)
    REPLACE CrAvail WITH CrAvail - lnOpen
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    SELECT(THIS.lcOrders)
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
	*  SELECT ORDHDR
	*!*	  REPLACE 	STATUS     WITH "O"           ,;
	*!*				DECL_CODE  WITH SPACE(2)      ,;
	*!*				APPRAMT    WITH CEILING(OPENAMT)
	*!*				
	*!*	  This.ORDHDR.REPLACE('STATUS     WITH "O"           ,;
	*!*					       DECL_CODE  WITH SPACE(2)      ,;
	*!*				           APPRAMT    WITH CEILING(OPENAMT)')

	  
	  REPLACE 	STATUS     WITH "O"           ,;
				DECL_CODE  WITH SPACE(2)      ,;
				APPRAMT    WITH OPENAMT
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  IF llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    This.ORDHDR.REPLACE('STATUS     WITH "O"           ,;
	  			         DECL_CODE  WITH SPACE(2)      ,;
		  	             APPRAMT    WITH OPENAMT')
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  IF !llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End] 
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value  = EVALUATE(THIS.lcOrders+'.APPRAMT')
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
    This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value = "      "
  
    This.llEnableOrdDet = .T.
    This.llEnableOrdRea = .F.

*!*	  *C102556,1 (Begin) Is this Scope Greek.
*!*	  IF llScopeGrk
*!*	    STORE .F. TO llEanabled,llPopMover
*!*	    llClearReas = .T.
*!*	    llReturn = gfDoTriger('SOORAPR',PADR('HOLDRES',10))
*!*	  ENDIF
*!*	  *C102556,1 (End)

  
  *C200089,1 Reham On 07/22/1999  *** Begin ***
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
*!*	  IF ASCAN(laEvntTrig , PADR('APR_OPN',10)) <> 0
*!*	    llReturn = gfDoTriger('SOORAPR',PADR('APR_OPN',10))
*!*	  ENDIF
  *C200089,1 Reham On 07/22/1999  *** End   ***
  
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtOpenOrd.Value = This.loFormSet.ariaform1.pgfOrders.pgOrders.txtOpenOrd.Value + lnOpen
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtHoldOrd.Value  = This.loFormSet.ariaform1.pgfOrders.pgOrders.txtHoldOrd.Value - lnOpen          
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
ELSE
  *C102556,1 (Begin) If change from Open To Hold.
*!*	  IF llScopeGrk
*!*	    *--Enable Reasons Button and Display Hold Reasons Mover.
*!*	    STORE .T. TO llEanabled,llPopMover
*!*	    llClearReas = .F.
*!*	    IF !gfDoTriger('SOORAPR',PADR('HOLDRES',10))
*!*	      RETURN
*!*	    ENDIF
*!*	  ENDIF
  *C102556,1 (End)
  
  SELECT Customer

  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[START]
  *  lnOpen = OrdHdr.ApprAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
  lnOpen = EVALUATE(THIS.lcOrders+'.OPENAMT') &lcExRSin EVALUATE(THIS.lcOrders+'.nExRate') &lcUntSin EVALUATE(THIS.lcOrders+'.nCurrUnit')
  lnAppOpen = EVALUATE(THIS.lcOrders+'.ApprAmt') &lcExRSin EVALUATE(THIS.lcOrders+'.nExRate') &lcUntSin EVALUATE(THIS.lcOrders+'.nCurrUnit')
  *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
*!*	  Replace  CrAvail WITH CrAvail + lnOpen
*!*	  This.Customer.Replace('CrAvail WITH CrAvail + lnOpen')
*  Replace  CrAvail WITH CrAvail + lnAppOpen
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  IF llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    This.Customer.Replace('CrAvail WITH CrAvail + lnAppOpen')
    Replace  CrAvail WITH CrAvail + lnAppOpen
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ELSE
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    SELECT(This.lcTempCust)
    * Replace  CrAvail WITH CrAvail + lnOpen
    Replace  CrAvail WITH CrAvail + lnAppOpen 
  
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[START]
    SELECT(THIS.lcOrders)
    *B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
  
    *  SELECT ORDHDR
    Replace STATUS 	WITH "H"       ,;
	        APPRAMT   WITH 0         ,;
            APPROVAL  WITH SPACE(10)
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  IF llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    This.ORDHDR.Replace('STATUS     WITH "H"       ,;
					   APPRAMT    WITH 0         ,;
			           APPROVAL   WITH SPACE(10)')
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ELSE
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
   This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value  = 0
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppNum.Value  = ''
    This.llEnableOrdDet = .F.
    This.llEnableOrdRea = .T.
    This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value = This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.codedefaultvalue
    *--mmt
    lcReason = This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value
    REPLACE DECL_CODE WITH lcReason
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF  
  IF llFromOK
    lcReason = This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    This.ordhdr.REPLACE('DECL_CODE WITH lcReason')
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]  

  *--mmt


 *C200089,1 Reham On 07/28/1999  *** Begin ***
  *C200089,1 Update the hold reason before calling the trigger.

*!*	  *C102556,1 (Begin) Update default if Standard, don't overwrite Scope Greek.
*!*	  *=lfvReason()
*!*	  IF !llScopeGrk
*!*	    *B606112,1 SSH Fix bug variable not found
*!*	    *=lfvReason
*!*	    =lfvReason()
*!*	    *B606112,1 SSH Fix bug variable not found
*!*	  ENDIF
*!*	  *C102556,1 (End)  
  
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
*!*	  IF ASCAN(laEvntTrig , PADR('APR_HLD',10)) <> 0
*!*	    llReturn = gfDoTriger('SOORAPR',PADR('APR_HLD',10))
*!*	  ENDIF
  *C200089,1 Reham On 07/28/1999  *** End   ***
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  IF !llFromOK
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtOpenOrd.Value = This.loFormSet.ariaform1.pgfOrders.pgOrders.txtOpenOrd.Value - lnOpen
    This.loFormSet.ariaform1.pgfOrders.pgOrders.txtHoldOrd.Value  = This.loFormSet.ariaform1.pgfOrders.pgOrders.txtHoldOrd.Value + lnOpen
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
  ENDIF
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
ENDIF  

SELECT ORDHDR
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
  DIMENSION laTableUpdate[2]
  laTableUpdate[1] = This.OrdHdr
  laTableUpdate[2] = This.Customer
  =This.lfTableUpdate()
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]  

*!*************************************************************
*! Name      : lfvReason
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/21/2005
*! Purpose   : Update Order hold reason
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvReason()
*!*************************************************************
FUNCTION lfvReason
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
LPARAMETERS llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[START]
SELECT(THIS.lcOrders)
*SELECT ORDHDR
This.Ordhdr.Seek(Customer.ACCOUNT + 'O' + EVALUATE(THIS.lcOrders+'.Order'),	'ORDACCT')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

=RLOCK()
lcReason = This.loFormSet.ariaform1.pgfOrders.pgOrders.cboReason.Value
REPLACE DECL_CODE WITH lcReason
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
This.ordhdr.REPLACE('DECL_CODE WITH lcReason')
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
UNLOCK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
DIMENSION laTableUpdate[1]
laTableUpdate[1] = This.OrdHdr
=This.lfTableUpdate()
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*!*************************************************************
*! Name      : lfvApproval
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/21/2005
*! Purpose   : Update Order Approval number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApproval()
*!*************************************************************
FUNCTION lfvApproval
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
LPARAMETERS llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
SELECT(THIS.lcOrders)
This.Ordhdr.Seek(Customer.ACCOUNT + 'O' + EVALUATE(THIS.lcOrders+'.Order'),	'ORDACCT')
*SELECT ORDHDR
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]
=RLOCK()
lcAppNum =This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppNum.Value
REPLACE Approval WITH lcAppNum
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
This.ordhdr.REPLACE('Approval WITH lcAppNum')
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
UNLOCK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
DIMENSION laTableUpdate[1]
laTableUpdate[1] = This.OrdHdr
=This.lfTableUpdate()
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*!*************************************************************
*! Name      : lfvApprAmnt
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/21/2005
*! Purpose   : Update Order Approved Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApprAmnt()
*!*************************************************************
FUNCTION lfvApprAmnt
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
LPARAMETERS llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
*SELECT ORDHDR
SELECT(THIS.lcOrders)
This.Ordhdr.Seek(Customer.ACCOUNT + 'O' + EVALUATE(THIS.lcOrders+'.Order'),	'ORDACCT')
lnOldAmount = EVALUATE(THIS.lcOrders+'.APPRAMT')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

=RLOCK()
lnAppAmnt = This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value
*--mmt
IF lnAppAmnt < 0
  *-- Force user to enter positive values. 
  *Message : 32141==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('TRM32141B00000', 'DIALOG',LANG_ApprAmount)
  lnAppAmnt = lnOldAmount
  This.loFormSet.ariaform1.pgfOrders.pgOrders.txtAppAmnt.Value = lnAppAmnt
  RETURN
ENDIF    && End of IF
*--mmt

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
IF lnAppAmnt >  EVALUATE(THIS.lcOrders+'.OpenAmt')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

  *warning message to tell user that: Approved amount is greater than the order open amount
  * OK
  =gfModalGen('INM32142B00000')&&,'customers')
ENDIF
REPLACE ApprAmt WITH  lnAppAmnt   
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
  This.OrdHdr.REPLACE('ApprAmt WITH   lnAppAmnt')
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]  
UNLOCK

lcUntSin = ' '
lcExRSin = gfGetExSin(@lcUntSin,EVALUATE(THIS.lcOrders+'.cCurrCode'))

*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[Start]
*lnOpen = CEILING(lnOldAmount) &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
*lnApproAmnt = CEILING(lnAppAmnt) &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
lnOpen = lnOldAmount &lcExRSin EVALUATE(THIS.lcOrders+'.nExRate') &lcUntSin EVALUATE(THIS.lcOrders+'.nCurrUnit')
lnApproAmnt = lnAppAmnt &lcExRSin EVALUATE(THIS.lcOrders+'.nExRate') &lcUntSin EVALUATE(THIS.lcOrders+'.nCurrUnit')
*B131985,1 MMT 17/05/2006 fix bug of slow Navigation in screen[End]

*!*	SELECT(This.lcTempCust)
*!*	REPLACE CrAvail WITH CrAvail + lnOpen -lnApproAmnt
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
  SELECT Customer
  *REPLACE CrAvail WITH CrAvail + lnOpen -lnApproAmnt
  *E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
*!*	  THIS.CUSTOMER.REPLACE('CrAvail WITH CrAvail + lnOpen -lnApproAmnt')
*!*	  REPLACE CrAvail WITH CrAvail + lnOpen -lnApproAmnt
  lnCustCrVal = EVALUATE(This.lcTempCust+'.CrAvail')
  THIS.CUSTOMER.REPLACE('CrAvail WITH lnCustCrVal')
  REPLACE CrAvail WITH lnCustCrVal
ELSE
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
*----------
 SELECT(This.lcTempCust)
 REPLACE CrAvail WITH CrAvail + lnOpen -lnApproAmnt
*------------
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
IF llFromOK
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]
DIMENSION laTableUpdate[2]
laTableUpdate[1] = This.OrdHdr
laTableUpdate[2] = This.Customer
=This.lfTableUpdate()
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
ENDIF
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]

ENDFUNC 
*!*************************************************************
*! Name      : lfvSelect
*: Developer : Mariam Mazhar (MMT)
*: Date      : 08/16/2005 
*! Purpose   : function to make selection
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType  
PRIVATE llSelAll 
llSelAll  = .F. 
lnRecNo = RECNO()
DO CASE 
  CASE lcType = 'S'
    SELECT (This.lcTempCust)
    REPLACE LLSel WITH !LLSel
  CASE lcType = 'A'
     llSelAll  = .F.
	 SELECT (This.lcTempCust)
     REPLACE  ALL LLSel WITH .T.
     THIS.llSelectAllEna =.F. 
  CASE lcType = 'N'
     llSelAll  = .T.
	 SELECT (This.lcTempCust)
     REPLACE  ALL LLSel WITH .F.
     THIS.llSelectnOneEna =.F. 
 
  CASE lcType = 'V'
     llSelAll  = !llSelAll
	 SELECT (This.lcTempCust)
     REPLACE  ALL LLSel WITH !LLSel
 ENDCASE    
 
 This.llSelectAllEna =llSelAll
 
 SELECT (This.lcTempCust)

 
 LOCATE FOR LLSEL
 
 IF FOUND()
   THIS.llAppEna =.T. 
   THIS.llRecEna =.T. 
   THIS.llSelectnOneEna =.T. 
 ELSE
   THIS.llAppEna =.F. 
   THIS.llRecEna =.F. 
   THIS.llSelectnOneEna =.F. 
 ENDIF 
 
 LOCATE FOR !LLSEL
 
 IF FOUND()
   This.llSelectAllEna = .T.
 ELSE 
   This.llSelectAllEna = .F.
 ENDIF 
 
 IF BETWEEN(lnRecNo,1,RECCOUNT())
   GO RECORD lnRecNo
 ENDIF   

 THIS.llSelectIvertEna =.T. 
 
 This.lfvpbSel()
 
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/21/2005
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
SELECT (This.lcTempCust)
IF LLSEL 
  This.lcSelectCap = LANG_unSelect
ELSE 
  This.lcSelectCap = LANG_Select
ENDIF
*!*************************************************************
*! Name      : lfgetValueLogic
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/17/2005
*! Purpose   : Function to fill check box in grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*!
FUNCTION lfgetValueLogic
  PRIVATE lnRetVal
  lnRetVal = EVAL(This.lcTempCust+'.llSel')
  RETURN lnRetVal
ENDFUNC 
*!*************************************************************
*! Name      : lfvApprove
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/17/2005
*! Purpose   : Approve by credit line for a group of customers
*!*************************************************************
*! Calls     : gfGetExSin,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApprove()
*!*************************************************************
FUNCTION lfvApprove

PRIVATE lcUntSin,lcCurRec,lcStatus

lnAlias = SELECT(0)
SELECT ORDHDR
lcOldOrd = ORDER()

=This.lfopenfile('OrdHdr','ORDACCT')

SELECT (lnAlias)

lcUntSin = ' '

SELECT (This.lcTempCust)
lnCurrRec = RECNO()
LOCATE FOR llSel

llFound = FOUND()

SELECT CUSTOMER

lcCurRec = Account

lcFile = This.lcTempCust
  
IF llFound
  This.loFormSet.ariaform1.LOCKSCREEN = .T.
  WAIT LANG_Wait_Approve  WINDOW NOWAIT
  

  GO TOP IN (This.lcTempCust)

  SELECT (lcFile)

  PRIVATE llAprovRem
  llAprovRem = .F.
  
  SCAN FOR LLSEL
	= THIS.CUSTOMER.SEEK('M'+Account)  
    IF CUSTOMER.STATUS $'HX'
    
      *Message :One or more customer(s) are Hold or Cancelled. 
      *Cannot approve order(s) for those customers. Do you want to approve order(s) for the other Active customer(s)?
      * Yes No 
      IF !llAprovRem .AND. gfModalGen("QRM32120B32000",'DIALOG') = 2
        This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.refresh()
        EXIT
      ELSE
        llAprovRem = .T.     
        LOOP
      ENDIF
    ELSE
     = This.lfCustAging(&lcFile..Account)

     = THIS.CUSTOMER.SEEK('M'+&lcFile..Account)  
     
  	  STORE Customer.CrAvail TO  lnCrAvail
          
      THIS.ORDHDR.SEEK(&lcFile..ACCOUNT+'O')
      
      SELECT ORDHDR
      
      SCAN REST WHILE Ordhdr.Account+OrdHdr.cOrdType = &lcFile..Account+'O' ;
                 FOR  INLIST(ORDHDR.Status,'O','H')
         
        *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
        IF This.llFormScope AND !SEEK(Ordhdr.Account+Ordhdr.Order,This.lcScopeOrder)
          LOOP 
        ENDIF
        *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]
                 
        WAIT WINDOW LANG_Wait_Approve_ORDERS + ordhdr.account + LANG_Approve_ORDERS_NUM + ordhdr.order NOWAIT

        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)
        *           CEILING(OpenAmt) &lcExRSin nExRate &lcUntSin nCurrUnit <= lnCrAvail
        IF APPRAMT = 0 .AND. ;
           OpenAmt &lcExRSin nExRate &lcUntSin nCurrUnit <= lnCrAvail
*           THIS.ORDHDR.REPLACE('ApprAmt    WITH CEILING(OpenAmt) ,;
				                 Status     WITH "O"       ,;
                				 DECL_CODE  WITH SPACE(2)')
		   THIS.ORDHDR.REPLACE('ApprAmt    WITH OpenAmt ,;
				                 Status     WITH "O"       ,;
                				 DECL_CODE  WITH SPACE(2)')

           REPLACE ApprAmt    WITH OpenAmt ,;
	               Status     WITH "O"       ,;
    	    	   DECL_CODE  WITH SPACE(2)
            
            *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
            IF !EMPTY(ALLTRIM(This.lcApprovalNo))
              THIS.ORDHDR.REPLACE("approval  WITH '"+ALLTRIM(This.lcApprovalNo)+"'")
              REPLACE  approval  WITH ALLTRIM(This.lcApprovalNo)
            ENDIF
            *E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]
                				 
            lnCrAvail = lnCrAvail - ;
                        ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
            
            *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
            * add audit trail record(s)
            This.loFormSet.AddAudit('Order Approved')
            *E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]
            *C200089,1 Reham On 07/22/1999  *** Begin ***
            *C200089,1 When Order header status changed to "Open", Call process Id to
            *C200089,1 execute special function.
*!*	            IF ASCAN(laEvntTrig , PADR('APR_OPN',10)) <> 0
*!*	              llReturn = gfDoTriger('SOORAPR',PADR('APR_OPN',10))
*!*	            ENDIF
          *C200089,1 Reham On 07/22/1999  *** End   ***
         ELSE
         	*Message 
         	*Customer ð has exceeded his credit limit for order ð. Do you want to keep approving other customer orders?
         	*	Yes 		No	
            IF ORDHDR.STATUS = 'H' .AND. gfModalGen("QRM32076B32000",'DIALOG', ORDHDR.ACCOUNT+"|"+OrdHdr.ORDER) = 2
              This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.refresh()
              EXIT
            ENDIF
         ENDIF
         SELECT ORDHDR
      ENDSCAN
      
      SELECT(This.lcTempCust)
      REPLACE CrAvail WITH lnCrAvail
      
      SELECT Customer
      THIS.CUSTOMER.REPLACE('CrAvail WITH lnCrAvail')
      Replace CrAvail WITH lnCrAvail
      
      SELECT (lcFile)
	ENDIF
  ENDSCAN
ELSE  
  * Message : 32038
  * No customers selected
  * Button : 00000
  * Ok
  =gfModalGen('TRM32038B00000','ALERT','customers')
   This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.refresh()
ENDIF

SELECT Customer
SELECT (lcFile)
This.customer.SEEK('M'+lcCurRec)

SELECT ORDHDR
= This.lfopenfile('OrdHdr',lcOldOrd)

DIMENSION laTableUpdate[2]
laTableUpdate[1] = This.OrdHdr
laTableUpdate[2] = This.Customer
=This.lfTableUpdate()
 This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.SetFocus()
 This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.AfterRowColChange()
 This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.refresh()
 WAIT CLEAR
 SELECT (lcFile)
 
 IF BETWEEN(lnCurrRec,1,RECCOUNT())
   GO RECORD lnCurrRec
 ENDIF   

 
 This.loFormSet.ariaform1.LOCKSCREEN = .F.
 SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvCompute
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/17/2005
*! Purpose   : Approve by credit line for a group of customers
*!*************************************************************
FUNCTION lfvCompute
 lcFile = This.lcTempCust
 lcRcNo = RECNO(This.lcTempCust)
 SELECT (lcFile)
 SCAN FOR LLSEL
   = This.lfCustAging(&lcFile..Account)
   SELECT (lcFile)
   = THIS.CUSTOMER.SEEK('M'+&lcFile..Account)  
   SELECT (lcFile)
   REPLACE  CrAvail WITH Customer.CrAvail
 ENDSCAN  
 IF BETWEEN(lcRcNo,1,RECCOUNT(This.lcTempCust))
   GO RECORD lcRcNo IN (This.lcTempCust)
 ENDIF   
 
 This.loFormSet.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.AfterRowColChange()
ENDFUNC 
*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/17/2005
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.
*!*************************************************************
*! Name      : lfCustAging
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/18/2005
*! Purpose   : function to update customer file aging
*!*************************************************************
FUNCTION lfCustAging
PARAMETERS lcCurrAccount

PRIVATE lnCur,lnAge30,lnAge60,lnAge90,lnAge120,lnOpnCr,lnChgBack,lcUnitSign,;
        lnTCur,lnTAge30,lnTAge60,lnTAge90,lnTAge120,lnOpenAmount

lcUnitSign = '/' 
STORE 0.00 TO lnCur,lnAge30,lnAge60,lnAge90,lnAge120,lnOpnCr,lnChgBack,;
              lnTCur,lnTAge30,lnTAge60,lnTAge90,lnTAge120,lnOpenAmount

lcCurrAlias = SELECT(0)
*-- Age debits
IF This.Debit.SEEK(lcCurrAccount)
  SELECT DEBIT
  SCAN REST WHILE Account = lcCurrAccount
    lcRateSign = gfGetExSin(@lcUnitSign, cCurrCode)
    lnExRate   = nExRate
    lnCurrUnit = nCurrUnit

    * This computes the age of a transaction based on the tran
    * due date. The due date is computed based on the data in the code
    * file for terms.
    lnDays = oAriaApplication.SystemDate - IIF(EMPTY(DueDate),TranDate+30,DueDate)
    DO CASE
      CASE lnDays >= 91
        lnTAge120 = lnTAge120 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 61
        lnTAge90 = lnTAge90 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 31
        lnTAge60 = lnTAge60 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 1
        lnTAge30 = lnTAge30 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      OTHERWISE
        lnTCur = lnTCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDCASE

    * This computes the age of a transaction based on the tran date
    lnDays = oAriaApplication.SystemDate - Debit.TranDate
    DO CASE
      CASE lnDays >= 120
        lnAge120 = lnAge120 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 90
        lnAge90 = lnAge90 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 60
        lnAge60 = lnAge60 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 30
        lnAge30 = lnAge30 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      OTHERWISE
        lnCur = lnCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDCASE
    *-- Charge Backs
    IF TranType = '3'
      lnChgBack = lnChgBack + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDIF
  ENDSCAN
ENDIF

*-- Aging credits
IF This.CREDIT.SEEK(lcCurrAccount)
  SELECT CREDIT
  SCAN REST WHILE Account = lcCurrAccount
    lcRateSign = gfGetExSin(@lcUnitSign, cCurrCode)
    lnExRate   = nExRate
    lnCurrUnit = nCurrUnit
    lnOpnCr = lnOpnCr + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
  ENDSCAN
ENDIF

lcOldorder = ORDER('Ordhdr_x')
SELECT OrdHdr_x
This.loOrdHdrSec.SetOrder('ORDBULK')
This.loOrdHdrSec.Seek(lcCurrAccount+'O')
SELECT OrdHdr_x
SCAN REST WHILE  ACCOUNT+STATUS+BULK+CORDTYPE+ORDER = lcCurrAccount+'O'
  lcRateSign = gfGetExSin(@lcUnitSign, cCurrCode)
  lnOpenAmount = lnOpenAmount + ApprAmt &lcRateSign nExRate &lcUnitSign nCurrUnit
ENDSCAN 
This.loOrdHdrSec.SetOrder(lcOldorder)

*-- Update customer aging information
This.Customer.Seek('M'+lcCurrAccount)

This.Customer.REPLACE('AgeDate    WITH oAriaApplication.SystemDate ,;
					   CURRENT    WITH lnCur     ,;
				       AGE30      WITH lnAge30   ,;
				       AGE60      WITH lnAge60  ')
This.Customer.REPLACE('AGE90      WITH lnAge90   ,;
				       AGE120     WITH lnAge120  ,;
				       TOTAGE     WITH lnCur+lnAge30+lnAge60+lnAge90+lnAge120 ,;
				       OPENCR     WITH lnOpnCr   ,;
				       CHGBACK    WITH lnChgBack ')
This.Customer.REPLACE('NETBAL     WITH TOTAGE + OPENCR ,;
				       TERCURRENT WITH lnTCur   ,;
				       TERAGE30   WITH lnTAge30 ,;
				       TERAGE60   WITH lnTAge60 ')
This.Customer.REPLACE('TERAGE90   WITH lnTAge90 ,;
				       TERAGE120  WITH lnTAge120 ,;
				       nHgWtrMark WITH IIF(NETBAL > nHgWtrMark, NETBAL, nHgWtrMark),;
				       CrAvail    WITH CrLimit-NETBAL-lnOpenAmount')

DIMENSION laTableUpdate[1]

laTableUpdate[1] = This.Customer

=This.lfTableUpdate()				       
SELECT(lcCurrAlias)				       
ENDFUNC  
*!*************************************************************
*! Name      : lfBeforeDestory
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/18/2005
*! Purpose   : function to release relation between files
*!*************************************************************
FUNCTION lfBeforeDestory
*  lcAlais = ALIAS()
  SELECT ORDHDR 
  SET FILTER TO 
  SET KEY TO 
 * SELECT (lcAlais)
ENDFUNC 
*!*************************************************************
*! Name      : lfaddOrderControl
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/23/2005
*! Purpose   : function to add control source for orders grid
*!*************************************************************
FUNCTION lfaddOrderControl

  WITH This.loFormSet.ariaform1.pgfOrders.pgOrders.grdOrders
	IF 	.columnCount <> 8    
	.ColumnCount = 8
	.Columns(1).header1.caption = LANG_Order
    .Columns(2).header1.caption = LANG_Status
    .Columns(3).header1.caption = LANG_Season
    .Columns(4).header1.caption = LANG_Factor
    .Columns(5).header1.caption = LANG_Complete
    .Columns(6).header1.caption = LANG_OpenAmnt
    .Columns(7).header1.caption = LANG_AprNum
    .Columns(8).header1.caption = LANG_AppAmnt
    
    .Columns(1).Width = 75
    .Columns(2).Width = 75
    .Columns(3).Width = 139
    .Columns(4).Width = 75
    .Columns(5).Width = 75
    .Columns(6).Width = 75
    .Columns(7).Width = 75
    .Columns(8).Width = 75    
  	.RecordSource = ""
    .RecordSource = THIS.lcOrders
    .cSeekIndex = THIS.lcOrders
    .Columns(1).ControlSource = THIS.lcOrders+'.ORDER'
    .Columns(1).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(1).RemoveObject('TEXT1')
    .Columns(1).CurrentControl	 = "Gridcolumn1"
    .Columns(1).gridcolumn1.Visible = .T.
    .Columns(1).ReadOnly = .T.

    .Columns(2).ControlSource = 'Thisformset.loBusObj.lfGetValueOrdStat()'
    .Columns(2).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(2).RemoveObject('TEXT1')
    .Columns(2).CurrentControl	 = "Gridcolumn1"
    .Columns(2).gridcolumn1.Visible = .T.
    .Columns(2).ReadOnly = .T.

    .Columns(3).ControlSource = 'Thisformset.loBusObj.lfGetSeason()'
    .Columns(3).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(3).RemoveObject('TEXT1')
    .Columns(3).CurrentControl	 = "Gridcolumn1"
    .Columns(3).gridcolumn1.Visible = .T.
    .Columns(3).ReadOnly = .T.

    .Columns(4).ControlSource = THIS.lcOrders+'.cFacCode'
    .Columns(4).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(4).RemoveObject('TEXT1')
    .Columns(4).CurrentControl	 = "Gridcolumn1"
    .Columns(4).gridcolumn1.Visible = .T.
    .Columns(4).ReadOnly = .T.

    .Columns(5).ControlSource = THIS.lcOrders + '.COMPLETE'
    .Columns(5).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(5).RemoveObject('TEXT1')
    .Columns(5).CurrentControl	 = "Gridcolumn1"
    .Columns(5).gridcolumn1.Visible = .T.
    .Columns(5).ReadOnly = .T.

    .Columns(6).ControlSource = THIS.lcOrders+'.OpenAmt'
    .Columns(6).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(6).RemoveObject('TEXT1')
    .Columns(6).CurrentControl	 = "Gridcolumn1"
    .Columns(6).gridcolumn1.Visible = .T.
    .Columns(6).ReadOnly = .T.

    .Columns(7).ControlSource = THIS.lcOrders+'.Approval'
    .Columns(7).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(7).RemoveObject('TEXT1')
    .Columns(7).CurrentControl	 = "Gridcolumn1"
    .Columns(7).gridcolumn1.Visible = .T.
    .Columns(7).ReadOnly = .T.

    .Columns(8).ControlSource = THIS.lcOrders+'.ApprAmt'
    .Columns(8).AddObject('Gridcolumn1','GRIDCOLUMN')
    .Columns(8).RemoveObject('TEXT1')
    .Columns(8).CurrentControl	 = "Gridcolumn1"
    .Columns(8).gridcolumn1.Visible = .T.
    .Columns(8).ReadOnly = .T.

    .SETALL('ReadOnly',.T.,'COLUMN')
    .Enabled = .T. 
    .Refresh()
    .AfterRowColChange ()           
    ENDIF 
  ENDWITH 
ENDFUNC 


*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[Start]
*!*************************************************************
*! Name      : lfGetOldValues
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/04/2012
*! Purpose   : function to Get old values
*!*************************************************************
FUNCTION lfGetOldValues
LOCAL lcCurAlis 
lcCurAlis = SELECT()
This.Ordhdr.Seek(Customer.ACCOUNT + 'O' + EVALUATE(THIS.lcOrders+'.Order'),	'ORDACCT')
SELECT ORDHDR
SCATTER MEMO MEMVAR 
SELECT (THIS.lcOrders)
GATHER MEMO MEMVAR 
SELECT Customer 
SCATTER MEMO MEMVAR 
SELECT(THIS.lcTempCust) 
GATHER MEMO MEMVAR 

STORE 0 TO lnTotHold,lnTotOpen
SELECT (THIS.lcOrders) 
lnOrdRec = RECNO()
SCAN
  IF Status = 'H'
    lnTotHold = lnTotHold + OpenAmt
  ELSE
    lnTotOpen = lnTotOpen + OpenAmt
  ENDIF 
ENDSCAN 
IF BETWEEN(lnOrdRec ,1,RECCOUNT())
  GO RECORD lnOrdRec 
ENDIF
WITH This.loFormSet.ariaform1.pgfOrders.pgOrders
  .txtOpenOrd.Value = lnTotOpen
  .txtHoldOrd.Value = lnTotHold
ENDWITH 
SELECT(lcCurAlis)
*E303162,2 MMT 06/04/2012 Add Credit Release Status Changes to Audit Trail[End]

*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]
*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/03/2016
*! Purpose   : Call Scope OG
*!*************************************************************
FUNCTION lfvScope
LPARAMETERS loFormSet
lcOgStatus = 'OH'
lcPrgApp = ''
DECLARE laScopExpr[1]
lcExpr = gfOpGrid('SOORDAPR',.T.,.F.,.F.,.f.,.T.)
IF lcExpr =".F."
  RETURN .F.
ENDIF
lnFldPos = ASCAN(laScopExpr,"ORDHDR.SEASON")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'SEASON',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.SEASON',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.SEASON,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

lnFldPos = ASCAN(laScopExpr,"ORDHDR.CDIVISION")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'CDIVISION',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.CDIVISION',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.CDIVISION,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF


lnFldPos = ASCAN(laScopExpr,"ORDHDR.SPCINST")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'SPCINST',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.SPCINST',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.SPCINST,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

lnFldPos = ASCAN(laScopExpr,"ORDHDR.CTERMCODE")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'CTERMCODE',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.CTERMCODE',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.CTERMCODE,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

lnFldPos = ASCAN(laScopExpr,"ORDHDR.SHIPVIA")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'SHIPVIA',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.SHIPVIA',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.SHIPVIA,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

lnFldPos = ASCAN(laScopExpr,"ORDHDR.CORDERCAT")
IF lnFldPos  > 0
  lnFldPos  = ASUBSCRIPT(laScopExpr,lnFldPos ,1)
  lcFldSel =IIF(!EMPTY(laScopExpr[lnFldPos ,6]),laScopExpr[lnFldPos,6],'')
  IF !EMPTY(lcFldSel)
    lcFldFile = gfTempName()
    llUseFld = IIF(LEN(lcFldSel )>0,.T.,.F.) AND lfConvertToCursor(lcFldSel,'CORDERCAT',lcFldFile)
    IF llUseFld 
      lnFldStart = AT('INLIST(ORDHDR.CORDERCAT',lcExpr)
      IF lnFldStart > 0
         lnEndPos = AT(")",SUBSTR(lcExpr ,lnFldStart))+lnFldStart -1
         lnNumChar = lnEndPos -lnFldStart +1
         lcExpr = STUFF(lcExpr ,lnFldStart ,lnNumChar,"Seek(ORDHDR.CORDERCAT,'&lcFldFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF
 
This.lcApprovalNo = lcPrgApp
This.llFormScope = .T.
SELECT (This.lcScopeCustomer)
ZAP 
SELECT (This.lcScopeOrder)
ZAP
SELECT Ordhdr 
lcExpr = lcExpr + " AND Status $ lcOgStatus "
*SET ORDER TO ORDHDR   && CORDTYPE+ORDER
=gfSetOrder("ORDHDR")
=gfSEEK('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR &lcExpr.
  SCATTER MEMO MEMVAR 
  IF !SEEK(m.Account+m.Order,This.lcScopeOrder)
    INSERT INTO (This.lcScopeOrder) FROM MEMVAR 
  ENDIF  
  IF !SEEK(m.Account,This.lcScopeCustomer)
    INSERT INTO (This.lcScopeCustomer) VALUES (m.Account)
  ENDIF
ENDSCAN 
SELECT (This.lcTempCust)
lnCurRecNo = RECNO()
REPLACE LLSel WITH .F. ALL 
LOCATE 
SCAN 
  IF SEEK(Account,This.lcScopeCustomer) 
    REPLACE LLSel WITH .T. 
  ENDIF
ENDSCAN 
 This.llSelectAllEna = .F.
 SELECT (This.lcTempCust)
 LOCATE FOR LLSEL
 IF FOUND()
   THIS.llAppEna =.T.
   THIS.llRecEna =.T.
   THIS.llSelectnOneEna =.T.
 ELSE
   THIS.llAppEna =.F.
   THIS.llRecEna =.F.
   THIS.llSelectnOneEna =.F.
 ENDIF
 LOCATE FOR !LLSEL
 IF FOUND()
   This.llSelectAllEna = .T.
 ELSE
   This.llSelectAllEna = .F.
 ENDIF
IF BETWEEN(lnCurRecNo,1,RECCOUNT())
  GO RECORD lnCurRecNo
ENDIF
THIS.llSelectIvertEna =.T.
This.lfvpbSel()
loFormset.ariaform1.pgfOrders.pgAccounts.grdAccounts.grdMultiSelectionGrid.AfterRowColChange() 
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]

ENDDEFINE

*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
*!*************************************************************
*! Name      : lfApprChanged
*! Developer : Saber A.Razek [SAB]
*! Date      : 05/27/2012
*! Purpose   : function to check if approve info changed
*!*************************************************************
FUNCTION lfApprChanged
LPARAMETERS loFormSet

DO CASE
  CASE !EMPTY(loFormSet.lnApprAmnt) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.APPRAMT')   <> loFormSet.lnApprAmnt
    RETURN .T.
  CASE !EMPTY(loFormSet.lnApprNum) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.APPROVAL')  <> loFormSet.lnApprNum
    RETURN .T.
  CASE !EMPTY(loFormSet.llStatus) AND IIF(EVALUATE(loFormSet.loBusObj.lcOrders+'.STATUS') == 'O', 1, 2)  <> loFormSet.llStatus
    RETURN .T.
  CASE !EMPTY(loFormSet.lcReason) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.DECL_CODE') <> loFormSet.lcReason
    RETURN .T.
  OTHERWISE
    RETURN .F.
ENDCASE
ENDFUNC 


*!*************************************************************
*! Name      : lfGetKey
*! Developer : Saber A.Razek [SAB]
*! Date      : 05/27/2012
*! Purpose   : function to get current key for grid source
*!*************************************************************
FUNCTION lfGetKey
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT()
SELECT (loFormSet.loBusObj.lcOrders)
loFormSet.lcGridKey = EVALUATE(KEY())

SELECT (lnAlias)
ENDFUNC 
*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]

*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][Start]


*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][Start]
*!*************************************************************
*! Name      : lfApprChanged
*! Developer : Saber A.Razek [SAB]
*! Date      : 05/27/2012
*! Purpose   : function to check if approve info changed
*!*************************************************************
FUNCTION lfApprChanged
LPARAMETERS loFormSet

DO CASE
  CASE !EMPTY(loFormSet.lnApprAmnt) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.APPRAMT')   <> loFormSet.lnApprAmnt
    RETURN .T.
  CASE !EMPTY(loFormSet.lnApprNum) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.APPROVAL')  <> loFormSet.lnApprNum
    RETURN .T.
  CASE !EMPTY(loFormSet.llStatus) AND IIF(EVALUATE(loFormSet.loBusObj.lcOrders+'.STATUS') == 'O', 1, 2)  <> loFormSet.llStatus
    RETURN .T.
  CASE !EMPTY(loFormSet.lcReason) AND EVALUATE(loFormSet.loBusObj.lcOrders+'.DECL_CODE') <> loFormSet.lcReason
    RETURN .T.
  OTHERWISE
    RETURN .F.
ENDCASE
ENDFUNC


*!*************************************************************
*! Name      : lfGetKey
*! Developer : Saber A.Razek [SAB]
*! Date      : 05/27/2012
*! Purpose   : function to get current key for grid source
*!*************************************************************
FUNCTION lfGetKey
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT()
SELECT (loFormSet.loBusObj.lcOrders)
loFormSet.lcGridKey = EVALUATE(KEY())

SELECT (lnAlias)
ENDFUNC
*E303162,1 SAB 05/28/2012 Add Credit Release Status Changes to Audit Trail[T20120410.0111][End]
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
FUNCTION lfasignvar
IF llMultCurr
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ENDIF
  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End]
ENDIF
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : When function of OG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*
FUNCTION lfwRepWhen

lnRepPos  = lfItmPos('ORDHDR.REP1')
lnOrdPos  = lfItmPos('ORDHDR.ORDER')
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ENDIF

IF EMPTY(laRpSource)
  DECLARE laRpSource[2],laRpTarget[2]  && Redeclare the source and target arrays.
  STORE "Open" TO laRpSource[1],laRpTarget[1]
  STORE "Hold" TO laRpSource[2],laRpTarget[2]
ENDIF
LOCAL lnCount
IF !EMPTY(laRpTarget[1])
  lcRpStatus = ' '
  FOR lnCount = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnCount] = "Open",'O',;
                              IIF(laRpTarget[lnCount] = "Hold",'H',''))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'OH',ALLTRIM(lcRpStatus))
*!*************************************************************
*! Name      : lfItmPos
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loogScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*!*************************************************************
*! Name      : lfsrOrder
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    IF !EMPTY(lcRpStatus) 
      SELECT ORDHDR
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) AND cOrdType = 'O'
      LOCATE
    ELSE
      SELECT ORDHDR
      SET FILTER TO (ORDHDR.STATUS$'OH') AND cOrdType = 'O'
      LOCATE
    ENDIF
     SET ORDER TO ORDHDR   && CORDTYPE+ORDER
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
ENDCASE
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE

*!*************************************************************
*! Name      : lfsrRep
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/03/2016
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr
lcOldStat = lcRpStatus  && Save old status value.
= lfOGMover(@laRpSource,@laRpTarget,"Select Order Status",.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = "Open",'O',;
                              IIF(laRpTarget[lnI] = "Hold",'H',''))  
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'OH',ALLTRIM(lcRpStatus))
lnOrdPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
  IF USED(lcOrderSel)
    SELECT(lcOrderSel)
    ZAP
  ENDIF
ENDIF
llClearOrd = .T.
lcOgStatus = lcRpStatus 
FUNCTION lfCopyExp
=ACOPY(loOGScroll.laOGVrFlt , laScopExpr)
lcPrgApp = lcRpAPprv
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 04/03/2016
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName
laTempacstru[1,2]='C'
laTempacstru[1,3]= 6
laTempacstru[1,4]= 0
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*E303656,1 MMT 04/03/2016 Add scope option to Sales order approval screen[T20150126.0032][End]