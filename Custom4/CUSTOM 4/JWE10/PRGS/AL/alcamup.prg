*:***************************************************************************
*: Program file  : ALCAMUPS.PRG
*: Program desc. : Export Shipment to UPS.
*: System        : Aria Apparel System (A27).
*: System        : Aria Advantage Series.
*: Module        : Sales Order Allocation (AL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102781,1
*: Date          : 12/26/2002
*:***************************************************************************
*: Calls :  
*:         Procedures : 
*:               -----------------------------------------------------------
*:          Functions : lfUpdShp ,  lfOpenFiles  , lfCloseFls , lfsrPkt
*:                    : lfsrAcct ,  lfSeTOrdr    , lfSRVWar   , lfupdate
*:                    : lfGetAddr,  lfGetTypeD
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO ARFIXAR
*:***************************************************************************
*:Modifications :
*:B606936,1 ASH 02/06/03 Process only open pick tickets.
*:B122243,1 NNA 04/18/2004 fix bug of that if the sales order has an alternative Shipto Addresses
*:B122243,1 NNA            The program save address3 (City,State,Zip) in the field of [cstcity] insteed of
*:B122243,1 NNA            [cstcity1 , cststat1 and cstzip1] 
*:B132533,1 TMI 06/18/2006 take into considration the ORDHDR.ALT_SHPTO is .T.
*:T20060804.0022 - C200672 Convert to Aria4xp
*:B611265,1 MMT 02/09/2017 Issue#13:Custom Export picking ticket to UPS does not export ZIP code[P20140306.0001]
*:B611275,1 MMT 03/05/2017 Issue#13: Export Picking ticket to UPS is not exporting complete ship to address[P20140306.0001]
*:***************************************************************************
*:
*-- llMultWare :- Variable to know if the company is multi warehouse or not.
*-- lcExpr     :- Variable hold the filter expration. 
*-- llRpUnShip :- Variable hold true & false for shipped PT.
DIMENSION laOpenFile[2,2]
*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
loDBFPIKTKT   = ''
loDBFORDHDR  = ''
loDBFCUSTOMER = ''
loDBFWAREHOUS   = ''

DIMENSION laFXFlT[1,1]

llRpUnShip = .F.
llMultWare = (gfGetMemVar('M_WareHouse') = 'Y')
lcExpr     = gfOpGrid('ALCAMUPS',.T.)

*-- If user close the option grid return and don't complete the program.
IF  lcExpr = ".F."
  *TYPE('&lcExpr') = "L" .AND. !&lcExpr
  RETURN
ELSE

  *-- Text Message : - Are you sure you want to proceed Updating.
  *-- Text No      : - 42195.
  *-- Text Button  : - Yes  - No .
  *-- Button No    : -42002
  IF gfModalGen('INM00000B32000','F','ALERT',' ','Are you sure you want to proceed updating now.') = 2
    RETURN
  ENDIF
  
  *-- Open needed files.
  = lfOpenFiles ()

  *-- Update shipmemnt file.
  = lfUpdShp ()
  
  *-- Function to close opened files.
  = lfCloseFls ()

ENDIF

*-- Text Message : - Updating process completed successfully.
*-- Text No      : - 42219.
*-- Text Button  : - Ok.
*-- Button No    : -00000
= gfModalGen('INM00000B00000','F','ALERT',' ','Process completed successfully.')

*-- End of code
*:*************************************************************
*: Name      : lfUpdShp
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : Update shipmemnt file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Example   : =lfUpdShp()
*:*************************************************************
*:
FUNCTION lfUpdShp

PRIVATE lnAlias
lnAlias = SELECT()


llUsePiktkt  = .F.
llUseAccount = .F.
llUseOrder 	 = .F.
llUseEnterd  = .F.
llUseWare  = .F.

*PIKTKT.CWARECODE
IF llMultWare
  lcWareFile = ''
  lnPosition = ASUBSCRIPT(laFXFlT,ASCAN(laFXFlT,'PIKTKT.CWARECODE'),1)
  IF lnPosition > 0
    lcWareFile  = laFXFlT[lnPosition,6]
    llUseWare   = IIF(!EMPTY(lcWareFile) .AND. USED(lcWareFile) .AND. RECCOUNT(lcWareFile)>0,.T.,.F.)
  ENDIF
  IF llUseWare
    SELECT(lcWareFile)
    LOCATE 
    IF EOF()
      llUseWare = .F.
    ENDIF 
  ENDIF 
ENDIF 

lcPikFile = ''
lnPosition = ASUBSCRIPT(laFXFlT,ASCAN(laFXFlT,'PIKTKT.PIKTKT'),1)
IF lnPosition > 0
  lcPikFile = laFXFlT[lnPosition,6]
  llUsePiktkt = IIF(!EMPTY(lcPikFile) .AND. USED(lcPikFile) .AND. RECCOUNT(lcPikFile)>0,.T.,.F.)
ENDIF
IF llUsePiktkt   
  SELECT(lcPikFile)
  LOCATE 
  IF EOF()
    llUsePiktkt  = .F.
  ENDIF 
ENDIF 

lcAccFile = ''
lnPosition = ASUBSCRIPT(laFXFlT,ASCAN(laFXFlT,'PIKTKT.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile   = laFXFlT[lnPosition,6]
  llUseAccount  = IIF(!EMPTY(lcAccFile) .AND. USED(lcAccFile) .AND. RECCOUNT(lcAccFile)>0,.T.,.F.)
ENDIF
IF llUseAccount 
  SELECT(lcAccFile)
  LOCATE 
  IF EOF()
    llUseAccount   = .F.
  ENDIF 
ENDIF 

lcOrdFile = ''
lnPosition = ASUBSCRIPT(laFXFlT,ASCAN(laFXFlT,'PIKTKT.ORDER'),1)
IF lnPosition > 0
  lcOrdFile     = laFXFlT[lnPosition,6]
  llUseOrder    = IIF(!EMPTY(lcOrdFile) .AND. USED(lcOrdFile) .AND. RECCOUNT(lcOrdFile)>0,.T.,.F.)
ENDIF
IF llUseOrder 	 
  SELECT(lcOrdFile)
  LOCATE 
  IF EOF()
    llUseOrder = .F.
  ENDIF 
ENDIF 

ldCompDateS = {}
ldCompDateE = {}
lnPosition = ASUBSCRIPT(laFXFlT,ASCAN(laFXFlT,'PIKTKT.DATE'),1)
IF lnPosition > 0 .AND. !EMPTY(laFXFlT[lnPosition,6])
 ldCompDateS = IIF(EMPTY(SUBSTR(laFXFlT[lnPosition,6],1,10)),DTOC(CTOD("")),SUBSTR(laFXFlT[lnPosition,6],1,10))
 ldCompDateE = IIF(EMPTY(SUBSTR(laFXFlT[lnPosition,6],12,21)),DTOC(CTOD("")),SUBSTR(laFXFlT[lnPosition,6],12,21))
ENDIF

llUseEnterd  = !EMPTY(ldCompDateS) and !EMPTY(ldCompDateE)

IF llUsePiktkt
  SELECT(lcPikFile)
  SCAN 
    IF loDBFPIKTKT.Seek(&lcPikFile..PIKTKT,'PIKTKT') AND IIF(llUseAccount,SEEK(PIKTKT.ACCOUNT,lcAccFile),.T.);
       AND IIF(llUseOrder,SEEK(PIKTKT.ORDER,lcOrdFile),.T.) AND ;
       IIF(llUseEnterd,BETWEEN(PIKTKT.DATE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.);
       AND IIF(llMultWare and llUseWare ,SEEK(PIKTKT.CWARECODE,lcWareFile),.T.) AND PIKTKT.Status = 'O' 
	  SELECT PIKTKT
      SCATTER MEMVAR MEMO
      WAIT WINDOW 'Updating piktkt # :'+m.Piktkt NOWAIT
      *-- this check for the only unshipped pick tickets.
      IF !llRpUnShip .OR. !SEEK(M.Piktkt,'ARUPSSHP')
        *-- function to update the custom file.
        = lfupdate ()
      ENDIF
    ENDIF 
  ENDSCAN 
ELSE
  IF llUseOrder
    SELECT (lcOrdFile)
    SCAN 
      IF loDBFPIKTKT.Seek(&lcOrdFile..Order,'ORDPIK')
        SELECT PIKTKT
        SCAN REST WHILE ORDER+PIKTKT = &lcOrdFile..Order FOR IIF(llUseAccount,SEEK(PIKTKT.ACCOUNT,lcAccFile),.T.);
        	 AND  IIF(llUseEnterd,BETWEEN(PIKTKT.DATE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND ;
        	 IIF(llMultWare and llUseWare ,SEEK(PIKTKT.CWARECODE,lcWareFile),.T.) AND ;
        	 PIKTKT.Status = 'O'
		  SELECT PIKTKT
	      SCATTER MEMVAR MEMO
	      WAIT WINDOW 'Updating piktkt # :'+m.Piktkt NOWAIT
	      *-- this check for the only unshipped pick tickets.
	      IF !llRpUnShip .OR. !SEEK(M.Piktkt,'ARUPSSHP')
	        *-- function to update the custom file.
	        = lfupdate ()
	      ENDIF
        ENDSCAN 
      ENDIF  
    ENDSCAN 
  ELSE
    SELECT PIKTKT
    SCAN FOR IIF(llUseAccount,SEEK(PIKTKT.ACCOUNT,lcAccFile),.T.);
      	 AND  IIF(llUseEnterd,BETWEEN(PIKTKT.DATE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND ;
      	 IIF(llMultWare and llUseWare ,SEEK(PIKTKT.CWARECODE,lcWareFile),.T.) AND PIKTKT.Status = 'O'
  	  SELECT PIKTKT
      SCATTER MEMVAR MEMO
      WAIT WINDOW 'Updating piktkt # :'+m.Piktkt NOWAIT
      *-- this check for the only unshipped pick tickets.
      IF !llRpUnShip .OR. !SEEK(M.Piktkt,'ARUPSSHP')
        *-- function to update the custom file.
        = lfupdate ()
      ENDIF
    ENDSCAN   
  ENDIF 
ENDIF




*!*	SELECT PIKTKT
*!*	*B606936,1 ASH 02/06/03 (Begin) Only Open Pick Tickets.
*!*	*SCAN FOR &lcExpr 
*!*	SCAN FOR &lcExpr .AND. Status = 'O'
*!*	*B606936,1 ASH 02/06/03 (End)
*!*	  SCATTER MEMVAR MEMO
*!*	  WAIT WINDOW 'Updating piktkt # :'+m.Piktkt NOWAIT
*!*	  *-- this check for the only unshipped pick tickets.
*!*	  IF !llRpUnShip .OR. !SEEK(M.Piktkt,'ARUPSSHP')
*!*	    *-- function to update the custom file.
*!*	    = lfupdate ()
*!*	  ENDIF
*!*	ENDSCAN

SELECT (lnAlias)
*-- End OF lfUpdShp
*:*************************************************************
*: Name      : lfOpenFiles
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : = lfOpenFiles ()
*:*************************************************************
*:
FUNCTION lfOpenFiles

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .

laOpenFile[1,1] = 'ALSHPINF'
laOpenFile[1,2] = 'ALSHPINF'
*-- O will open this file Excl to zap it as per the specs.
=gfOpenFile(oAriaApplication.DataDir+'ALSHPINF','ALSHPINF', "EX")
ZAP
USE

=gfOpenFile(oAriaApplication.DataDir+'ALSHPINF','ALSHPINF', "SH")
laOpenFile[2,1] = 'ARUPSSHP'
laOpenFile[2,2] = 'ARUPSSHP'

=gfOpenFile(oAriaApplication.DataDir+'ARUPSSHP','ARUPSSHP', "SH")

*!*	laOpenFile[3,1] = 'PIKTKT'
*!*	laOpenFile[3,2] = 'PIKTKT'

*!*	laOpenFile[4,1] = 'ORDHDR'
*!*	laOpenFile[4,2] = 'ORDHDR'

*!*	laOpenFile[5,1] = 'CUSTOMER'
*!*	laOpenFile[5,2] = 'CUSTOMER'

*!*	laOpenFile[6,1] = 'WAREHOUS'
*!*	laOpenFile[6,2] = 'WAREHOUS'

loDBFPIKTKT   = CreateObject("RemoteTable","PIKTKT","PIKTKT",'PIKTKT',SET("DATASESSION"))&&,"",.T.)
loDBFORDHDR   = CreateObject("RemoteTable",'ORDHDR','ORDHDR','ORDHDR',SET("DATASESSION"))
loDBFCUSTOMER = CreateObject("RemoteTable",'CUSTOMER','CUSTOMER','CUSTOMER',SET("DATASESSION"))
loDBFWAREHOUS = CreateObject("RemoteTable",'WAREHOUS','WAREHOUS','WAREHOUS',SET("DATASESSION"))


*!*	FOR I = 1 To ALEN(laOpenFile,1)
*!*	  IF !USED(laOpenFile[I,1])
*!*	    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
*!*	  ENDIF
*!*	ENDFOR

*-- End of lfOpenFiles.
*:*************************************************************
*: Name      : lfCloseFls
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : = lfCloseFls ()
*:*************************************************************
*:
FUNCTION lfCloseFls

FOR I = 1 To ALEN(laOpenFile,1)
  IF USED(laOpenFile[I,1]) &&.AND.  laOpenFile[I,3]
    USE IN (laOpenFile[I,1])
*    = gfCloseFile(laOpenFile[I,1])
  ENDIF
ENDFOR

loDBFPIKTKT   = null  
loDBFORDHDR   = null
loDBFCUSTOMER = null
loDBFWAREHOUS = null

*-- End of lfCloseFls
*:*************************************************************
*: Name      : lfsrPkt
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : go top at the Piktkt file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : =lfsrPkt()
*:*************************************************************
*: Note      : S symbol is [S,Set] , R symbol is Reset
*:*************************************************************
*:
FUNCTION lfsrPkt
PARAMETERS lcParm

SELECT PIKTKT
LOCATE

*-- End of lfsrPkt.
*!*************************************************************
*: Name      : lfsrAcct
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : go top at the customer file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : =lfsrAcct()
*:*************************************************************
*: Note      : S symbol is [S,Set] , R symbol is Reset
*:*************************************************************
*:
FUNCTION lfsrAcct
PARAMETERS lcParm

SELECT CUSTOMER
LOCATE

*-- End of lfsrAcct.
*!*************************************************************
*: Name      : lfSeTOrdr
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : go top at the order header file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : =lfSeTOrdr()
*:*************************************************************
*: Note      : S symbol is [S,Set] , R symbol is Reset
*:*************************************************************
*:
FUNCTION lfSeTOrdr
PARAMETERS lcParm

SELECT ORDHDR
LOCATE

*-- End of lfSeTOrdr.
*!*************************************************************
*: Name      : lfSRVWar
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : go top at the warehouse file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : =lfSRVWar()
*:*************************************************************
*: Note      : S symbol is [S,Set] , R symbol is Reset
*:*************************************************************
*:
FUNCTION lfSRVWar
PARAMETERS lcParm

SELECT WAREHOUS 
LOCATE

*-- End of lfSRVWar.
*!*************************************************************
*: Name      : lfupdate
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : Update the ship ups file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : = lfupdate ()
*:*************************************************************
*:
FUNCTION lfupdate

PRIVATE lnAlias

lnAlias = SELECT(0)

STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
STORE '' TO lcWName,lcWAddr1,lcWAddr2,lcWAddr3,lcWAddr4,lcWAddr5,lcWAddr6,lcUpsCode
*:B611275,1 MMT 03/05/2017 Issue#13: Export Picking ticket to UPS is not exporting complete ship to address[P20140306.0001][Start]
STORE '' TO LCSTADDR33
*:B611275,1 MMT 03/05/2017 Issue#13: Export Picking ticket to UPS is not exporting complete ship to address[P20140306.0001][End]

*-- this function to get the needed address.
= lfGetAddr ()


SELECT ALSHPINF
APPEND BLANK
REPLACE PIKTKT     WITH M.Piktkt                      ,;
        STATUS     WITH 'O'                           ,;
        ORDER      WITH M.ORDER                       ,;
        CUSTPO     WITH M.custPo                      ,;
        ACCOUNT    WITH M.Account                     ,;
        STORE      WITH M.STORE                       ,;
        CUPSCODE   WITH ALLTRIM(lcUpsCode)            ,; 
        CUPSTYPE   WITH lfGetTypeD(ALLTRIM(lcUpsCode)),;
        STNAME     WITH lcStName                      ,;
        CSTADDR1   WITH lcStAddr1                     ,;
        CSTADDR2   WITH lcStAddr2                     ,;
        CSTCITY1   WITH IIF(OrdHdr.Alt_ShpTo .AND. OCCURS(',',lcStAddr3)>0 ;
                        ,SUBSTR(lcStAddr3,1,AT(',',lcStAddr3,1)-1) ;
                        ,lcStAddr3) ,;
        CSTSTAT1   WITH IIF(OrdHdr.Alt_ShpTo .AND. OCCURS(',',lcStAddr3)>0 ;
                        ,SUBSTR(lcStAddr3,AT(',',lcStAddr3,1)+1 , (AT(',' , lcStAddr3 , 2)-AT(',',lcStAddr3,1))-1)  ;
                        ,lcStAddr4) ,;
        CSTZIP1    WITH IIF(OrdHdr.Alt_ShpTo .AND. OCCURS(',',lcStAddr3)>0 ;
                        ,SUBSTR(lcStAddr3,AT(',',lcStAddr3,2)+1) ;
                        ,lcStAddr5) ,;
        CSTCOUNTRY WITH IIF(OrdHdr.Alt_ShpTo,lcStAddr4,lcStAddr6) ,;
        CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)

REPLACE cSFNAME    WITH lcWName                       ,;
        CSFADDR1   WITH lcWAddr1                      ,;
        CSFADDR2   WITH lcWAddr2                      ,;
        CSFCITY    WITH lcWAddr3                      ,;
        CSFSTATE   WITH lcWAddr4                      ,;
        CSFZIP     WITH lcWAddr5                      ,;
        CSFCOUNTRY WITH lcWAddr6                      ,;
        CSFPHONE   WITH WAREHOUS.CPHONE               ,;
        CUPSACCT   WITH gfGetMemVar('XUPSACCT')
*:B611275,1 MMT 03/05/2017 Issue#13: Export Picking ticket to UPS is not exporting complete ship to address[P20140306.0001][Start]
REPLACE CSTADDR3   WITH LCSTADDR33  
*:B611275,1 MMT 03/05/2017 Issue#13: Export Picking ticket to UPS is not exporting complete ship to address[P20140306.0001][End]  
=gfAdd_Info('ALSHPINF')

SELECT (lnAlias)
*-- End OF lfupdate
*!*************************************************************
*: Name      : lfGetAddr
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : get the address.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : program.
*:*************************************************************
*: Example   : = lfGetAddr ()
*:*************************************************************
*:
FUNCTION lfGetAddr
PARAMETERS lcFrom
PRIVATE lnalias

lnalias = SELECT(0)

loDBFORDHDR.SEEK('O'+M.Order,'Ordhdr')
=IIF(EMPTY(M.Store),loDBFCUSTOMER.SEEK('M'+M.Account,'Customer'),loDBFCUSTOMER.SEEK('S'+M.Account+M.Store,'Customer'))
IF OrdHdr.Alt_ShpTo
  *:B611265,1 MMT 02/09/2017 Issue#13:Custom Export picking ticket to UPS does not export ZIP code[P20140306.0001][Start]
*!*	  lcStName  = OrdHdr.STNAME
*!*	  lcStAddr1 = OrdHdr.cAddress1
*!*	  lcStAddr2 = OrdHdr.cAddress2
*!*	  lcStAddr3 = OrdHdr.cAddress3
*!*	  lcStAddr4 = OrdHdr.cAddress4
*!*	  lcStAddr5 = OrdHdr.cAddress5
  lcStName  = OrdHdr.STNAME
  IF OCCURS(',',ORDHDR.CADDRESS4)=2
    LCSTADDR1 = ORDHDR.CADDRESS1
    LCSTADDR2 = ORDHDR.CADDRESS2
    LCSTADDR33 = ORDHDR.CADDRESS3
    LCSTADDR3 = ORDHDR.CADDRESS4
    LCSTADDR4 = ORDHDR.CADDRESS5
    LCSTADDR5 = ''
  ELSE
    lcStAddr1 = OrdHdr.cAddress1
    lcStAddr2 = OrdHdr.cAddress2
    lcStAddr3 = OrdHdr.cAddress3
    lcStAddr4 = OrdHdr.cAddress4
    lcStAddr5 = OrdHdr.cAddress5
  ENDIF
  *:B611265,1 MMT 02/09/2017 Issue#13:Custom Export picking ticket to UPS does not export ZIP code[P20140306.0001][End]
ELSE
  SELECT CUSTOMER
  IF !EMPTY(DIST_CTR)
    loDBFCUSTOMER.SEEK('S'+Account+DIST_CTR) 
  ENDIF
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  lcStAddr1 = Customer.cAddress1
  lcStAddr2 = Customer.cAddress2
  lcStAddr3 = Customer.cAddress3
  lcStAddr4 = Customer.cAddress4
  lcStAddr5 = Customer.cAddress5
  lcStAddr6 = Customer.cAddress6
ENDIF


SELECT WAREHOUS
IF loDBFWAREHOUS.SEEK(M.cWarecode)
  SELECT WAREHOUS
  lcWName= SUBSTR(CDESC,1,30)
  lcWAddr1 = cAddress1
  lcWAddr2 = cAddress2
  lcWAddr3 = cAddress3
  lcWAddr4 = cAddress4
  lcWAddr5 = cAddress5
  lcWAddr6 = cAddress5
ENDIF

STORE '' TO lcUpsCode
DIMENSION laUpsCode[1,2] 
laUpsCode[1,1] = 'CUPS      '     && Array to get the Division long name
laUpsCode[1,2] = 'lcUpsCode'
*:B132533,1 TMI 06/18/2006 take into considration the ORDHDR.ALT_SHPTO is .T.
*=gfRltFld(OrdHdr.ShipVia,@laUpsCode,'SHIPVIA')
=gfRltFld(IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia),@laUpsCode,'SHIPVIA')
*B132533,1  TMI [End  ] 

SELECT(lnalias)

*-- End OF lfGetAddr.
*:*************************************************************
*: Name      : lfGetTypeD
*: Developer : Abdou Elgendy
*: Date      : 07/02/2000
*: Purpose   : Get UPS Types.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : program.
*:*************************************************************
*: Example   : = lfGetTypeD ()
*:*************************************************************
*:
FUNCTION lfGetTypeD
PARAMETERS lcType

DO CASE
  CASE lcType = 'USUPSN'
    RETURN 'Next Day Air'
  CASE lcType = 'USUPST'
    RETURN 'Next Day Air Saver'
  CASE lcType = 'USUPSE'
    RETURN '2ND Day Air AM'
  CASE lcType = 'USUPS2'
    RETURN '2ND Day Air'
  CASE lcType = 'USUPS3'
    RETURN '3 Day Select'
  CASE lcType = 'USUPSG'
    RETURN 'Ground'
  OTHERWISE
    RETURN ''
ENDCASE

*-- End OF lfGetTypeD
*:*************************************************************

FUNCTION lfCreatExp
ACOPY(loogscroll.laogfxflt,laFXFlT)