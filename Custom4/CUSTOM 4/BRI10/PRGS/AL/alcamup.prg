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
*:C200772,1 WAM 04/10/2007 Generate packing list 
*:C200772,1 WAM 04/10/2007 Send one record for each carton
*:C200772,1 WAM 04/10/2007 Add more fields to the ALSHPINF
*!* C201587,1 HES 07/11/2013 export the ship file to CSV [T20130603.0009]
*B610856,1 MMT 09/16/2014 Export shipments to UPS is not considering direct to store flag[T20140827.0012]
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

*C200772,1 WAM 04/10/2007 Generate packing list if not found
lcPackLin = ''
lcTmpRprt = ''
*C200772,1 WAM 04/10/2007 (End)

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

  *C200772,1 WAM 04/10/2007 Print rejected pick tickets
  SELECT (lcTmpRprt)
  GO TOP
  IF !EOF()
    *C200772,1 WAM 04/10/2007 Print rejected pick tickets[Start]
    IF  gfModalGen('QRM52026B44009','DIALOG')= 1
      *MESSAGEBOX('One or more pick ticket has been rejected. Would you like to print rejected pick tickets?' ,36) = 6
      LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
	    lnDataSess = SET("Datasession")
    	lcDevice   = oAriaApplication.gcDevice
      oAriaApplication.gcDevice = 'PRINTER'
      lcClassDir   = ADDBS(oAriaApplication.ClassDir)
      oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
      loOGScroll   = oOptionGrid.OptionGrid.oHost
      lcOGPlatForm = ''
      loOgScroll.lcOGPlatForm  = 	''
      loOgScroll.lcOGLastForm  = 'ALCAMUP'
      loOGScroll.llPrintPDF = .F.
      loogScroll.cCROrientation = 'P'
      LoOGScroll.llCrystal = .T.

      
      
      DIMENSION loOGScroll.laSelFile[1,3]
      loOGScroll.laSelFile = ''
      
      SELECT (lcTmpRprt)
      GO TOP

      DIMENSION LOogsCROLL.laCRTables[1]
      loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpRprt + ".DBF"
      SELECT(lcTmpRprt)
      USE 
      =gfDispRe('ALCAMUP')
      SET DATASESSION TO (lnDataSess)
      oOptionGrid = .NULL.
      LOogsCROLL =  .NULL.
      *C200772,1 WAM 04/10/2007 Print rejected pick tickets[End]
    ENDIF
  ENDIF
  *C200772,1 WAM 04/10/2007 (End)

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

*C200772,1 WAM 04/10/2007 Get Case Pack profile code
lcProCode = ''
DECLARE laProfiles[1]
STORE '' TO laProfiles
SELECT ccode_no FROM CODES WHERE cdefcode+crltfield+cfld_name = 'NNCPRO_CODE' AND UPPER(ALLTRIM(cdiscrep)) = 'CASE PACK' INTO ARRAY laProfiles
IF _TALLY > 0
  FOR lnCount = 1 TO ALEN(laProfiles)
    IF SEEK('N'+'CPRO_CODE '+laProfiles[lnCount]+SPACE(30)+'CPRO_SCR  ','CODES','CCODE_NO') AND ALLTRIM(CODES.cRlTd_Vlu)='SO' AND ;
       SEEK('N'+'CPRO_CODE '+laProfiles[lnCount]+SPACE(30)+'CPRO_TYPE ','CODES','CCODE_NO') AND ALLTRIM(CODES.cRlTd_Vlu)='ST'
      lcProCode = laProfiles[lnCount]
      EXIT
    ENDIF 
  ENDFOR
ENDIF
*C200772,1 WAM 04/10/2007 (End)

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

*!* C201587,1 HES 07/11/2013 export the ship file to CSV [Start]
lcPath = GETDIR('C:\','Please choose a path to save the CSV file','Select a directory')
SELECT ALSHPINF
COPY TO ADDBS(lcPath)+'ALSHPINF' TYPE CSV
MESSAGEBOX('CSV file for ALSHPINF saved at '+ADDBS(lcPath)+' Path successfully.',64+512,"Success.")
*!* C201587,1 HES 07/11/2013 export the ship file to CSV [End  ]

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

*C200772,1 WAM 04/10/2007 Send more information to UPS
=gfOpenFile(oAriaApplication.DataDir+'PACK_HDR','PACK_HDR', "SH")
=gfOpenFile(oAriaApplication.DataDir+'PACK_LIN','PACK_LIN', "SH")
=gfOpenFile(oAriaApplication.DataDir+'CODES','IDRLTFNAME', "SH")
=gfOpenFile(oAriaApplication.DataDir+'PROFVALU','PROFILE', "SH")
=gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE', "SH")
=gfOpenFile(oAriaApplication.DataDir+'ORDLINE','ORDLINE', "SH")

lcPackLin = gfTempName()
lcTmpRprt = gfTempName()
SELECT PACK_LIN
=AFIELDS(laFileStru)
DECLARE laIndex[1,2]
laIndex[1,1] = 'pack_no+STR(line_no,6)'
laIndex[1,2] = lcPackLin 
=gfCrtTmp(lcPackLin ,@laFileStru,@laIndex)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpRprt) (Piktkt C(6), mError M)
*C200772,1 WAM 04/10/2007 (End)


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

*-- this function to get the needed address.
= lfGetAddr ()
*C200772,1 WAM 04/10/2007 Generate packing list if not found
IF !lfGenPack()
  INSERT INTO (lcTmpRprt) (PikTkt, mError) VALUES (M.Piktkt,'Case pack has not been defined for one or more lines of pick ticket.' )
  RETURN
ENDIF
*C200772,1 WAM 04/10/2007 Get billing options
lcBillOpt = gfCodDes(ORDHDR.CBILLTYPE,'CBILLTYPE')
*C200772,1 WAM 04/10/2007 Send one record for each carton
SELECT PACK_LIN
=SEEK(M.Piktkt)
DO WHILE pack_no+STR(no_cart,4)+style+dyelot = M.Piktkt
  lnCarton = no_cart
  lnWeight = 0
  =SEEK('O'+m.Order+STR(nordlineno,6),'ORDLINE')
  SCAN REST WHILE pack_no+STR(no_cart,4)+style+dyelot = M.Piktkt+STR(lnCarton,4)
    lnWeight = lnWeight + Weight
  ENDSCAN
  *C200772,1 WAM 04/10/2007 (End)

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
                          ,SUBSTR(lcStAddr3,1,AT(',',lcStAddr3,1)-1) ,lcStAddr3) ,;
          CSTSTAT1   WITH IIF(OrdHdr.Alt_ShpTo .AND. OCCURS(',',lcStAddr3)>0 ;
                          ,SUBSTR(lcStAddr3,AT(',',lcStAddr3,1)+1 , (AT(',' , lcStAddr3 , 2)-AT(',',lcStAddr3,1))-1),lcStAddr4) ,;
          CSTZIP1    WITH IIF(OrdHdr.Alt_ShpTo .AND. OCCURS(',',lcStAddr3)>0 ;
                          ,SUBSTR(lcStAddr3,AT(',',lcStAddr3,2)+1) ,lcStAddr5) ,;
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

  *C200772,1 WAM 04/10/2007 Send carton#, Goods description, Package type, Billing option and package weight
  REPLACE No_Cart    WITH lnCarton ,;
          cGoodsDesc WITH ORDLINE.Desc1 ,;
          cBillOpt   WITH lcBillOpt ,;
          cPckgType  WITH 'Package' ,;
          cPackWght  WITH ALLTRIM(STR(lnWeight,5,2))
  *C200772,1 WAM 04/10/2007 (End)
  

  =gfAdd_Info('ALSHPINF')

  *C200772,1 WAM 04/10/2007 Send one record for each carton
  SELECT PACK_LIN
ENDDO
*C200772,1 WAM 04/10/2007 (End)

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
  lcStName  = OrdHdr.STNAME
  lcStAddr1 = OrdHdr.cAddress1
  lcStAddr2 = OrdHdr.cAddress2
  lcStAddr3 = OrdHdr.cAddress3
  lcStAddr4 = OrdHdr.cAddress4
  lcStAddr5 = OrdHdr.cAddress5
ELSE
  SELECT CUSTOMER
  *B610856,1 MMT 09/16/2014 Export shipments to UPS is not considering direct to store flag[T20140827.0012][Start]
  *IF !EMPTY(DIST_CTR)
  IF !EMPTY(DIST_CTR) AND !ORDHDR.lStrDirct
  *B610856,1 MMT 09/16/2014 Export shipments to UPS is not considering direct to store flag[T20140827.0012][End]
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


*:*************************************************************
*: Name      : lfGenPack
*: Developer : Wael Ali Mohamed C200772
*: Date      : 04/10/2007
*: Purpose   : Generate packing list for selected pick ticket
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : program.
*:*************************************************************
*: Example   : = lfGenPack()
*:*************************************************************
*:
FUNCTION lfGenPack

lnCasePack = 0
lnLineNo = 0
lnTotWht = 0
lnTotPcs = 0
lnCartNo = 0
IF !SEEK(M.Piktkt,'PACK_HDR','PACK_HDR')
  ZAP IN (lcPackLin)
  SELECT ORDLINE
  =SEEK('O'+m.Order) 
  SCAN REST WHILE cORdType+ORder+STR(LINENO,6) = 'O'+m.Order FOR PIKTKT = m.PIKTKT
    lnCasePack = 0
    SELECT PROFVALU
    =SEEK('SO'+'O'+m.Order+STR(ORDLINE.LINENO,6))
    LOCATE REST WHILE cpro_type+ckey+cpro_code = 'SO'+'O'+m.Order+STR(ORDLINE.LINENO,6) FOR cPro_code =lcProCode
    IF FOUND()
      lnCasePack = VAL(ALLTRIM(LEFT(cPro_Value,AT(SPACE(1),cPro_Value))))
    ENDIF
    IF lnCasePack > 0
      SELECT ORDLINE
      =SEEK(Style,'Style')
      lnCartons = CEILING(TotPik/lnCasePack)
      SCATTER FIELDS PIK1, PIK2, PIK3, PIK4, PIK5, PIK6, PIK7, PIK8 TO laPikQty 
      FOR lnCarton = 1 TO lnCartons
        lnRemain = lnCasePack
        STORE 0 TO lnPack1, lnPack2, lnPack3, lnPack4, lnPack5, lnPack6, lnPack7, lnPack8
        FOR lnSize = 1 TO 8
          lcSize = STR(lnSize,1)
          lnPack&lcSize = MIN(laPikQty[lnSize],lnRemain)
          lnRemain = lnRemain - lnPack&lcSize
          laPikQty[lnSize] = laPikQty[lnSize] - lnPack&lcSize
          IF lnRemain = 0
            EXIT
          ENDIF
        ENDFOR
        lnLineNo = lnLineNo + 1
        lnCartNo = lnCartNo + 1
        lnTotPack = lnPack1+ lnPack2+ lnPack3+ lnPack4+ lnPack5+ lnPack6+ lnPack7+ lnPack8
        lnWeight = lnTotPack*Style.nStyWeight
        INSERT INTO (lcPackLin) (pack_no,Line_No,nordlineno,No_Cart,style,qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,Weight)  VALUES ;
        (M.Piktkt,lnLineNo,ordline.lineNo,lnCartNo,ordline.style,lnPack1, lnPack2, lnPack3, lnPack4, lnPack5, lnPack6, lnPack7, lnPack8,lnTotPack,lnWeight)
        =gfAdd_Info(lcPackLin)
      
        lnTotWht = lnTotWht + lnWeight
        lnTotPcs = lnTotPcs + lnTotPack
      ENDFOR
    ELSE
      RETURN .F.
    ENDIF
  ENDSCAN
  SELECT (lcPackLin)
  SCAN 
    SCATTER MEMVAR
    INSERT INTO PACK_LIN FROM MEMVAR
    =SEEK(m.Order+STR(m.nordlineno,6),'ORDLINE','ORDLINE')
    REPLACE nPck1 WITH nPck1 + m.Qty1 ,;
            nPck2 WITH nPck2 + m.Qty2 ,;
            nPck3 WITH nPck3 + m.Qty3 ,;
            nPck4 WITH nPck4 + m.Qty4 ,;
            nPck5 WITH nPck5 + m.Qty5 ,;
            nPck6 WITH nPck6 + m.Qty6 ,;
            nPck7 WITH nPck7 + m.Qty7 ,;
            nPck8 WITH nPck8 + m.Qty8 ,;
            nPWght WITH nPWght + m.Weight IN ORDLINE
  ENDSCAN
  INSERT INTO Pack_HDr (pack_no,order,account,store,Tot_wght,tot_cart,tot_pcs,shipvia,cpkchcode,cpkdscode,lstandctn,ctostorcn,cwarecode,piktkt,nLastLNo,nLastCart)  VALUES ;
  (M.Piktkt,m.order,m.account,m.store,lnTotWht,lnCartNo,lnTotPcs,ordhdr.shipvia,'','',.F.,'S',m.cwarecode,m.PIKTKT,lnLineNo,lnCartNo)
  =gfAdd_Info('PACK_HDR')
ENDIF
RETURN .T.