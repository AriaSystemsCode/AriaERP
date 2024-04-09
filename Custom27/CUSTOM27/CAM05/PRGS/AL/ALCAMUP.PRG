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
*:C200687,1 TMI 10/09/2006  (T20060812.0003) update a new field CROUTING from the ORDHDR.NOTE1 field
*:C200817,1 NNA 07/11/2007 (TT20070615.0007) Export customer.usr_dfnd1 to Alshpinf table that by adding 2 new fields to this table. 
*:***************************************************************************
*:
*-- llMultWare :- Variable to know if the company is multi warehouse or not.
*-- lcExpr     :- Variable hold the filter expration. 
*-- llRpUnShip :- Variable hold true & false for shipped PT.
DIMENSION laOpenFile[6,3]
*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
llRpUnShip = .F.
llMultWare = (gfGetMemVar('M_WareHouse') = 'Y')
lcExpr     = gfOpGrid('ALCAMUPS',.T.)


*-- If user close the option grid return and don't complete the program.
IF TYPE('lcExpr') = "L" .AND. !lcExpr
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

SELECT PIKTKT
*B606936,1 ASH 02/06/03 (Begin) Only Open Pick Tickets.
*SCAN FOR &lcExpr 
SCAN FOR &lcExpr .AND. Status = 'O'
*B606936,1 ASH 02/06/03 (End)
  SCATTER MEMVAR MEMO
  WAIT WINDOW 'Updating piktkt # :'+m.Piktkt NOWAIT
  *-- this check for the only unshipped pick tickets.
  IF !llRpUnShip .OR. !SEEK(M.Piktkt,'ARUPSSHP')
    *-- function to update the custom file.
    = lfupdate ()
  ENDIF
ENDSCAN

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
 =gfOpenFile(gcDataDir+'ALSHPINF','ALSHPINF', "EX")
 ZAP
 USE

laOpenFile[2,1] = 'ARUPSSHP'
laOpenFile[2,2] = 'ARUPSSHP'


laOpenFile[3,1] = 'PIKTKT'
laOpenFile[3,2] = 'PIKTKT'

laOpenFile[4,1] = 'ORDHDR'
laOpenFile[4,2] = 'ORDHDR'

laOpenFile[5,1] = 'CUSTOMER'
laOpenFile[5,2] = 'CUSTOMER'

laOpenFile[6,1] = 'WAREHOUS'
laOpenFile[6,2] = 'WAREHOUS'


FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

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
  IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
    = gfCloseFile(laOpenFile[I,1])
  ENDIF
ENDFOR

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

STORE '' TO lcStAddr33

*-- this function to get the needed address.
= lfGetAddr ()


SELECT ALSHPINF
APPEND BLANK
*B122243,1 NNA 04/18/2004 (Begin) Replace fields [cstcity1 , cststat1 and cstzip1] with their data from Address3
*REPLACE PIKTKT     WITH M.Piktkt                      ,;
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
        CSTCITY1   WITH lcStAddr3                     ,;
        CSTSTAT1   WITH lcStAddr4                     ,;
        CSTZIP1    WITH lcStAddr5                     ,;
        CSTCOUNTRY WITH lcStAddr6                     ,;
        CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)

*B132533,1  TMI [Start] comment this code , and take into considration the ORDHDR.ALT_SHPTO is .T. , not only the 
*REPLACE PIKTKT     WITH M.Piktkt                      ,;
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
        CSTCITY1   WITH IIF(OCCURS(',' , lcStAddr3) > 0 , SUBSTR(lcStAddr3 , 1 ,;
                        at(',' , lcStAddr3 , 1)-1) ,lcStAddr3) ,;
        CSTSTAT1   WITH IIF(OCCURS(',' , lcStAddr3) > 1 , SUBSTR(lcStAddr3 ,;
                        at(',' , lcStAddr3 , 1)+1 ,(at(',' , lcStAddr3 , 2)-;
                        at(',' , lcStAddr3 , 1))-1) , SUBSTR(lcStAddr3 , at(',' , lcStAddr3 , 1)+1 ,LEN(lcStAddr3))),;
        CSTZIP1    WITH IIF(OCCURS(',' , lcStAddr3) > 1 , SUBSTR(lcStAddr3 ,;
                        at(',' , lcStAddr3 , 2)+1 ,LEN(lcStAddr3)) , ' '),;
        CSTCOUNTRY WITH lcStAddr6                     ,;
        CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)
*B122243,1 NNA (End)
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
        CSTADDR3   WITH lcStAddr33                     ,;
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
*B132533,1  TMI [End  ] 
REPLACE cSFNAME    WITH lcWName                       ,;
        CSFADDR1   WITH lcWAddr1                      ,;
        CSFADDR2   WITH lcWAddr2                      ,;
        CSFCITY    WITH lcWAddr3                      ,;
        CSFSTATE   WITH lcWAddr4                      ,;
        CSFZIP     WITH lcWAddr5                      ,;
        CSFCOUNTRY WITH lcWAddr6                      ,;
        CSFPHONE   WITH WAREHOUS.CPHONE               ,;
        CUPSACCT   WITH gfGetMemVar('XUPSACCT')
        
*T20060812.0003 TMI [Start] update the CROUTING data
REPLACE CROUTING   WITH ORDHDR.NOTE1
*T20060812.0003 TMI [End  ]         

*C200817,1 NNA 07/11/2007 (Begin) Add Customer's Email from customer.usr_dfnd1 to the new field with flag .T. in Lmail Field (new field)
*C200817,3 TMI [Start] 
*REPLACE Cusr_Dfnd1 WITH CUSTOMER.Usr_Dfnd1,;
        Lmail      WITH IIF(EMPTY(CUSTOMER.Usr_Dfnd1),.F.,.T.)
REPLACE Cusr_Dfnd1 WITH CUSTOMER.Usr_Dfnd1,;
        Lmail      WITH IIF(EMPTY(CUSTOMER.Usr_Dfnd1),'N','Y')
*C200817,3 TMI (End)
*C200817,1 NNA (End)

=gfAdd_Info()

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

= SEEK('O'+M.Order,'Ordhdr')
=IIF(EMPTY(M.Store),SEEK('M'+M.Account,'Customer'),SEEK('S'+M.Account+M.Store,'Customer'))
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  
  IF occur(',',OrdHdr.caddress4) = 2
    lcStAddr1 = OrdHdr.cAddress1
    lcStAddr2 = OrdHdr.cAddress2
    lcStAddr33= OrdHdr.cAddress3
    lcStAddr3 = OrdHdr.cAddress4
    lcStAddr4 = OrdHdr.cAddress5
    lcStAddr5 = ''
  ELSE
    lcStAddr1 = OrdHdr.cAddress1
    lcStAddr2 = OrdHdr.cAddress2
    lcStAddr3 = OrdHdr.cAddress3
    lcStAddr4 = OrdHdr.cAddress4
    lcStAddr5 = OrdHdr.cAddress5
  ENDIF
ELSE
  SELECT CUSTOMER
  IF !EMPTY(DIST_CTR)
    =SEEK('S'+Account+DIST_CTR) 
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
IF SEEK(M.cWarecode)
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

FUNCTION x
x=RIGHT(ALLTRIM(lcAddress),10)
IF ISDIGIT(SUBSTR(x,1,1)) AND ISDIGIT(SUBSTR(x,2,1)) AND ISDIGIT(SUBSTR(x,3,1)) AND ISDIGIT(SUBSTR(x,4,1)) AND ISDIGIT(SUBSTR(x,5,1)) AND ;
  SUBSTR(x,1,1)='-' AND ISDIGIT(SUBSTR(x,7,1)) AND ISDIGIT(SUBSTR(x,8,1)) AND ISDIGIT(SUBSTR(x,9,1)) AND ISDIGIT(SUBSTR(x,10,1))
   
ENDIF