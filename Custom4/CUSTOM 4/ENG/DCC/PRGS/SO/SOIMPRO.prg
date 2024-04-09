*:***************************************************************************************************************************
*: Program file  : soimpro.PRG
*: Program desc. : IMPORTING ROLL OUT ORDERS FROM SPREADSHEET FOR DCC
*: Module        : SO
*: System        : Aria4XP
*: Developer     : Mostafa Eid (mos)  
*! Ticket #      : T20080909.0025
*! Tracking #    : C201130 a27  -  C201131 a4xp
*!***************************************************************************************************************************
*: Passed Parameters  : None
*!***************************************************************************************************************************
*! Modifications:
*B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[T20080812.0001]
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[T20091130.0007]
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[T20091130.0007]	
*B609378,1 MMT 08/09/2010 Import roll out from spreadsheet not updated all SO order header fields[T20100723.0001]
*B609524,1 MMT 02/16/2011 SO Import Roll out spread sheet: Currency is Hardcoded[T20110120.0003]
*C201322,1 MMT 03/27/2011 Update 3 fields in Ordhdr table[T20110304.0007]
*B609684,1 MMT 10/04/2011 import roll out orders adding employees that do not exist in the spreadsheet[T20110914.0020]
*B610209,1 HIA 01/22/2013 SO - Import Sales Order Rollout putting wrong sizes on sales order lines DIRECT CORPORATE [T20130115.0304]
*!***************************************************************************************************************************
*! Previewing The OG

lcExpr = gfOpGrid('SOIMPRO',.T. )
RETURN
STORE '' TO lcFileName , lcRpSphis , lcRpSpdir , lcCatSel 

*********************************************************************

FUNCTION lfMain 
*!*** Open tables
STORE 0 TO lnClrLen , lnClrPos 

IF !USED('CONTACT')
  =gfOpenTable(oAriaApplication.DataDir+'CONTACT','CONTACT','SH')
ENDIF

IF !USED('CUSTOMER')
  =gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF

IF !USED('Codes')
  =gfOpenTable(oAriaApplication.DataDir+'Codes','CCODE_NO','SH')
ENDIF

IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF

IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF

IF !USED('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
ENDIF

*! Global variabls 
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
IF !USED('NOTEPAD')
  =gfOpenTable(oAriaApplication.DataDir+'NOTEPAD','NOTEPAD','SH')
ENDIF

DIMENSION laOrderCreated[1]
STORE '' TO  laOrderCreated
STORE .F. TO llLastOrder
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
llMessageCalled  = .F.
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]

IF lfChkXls()
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  lcOrdHdr   = gfTempName()
  lcOrdLine  = gfTempName()    
  IF LLRPMLST 
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
    =lfProXls() 
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  ELSE
    SELECT(lcTmpXls)
    DIMENSION laStoresArr[1]
    STORE '' TO laStoresArr
    SELECT DISTINCT E FROM (lcTmpXls) WHERE !EMPTY(E) AND RECNO()>3 INTO ARRAY laStoresArr
    IF _TAlly > 0 
      FOR lnF = 1 TO ALEN(laStoresArr,1)
        IF lnF = ALEN(laStoresArr,1)
          llLastOrder = .T.
        ENDIF 
        =lfProXls(laStoresArr[lnF,1])   
      ENDFOR 
    ELSE
      =lfProXls(.F.)           
    ENDIF 
  ENDIF   
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
ENDIF 

*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
IF !llMessageCalled  
  IF !LLRPMLST
    llLastOrder = .T.
  ENDIF 
  lfCustMsg()
ENDIF 
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]


RELEASE loObj
RETURN .F.  && do not close the OG to allow to create new SO
*!--- end of main function

*!*************************************************************
*! Name      : lfChkXls
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : Check xls Sheet For errors 
*!*************************************************************
*! Example   : =lfChkXls()
*!*************************************************************
FUNCTION lfChkXls 

=lfCreaTemp()   
STORE '' TO lcqty 
STORE 0 TO lnCount 
*- IMPORT PROCESS
SELECT 0
lcSvErr = ON('ERROR')
llErr = .F.
ON ERROR llErr = .T.
IMPORT FROM (lcXlsFIle) TYPE XL8


ON ERROR &lcSvErr
IF llErr
  MESSAGEBOX('Spread Sheet is being used , Please Close !')
  RETURN .F.
ENDIF
lcAlias = DBF()
USE 

lcTmpXls = loOgScroll.gfTempName()
lcWorkXLDir = (oAriaApplication.WorkDir + lcTmpXls +'.dbf')
IF FILE(lcWorkXLDir) 
  USE IN (lcWorkXLDir)
  DELETE FILE (lcWorkXLDir)
ENDIF 

RENAME (lcAlias) TO (lcWorkXLDir) 
USE (lcWorkXLDir)
lnMaxCol = AFIELDS(laXlsArray) 
lnColno = 11
FOR  I = 11 TO lnMaxCol       
    lcColnum = laXlsArray[lncolno,1]
    STORE &lcColnum TO laXlsArray[lncolno,5]
    lnColno = lnColno+1
ENDFOR 


*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*IF MOD((lnMaxCol - 11)+1,3)<> 0
*lcMsg = 'WRONG FILE STRUCTURE.. ! Please Modify Structure Of SpreadSheet coulmns To be  Style - scale - fit respectively'
IF MOD((lnMaxCol - 11)+1,4)<> 0
  lcMsg = 'WRONG FILE STRUCTURE.. ! Please Modify Structure Of SpreadSheet coulmns To be  Style - scale - fit - Qty respectively           ' 
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
  IF !EMPTY(lcMsg)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
     RETURN .F.
  ENDIF

 RETURN .f.  
ENDIF 

GO 4

*!-- check the employee 
SCAN REST FOR !EMPTY(&lcTmpXls..A) 
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  =gfSeek('M'+PADR(ALLTRIM(lcRpAccnt),5),'CUSTOMER','CUSTOMER')
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
  SELECT CONTACT   
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  *IF gfSEEK('C'+ PADR(ALLTRIM(lcRpAccnt),8)+PADR(ALLTRIM(&lcTmpXls..E),8),'CONTACT')
  IF Customer.lchkentlm AND gfSEEK('C'+ PADR(ALLTRIM(lcRpAccnt),8)+PADR(ALLTRIM(&lcTmpXls..E),8),'CONTACT')
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
     LOCATE  REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT ='C'+ PADR(ALLTRIM(lcRpAccnt),8)+PADR(ALLTRIM(&lcTmpXls..E),8) ;
     FOR CCNTCTCODE = PADR(ALLTRIM(&lcTmpXls..A),12)
     IF !FOUND()
       SELECT(lcTemLog)    
       APPEND BLANK
       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
*!*	       Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
*!*	                          desc   WITH 'Employee Code    '+ ALLTRIM(&lcTmpXls..A) + '  doesnt not exist for Account'+ lcRpAccnt ;
*!*	                                       +'store Code    ' + ALLTRIM(&lcTmpXls..E)
       Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
                          desc   WITH 'Employee Code '+ ALLTRIM(&lcTmpXls..A) + ' doesnt not exist for Account '+ lcRpAccnt ;
                                       +' store Code ' + ALLTRIM(&lcTmpXls..E)
	   *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]	                                       
     ENDIF                            
  ELSE                                                          

*!-- check the store 
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  *IF !EMPTY(&lcTmpXls..E)      
  IF (!EMPTY(&lcTmpXls..E) AND Customer.lchkentlm) OR (!EMPTY(&lcTmpXls..E) AND !Customer.lchkentlm AND !gfSeek('S'+PADR(ALLTRIM(lcRpAccnt),5)+ALLTRIM(&lcTmpXls..E),'CUSTOMER','CUSTOMER'))
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
    SELECT(lcTemLog)    
    APPEND BLANK
    *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
*!*	    Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
*!*	                       desc   WITH 'Store Code    '+ ALLTRIM(&lcTmpXls..E) + '  doesnt not exist for Account  '+ lcRpAccnt 
    Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
                       desc   WITH 'Store Code '+ ALLTRIM(&lcTmpXls..E) + ' doesnt not exist for Account '+ lcRpAccnt 
	*B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]                       
   ENDIF 
  ENDIF
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
  *FOR  I = 11 TO lnMaxCol STEP 3         
  FOR  I = 11 TO lnMaxCol STEP 4 
  *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
    lcstylecode = PADR(ALLTRIM(laXlsArray[I,5]),12)
    lcScale   =   PADR(ALLTRIM(laXlsArray[I+1,5]),3)
    lcColname =   laXlsArray[I+1,1]
    lcszPOs   =   laXlsArray[I,1]
    lcSze     =   PADR(ALLTRIM(&lcTmpXls..&lcszPOs),5)
    lcqty     = ""  
    lcColCode = ""
    lcFitpos  =  laXlsArray[I+2,1]
    lcfit     =  PADR(ALLTRIM(&lcTmpXls..&lcFitpos),5)
    lcFullscale= ""
    
    SELECT codes 
    IF !EMPTY(&lcTmpXls..&lcColname)
      =SEEK("N"+"COLOR",'Codes','CCODE_NO')
       LOCATE REST WHILE CDEFCODE+CFLD_NAME = "N"+"COLOR" FOR CDISCREP = LTRIM(PADR(&lcTmpXls..&lcColname,30))
      =lfGetClrD()
      lcColCode =   PADR(Codes.CcODE_no,lnClrLen)
    ENDIF   
      
 *!-- check the size code   
      =lfGetQty(lcScale,lcSze,lcfit)      
       lcStyle  = lcstylecode+'-'+lcColCode+lcFullscale
     
     IF !EMPTY(lcSze) 
      IF empty(lcqty) 
        SELECT(lcTemLog)    
        APPEND BLANK
        *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
*!*	        Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
*!*	                           desc   WITH 'Size    '+ lcSze + 'doesnt not exist for Scale  '+ lcScale 
        Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
                           desc   WITH 'Size '+ lcSze + ' doesnt not exist for Scale '+ lcScale 
        *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]                   
      ENDIF   
     ENDIF 
      
 *!-- check the style color code 
    IF !EMPTY(lcColCode)
      IF !Empty(lcqty) 
        SELECT style 
        IF !gfseek(lcStyle,'STYLE','STYLE')
           SELECT(lcTemLog)    
           APPEND BLANK
           *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]           
*!*	           Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
*!*	                              desc   WITH 'colour    '+&lcTmpXls..&lcColname + 'doesnt not exist for style code '+ lcstylecode
           Replace &lcTemLog..LineNo WITH RECNO('&lcTmpXls');
                              desc   WITH 'Colour '+&lcTmpXls..&lcColname + ' doesnt not exist for style code '+ lcstylecode
	       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]                              
        ENDIF  
      ENDIF  
   ENDIF 
  
  ENDFOR 
ENDSCAN   

SELECT(lcTemLog) 
LOCATE 

IF !EOF()
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,"Some lines in the Spreadsheet are Rejected. Do you want to print error log?") = 1 
    o = CREATEOBJECT('excel.application')
    o.Workbooks.Add
    lnI = 0
    SELECT (lcTemLog)
    SCAN
      lnI = lnI + 1
      o.Cells(lnI,1) = LineNo 
      o.Cells(lnI,2) = desc 
    ENDSCAN
    o.visible = .T.
  
  ENDIF 
  RETURN .F.
ENDIF    
RETURN .T.
*! --  end of lfChkXls
*!*************************************************************
*! Name      : lfProXls
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : Process The the importated excel Sheet 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfProXls()
*!*************************************************************

FUNCTION lfProXls 
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
LPARAMETERS lcOrdStore
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
lcTimes = gfGetTime()

*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*lcOrdHdr   = gfTempName()
*lcOrdLine  = gfTempName()
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
STORE 0  TO lnColno , lnMaxCol ,Lnlineno ,lcszPOs , lcTotQty
STORE '' To lcColnum ,lcstylecode , lcScale , lcSze , lcColCode ,lcStyle 
STORE 1 TO Lnlineno , lcAddedValue

ldStartDate = ldRpStartDat
      
*! selection by complete date 

ldCompleteStartDate = ldRpComDat
     
*! select the category       

lnCatPos = ASCAN(loOgScroll.laOgFXFlt,'ORDHDR.CORDERCAT')
lnCatPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnCatPos,1)
lcCatSel = IIF(!EMPTY(loOgScroll.laOgFXFlt[lnCatPos,6]),loOgScroll.laOgFXFlt[lnCatPos,6],'')

SELECT ORDHDR
=AFIELDS(laFileStru)
=gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)

SELECT ORDLINE
=AFIELDS(laFileStru)
=gfCrtTmp(lcOrdLine,@laFileStru,[cOrdType+ORDER],lcOrdline)

=gfSEEK('M'+lcRpAccnt,'CUSTOMER')
SELECT (lcOrdHdr)
APPEND BLANK 
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*!*	     REPLACE cdivision WITH 'DCC' ;
*!*	             multi     WITH 'Y';
*!*	             spcinst   WITH CUSTOMER.spcinst ;
*!*	             entered   WITH oAriaApplication.SystemDate  ;
*!*	             link_code WITH 'DEFDEF';
*!*	             gl_sales  WITH 'DEF';
*!*	             cwarecode WITH lcRpWareHous ;
*!*	             nexrate   WITH 1.0000;
*!*	             ccurrcode WITH 'GBP';
*!*	             ncurrunit WITH 1    ;
*!*	             cadd_user WITH OAriaApplication.user_id ;
*!*	             dadd_date WITH oAriaApplication.SystemDate ;   
*!*	             season    WITH "*";
*!*	             ACCOUNT   WITH lcRpAccnt;
*!*	             shipvia   WITH CUSTOMER.shipvia ;
*!*	             Cordercat WITH lcCatSel ;
*!*	             start     WITH ldStartDate ;
*!*	             complete  WITH ldCompleteStartDate ;
*!*	             ccontref  WITH lcContref;
*!*	             cordtype  WITH 'O';
*!*	             STATUS    WITH 'O';
*!*	             priority  WITH STR(5);
*!*	             custpo    WITH lcContref ;
*!*	             cadd_time WITH lcTimes
*B609524,1 MMT 02/16/2011 SO Import Roll out spread sheet: Currency is Hardcoded[T20110120.0003][Start]
*!*	 REPLACE cdivision WITH 'DCC' ;
*!*	             multi     WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),'N','Y');
*!*	             spcinst   WITH CUSTOMER.spcinst ;
*!*	             entered   WITH oAriaApplication.SystemDate  ;
*!*	             link_code WITH 'DEFDEF';
*!*	             gl_sales  WITH 'DEF';
*!*	             cwarecode WITH lcRpWareHous ;
*!*	             nexrate   WITH 1.0000;
*!*	             ccurrcode WITH 'GBP';
*!*	             ncurrunit WITH 1    ;
*!*	             cadd_user WITH OAriaApplication.user_id ;
*!*	             dadd_date WITH oAriaApplication.SystemDate ;   
*!*	             season    WITH "*";
*!*	             ACCOUNT   WITH lcRpAccnt;
*!*	             shipvia   WITH CUSTOMER.shipvia ;
*!*	             Cordercat WITH lcCatSel ;
*!*	             start     WITH ldStartDate ;
*!*	             complete  WITH ldCompleteStartDate ;
*!*	             ccontref  WITH lcContref;
*!*	             cordtype  WITH 'O';
*!*	             STATUS    WITH 'O';
*!*	             priority  WITH STR(5);
*!*	             custpo    WITH lcContref ;
*!*	             cadd_time WITH lcTimes;
*!*	             Store     WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),ALLTRIM(lcOrdStore),'')
lcCustCurr = IIF(EMPTY(CUSTOMER.cCurrCode),oAriaApplication.BaseCurrency,CUSTOMER.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCustCurr <> oAriaApplication.BaseCurrency
  lnExRate = gfChkRate('lnCurrUnit',lcCustCurr ,oAriaApplication.SystemDate,.T.,.F.,.F.,.F.)
  IF lnExRate = 0
    lcCustCurr = oAriaApplication.BaseCurrency
    STORE 1 TO lnExRate, lnCurrUnit
  ENDIF
ENDIF
 REPLACE cdivision WITH 'DCC' ;
             multi     WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),'N','Y');
             spcinst   WITH CUSTOMER.spcinst ;
             entered   WITH oAriaApplication.SystemDate  ;
             link_code WITH 'DEFDEF';
             gl_sales  WITH 'DEF';
             cwarecode WITH lcRpWareHous ;
             nexrate   WITH lnExRate ;
             ccurrcode WITH lcCustCurr ;
             ncurrunit WITH lnCurrUnit   ;
             cadd_user WITH OAriaApplication.user_id ;
             dadd_date WITH oAriaApplication.SystemDate ;   
             season    WITH "*";
             ACCOUNT   WITH lcRpAccnt;
             shipvia   WITH CUSTOMER.shipvia ;
             Cordercat WITH lcCatSel ;
             start     WITH ldStartDate ;
             complete  WITH ldCompleteStartDate ;
             ccontref  WITH lcContref;
             cordtype  WITH 'O';
             STATUS    WITH 'O';
             priority  WITH STR(5);
             custpo    WITH lcContref ;
             cadd_time WITH lcTimes;
             Store     WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),ALLTRIM(lcOrdStore),'')

*B609524,1 MMT 02/16/2011 SO Import Roll out spread sheet: Currency is Hardcoded[T20110120.0003][End]
*B609378,1 MMT 08/09/2010 Import roll out from spreadsheet not updated all SO order header fields[START]
REPLACE cTermCode WITH CUSTOMER.cTermCode ,;
		priority  WITH CUSTOMER.priority,;
		Cinsur    WITH CUSTOMER.Cinsur ,;
		Rep1      WITH Customer.salesrep   ,;
		Multi 	  WITH 'N',;
		cReorder  WITH  'N',;
		Bulk 	  WITH 'N'

*C201322,1 MMT 03/27/2011 Update 3 fields in Ordhdr table[Start]
REPLACE LFROMWEB WITH  .T.,;
		CWEBNAME  WITH  'DCC',;
		cwebid 	 WITH oAriaApplication.User_ID 
*C201322,1 MMT 03/27/2011 Update 3 fields in Ordhdr table[End]
		
IF EMPTY(ALLTRIM(lcCatSel)) AND gfSEEK("D"+"CORDERCAT",'Codes','CCODE_NO')
  REPLACE CORDERCAT WITH CODES.CCODE_NO
ENDIF  
lnComm1 = 0
IF (gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)='D')
  IF !USED('REP_DIV')
    =gfOpenTable('REP_DIV','REP_DIV')
  ENDIF   
  IF gfSEEK(Customer.salesrep +'DCC','REP_DIV','REP_DIV')
    STORE REP_DIV.Comm_Rate TO lnComm1
  ENDIF   
ENDIF
IF lnComm1 = 0
  lnComm1 = customer.comm
ENDIF 
REPLACE Comm1 WITH lnComm1
*B609378,1 MMT 08/09/2010 Import roll out from spreadsheet not updated all SO order header fields[END]          

             
SELECT NOTEPAD
APPEND BLANK 
REPLACE Type WITH 'B'
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]             
             
SELECT(lcTmpXls)
LOCATE 
lnMaxCol = AFIELDS(laXlsArray) 
lnColno = 11
FOR J = 11 TO lnMaxCol       
    lcColnum = laXlsArray[lncolno,1]
    STORE &lcColnum TO laXlsArray[lncolno,5]
    lnColno = lnColno+1
ENDFOR 

GO 4
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*SCAN REST FOR !EMPTY(&lcTmpXls..A)   
*B609684,1 MMT 10/04/2011 import roll out orders adding employees that do not exist in the spreadsheet[Start]
*SCAN REST FOR !EMPTY(&lcTmpXls..A) AND IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),ALLTRIM(&lcTmpXls..E) = ALLTRIM(lcOrdStore),.T.)  
SCAN REST FOR !EMPTY(&lcTmpXls..A) AND IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),ALLTRIM(&lcTmpXls..E) == ALLTRIM(lcOrdStore),.T.)  
*B609684,1 MMT 10/04/2011 import roll out orders adding employees that do not exist in the spreadsheet[END]
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
WAIT WINDOW " Processsing The Spread Sheet Is In Progress ..... " NOWAIT 
    *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
    *FOR  lnxI = 11 TO lnMaxCol STEP 3           
	SELECT NOTEPAD
	REPLACE Mnotes WITH mnotes+'Employee:'+ALLTRIM(&lcTmpXls..A)+" "+ALLTRIM(&lcTmpXls..B)+" "+ALLTRIM(&lcTmpXls..C)+'  Location:'+ALLTRIM(&lcTmpXls..E)+CHR(13)+CHR(10)              
    SELECT(lcTmpXls) 
    FOR  lnxI = 11 TO lnMaxCol STEP 4     
	*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
    lcstylecode = PADR(ALLTRIM(laXlsArray[lnxI,5]),12)
    lcScale   =   PADR(ALLTRIM(laXlsArray[lnxI+1,5]),3)
    lcColname =   laXlsArray[lnxI+1,1]
    lcszPOs   =   laXlsArray[lnxI,1]
    lcSze     =   PADR(ALLTRIM(&lcTmpXls..&lcszPOs),5)
    lcqty = ""  
    lcbook = ""
    lcColCode = ""
    lcFitpos  =  laXlsArray[lnxI+2,1]
    lcfit     =  PADR(ALLTRIM(&lcTmpXls..&lcFitpos),5)
    *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
    lcQtyPos = laXlsArray[lnxI+3,1]
    lcAddedValue = VAL(ALLTRIM(&lcTmpXls..&lcQtyPos))
    *C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
    lcFullscale= ""
    lnGrssPric = ""
    IF EMPTY(&lcTmpXls..&lcszPOs)AND EMPTY(&lcTmpXls..&lcColname ) 
      LOOP
    ENDIF 
      
    IF !EMPTY(&lcTmpXls..&lcColname )
      SELECT codes 
      =SEEK("N"+"COLOR",'Codes','CCODE_NO')
      LOCATE REST WHILE CDEFCODE+CFLD_NAME = "N"+"COLOR" FOR CDISCREP = PADR(ALLTRIM(&lcTmpXls..&lcColname),30)
      lcColCode = PADR(Codes.CcODE_no,3)
    ELSE 
      lcColCode = PADR("N/A",3)     
    ENDIF   
 
    =lfGetQty(lcScale,lcSze,lcfit)
     lcStyle =lcstylecode+'-'+lcColCode+lcFullscale   
    
    SELECT (lcOrdLine)
    APPEND BLANK 
*B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
*!*	    Replace  cordtype             WITH 'O' ;
*!*	             ACCOUNT              WITH lcRpAccnt; 
*!*	             CWARECODE            WITH lcRpWareHous;
*!*	             STORE                WITH (&lcTmpXls..E);
*!*	             LINENO               WITH Lnlineno;
*!*	             style                WITH lcStyle ;
*!*	             season               WITH 'ALL';
*!*	             SCALE                WITH lcFullscale;
*!*	             start                WITH ldStartDate;
*!*	             complete             WITH ldCompleteStartDate ;
*!*	             &Lcordline..&lcqty   WITH lcAddedValue ;
*!*	             cadd_user            WITH OAriaApplication.user_id;
*!*	             dadd_date            WITH oAriaApplication.SystemDate  ;
*!*	             totqty               WITH lcAddedValue ;
*!*	             totbook              WITH lcAddedValue	;
*!*	             &Lcordline..&lcbook  WITH lcAddedValue ;
*!*	             flag                 WITH 'N'           ;
*!*	             EMPLOYEE             WITH ALLTRIM(&lcTmpXls..A);
*!*	             cadd_time            WITH lcTimes ;                
*!*	             gl_sales             WITH 'DEFDEF' 

*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*!*	    Replace  cordtype             WITH 'O' ;
*!*	             ACCOUNT              WITH lcRpAccnt; 
*!*	             CWARECODE            WITH lcRpWareHous;
*!*	             STORE                WITH ALLTRIM((&lcTmpXls..E));
*!*	             LINENO               WITH Lnlineno;
*!*	             style                WITH lcStyle ;
*!*	             season               WITH 'ALL';
*!*	             SCALE                WITH lcFullscale;
*!*	             start                WITH ldStartDate;
*!*	             complete             WITH ldCompleteStartDate ;
*!*	             &Lcordline..&lcqty   WITH lcAddedValue ;
*!*	             cadd_user            WITH OAriaApplication.user_id;
*!*	             dadd_date            WITH oAriaApplication.SystemDate  ;
*!*	             totqty               WITH lcAddedValue ;
*!*	             totbook              WITH lcAddedValue	;
*!*	             &Lcordline..&lcbook  WITH lcAddedValue ;
*!*	             flag                 WITH 'N'           ;
*!*	             EMPLOYEE             WITH ALLTRIM(&lcTmpXls..A);
*!*	             cadd_time            WITH lcTimes ;                
*!*	             gl_sales             WITH 'DEFDEF' ,;
*!*	             desc1 				  WITH IIF(gfSeek(lcStyle ,'Style','Style'),style.desc1 ,'')
    *B609684,1 MMT 10/04/2011 import roll out orders adding employees that do not exist in the spreadsheet[Start]
*!*      Replace  cordtype             WITH 'O' ;
*!*               ACCOUNT              WITH lcRpAccnt; 
*!*               CWARECODE            WITH lcRpWareHous;
*!*               STORE                WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),'',ALLTRIM(&lcTmpXls..E));
*!*               LINENO               WITH Lnlineno;
*!*               style                WITH lcStyle ;
*!*               season               WITH 'ALL';
*!*               SCALE                WITH lcFullscale;
*!*               start                WITH ldStartDate;
*!*               complete             WITH ldCompleteStartDate ;
*!*               &Lcordline..&lcqty   WITH lcAddedValue ;
*!*               cadd_user            WITH OAriaApplication.user_id;
*!*               dadd_date            WITH oAriaApplication.SystemDate  ;
*!*               totqty               WITH lcAddedValue ;
*!*               totbook              WITH lcAddedValue	;
*!*               &Lcordline..&lcbook  WITH lcAddedValue ;
*!*               flag                 WITH 'N'           ;
*!*               EMPLOYEE             WITH ALLTRIM(&lcTmpXls..A);
*!*               cadd_time            WITH lcTimes ;                
*!*               gl_sales             WITH 'DEFDEF' ,;
*!*               desc1 				  WITH IIF(gfSeek(lcStyle ,'Style','Style'),style.desc1 ,'')
    Replace  cordtype             WITH 'O' ;
             ACCOUNT              WITH lcRpAccnt; 
             CWARECODE            WITH lcRpWareHous;
             STORE                WITH IIF(TYPE('lcOrdStore') = 'C' AND !EMPTY(lcOrdStore),lcOrdStore,ALLTRIM(&lcTmpXls..E));
             LINENO               WITH Lnlineno;
             style                WITH lcStyle ;
             season               WITH 'ALL';
             SCALE                WITH lcFullscale;
             start                WITH ldStartDate;
             complete             WITH ldCompleteStartDate ;
             &Lcordline..&lcqty   WITH lcAddedValue ;
             cadd_user            WITH OAriaApplication.user_id;
             dadd_date            WITH oAriaApplication.SystemDate  ;
             totqty               WITH lcAddedValue ;
             totbook              WITH lcAddedValue  ;
             &Lcordline..&lcbook  WITH lcAddedValue ;
             flag                 WITH 'N'           ;
             EMPLOYEE             WITH ALLTRIM(&lcTmpXls..A);
             cadd_time            WITH lcTimes ;                
             gl_sales             WITH 'DEFDEF' ,;
             desc1           WITH IIF(gfSeek(lcStyle ,'Style','Style'),style.desc1 ,'')
*B609684,1 MMT 10/04/2011 import roll out orders adding employees that do not exist in the spreadsheet[END]             
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
*B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]
    lcTotQty =  lcAddedValue + lcTotQty
    Lnlineno =  Lnlineno + 1
  
    SELECT (lcordhdr)
    replace open wiTH lcTotQty ;
            book wiTH lcTotQty              
 
   *! -- Pricing code 
   
   IF !EMPTY(customer.priccode)
     =lfCodPrice()
    IF EMPTY(lnGrssPric)
       PriceLevel = CUSTOMER.PRICELVL 
       STORE 0 TO lcGros_Price,lcPrice,lcDisc_Pcnt , lcGetPrice
       lnTotQty = 1
       lcContract = ''
       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
       *lcStore = (&lcTmpXls..E)
       lcStore = ALLTRIM(&lcTmpXls..E)
       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]
       =lfSTYPRICE(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,"lcContract", lnTotQty)
       SELECT (lcOrdLine) 
       REPLACE PRICE WITH lcGetPrice ;
          gros_price WITH lcGetPrice
     ENDIF   
   ELSE 
       
       PriceLevel = CUSTOMER.PRICELVL 
       STORE 0 TO lcGros_Price,lcPrice,lcDisc_Pcnt , lcGetPrice
       lnTotQty = 1
       lcContract = ''
       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]
       *lcStore = (&lcTmpXls..E)
       lcStore = ALLTRIM((&lcTmpXls..E))
       *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]
       =lfSTYPRICE(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,"lcContract", lnTotQty)
       SELECT (lcOrdLine) 
       REPLACE PRICE WITH lcGetPrice   ;
               gros_price WITH lcGetPrice   
   ENDIF   
     
   *!---- end of pricing code  
  
 ENDFOR 
ENDSCAN 
=lfUpdHdr() && update order header 


PRIVATE loObj   && an object that imitates a formset
loObj = CREATEOBJECT('FORMSET')
WITH loObj
  .AddProperty('laEvntTrig[1,1]')
  .AddProperty('lcdeposittemp',gfTempName())
  .AddProperty('Activemode','A')  
  *C201322,1 MMT 04/06/2011 Update 3 fields in Ordhdr table[Start]
  .ADDPROPERTY('lcOldStatus',SPACE(1))
  *C201322,1 MMT 04/06/2011 Update 3 fields in Ordhdr table[End]
ENDWITH
loObj.laEvntTrig[1] = PADR('SOIMPRO',10)
llCusmsg = .T.
SELECT STYLE 
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
lcOrderNo = ''
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
SELECT(lcOrdLine)
LOCATE 
DELETE FOR TOTQTY = 0
LOCATE
IF !EOF()
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]
DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') WITH .F.,'A',lcOrdHdr,lcOrdLine,'','','',loObj
SELECT NOTEPAD
REPLACE KEY WITH lcOrderNo,;
		cdesc WITH 'Notes For Order Number : '+lcOrderNo 
lcOrdNote = Notepad.Mnotes		
=gfAdd_Info('Notepad')
=gfReplace('')		
=gfReplace('Mnotes With lcOrdNote')		
=gfTableUpdate()
IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF   
IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF 
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
ELSE
  SELECT NOTEPAD
  DELETE 
  =gfTableUpdate()
ENDIF
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]
*! --- END OF lfProXls
*!:**************************************************************************
*:* Name        : lfvAcct
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/29/2009
*:* Purpose     : Valid function for Account code
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************

FUNCTION lfvGetAcct 
IF !EMPTY(lcRpAccnt) AND _SCREEN.ActiveControl.OldValue <> lcRpAccnt
  IF !SEEK('M'+lcRpAccnt,'CUSTOMER','CUSTOMER')   && TYPE+ACCOUNT+STORE
    DO CUSBROWM WITH lcRpAccnt
  ENDIF
ENDIF
*-- end of lfvAcct.

*:**************************************************************************
*:* Name        : lfvWareHous
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2009
*:* Purpose     : check warehous
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************

FUNCTION lfvWareHous 

IF !EMPTY(lcRpWareHous) AND _SCREEN.ActiveControl.OldValue <> lcRpWareHous
  PRIVATE laTemp
  DIMENSION laTemp[1]
  laTemp = ''
  SELECT WAREHOUS
  lcBrFields = "CWARECODE  :R:H='Warehous',CDESC :R:H='Description'"
  =AriaBrow('',"Warehouses",.F.,.F.,.F.,.F.,'',.T.,'CWARECODE  ','laTemp')
  lcRpWareHous = IIF(!EMPTY(laTemp[1]),laTemp[1],'')
  
ENDIF
*-- end of lfvWareHous.
*:**************************************************************************
*:* Name        : lfvSourcDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2009
*:* Purpose     : Source folder where xls files reside
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************

*FUNCTION lfvSourcDir 

*=lfCheckDir(@lcRpSpDir)

*-- end of lfvSourcDir.
*:**************************************************************************
*:* Name        : lfvHistDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2009
*:* Purpose     : Historey folder where xls files are copied to after processing
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*!*	FUNCTION lfvHistDir 

*!*	=lfCheckDir(@lcRpSphis)

*!*	*-- end of lfvHistDir.
************************************************************
*! Name      : lfwrepwhen
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Called    : From OG
*!*************************************************************
*! Example   : =lfwrepwhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF !USED('CODES')
  =gfOpenTable(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')
ENDIF

*!*	SELECT CODES 
*!*	=gfSEEK("D"+"CORDERCAT",'Codes','CCODE_NO')
*!*	LOCATE FOR CDEFCODE+CFLD_NAME = "D"+"CORDERCAT" 
*!*	lcCatSel = ALLTRIM(CODES.CDISCREP)

*!*	lnCatPos = ASCAN(loOgScroll.laOgFXFlt,'ORDHDR.CORDERCAT')
*!*	lnCatPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnCatPos,1)
*!*	loOgScroll.laOgFXFlt[lnCatPos,6] = lcCatSel 

lcRpSpdir=gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)
lcRpSphis=gfGetMemVar('M_DIRPRO',oAriaApplication.ActiveCompanyID)

*loOGScroll.EnableObject("lcRpSphis",.F.)
*loOGScroll.EnableObject("lcRpSpDir",.F.)

*- Assign the default warehous value
SELECT WAREHOUS
LOCATE FOR LDEFWARE = .T.
IF FOUND()
  lcRpWareHous = WAREHOUS.CWARECODE
ENDIF	

*!*************************************************************
*! Name      : lfGetXlsFile
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : Importing Excel Sheet 
*!*************************************************************
*! Example   : =lfGetXlsFile()
*!*************************************************************
FUNCTION lfGetXlsFile  
LOCAL lcSaveDir
IF EMPTY(lcXlsFile) OR "?" $ lcXlsFile
  lcSaveDir = FULLPATH("")
  CD (lcRpSpDir)
  lcXlsFIle = GETFILE("XLS") 
  lcFileName = JUSTFNAME(lcXlsFIle)
  CD (lcSaveDir)
ENDIF 
RETURN lcFileName 

*End of lfGetXlsFile.

*!*************************************************************
*! Name      : lfXLSDIR
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : select xls dir 
*!*************************************************************
*! Example   : =lfXLSDIR()
*!*************************************************************

FUNCTION lfXLSDIR 
LOCAL lcSaveDir
IF EMPTY(lcRpSpDir)
  lcSaveDir = FULLPATH("")
  lcRpSpdir=gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)
  CD (lcRpSpDir)
  lcRpSpDir = GETDIR() 
  CD (lcSaveDir)
ENDIF 
RETURN lcRpSpDir 
*End of lfXLSDIR.

*!*************************************************************
*! Name      : lfXLSHisdir
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : select xls history 
*!*************************************************************
*! Example   : =lfXLSHisdir()
*!*************************************************************

FUNCTION lfXLSHisdir

LOCAL lcSaveDir
IF EMPTY(lcRpSphis)
  lcSaveDir = FULLPATH("")
  lcRpSphis=gfGetMemVar('M_DIRPRO',oAriaApplication.ActiveCompanyID)
  CD (lcRpSphis)
  lcRpSphis = GETDIR() 
  CD (lcSaveDir)
ENDIF 
RETURN lcRpSphis 
*End of lfXLSHisdir.
*!*************************************************************
*! Name      : lfCreaTemp  
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : Create Temp file to collect data FOR log error
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp   

*-- check If File is created or not
IF USED(lcTemLog) AND RECCOUNT(lcTemLog) > 0
 USE IN (lcTemLog)
ENDIF

*-- Create File
IF !USED(lcTemLog)
      
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'LineNo'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'desc'
  laTempStru[lnI,2] = 'C'
  *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[Start]           
  *laTempStru[lnI,3] = 100
  laTempStru[lnI,3] = 200
  *B609001,1 MMT 09/10/2009 fix bug of error log format of ROLL out program[End]           
  laTempStru[lnI,4] = 0
        
=gfCrtTmp(lcTemLog,@laTempStru,'LineNo')
ENDIF  

*-- end of lfCreaTemp  
*:**************************************************************************
*:* Name        : lfProcess
*:* Developer   : Mostsfa eid 
*:* Date        : 03/29/2009
*:* Purpose     : CHECKING THE INPUTS 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfProcess 
*- checking if the there is a missing data
LOCAL lcMsg
lcMsg = ''
DO CASE

CASE !FILE(lcXlsFIle)
  lcMsg = 'File Not Found'

CASE EMPTY(lcRpAccnt)
  lcMsg = 'You should enter an accout code'

CASE EMPTY(lcRpWareHous)
  lcMsg = 'You should enter a warehous code'

CASE EMPTY(lcContref)
  lcMsg = 'You should enter a contact refrence value'

CASE EMPTY(lcRpSpdir)
  lcMsg = 'You should enter the Source folder'

CASE EMPTY(lcRpSphis)
  lcMsg = 'You should enter the history folder'

ENDCASE

IF !EMPTY(lcMsg)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  RETURN .F.
ENDIF

*! --  Calling main functions 
IF llOGFltCh
  =lfMain()
ENDIF 
RETURN .F.
*!*************************************************************
*! Name      : lfGetQty
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : GET quantites using scale file 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfGetQty()
*!*************************************************************
FUNCTION lfGetQty  
PARAMETERS lcScal,lcSize ,lcfit
lcqty = ""
SELECT SCALE
gfSetOrder("SCALE")
LOCATE FOR SCALE = ALLTRIM(UPPER(lcScal)) AND SCALE.CDIM1 = PADR(lcfit,5)
SCAN REST WHILE "S"+ SCALE = "S"+ALLTRIM(UPPER(lcScal)) FOR  SCALE.CDIM1 = PADR(lcfit,5)
  IF  SCALE.CNT = 1 AND EMPTY(lcqty)
    lcqty  = "QTY1"
    lcbook = "book1"    
    lcFullscale= scale.scale 
  ELSE     
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      *B610209,1 HIA 01/22/2013 SO - Import Sales Order Rollout putting wrong sizes on sales order lines DIRECT CORPORATE [T20130115.0304][Start]
      *IF ALLTRIM(SCALE.sz&lcI.) <> ALLTRIM(lcSize)
      *  LOOP 
      *ENDIF 
      *lcqty  = "qty&lcI."
      *lcbook = "book&lcI."
      *lcFullscale= scale.scale 
      
      IF ALLTRIM(SCALE.sz&lcI.) == ALLTRIM(lcSize)
        lcqty  = "qty&lcI."
        lcbook = "book&lcI."
        lcFullscale= scale.scale 
        EXIT
      ENDIF 
      *B610209,1 HIA 01/22/2013 SO - Import Sales Order Rollout putting wrong sizes on sales order lines DIRECT CORPORATE [T20130115.0304][End]
    ENDFOR 
  ENDIF   
ENDSCAN 
RETURN lcqty

*!----- end of lfGetQty
*!*************************************************************
*! Name      : lfStyPrice 
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : get style price 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfStyPrice ()
*!*************************************************************

FUNCTION lfStyPrice 
PARAMETERS lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,lcContract, lnTotQty

lcStyle = PADR(lcStyle,19)
=gfseek(lcStyle,'STYLE','STYLE')
  *-- Get Style Gross Price
  lcGros_Price = gfGetprice(lcStyle,PriceLevel,lnTotQty,&lcOrdHDr..cCurrCode)
  IF lcGros_Price = 0
    IF PriceLevel = 'Q'
      DO CASE
        CASE Style.nAtQtyC > 0 AND lnTotQty > Style.nAtQtyC
          lcLevel = 'C'
        CASE Style.nAtQtyB > 0 AND lnTotQty > Style.nAtQtyB
          lcLevel = 'B'
        OTHERWISE
          lcLevel = 'A'
      ENDCASE
    ELSE
      lcLevel=IIF(INLIST(PriceLevel,'A','B','C'),PriceLevel,'A')
    ENDIF
      IF lcGros_Price <> 0
       lcGros_Price = lfCheckPri(lcStyle,lcLevel,&lcOrdHDr..cCurrCode)
      ENDIF 
  ENDIF  

  *-- get style discount
   lcDiscCode  = IIF(SEEK(lcStyle+&lcOrdHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
   m.Disc_Pcnt = 0 
  IF !EMPTY(lcDiscCode)
    *-- Get discount type, start date, end date, and discount percent
    DECLARE laDisRltFld[4,2]
    STORE '' TO lcDisType
    STORE {} TO ldstartDte, ldEndDate
    STORE 0  TO lnDisc_Pcnt
    laDisRltFld[1,1] = 'CCOSTAFECT'
    laDisRltFld[1,2] = 'lcDisType'
    laDisRltFld[2,1] = 'START'
    laDisRltFld[2,2] = 'ldstartDte'
    laDisRltFld[3,1] = 'DENDATE'
    laDisRltFld[3,2] = 'ldEndDate'
    laDisRltFld[4,1] = 'DISCPCNT'
    laDisRltFld[4,2] = 'lnDisc_Pcnt'
    =gfRltFld(lcDiscCode, @laDisRltFld, 'CDISCCODE')
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(&lcOrdHDr..Entered,ldstartDte,ldEndDate)
      lcDisc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
lcGetPrice = lcGros_Price*(100-lcDisc_Pcnt)/100

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : TMI
*! Date      : 04/03/2001
*! Purpose   : To get color position also color length
*!*************************************************************
*! Called from : ICSLCAT1.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)

*--end function lfGetClrD
*:***************************************************************************
*:* Example     :  = lfCodPrice()
*:***************************************************************************

FUNCTION lfCodPrice
lnSlct = SELECT()
=lfOpenTbls()

lcPricCursr = gfTempName()
STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()    

STORE '' TO  lcPricCode
*-- loop here on sizes to gather them into price-groups
*- Get the price code
*B609524,1 MMT 02/16/2011 SO Import Roll out spread sheet: Currency is Hardcoded[T20110120.0003][Start]
*lcCurrCode = "GBP"
lcCurrCode = CUSTOMER.cCurrCode
*B609524,1 MMT 02/16/2011 SO Import Roll out spread sheet: Currency is Hardcoded[T20110120.0003][End]
lcPricCode = lfPRICCODE()

*- Get the currency code
IF SEEK(lcPricCode+lcCurrCode+&lcordline..Style,'CSTPRICE')
  CREATE CURSOR &lcPricCursr (SIZES C(8),Gros_Price N(12,2),COMMDV N(12,2),;
         QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(6))
  INDEX ON Gros_Price TAG Gros_Price
  LOCAL lnI,lcI
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    IF !EMPTY(&lcordline..QTY&lcI)
      *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 ) 
      lnGrssPric = IIF(CSTPRICE.PRICE&lcI<>0,CSTPRICE.PRICE&lcI,;
                   IIF(CSTPRICE.PRICEDV<>0  ,CSTPRICE.PRICEDV,  ;
                                             &lcordline..GROS_PRICE))
      lnStyComm  = IIF(STYLE.COMMISSION,CSTPRICE.COMMDV,0)
    
      *- Add a line per price to the temp ordline file
      IF !SEEK(lnGrssPric,lcPricCursr)
        INSERT INTO &lcPricCursr (SIZES,Gros_Price,COMMDV,QTY&lcI,TOTQTY) VALUES (lcI,lnGrssPric,lnStyComm,&lcordline..QTY&lcI,&lcordline..QTY&lcI)
      ELSE
        SELECT &lcPricCursr 
        REPLACE QTY&lcI WITH &lcordline..QTY&lcI ;
                TOTQTY  WITH TOTQTY + QTY&lcI       ;
                SIZES   WITH ALLTRIM(SIZES)+lcI
      ENDIF
    ENDIF
  ENDFOR
  
   *- save the current line data to memo variables and Delete it.
  lnLnCnt = 1
  SELECT &lcordline
  SCATTER MEMVAR MEMO FIELDS EXCEPT QTY*
  
  SELECT &lcPricCursr
  SCAN
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,GROS_PRICE MEMVAR
    SELECT &lcordline
    
    && if only one price group then just update the line , otherwise add extra lines for groups other than the first one
    IF lnLnCnt>1  
      APPEND BLANK
      SELECT &lcOrdHdr     
      REPLACE LASTLINE WITH LASTLINE + 1
      m.LINENO = &lcOrdHdr..LASTLINE
      SELECT &lcordline
    ENDIF
    
    GATHER MEMVAR MEMO
    REPLACE PRICE  WITH GROS_PRICE*(100-DISC_PCNT)/100 ;
            COWNER WITH &lcPricCursr..SIZES ;
            BOOK1  WITH QTY1 ;
            BOOK2  WITH QTY2 ;
            BOOK3  WITH QTY3 ;
            BOOK4  WITH QTY4 ;
            BOOK5  WITH QTY5 ;
            BOOK6  WITH QTY6 ;
            BOOK7  WITH QTY7 ;
            BOOK8  WITH QTY8 ;
            TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 ;
            CSTSZPRICE WITH &lcPricCursr..SIZES
    lnLnCnt = lnLnCnt + 1
   
   ENDSCAN
  
  *- Release the temp cursor
  USE IN &lcPricCursr
 
ENDIF
SELECT (lnSlct)
RETURN  lnGrssPric   
*-- end of lfCodPrice.

*:**************************************************************************
*:* Name        : lfOpenTbls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/01/2007
*:* Purpose     : open needed tables
*:***************************************************************************
FUNCTION lfOpenTbls

IF !USED('SycCurr')
  =gfOpenTable(oAriaApplication.SysPath+'SycCurr',oAriaApplication.SysPath+'cCurrCode','SH')
ENDIF
IF !USED('CSTPRICH')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICH','CSTPRICH','SH')
ENDIF
IF !USED('CSTPRICE')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF
*-- end of lfOpenTbls.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/02/2003
*:* Purpose     : get Customer Price code 
*:***************************************************************************
FUNCTION lfPRICCODE
LOCAL lcSlct,lcPricCode,lcSvKey
lcSlct = SELECT()

SELECT CUSTOMER
lcAccount = lcRpAccnt

lcSvKey = EVALUATE(KEY())
=gfSEEK('M'+lcAccount,'CUSTOMER')

LOCAL lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = CHR(255)
  
*- Download needed lines to seek in from cstprice and cstprich sql files
SELECT CSTPRICE
=gfSetOrder('CSTYCODE')
=gfSEEK(&lcordline..STYLE,'CSTPRICE')
=gfSetOrder('CSTPRICE')

PRIVATE lcStyClr
lcStyClr = PADR(SUBSTR(&lcordline..STYLE,1,lnClrPos+lnClrLen-1),19) 
SELECT CSTPRICH
=gfSetOrder('STYLE')
=gfSEEK(lcStyClr,'CSTPRICH')
=gfSetOrder('CSTPRICH')
  
FOR lnI = 1 TO 15
  lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') ) 
    
  IF !EMPTY(CUSTOMER.PRICCODE&lcI).AND. SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+&lcordline..STYLE,'CSTPRICE') ;
                                  .AND. SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    IF EMPTY(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = UPPER(CUSTOMER.PRICCODE&lcI)
    ELSE
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      IF BETWEEN(ldStartDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        EXIT
      ENDIF

    ENDIF
  ENDIF    
ENDFOR

lcPRICCODE = IIF(lnI < 16 , UPPER(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'CUSTOMER')

SELECT (lcSlct)
RETURN lcPRICCODE
*-- end of lfPRICCODE.
*:**************************************************************************
*:* Name        : lfUpdHdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Update the lcOrdHdr file
*:***************************************************************************
FUNCTION lfUpdHdr

LOCAL lnLastLine,lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
SELECT &lcordline
LOCATE
CALCULATE SUM(TOTQTY),SUM(TOTQTY*PRICE),SUM(TOTBOOK),SUM(TOTBOOK*PRICE) TO lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
GO BOTTOM
SELECT (lcOrdhdr)
REPLACE OPEN     WITH lnOpenQty  ;
        OPENAMT  WITH lnOpenPrice ;
        BOOK     WITH lnBookQty ;
        BOOKAMT  WITH lnBookAmt

*-- end of lfUpdHdr.

*:**************************************************************************
*:* Name        : lfCustMsg  
*:* Developer   :  mos 
*:* Date        : 11/14/2007
*:* Purpose     : display so saving cust msf
*:* called from : SOUPDATE  
*:***************************************************************************
FUNCTION lfCustMsg  
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[Start]
*lcMsg = 'Sales Order  '+ lcOrderNo + '  has been Created For spread sheet '+ lcFileName 
IF !LLRPMLST
  *C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
  IF TYPE('lcOrderNo') = 'C'
  *C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]  
    IF EMPTY(laOrderCreated[1])
	  laOrderCreated[1]= lcOrderNo 
	ELSE
	  DIMENSION laOrderCreated[ALEN(laOrderCreated,1)+1]
	  laOrderCreated[ALEN(laOrderCreated,1)]= lcOrderNo 
	ENDIF 
  *C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]	
  ENDIF 
  *C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]  
  IF !llLastOrder 
    RETURN 
  ELSE
   IF ALEN(laOrderCreated,1) > 1
     lcMsg = 'Sales Orders from  '+ laOrderCreated[1]+' to '+laOrderCreated[ALEN(laOrderCreated,1)] +'  have been Created For spread sheet '+ lcFileName     
   ELSE
     lcMsg = 'Sales Order  '+ laOrderCreated[1]+ '  has been Created For spread sheet '+ lcFileName     
   ENDIF    
  ENDIF 
ELSE
  lcMsg = 'Sales Order  '+ lcOrderNo + '  has been Created For spread sheet '+ lcFileName  
ENDIF   
*C201228,1 MMT 03/24/2010 Add New Option to create Multi Store order or order/Store[End]

=gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
lcRpSphis = IIF(RIGHTC(ALLTRIM(lcRpSphis),1)="\",ALLTRIM(lcRpSphis),ALLTRIM(lcRpSphis)+"\")
lcEndName = ALLTRIM(lcRpSphis)+UPPER(lcRpAccnt)+"-"+UPPER(ALLTRIM(lcFileName))
IF FILE(lcEndName)
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,'Spread Sheet With the Name  '+lcRpAccnt+"-"+ALLTRIM(lcFileName)+'  already Exists, Replace ?') = 1 
    DELETE FILE (lcEndName)
    RENAME (lcXlsFIle) TO (lcEndName)	
  ELSE 
	RETURN .F. 
  ENDIF    
ELSE 
  RENAME (lcXlsFIle) TO (lcEndName)
ENDIF 
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[Start]
llMessageCalled  = .T.
*C201228,2 MMT 04/28/2010 Fix error while saving and lines have qty in tmp ordline[End]
*:**************************************************************************
*:* Name        : lflfChkDate 
*:* Developer   :  mos 
*:* Date        : 11/14/2007
*:* Purpose     : ENSURE THAT COMPLETE DATE IS LATER THAN START DATE
 
*:***************************************************************************
FUNCTION lfChkDate

IF MAX(ldRpStartDat,ldRpComDat)=ldRpStartDat
 lcMsg = 'Complete Date Must Be later than Start Date' 
 =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)  
ENDIF 


