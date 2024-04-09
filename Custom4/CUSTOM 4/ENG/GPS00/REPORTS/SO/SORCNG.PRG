*:***************************************************************************
*: Program file  : SORCNG
*: Program desc. : Order Confirmation With margin
*: For Report    : SORCNGP.FRX
*: System        : Aria Advantage Series 4XP.
*: Module        : Sales Order (SO) 04/08/2008
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
*: This Program is due to C200979...
*:***************************************************************************
*: Modifications:
*: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [T20061201.0001] 
*: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[T20081008.0008]
*: E303095,1 HIA 03/26/2012 Order margin confirmation form [T20120118.0009]
*: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011]
*:***************************************************************************
lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
lcOrdsNum=''
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llPrintBox = .F.                        && Box around Notes.
llPrtCmpdt = gfGetMemVar('M_CMPDOLN')  && Checking Comp. Date by Order line. used in the complete date in the frx form A.
llSalsOrd = .T.          && llarpinv it will be a commen variable.

loOGScroll.cCROrientation='P'

IF !llFrTime
  lcFormName = lfGetForm()
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcFormName,'',llOGRefForm)  && Create Temp. file for new form.
ENDIF
llNoRec= .T.
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)

llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = llRpOrdLnt AND llRpOrdNot  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval 
llTitle    = .T.                        && Flag to print Detail header.

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'
IF llFrTime
  lcStyTitle = gfItemMask('HI')        && Title of the style.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF gfSEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     gfSEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF

*-- Note that the files was opened in Rep. Gen.
SELECT ORDHDR

DIMENSION laFilStruc[1,4]
DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate,lnTrmPcnt,lnA,lnB,lnTotal,lnDsc,lnTOTDisc,lnTotTrm ,lnAllDis
STORE '' TO laEngStyTax,lcAlis,lcOrd
DECLARE laEngStyTax[1,2]
laEngStyTax[1,1] = 'NTAXRATE'
laEngStyTax[1,2] = 'lnTaxRate'
STORE 0 TO lnTaxQty,lnTaxRate
SELECT ORDLINE
= AFIELDS(laFilStruc)

lnFileStru = ALEN(laFilStruc,1)
lnlastFl = lnFileStru 
lnFrst = lnFileStru 
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'lnVat'
laFilStruc[lnFileStru+1,2] = 'N'
laFilStruc[lnFileStru+1,3] = 12
laFilStruc[lnFileStru+1,4] = 2
lnlastFl = lnlastFl + 1

lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'lnBset'
laFilStruc[lnFileStru+1,2] = 'N'
laFilStruc[lnFileStru+1,3] = 12
laFilStruc[lnFileStru+1,4] = 2
lnlastFl = lnlastFl + 1

lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'lnAset'
laFilStruc[lnFileStru+1,2] = 'N'
laFilStruc[lnFileStru+1,3] = 12
laFilStruc[lnFileStru+1,4] = 2
lnlastFl = lnlastFl + 1

lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'lcDsc'
laFilStruc[lnFileStru+1,2] = 'C'
laFilStruc[lnFileStru+1,3] = 14
laFilStruc[lnFileStru+1,4] = 0
lnlastFl = lnlastFl + 1

lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'lnTrm'
laFilStruc[lnFileStru+1,2] = 'N'
laFilStruc[lnFileStru+1,3] = 14
laFilStruc[lnFileStru+1,4] = 2
lnlastFl = lnlastFl + 1


*: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start]
lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+1,18]
laFilStruc[lnFileStru+1,1] = 'CSTYGROUP'
laFilStruc[lnFileStru+1,2] = 'C'
laFilStruc[lnFileStru+1,3] = 6
laFilStruc[lnFileStru+1,4] = 0
lnlastFl = lnlastFl + 1
*: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End]


IF TYPE('lcMargFrm')='C' AND lcMargFrm = 'Y'
  STORE .T. TO llMargFrm
  =lfUsrVldFn('lfAddFlds','SOORCNMO')
ELSE
  STORE .F. TO llMargFrm
ENDIF

FOR lnLoop = lnFrst  to  lnlastFl 
  STORE ' ' TO  laFilStruc[lnLoop,7],laFilStruc[lnLoop,8],;
        laFilStruc[lnLoop,9],laFilStruc[lnLoop,10],;
        laFilStruc[lnLoop,11],laFilStruc[lnLoop,12],;
        laFilStruc[lnLoop,13],laFilStruc[lnLoop,14],;
        laFilStruc[lnLoop,15],laFilStruc[lnLoop,16]
  STORE 0 TO    laFilStruc[lnLoop,17] ,laFilStruc[lnLoop,18]
ENDFOR


IF llMargFrm
  lcRcpIdx =lcTmpIndx
ELSE
  IF lcRpSortBy = 'S'
    lcRcpIdx ="CORDTYPE + ORDER + STORE + STYLE "
  ELSE
    lcRcpIdx ="CORDTYPE + ORDER + STORE + STR(LINENO,6)"
  ENDIF
ENDIF


gfCrtTmp(lcTempOrd,@laFilStruc,lcRcpIdx,lcTempOrd,.F.)

DIMENSION laFilStr[8,4]

laFilStr[1,1] ='CordType'
laFilStr[1,2] ='C'
laFilStr[1,3] =1
laFilStr[1,4] =0

laFilStr[2,1] ='COrdr'
laFilStr[2,2] ='C'
laFilStr[2,3] =6
laFilStr[2,4] =0

laFilStr[3,1] ='ACCOUNT'
laFilStr[3,2] ='C'
laFilStr[3,3] =6
laFilStr[3,4] =0

laFilStr[4,1] ='NDue10'
laFilStr[4,2] ='N'
laFilStr[4,3] =14
laFilStr[4,4] =2

laFilStr[5,1] ='NDue30'
laFilStr[5,2] ='N'
laFilStr[5,3] =14
laFilStr[5,4] =2

laFilStr[6,1] ='NDue60'
laFilStr[6,2] ='N'
laFilStr[6,3] =14
laFilStr[6,4] =2


laFilStr[7,1] ='NDue61'
laFilStr[7,2] ='N'
laFilStr[7,3] =14
laFilStr[7,4] =2


laFilStr[8,1] ='MPiktkts'
laFilStr[8,2] ='M'
laFilStr[8,3] =10
laFilStr[8,4] =0


gfCrtTmp(lcTempAge,@laFilStr,"CordType+COrdr+ACCOUNT",lcTempAge,.F.)

IF lcRpOrdTyp = "T"

  DIMENSION laFileStruct[16,4]
  laFileStruct[1,1] = 'Account'
  laFileStruct[1,2] = "C"
  laFileStruct[1,3] = 6 
  laFileStruct[1,4] = 0
  
  laFileStruct[2,1] = 'Style'
  laFileStruct[2,2] = 'C'
  laFileStruct[2,3] = 19
  laFileStruct[2,4] = 0

  laFileStruct[3,1] = 'Qty1'
  laFileStruct[3,2] = 'N'
  laFileStruct[3,3] = 7
  laFileStruct[3,4] = 0

  laFileStruct[4,1] = 'Qty2'
  laFileStruct[4,2] = 'N'
  laFileStruct[4,3] = 7
  laFileStruct[4,4] = 0

  laFileStruct[5,1] = 'Qty3'
  laFileStruct[5,2] = 'N'
  laFileStruct[5,3] = 7
  laFileStruct[5,4] = 0

  laFileStruct[6,1] = 'Qty4'
  laFileStruct[6,2] = 'N'
  laFileStruct[6,3] = 7
  laFileStruct[6,4] = 0

  laFileStruct[7,1] = 'Qty5'
  laFileStruct[7,2] = 'N'
  laFileStruct[7,3] = 7
  laFileStruct[7,4] = 0

  laFileStruct[8,1] = 'Qty6'
  laFileStruct[8,2] = 'N'
  laFileStruct[8,3] = 7
  laFileStruct[8,4] = 0

  laFileStruct[9,1] = 'Qty7'
  laFileStruct[9,2] = 'N'
  laFileStruct[9,3] = 7
  laFileStruct[9,4] = 0

  laFileStruct[10,1] = 'Qty8'
  laFileStruct[10,2] = 'N'
  laFileStruct[10,3] = 7
  laFileStruct[10,4] = 0

  laFileStruct[11,1] = 'TotQty'
  laFileStruct[11,2] = 'N'
  laFileStruct[11,3] = 8
  laFileStruct[11,4] = 0

  laFileStruct[12,1] = 'Amount'
  laFileStruct[12,2] = 'N'
  laFileStruct[12,3] = 13
  laFileStruct[12,4] = 2

  laFileStruct[13,1] = 'nSRP'
  laFileStruct[13,2] = 'N'
  laFileStruct[13,3] = 8
  laFileStruct[13,4] = 2

  laFileStruct[14,1] = 'Scale'
  laFileStruct[14,2] = 'C'
  laFileStruct[14,3] = 3
  laFileStruct[14,4] = 0

  laFileStruct[15,1] = 'note1'
  laFileStruct[15,2] = 'C'
  laFileStruct[15,3] = 30
  laFileStruct[15,4] = 0

  laFileStruct[16,1] = 'note2'
  laFileStruct[16,2] = 'C'
  laFileStruct[16,3] = 20
  laFileStruct[16,4] = 0

  gfCrtTmp(lcTempRcp,@laFileStruct,"Account+Style",lcTempRpt ,.F.)
ENDIF


SELECT (lcTempOrd)
SELECT ORDHDR
=AFIELDS(laOrdHdStr)
gfCrtTmp(lcORDHDR,@laOrdHdStr,"cOrdType+ORDER",lcORDHDR ,.F.)


lcEdiExpr = IIF(lcRpEDIFlt='O',[!ORDHDR.lEDIOrder],IIF(lcRpEDIFlt='E',[ORDHDR.lEDIOrder],""))
IF !EMPTY(lcEdiExpr)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
    lcRpExp = lcRpExp + lcEdiExpr
  ENDIF
  
  lcRpExp = lcRpExp + IIF(lcRpOrdTyp <> 'A' ," AND ordhdr.CordType = '"+lcRpOrdTyp+"'",'')+;
            IIF(lcRpOrdSta <> 'A' ," AND ordhdr.Status = '"+lcRpOrdSta +"'",'')
  
  
  SCAN FOR &lcRpExp
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcORDHDR) FROM MEMVAR 
    SELECT CODES
    gfSetOrder('CODES')
    =gfSEEK ('N'+ORDHDR.CTERMCODE+'Y',"CODES")
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+ORDHDR.CTERMCODE+'Y'
      IF crltd_nam='NTERDISCR'
        lnTrmPcnt=VAL(CODES.crltd_vlu)
        LOOP
      ENDIF
    ENDSCAN
    STORE 0 TO lnDu10,lnDu30,lnDu60,lnDu61
    lcPiktkt=''
    lcExSin1 = ' '
    lcExSin2 = ' '
    SELECT DEBIT
    IF gfSEEK (ORDHDR.ACCOUNT)
      SCAN REST WHILE account+tran+cinstalno+DTOS(trandate)=ORDHDR.ACCOUNT
        IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency                     
          lcExSin1    = gfGetExSin(@lcExSin2,DEBIT.CCURRCODE,oAriaApplication.BaseCurrency)
          lnExRate    = DEBIT.NEXRATE           
        ENDIF  
        IF TRANTYPE='3' AND EMPTY(DUEDATE)
          DO CASE
            CASE BETWEEN (trandate,oAriaapplication.SystemDate, oAriaapplication.SystemDate+10) 
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency                     
                lnDu10=lnDu10+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu10=lnDu10+AMOUNT
              ENDIF  
        
            CASE BETWEEN (oAriaapplication.SystemDate,trandate+1, trandate+30)  
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu30=lnDu30+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu30=lnDu30+AMOUNT
              ENDIF  
              
            CASE BETWEEN (oAriaapplication.SystemDate,trandate+31, trandate+59) 
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu60=lnDu60+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu60=lnDu60+AMOUNT
              ENDIF  
              
            CASE oAriaapplication.SystemDate>trandate+59
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu61=lnDu61+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu61=lnDu61+AMOUNT
              ENDIF  
           ENDCASE
        ELSE
          DO CASE
            CASE BETWEEN (DUEDATE,oAriaapplication.SystemDate, oAriaapplication.SystemDate+10)  
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu10=lnDu10+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu10=lnDu10+AMOUNT
              ENDIF
  
            CASE BETWEEN (oAriaapplication.SystemDate,DUEDATE+1, DUEDATE+30) 
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu30=lnDu30+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu30=lnDu30+AMOUNT
              ENDIF  
        
            CASE BETWEEN (oAriaapplication.SystemDate,DUEDATE+31, DUEDATE+59) 
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu60=lnDu60+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu60=lnDu60+AMOUNT
              ENDIF  
             
            CASE oAriaapplication.SystemDate>DUEDATE+59
              IF DEBIT.CCURRCODE # oAriaApplication.BaseCurrency  
                lnDu61=lnDu61+ROUND(DEBIT.AMOUNT &lcExSin1 lnExRate ,2)
              ELSE
                lnDu61=lnDu61+AMOUNT
              ENDIF  
          ENDCASE
        ENDIF
     ENDSCAN  
  ENDIF  
  SELECT ORDLINE
  =gfSeek(ORDHDR.CORDTYPE+ORDHDR.ORDER)
  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SCATT MEMVAR MEMO
    IF llMargFrm
      =lfUsrVldFn('lfRepFlds','SOORCNMO')
    ENDIF
    
    *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start]
    gfSeek(m.Style,'Style','Style')
    m.CstyGroup = Style.CstyGroup
    *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End]
    
    INSERT INTO (lcTempOrd) FROM MEMVAR
    lcAlis=ALIAS()
    SELECT CUSTOMER
    lcOrd=ORDER()
    gfSetORder('CUSTOMER')
    
    *E303095,1 HIA 03/26/2012 Order margin confirmation form [BEGIN]
    *gfSEEK ('M'+ORDHDR.Account)
    
    SELECT CUSTOMER
    IF !EMPTY(ALLTRIM(ORDLINE.STORE))
      gfSEEK ('S'+ORDHDR.Account+ORDLINE.STORE)
    ELSE
      gfSEEK ('M'+ORDHDR.Account)
    ENDIF   
    
    *E303095,1 HIA 03/26/2012 Order margin confirmation form [END]
    
    gfSetORder(lcOrd)
    SELECT STYLE
    lcOrd=''
    lcOrd=ORDER()
    gfSetOrder('STYLE')
    gfSEEK(ORDLINE.Style)
    gfSetOrder(lcOrd)
    IF !Customer.lVatExem  .AND. Style.nTaxBreak <> 0 
    
      *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start]
      *=gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE') 
      lcAlias = ALIAS()
      SELECT Codes
      gfSetOrder('Codes')
      IF gfSEEK('N'+Style.cTaxCode +'Y'+'CTAXCODE')
        SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+Style.cTaxCode +'Y'+'CTAXCODE'
          IF crltd_nam = 'NTAXRATE'
            lnTaxRate = VAL(crltd_vlu)
          ENDIF
        ENDSCAN
      ENDIF
      SELECT (lcAlias)
      *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End]
      
    ENDIF
    lnTaxQty=0  
    IF !Customer.lVatExem  .AND. Style.nTaxBreak <> 0 
      FOR lnCount = Style.nTaxBreak TO 8
        lnTaxQty = lnTaxQty + EVAL(lcTempOrd+'.Qty'+STR(lnCount,1))
      ENDFOR
    ENDIF
    IF ORDLINE.DISC_PCNT=0
      IF STYLE.PRICEA<>0
        lnDsc=IIF((STYLE.PRICEA-ORDLINE.PRICE)=0,0,(STYLE.PRICEA-ORDLINE.PRICE))/(STYLE.PRICEA)*100
        lnDsc=IIF(lcRpDeciml='Y',TRANSFORM(lnDsc ,'999999.99'),TRANSFORM(lnDsc ,'999999999'))+'NP'   
      ELSE
        lnDsc=0
        lnDsc=IIF(lcRpDeciml='Y',TRANSFORM(lnDsc ,'999999.99'),TRANSFORM(lnDsc ,'999999999'))+'NP'   
      ENDIF  
    ELSE
*: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start] 
*!*        IF STYLE.PRICEA<>0
*!*          lnDsc=ORDLINE.DISC_PCNT
*!*          lnDsc=IIF(lcRpDeciml='Y',TRANSFORM(lnDsc ,'999999.99'),TRANSFORM(lnDsc ,'999999999'))  
*!*        ELSE
*!*          lnDsc=0
*!*          lnDsc=IIF(lcRpDeciml='Y',TRANSFORM(lnDsc ,'999999.99'),TRANSFORM(lnDsc ,'999999999'))  
*!*        ENDIF
      lnDsc=ORDLINE.DISC_PCNT
      lnDsc=IIF(lcRpDeciml='Y',TRANSFORM(lnDsc ,'999999.99'),TRANSFORM(lnDsc ,'999999999'))  
*: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End] 
    ENDIF
    *: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[Start]
    *lnTotal=ORDLINE.TOTQTY*ORDLINE.PRICE
    lnTotal=(ORDLINE.TOTQTY*ORDLINE.PRICE)/ordhdr.nexrate
    *: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[end]
    IF lnTotal<>0
      lnB=IIF(((lnTotal*(100-ORDHDR.DISC)/100)-(STYLE.AVE_COST*ORDLINE.TOTQTY))=0,0,((lnTotal*(100-ORDHDR.DISC)/100)-(STYLE.AVE_COST*ORDLINE.TOTQTY))/(lnTotal)*100)
      lnTOTDisc=lnTotal*(100-ORDHDR.DISC)/100
      lnTotTrm=lnTotal*(lnTrmPcnt)/100
      lnAllDis=lntotdisc-lntottrm
      lnA=IIF(((lnAllDis)-(STYLE.AVE_COST*ORDLINE.TOTQTY))=0,0,((lnAllDis)-(STYLE.AVE_COST*ORDLINE.TOTQTY))/(lnTotal)*100)
    ELSE
      lnB=0
      lnA=0
    ENDIF               
    SELECT (lcTempOrd)
    REPLACE lnVat WITH lnTaxQty * ORDLINE.Price * lnTaxRate/100
    REPLACE lnTrm WITH lnTrmPcnt
    REPLACE lnBset WITH lnB 
    REPLACE lnAset WITH lnA
    REPLACE lcDsc WITH lnDsc
    SELECT (lcAlis)
    IF lcRpOrdTyp = "T"
      SELECT (lcTempRcp)
      IF !SEEK(m.Account+m.Style)
        INSERT INTO (lcTempRcp) (Account,Style,nSRP,Scale) VALUES ;
                                (m.Account,m.Style,m.nSugRetPri,m.Scale)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
              Qty2   WITH Qty2   + m.Qty2 ,;
              Qty3   WITH Qty3   + m.Qty3 ,;
              Qty4   WITH Qty4   + m.Qty4 ,;
              Qty5   WITH Qty5   + m.Qty5 ,;
              Qty6   WITH Qty6   + m.Qty6 ,;
              Qty7   WITH Qty7   + m.Qty7 ,;
              Qty8   WITH Qty8   + m.Qty8 ,;
              TotQty WITH TotQty + m.TotQty ,;
              Amount WITH Amount + m.TotQty*m.Price
    ENDIF
    
    *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start] 
    *lcPiktkt=IIF(PIKTKT='******','',PIKTKT)+IIF(EMPTY(PIKTKT),'',',')+lcPiktkt
    IF PIKTKT <> '******' AND !(PIKTKT $ lcPiktkt)
      lcPiktkt=IIF(PIKTKT='******','',PIKTKT)+IIF(EMPTY(PIKTKT),'',',')+lcPiktkt
    ENDIF   
    *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End] 
  ENDSCAN
  SELECT (lcTempAge)
  APPEND BLANK
  REPLACE CordType   WITH ORDHDR.CordType ,;
          CORDR      WITH ORDHDR.Order    ,;
          ACCOUNT    WITH ORDHDR.ACCOUNT  ,;
          NDue10     WITH lnDu10         ,;
          NDue30     WITH lnDu30         ,;
          NDue60     WITH lnDu60         ,;
          NDue61     WITH lnDu61         ,;
          MPiktkts   WITH lcPiktkt       
ENDSCAN
IF llMargFrm
  =lfUsrVldFn('lfChcFlds','SOORCNMO')
ENDIF

SELECT (lcORDHDR)
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE


SELECT (lcTempOrd)
SET FILTER TO TotQty != 0

*!*  SELECT OBJLINK_A
*!*  SET RELATION TO cobject_id INTO OBJECTS_A ADDITIVE
*--- Relation between opened files [end]

IF llFrTime
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laCompAdd = ''                    && Array to hold the Company address
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  lcCompFax  = cCom_fax              && Variable to hold the Company Fax
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Load Company address.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
ENDIF

lcSkipExpr  = [&lcTempOrd]

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created 
*--           : one for every optional grid seasson run.
lcNoteLns = IIF(EMPTY(lcNoteLns),loogscroll.gfTempName(),lcNoteLns)

*-- if you don't find temp. file, create it if you have both types of notes.
IF !USED(lcNoteLns) AND llPrntBoth
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

IF llPrntBoth
  SELECT (lcTempOrd)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [&lcTempOrd,&lcNoteLns]
ENDIF

*-- if we are in case of print both types of notes.  [begin]

*-- Select Master report file.
SELECT (lcORDHDR)
SET SKIP TO &lcSkipExpr
lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcTempOrd) = lnLastRec ,.T.),.T.)]

lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) + IIF(lcRpOrdTyp <> 'A' ," AND ordhdr.CordType = '"+lcRpOrdTyp+"'",'')


lcRpExp    = STRTRAN(UPPER(lcRpExp ),'ORDHDR','&lcOrdhdr')

=lfOptProg()
IF llMargFrm
  =lfUsrVldFn('lfDntPrn','SOORCNMO')
ENDIF


SELECT (lcORDHDR)
LOCATE 


IF lcRpOrdTyp <> "T" OR !llRcpOnly 
  IF llIsAparel
    =gfSetDevice()
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    *--
    lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
    *--
    IF llSalsOrd
      DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
    ENDIF
  ENDIF  
ENDIF
SELECT(lcORDHDR)
SET RELATION TO


IF lcRpOrdTyp = "T"
  lcRCFrmNam = lfGetForm('RC')
  lcPrgName  = lcRCFrmNam
  llIsAparel = lfIsApparl(@lcPrgName)
  =lfOptProg()
  SELECT (lctemprcp)
*!*    SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
*!*    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
*!*    SET RELATION TO Style INTO Style ADDITIVE
*!*    SET RELATION TO 'M'+Account INTO Customer ADDITIVE
  lcOldForm = lcOGTmpForm
  IF llIsAparel
    =gfSetDevice()
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),loogscroll.gfTempName(),lcOgTmpForm)
    =gfCrtFrm(lcFormName,'',llOGRefForm)  && Create Temp. file for new form.
    SELECT (lctemprcp)
    GO TOP
    DO gfDispRe WITH EVAL('lcRCFrmNam')
  ENDIF 
  lcOGTmpForm = lcOldForm
ENDIF

IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (gcWorkDir+lcTempOrd+'.DBF')
  ERASE (gcWorkDir+lcTempOrd+'.CDX')
ENDIF
*-- end of Report code.


*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Mariam Mazhar
*! Date      : 04/07/2008
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************
FUNCTION lfSortDumy

IF RIGHT(lcFormName,2) ='GM'
  DIMENSION laSortDesc[3,1] , laSortVal[3,1]
  laSortDesc[1] = 'Style'
  laSortDesc[2] = 'Line Number'
  laSortDesc[3] = 'Pack'

  laSortVal[1] = 'S'
  laSortVal[2] = 'L'
  laSortVal[3] = 'P'
ELSE
  DIMENSION laSortDesc[2,1] , laSortVal[2,1]
  laSortDesc[1] = 'Style'
  laSortDesc[2] = 'Line Number'
  laSortVal[1] = 'S'
  laSortVal[2] = 'L'    
ENDIF  
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/07/2008
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lnPos1

IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  llRpOrdLnt = .F.
  lnPos1 = ASCAN(laOgObjType,'llRpOrdLnt')
  IF lnPos1 > 0
    lnPos1= ASUBSCRIPT(laOgObjType,lnPos1,1)
    laOGObjCnt[lnPos1] = .F.
    =lfOGShowGet('llRpOrdLnt')  && Disable the Print order line notepad Object
  ENDIF
ENDIF    && End of IF

lnPos = ASCAN(laOgObjType,'LLRCPONLY')
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOgObjType,lnPos,1)
  laOGObjCnt[lnPos] = lcRpOrdTyp = "T"
  = lfOGShowGet('LLRCPONLY')
ENDIF

gfOpenTable('ORDHDR','ORDHDR','SH')
gfOpenTable('Ordline','Ordline','SH')
gfOpenTable('CUSTOMER','CUSTOMER','SH')
gfOpenTable('Style','Style','SH')
gfOpenTable('Codes','CCODE_NO','SH')
gfOpenTable('SCALE','SCALE','SH')
gfOpenTable('OBJLINK','OBJLNKTY','SH')
gfOpenTable('OBJECTS','OBJECTID','SH')
gfOpenTable('debit','debit','SH')
gfOpenTable('NotePad','NotePad','SH')
gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK_A')
gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS_A')

SELECT ORDHDR
gfSetOrder('ORDHDR')
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfsChOrder
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsChOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsChOrder
PARAMETERS lcParm

DO CASE 
  CASE lcParm = 'S'
    SELECT ORDHDR
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
	     *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][Start]
*!*	         SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
*!*	           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
*!*	           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
*!*	           IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         SET FILTER TO IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
           IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
		 *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][End]
         LOCATE
         
      CASE lcRpOrdTyp = "O" 
        *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][Start]
*!*	         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
*!*	            STATUS = lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
*!*	            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][End]
            LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
         *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][Start]
*!*	         SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
*!*	            STATUS = lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
*!*	            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         SET FILTER TO (CORDTYPE + ORDER = "C")  AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
        *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][End]         
         LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T" 
         *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][Start]
*!*	         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
*!*	            STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
*!*	            ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
*!*	            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(ORDHDR.cOrdType='T' .AND. ;
            ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         *: B611127,1 MMT 03/16/2016 Order browser takes long time in custom order conf. margins report[T20160309.0011][End]            
         LOCATE FOR CORDTYPE+ORDER = "T"

    ENDCASE                  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
ENDCASE
*!*************************************************************
*! Name      : lfvType
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Transaction Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
*
FUNCTION lfvType

IF lcRpOrdTyp <> 'A'
  lnPOrdPos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"ORDHDR.ORDER"),1)
  IF lnPOrdPos  > 0
    IF !EMPTY(laOGVRFlt[lnPOrdPos  ,6]) AND USED(laOGVRFlt[lnPOrdPos  ,6])
      SELECT (laOGvrFlt[lnPOrdPos  ,6])
      ZAP 
    ENDIF 
  ENDIF 
  =loOGScroll.RefreshScroll()
ENDIF   

IF lcRpOrdTyp $ "TC"
  lcRpEDIFlt = "B"
ENDIF

*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mariam Mazahr (MMT)
*! Date      : 04/07/2008
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcLogoPic = loogscroll.gfTempName()
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*-- end of lfGetLogo.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCNG.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Mariam Mazahr (MMT)
*! Date        : 04/07/2008
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSolSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr


STORE 0 TO lnDue10,lnDue30,lnDue60,lnDue61,lnCredit,lnDebit,lnCrLimit,lnRecno
lcPiktkts=''
lnSavAlias = SELECT(0)
lcStore = &lcTempOrd..Store



lcAlias = ALIAS()
SELECT Codes
gfSetOrder('Codes')
IF gfSEEK('N'+&lcORDHDR..cDivision +'Y'+'CDIVISION')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+&lcORDHDR..cDivision +'Y'+'CDIVISION'
    IF crltd_nam = 'DIVLNAME  '
      lcDivLName = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)

*= gfRltFld(&lcORDHDR..cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(&lcORDHDR..ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(&lcORDHDR..Season  , 'SEASON'   )
lcSpcInst = gfCodDes(&lcORDHDR..SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(&lcORDHDR..CTERMCODE,'CTERMCODE')
SELECT (lcTempAge)
IF SEEK (&lcORDHDR..CORDTYPE+&lcORDHDR..ORDER+&lcORDHDR..ACCOUNT)
   lnDue10= NDUE10
   lnDue30= NDUE30
   lnDue60= NDUE60
   lnDue61= NDUE61
   lcPiktkts=MPiktkts
ENDIF   
SELECT CUSTOMER
lnRecno = RECNO()  




= gfSEEK('M' + &lcTempOrd..Account, "CUSTOMER")


lnCredit =Customer.OpenCr
lnDebit  =Customer.NetBal
lnCrLimit=Customer.CrLimit

*: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[Start]
*IF lnRecno # RECNO() 
IF lnRecno # RECNO()  AND BETWEEN(lnRecno,1,RECCOUNT())
*: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[End]

  GOTO lnRecno
ENDIF 

*: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[Start]
= gfSEEK(IIF(!EMPTY(&lcTempOrd..Store),'S','M') + &lcTempOrd..Account + IIF(!EMPTY(&lcTempOrd..Store),&lcTempOrd..Store,''), "CUSTOMER")
*: B608735,1 MMT 11/02/2008 fix bug of wrong Sold to Address and A,B Set Value[End] 

IF &lcORDHDR..MULTI = 'Y'
  = gfSEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")
  
  IF ALLTRIM(&lcORDHDR..ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  
 ENDIF

lcSolTName = BTName
lcShpTName = IIF(&lcORDHDR..Alt_ShpTo , &lcORDHDR..STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF &lcORDHDR..Alt_ShpTo
  SELECT (lcORDHDR)
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else

 *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [Start] 
 = gfSEEK(IIF(!EMPTY(&lcTempOrd..Store),'S','M') + &lcTempOrd..Account + IIF(!EMPTY(&lcTempOrd..Store),&lcTempOrd..Store,''), "CUSTOMER")
 *: C200979,2 MMT 04/21/2008 Fix Bugs Of Repeated Piktkt and Discout [End] 

  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =gfSEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF


  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = gfSEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)
*-- end of lfSolSpAdr.


*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Marim Mazhar
*! Date        : 04/07/2008
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes    = lfNoteData()     && Note Data.
IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
ELSE
  lcTitle    =""
ENDIF


llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name        : lfNoteHead
*! Developer   : Mariam MAzhar (MMT)
*! Date        : 04/07\2008
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteHead()
*!*************************************************************
FUNCTION lfNoteHead
lcNoteHead = ''
*-- If you have order lines.
IF &lcORDHDR..LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE 
       CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'
        
      CASE &lcNoteLns..cRecord = 'N2' AND gfSEEK('B'+IIF(&lcORDHDR..cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
           
        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(&lcORDHDR..cOrdType='O','Order Notepad',IIF(&lcORDHDR..cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')

    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'


      CASE llRpOrdNot AND gfSEEK('B'+IIF(&lcORDHDR..cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(&lcORDHDR..cOrdType='O','Order Notepad',IIF(&lcORDHDR..cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')

    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- end of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 04/07/2008
*! Purpose     : Function to fill the approparate Note Data Field in report.
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteData()
*!*************************************************************
FUNCTION lfNoteData
lcNoteData  = ''
lcPrntNote = ''
*-- If you have order lines.
IF &lcORDHDR..LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote
      CASE &lcNoteLns..cRecord = 'N2' AND gfSEEK('B'+IIF(&lcORDHDR..cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
        lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote
      CASE llRpOrdNot AND gfSEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- end of lfNoteData.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/07/2008
*! Purpose   : Function to Update the End of Group flag 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.
RETURN '    '
*-- end of lfEndGroup.
*!*************************************************************
*! Name      : lfLastRec
*! Developer : MARIAM mAZHAR (MMT)
*! Date      : 04/07/2008
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec


PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTempOrd)
lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = Store
LOCATE REST FOR ( cordtype+order+store+style+STR(lineno,6) > &lcORDHDR..cordtype + &lcORDHDR..order + lcThStore)
IF (order != &lcORDHDR..order) OR (store != lcThStore)
  SKIP -1
ENDIF   
lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF

SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*-- end of lfLastRec.


FUNCTION lfvOrdSt

IF lcRpOrdSta <> 'A'
  lnPOrdPos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,"ORDHDR.ORDER"),1)
  IF lnPOrdPos  > 0
    IF !EMPTY(laOGVRFlt[lnPOrdPos  ,6]) AND USED(laOGVRFlt[lnPOrdPos  ,6])
      SELECT (laOGvrFlt[lnPOrdPos  ,6])
      ZAP 
    ENDIF 
  ENDIF 
  =loOGScroll.RefreshScroll()
ENDIF   