*:***************************************************************************
*: Program file  : ALPKTKB.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM 'B'.
*! Date          : 11/18/2008 
*: System        : ARIA4XP 
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mostafa Eid (MOS)
*: Tracking Job Number: N000627{T20080610.0010}
*:***************************************************************************
*: Calls :
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKB
*:***************************************************************************
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcPhonPict ='@R'+' '+lcPhonPict
lnCurRec = RECNO()
*-- We Will use the temp file of pktkt.prg to make this report work
LNTOTAMT = 0
lcShipVia=""
XTIME = TIME()

lcDivLName = ''        && Variable to hold the Division long name
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'



lcStyTitl = gfItemMask("HI")

lnRecord = 1 



PRIVATE lnCol ,lnX , lnScaleCnt 


PRTPRICE = IIF(llRpStyPrc , 'Y' , 'N')
STORE 0.00 TO XORDTOT, XVALUE , SVALUE
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])                                    TO HLINE4


HLINE2 = IIF(llPrntComp,HLINE2,'')
HLINE3 = IIF(llPrntComp,HLINE3,'')
HLINE4 = IIF(llPrntComp,HLINE4,'')



XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)




XPHONE = lcXphone



A='-------------------------------------------------------------------------------'
B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'

IF XDYELOT_S
 C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT' 
 ELSE
 C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT'  
ENDIF
 
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'

F='|                 |           |       |           |           |'


G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'


MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3

IF LEN(HLINE3) = 0
   STORE HLINE4 TO HLINE3
ENDIF


SELECT (lcTmpOrdL)
XORDER = ORDER 
lcWDesc  = CWARECODE
NEWDOC = .T.
XPIKTKT = SPACE(6)
XSTYDESC = SUBSTR(&lcTmpOrdL..DESC1,1,20)

lnScaleCnt = 8 
IF SEEK('S'+Style.SCALE,'SCALE') 
  lnScaleCnt = Scale.Cnt
ENDIF
lnCurRec = RECNO()

HLINE2 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 1) , HLINE2 ) ,'')
HLINE3 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 2) , HLINE3 ) ,'')
HLINE4 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 3) , HLINE4 ) ,'')
HLINE5 = IIF(llPrntComp , IIF(llRpWareH , TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)   , IIF(TYPE('HLINE5') = 'U' , '', HLINE5) ) ,'')

IF LEN(HLINE3) = 0
  STORE HLINE4 TO HLINE3
  STORE HLINE5 TO HLINE4
  STORE ' '    TO HLINE5
ENDIF


IF PIKTKT <> XPIKTKT
  lnRecord = RECNO()  
  =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
  XVALUE = 0.00
  
ENDIF
XPIKTKT = PIKTKT
XSTORE = STORE
XVALUE   = XVALUE + TOTPIK * PRICE
SELECT CUSTOMER  
=SEEK(IIF(EMPTY(PIKTKT.STORE),"M","S")+PIKTKT.ACCOUNT+PIKTKT.STORE)

  XBTNAME  = CUSTOMER->BTNAME
  XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
  XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')   
  XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
              + ' ' +  gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))
      
  IF LEN(TRIM(XBTADDR2)) = 0
    XBTADDR2 = XBTADDR3
    XBTADDR3 = ''
  ENDIF
 
SELECT (lcTmpOrdL)
XSTORE = STORE
gfseek('O'+&lcTmpOrdL..order,'ORDHDR','ORDHDR')

  lcShipVia = IIF(OrdHdr.ShipVia="*",;
              IIF(SEEK("S"+PIKTKT.ACCOUNT+PIKTKT.STORE,"CUSTOMER"),Customer.ShipVia,"*"),;
              OrdHdr.ShipVia)

  XSHIPVIA = gfCodDes(lcShipVia , 'SHIPVIA')
  XSPCINST = gfCodDes(ORDHDR->SPCINST , 'SPCINST')
  XSEASON  = gfCodDes(ORDHDR->SEASON , 'SEASON')
  XTERMS = gfCodDes(ORDHDR->CTERMCODE , 'CTERMCODE')
  SELECT CODES
  SET ORDER TO CODES IN CODES 
  
  HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
  HLINE1 = IIF(llRpWareH , LEFT(WAREHOUS.CDESC,30) , HLINE1 )
  HLINE1 = IIF(llPrntComp,HLINE1,'')


  STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
  XPIECES = 0
  * GET DESCRIPTIONS FOR CODED FIELDS [end]
  
  *--Get Alt Ship to if exsits if !exist account
 SELECT  OrdHdr
  lcDistCntr = ""
 
  IF Alt_ShpTo
 
    lcShpTName  = OrdHdr.STName   
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
   
    XSTADDR3 = TRIM(laShipTo[3]) 
    XSTADDR4 = IIF(EMPTY(TRIM(laShipTo[4])),TRIM(laShipTo[5]),;
                          TRIM(laShipTo[4]) + ' ' + laShipTo[5])   
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
  SELECT CUSTOMER
  IF !EMPTY(Dist_Ctr) 
    lcDistCntr = ALLTRIM(CUSTOMER.Dist_Ctr)
    =seek('S' + PIKTKT.ACCOUNT + lcDistCntr)        
  ENDIF
    
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
      
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                    +','+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                     +''+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
 
   IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
   ENDIF
 
 ENDIF
 

  lcScl = SPACE(0)
  lnScaleCnt = 8 

 lcScl=' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ1),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ2),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ3),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ4),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ5),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ6),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ7),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ8),1,5),5,' ')   
      

  lnScaleCnt = &lcScaleFile..Cnt
 
  IF llRpOrdLNt  .AND. !EMPTY(ORDLINE.Note_Mem) 

      lnMemoLnNo = 0
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      *-- get no of lines in ordline.note_mem
      lnMemolins = MEMLINES (lcTempOrdLine+'.Note_Mem')
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          lnMemoLnNo = lnMemoLnNo + 1
          lcNotes = MLINE(&lcTmpOrdL..Note_Mem,lnMemoLnNo)
        ENDDO
     ENDIF 
 ENDIF

  PRTNPAD = IIF(llRpOrdNot,'Y','N')

  IF PRTNPAD='Y' .AND. NEWDOC
  SELECT NOTEPAD
    IF SEEK('B' + XORDER)
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      lnMemoLnNo = 0
      lnMemolins = 0
      lnMemolins = MEMLINES(mNotes)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
      SELECT ORDHDR         
           XORDER=ORDER
           lnMemoLnNo = lnMemoLnNo + 1
           lcNotes = MLINE(NOTEPAD.mNotes,lnMemoLnNo)
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  
SELECT (lcTmpOrdL)
XPIECES = XPIECES + &lcTmpOrdL..TOTPIK

LOCATE
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE 

SELECT (lcTmpOrdL)

IF llOGFltCh
DIMENSION laTmpOrdLSTr[1,18]
lnFldTot = AFIELDS(laTmpOrdLSTr)
DIMENSION laTmpOrdLSTr[lnFldTot +1,18] 
laTmpOrdLSTr[lnFldTot +1,1] = 'cSum'
laTmpOrdLSTr[lnFldTot +1,2] = 'C'
laTmpOrdLSTr[lnFldTot +1,3] = 1
laTmpOrdLSTr[lnFldTot +1,4] = 0
STORE ' ' TO  laTmpOrdLSTr[lnFldTot +1,7],laTmpOrdLSTr[lnFldTot +1,8],;
                laTmpOrdLSTr[lnFldTot +1,9],laTmpOrdLSTr[lnFldTot +1,10],;
                laTmpOrdLSTr[lnFldTot +1,11],laTmpOrdLSTr[lnFldTot +1,12],;
                laTmpOrdLSTr[lnFldTot +1,13],laTmpOrdLSTr[lnFldTot +1,14],;
                laTmpOrdLSTr[lnFldTot +1,15],laTmpOrdLSTr[lnFldTot +1,16]
STORE 0 TO    laTmpOrdLSTr[lnFldTot +1,17] ,laTmpOrdLSTr[lnFldTot +1,18]
lcTempFile = loogscroll.gfTempName()

IF TYPE('lcRpSortBy') = 'U' AND TYPE('lcRpPrtBy') = 'U'
  =gfCrtTmp(lcTempFile ,@laTmpOrdLSTr,"PikTkt+csum + Order + cGrupDetal + STR(LineNo , 6)",lcTempFile ,.T.)
ELSE
  *-- IF Print By Picking Ticket Number, the sorting will be as is it.
  IF lcRpPrtBy = 'P'
    IF lcRpSortBy = 'S'
      =gfCrtTmp(lcTempFile ,@laTmpOrdLSTr,"PikTkt+cSum+ Order + cGrupDetal + STYLE",lcTempFile ,.T.)
    ELSE
      =gfCrtTmp(lcTempFile ,@laTmpOrdLSTr,"PikTkt+cSum + Order + cGrupDetal + STR(LineNo , 6)",lcTempFile ,.T.)
    ENDIF
  ELSE
    IF lcRpSortBy = 'S'
      =gfCrtTmp(lcTempFile ,@laTmpOrdLSTr,"Account + PikTkt+cSum + Order + cGrupDetal + STYLE",lcTempFile ,.T.)    
    ELSE
       =gfCrtTmp(lcTempFile ,@laTmpOrdLSTr,"Account + PikTkt+cSum + Order + cGrupDetal + STR(LineNo , 6)",lcTempFile ,.T.)    
    ENDIF
  ENDIF
ENDIF


SELECT (lcTmpOrdL)
lcSetR = SET("Relation")
SCAN FOR !EMPTY(Style)
  m.cSum = ''
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcTempFile) FROM MEMVAR 
  IF llRpPrtBlk
    m.cSum = 'S'
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ENDIF   
ENDSCAN 
lcTmpOrdL = lcTempFile
SELECT (lcTmpOrdL)
SET RELATION TO &lcSetR 
LOCATE 
ENDIF 
llNewPage = .T.

lcQtyVar = "SPACE(5)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')),'ORDR:','ORDR:')+SPACE(2)+"+;
		   "IIF(lnScalecnt>0,STR(eval(lcTmpOrdL+'.QTY1'),5),'')+SPACE(1)+"+;
		   "IIF(lnScalecnt>1,STR(eval(lcTmpOrdL+'.QTY2'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>2,STR(eval(lcTmpOrdL+'.QTY3'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>3,STR(eval(lcTmpOrdL+'.QTY4'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>4,STR(eval(lcTmpOrdL+'.QTY5'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>5,STR(eval(lcTmpOrdL+'.QTY6'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>6,STR(eval(lcTmpOrdL+'.QTY7'),5),'')+Space(1)+"+;
		   "IIF(lnScalecnt>7,STR(eval(lcTmpOrdL+'.QTY8'),5),'')+Space(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),STR(eval(lcTmpOrdL+'.TOTPIK'),6),'')+Space(1)+"+;
		   "STR(eval(lcTmpOrdL+'.TOTQTY'),6)+CHR(13)+"+;
		   "SPACE(5)+IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),'ALLO:','')+SPACE(2)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>0,STR(eval(lcTmpOrdL+'.PIK1'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>1,STR(eval(lcTmpOrdL+'.PIK2'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>2,STR(eval(lcTmpOrdL+'.PIK3'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>3,STR(eval(lcTmpOrdL+'.PIK4'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>4,STR(eval(lcTmpOrdL+'.PIK5'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>5,STR(eval(lcTmpOrdL+'.PIK6'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>6,STR(eval(lcTmpOrdL+'.PIK7'),5),''),'')+SPACE(1)+"+;
		   "IIF(PRTPIK OR !Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>7,STR(eval(lcTmpOrdL+'.PIK8'),5),''),'')+SPACE(8)+"+;
		   "IIF(!Empty(eval(lcTmpOrdL+'.CSUM')),STR(eval(lcTmpOrdL+'.TOTPIK'),6),'')+CHR(13)+"+;
		   "SPACE(5)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')),'PICK:','')+SPACE(2)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>0,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>1,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>2,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>3,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>4,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>5,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>6,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>7,'-----',''),'')+SPACE(1)+"+;
		   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(lnScalecnt>8,'-----',''),'')+SPACE(1)"
		   
		   
		   
		   
		   
		   
lcHeadrVariable = "IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,'ORDER :','')+SPACE(1)+IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,eval(lcTmpOrdL+'.ORDER'),'')+"+;
				  "SPACE(2)+IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,SUBSTR(DTOC(eval(lcOrdhdr+'.START')),1,5),'')+"+;
				  "SPACE(3)+IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,SUBSTR(DTOC(eval(lcOrdhdr+'.COMPLETE')),1,5),'')+"+;
				  "SPACE(6)+PADR(IIF(EMPTY(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,HLINE3,''),25)+SPACE(8)+IIF(EMPTY(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,XTIME,'')+"+;
				  "CHR(13)+SPACE(0)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(!EMPTY(eval(lcOrdhdr+'.APPROVAL')),'APPRVL: '+eval(lcOrdhdr+'.APPROVAL'),''),''),25)+"+;
				  "SPACE(10)+PADR(IIF(EMPTY(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,HLINE4,''),25)+CHR(13)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(llWareHous .AND. !EMPTY(lcWDesc),lcWDesc,''),''),30)+"+;
				  "CHR(13)+CHR(13)+SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,'..... SOLD TO .....',''),20)+"+;
				  "SPACE(26)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,'..... SHIP TO .....',''),20)+CHR(13)+"+;
				  "SPACE(4)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,eval(lcOrdhdr+'.ACCOUNT'),'')+SPACE(2)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,TRANSFORM(eval(lcOrdhdr+'.PHONE'),lcPhonPict),''),30)+"+;
				  "SPACE(9)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,IIF(LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*', 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr),''),'')+CHR(13)+"+;
				  "SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(EMPTY(XBTNAME),CHR(255),XBTNAME),''),30)+SPACE(16)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,XSTNAME,'')+CHR(13)+"+;
				  "SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(EMPTY(XBTADDR1),CHR(255),XBTADDR1),''),30)+SPACE(16)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,XSTADDR1,'')+CHR(13)+"+;
				  "SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(EMPTY(XBTADDR2),CHR(255),XBTADDR2),''),30)+SPACE(16)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,XSTADDR2,'')+CHR(13)+"+;
				  "SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(EMPTY(XBTADDR3),CHR(255),XBTADDR3),''),30)+SPACE(16)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,XSTADDR3,'')+CHR(13)+"+;
				  "SPACE(4)+PADR(IIF(Empty(eval(lcTmpOrdL+'.CSUM'))  AND llNewPage,IIF(EMPTY(XBTADDR4),CHR(255),XBTADDR4),''),30)+SPACE(16)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,XSTADDR4,'')+CHR(13)+"+;
				  "IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,A,'')+CHR(13)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,B,'')+CHR(13)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,A,'')+SPACE(2)+"+;
				  "IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,LEFT(XSHIPVIA,15) + '|','')+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,LEFT(XSEASON,7) + ' |','')+SPACE(0)+"+;
				  "IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,LEFT(XSPCINST,18) + '|','')+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,eval(lcTmpOrdL+'.STORE') + ' ' + ' |','')+"+;
				  "SPACE(1)+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,eval(lcOrdhdr+'.DEPT ')+ ' |','')+IIF(Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage,LEFT(eval(lcTmpOrdH+'.Custpo'),13) +' |','')+"+;
				  "CHR(13)+IIF((Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage) OR !Empty(eval(lcTmpOrdL+'.CSUM')),A,'')+CHR(13)+"+;
				  "iif( llNewPage,IIF(Empty(eval(lcTmpOrdL+'.CSUM')),C,'     ' + lcStyTitl + ' DESCRIPTION '),'')+CHR(13)+IIF((Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage) OR (!Empty(eval(lcTmpOrdL+'.CSUM')) AND llNewPage),A,'')"
				  				  
lcHdrVariable = "' '+CHR(13)+A+CHR(13)+'     ' + lcStyTitl + ' DESCRIPTION '+CHR(13)+A"

lcNotesField = "IIF(Empty(eval(lcTmpOrdL+'.CSUM')),IIF(llRpOrdLNt AND !EMPTY(eval(lcOrdLnTmp+'.Note_Mem')),"+;
			   "CHR(255)+CHR(13)+'*-- Line Note Pad --*'+CHR(13)+eval(lcOrdLnTmp+'.Note_Mem')+chr(13)+'*-- END OF Line NotePad --*'+CHR(13),'')+CHR(255),REPL('_',77))+CHR(13)+"+;
			   "IIF(Empty(eval(lcTmpOrdL+'.CSUM')) and (lnLastLine= eval(lcTmpOrdL+'.LINENO') ) and llRpOrdNot AND Seek('B'+ Order,lcNotePad) and !Empty(EVAL(LCNOTEPAD+'.mnotes')), '*-- N O T E P A D --*'+CHR(13)+EVAL(LCNOTEPAD+'.mnotes'),'')"
				  
lnLastLine = 0				  			  
SELECT (lcTmpOrdL)
PRTPIK = PIK1 # QTY1 .OR. PIK2 # QTY2 .OR. PIK3 # QTY3 .OR. ;
             PIK4 # QTY4 .OR. PIK5 # QTY5 .OR. PIK6 # QTY6 .OR. ;
             PIK7 # QTY7 .OR. PIK8 # QTY8

= gfDispRe()
IF loOgScroll.ll2Printer=.T.  
  llPrinter = .T.
ENDIF   


llAlpktk = .F.
ENDIF
SELECT (lcTmpOrdL)


*!*************************************************************
*! Name      : lfNotNewPage
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 11/18/2008 
*! Purpose   : ReSet New Page variable
*!*************************************************************
*! Called from : ALPKTKTB.FRX (Inside the FRX)
*!*************************************************************
*
FUNCTION lfNotNewPage
*!*	IF lnLastLine <> LINENO
*!*	  USE (lcOGRprtNam+'.FRX') SHARED IN 0
*!*	  SELECT(juststem(lcOGRprtNam))
*!*	  LOCATE FOR objcode = 7 
*!*	  IF FOUND() and height  > 2
*!*	    DELETE all FOR UPPER('xxx') $ UPPER(comment) 
*!*	    LOCATE FOR objcode = 7 
*!*	    IF FOUND()
*!*	      REPLACE height WITH 2
*!*	    ENDIF 
*!*	  ENDIF   
*!*	  USE 
*!*	ELSE
*!*	  USE (lcOGRprtNam+'.FRX') SHARED IN 0
*!*	  SELECT(juststem(lcOGRprtNam))
*!*	  LOCATE FOR objcode = 7 
*!*	  IF FOUND() AND  height  < 15
*!*	    REPLACE height WITH 15
*!*	    RECALL ALL FOR UPPER('xxx') $ UPPER(comment) 
*!*	  ENDIF 
*!*	  USE 
*!*	ENDIF   
SELECT (lcTmpOrdL)
llNewPage = .F.
lcScl = SPACE(0)
lnScaleCnt = 8 
LNTOTAMT = LNTOTAMT +  eval(lcTmpOrdL+'.PRICE')*eval(lcTmpOrdL+'.TOTPIK')
lcScl=' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ1),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ2),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ3),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ4),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ5),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ6),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ7),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ8),1,5),5,' ')   
      
lnScaleCnt = &lcScaleFile..Cnt

SELECT (lcTmpOrdL)
PRTPIK = PIK1 # QTY1 .OR. PIK2 # QTY2 .OR. PIK3 # QTY3 .OR. ;
             PIK4 # QTY4 .OR. PIK5 # QTY5 .OR. PIK6 # QTY6 .OR. ;
             PIK7 # QTY7 .OR. PIK8 # QTY8

RETURN ''
*!*************************************************************
*! Name      : lfNewPage
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 11/18/2008 
*! Purpose   : Set New Page variable
*!*************************************************************
*! Called from : ALPKTKTB.FRX (Inside the FRX)
*!*************************************************************
FUNCTION lfNewPage

llEndGroup = .F.
=gfRltFld(EVALUATE(lcORDHDR+'.cDivision') , @laDivLName , 'CDIVISION')
llNewPage = .T.
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
HLINE1 = IIF(llRpWareH , LEFT(&lcWareHous..CDESC,30) , HLINE1 )
HLINE1 = IIF(llPrntComp,HLINE1,'')
STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4

XSHIPVIA =  gfCodDes(&lcORDHDR..ShipVia , 'SHIPVIA')
XSPCINST =  gfCodDes(&lcORDHDR..SpcInst , 'SPCINST')
XSEASON = gfCodDes(&lcORDHDR..Season , 'SEASON')
XTERMS =  gfCodDes(&lcORDHDR..cTermCode , 'CTERMCODE')
XSTORE = STORE


SELECT  (lcOrdhdr)
lcDistCntr = ""
IF Alt_ShpTo
  lcShpTName  = &lcORDHDR..STName   
  laShipTo[1] = &lcORDHDR..cAddress1
  laShipTo[2] = &lcORDHDR..cAddress2
  laShipTo[3] = &lcORDHDR..cAddress3
  laShipTo[4] = &lcORDHDR..cAddress4
  laShipTo[5] = &lcORDHDR..cAddress5

  XSTNAME  = lcShpTName
  XSTADDR1 = laShipTo[1]
  XSTADDR2 = laShipTo[2]
   
  XSTADDR3 = TRIM(laShipTo[3]) 
  XSTADDR4 = IIF(EMPTY(TRIM(laShipTo[4])),TRIM(laShipTo[5]),;
                      TRIM(laShipTo[4]) + ' ' + laShipTo[5])   
  IF LEN(TRIM(XSTADDR2)) =0
    XSTADDR2 = XSTADDR3
    XSTADDR3 = ''
  ENDIF
ELSE
   SELECT (lcCustomer)
   IF !EMPTY(Dist_Ctr) 
     lcDistCntr = ALLTRIM(&lcCustomer..Dist_Ctr)
     =seek('S' + &lcPiktktTemp..ACCOUNT + lcDistCntr)        
   ENDIF

   XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
   XSTADDR1 = gfGetAdr(lcCustomer , '' , '' , '' , 1 )
   XSTADDR2 = gfGetAdr(lcCustomer, '' , '' , '' , 2 )
   XSTADDR3 = gfGetAdr(lcCustomer , '' , '' , '' , 3 )
  
   XSTADDR4 = IIF(EMPTY(gfGetAdr(lcCustomer , '' , '' , '' , 4 )) ,;
                     gfGetAdr(lcCustomer , '' , '' , '' , 5 )  ,;
                     gfGetAdr(lcCustomer , '' , '' , '' , 4 )   ;
                +','+gfGetAdr(lcCustomer , '' , '' , '' , 5 ))
                
   XSTADDR4 = IIF(EMPTY(gfGetAdr(lcCustomer , '' , '' , '' , 4 )) ,;
                     gfGetAdr(lcCustomer , '' , '' , '' , 5 )  ,;
                     gfGetAdr(lcCustomer , '' , '' , '' , 4 )   ;
                 +''+gfGetAdr(lcCustomer , '' , '' , '' , 5 ))
 
   IF LEN(TRIM(XSTADDR2)) =0
     XSTADDR2 = XSTADDR3
     XSTADDR3 = ''
   ENDIF
ENDIF
SELECT (lcTmpOrdL)

HLINE2 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr(lcWareHous , '' , '' , '' , 1) , HLINE2 ) ,'')
HLINE3 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr(lcWareHous , '' , '' , '' , 2) , HLINE3 ) ,'')
HLINE4 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr(lcWareHous , '' , '' , '' , 3) , HLINE4 ) ,'')
HLINE5 = IIF(llPrntComp , IIF(llRpWareH , TRANSFORM(&lcWareHous..CPHONE,lcPhonPict)   , IIF(TYPE('HLINE5') = 'U' , '', HLINE5) ) ,'')

IF LEN(HLINE3) = 0
  STORE HLINE4 TO HLINE3
  STORE HLINE5 TO HLINE4
  STORE ' '    TO HLINE5
ENDIF



IF PIKTKT <> XPIKTKT
  lnRecord = RECNO()  
  =gfRltFld(&lcORDHDR..cDivision , @laDivLName , 'CDIVISION')
  XVALUE = 0.00
ENDIF
XPIKTKT = PIKTKT
XSTORE = STORE
XVALUE   = XVALUE + TOTPIK * PRICE
SELECT (lcCUSTOMER  )
=SEEK(IIF(EMPTY(&lcPiktktTemp..STORE),"M","S")+&lcPiktktTemp..ACCOUNT+&lcPiktktTemp..STORE)

XBTNAME  = &lcCustomer..BTNAME
XBTADDR1 = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2')
XBTADDR2 = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2') 
XBTADDR3 = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')   
XBTADDR4 = IIF(EMPTY(gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')),;
                     gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2') ,;
                     gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')  ;
            + ' ' +  gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2'))
    
IF LEN(TRIM(XBTADDR2)) = 0
  XBTADDR2 = XBTADDR3
  XBTADDR3 = ''
ENDIF

SELECT (lcTmpOrdL) 
lnRecNo = RECNO()
SCAN FOR PIKTKT = XPIKTKT 
  lnLastLine = LINENO
ENDSCAN 
IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 
RETURN ''
*!*************************************************************
*! Name      : lfGrpHD
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 11/18/2008 
*! Purpose   : ReSet TotAmt Variable
*!*************************************************************
*! Called from : ALPKTKTB.FRX (Inside the FRX)
*!*************************************************************
FUNCTION lfGrpHD
LNTOTAMT = 0
RETURN ''