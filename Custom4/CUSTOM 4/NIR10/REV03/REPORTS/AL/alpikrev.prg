*:***************************************************************************
*: Developer     : MOSTAFA EID (MOS)
*: Date          : 06/01/2008
*: Refrence      : C201025
*: Program file  : ALPIKREV
*: Program desc. : OUTSTANDING PICK TICKET REPORT FOR (REV03)
*: System        : 4XP
*: Module        : ALLOCATION (AL)
*:***************************************************************************

STORE '' TO lcTitle
STORE .F. To llEndReprt 
lcStyHeadr = gfItemMask('HI')                                                                                     
lcTime = TIME()
IF lcRpSortBy = 'C'   && If sort by Complete Date.
   lcTitle = 'Sort by complete date'
ELSE
   IF lcRpSortBy = 'S'  && If Sort by Start Date. 
      lcTitle = 'Sort by start date'
   ELSE   && If Sort by Piktkt #.
      lcTitle = 'Sort by Pick ticket number'
   ENDIF
ENDIF    

IF llOGFltCh
  DO lfCreaTemp   && Create Temp file to collect data in
  DO lfCollect    && Collect data
ENDIF

SELECT (lcTempFile)
GO BOTTOM
REPLACE &lcTempFile..lEndRep WITH .T.
LOCATE
IF EOF(lcTempFile)
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

DO gfDispRe WITH EVALUATE('lcRpName') 

*!*************************************************************
*! Name      : lfCreaTemp  
*: Developer : Mostafa Eid(mos)
*: Date      : 06/03/2008
*! Purpose   : Create Temp file to collect data in
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp  

*-- check If File is created or not
IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
 USE IN (lcTempFile)
ENDIF
*-- Create File
IF !USED(lcTempFile)
  
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PikTkt'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cWareCode'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 36
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Account'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 5
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Store'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DistCent'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ShpToNam'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 30
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Date'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Order'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'BolNum'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Style'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Start' 
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Complete'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TotPik'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Age'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'lEndRep'     
  laTempStru[lnI,2] = 'L'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
*ash1
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Printed'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0

=gfCrtTmp(lcTempFile,@laTempStru,'PikTkt',lcTempFile,.T.)
  SELECT (lcTempFile)
  IF lcRpSortBy = 'C'   && If sort by Complete Date.
    INDEX ON  cWareCode + DTOS(Complete) + PIKTKT  TAG (lcTempFile) 
      ELSE
    IF lcRpSortBy = 'S'  && If Sort by Start Date. 
      INDEX ON  cWareCode + DTOS(START) + PIKTKT  TAG (lcTempFile)
        ELSE   && If Sort by Piktkt #.
	  INDEX ON  cWareCode + PIKTKT TAG (lcTempFile) 
    ENDIF
  ENDIF    
  
  IF !USED('WareHous')
      =gfOpenTable('WareHous','WareHous','SH')
  ENDIF
  
  IF !USED('Pack_hdr')  
     =gfOpenTable('Pack_hdr','Pack_hdr','SH')
  ENDIF  
  
ENDIF  

*!*************************************************************
*! Name      : lfCollect    
*: Developer : Mostafa Eid (MOS)
*: Date      : 06/03/2008
*! Purpose   : Create Temp file to collect data in
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect

llDateStartSelect= .F.
llDateCompleteSelect= .F.
llDatePiktktSelect= .F.
lcPikTktSel = ''

ldDate = oAriaApplication.SystemDate
                
IF!USED('OrdLine') 
 =gfOpenTable('OrdLine','OrdLine','SH') 
ENDIF
IF !USED('piktkt')
=gfOpenTable('piktkt','piktkt','SH') 
ENDIF 

IF !USED('ordhdr')
=gfOpenTable('ordhdr','ORDHDR','SH') 
ENDIF 

IF !USED('CUSTOMER')
=gfOpenTable('CUSTOMER','CUSTOMER ','SH') 
ENDIF 

*! selection by ticket

llSelectPiktkt = .F.
lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
IF lnPosPikTkt > 0 
  lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
  lcPikTktSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
  IF !EMPTY(lcPikTktSel) AND USED(lcPikTktSel)
    SELECT(lcPikTktSel)
    LOCATE
     IF  !EOF()
       llSelectPiktkt  =.T.
    ENDIF 
   ENDIF  
ENDIF 

*! selection by Start date 
lnStartDatePos = ASCAN(laOGFxFlt,'ORDLINE.START')
lnStartDatePos = ASUBSCRIPT(laOGFxFlt,lnStartDatePos,1)
llDateStartSelect = !EMPTY(laOGFxFlt[lnStartDatePos,6])
ldStartDate = CTOD(SUBSTR(laOGFxFlt[lnStartDatePos,6],1,10))
ldEndDate   = CTOD(SUBSTR(laOGFxFlt[lnStartDatePos,6],12,21))
      
*! selection by complete date 
lnCompleteDatePos = ASCAN(laOGFxFlt,'ORDLINE.COMPLETE')
lnCompleteDatePos = ASUBSCRIPT(laOGFxFlt,lnCompleteDatePos,1)
llDateCompleteSelect = !EMPTY(laOGFxFlt[lnCompleteDatePos,6])
ldCompleteStartDate = CTOD(SUBSTR(laOGFxFlt[lnCompleteDatePos,6],1,10))
ldCompleteEndDate   = CTOD(SUBSTR(laOGFxFlt[lnCompleteDatePos,6],12,21))
     
*! selection by piktktdate date        
lnPiktktDatePos = ASCAN(laOGFxFlt,'PIKTKT.DATE')
lnPiktktDatePos = ASUBSCRIPT(laOGFxFlt,lnPiktktDatePos,1)
llDatePiktktSelect = !EMPTY(laOGFxFlt[lnPiktktDatePos,6])
ldPiktktStartDate = CTOD(SUBSTR(laOGFxFlt[lnPiktktDatePos,6],1,10))
ldPiktktendDate = CTOD(SUBSTR(laOGFxFlt[lnPiktktDatePos,6],12,21))

*! selection by warehouse 

llUseWareHouse  = .F.
lcWareFile  = ''
lnWarePos = ASCAN(loOgScroll.laOgFXFlt,'PIKTKT.CWARECODE')
IF lnWarePos > 0 
   lnWarePos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnWarePos,1)
   lcWareSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnWarePos,6]),loOgScroll.laOgFXFlt[lnWarePos,6],'')
  IF !EMPTY(lcWareSel) 
    lcWareFile = loOGScroll.gfTempName()
    llUseWareHouse = IIF(LEN(lcWareSel)>0,.T.,.F.) AND lfConvertToCursor(lcWareSel,'CWARECODE',lcWareFile)
  ENDIF   
ENDIF          

IF llSelectPiktkt 
  SELECT (lcPikTktSel)
  SCAN 
     SELECT piktkt
     IF GfSEEK(&lcPikTktSel..PIKTKT) AND IIF(llDatePiktktSelect,BETWEEN(piktkt.date,ldPiktktStartDate,ldPiktktendDate),.t.)
         SELECT ordline 					
         IF gfSEEK('O'+ piktkt.order,'ORDLINE','ORDLINE')
           SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)= 'O'+ piktkt.order ;
             FOR IIF(llDateStartSelect,BETWEEN(start,ldStartDate,ldEndDate),.t. ) AND ;
             IIF(llDateCompleteSelect,BETWEEN(complete,ldCompleteStartDate,ldCompleteEndDate),.t.)and ; 
            (ORDLINE.PIKTKT)=(PIKTKT.PIKTKT) 
             gfseek(ORDLINE.PIKTKT,'PIKTKT','PIKTKT')  
              IF llUseWareHouse AND !SEEK(PIKTKT.CWARECODE,LCWAREFILE)         
                LOOP
              ENDIF  
              IF lcRPPrntFactr $ 'YN' 
	            gfseek('O'+ ordline.order,'ORDHDR','ORDHDR')
	            IF !(lcRPPrntFactr='Y' AND !EMPTY(ORDHDR.CFACCODE))AND !(lcRPPrntFactr='N' AND  EMPTY(ORDHDR.CFACCODE))
                  LOOP 
                ENDIF                                      
              ENDIF             
	          SCATTER MEMVAR MEMO
			  m.Store = IIF(EMPTY(ORDLINE.STORE),ORDLINE.Account,ORDLINE.Store) 
			  m.cWareCode = IIF(GFSEEK(PikTkt.cWareCode,'Warehous','WAREHOUS'),Warehous.cDesc,'')
			  GFseek(IIF(EMPTY(ORDLINE.STORE),'M'+ Account,'S'+ Account + Store),'CUSTOMER')
			  m.DistCent  = IIF(EMPTY(ORDLINE.STORE),'',CUSTOMER.dist_ctr)
			  m.ShpToNam  = CUSTOMER.StName
			  m.Date      = PIKTKT.Date
			  m.Age       = ldDate - PIKTKT.Date
			  m.BolNum    = IIF(gfseek(Piktkt,'Pack_Hdr'),Pack_Hdr.bill_ladg,'')
			  m.lEndRep   = .F.
	          m.Printed   = IIF(PIKTKT.prtflag='P','Y','N')
              INSERT INTO (lcTempFile) FROM MEMVAR
           ENDSCAN 
         ENDIF 
     ENDIF 
  ENDSCAN    
ELSE 
  SELECT ordline
  gfseek('O')
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O';
        FOR Gfseek(ordline.piktkt,'piktkt','piktkt')and ;
        IIF(llDatePiktktSelect,BETWEEN(piktkt.date,ldPiktktStartDate,ldPiktktendDate),.t.)and ;
        IIF(llDateStartSelect,BETWEEN(start,ldStartDate,ldEndDate),.T. ) AND ;
        IIF(llDateCompleteSelect,BETWEEN(complete,ldCompleteStartDate,ldCompleteEndDate),.T.);
        AND !EMPTY(ordline.piktkt)AND(ordline.piktkt <> '******')      
          IF llUseWareHouse AND !SEEK(PIKTKT.CWARECODE,LCWAREFILE)         
            LOOP
          ENDIF  
          IF lcRPPrntFactr $ 'YN' 
	        gfseek('O'+ ordline.order,'ORDHDR','ORDHDR')
	        IF !(lcRPPrntFactr='Y' AND !EMPTY(ORDHDR.CFACCODE))AND !(lcRPPrntFactr='N' AND  EMPTY(ORDHDR.CFACCODE))
              LOOP 
            ENDIF                                      
          ENDIF                    
         SCATTER MEMVAR MEMO
		  m.Store = IIF(EMPTY(ORDLINE.STORE),ORDLINE.Account,ORDLINE.Store) 
		  m.cWareCode = IIF(gfSEEK(PikTkt.cWareCode,'Warehous','WAREHOUS'),Warehous.cDesc,'')
		  gfseek(IIF(EMPTY(ORDLINE.STORE),'M'+ Account,'S'+ Account + Store),'CUSTOMER')
		  m.DistCent  = IIF(EMPTY(ORDLINE.STORE),'',CUSTOMER.dist_ctr)
		  m.ShpToNam  = CUSTOMER.StName
		  m.Date      = PIKTKT.Date
		  m.Age       = ldDate - PIKTKT.Date
		  m.BolNum    = IIF(gfSEEK(Piktkt,'Pack_Hdr'),Pack_Hdr.bill_ladg,'')
		  m.lEndRep   = .F.
          m.Printed   = IIF(PIKTKT.prtflag='P','Y','N')
          INSERT INTO (lcTempFile) FROM MEMVAR
  ENDSCAN  
ENDIF  
 
 *!*************************************************************
*! Name      : lfsrPkt
*! Developer : Mostafa EID (MOS)
*! Date      : 07/02/2008
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************

*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrPkt
PARAMETERS lcParm
SELECT PIKTKT
GO TOP
*-- End of lfsrPkt.
*!*********************************************************************
*! Name      : lfDummy
*! Developer : Mostafa Eid (MOS)
*! Date      : 07/02/2008
*! Purpose   : To check if report end or not
*!*********************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : ALPIKREV.FRX
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfDummy()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfDummy
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
llEndReprt =  &lcTempFile..lEndRep 

SELECT (lnCurAlias)
RETURN 0

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mostafa Eid(MOS)
*: Date      : 07/02/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  CASE  ALLTRIM(lcFieldName) = 'CWARECODE'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6 
    laTempacstru[1,4]= 0
ENDCASE 

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

