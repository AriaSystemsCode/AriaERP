*:************************************************************************
*: Program file  : ARPINVH.PRG
*:         System: ARIA 4xp
*:         Module: Accounts Recevible
*:         Author: Mariam Mazher[MMT]
*:  Documented    05/07/2006
*:  Modifications:
*:************************************************************************
llAltShp=.F.
lcLines = loOGScroll.gfTempName()
lcHeader = loOGScroll.gfTempName()
*!*  lcPackFile = loOGScroll.gfTempName()
XTAX      = IIF(gfGetMemVar("M_TAX",oAriaApplication.ActiveCompanyID)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',oAriaApplication.ActiveCompanyID)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',oAriaApplication.ActiveCompanyID)
XTAX_METH = gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID)

RELE ALL LIKE M_*

lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")

IF !USED('CONSINVH')
  =gfOpenFile(oAriaApplication.DataDir+'CONSINVH',oAriaApplication.DataDir+'CONSINVH','SH')
ENDIF
lfCreateTemp()

*-- a canadian company or not.
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)

NEWDOC = .T.
MAXROW = 47
lcDist = ' '


llNote = llRpInvNot
lnNotLine = 1

SELECT INVHDR
LINEUP = .F.
STORE TRIM(syccomp.cCom_Name)							   TO HLINE1  &&TRIM(QCOMPANY)                                    
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])                                   TO HLINE4
lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon 					                       TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

lcZone = ''
DECLARE laZone[1,2]

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg1
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XINVNAME = lcPrnComp
XPHONE    = laCompAdd[5]

gcPhnFrmt = lcPhonPict

DECLARE loOGScroll.laCRParams[13,2]
loOGScroll.laCRParams[1,1] = 'HLINE1'
loOGScroll.laCRParams[1,2] = HLINE1
loOGScroll.laCRParams[2,1] = 'XINVNAME'
loOGScroll.laCRParams[2,2] = IIF(XINVNAME = 'Y',1,0)
loOGScroll.laCRParams[3,1] = 'HLINE2'
loOGScroll.laCRParams[3,2] = HLINE2
loOGScroll.laCRParams[4,1] = 'HLINE3'
loOGScroll.laCRParams[4,2] = HLINE3
loOGScroll.laCRParams[5,1] = 'HLINE4'
loOGScroll.laCRParams[5,2] = HLINE4
loOGScroll.laCRParams[6,1] = 'XPHONE'
loOGScroll.laCRParams[6,2] = XPHONE
loOGScroll.laCRParams[7,1] = 'lcDunsNo'
loOGScroll.laCRParams[7,2] = lcDunsNo
loOGScroll.laCRParams[8,1] = 'HLINE5'
loOGScroll.laCRParams[8,2] = HLINE5
loOGScroll.laCRParams[9,1] = 'llPrnFact'
loOGScroll.laCRParams[9,2] = IIF(llPrnFact,1,0)
loOGScroll.laCRParams[10,1] = 'cTaxRefr'
loOGScroll.laCRParams[10,2] = IIF(XTAX .AND. (XTAX_METH = 'M' or XTAX_METH = 'A').AND. INVHDR.TAX_AMT # 0 and !EMPTY(lcTaxRefr),lcTaxRefr,'')
loOGScroll.laCRParams[11,1] = 'XTAX_DESC'
loOGScroll.laCRParams[11,2] = IIF(XTAX .AND. (XTAX_METH = 'M' or XTAX_METH = 'A') .AND. INVHDR.TAX_AMT # 0 and !EMPTY(XTAX_DESC),XTAX_DESC,'')
loOGScroll.laCRParams[12,1] = 'llNotePrnt'
loOGScroll.laCRParams[12,2] = IIF(llRpInvNot,1,0)
loOGScroll.laCRParams[13,1] = 'llRpInvLNt'
loOGScroll.laCRParams[13,2] = IIF(llRpInvLNt,1,0)




SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)




SCAN FOR &lcASExp   WHILE INKEY() <> 32
   STORE "" TO  m.INVOICE,m.ORDER,m.Note1,m.Note2,m.PIKTKT,m.PHONE,m.ACCOUNT,m.CARTONS,m.INVDATE,;
                 m.STORE,m.CDIVISION,m.c_FactAcct,m.XBTNAME,m.CCADDRESS1,m.CCADDRESS2,;
                 m.CCADDRESS3,m.O_CADDRES1,m.O_CADDRES3,m.O_STADDR2,m.XSTNAME
    m.INVOICE = INVHDR.INVOICE 
    PRTERR = 0
    STORE 0.00 TO XPIECES, XSUBTOTAL
    SELECT INVHDR
    SEEK INVOICE
    NEWDOC = .F.
    
    m.ORDER = ORDER
    XPHONE    = laCompAdd[5]
    m.Note1   = IIF(NOTE1<>'*', NOTE1, '')
    m.Note2   = IIF(NOTE2<>'*', NOTE2, '')
	  m.PIKTKT   = PIKTKT
	  m.PHONE  = TRANSFORM(INVHDR->PHONE,'@R '+lcPhonPict)
    m.ACCOUNT  = ACCOUNT
    m.CARTONS = INVHDR->CARTONS
    m.INVDATE  = INVDATE
   	m.STORE    = STORE
    m.CDIVISION = CDIVISION
    SELE CUSTOMER
    m.c_FactAcct = IIF(SEEK('M'+ m.ACCOUNT),FactAcct,'')
    SEEK(IIF(m.STORE= SPACE(8),'M'+ m.ACCOUNT,'S' + m.ACCOUNT + m.STORE))
    m.XBTNAME  = BTNAME
    m.CCADDRESS1 = CADDRESS1
    m.CCADDRESS2 = CADDRESS2
    m.CCADDRESS3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
    IF LEN(TRIM(m.CCADDRESS2)) =0
      m.CCADDRESS2 = m.CCADDRESS3
      m.CCADDRESS3 = ''
    ENDIF
    SELE ORDHDR
    SEEK  'O'+ m.ORDER
      
    IF ALT_SHPTO
	    m.O_CADDRES1 = CADDRESS1
	    m.O_STADDR2 = laShipTo[2]        
  		m.O_CADDRES3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
      IF LEN(TRIM(m.O_STADDR2)) = 0
        m.O_STADDR2 = m.O_CADDRES3
        m.O_CADDRES3 = ''
      ENDIF
    ELSE
      IF INVHDR->CONSOL = 'Y'       
        SELECT CONSINVH
        SEEK(m.INVOICE)
        SELECT CUSTOMER
        SEEK('S'+m.ACCOUNT+CONSINVH->STORE)
      ENDIF
      SELE CUSTOMER
      IF !EMPTY(DIST_CTR)
        lcDist = DIST_CTR
        = SEEK('S'+ m.ACCOUNT + lcDist)
      ENDIF
   
      m.XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
      m.O_CADDRES1 = CADDRESS12
      m.O_STADDR2  = CADDRESS22      
      m.O_CADDRES3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
      
      IF LEN(TRIM(m.O_STADDR2)) =0
        m.O_STADDR2 = m.O_CADDRES3
        m.O_CADDRES3 = ''
      ENDIF
    ENDIF

    ****** FIND THE INVOICE LINES
    SELECT INVLINE
    SEEK(m.INVOICE)
    IF EOF()
      PRTERR = 2
    ENDIF
    STORE "" TO   m.PTERMS,m.PSHIPVIA,m.PSPCINST
    
    m.PTERMS    = LEFT(gfCodDes(INVHDR->CTERMCODE,'CTERMCODE'),21)
    m.PSHIPVIA  = LEFT(gfCodDes(INVHDR->SHIPVIA,'SHIPVIA'),13)
    m.PSPCINST  = gfCodDes(INVHDR->SPCINST,'SPCINST')
 

    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS

    SELECT INVHDR
    
    =lfSolSpAdr()
    m.XBTNAME  = lcSolTName
    m.CCADDRESS1  = laSoldTo[1]
    m.CCADDRESS2  = laSoldTo[2]
    m.CCADDRESS3  = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
        
    IF LEN(TRIM(laSoldTo[2])) =0
      m.CCADDRESS2 = laSoldTo[3]
      m.CCADDRESS3 = ''
    ENDIF
      m.XSTNAME  = lcShpTName
      m.O_CADDRES1 = laShipTo[1]
      m.O_STADDR2  = laShipTo[2]  

  
    llAltShp = OrdHdr.Alt_ShpTo
    IF llAltShp 
      m.O_CADDRES3 = TRIM(laShipTo[3]) 
      m.O_CADDRES4  = TRIM(laShipTo[4])
      m.O_CADDRES5  = TRIM(laShipTo[5])
    ELSE     
      m.O_CADDRES3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    ENDIF 
 
   
    IF LEN(TRIM(laShipTo[2])) =0
      m.O_STADDR2   = laShipTo[3]
      IF llAltShp    
        m.O_CADDRES3 = m.O_CADDRES4
        m.O_CADDRES4 = m.O_CADDRES5
        m.O_CADDRES5 = ''
      ELSE
        m.O_CADDRES3 = ''
      ENDIF
    ENDIF
    SELECT INVHDR
    
    STORE "" TO m.cFacName ,m.cFactor1,m.cFactor2,APPROVAL ,m.cFactor3,m.cFactor4 ,m.cFactor5
    
    IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  		m.cFacName = lcFacName  
    ENDIF  
  
 
    IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
      m.cFactor1 = laFactor[1]
    ENDIF  
   
    IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
      m.cFactor2 = laFactor[2]
    ENDIF
    m.APPROVAL    = APPROVAL    
    IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
      m.cFactor3 = laFactor[3]
    ENDIF
     
    IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)    
      m.cFactor4 = laFactor[4]
      m.cFactor5 = laFactor[5]
    ENDIF
    STORE "" TO m.CUSTPO ,m.STORE,m.DEPT,m.REP1,m.REP2
    STORE 0 TO m.FR_INS_COD,m.TAX_RATE,m.TAX_AMT,m.DISCOUNT,M.nPstRate,M.nPstAmt,m.nHSTRate,m.nHSTAmt,m.TOTALCHG,m.CARTONS 
    m.CUSTPO = LEFT(CUSTPO,12)
    m.STORE = STORE
    m.DEPT   = DEPT  
    m.REP1 = REP1
    m.REP2 = REP2
    IF XTAX .AND. XTAX_METH = 'M' .AND. INVHDR.TAX_AMT # 0
      m.TAX_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
      m.TAX_AMT  = INVHDR->TAX_AMT   
    ENDIF  
    IF XTAX .AND. XTAX_METH = 'A' .AND. INVHDR.TAX_AMT # 0
      m.TAX_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
      m.TAX_AMT  = INVHDR->TAX_AMT   
    ENDIF  

    m.FR_INS_COD  = invhdr.FREIGHT + invhdr.INSUR + invhdr.COD
    m.DISCOUNT = invhdr.DISCOUNT
   IF xTAx .AND. llIsCanada 
     lcStrRate = ALLTRIM(STR(InvHdr->nPstRate,5,2))
     M.nPstRate = lcStrRate 
     M.nPstAmt = InvHdr->nPstAmt 
     IF InvHdr.nHSTAmt <> 0
       m.nHSTRate = InvHdr.nHSTRate
       m.nHSTAmt = InvHdr.nHSTAmt 
     ENDIF  
   ENDIF
   m.TOTALCHG   = INVHDR->TOTALCHG  
   m.CARTONS = INVHDR->CARTONS   
   IF llNote
     STORE "" TO m.MnotHd
     STORE "F" TO m.hasnot
     SELECT NOTEPAD
     IF SEEK('C' + m.INVOICE,"NOTEPAD")
       lnOldMemW = SET("MEMOWIDTH")
       SET MEMOWIDTH TO 75
       m.MnotHd  = NOTEPAD.MNOTES
       m.hasnot = IIF(!EMPTY(NOTEPAD.MNOTES),'T','F')
       SET MEMOWIDTH TO lnOldMemW
     ENDIF
   ENDIF 
   SELECT(lcHeader)
   APPEND BLANK 
   GATHER MEMO MEMVAR 
  

   SELECT INVLINE
   IF SEEK(invhdr.INVOICE)
     SCAN REST WHILE INVOICE+STR(LINENO,6) = invhdr.INVOICE FOR TOTQTY <>  0
       SELECT(lcLines)
       APPEND BLANK 
       REPLACE Invoice WITH invhdr.INVOICE
       IF SEEK ('S'+ INVLINE.SCALE,'SCALE')
         SELECT (lcLines)
         REPLACE scale WITH invline.scale 
         REPLACE  SZ1 WITH scale.Sz1 ,;
                  SZ2 WITH scale.Sz2 ,; 
                  SZ3 WITH scale.Sz3 ,; 
                  SZ4 WITH scale.Sz4 ,; 
                  SZ5 WITH scale.Sz5 ,; 
                  SZ6 WITH scale.Sz6 ,; 
                  SZ7 WITH scale.Sz7 ,; 
                  SZ8 WITH scale.Sz8 
       ENDIF 
       SELECT(lcLines)
       REPLACE STORE     WITH INVLINE.STORE,;
               Account   WITH INVLINE.ACCOUNT,;
               STYLE     WITH INVLINE.Style,;
               StyDesc   WITH  IIF(SEEK(invline.style,'Style'),SUBSTR(STYLE.DESC,1,60),''),;
               QTY1      WITH INVLINE.QTY1 ,;
               QTY2      WITH INVLINE.QTY2 ,;
               QTY3      WITH INVLINE.QTY3 ,;
               QTY4      WITH INVLINE.QTY4 ,;
               QTY5      WITH INVLINE.QTY5 ,;
               QTY6      WITH INVLINE.QTY6 ,;
               QTY7      WITH INVLINE.QTY7 ,;
               QTY8      WITH INVLINE.QTY8  ,;
               TOTQTY    WITH INVLINE.TOTQTY ,;
               Price     WITH INVLINE.Price ,;
               Dscrption WITH INVLINE.dESC1
               IF llRpInvLNt
                 SELECT(lcLines)
                 REPLACE MNote WITH invline.NOTE_MEM
                 REPLACE lHasNotes WITH IIF(EMPTY(invline.NOTE_MEM),'F','T')
               ENDIF 
               DO lpPrtSku      
     ENDSCAN   
   ENDIF 
   SELECT INVLINE
ENDSCAN  
lcLines1 = loOGScroll.gfTempName()
lcHeader1 = loOGScroll.gfTempName()
*!*  lcPackFile1 = loOGScroll.gfTempName()

SELECT(lcHeader)
COPY TO oAriaApplication.WorkDir +  lcHeader1  + ".DBF" 
SELECT(lcLines)
COPY TO oAriaApplication.WorkDir +  lcLines1  + ".DBF" 
*!*  SELECT(lcPackFile)
*!*  COPY TO oAriaApplication.WorkDir +  lcPackFile1  + ".DBF" 
DIMENSION loOgScroll.lacrTABLES[2]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir +  lcHeader1  + ".DBF" 
loOgScroll.lacrTABLES[2]= oAriaApplication.WorkDir +  lcLines1  + ".DBF" 
*!*  loOgScroll.lacrTABLES[3]= oAriaApplication.WorkDir +  lcPackFile1 + ".DBF" 
loogScroll.cCROrientation = 'P'
gfdispre()
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)


SELECT(lcHeader)
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [Start]
SCAN 
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (&lcHeader..INVOICE)
ENDSCAN
RETURN 
      *B802987,1 [START] Print Line Notes

   *** END LINE PROCESSING
   *SET DEVICE TO PRINT
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
   
     *B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
      ROW = 49
     *B803203,1 [END]
      @ ROW+1,12 SAY 'C O N T I N U E D ...'

      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
      *B802337,1 BWA 06/14/1999 Modify the phone format
      *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
      @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
      *B802337,1 BWA [END]
      *@ 50,07 SAY INVHDR->PHONE
      *B500772,1 HISH 06/29/95.  ( End )

      SELECT INVHDR
      @ 50,56 SAY 'MERCHANDISE'
      @ 50,75 SAY '**********'
      ROW=ROW+1
      
      *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
      *@ 51,07 SAY INVHDR->NOTE1
      *@ 53,07 SAY INVHDR->NOTE2
      @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
      @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
      *:B802642,1 ABD 12/02/1999 [ End ]
     
     *B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
     *@ 58,18 SAY INVHDR->CARTONS                   PICTURE '@Z 999'
     *@ 58,62 SAY '******'
     *@ 58,75 SAY '******'
      
      @ 56,16 SAY INVHDR->CARTONS                   PICTURE '@Z 999'
      @ 56,65 SAY '*******'
      @ 56,75 SAY '**********'
     *B802722,1 BWA 10/24/1999 [END]
     
      ROW = 1
      *B602953,1 BWA [END]
      LOOP
   ENDIF
   
IF llNote
   SELECT NOTEPAD
   
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75
   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
     
     *B802373,1 BWA 06/27/1999 Fix the bug of not printing the notepad in the second time preview
     *DO WHILE lnNotLine <= lnMemLins
     DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
     *B802373,1 BWA 06/27/1999 [END]
       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
       *B602953,1 BWA 05/31/1999 Adjust invoice form H due to the attached form.          
         *EXIT
       ELSE
         XNOTE_LOOP = .F.
         *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         *B802642,1  if the line Begin With '*'  Don't Print It. [ Begin ]
         *@ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
         *ROW = ROW + 1
         lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
         *B802987,1 [start] check the alltrim of the notepad
         *IF  LEFT(lcCurrLine,1) # '*'
         IF  LEFT(ALLTRIM(lcCurrLine),1) # '*'
         *B802987,1 [end]
           @ ROW,02 SAY lcCurrLine
           ROW = ROW + 1
         ENDIF
        *B802642,1 Abd [ End ]
       ENDIF
       IF ROW >= MAXROW      && if there is a line will de delete from 
         =lfGetcont()        && the memo field
         =lfGetInHdr()
       ENDIF
       *B602953,1 BWA 05/31/1999 [END]
       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF !XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       *B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
       ROW = 49
       *B803203,1 [END]
     
       @ ROW+1,12 SAY 'C O N T I N U E D ...'

       *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
       *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
       @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
       *@ 50,07 SAY INVHDR->PHONE
       *B500772,1 HISH 06/29/95.  ( End )

       SELECT INVHDR
       @ 50,56 SAY 'MERCHANDISE'
       @ 50,75 SAY '**********'
       ROW = ROW + 1
       
       *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
       *@ 51,07 SAY INVHDR->NOTE1
       *@ 53,07 SAY INVHDR->NOTE2
       @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
       @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
       *:B802642,1 ABD 12/02/1999 [ End ]

     *B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
     *@ 58,18 SAY INVHDR->CARTONS PICTURE '@Z 999'
     *@ 58,62 SAY '******'
     *@ 58,75 SAY '******'
      
     @ 56,16 SAY INVHDR->CARTONS PICTURE '@Z 999'
     @ 56,65 SAY '*******'
     @ 56,75 SAY '**********'
     *B802722,1 BWA 10/24/1999 [END]
     
     *B602953,1 BWA 05/31/1999 Adjust invoice form H due to the attached form.
       ROW = 1
       *B602953,1 BWA [END]
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF

   *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
   *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
   @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
   *@ 50,07 SAY INVHDR->PHONE
   *B500772,1 HISH 06/29/95.  ( End )

   SELECT INVHDR
   @ 50,56 SAY 'MERCHANDISE'
   @ 50,74 SAY XSUBTOTAL            PICTURE '9999999.99'  

   ROW=51
   
   *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
   *@ 51,07 SAY INVHDR->NOTE1
   @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
   *:B802642,1 ABD 12/02/1999 [ End ]

   
   
   *B802722,1 BWA 10/24/1999 Fix the bug of the Sales tax = 0  we don't print it[START]
*!*************************************************************
*! Name : gfGetZone.
*! Auth : Tarek Mohamed Ismael (TMI).
*! Date : 04/05/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

IF !USED('FRTZONES')
  lnOldWrk = SELECT()
  SELECT 0
  DO NETUSE WITH '&QLB.FRTZONES','&QLB.FRTZONES','SH'
  SELECT (lnOldWrk)
ENDIF

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'FRTZONES'),'',FRTZONES.ZONE)


*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Timour Abdalla Khalil.
*! Date : 07/20/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
PROCEDURE lpPrtSku

*IF ! SEEK('S'+InvLine.Account+InvLine.Style+InvLine.Color,'Spck_Lin')
IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
  laSku = SPACE(16)
*  SCAN WHILE Type+Account+Style+Color = 'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. lnI < 9
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
*  = SEEK('S'+InvLine.Account+InvLine.Style+InvLine.Color,'Spck_Lin')
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
*  DO WHILE Type+Account+Style+Color = 'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. lnI < 9
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
*    DO WHILE Type+Account+Style+Color = ;
*             'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. !EOF()
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()

      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    IF lnI = 1
      SELECT(lcLines)
      REPLACE cStrToPrn WITH lcStrToPrn
    ELSE
      SELECT(lcLines)
      SCATTER MEMO MEMVAR 
      APPEND BLANK 
      GATHER MEMO MEMVAR 
      REPLACE cStrToPrn WITH lcStrToPrn
    ENDIF 
*!*      SELECT(lcPackFile)
*!*      APPEND BLANK 
*!*      REPLACE Account   WITH spck_lin.Account,;
*!*              style     WITH spck_lin.Style,;
*!*              cStrToPrn WITH lcStrToPrn

    SELECT Spck_Lin
  ENDDO  
ELSE
  SELECT(lcLines)
  REPLACE  Sku  WITH spck_lin.Sku
*!*    SELECT(lcPackFile)
*!*    APPEND BLANK 
*!*    REPLACE Account   WITH spck_lin.Account,;
*!*              style     WITH spck_lin.Style,;
*!*              Sku  WITH spck_lin.Sku
  SELECT Spck_Lin
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*:Modifications:
*:B602962,1 BWA Collect all the header lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lnSelFile , lcalias

lcAlias = ALIAS()
*B802722,1 BWA 10/24/1999 Fix the bug to print REF. ACCOUNT[START]
IF XINVNAME='Y'
  *@ 03,05 SAY HLINE1
  @ 01,05 SAY HLINE1
  SELE INVHDR
ENDIF

@ 01,60 SAY XINVOICE
@ 01,75 SAY INVDATE        

IF XINVNAME='Y'
    @ 02,05 SAY HLINE2    
ENDIF

* LINE 4
*IF XINVNAME='Y'
  *@ 04,05 SAY HLINE2
*ENDIF

* LINE 5
IF XINVNAME='Y'
    *@ 05,05 SAY HLINE3
    @ 03,05 SAY HLINE3    
ENDIF

@ 03,75 SAY SUBSTR( CDIVISION, 1, 14 )  

IF XINVNAME='Y'
    @ 04,05 SAY HLINE4
ENDIF
  
* LINE 6
*IF XINVNAME='Y'
    *@ 06,05 SAY HLINE4
*ENDIF

* LINE 7
IF XINVNAME='Y'
  *@ 07,05 SAY XPHONE  SIZE 1,16
  @ 05,05 SAY XPHONE  SIZE 1,16     
ENDIF

@ 05,75 SAY ORDER    

*B802722,1 BWA 10/24/1999 [END]
IF XINVNAME='Y' AND !EMPTY(lcDunsNo)  
  @ 06,05 SAY 'Duns# :'
  @ 06,13 SAY lcDunsNo
ENDIF

=lfSolSpAdr()
m.XBTNAME  = lcSolTName
XBTADDR1 = laSoldTo[1]
XBTADDR2 = laSoldTo[2]
XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(laSoldTo[2])) =0
  XBTADDR2 = laSoldTo[3]
  XBTADDR3 = ''
ENDIF

XSTNAME = lcShpTName
XSTADDR1 = laShipTo[1]
XSTADDR2 = laShipTo[2]
*[Start]
*B802988 ,1 NAD  01/25/2000 Add  address4 ,address5 to the ship to section 
*XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  
llAltShp=OrdHdr.Alt_ShpTo
IF llAltShp
  XSTADDR3 = TRIM(laShipTo[3]) 
  XSTADDR4 =TRIM(laShipTo[4])
  XSTADDR5 = TRIM(laShipTo[5])
  
ELSE     
  XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
 
ENDIF 
*B802988 ,1 NAD 01/25/2000  [End]
   
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      *[Start]
      *B802988 ,1  01/25/2000  NAD Shift address4 ,address5 if address2 was empty  
      *XSTADDR3 = ''
      IF llAltShp 
        XSTADDR3 = XSTADDR4
        XSTADDR4 = XSTADDR5
        XSTADDR5 = ''
      ELSE
        XSTADDR3 = ''
      ENDIF 
   *IF LEN(TRIM(laShipTo[2])) =0
     * XSTADDR2 = laShipTo[3]
     * XSTADDR3 = ''
   *ENDIF
   ENDIF
  *B802988 ,1 NAD 01/25/2000  [End]

SELECT INVHDR

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 07,14 SAY "Remit to :"
  @ 07,25 SAY lcFacName 
ENDIF    
@ 07,75 SAY PIKTKT        
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 08,25 SAY laFactor[1]
ENDIF  
*B802722,1 BWA 10/24/1999 [END]

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 09,25 SAY laFactor[2]
ENDIF
*B802722,1 BWA 10/24/1999 [END]

@ 09,75 SAY APPROVAL      

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 10,25 SAY laFactor[3]
ENDIF
*B802722,1 BWA 10/24/1999 [END]

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
ENDIF

*B802722,1 BWA 10/24/1999 Fix the bug to print REF. ACCOUNT[START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 12,25 SAY "Ref. Account:"
  *B802988 ,1 NAD 01/24/2000 [BEGIN] Print the factor account from the customer file.
  *@ 12,39 SAY CUSTOMER.FACTACCT
*  @  12,39 SAY lcFACTACCT
ENDIF
*B802988 ,1 NAD 01/24/2000 [END]
*B802722,1 BWA 10/24/1999 [END]
  
*B802722,1 BWA 10/24/1999 [END]
*!*	@ 13,06 SAY XBTNAME
*!*	@ 13,56 SAY XSTNAME
*!*	@ 14,06 SAY XBTADDR1
*!*	@ 14,56 SAY SUBSTR(XSTADDR1,1,25)
*!*	@ 15,06 SAY XBTADDR2
*!*	@ 15,56 SAY SUBSTR(XSTADDR2,1,25)
*!*	@ 16,06 SAY SUBSTR(XBTADDR3,1,45)
*!*	@ 16,56 SAY SUBSTR(XSTADDR3,1,29)
*[Start]
*B802988 ,1 NAD 01/25/2000 Print address4 ,address5 
IF llAltShp

*!*	  @ 17,56 SAY ALLTRIM(SUBSTR(XSTADDR4,1,29))
*!*	  
*!*	  @ 18,56 SAY ALLTRIM(SUBSTR(XSTADDR5,1,29))
ENDIF
*B802988 ,1 NAD 01/25/2000  [End]

*!*	@ 21,02 SAY ACCOUNT

*B804555,1 Shift the Custpo 3 characters to the right. [Begin]
*mmm
*@ 21,15 SAY LEFT(CUSTPO,10)
*@ 21,12 SAY LEFT(CUSTPO,10)
*!*	@ 21,11  SAY LEFT(CUSTPO,12)
* mmm
*B804555,1 Shift the Custpo 3 characters to the right. [End]
m.Store = IIF(!EMPTY(lcDist),lcDist,STORE)
*@ 21,23 SAY IIF(!EMPTY(lcDist),lcDist,STORE)

*B803203,1 BWA 06/21/2000 2-Fix the bug of moving the departement field 2 character right.[START]
*@ 21,33 SAY DEPT
*@ 21,35 SAY DEPT
*B803203,1 [END]

@ 21,41 SAY LEFT(PTERMS,21)
@ 21,64 SAY REP1                         
@ 21,68 SAY REP2
@ 21,72 SAY SUBSTR(PSHIPVIA,1,13)

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
ROW = 23
*B802797,1 BWA 11/18/1999 Fix the bug of not printing all the scales[START]
*lnSelFile =  SELECT(0)
*SELECT SCALE
*@ ROW,20 SAY SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
*SELECT (lnSelFile)                     
@ 23,20 SAY lcScale1
@ 24,20 SAY lcScale2
*B802797,1 BWA 11/18/1999 [END]
*B802722,1 BWA 10/24/1999 [END]
ROW = 27

SELECT (lcalias)
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*:Modifications:
*:B602962,1 BWA Collect all the footer lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetcont


*B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
ROW = 49
*B803203,1 [END]

@ ROW+1,12 SAY 'C O N T I N U E D ...'
*B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]
*@ 50,07 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
@ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
*B802722,1 BWA 10/24/1999 [END]

SELECT INVHDR
@ 50,56 SAY 'MERCHANDISE'
@ 50,75 SAY '**********'
ROW = ROW + 1


*:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
*@ 51,07 SAY INVHDR->NOTE1
*@ 53,07 SAY INVHDR->NOTE2
@ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
@ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
*:B802642,1 ABD 12/02/1999 [ End ]


*B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
*@ 58,18 SAY INVHDR->CARTONS PICTURE '@Z 999'
*@ 58,62 SAY '******'
*@ 58,75 SAY '******'

@ 56,16 SAY INVHDR->CARTONS PICTURE '@Z 999'
@ 56,65 SAY '*******'
@ 56,75 SAY '**********'
*B802722,1 BWA 10/24/1999 [END]



FUNCTION lfCreateTemp
DIMENSION laHeader[51,4]
STORE '' TO laHeader


laHeader[1,1] = 'INVOICE'
laHeader[1,2] = 'C'
laHeader[1,3] = 6
laHeader[1,4] = 0

laHeader[2,1] = 'NOTE1'
laHeader[2,2] = 'C'
laHeader[2,3] = 30
laHeader[2,4] = 0

laHeader[3,1] = 'NOTE2'
laHeader[3,2] = 'C'
laHeader[3,3] = 30
laHeader[3,4] = 0

laHeader[4,1] = 'ORDER'
laHeader[4,2] = 'C'
laHeader[4,3] = 6
laHeader[4,4] = 0

laHeader[5,1] = 'PIKTKT'
laHeader[5,2] = 'C'
laHeader[5,3] = 6
laHeader[5,4] = 0

laHeader[6,1] = 'ACCOUNT'
laHeader[6,2] = 'C'
laHeader[6,3] = 5
laHeader[6,4] = 0

laHeader[7,1] = 'STORE'
laHeader[7,2] = 'C'
laHeader[7,3] = 8
laHeader[7,4] = 0

laHeader[8,1] = 'CDIVISION'
laHeader[8,2] = 'C'
laHeader[8,3] = 6
laHeader[8,4] = 0

laHeader[9,1] = 'XBTNAME'
laHeader[9,2] = 'C'
laHeader[9,3] = 30
laHeader[9,4] = 0

laHeader[10,1] = 'CCADDRESS1'
laHeader[10,2] = 'C'
laHeader[10,3] = 30
laHeader[10,4] = 0

laHeader[11,1] = 'CCADDRESS2'
laHeader[11,2] = 'C'
laHeader[11,3] = 30
laHeader[11,4] = 0

laHeader[12,1] = 'CCADDRESS3'
laHeader[12,2] = 'C'
laHeader[12,3] = 30
laHeader[12,4] = 0

laHeader[13,1] = 'CCADDRESS4'
laHeader[13,2] = 'C'
laHeader[13,3] = 30
laHeader[13,4] = 0

laHeader[14,1] = 'CCADDRESS5'
laHeader[14,2] = 'C'
laHeader[14,3] = 30
laHeader[14,4] = 0

laHeader[15,1] = 'c_FactAcct'
laHeader[15,2] = 'C'
laHeader[15,3] = 10
laHeader[15,4] = 0

laHeader[16,1] = 'O_CADDRES1'
laHeader[16,2] = 'C'
laHeader[16,3] = 30
laHeader[16,4] = 0

laHeader[17,1] = 'O_STADDR2'
laHeader[17,2] = 'C'
laHeader[17,3] = 30
laHeader[17,4] = 0

laHeader[18,1] = 'O_CADDRES3'
laHeader[18,2] = 'C'
laHeader[18,3] = 30
laHeader[18,4] = 0

laHeader[19,1] = 'O_CADDRES4'
laHeader[19,2] = 'C'
laHeader[19,3] = 30
laHeader[19,4] = 0


laHeader[20,1] = 'O_CADDRES5'
laHeader[20,2] = 'C'
laHeader[20,3] = 30
laHeader[20,4] = 0

laHeader[21,1] = 'INVDATE'
laHeader[21,2] = 'D'
laHeader[21,3] = 8
laHeader[21,4] = 0

laHeader[22,1] = 'CFACCODE'
laHeader[22,2] = 'C'
laHeader[22,3] = 6
laHeader[22,4] = 0

laHeader[23,1] = 'APPROVAL'
laHeader[23,2] = 'C'
laHeader[23,3] = 10
laHeader[23,4] = 0

laHeader[24,1] = 'cFactor1'
laHeader[24,2] = 'C'
laHeader[24,3] = 30
laHeader[24,4] = 0

laHeader[25,1] = 'cFactor2'
laHeader[25,2] = 'C'
laHeader[25,3] = 30
laHeader[25,4] = 0

laHeader[26,1] = 'cFactor3'
laHeader[26,2] = 'C'
laHeader[26,3] = 30
laHeader[26,4] = 0

laHeader[27,1] = 'cFactor4'
laHeader[27,2] = 'C'
laHeader[27,3] = 30
laHeader[27,4] = 0

laHeader[28,1] = 'cFactor5'
laHeader[28,2] = 'C'
laHeader[28,3] = 30
laHeader[28,4] = 0

laHeader[29,1] = 'XSTNAME'
laHeader[29,2] = 'C'
laHeader[29,3] = 30
laHeader[29,4] = 0

laHeader[30,1] = 'llAltShp'
laHeader[30,2] = 'N'
laHeader[30,3] = 1
laHeader[30,4] = 0

laHeader[31,1] = 'CUSTPO'
laHeader[31,2] = 'C'
laHeader[31,3] = 15
laHeader[31,4] = 0

laHeader[32,1] = 'DEPT'
laHeader[32,2] = 'C'
laHeader[32,3] = 5
laHeader[32,4] = 0

laHeader[33,1] = 'PTERMS'
laHeader[33,2] = 'C'
laHeader[33,3] = 30
laHeader[33,4] = 0

laHeader[34,1] = 'PSHIPVIA'
laHeader[34,2] = 'C'
laHeader[34,3] = 30
laHeader[34,4] = 0

laHeader[35,1] = 'PSPCINST'
laHeader[35,2] = 'C'
laHeader[35,3] = 30
laHeader[35,4] = 0

laHeader[36,1] = 'REP1'
laHeader[36,2] = 'C'
laHeader[36,3] = 3
laHeader[36,4] = 0

laHeader[37,1] = 'REP2'
laHeader[37,2] = 'C'
laHeader[37,3] = 3
laHeader[37,4] = 0

laHeader[38,1] = 'PHONE'
laHeader[38,2] = 'C'
laHeader[38,3] = 20
laHeader[38,4] = 0

laHeader[39,1] = 'CARTONS'
laHeader[39,2] = 'N'
laHeader[39,3] = 5
laHeader[39,4] = 0

laHeader[40,1] = 'FR_INS_COD'
laHeader[40,2] = 'N'
laHeader[40,3] = 10
laHeader[40,4] = 2

laHeader[41,1] = 'TOTALCHG'
laHeader[41,2] = 'N'
laHeader[41,3] = 14
laHeader[41,4] = 2

laHeader[42,1] = 'nHSTRate'
laHeader[42,2] = 'N'
laHeader[42,3] = 10
laHeader[42,4] = 2

laHeader[43,1] = 'nHSTAmt'
laHeader[43,2] = 'N'
laHeader[43,3] = 13
laHeader[43,4] = 2

laHeader[44,1] = 'nPstRate'
laHeader[44,2] = 'N'
laHeader[44,3] = 10
laHeader[44,4] = 2

laHeader[45,1] = 'nPstAmt'
laHeader[45,2] = 'N'
laHeader[45,3] = 13
laHeader[45,4] = 2

laHeader[46,1] = 'cFacName'
laHeader[46,2] = 'C'
laHeader[46,3] = 30
laHeader[46,4] = 0

laHeader[47,1] = 'MnotHd'
laHeader[47,2] = 'M'
laHeader[47,3] = 256
laHeader[47,4] = 0

laHeader[48,1] = 'TAX_AMT'
laHeader[48,2] = 'N'
laHeader[48,3] = 10
laHeader[48,4] = 2

laHeader[49,1] = 'TAX_RATE'
laHeader[49,2] = 'N'
laHeader[49,3] = 15
laHeader[49,4] = 2

laHeader[50,1] = 'Discount'
laHeader[50,2] = 'N'
laHeader[50,3] = 13
laHeader[50,4] = 2
   
laHeader[51,1] = 'hasnot'
laHeader[51,2] = 'C'
laHeader[51,3] = 1
laHeader[51,4] = 0
   

gfCrtTmp(lcHeader,@laHeader,"Invoice",lcHeader,.F.)


DIMENSION laLines[37,4]

laLines[1,1]='Invoice'
laLines[1,2]='C'
laLines[1,3]=8
laLines[1,4]=0

laLines[2,1]='Required'
laLines[2,2]='N'
laLines[2,3]=8
laLines[2,4]=2

laLines[3,1]='Shipped'
laLines[3,2]='N'
laLines[3,3]=8
laLines[3,4]=2

laLines[4,1]='Price'
laLines[4,2]='N'
laLines[4,3]=8
laLines[4,4]=2

laLines[5,1]='Amount'
laLines[5,2]='N'
laLines[5,3]=19
laLines[5,4]=2

laLines[6,1]='TotAmnt'
laLines[6,2]='N'
laLines[6,3]=8
laLines[6,4]=2

laLines[7,1]='Dscrption'
laLines[7,2]='C'
laLines[7,3]=30
laLines[7,4]=0

laLines[8,1]='Dscrpt2'
laLines[8,2]='C'
laLines[8,3]=30
laLines[8,4]=0

laLines[9,1]='StyDesc'
laLines[9,2]='C'
laLines[9,3]=60
laLines[9,4]=0

laLines[10,1]='PackId'
laLines[10,2]='C'
laLines[10,3]=16
laLines[10,4]=0

laLines[11,1]='Count'
laLines[11,2]='N'
laLines[11,3]=1
laLines[11,4]=0


laLines[12,1] = 'llPack'
laLines[12,2] = 'L'
laLines[12,3] = 1
laLines[12,4] = 0

laLines[13,1] = 'Scale'
laLines[13,2] = 'C'
laLines[13,3] = 3
laLines[13,4] = 0

laLines[14,1] = 'SZ1'
laLines[14,2] = 'C'
laLines[14,3] = 5
laLines[14,4] = 0

laLines[15,1] = 'SZ2'
laLines[15,2] = 'C'
laLines[15,3] = 5
laLines[15,4] = 0

laLines[16,1] = 'SZ3'
laLines[16,2] = 'C'
laLines[16,3] = 5
laLines[16,4] = 0

laLines[17,1] = 'SZ4'
laLines[17,2] = 'C'
laLines[17,3] = 5
laLines[17,4] = 0

laLines[18,1] = 'SZ5'
laLines[18,2] = 'C'
laLines[18,3] = 5
laLines[18,4] = 0

laLines[19,1] = 'SZ6'
laLines[19,2] = 'C'
laLines[19,3] = 5
laLines[19,4] = 0

laLines[20,1] = 'SZ7'
laLines[20,2] = 'C'
laLines[20,3] = 5
laLines[20,4] = 0

laLines[21,1] = 'SZ8'
laLines[21,2] = 'C'
laLines[21,3] = 5
laLines[21,4] = 0

laLines[22,1] = 'STORE'
laLines[22,2] = 'C'
laLines[22,3] = 8
laLines[22,4] = 0

laLines[23,1] = 'STYLE'
laLines[23,2] = 'C'
laLines[23,3] = 19
laLines[23,4] = 0

laLines[24,1] = 'QTY1'
laLines[24,2] = 'N'
laLines[24,3] = 6
laLines[24,4] = 0

laLines[25,1] = 'QTY2'
laLines[25,2] = 'N'
laLines[25,3] = 6
laLines[25,4] = 0

laLines[26,1] = 'QTY3'
laLines[26,2] = 'N'
laLines[26,3] = 6
laLines[26,4] = 0

laLines[27,1] = 'QTY4'
laLines[27,2] = 'N'
laLines[27,3] = 6
laLines[27,4] = 0

laLines[28,1] = 'QTY5'
laLines[28,2] = 'N'
laLines[28,3] = 6
laLines[28,4] = 0

laLines[29,1] = 'QTY6'
laLines[29,2] = 'N'
laLines[29,3] = 6
laLines[29,4] = 0

laLines[30,1] = 'QTY7'
laLines[30,2] = 'N'
laLines[30,3] = 6
laLines[30,4] = 0

laLines[31,1] = 'QTY8'
laLines[31,2] = 'N'
laLines[31,3] = 6
laLines[31,4] = 0

laLines[32,1] = 'TOTQTY'
laLines[32,2] = 'N'
laLines[32,3] = 8
laLines[32,4] = 0

laLines[33,1] = 'MNote'
laLines[33,2] = 'M'
laLines[33,3] = 256
laLines[33,4] = 0

laLines[34,1] = 'Account'
laLines[34,2] = 'C'
laLines[34,3] = 5
laLines[34,4] = 0

laLines[35,1] = 'lHasNotes'
laLines[35,2] = 'C'
laLines[35,3] = 1
laLines[35,4] = 0

laLines[36,1] = 'cStrToPrn'
laLines[36,2] = 'C'
laLines[36,3] = 100
laLines[36,4] = 0

laLines[37,1] = 'Sku'
laLines[37,2] = 'C'
laLines[37,3] = 16
laLines[37,4] = 0


gfCrtTmp(lcLines,@laLines,"Invoice",lcLines,.F.)


*!*  DIMENSION laPackFile[4,4]

*!*  laPackFile[1,1] = 'Account'
*!*  laPackFile[1,2] = 'C'
*!*  laPackFile[1,3] = 5
*!*  laPackFile[1,4] = 0

*!*  laPackFile[2,1] = 'Style'
*!*  laPackFile[2,2] = 'C'
*!*  laPackFile[2,3] = 19
*!*  laPackFile[2,4] = 0

*!*  laPackFile[3,1] = 'cStrToPrn'
*!*  laPackFile[3,2] = 'C'
*!*  laPackFile[3,3] = 60
*!*  laPackFile[3,4] = 0

*!*  laPackFile[4,1] = 'Sku'
*!*  laPackFile[4,2] = 'C'
*!*  laPackFile[4,3] = 16
*!*  laPackFile[4,4] = 0

*!*  gfCrtTmp(lcPackFile ,@laPackFile,"Account+Style",lcPackFile ,.F.)