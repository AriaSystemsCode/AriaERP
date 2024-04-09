*:***************************************************************************
*: Program file  : ARPINVDR.PRG
*: Program desc. : Customized Invoice Form for Direct Corporate Clothing [DIR03]
*: Date          : 29/11/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Nader Nabil Abd Elmonam (NNA)
*: Tracking Job Number : C123950
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Example : DO ARPINVDR
*:**************************************************************************
*:Modification:
*:**************************************************************************
*:C127805,1 MMR 06/15/2005 Adding order charges to the both forms.
PRIVATE lcCentury
STORE '' TO lcCentury
lcCentury = SET('CENTURY')
SET CENTURY ON

IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH') 
ENDIF
IF !USED('PACK_HDR')
  = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
ENDIF
*:C127805,1 MMR 06/15/2005 Open the invchrg table [Start]
 IF !USED('INVCHRG')
   =gfOpenFile(gcDataDir+'INVCHRG','INVCHRG','SH')
   SET ORDER TO Invchrg
 ENDIF
*:C127805,1 MMR [End]
DO CASE
  CASE lcRpFormN = 'S'
    lcFormName  = 'ARPINVM'
    =gfCrtFrm(lcFormName,'',.T.)

    SELECT INVHDR
     DO (gcRepHome + 'ARPINVM.PRG')
    RETURN

  CASE lcRpFormN = 'C'
    lcRpExp = lcRpExp + " AND INVHDR.CONSOL='Y'"
    = lfSetRel_1()
    = lfCrtTmp()
    = lfColDataC()
    = lfSetRel_2()
    lcFormName  = 'ARPINVDR'
    =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
    SELECT INVHDR
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
    SET DEVICE TO SCREEN
    llarpinv = .F.

  CASE lcRpFormN = 'N'
    lcRpExp = lcRpExp + " AND INVHDR.CONSOL='Y'"
    = lfSetRel_1()
    = lfCrtTmp()
    = lfColDataN()
    = lfSetRel_2()
    lcFormName  = 'ARPINVDN'
    =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
    SELECT INVHDR
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
    SET DEVICE TO SCREEN
    llarpinv = .F.
ENDCASE
=lfRelease()
SET CENTURY &lcCentury
*:C127805,1 MMR 06/15/2005 Close the invchrg table [Start]
SELECT INVCHRG
USE
*:C127805,1 MMR [End]
*!*************************************************************
*! Name        : lfCrtTmp
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Creat the temp. File for form ARPINVDC
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
*!C123950,1
FUNCTION lfCrtTmp
IF lcRpFormN = 'C'
  CREATE TABLE (gcWorkDir + lcCoTmpFil)(Invoice C(6),STORE C(8) , Dpoint C(50) , NetVal N(10,2),;
              Vat N(10,5) , Gross N(10,5))
  INDEX ON (INVOICE+STORE) TAG (lcCoTmpFil) OF (lcCoTmpFil)
  CREATE TABLE (gcWorkDir + lcTmpDate)(Invoice C(6),FDate D(8) , ToDate D(8))
  INDEX ON (INVOICE) TAG (lcTmpDate) OF (lcTmpDate)
  *:C127805,1 MMR 06/15/2005 Create the temp file to hold charges for form by Site [Start]
  CREATE TABLE (gcWorkDir + lcSTmpChr)(Invoice C(6),STORE C(8) , Dpoint C(50) , NetVal N(10,2),;
              Vat N(10,5) , Gross N(10,5))
  INDEX ON (INVOICE+STORE) TAG (lcSTmpChr) OF (lcSTmpChr)
  *:C127805,1 MMR [End]
ELSE
  CREATE TABLE (gcWorkDir + lcDnTmpFil)(Invoice C(6),CustRef C(15) , Piktkt C(6) ,dShipDate D(8),;
               NetVal N(10,2) , Vat N(10,5) , Gross N(10,5))
  INDEX ON (INVOICE+PIKTKT) TAG (lcDnTmpFil) OF (lcDnTmpFil)
  *--NNA
  CREATE TABLE (gcWorkDir + lcTotalTmp)(Invoice C(6),GTotVat N(10,5) , GTotGross N(10,5))
  INDEX ON (INVOICE) TAG (lcTotalTmp) OF (lcTotalTmp)
  *--NNA
  *:C127805,1 MMR 06/15/2005 Create the temp file to hold charges for form by DN [Start]
  CREATE TABLE (gcWorkDir + lcDnTmpChr)(Invoice C(6), Piktkt C(6) ,;
               NetVal N(10,2) , Vat N(10,5) , Gross N(10,5))
  INDEX ON (INVOICE+PIKTKT) TAG (lcDnTmpChr) OF (lcDnTmpChr)
  *:C127805,1 MMR [End]
ENDIF
*--End function lfCrtTmp.
*!*************************************************************
*! Name        : lfColDataC
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Collect data for form ARPINVDC
*!             : (Consolidated By Site)
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataC()
*!*************************************************************
*!C123950,1
FUNCTION lfColDataC
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
*PRIVATE lcStore , lnNetVal , lnVat , lnGross
PRIVATE lcStore , lnNetVal , lnVat , lnGross,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcStore
STORE 0 TO lnNetVal , lnVat , lnGross
SELECT INVHDR
SCAN FOR &LCRPEXP 
  STORE {} TO ldFromDate , ldToDate
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE
      IF SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') AND;
         SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
         SELECT PACK_HDR
           IF !(lcStore == Consinvh.Store)
             lcStore = Consinvh.Store
             STORE 0 TO lnNetVal , lnVat , lnGross
           ENDIF
           IF PACK_HDR.dShipDate<>{}
             IF ldFromDate={} AND ldToDate = {}
               ldFromDate = PACK_HDR.dShipDate
               ldToDate   = PACK_HDR.dShipDate
             ELSE
               IF PACK_HDR.dShipDate < ldFromDate
                 ldFromDate = PACK_HDR.dShipDate
               ENDIF
               IF PACK_HDR.dShipDate > ldToDate
                 ldToDate = PACK_HDR.dShipDate
               ENDIF                        
             ENDIF
           ENDIF
           lnNetVal = lnNetVal + (Consinvh.ShipAmt - Consinvh.Discount)
           lnVat    = lnVat    + ((Consinvh.Shipamt*Consinvh.Tax_rate)/100)
           lnGross  = lnVat + lnNetVal
          *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
          lcAlias = ALIAS()
          SELECT Consinvh 
          IF SEEK(Consinvh.INVOICE + Consinvh.Store,'INVCHRG') AND !SEEK(Consinvh.INVOICE + Consinvh.Store,lcSTmpChr)  
             SELECT INVCHRG
             SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE + Consinvh.Store 
               SELECT (lcSTmpChr)
               IF SEEK(Consinvh.Invoice + Consinvh.Store)
                 REPLACE NetVal  WITH NetVal + INVCHRG.nchrgamnt                 ,;
                         Vat     WITH Vat + IIF(INVCHRG.ntaxrate>0               ,;
                         (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;     
                         Gross   WITH  NetVal + Vat  
               ELSE
                APPEND BLANK 
                REPLACE Invoice   WITH INVCHRG.Invoice                                        ,;
                        Store     WITH Consinvh.Piktkt                                        ,;
                        Dpoint    WITH ALLTRIM(Consinvh.Store) + ' ' +ALLTRIM(CUSTOMER.STNAME),;
                        NetVal    WITH INVCHRG.nchrgamnt                                      ,;
                        Vat       WITH IIF(INVCHRG.ntaxrate>0                                 ,;
                                  (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)               ,;
                        Gross     WITH NetVal + Vat                                        
               ENDIF                                                        
            ENDSCAN
          ENDIF
          SELECT (lcAlias)                              
          SEEK(Consinvh.Invoice + Consinvh.Store)
          lnN=EVAL(lcSTmpChr+'.NetVal')
          lnV=EVAL(lcSTmpChr+'.Vat')
          lnG=EVAL(lcSTmpChr+'.Gross')
          *:C127805,1 MMR [End]
           SELECT (lcCoTmpFil)
          *:C127805,1 MMR 06/15/2005 Adding the Charges to the invoices [Start]
          *IF SEEK(Consinvh.Invoice + Consinvh.Store)
            *REPLACE NetVal  WITH lnNetVal               , ;
                    *Vat     WITH lnVat                  , ;
                    *Gross   WITH lnGross 
          *ELSE
            *APPEND BLANK
            *REPLACE Invoice WITH Consinvh.Invoice , ;
                    *Store   WITH Consinvh.Store   , ;
                    *Dpoint  WITH ALLTRIM(Consinvh.Store) + ' ' +ALLTRIM(CUSTOMER.STNAME) ,;
                    *NetVal  WITH lnNetVal              , ;
                    *Vat     WITH lnVat                 , ;
                    *Gross   WITH lnGross          
          *ENDIF
          IF SEEK(Consinvh.Invoice + Consinvh.Store)
            REPLACE NetVal  WITH lnNetVal + lnN          , ;
                    Vat     WITH lnVat + lnV             , ;
                    Gross   WITH lnGross + lnG 
          ELSE
            APPEND BLANK
            REPLACE Invoice WITH Consinvh.Invoice , ;
                    Store   WITH Consinvh.Store   , ;
                    Dpoint  WITH ALLTRIM(Consinvh.Store) + ' ' +ALLTRIM(CUSTOMER.STNAME) ,;
                    NetVal  WITH lnNetVal + lnN        , ;
                    Vat     WITH lnVat + lnV           , ;
                    Gross   WITH lnGross + lnG          
          ENDIF
         *:C127805,1 MMR [End] 
      ENDIF
    ENDSCAN
    SELECT (lcTmpDate)
    APPEND BLANK
    REPLACE INVOICE WITH INVHDR.INVOICE ,;
            FDATE   WITH ldFromDate     ,;
            TODATE  WITH ldToDate
  ENDIF
ENDSCAN
*--End function lfColDataC
*!*************************************************************
*! Name        : lfColDataN
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Collect data for form ARPINVDN
*!             : (Consolidated By D/N)
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataN()
*!*************************************************************
*!C123950,1
FUNCTION lfColDataN
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
*PRIVATE lcPiktkt , lnNetVal , lnVat , lnGross ,ldShpDate , lcCustPo
PRIVATE lcPiktkt , lnNetVal , lnVat , lnGross ,ldShpDate , lcCustPo,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcPiktkt , lcCustPo
STORE 0 TO lnNetVal , lnVat , lnGross
STORE {} TO ldShpDate

SELECT INVHDR
SCAN FOR &LCRPEXP 
  STORE 0 TO lnTotalVat,lnTotalGr
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE
      IF SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') AND;
        SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
        *NNA 07/02/2005 (Begin) seek about the Piktkt also inside the Pack_hdr file
        SELECT PACK_HDR
        *LOCATE FOR PACK_HDR.ORDER = Consinvh.order AND PACK_HDR.PIKTKT = Consinvh.piktkt
        *IF FOUND()
        *NNA (End)
          IF !(lcPiktkt == Consinvh.Piktkt)
            lcPiktkt = Consinvh.Piktkt
            STORE 0 TO lnNetVal , lnVat , lnGross
            STORE {} TO ldShpDate  
            STORE '' TO lcCustPo      
          ENDIF
          lnNetVal   = lnNetVal + (Consinvh.ShipAmt - Consinvh.Discount)
          lnVat      = lnVat    + ((Consinvh.Shipamt*Consinvh.Tax_rate)/100)
          lnGross    = lnVat + lnNetVal
          lnTotalVat = lnTotalVat + lnVat
          lnTotalGr  = lnTotalGr + lnGross
          *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
          lcAlias = ALIAS()
          SELECT Consinvh 
          IF SEEK(Consinvh.INVOICE,'INVCHRG') AND !SEEK(Consinvh.INVOICE,lcDnTmpChr)  
             SELECT INVCHRG
             SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE 
               SELECT (lcDnTmpChr)
               IF SEEK(Consinvh.Invoice + Consinvh.Piktkt)
                 REPLACE NetVal  WITH NetVal + INVCHRG.nchrgamnt                 ,;
                         Vat     WITH Vat + IIF(INVCHRG.ntaxrate>0               ,;
                         (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;     
                         Gross   WITH  NetVal + Vat  
               ELSE
                APPEND BLANK 
                REPLACE Invoice    WITH INVCHRG.Invoice                                    ,;
                        Piktkt     WITH Consinvh.Piktkt                                    ,;
                        NetVal     WITH INVCHRG.nchrgamnt                                  ,;
                        Vat        WITH IIF(INVCHRG.ntaxrate>0                             ,;
                                   (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                        Gross      WITH NetVal + Vat                                        
               ENDIF                                                        
            ENDSCAN
          ENDIF  
          SELECT (lcAlias)                              
          *:C127805,1 MMR [End]
          SELECT (lcDnTmpFil)
          IF SEEK(Consinvh.Invoice + Consinvh.Piktkt)
            REPLACE NetVal  WITH lnNetVal         , ;
                    Vat     WITH lnVat            , ;
                    Gross   WITH lnGross 
          ELSE
            APPEND BLANK
            REPLACE Invoice   WITH Consinvh.Invoice   ,;
                    Piktkt    WITH Consinvh.Piktkt    ,;
                    CustRef   WITH PIKTKT.CUSTPO      ,; 
                    dShipDate WITH PACK_HDR.dShipDate ,;
                    NetVal    WITH lnNetVal           ,;
                    Vat       WITH lnVat              ,;
                    Gross     WITH lnGross 
          ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF !SEEK(INVHDR.INVOICE,lcTotalTmp)
    *:C127805,1 MMR 06/15/2005 Collecting Total Vat and Gross[Start]
    lcAlias2=Alias()
    SELECT (lcDnTmpChr)
    SEEK INVHDR.INVOICE
    lnVatCh=   Vat
    lnGrossCh= Gross
    SELECT (lcAlias2)
    *INSERT INTO (lcTotalTmp)(INVOICE,GTotVat,GTotGross) VALUES(INVHDR.INVOICE,lnTotalVat,lnTotalGr)
    INSERT INTO (lcTotalTmp)(INVOICE,GTotVat,GTotGross)  VALUES(INVHDR.INVOICE,(lnTotalVat+lnVatCh),(lnTotalGr+lnGrossCh))
    *:C127805,1 MMR
  ENDIF
ENDSCAN
*--End function lfColDataN.

*!*************************************************************
*! Name        : lfSetRel_1
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Creat Relationship between files
*!             : before collecting data.
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel_1()
*!*************************************************************
*!C123950,1
FUNCTION lfSetRel_1
SELECT INVHDR
SET RELATION TO
SET RELATION TO INVOICE INTO Consinvh ADDITIVE
SELECT Consinvh
SET RELATION TO
SET RELATION TO Consinvh.order+ Consinvh.piktkt INTO Piktkt ADDITIVE
SET RELATION TO 'S'+ Consinvh.account+ Consinvh.store INTO Customer ADDITIVE
SELECT PIKTKT
SET RELATION TO
SET RELATION TO Piktkt.order INTO Pack_hdr ADDITIVE

*--End function lfSetRel_1.

*!*************************************************************
*! Name        : lfSetRel_2
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Creat Relationship between files
*!             : after collecting data.
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel_2()
*!*************************************************************
*!C123950,1
FUNCTION lfSetRel_2
IF llRpInvNot 
    SELECT INVHDR
    SET RELATION TO
    SET RELATION TO '' INTO (lcTmpDbt)
    SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                       'S' + Account + Store) INTO CUSTOMER ADDITIVE
    *--NNA
    IF lcRpFormN = 'N'
      SET RELATION TO INVOICE INTO (lcTotalTmp) ADDITIVE
    ENDIF
    *--NNA
    SELECT (lcTmpDbt)
    SET RELATION TO
    SET RELATION TO IIF(CFILE_NUM = '3', INVHDR.Invoice, '*') INTO ARINSTMD
    SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO ;
                IIF(lcRpFormN = 'C',(lcCoTmpFil),(lcDnTmpFil)) ADDITIVE
    SELECT INVHDR
    SET SKIP TO (lcTmpDbt) , IIF(lcRpFormN = 'C',(lcCoTmpFil),(lcDnTmpFil)) , ARINSTMD
ELSE
  SELECT INVHDR
  SET RELATION TO
  SET RELATION TO INVHDR.INVOICE INTO IIF(lcRpFormN = 'C',(lcCoTmpFil),(lcDnTmpFil)) ADDITIVE
  SET SKIP TO IIF(lcRpFormN = 'C',(lcCoTmpFil),(lcDnTmpFil))
  *--NNA
  IF lcRpFormN = 'N'
    SET RELATION TO INVOICE INTO (lcTotalTmp) ADDITIVE
    *:C127805,1 MMR 06/15/2005 Create Relation betwwen the two files to print in the FRX[Start]
    SET RELATION TO INVOICE+Piktkt INTO (lcDnTmpChr) ADDITIVE
    *:C127805,1 MMR 06/15/2005 [End]  
  ENDIF
  *--NNA
ENDIF
IF lcRpFormN = 'C'
  SELECT INVHDR
  SET RELATION TO INVOICE INTO (lcTmpDate) ADDITIVE
ENDIF
*--End function lfSetRel_2.
*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : Function to Evaluate Notes To be Printed 
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************
*!C123950,1
FUNCTION lfPrtNotes
PARAMETER lcReturn
DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2'                          ;
       .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))                                     ;
       .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')
    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : Function to Validate Changing of the Forms.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*!C123950,1
FUNCTION lfvRpForm 
PARAMETERS lcDummy
=lfRepPltFr(lcFormName)
DO CASE
  CASE lcRpFormN = 'S'
    lcFormName  = 'ARPINVM'
    lcOGPlatForm = IIF(lcRepAvlM $ 'BG' , 'WINDOWS ' , 'DOS')
    lcRepMode    = IIF(lcRepAvlM $ 'BG' , 'Graphics' , 'Text')
    lcRepModeS   = IIF(lcRepAvlM = 'B' , 'ENABLE' , 'DISABLE')
    SHOW GET lcRepMode &lcRepModeS
  CASE lcRpFormN = 'C'
    lcFormName  = 'ARPINVDR'
    lcOGPlatForm = 'WINDOWS ' 
    lcRepMode    = 'Graphics' 
    lcRepModeS   =  'DISABLE'
    SHOW GET lcRepMode &lcRepModeS
  CASE lcRpFormN = 'N'
    lcFormName  = 'ARPINVDN'
    lcOGPlatForm = 'WINDOWS ' 
    lcRepMode    = 'Graphics' 
    lcRepModeS   =  'DISABLE'
    SHOW GET lcRepMode &lcRepModeS
ENDCASE

lcDummy =.T. 
RETURN lcDummy
*--End of Function lfvRpForm

*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : RELEASE RELATIONSHIP AND FILES
*!*************************************************************
*! Called from : The PRG.
*!*************************************************************
*!C123950,1
FUNCTION lfRelease
DIMENSION laFiles[2]
STORE 'PIKTKT'   TO laFiles[1]
STORE 'PACK_HDR' TO laFiles[2]
FOR I = 1 TO 2
  IF USED(laFiles[I])
    SELECT &laFiles[I]
    SET RELATION TO
    USE IN &laFiles[I]
  ENDIF
ENDFOR
*--End of Function lfRelease