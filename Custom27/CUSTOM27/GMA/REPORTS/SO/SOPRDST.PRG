*:***************************************************************************
*: Program file  : Soprdst.PRG
*: Program desc. : Custom Production Status Report for GMA
*: Date          : 03/04/2005
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Nader Nabil Abd Elmonem (NNA)
*: Tracking NO.  : C124806
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Example : DO ARPINVDIR
*:**************************************************************************
*:Modification:
*:**************************************************************************
PRIVATE lcCustRel,laSizes
STORE '' TO lcCustRel
DECLARE laSizes[8]
STORE '' TO laSizes
*--cut the Style.CstyGroup Part from the lcrpExp
lcStyGroup = " .T. "
lnStyPos = ATC("INLIST(STYLE.CSTYGROUP",lcRpExp)
IF lnStyPos <> 0
  llLast = (ATC('AND',SUBSTR(lcRpExp,lnStyPos)) =0)
  lcStyGroup = SUBSTR(lcRpExp,lnStyPos,IIF(llLast,LEN(lcRpExp),ATC('AND',SUBSTR(lcRpExp,lnStyPos))-1))
  lcRpExp = STRTRAN(lcRpExp,lcStyGroup,".T.")
ENDIF

=lfCretTemp()	&& Create Temp File
=lfSetRelat()	&& Set Relation Between Files

=lfCollData()   && Collecting data
=lfExprt2xl()   && Export Report to Excel Sheet

*--End Of Report
*!*************************************************************
*! Name      : lfCretTemp
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Create Temp File.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfCretTemp()
*!*************************************************************
FUNCTION lfCretTemp
CREATE TABLE (gcWorkDir + lcTmpFile)(Order C(6),LineNo N(6),dEntered D(8),StoreN C(30),Rep C(3),;
       StorePO C(15),OrdCateg C(30),Ctktno1 C(10),Est_Date1 D(8),Rec_Date D(8),Client C(30),;
       Ctktno2 C(10),StyleG C(6),StyleClr C(19),Siz1 C(5),Siz2 C(5),Siz3 C(5),Siz4 C(5),;
       Siz5 C(5),Siz6 C(5),Siz7 C(5),Siz8 C(5),Est_Date2 D(8),In_Date D(8),Due_Date D(8),;
       Complete D(8),Event_Date D(8),Inv_Date D(8),AR_Inv C(8),Ship_Date D(8),Ups_Track C(30),;
       Bill_Date D(8),Ap_Inv C(12),DJ_Inv C(12),Notes M(1))
INDEX ON (Order+Ctktno1+STR(LineNo,6)) TAG (lcTmpFile) 
*--End of Function lfCretTemp.
*!*************************************************************
*! Name      : lfSetRelat
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Create RelationShip Between The report files
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfSetRelat()
*!*************************************************************
FUNCTION lfSetRelat

SELECT ORDHDR
SET RELATION TO
SET RELATION TO Ordhdr.Cordtype+ Ordhdr.Order INTO Ordline ADDITIVE
SET RELATION TO 'B'+ Ordhdr.Order INTO Notepad ADDITIVE

SELECT ORDLINE
SET RELATION TO
SET RELATION TO '1'+ Ordline.Order+STR(Ordline.Lineno,6) INTO Cutpick ADDITIVE
SET RELATION TO style INTO Style ADDITIVE
SET RELATION TO 'S'+Scale INTO Scale ADDITIVE
SET RELATION TO Ordline.Order+ STR(Ordline.Lineno,6) INTO Invline ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store) INTO CUSTOMER ADDITIVE  && To customer file.

SELECT CUTPICK
SET RELATION TO
SET RELATION TO 'M'+ Cutpick.ctktno INTO Mfgoprhd ADDITIVE
SET RELATION TO 'M'+ Cutpick.ctktno INTO Apvinvdt ADDITIVE
SET RELATION TO Cutpick.ctktno INTO Cuttkth ADDITIVE

SELECT Mfgoprhd
SET RELATION TO
SET RELATION TO Mfgoprhd.Cimtyp+ Mfgoprhd.Ctktno+ Mfgoprhd.Coprcode INTO Mfgoprdt ADDITIVE

SELECT Apvinvdt
SET RELATION TO Apvinvdt.capinvno INTO Apinvhdr ADDITIVE

SELECT INVLINE
SET RELATION TO
SET RELATION TO Invline.Invoice INTO Invhdr ADDITIVE
SET RELATION TO Invline.invoice INTO Shdaimpr ADDITIVE

*--End of Function lfSetRelat.
*!*************************************************************
*! Name      : lfCollData
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Collecting Data.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcOrderNo , ldEntered , lcRep1   , lcNote1  , lcOrdCatg , ldEvent,lcNotePad;
        ldEstDate1 , ldEstDate2 , ldRecDate1 , ldRecDate2 , lcApinv1 , lcApinv2
STORE {} TO ldEstDate1 , ldEstDate2 , ldRecDate1 , ldRecDate2        
STORE '' TO lcOrderNo , lcRep1 , lcNote1 , lcOrdCatg , lcApinv1 , lcApinv2 , lcNotePad
SELECT ORDHDR
SCAN FOR &lcRpExp AND STATUS $ lcRpStatus
  WAIT WINDOW NOWAIT 'Collecting data for order : ' + OrdHdr.Order
  lcOrderNo = OrdHdr.Order
  ldEntered = OrdHdr.Entered
  lcRep1    = OrdHdr.Rep1
  lcNote1   = OrdHdr.Note1
  ldEvent   = OrdHdr.dEvent_Dat
  lcNotePad = ''
  IF llRpordnot AND SEEK('B'+OrdHdr.Order,'Notepad')
    SELECT NOTEPAD
    FOR M = 1 TO MEMLINES(MNOTES)
        lcNotePad = lcNotePad + IIF(!EMPTY(lcNotePad) AND MEMLINES(MNOTES)>1,',','') + MLINE(MNOTES,M)
    ENDFOR
    lcNotePad = SUBSTR(lcNotePad,1,60)
  ENDIF
  lcOrdCatg = ALLTRIM(gfCodDes(OrdHdr.CorderCat,'CORDERCAT'))
  SELECT ORDLINE
  SCAN REST WHILE Cordtype+Order+STR(Lineno,6) = OrdHdr.Cordtype+OrdHdr.Order
    IF !&lcStyGroup
      LOOP
    ENDIF
    IF SEEK("1"+Ordline.order+STR(Ordline.lineno,6),'CUTPICK') 
      SELECT CutPick
      STORE '' TO lcApinv1 , lcApinv2
      SCAN REST WHILE Trancd+Order+Cordline = "1"+Ordline.order+STR(Ordline.lineno,6) FOR ;
        CutTkth.Status $ lcStatus
        STORE {} TO ldEstDate1 , ldEstDate2 , ldRecDate1 , ldRecDate2
        INSERT INTO (lcTmpFile)(Order,LineNo,dEntered,StoreN,Rep,StorePO,OrdCateg,Ctktno1,;
                Client,Ctktno2,StyleG,StyleClr,Siz1,Siz2,Siz3,Siz4,Siz5,Siz6,Siz7,Siz8,Due_Date,;
                Complete,Event_Date,Bill_Date,Notes) ;
        VALUES (lcOrderNo,Ordline.LineNo,ldEntered,Customer.Stname,lcRep1,OrdLine.CustPo,;
                lcOrdCatg,CutPick.CtktNo+' - 1',lcNote1,CutPick.CtktNo+' - 2',Style.cStyGroup,OrdLine.Style,;
                IIF(OrdLine.Qty1>0,Scale.SZ1,''),IIF(OrdLine.Qty2>0,Scale.SZ2,''),;
                IIF(OrdLine.Qty3>0,Scale.SZ3,''),IIF(OrdLine.Qty4>0,Scale.SZ4,''),;
                IIF(OrdLine.Qty5>0,Scale.SZ5,''),IIF(OrdLine.Qty6>0,Scale.SZ6,''),;
                IIF(OrdLine.Qty7>0,Scale.SZ7,''),IIF(OrdLine.Qty8>0,Scale.SZ8,''),;
                OrdLine.Start,OrdLine.Complete,ldEvent,Apinvhdr.Dinvdate,lcNotePad)
        IF SEEK('M'+CutPick.Ctktno,'Mfgoprhd')
          SELECT Mfgoprhd
          SCAN REST WHILE Cimtyp+Ctktno+Coperseq = 'M' + CutPick.Ctktno
            SELECT Mfgoprdt
            SCAN REST WHILE Cimtyp+Ctktno+Coprcode =;
                Mfgoprhd.cimtyp+Mfgoprhd.ctktno+Mfgoprhd.coprcode FOR Item = CutPick.Style
                IF Mfgoprhd.Coperseq = '01' AND Mfgoprdt.TranCD = '1'
                ldEstDate1 = Mfgoprdt.DtranDate
                IF SEEK('M'+CutPick.Ctktno,'Apvinvdt')
                  SELECT Apvinvdt
                  SCAN REST WHILE cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = 'M'+CutPick.Ctktno ;
                                  FOR Apvinvdt.Coprcode  = Mfgoprhd.Coprcode ;
                                  AND Apvinvdt.Item = CutPick.Style
                    lcApinv1 = Apvinvdt.CapinvNo 
                  ENDSCAN
                ENDIF
              ENDIF
              IF Mfgoprhd.Coperseq = '01' AND Mfgoprdt.TranCD = '2'
                ldRecDate1 = Mfgoprdt.DtranDate
              ENDIF
              IF Mfgoprhd.Coperseq = '02' AND Mfgoprdt.TranCD = '1'
                ldEstDate2 = Mfgoprdt.DtranDate
                IF SEEK('M'+CutPick.Ctktno,'Apvinvdt')
                  SELECT Apvinvdt
                  SCAN REST WHILE cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+;
                                  item+color+cdyelot = 'M'+CutPick.Ctktno FOR ;
                                  Apvinvdt.Coprcode  = Mfgoprhd.Coprcode AND  ;
                                  Apvinvdt.Item      = CutPick.Style
                    lcApinv2 = Apvinvdt.CapinvNo 
                  ENDSCAN
                ENDIF
              ENDIF
              IF Mfgoprhd.Coperseq = '02' AND Mfgoprdt.TranCD = '2'
                ldRecDate2 = Mfgoprdt.DtranDate
              ENDIF
              SELECT(lcTmpFile)
              REPLACE Est_Date1   WITH ldEstDate1	,;
                      Rec_Date    WITH ldRecDate1	,;
                      Est_Date2   WITH ldEstDate2	,;
                      In_Date     WITH ldRecDate2	,;
                      Ap_Inv      WITH lcApinv1     ,;
                      DJ_Inv      WITH lcApinv2
            ENDSCAN
          ENDSCAN
        ENDIF
      ENDSCAN
    ELSE
      IF lcRpStatus = 'X'
        INSERT INTO (lcTmpFile)(Order,LineNo,dEntered,StoreN,Rep,StorePO,OrdCateg,;
                Client,StyleG,StyleClr,Siz1,Siz2,Siz3,Siz4,Siz5,Siz6,Siz7,Siz8,Due_Date,;
                Complete,Event_Date,Notes) ;
        VALUES (lcOrderNo,Ordline.LineNo,ldEntered,Customer.Stname,lcRep1,OrdLine.CustPo,;
                lcOrdCatg,lcNote1,Style.cStyGroup,OrdLine.Style,;
                IIF(OrdLine.Qty1>0,Scale.SZ1,''),IIF(OrdLine.Qty2>0,Scale.SZ2,''),;
                IIF(OrdLine.Qty3>0,Scale.SZ3,''),IIF(OrdLine.Qty4>0,Scale.SZ4,''),;
                IIF(OrdLine.Qty5>0,Scale.SZ5,''),IIF(OrdLine.Qty6>0,Scale.SZ6,''),;
                IIF(OrdLine.Qty7>0,Scale.SZ7,''),IIF(OrdLine.Qty8>0,Scale.SZ8,''),;
                OrdLine.Start,OrdLine.Complete,ldEvent,lcNotePad)
      ENDIF
    ENDIF
    IF SEEK(Ordline.Order+ STR(Ordline.Lineno,6),'INVLINE')
      SELECT INVLINE
      SCAN REST WHILE order+STR(lineno,6)+invoice = Ordline.Order+ STR(Ordline.Lineno,6) FOR ;
                Invhdr.Status <>'V'
        SELECT(lcTmpFile)
        REPLACE Inv_Date 	WITH Invhdr.InvDate		,;
        		AR_Inv      WITH Invhdr.Invoice 	,;
                Ship_Date   WITH Invhdr.ShipDate    ,;
                Ups_Track   WITH Shdaimpr.Cpktrknum
        EXIT
      ENDSCAN
    ENDIF
    FOR S=1 TO 8
      LCS = ALLTRIM(STR(S))
      IF !EMPTY(&lcTmpFile..SIZ&LCS)
        STORE EVAL(lcTmpFile+'.SIZ&LCS') TO laSizes[S]
      ENDIF
    ENDFOR
  ENDSCAN
ENDSCAN
*--End of Function lfCollData.
*!*************************************************************
*! Name      : lfExprt2xl
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Export the Temp File to An Excel Sheet.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfExprt2xl()
*!*************************************************************
FUNCTION lfExprt2xl
SELECT(lcTmpFile)
LOCATE
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF
WAIT WINDOW NOWAIT 'Please wait...'
Dimension laHdrs[1]
lcStr = "Date,Store Name,Rep,Store PO#,T.S.,DA/F/B,Est.Date,Rec.Date,Client,Cut Ticket,Style Group,"
lcStr = lcStr + "Style-Color,Size1,Size2,Size3,Size4,Size5,Size6,Size7,Size8,Est. Date,In Date,Due Date,"
lcStr = lcStr + "Complete,Event Date,Invoice Date,Invoice#,Ship Date,Ups Tracking#,Billing Date,"
lcStr = lcStr + "VE/F Invoice,DJ Invoice,Notes/Changes"
= gfSubStr(lcStr,@laHdrs,',')         
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

IF USED(lcTmpFile)
  USE IN &lcTmpFile
ENDIF

lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'SO\'+[SOPRD2XL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
WAIT CLEAR

*--End of Function lfExprt2xl.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PARAMETER lcCallFrom
PRIVATE lcOldStat,lcCurrChr
IF lcCallFrom = 'O'
  lcOldStat = lcRpStatus  && Save old status value.
  = gfMover(@laRpOrdSrc,@laRpOrdTrg,'Select Order Status',.T.,'')  && call mover function.
  lcRpStatus = ' '
  *-- Loop to make Status expression.
  IF !EMPTY(laRpOrdTrg[1])
    FOR lnI = 1 TO ALEN(laRpOrdTrg,1)
      lcRpStatus = lcRpStatus + IIF(laRpOrdTrg[lnI] = 'Open','O',;
                                IIF(laRpOrdTrg[lnI] = 'Hold','H',;
                                IIF(laRpOrdTrg[lnI] = 'Complete','C',;                                                            
                                IIF(laRpOrdTrg[lnI] = 'Canceled','X',''))))
    ENDFOR  
  ENDIF
  lcRpStatus = IIF(EMPTY(lcRpStatus),'OHCX',ALLTRIM(lcRpStatus))
ELSE
  = gfMover(@laRpCutSrc,@laRpCutTrg,'Select Cutting Ticket Status',.T.,'')  && call mover function.
  lcStatus = ' '
  *-- Loop to make Status expression.
  IF !EMPTY(laRpCutTrg[1])
    FOR lnI = 1 TO ALEN(laRpCutTrg,1)
      lcStatus = lcStatus + IIF(laRpCutTrg[lnI] = 'Open'    ,'O',;
                            IIF(laRpCutTrg[lnI] = 'Hold'    ,'H',;
                            IIF(laRpCutTrg[lnI] = 'Complete','C',;
                            IIF(laRpCutTrg[lnI] = 'Closed'  ,'S',''))))
    ENDFOR  && end Loop to make Status expression.
  ENDIF
  *-- if empty of status is like select all available values.
  lcStatus = IIF(EMPTY(lcStatus),'OHCS',ALLTRIM(lcStatus))
ENDIF
*-- End of Function lfvOStatus.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
DECLARE laRpOrdSrc[4],laRpCutSrc[4],laRpOrdTrg[1],laRpCutTrg[1]
STORE 'Open'     TO laRpOrdSrc[1]
STORE 'Hold'     TO laRpOrdSrc[2]
STORE 'Complete' TO laRpOrdSrc[3]
STORE 'Canceled' TO laRpOrdSrc[4]
lcRpStatus = 'OHCX'

STORE 'Open'     TO laRpCutSrc[1]
STORE 'Hold'     TO laRpCutSrc[2]
STORE 'Complete' TO laRpCutSrc[3]
STORE 'Closed'   TO laRpCutSrc[4]
lcStatus = 'OHCS'
SHOW GET pbRun DISABLE
*!*************************************************************
*! Name      : lfSetFunc
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Go top in customer and Ordhdr file
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSetFunc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfSetFunc
PARAMETERS lcParm
IF lcParm = 'S'
  GO TOP IN CUSTOMER
  GO TOP IN ORDHDR  
ENDIF
*-- End of lfSetFunc.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : NADER NABIL (NNA)
*! Date      : 03/03/2005
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns   : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.
