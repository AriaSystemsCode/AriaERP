*!***************************************************************************
*! Program file        : SORDCHNG.PRG   
*! Program desc.       : Custom Order/Sales Net Change report [THE18 - THE NAT NAST]
*! For Report          : SORDCHNG.FRX
*! System              : Aria Advantage Series VER. 2.7
*! Module              : Sales Oredr - (SO)
*! Developer           : NADER NABIL (NNA)
*! Tracking Job Number : C200978 [T20080214.0015]
*! Date                : 04/13/2008
*!****************************************************************************
*! Calls :  Functions  : lfvDateRng , lfOpnFiles , lfCrtTemp , lfGetProd , lfGetSales
*!                     : lfVToDate
*!****************************************************************************
*! Called From         : System Menu (SO --> Output --> Sales Order Change Report)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             : DO SORDCHNG
*!****************************************************************************
*! Modification        :
*!
*!****************************************************************************
STORE '' TO lcCompSt,lcCompEd,lcStartSt,lcStartEd,lcEditSt,lcEditEd
lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,ATC('|',laOGFxFlt[lnCompPos,6])-1)))
lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],ATC('|',laOGFxFlt[lnCompPos,6])+1)))

lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,ATC('|',laOGFxFlt[lnStartPos,6])-1)))
lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],ATC('|',laOGFxFlt[lnStartPos,6])+1)))

ldEditSt  = CTOD(SUBSTR(laOGFxFlt[lnEditPos,6],1,ATC('|',laOGFxFlt[lnEditPos,6])-1))
ldEditEd  = CTOD(SUBSTR(laOGFxFlt[lnEditPos,6],ATC('|',laOGFxFlt[lnEditPos,6])+1))

lcEditSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnEditPos,6],1,ATC('|',laOGFxFlt[lnEditPos,6])-1)))
lcEditEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnEditPos,6],ATC('|',laOGFxFlt[lnEditPos,6])+1)))
SET CENTURY ON 
IF EMPTY(lcEditSt) OR EMPTY(lcEditEd)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Sorry , you have to input a change date range')  
  RETURN .F.
ENDIF
IF 'STYLE.STYLE' $ UPPER(lcRpExp)
  lcRpExp = STRTRAN(lcRpexp ,'STYLE.STYLE','STYLE.CSTYMAJOR')
ENDIF
= lfOpnFiles(.T.,"INVLINE,CONSINVL,INVHDR","INVLINEO,CINVLINE,INVHDR")
= lfCrtTemp()
= lfSetRela()
= lfchngFltr()
= lfGetData()
SELECT (lcOrdTemp)
loOGScroll.cCROrientation='L'
=gfDispRe()

*--> Close Files that I opened
= lfOpnFiles(.F.,"INVLINE,CONSINVL,INVHDR")
RETURN
*!****************************************************************************
*!* Name        : lfGetData
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/013/2008
*!* Purpose     : Create the Temp file
*!****************************************************************************
*!* Called from : SORDCHNG.PRG
*!****************************************************************************
*!* Parameters  : None
*!****************************************************************************
*!* Return      : None
*!****************************************************************************
*!* Example     : = lfGetData()
*!****************************************************************************
FUNCTION lfGetData
DIMENSION laPicked(1) ,laShipped(2)
STORE 0 TO laPicked,laShipped
STORE .F. TO llKNOW_EDT   && to know orders that has a reason for edit and unknown orders
STORE '' TO lcOrdType , lcOrder
SELECT ORDHDR
SCAN FOR BETWEEN(DTOS(ORDHDR.Dedit_Date) , lcEditSt , lcEditEd) AND &lcRpexp
  STORE 0 TO laPicked,laShipped
  lcOrdType = ORDHDR.CORDTYPE
  lcOrder   = ORDHDR.ORDER
  llKNOW_EDT = .F.
  SCATTER MEMVAR MEMO FIELDS LIKE ;
          Order,Account,Entered,Start,Complete,BOOK,BOOKAMT,OPEN,OPENAMT,SHIP,SHIPAMT,CANCEL,CANCELAMT,cCancReson,PICKED
  m.Acc_Name   = CUSTOMER.BTNAME
  m.Can_Reason =''

  *--> Search for Picked Orders . <--*
  IF SEEK('O'+ORDHDR.ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE CORDTYPE+ORDER = lcOrdType + lcOrder
      IF BETWEEN(DTOS(ORDLINE.PIKDATE),lcEditSt , lcEditEd)
        laPicked[1] = laPicked[1] + ORDLINE.TOTPIK
      ENDIF
    ENDSCAN
    m.PICKED = laPicked[1]
    IF m.PICKED>0
      m.Sec_Seq  = 2
      SELECT (lcOrdTemp)
      APPEND BLANK
      GATHER MEMVAR MEMO     
      llKNOW_EDT = .T.
    ENDIF  
  ENDIF 

  *-->Search for New Orders. <--*
  IF BETWEEN(DTOS(ORDHDR.ENTERED),lcEditSt , lcEditEd) AND INLIST(ORDHDR.STATUS,'H','O')
     m.Sec_Seq  = 1
     SELECT (lcOrdTemp)
     APPEND BLANK
     GATHER MEMVAR MEMO     
     llKNOW_EDT = .T.     
  ENDIF             

  *-->Search for Cancelled Orders. <--*  
  IF BETWEEN(DTOS(ORDHDR.Cancelled),lcEditSt , lcEditEd) AND ORDHDR.STATUS='X'
    IF !EMPTY(ORDHDR.cCancReson) AND SEEK('N'+ORDHDR.cCancReson+'N'+'CCANCRESON','CODES')
      m.Can_Reason=CODES.CDiscRep
    ENDIF     
     m.Sec_Seq  = 3
     SELECT (lcOrdTemp)
     APPEND BLANK
     GATHER MEMVAR MEMO     
     llKNOW_EDT = .T.
  ENDIF             
  *-->Search for Shipped Orders. <--*  
  IF NOT ISNULL(M.SHIP) AND m.SHIP>0 
    IF ORDHDR.CONSOL<>'Y'
      SELECT INVLINE
      SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= ORDHDR.ORDER FOR INVHDR.STATUS<>'V'
        IF BETWEEN(DTOS(INVLINE.INVDATE),lcEditSt , lcEditEd)
          laShipped[1] = laShipped[1] + INVLINE.TOTQTY
          laShipped[2] = laShipped[2] + (INVLINE.TOTQTY * INVLINE.PRICE)
        ENDIF
      ENDSCAN
      m.Ship    = laShipped[1]
      m.ShipAmt = laShipped[2]
      m.Sec_Seq = 4
      IF NOT ISNULL(M.SHIP) AND m.SHIP>0 
        SELECT (lcOrdTemp)
        APPEND BLANK
        GATHER MEMVAR MEMO     
        llKNOW_EDT = .T.
      ENDIF
    ENDIF  
  ENDIF
  *-->Search for Unknown Edited Orders.<--*  
  IF !llKNOW_EDT AND BETWEEN(DTOS(ORDHDR.Dedit_Date) , lcEditSt , lcEditEd)
     m.Sec_Seq  = 5
     SELECT (lcOrdTemp)
     APPEND BLANK
     GATHER MEMVAR MEMO     
  ENDIF
ENDSCAN
*-->End of Function lfGetData.
*!****************************************************************************
*!* Name        : lfCrtTemp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/013/2008
*!* Purpose     : Create the Temp file
*!****************************************************************************
*!* Called from : SORDCHNG.PRG
*!****************************************************************************
*!* Parameters  : None
*!****************************************************************************
*!* Return      : None
*!****************************************************************************
*!* Example     : = lfCrtTemp()
*!****************************************************************************
FUNCTION lfCrtTemp
CREATE TABLE (gcWorkDir+lcOrdTemp)(Order c(6),Account c(5),Acc_Name C(30),Entered D(8),Start D(8),Complete D(8),;
                                  BOOK N(8),BOOKAMT N(14,2),OPEN N(8),OPENAMT N(14,2),SHIP N(8),SHIPAMT N(14,2),;
                                  CANCEL N(8),CANCELAMT N(14,2),PICKED N(6),cCancreson C(6),Can_Reason C(30),Sec_Seq N(1))
INDEX On ALLTRIM(STR(Sec_Seq))+ cCancreson + Order TAG (lcOrdTemp)
*-->End of FUNCTION lfCrtTemp.
*!****************************************************************************
*! Name      : lfSetRela
*! Developer : Nader Nabil (NNA)
*! Date      : 04/013/2008
*! Purpose   : Set relationship between tables 
*!****************************************************************************
*! Called from : SordChng.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfSetRela()
*!****************************************************************************
FUNCTION lfSetRela
SELECT ORDHDR
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO ORDLINE ADDITIVE
SET RELATION TO 'M'+Account INTO CUSTOMER ADDITIVE
SET RELATION TO Ordhdr.order INTO INVLINE ADDITIVE
SET RELATION TO Ordhdr.order INTO CONSINVL ADDITIVE

SELECT ORDLINE
SET RELATION TO ORDLINE.Style INTO STYLE ADDITIVE

SELECT INVLINE
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
*-->End of Function lfSetRela.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
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
PRIVATE lcOldStat,lcCurrChr
lcOldStat = lcRpStatus  && Save old status value.
= lfogmover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- end of lfvOStatus.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
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
DECLARE laRpSource[5]
  IF TYPE('laRpTarget[1]') = 'C'
    IF EMPTY(laRpTarget[1])
      DECLARE laRpTarget[1]
    ELSE
      FOR lnI = 2 TO ALEN(laRpTarget)
        IF TYPE('laRpTarget[lnI]') = 'U'
          laRpTarget[lnI] = ""
        ENDIF
      ENDFOR    
    ENDIF
  ELSE
    DECLARE laRpTarget[1]
  ENDIF
  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'

  *-- MAB 05/02/99 Work sheet bug By default all status was selected.. [End  ]

  SET ORDER TO ORDHDR IN ORDHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.

   lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
   lnStyPos   = lfItmPos('STYLE.STYLE')
   lnOrdPos   = lfItmPos('ORDHDR.ORDER')
   lnStartPos = lfItmPos('ORDHDR.START')
   lnCompPos  = lfItmPos('ORDHDR.COMPLETE')
   lnEditPos  = lfItmPos('ORDHDR.DEDIT_DATE')
   lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
   lnSeaPos   = lfItmPos('ORDHDR.SEASON')
   lnDivPos   = lfItmPos('ORDHDR.CDIVISION')
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
*--The Style length
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))
*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSrvSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.
*!*************************************************************
*! Name      : lfSROrder
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- end of lfsChOrder.
*!*************************************************************
*! Name      : lfStySum
*! Developer : NADER NABIL (NNA)
*! Date      : 04/013/2008
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0
IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(LEFT(Style,lnLenth)) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : NNA - NADER NABIL ABD-ALMONAM
*! Date      : 04/013/2008
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : lcItmInFlt
*!*************************************************************
*! Returns            : Position
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
*--> End of Function lfItmPos.
*!****************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/13/2008
*!* Purpose     : Open the needed files
*!****************************************************************************
*!* Called from : SALPRDSC.PRG
*!****************************************************************************
*!* Parameters  : llOpen     --> True for open files , False for closing files
*!*             : lcFilesExp --> Files that need to open
*!*             : lcTages    --> Tags for the opened files.
*!****************************************************************************
*!* Return      : None
*!****************************************************************************
*!* Example     : = lfOpnFiles()
*!****************************************************************************
FUNCTION lfOpnFiles
PARAMETERS llOpen,lcFilesExp,lcTages
DIMENSION laFiles[1],laTages[1]

=GFSUBSTR(lcFilesExp,@laFiles,',')
IF llOpen
  =GFSUBSTR(lcTages,@laTages,',')
  FOR I=1 TO ALEN(laFiles,1)
    IF USED(laFiles[I])
       USE IN laFiles[I]
    ENDIF
    =gfOpenFile(gcDataDir+laFiles[I],laTages[I],'SH')
  ENDFOR
ELSE
  FOR I=1 TO ALEN(laFiles,1)
    IF USED(laFiles[I])
      USE IN laFiles[I]
    ENDIF
  ENDFOR
ENDIF
*-- End of Function lfOpnFiles.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : NNA - NADER NABIL ABD-ALMONAM
*! Date      : 04/13/2008
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : NNA - NADER NABIL ABD-ALMONAM
*! Date      : 04/13/2008
*! Purpose   : -Return the selected status in the ReadBox
*!             -Due to E127836,1 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
*-- end of RefreshStatus.
*!*************************************************************
*! Name      : lfchngFltr
*! Developer : NNA - NADER NABIL ABD-ALMONAM
*! Date      : 04/13/2008
*! Purpose   : Chnage mover filter from inlist to cursor for 
*!           : style group , Season , Devision 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
FUNCTION lfchngFltr
*Change Style Group Filter
llUseGrp  = .F.
lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
IF lnGrpPos  > 0 
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnGrpPos ,6]),loOgScroll.laOgFXFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel) 
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
    IF llUseGrp 
      lnGrpStart = AT('INLIST(STYLE.CSTYGROUP',lcRpexp)
      IF lnGrpStart > 0
         lnEndPos = AT(")",SUBSTR(lcRpexp,lnGrpStart))+lnGrpStart-1
         lnNumChar = lnEndPos -lnGrpStart+1
         lcRpexp = STUFF(lcRpexp,lnGrpStart,lnNumChar,"Seek(STYLE.CSTYGROUP,'&lcGrpFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   
*--> Change SEASON filter
llUseSeason  = .F.
lnSeaPos   = lfItmPos('ORDHDR.SEASON')
IF lnSeaPos > 0 
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnSeaPos,6]),loOgScroll.laOgFXFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
    IF llUseSeason 
      lnSEAStart = AT('INLIST(STYLE.SEASON',lcRpExp)
      IF lnSEAStart > 0
         lnEndPos = AT(")",SUBSTR(lcRpExp,lnSEAStart))+lnSEAStart-1
         lnNumChar = lnEndPos -lnSEAStart+1
         lcRpExp = STUFF(lcRpExp,lnSEAStart,lnNumChar,"Seek(STYLE.SEASON,'&lcSeaFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   

*--> Change DIVISION filter
llUseDiv  = .F.
lnDivPos   = lfItmPos('ORDHDR.CDIVISION')
IF lnDivPos > 0 
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnDivPos,6]),loOgScroll.laOgFXFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel) 
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
    IF llUseDiv  
      lnDivStart = AT('INLIST(STYLE.CDIVISION',lcRpExp)
      IF lnDivStart > 0
         lnEndPos = AT(")",SUBSTR(lcRpExp,lnDivStart))+lnDivStart-1
         lnNumChar = lnEndPos -lnDivStart+1
         lcRpExp = STUFF(lcRpExp,lnDivStart,lnNumChar,"Seek(STYLE.CDIVISION,'&lcDivFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   
*--> End of Function lfchngFltr.
*!*************************************************************
*! Name      : lfConvertToCursor
*! Developer : NNA - NADER NABIL ABD-ALMONAM
*! Date      : 04/13/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'ROYALTY'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR2'
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
*--> End of FUNCTION lfConvertToCursor.

