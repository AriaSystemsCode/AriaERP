*:*************************************************************
*: Name      : lfPOSTYPHK
*: Developer : Mariam Mazhar (MMT)
*: Date      : 07/14/2004
*: Purpose   : Call the POSTYHK.FRX form for GMA
*:*************************************************************
*: due to issue #038205
*:*************************************************************
llPoDisp = .F.
lcType    = IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
 lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
  IF lnPos <> 0 
    lcSelFld  = "[order],CUTPICK.trancd,CUTPICK.ctktno,CUTPICK.style "
    lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
    lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
*    lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'POSHDR',loOGScroll.laogFxflt[lnPos,6])
     *****************************************************
    IF EMPTY(lcOrders)
      lcTable   = "CUTPICK(Index = CUTPICK),PosHdr(Index = POSHDR)"
      lcType    = IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
      lcSelCond = "CUTPICK.TRANCD = '2' AND CUTPICK.CTKTNO = PosHdr.PO AND PosHdr.CBUSDOCU ='"+lcRpForm+"' AND POSHDR.CSTYTYPE = '"+lcType+"'"
    ELSE 
*!*	      llFirst = .T.
*!*	      lcSelOrders =""
*!*	      ***********************************************
  	  SELECT &lcOrders 
  	  LOCATE 
      IF !EOF()
        lcCurName = lcOrders 
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
            lcTable   = 'POSHDR(Index = POSHDR),CUTPICK(Index = CUTPICK) INNER JOIN '+lcSQLOrder +' ON CUTPICK.CTKTNO ='+lcSQLOrder+'.PO'
            lcSelCond = "TRANCD = '2'"+'  AND CUTPICK.CTKTNO ='+lcSQLOrder+'.PO'+"  AND CUTPICK.CTKTNO = PosHdr.PO AND PosHdr.CBUSDOCU ='"+lcRpForm+"' AND POSHDR.CSTYTYPE = '"+lcType+"'"
          ENDIF 
        ENDIF 
      ***********************************************
*!*		  SELECT &lcOrders 
*!*		  IF !EOF()
*!*	  	    SCAN 
*!*		      lcSelOrders = lcSelOrders + IIF(llFirst," ",",") + "'"+PO+"'"
*!*		      llFirst = .F.
*!*	    	ENDSCAN 
*!*	        lcTable   = "CUTPICK"
*!*	        lcSelCond = " TRANCD = '2' AND CTKTNO IN ("+lcSelOrders+") "
      ELSE 
        lcTable   = "CUTPICK(Index = CUTPICK),PosHdr(Index = POSHDR)"
        lcSelCond = " CUTPICK.TRANCD = '2' AND CUTPICK.CTKTNO = PosHdr.PO AND PosHdr.CBUSDOCU ='"+lcRpForm+"' AND POSHDR.CSTYTYPE = '"+lcType+"'"      
      ENDIF 
    ENDIF 
    IF lfSqlOpen(lcSelFld,lcTable,'CUTPICK',lcSelCond)
      SELECT CUTPICK
	  SET RELATION TO
	  LOCATE
    ENDIF 
    *****************************************************
*!*	    SELECT &lcOrders 
*!*	    SCAN 
*!*	      lcSelOrd = lcSelOrd + IIF(llFirst," ",",") + "'"+PO+"'"
*!*	      llFirst = .F.
*!*	    ENDSCAN 
*!*	    
*!*	    IF lfSqlOpen(lcSelFld,lcTable,'CUTPICK',lcSelCond)
*!*	      SELECT CUTPICK
*!*	      SET RELATION TO
*!*	      LOCATE
*!*	    ENDIF 
   ENDIF 

SELECT POSHDR
LOCATE 
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

IF USED('CUTPICK')
  USE IN ('CUTPICK')
ENDIF
*:*************************************************************
*: Name      : lfBRTSO  lfBRTSO
*: Developer : Mariam Mazhar (MMT)
*: Date      : 07/14/2004
*: Purpose   : GET BERYTOS SALES ORDER #
*:*************************************************************
*: Called from : POSTYPHK.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : lcReturn
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfUsrVldFn('lfBRTSO','POSTYPHK')
*:*************************************************************
FUNCTION lfBRTSO
PARAMETERS lcReturn
*PRIVATE lcBerytoSo
*STORE "" TO lcBerytoSo
*lcAlias = ALIAS()

*SELECT CUTPICK 
IF SEEK('2'+POSHDR.PO,'CUTPICK')
	lcReturn = CUTPICK.ORDER
ENDIF

*lcReturn = lcBerytoSo
*SELECT &lcAlias

*:*************************************************************
*: Name      : lfGmaStypo
*: Developer : Mariam Mazhar (MMT)
*: Date      : 07/14/2004
*: Purpose   : To delete * form Style and PO notes.
*:*************************************************************
*: Called from : POSTYPHK.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : lcReturn
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfUsrVldFn('lfGmaStyPO','POSTYPHK')
*:*************************************************************
*:
FUNCTION lfgmastypo
PARAMETERS lcreturn
PRIVATE lclstnote
STORE '' TO lclstnote

IF !EMPTY(lcnotes)
	lnnotline = 1
	lnoldmemw = SET("MEMOWIDTH")

    SET MEMOWIDTH TO 100


	lnmemlins = MEMLINES(lcnotes)
	DIMENSION lanot[lnMemLins]
	DO WHILE lnnotline <= lnmemlins
		IF  '*' <> LEFT(MLINE(lcnotes,lnnotline),1)
			lanot[lnNotLine]= MLINE(lcnotes,lnnotline)
		ELSE
			lanot[lnNotLine] = ''
		ENDIF
		lnnotline = lnnotline + 1
	ENDDO
	FOR i=1 TO   lnmemlins
		IF !EMPTY(lanot[I])
			IF i <> lnmemlins
				lclstnote = lclstnote + lanot[i]+ CHR(13)
			ELSE
				lclstnote = lclstnote + lanot[i]
			ENDIF
		ENDIF
	ENDFOR
	lcreturn = lclstnote
ENDIF

*!*************************************************************
*! Name      : lfSqlOpen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 06/30/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfSqlOpen

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment 
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfTabindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag)&& OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfSqlOpen.

*!*************************************************************
*! Name      : lfTabindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 06/30/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfTabindex

LPARAMETERS lcTable
DO CASE
  CASE UPPER(lcTable) = "CUTPICK"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'trancd+ctktno+style'
    laIndex[1,2] = 'CUTPICK'
ENDCASE


