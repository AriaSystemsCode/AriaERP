*:**********************************************************************************************
*: Program file  : ICIMPIN.PRG 
*: Program desc. : Download Inventory From Excel
*: System        : Aria 4 XP
*: Module        : IC
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 02/10/2008
*: Refer to      : C200950 [T20070608.0020]
*:**********************************************************************************************
*Modifications:
*B608527,1 MMT 04/21/2008 Change File to be Imported structure{T20080327.0013}
*B608527,2 MMT 07/10/2008 Fix Error if line has empty line    {T20080327.0013}
*B608527,3 MMT 07/21/2008 Fix Error if line has empty line    [T20080327.0013]
*B608527,4 MMT 07/29/2008 Get Cost Value From style file      [T20080711.0016]
*:**********************************************************************************************
lcExpr = gfOpGrid('ICIMPINV' , .T.)
RETURN  

*!**************************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/24/2008
*! Purpose   : Create Temp File
*!**************************************************************************
FUNCTION lfCrtTemp

DIMENSION laFileStruct[11,4]

laFileStruct[1,1] =  'Style'
laFileStruct[1,2] =  'C'
laFileStruct[1,3] =  19 
laFileStruct[1,4] =  0


laFileStruct[2,1] =  'Qty1'
laFileStruct[2,2] =  'N'
laFileStruct[2,3] =  5 
laFileStruct[2,4] =  0

laFileStruct[3,1] =  'Qty2'
laFileStruct[3,2] =  'N'
laFileStruct[3,3] =  5 
laFileStruct[3,4] =  0

laFileStruct[4,1] =  'Qty3'
laFileStruct[4,2] =  'N'
laFileStruct[4,3] =  5 
laFileStruct[4,4] =  0

laFileStruct[5,1] =  'Qty4'
laFileStruct[5,2] =  'N'
laFileStruct[5,3] =  5 
laFileStruct[5,4] =  0

laFileStruct[6,1] =  'Qty5'
laFileStruct[6,2] =  'N'
laFileStruct[6,3] =  5 
laFileStruct[6,4] =  0

laFileStruct[7,1] =  'Qty6'
laFileStruct[7,2] =  'N'
laFileStruct[7,3] =  5 
laFileStruct[7,4] =  0

laFileStruct[8,1] =  'Qty7'
laFileStruct[8,2] =  'N'
laFileStruct[8,3] =  5 
laFileStruct[8,4] =  0

laFileStruct[9,1] =  'Qty8'
laFileStruct[9,2] =  'N'
laFileStruct[9,3] =  5 
laFileStruct[9,4] =  0

laFileStruct[10,1] = 'Scale'
laFileStruct[10,2] = 'C'
laFileStruct[10,3] = 3
laFileStruct[10,4] = 0

laFileStruct[11,1] = 'Cost'
laFileStruct[11,2] = 'N'
laFileStruct[11,3] = 10
laFileStruct[11,4] = 3

=gfCrtTmp(lcTempFile ,@laFileStruct,'Style',lcTempFile)

DIMENSION laFileStr[2,4]
laFileStr[1,1] = 'Style'
laFileStr[1,2] = 'C'
laFileStr[1,3] = 19
laFileStr[1,4] = 0

laFileStr[2,1] = 'Reason'
laFileStr[2,2] = 'C'
laFileStr[2,3] = 50
laFileStr[2,4] = 0

=gfCrtTmp(lcErrFile  ,@laFileStr,'Style',lcErrFile)

*!**************************************************************************
*! Name      : lfCrtBatFls
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/24/2008
*! Purpose   : Create Temp Batch Files
*!**************************************************************************
FUNCTION lfCrtBatFls

SELECT MDINVNTL
=AFIELDS(laTpFlds)
DIMENSION laIndexArr[2,2]

laIndexArr[1,1] = [cbattype+cLkBatch+style+color+dyelot+clocation]
laIndexArr[1,2] = lcBatInd

laIndexArr[2,1] = [style+color+dyelot+clocation+cbattype+cLkBatch]
laIndexArr[2,2] = lcStyInd

=gfCrtTmp(lcBatLin,@laTpFlds,@laIndexArr,lcBatLin)

SELECT MDINVNTH
=AFIELDS(laTpFlds)
lnTpFlds = ALEN(laTpFlds,1)
DIMENSION  laTpFlds[lnTpFlds+1,18]
laTpFlds[lnTpFlds+1,1] = 'nSteps'
laTpFlds[lnTpFlds+1,2] = 'N'
laTpFlds[lnTpFlds+1,3] = 2
laTpFlds[lnTpFlds+1,4] = 0

  STORE ' ' TO  laTpFlds[lnTpFlds+1,7],laTpFlds[lnTpFlds+1,8],;
                laTpFlds[lnTpFlds+1,9],laTpFlds[lnTpFlds+1,10],;
                laTpFlds[lnTpFlds+1,11],laTpFlds[lnTpFlds+1,12],;
                laTpFlds[lnTpFlds+1,13],laTpFlds[lnTpFlds+1,14],;
                laTpFlds[lnTpFlds+1,15],laTpFlds[lnTpFlds+1,16]
  STORE 0 TO    laTpFlds[lnTpFlds+1,17] ,laTpFlds[lnTpFlds+1,18]

=gfCrtTmp(lcBatHdr,@laTpFlds,[cbattype+cLkBatch],lcBatHdr)
*!**************************************************************************
*! Name      : lfvEPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/10/2008
*! Purpose   : Brwose for Error log Save in location
*!**************************************************************************
FUNCTION lfvEPath
IF ALLTRIM(lcRpEPath)  = "?"
 lcRpEPath= GETDIR()
ENDIF

FUNCTION lfvFile
IF "?" $ ALLTRIM(lcRpFile)  
 lcRpFile= GETFILE("XLS")
ENDIF

FUNCTION lfGetCrt
IF EMPTY(lcRpFile)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"You must Select the File to be Imported")
  RETURN .F.
ELSE
  IF !FILE(lcRpFile)
     =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File to be Imported")
     RETURN .F.
  ENDIF 
ENDIF 

IF EMPTY(lcRpEPath) OR (!EMPTY(lcRpEPath) AND !DIRECTORY(lcRpEPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Error Log Location")
  RETURN .F.
ENDIF 



=gfOpenTable(oAriaApplication.DataDir+'MDINVNTL',oAriaApplication.DataDir+'MDINVNTL','SH')
=gfOpenTable(oAriaApplication.DataDir+'MDINVNTH',oAriaApplication.DataDir+'MDINVNTH','SH')
=gfOpenTable(oAriaApplication.DataDir+'GLDIST','','SH')
=gfOpenTable(oAriaApplication.DataDir+'WareHous',oAriaApplication.DataDir+'WareHous','SH')
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')
=gfOpenTable(oAriaApplication.DataDir+'Style' ,'Style','SH')
=gfOpenTable(oAriaApplication.DataDir+'StyDye' ,'StyDye','SH')
=gfOpenTable(oAriaApplication.DataDir+'StyInvJl' ,'StyInvJl','SH')



lcFileName= JUSTSTEM(lcRpFile)
lnMajLen  = LEN(gfItemMask("PM"))
lcSep     =  ''
STORE  0 TO lnClrLen ,lnClrPos,lnScalLen,lnScPos

DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lcSep =   ALLT(laItemSeg[lnCount,6])
  ENDIF 
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
  ENDIF
  IF laItemSeg[lnCount,1]='S'
    lnScalLen  = LEN(laItemSeg[lnCount,3])
    lnScPos    = laItemSeg[lnCount,4]
  ENDIF 
ENDFOR

lcTempFile = gfTempName()
lcErrFile  = gfTempName()


lfCrtTemp()

llScale = .F.

IMPORT from (lcRpFile) TYPE XLS    
lcAlias = SELECT(0)


*B608527,2 MMT 07/10/2008 Fix Error if line has empty line[Start]
*SCAN 
SCAN FOR !EMPTY(A)
*B608527,2 MMT 07/10/2008 Fix Error if line has empty line[End]

  *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
  *IF UPPER(A) = "BOX"
  IF UPPER(A) = "STYLE"
	*!*	    m.Siz1 =  H
	*!*	    m.Siz2 = I
	*!*	    m.Siz3 = J
	*!*	    m.Siz4 = K
	*!*	    m.Siz5 = L
	*!*	    m.Siz6 = M
	*!*	    m.Siz7 = N
	*!*	    m.Siz8 = O                       
	*!*	    m.Siz9 = P
	*!*	    m.Siz10 = Q
	*!*	    m.Siz11 = R
	*!*	    m.Siz12 = S
	*!*	    m.Siz13 = T
	*!*	    m.Siz14 = U
	*!*	    m.Siz15 = V
	*!*	    m.Siz16 = W                       
    
    m.Siz1 =  IIF(TYPE('C')<>'U',C,'')
    m.Siz2  = IIF(TYPE('D')<>'U',D,'') 
    m.Siz3  = IIF(TYPE('E')<>'U',E,'')
    m.Siz4  = IIF(TYPE('F')<>'U',F,'')
    m.Siz5  = IIF(TYPE('G')<>'U',G,'')
    m.Siz6  = IIF(TYPE('H')<>'U',H,'')
    m.Siz7  = IIF(TYPE('I')<>'U',I,'')
    m.Siz8  = IIF(TYPE('J')<>'U',J,'')                       
    m.Siz9  = IIF(TYPE('K')<>'U',K,'')
    m.Siz10 = IIF(TYPE('L')<>'U',L,'')
    m.Siz11 = IIF(TYPE('M')<>'U',M,'')
    m.Siz12 = IIF(TYPE('N')<>'U',N,'')
    m.Siz13 = IIF(TYPE('O')<>'U',O,'')
    m.Siz14 = IIF(TYPE('P')<>'U',P,'')
    m.Siz15 = IIF(TYPE('Q')<>'U',Q,'')                       
    m.Siz16 = IIF(TYPE('R')<>'U',R,'')                       
    m.Siz17 = IIF(TYPE('S')<>'U',S,'')
    m.Siz18 = IIF(TYPE('T')<>'U',T,'')
    m.Siz19 = IIF(TYPE('U')<>'U',U,'')                       
    *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
    LOOP  
  ENDIF 
  lnLevl = 0
  
  *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
  *FOR lnI = 1 TO 16
  FOR lnI = 1 TO 19
  *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
     
    lcI = ALLTRIM(STR(lnI))
    
    
    *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
    *!*	    lcFld = lfGetFldName(lnI)
    *B608527,3 MMT 07/21/2008 Fix Error if line has empty line[Start]
    
    *IF EMPTY(m.Siz&lcI) 
    IF TYPE('m.Siz&lcI') ='U' OR EMPTY(m.Siz&lcI) 
    *B608527,3 MMT 07/21/2008 Fix Error if line has empty line[End]
    
      LOOP 
    ENDIF 
    *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
    
    llScaleFound = .F.
    
    *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
	*!*	    lnFlDValue = IIF(!EMPTY(&lcFld) and TYPE('&lcFld') <> 'N',VAL(&lcFld),IIF(!EMPTY(&lcFld) and TYPE('&lcFld') = 'N',&lcFld,0))
	*B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
	
   * IF lnFlDValue <> 0
	  SELECT Scale 
	  gfSeek('S')
	  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'
	    IF TYPE('m.Siz&lcI') <> 'C'
  	      m.Siz&lcI = STR(m.Siz&lcI,5)
	    ENDIF 
        IF ALLTRIM(scale.SZ1) == ALLTRIM(m.Siz&lcI)
          m.Scale = Scale.Scale
	      m.ScCnt = Scale.cnt
	      llScale = .T.
          llScaleFound = .T.
          lnLevl = lnLevl +1
          EXIT 
	    ENDIF 
	  ENDSCAN 
	  SELECT (lcAlias)
	  IF llScaleFound 
	  
	    *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
	    *m.Style = PADR(B,lnMajLen)+lcSep+PADR(D,lnClrLen )+lcClrSpr++PADR(m.Scale,lnScalLen)
        m.Style = PADR(A,lnMajLen)+lcSep+PADR(B,lnClrLen )+lcClrSpr++PADR(m.Scale,lnScalLen)
        *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
        
        lnCount = lnI
        m.TotQty = 0
        m.Qty1 = 0
        m.Qty2 = 0
        m.Qty3 = 0
        m.Qty4 = 0
        m.Qty5 = 0
        m.Qty6 = 0
        m.Qty7 = 0
        m.Qty8 = 0        
        FOR lnC = 1 TO m.ScCnt
          lcC = STR(lnC,1)
          lcFldName =lfGetFldName(lnCount)
          
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
          IF TYPE('&lcFldName')='U'
            m.Qty&lcC = 0
          ELSE
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
          
            m.Qty&lcC = &lcFldName 
          
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
          ENDIF   
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
          
          IF TYPE('m.Qty&lcC') <> 'N'
            m.Qty&lcC = VAL(m.Qty&lcC)
          ENDIF 
 	      m.TotQty = m.TotQty + m.Qty&lcC
          lnCount = lnCount + 1
          
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
          *IF lnCount > 16
          IF lnCount > 19
          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
          
            EXIT 
          ENDIF 
        ENDFOR 
        
        *B608527,4 MMT 07/29/2008 Get Cost Value From style file[Start]
		*!*	        IF lnLevl > 0
		*!*	        
		*!*	          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
		*!*	          *m.Cost = IIF(lnLevl = 1,Y,IIF(lnLevl = 2,Z,IIF(lnLevl = 3,AA,AB)))
		*!*	          m.Cost = IIF(lnLevl = 1,W,IIF(lnLevl = 2,X,IIF(lnLevl = 3,Y,IIF(lnLevl = 4,Z,AA))))
		*!*	          *B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]
		*!*	          
		*!*	        ENDIF 
        IF gfSeek(m.Style,'Style')
          m.Cost = Style.Ave_cost
        ELSE
          m.Cost = 0
        ENDIF 
        *B608527,4 MMT 07/29/2008 Get Cost Value From style file[End]
        
        IF TYPE('m.Cost') <> 'N'
          m.Cost = VAL(m.Cost)
        ENDIF 
        IF m.TotQty  <> 0
          INSERT INTO (lcTempFile) FROM MEMVAR 
        ENDIF   
        lnI = lnI + m.ScCnt - 1
      ENDIF 
  ENDFOR 
ENDSCAN 

IF !llScale
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Size scale codes in imported sheet do not match Aria scales")
  RETURN .F.
ENDIF  

SELECT (lcTempFile)
SCAN 
  IF !gfSeek(Style,'Style')
    IF !gfSeek(PADR(SUBSTR(Style,1,lnMajLen),19),'Style','Cstyle')
      SELECT (lcErrFile)
      APPEND BLANK 
      REPLACE Style WITH &lcTempFile..style,;
      		  reason WITH 'Style number does not exist in master files'	
  	ELSE
      SELECT (lcErrFile)
      APPEND BLANK 
      REPLACE Style WITH &lcTempFile..style,;
      		  reason WITH 'Color number does not exist in Aria'	
  	      		  
    ENDIF 
    SELECT (lcTempFile)
    DELETE 
  ELSE
    IF !lfCheckSty(&lcTempFile..style)  
      SELECT (lcErrFile)
      APPEND BLANK 
      REPLACE Style WITH &lcTempFile..style,;
      		  reason WITH "Style Locked Before"	
      		  
      SELECT (lcTempFile)
      DELETE 
    ENDIF 
  ENDIF 
ENDSCAN 


SELECT (lcErrFile)
LOCATE 
IF !EOF()
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,"Some lines in the Excel sheet are rejected. Do you want to import accepted lines only?") = 1
    SELECT (lcTempFile)  
    LOCATE FOR !DELETED()
    IF !EOF()
      lcBatLin = gfTempName()
	  lcBatHdr = gfTempName()
	  lcBatInd = gfTempName()
	  lcStyInd = gfTempName()
	  lfCrtBatFls()
	  *Call Locking procedure 
	  lfLocking()   
   ENDIF 
  ENDIF 
  lcFileExp = ALLTRIM(lcRpEPath) + ALLTRIM(lcFileName)+"_log" +".txt"
  lnOutFile = FCREATE(lcFileExp ,0)
  IF lnOutFile < 0
	=gfModalGen('TRM00000B00000','ALERT','','','Cannot Create Log file. Cannot proceed.')
    RETURN .F.
  ENDIF
  *lcFileName lcRpEPath
  SELECT (lcErrFile)
  SCAN
    lcStrPik  = &lcErrFile..Style +":"+ &lcErrFile..Reason
    FPUTS(lnOutFile ,lcStrPik)			 
  ENDSCAN 
  FCLOSE(lnOutFile )
ELSE
  SELECT (lcTempFile)  
  LOCATE FOR !DELETED()
  IF !EOF()
    lcBatLin = gfTempName()
    lcBatHdr = gfTempName()
    lcBatInd = gfTempName()
    lcStyInd = gfTempName()
    lfCrtBatFls()
    *Call Locking procedure 
    lfLocking()   
  ENDIF 
ENDIF 





*!**************************************************************************
*! Name      : lfGetFldName
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/24/2008
*! Purpose   : get the corresponding field name from the excel file
*!**************************************************************************
FUNCTION lfGetFldName
PARAMETERS lnSz
lcFldName = ''
DO CASE 
*B608527,1 MMT 04/21/2008 Change File to be Imported structure[Start]
*!*	CASE  lnSz = 1
*!*	  lcFldName  = 'H'
*!*	CASE  lnSz = 2  
*!*	  lcFldName  = 'I'
*!*	CASE  lnSz = 3
*!*	  lcFldName  = 'J'
*!*	CASE  lnSz = 4
*!*	  lcFldName  = 'K'
*!*	CASE  lnSz = 5
*!*	  lcFldName  = 'L'
*!*	CASE  lnSz = 6
*!*	  lcFldName  = 'M'
*!*	CASE  lnSz = 7
*!*	  lcFldName  = 'N'
*!*	CASE  lnSz = 8
*!*	  lcFldName  = 'O'
*!*	CASE  lnSz = 9
*!*	  lcFldName  = 'P'
*!*	CASE  lnSz = 10
*!*	  lcFldName  = 'Q'
*!*	CASE  lnSz = 11
*!*	  lcFldName  = 'R'
*!*	CASE  lnSz = 12
*!*	  lcFldName  = 'S'
*!*	CASE  lnSz = 13
*!*	  lcFldName  = 'T'
*!*	CASE  lnSz = 14
*!*	  lcFldName  = 'U'
*!*	CASE  lnSz = 15
*!*	  lcFldName  = 'V'
*!*	CASE  lnSz = 16  
*!*	  lcFldName  = 'W'

CASE  lnSz = 1
  lcFldName  = 'C'
CASE  lnSz = 2  
  lcFldName  = 'D'
CASE  lnSz = 3
  lcFldName  = 'E'
CASE  lnSz = 4
  lcFldName  = 'F'
CASE  lnSz = 5
  lcFldName  = 'G'
CASE  lnSz = 6
  lcFldName  = 'H'
CASE  lnSz = 7
  lcFldName  = 'I'
CASE  lnSz = 8
  lcFldName  = 'J'
CASE  lnSz = 9
  lcFldName  = 'K'
CASE  lnSz = 10
  lcFldName  = 'L'
CASE  lnSz = 11
  lcFldName  = 'M'
CASE  lnSz = 12
  lcFldName  = 'N'
CASE  lnSz = 13
  lcFldName  = 'O'
CASE  lnSz = 14
  lcFldName  = 'P'
CASE  lnSz = 15
  lcFldName  = 'Q'
CASE  lnSz = 16  
  lcFldName  = 'R'
CASE  lnSz = 17
  lcFldName  = 'S'
CASE  lnSz = 18  
  lcFldName  = 'T'
CASE  lnSz = 19  
  lcFldName  = 'U'
*B608527,1 MMT 04/21/2008 Change File to be Imported structure[End]

ENDCASE  
RETURN   lcFldName
*!**************************************************************************
*! Name      : lfLocking
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/24/2008
*! Purpose   : locking process
*!**************************************************************************
FUNCTION lfLocking
lcWare = WareHous.cwarecode
lcCostMeth = gfGetMemVar('M_COST_MET')

SELECT (lcTempFile)
SCAN FOR !DELETED()
  SELECT STYDYE
  IF gfSEEK(&lcTempFile..Style + lcWare , 'STYDYE')
    SCAN REST WHILE STYLE + CWARECODE + DYELOT = &lcTempFile..Style + lcWare
       SCATTER MEMVAR MEMO
       STORE 0 TO lnTotStk
       gfSeek(&lcTempFile..Style ,'Style','Style')
       FOR lnLop = 1 To 8
         lcLoop = STR(lnLop,1)
         lcStk  = 'STK' + ALLTRIM(STR(lnLop))
         m.&lcStk = &lcTempFile..QTY&lcLoop  + IIF(m.Stk&lcLoop  > 0 ,m.Stk&lcLoop,0)
         lnTotStk = lnTotStk +  m.&lcStk
       ENDFOR
        m.TotStk = lnTotStk
        m.StyDesc = Style.Desc1
        DO CASE
          CASE  lcCostMeth = 'S'  && Standard
            m.Cost    = STYLE.TotCost
            m.oCost   = STYLE.TotCost
          OTHERWISE       && 'A' Average
            m.Cost    = Ave_Cost
            m.oCost   = Ave_Cost
        ENDCASE
        m.Cost    = &lcTempFile..Cost
        m.ITEM    = ''
        m.Style   =&lcTempFile..Style 
        m.STOCK   = lnTotStk 
        m.oSTOCK  = TotStk
        m.nStkVal = nStkVal
        m.Cadd_user = oAriaApplication.User_Name
        m.Dadd_date = oAriaApplication.SystemDate
        m.Cadd_time = gfGetTime()
        SELECT MDINVNTL
        gfSetOrder('MDINVNTLS')
        INSERT INTO (lcBatLin) FROM MEMVAR
        SELECT (lcBatLin)
        Replace Dyelot   WITH StyDye.Dyelot   ,;
                cBatType WITH 'S'             ,;
                Scale    WITH Style.Scale     ,;
                OldCost  WITH StyDye.Ave_Cost ,;
                OldStk1  WITH StyDye.Stk1     ,;
                OldStk2  WITH StyDye.Stk2     ,;
                OldStk3  WITH StyDye.Stk3     ,;
                OldStk4  WITH StyDye.Stk4     ,;
                OldStk5  WITH StyDye.Stk5     ,;
                OldStk6  WITH StyDye.Stk6     ,;
                OldStk7  WITH StyDye.Stk7     ,;
                OldStk8  WITH StyDye.Stk8     ,;
                OldTotStk WITH StyDye.TotStk
    ENDSCAN 
  ENDIF 
ENDSCAN 

lfTmpBtchL()

*!*************************************************************
*! Name      : lfSaveFile
*! Developer : Mariam Mazhar
*! Date      : 10/23/2006
*! Purpose   : function to save files
*!*************************************************************
FUNCTION lfSaveFile
*-- Call default save
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == SET("Datasession")
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF

*!*************************************************************
*! Name      : lfTmpBtchL
*! Developer : Mariam Mazhar [MMT]
*! Date      : 11/19/2006
*! Purpose   : Function to Update mdwn line and hewder.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTmpBtchL()
*!*************************************************************
*!
FUNCTION lfTmpBtchL

lcKey = 'S'
m.Date      = oAriaApplication.SystemDate
m.cWareCode = WareHous.cwarecode
m.Type      = "M"
m.cBatType  = lcKey
m.content   = ''
lcBatchNo = gfsequence('cLkBatch')
m.cLkBatch  = lcBatchNo 
INSERT INTO (lcBatHdr) FROM MEMVAR
SELECT (lcBatLin)
REPLACE ALL cLkBatch WITH lcBatchNo,cBatType WITH lcKey

SELECT (lcBatHdr)
LOCATE 
SCATTER MEMVAR MEMO

IF nSteps < 1
  SELECT MDINVNTH
  lcOldFlt = SET("Filter")
  SET FILTER TO 
  LOCATE FOR EMPTY(mdinvnth.cbattype) AND EMPTY(mdinvnth.cLkBatch)
  IF FOUND()
    GATHER MEMVAR MEMO
    = gfAdd_Info('MDINVNTH')
  ELSE
    APPEND BLANK 
    GATHER MEMVAR MEMO
    = gfAdd_Info('MDINVNTH')
  ENDIF 
  gfReplace()
  SET FILTER TO &lcOldFlt
  SELECT (lcBatHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF

SELECT (lcBatLin)

SCAN
  SCATTER MEMVAR MEMO
  m.Cadd_user = oAriaApplication.User_Name
  m.Dadd_date = oAriaApplication.SystemDate
  m.Cadd_time = gfGetTime()
  IF nSteps < 1
     m.cAdjReason = m.cAdjReason
    INSERT INTO MDINVNTL FROM MEMVAR
    SELECT MDINVNTL 
    gfReplace()
    =lfLkStyInv(m.Style)
    SELECT (lcBatLin)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
ENDSCAN
lfSaveFile()
=gfModalGen('TRM42180B00000','DIALOG',lcBatchNo)

*!*************************************************************
*! Name      : lfLkStyInv
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/24/2008
*! Purpose   : Function to Update mdwn line and hewder.
*!*************************************************************
FUNCTION lfLkStyInv
PARAMETERS lcStyle
SELECT StyDye
=gfSEEK(lcStyle)
gfREPLACE([dLlokDate WITH oAriaApplication.SystemDate])

SELECT STYINVJL
gfSetorder('STYINVJL')
=gfSEEK(lcStyle)
SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = lcStyle
  gfREPLACE([lLockFlg WITH .T.])
ENDSCAN
SELECT MDINVNTL
*!*************************************************************
*! Name      : lfCheckSty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/24/2008
*! Purpose   : Check if style locked before.
*!*************************************************************
FUNCTION lfCheckSty

PARAMETER lcCSty

PRIVATE lcOldAls , lcOldInd
lcKey = 'S'
lcOldAls = SELECT(0)
SELECT MDINVNTL
gfSetOrder('MDINVNTLS')
=gfSEEK(lcCSty)
SCAN REST WHILE style+color+cbattype+cLkBatch = lcCSty

 IF gfSEEK(lcKey+MDINVNTL.cLkBatch,'MDINVNTH') AND MDINVNTH.Type $ 'HML'
    RETURN(.F.)
  ENDIF
ENDSCAN
SELECT (lcOldAls)

