*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah (WLD)
*: Date      : 11/11/2008
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200921,1 WLD 01/28/2008 Add general fields to hold Vertical Orientation of UPC Price tkt RT1& RT2
*C201067,1 WLD 11/11/2008 BCF requirement to receive unique UCC128 cross over the years
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfUniqUCC
*! Developer : Walid Hamed (WLD)
*! Date      : 01/27/2008
*! Purpose   : Custom of handling BCF requirement to receive unique UCC128 cross over the years
*:****************************************************************************************
*! Called from : Class EDISH.DO and SendUCCLabels.Do
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfUniqUCC

 XBOX_SER   = '000'+'09' + Right(MMVEND,5)+lcUCC9

ENDFUNC

*:****************************************************************************************
*! Name      : lfChgMnfID
*! Developer : Walid Hamed (WLD)
*! Date      : 01/27/2008
*! Purpose   : Custom of handling BCF requirement to receive unique UCC128 cross over the years - Change Manf. ID
*:****************************************************************************************
*! Called from : Class SendUCCLabels.Do
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfChgMnfID

  REPLACE MANUF_ID  WITH '09' + Right(MMVEND,5)
  
ENDFUNC

*:****************************************************************************************
*! Name      : lfIsFuLblDt
*! Developer : Rasha (RAS)
*! Date      : 05/15/2011
*! Purpose   : Custom of handling BCF requirement to show more than 22 style-color-size on label"function check if we should view full detail label or just first 22 style-size-color"
*:****************************************************************************************
*! Called from : Class prnlabel.proccessDetailedLabel
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : Logic
*:****************************************************************************************
FUNCTION lfIsFuLblDt
 IF(INLIST(lcDetailVer,'XCO'))
   return .T.
 ELSE 
   return .F.
 ENDIF
ENDFUNC
*:****************************************************************************************
*! Name      : lfGetFuLblDt
*! Developer : Rasha (RAS)
*! Date      : 05/15/2011
*! Purpose   : Custom of handling BCF requirement to show more than 22 style-color-size on label
*:****************************************************************************************
*! Called from : Class prnlabel.proccessDetailedLabel
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : Logic
*:****************************************************************************************
FUNCTION lfGetFuLblDt
set step on
IF(INLIST(lcDetailVer,'XCO'))
 IF !EMPTY(ALLT(laLblInfo[linCount,3]))
     S=ALLT(STR(laLblInfo[linCount,6]))
     IF laLblInfo[linCount,6]<=4 AND EMPTY(CopSize&S)
       CopSize&S=ALLT(laLblInfo[linCount,3])
     ENDIF
	 lBCFCount  = ALLTRIM(STR(lBCFNCount , 2))
	 lcCount = ALLTRIM(STR(linCount , 2))
	   IF LLastStyle <> laLblInfo[linCount,4] 
	      MDSTYLE&lBCFCount  = laLblInfo[linCount,4]
	      MDQTY&lBCFCount    = laLblInfo[linCount,2]
	      IF (laLblInfo[linCount,6]=1)
	       MDQTYSTR&lBCFCount = ALLTRIM(STR(laLblInfo[linCount,2]))
	      ELSE
	        IF (laLblInfo[linCount,6]=2)
	           MDQTYSTR&lBCFCount = ';'+ALLTRIM(STR(laLblInfo[linCount,2]))
	        ELSE
	           IF (laLblInfo[linCount,6]=3)
	              MDQTYSTR&lBCFCount = ';;'+ALLTRIM(STR(laLblInfo[linCount,2]))
	           ELSE
	             IF (laLblInfo[linCount,6])
	               MDQTYSTR&lBCFCount = ';;;'+ALLTRIM(STR(laLblInfo[linCount,2]))
	             ENDIF 
	           ENDIF
	        ENDIF
	      ENDIF  
	      MDSIZDES&lBCFCount = laLblInfo[linCount,3]
	 
	      MDPCK&lBCFCount    = laLblInfo[linCount,4]
	      IF ALEN(laLblInfo,2) => 8
	        INPCKQTY&lBCFCount = laLblInfo[linCount,8]
	      ENDIF
	      MDSTYMAJ&lBCFCount  = LEFT(laLblInfo[linCount,4] , lnMajorLen)
	      IF llUseColor
	        MDCOLOR&lBCFCount = SUBSTR(laLblInfo[linCount,4] , lnColorPos , lnColorLen)
	      ENDIF
	     IF EMPTY(laLblInfo[linCount,3]) && no size
	       MDSKU&lBCFCount = laLblInfo[linCount,4]
	     ENDIF
	     MDSTYUPC&lBCFCount  = laLblInfo[linCount, 5]
	     IF ALEN(laLblInfo, 2) => 7
	       MDPCS&lBCFCount  = laLblInfo[linCount, 7]
	       MDPQTY&lBCFCount   = laLblInfo[linCount,7]
	     ENDIF
	     lcCartons = IIF(llCOPCalled,COPPrnASN,'ASN_SHIP')
	     IF linCount = 1
	      ShortSKU = LFREAD(7,ALLTRIM(&lcCartons..CUSTPO)+MDSKU1,'Detail','850','PO1')
	      LongSku  = LFREAD(9,ALLTRIM(&lcCartons..CUSTPO)+MDSKU1,'Detail','850','PO1')
	      ITEMDESC = LFREAD(2,ALLTRIM(&lcCartons..CUSTPO),'Header','850','PO1')
	     ELSE
	      ShortSKU = IIF(ShortSKU = LFREAD(7,ALLTRIM(&lcCartons..CUSTPO)+MDSKU&lBCFCount,'Detail','850','PO1'),ShortSKU,"PREPACK")
	      LongSku  = IIF(LongSku  = LFREAD(9,ALLTRIM(&lcCartons..CUSTPO)+MDSKU&lBCFCount,'Detail','850','PO1'),LongSku ,LEFT(LongSku,LEN(LongSku)-3) +"***")
	    ENDIF
	    LLastStyle=MDSTYLE&lBCFCount
	    lBCFNCount=lBCFNCount+1
	   ELSE
	      lPrevBCFCount=ALLTRIM(STR(lBCFNCount-1 , 2))
	      IF laLblInfo[linCount,6]=3 AND LLastSize=1
	       MDQTYSTR&lPrevBCFCount =MDQTYSTR&lPrevBCFCount+';;'+ALLTRIM(STR(laLblInfo[linCount,2]))
	      ELSE 
	        IF laLblInfo[linCount,6]=4 AND LLastSize=1
	          MDQTYSTR&lPrevBCFCount =MDQTYSTR&lPrevBCFCount+';;;'+ALLTRIM(STR(laLblInfo[linCount,2]))      
	        ELSE
	         IF laLblInfo[linCount,6]=4 AND LLastSize=2
	           MDQTYSTR&lPrevBCFCount =MDQTYSTR&lPrevBCFCount+';;'+ALLTRIM(STR(laLblInfo[linCount,2]))
	         ELSE 
	           IF laLblInfo[linCount,6]<=4
	             MDQTYSTR&lPrevBCFCount =MDQTYSTR&lPrevBCFCount+';'+ALLTRIM(STR(laLblInfo[linCount,2]))
	           ENDIF
	         ENDIF
	        ENDIF  
	     ENDIF
	   ENDIF
	   LLastSize=laLblInfo[linCount,6]
	   I = ALLTRIM(STR(lBCFNCount-1))
	   MDQTY&I = MDQTYSTR&I
	   
	ELSE
	  I='0' 
	ENDIF
  return .T.
ELSE
  return .F.
ENDIF  
ENDFUNC