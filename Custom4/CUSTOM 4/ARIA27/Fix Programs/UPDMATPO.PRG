*:************************************************************************
*: Program file  : UPDMATPO.PRG
*: Program desc. : FIX ONHAND & ONORDER IN FABRIC AND FABDYE .
*: For screen    : NONE
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MA
*:      Developer: Walid Abou El-Magd (WAM)
*:           Date: 05/13/1999               
*:************************************************************************
*: Passed Parameters : none
*: Due to B802244,1
*:************************************************************************
* Run alone from Dotwin.	
*:************************************************************************

PARAMETER lcDataDir

IF TYPE('lcDataDir') <> 'C'
  lcDataDir = GETDIR('','Select data directory')
ENDIF

IF EMPTY(lcDataDir)
  RETURN
ENDIF  

STORE .F. TO llOpenMvj, llOpenPofh, llOpenPofl, llOpenFabD, llOpenFab

IF lfOpenFile()
  = lfReplace()
  =lfFixOrder()          &&-- function to fix ONORDER field .
  =lfFixOnHand()         &&-- function to fix onhand field .
ELSE
  WAIT WINDOW "Cannot open required file(s). Unable to proceed." NOWAIT
  RETURN
ENDIF

=lfClosFile()

*!*************************************************************
*! Name      : lfFixOrder
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : Function to calculate (+) or (-) onorder field due to 
*!           : pofln file only .
*!           : 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFixOrder()
*!*************************************************************
*!B802244,1
FUNCTION lfFixOrder
*-- NOTE THAT :
*-- THIS FUNCTION DOES NOT TAKE TRANCD='3' INTO CONSEDRATION  BECAUSE 
*-- THE MAIN PROGRAM MARECI.PRG DOES NOT DO . 


*-- INDEX
SELECT POFLN
SET ORDER TO TAG Poflnf

SELECT POFHDR
SET ORDER TO TAG Pofhdr

SELECT FABDYE
SET ORDER TO TAG Fabdye


*-- RELATIONS
SELECT FABRIC
SET RELATION TO Fabric + Color INTO POFLN
SELECT POFLN
SET RELATION TO cMatType + PoMat INTO POFHDR

SELECT FABRIC

SCAN &&--FOR FABRIC='TEST' AND COLOR='BLACK'  &&-- fabric       
  
  IF EOF('POFLN') &&-- it means onorder = 0 (i.e no orders for this mat)
    REPLACE ONORDER  WITH 0
  ELSE
    STORE 0 TO lnOn_Order

    *-- now scan pofln for all transaction on this fabric    
    *-- fabric+color+pono+trncd  
    *-- scan Po lines .
    lcCalOper = [IIF(cMatType='P', 1,-1) *]  && Transaction sign.
    SELECT POFLN
    SCAN REST WHILE Fabric+Color = Fabric.Fabric + Fabric.Color ;
      FOR cMatType <> 'C' AND !(Pofhdr.STATUS $ 'CX')
       
      lnUpOnOrd = 0      &&Variable used to update fabdye record.
      DO CASE
      
        CASE TRANCD = '1'    && Original line in PO.

         
          lnOn_Order = lnOn_Order + &lcCalOper. MAX( nFabTotQty , 0 )

          * Check for existance of Fabdye record to update onorder. [begin]
          IF SEEK(Fabric+Color+cWareCode,'FABDYE')
            lnUpOnOrd = IIF(Fabric.CONV=0, &lcCalOper. MAX( nFabTotQty , 0 ),;
                        ROUND(&lcCalOper. MAX( nFabTotQty , 0 ) * Fabric.CONV,1))
            *-walid
            =lfUpFabDye(lnUpOnOrd)
          ENDIF

        
        CASE TRANCD $ '24'  && Received, damage, or cancelled.
     
          
            
          IF TRANCD = '2'
            lnOn_Order = MAX( lnOn_Order - &lcCalOper. ABS( nFabTotQty ) , 0 )
          ELSE
            lnOn_Order = lnOn_Order - &lcCalOper. ABS( nFabTotQty ) 
          ENDIF

          * Check for existance of Fabdye record to update onorder. 
          IF SEEK(Fabric+Color+cWareCode,'FABDYE')
              
            lnUpOnOrd = IIF(Fabric.CONV=0, -1 * &lcCalOper.MAX( nFabTotQty , 0 ),;
                           ROUND(-1 * &lcCalOper.MAX( nFabTotQty , 0 ) * Fabric.CONV,1))

            =lfUpFabDye(lnUpOnOrd)
            
          ENDIF                  

           
      ENDCASE
 
    ENDSCAN  && end scan Material Po lines.

    SELECT Fabric
    lnOn_Order = IIF(CONV<>0, ROUND(lnOn_Order * CONV,1), lnOn_Order)
    REPLACE ONORDER WITH lnOn_Order
        
  ENDIF

ENDSCAN      &&-- FABRIC

*-- INDEX
SELECT POFLN
SET ORDER TO 

SELECT POFHDR
SET ORDER TO 

SELECT FABDYE
SET ORDER TO 


*-- RELATIONS
SELECT FABRIC
SET RELATION TO 
SELECT POFLN
SET RELATION TO 



*!*************************************************************
*! Name      : lfUpFabDye
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : Update fabdye record for current group.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Updated Value.
*!                        : 
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpFabDye(Parameters)
*!*************************************************************
*!

FUNCTION lfUpFabDye
PARAMETERS lnUpDtVal
PRIVATE lnAlias , lnDonorder , lnTrancd
lnAlias = SELECT(0)
lnTrancd = POFLN.TRANCD
lcMatType= POFLN.cMatType
SELECT ('Fabdye')
lnDonorder=Fabdye.onorder
lnUpDtVal=lnUpDtVal+lnDonorder
IF lnTrancd='2' AND lcMatType='P' AND lnUpDtVal < 0 
  lnUpDtVal = 0
ENDIF
REPLACE ONORDER WITH  lnUpDtVal
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfFixOnHand
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : calculate valuse for  
*!           : [OnHand , NSTKVAL , Nfave_Cost , Navecstbuy] fields
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFixOnHand()
*!*************************************************************
*!

FUNCTION lfFixOnHand
lcWareHous  =SPACE(6)
lnFOnHand   =0
lnFStkVal   =0
lnFAveCost  =0
lnFavecstbuy=0

lnDOnHand   =0
lnDStkVal   =0
lnDAveCost  =0
lnDavecstbuy=0

SELECT FABDYE
SET ORDER TO TAG FABDYE



SELECT MatInvJl
SET ORDER TO TAG Matinvjl &&--cfabric+ccolor+cwarecode+cdyelot+crsession+cisession+STR(RECNO(),7) 
SET RELATION TO Matinvjl.cfabric+Matinvjl.ccolor+Matinvjl.cwarecode INTO Fabdye ADDITIVE


SELECT FABRIC



SCAN    &&-- FABRIC
  lnConv      =conv
  lnFOnHand   =0
  lnFStkVal   =0
  lnFAveCost  =0
  lnFavecstbuy=0
  
  lnDOnHand   =0
  lnDStkVal   =0
  lnDAveCost  =0
  lnDavecstbuy=0
  
  SELECT MatInvJl
  lcSeek="FABRIC.FABRIC+FABRIC.COLOR,'MatInvJl'"

  IF SEEK(&lcSeek)
    lcWareHous= cwarecode
    SCAN WHILE cfabric+ccolor = FABRIC.FABRIC+FABRIC.COLOR 

      *-- CHECK IF THE LOCATION WAS CHANGED .
      IF MatInvJl.cwarecode # lcWareHous
        lcWareHous = MatInvJl.cwarecode
        lnDOnHand  = 0
        lnDStkVal  = 0
      ENDIF  
      
      
      IF MatInvJl.Nissued = 0
        
        lnDOnHand = lnDOnHand + MatInvJl.Nreceived
        lnDStkVal = lnDStkVal + (MatInvJl.NunitCost * MatInvJl.Nreceived)
        lnFOnHand = lnFOnHand + MatInvJl.Nreceived
        lnFStkVal = lnFStkVal + (MatInvJl.NunitCost * MatInvJl.Nreceived)
      
      ELSE
      
        lnDOnHand = lnDOnHand - MatInvJl.Nissued
        lnDStkVal = lnDStkVal - (MatInvJl.NunitCost * MatInvJl.Nissued)      
        lnFOnHand = lnFOnHand - MatInvJl.Nissued
        lnFStkVal = lnFStkVal - (MatInvJl.NunitCost * MatInvJl.Nissued)
      
      ENDIF
      
      lnDAveCost  =IIF( lnDOnHand = 0 ,0,lnDStkVal/lnDOnHand )
      lnDavecstbuy=ROUND((lnDAveCost * lnConv) , 3) 
    
      SELECT FABDYE
    
      REPLACE ONHAND      WITH lnDOnHand
      REPLACE NSTKVAL     WITH lnDStkVal 
      
      IF lnDOnHand <> 0
        REPLACE Nfave_Cost  WITH lnDAveCost
        REPLACE Navecstbuy  WITH lnDavecstbuy
      ENDIF
      
      SELECT MatInvJl
    
      IF EMPTY(Cdyelot)
        LOOP &&-- IN SCAN MatInvJl
      ELSE
        lcSubSeek= "Cfabric+Ccolor+Cwarecode+Cdyelot,'FABDYE'"
        IF SEEK(&lcSubSeek) &&-- THIS CONDITION MUST BE TRUE 
      
          SELECT FABDYE
          REPLACE ONHAND WITH lnDOnHand
          SELECT MatInvJl
      
        ENDIF
      ENDIF

    ENDSCAN  
  
    lnFAveCost  =IIF( lnFOnHand = 0 ,0,lnFStkVal/lnFOnHand )
    lnFavecstbuy=ROUND((lnFAveCost * lnConv) , 3)
    SELECT FABRIC
    REPLACE ONHAND      WITH lnFOnHand
    REPLACE NSTKVAL     WITH lnFStkVal 
    
    IF lnFOnHand <> 0
      REPLACE Nfave_Cost  WITH lnFAveCost
      REPLACE Navecstbuy  WITH lnFavecstbuy
    ENDIF
  
  ELSE
    LOOP &&-- IN SCAN FABRIC
  ENDIF  
ENDSCAN
*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : 
*!           : 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logic
*!*************************************************************
*! Example     : = lfOpenFile()
*!*************************************************************
*!
FUNCTION lfOpenFile
IF FILE(lcDataDir+'MatInvJl.DBF') AND ;
  FILE(lcDataDir+'Fabdye.DBF')    AND ;
  FILE(lcDataDir+'POFLN.DBF')     AND ;
  FILE(lcDatadir+'fabric.DBF')    AND ; 
  FILE(lcDatadir+'POFHDR.DBF')  
 llOpenMvj  = gfOpenFile(lcDataDir+'MatInvJl','','EX')
 llOpenPofh = gfOpenFile(lcDataDir+'POFHDR'  ,'','EX')
 llOpenPofl = gfOpenFile(lcDataDir+'POFLN'   ,'','EX')
 llOpenFabD = gfOpenFile(lcDataDir+'Fabdye'  ,'','EX')
 llOpenFab  = gfOpenFile(lcDataDir+'fabric'  ,'','EX')
  
  RETURN(.T.)
ELSE
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfClosFile
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : 
*!           : 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logic
*!*************************************************************
*! Example     : = lfClosFile()
*!*************************************************************
*!
FUNCTION lfClosFile
IF llOpenFab
  SELECT FABRIC
  SET FILTER TO
  USE IN FABRIC
ENDIF  

IF llOpenFabD
  USE IN FABDYE
ENDIF  

IF llOpenPofl
  USE IN POFLN
ENDIF  

IF llOpenPofh
  USE IN POFHDR
ENDIF  

IF llOpenMvj
  USE IN MatInvJl
ENDIF  
*!*************************************************************
*! Name      : lfReplace
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 05/13/1999
*! Purpose   : 
*!           : 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logic
*!*************************************************************
*! Example     : = lfReplace()
*!*************************************************************
*!
FUNCTION lfReplace

PRIVATE lcFabric , lcColor

SELECT FABDYE
SET ORDER TO TAG FABDYE

SELECT FABRIC
SET FILTER TO !MAKE
SET RELATION TO Fabric.fabric + Fabric.color INTO Fabdye 

SCAN
  REPLACE ONORDER WITH 0
  REPLACE ONHAND  WITH 0
  REPLACE NSTKVAL WITH 0

  lcFabric=FABRIC
  lcColor =COLOR
  IF !EOF('FABDYE')
    SELECT FABDYE
    SCAN REST WHILE FABDYE.FABRIC+FABDYE.COLOR=lcFabric+lcColor
      REPLACE ONORDER WITH 0
      REPLACE ONHAND  WITH 0
      REPLACE NSTKVAL WITH 0
    ENDSCAN  
    SELECT FABRIC
  ENDIF
ENDSCAN
SELECT FABRIC
SET ORDER TO
SET RELATION TO
SELECT FABDYE
SET ORDER TO