*:***************************************************************************
*: Program file  : MFFABCS
*: Program desc. : Fix the gldist file by posting material transaction
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 10/14/2002
*:     Developer : Khalid Mohi El-Din Mohamed (KHM)
*:***************************************************************************
*: Example : DO MFGLDST
*:***************************************************************************
*: Due to B606536
*:***************************************************************************
*

IF !gfSetup()
  RETURN
ENDIF  

SELECT GlDist
SCAN FOR INLIST(Tran_Type,"MA","MP")
  WAIT WINDOW "Transaction # " + Tran_No NOWAIT
  IF EMPTY(Posted)
    REPLACE Posted WITH "X"
  ENDIF
ENDSCAN
WAIT CLEAR
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Updating GL entries has been done successfully.')
