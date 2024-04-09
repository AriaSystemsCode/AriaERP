*:************************************************************************
*: Program file  : ICCANGMA.PRG
*: Program desc. : Call the gfDispRe for the ICCANGMA program reports
*:         System: Aria Advantage Series - Version 2.7
*:         Module: IC Module
*:      Developer: Mohamed Shokry
*:************************************************************************

lcOgPlatFo = "WINDOWS"
DO gfDispRe WITH (gcRepHome +gcWinAppl+"\"+ lcRpName)
