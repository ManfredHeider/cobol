000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WVVBC004.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WVVBC004.CPv  $
000140*
000150*   Rev 2.1   Feb 14 2002 14:42:48   t13ae30
000160*AVB00527 - Erfassen Gewerbeschein für DVAG-Wien
000170*
000180*   Rev 2.0   Dec 27 2001 14:51:02   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220* AENDERUNGEN:                                                  *
000230* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
000240* --------------------------------------------------------------*
000250* AVB00527!27.12.01! 2.0! NEU                       ! WILLIG    *
000260*         !        !    !                           !           *
000270*         !        !    !                           !           *
000280*                                                               *
000290* ENDE-LOGBUCH                                                  *
000300*****************************************************************
000310* COBOL DECLARATION FOR VIEW VIVBC004                           *
000320*****************************************************************
000330  05  WB-NI-VIVBC004-TAB.
000340     10  WB-NI-VIVBC004-ELE  PIC S9(04)  COMP   OCCURS  30 TIMES.
000350  05  WB-NI-VIVBC004           REDEFINES   WB-NI-VIVBC004-TAB.
000360     10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
000370     10 WB-NI-MA-LAST-UPDATE   PIC S9(04)  COMP.
000380     10 WB-NI-KTY-MA-TYP       PIC S9(04)  COMP.
000390     10 WB-NI-KNR-MA-TYP       PIC S9(04)  COMP.
000400     10 WB-NI-MA-BDV-MITGLIED  PIC S9(04)  COMP.
000410     10 WB-NI-MA-STRAFE        PIC S9(04)  COMP.
000420     10 WB-NI-MA-VERSANDSTOP   PIC S9(04)  COMP.
000430     10 WB-NI-MA-ZV-ZVII-ABL   PIC S9(04)  COMP.
000440     10 WB-NI-MA-RCK-STZ       PIC S9(04)  COMP.
000450     10 WB-NI-MA-RCK-STZ-DAT   PIC S9(04)  COMP.
000460     10 WB-NI-MA-FUEH-ZEUG-DAT PIC S9(04)  COMP.
000470     10 WB-NI-MA-SCHUFA-DAT    PIC S9(04)  COMP.
000480     10 WB-NI-MA-34C-DAT       PIC S9(04)  COMP.
000490     10 WB-NI-MA-AFO           PIC S9(04)  COMP.
000500     10 WB-NI-MA-DARLEHEN      PIC S9(04)  COMP.
000510     10 WB-NI-MA-AVAG-DVAG     PIC S9(04)  COMP.
000520     10 WB-NI-MA-KOMPASS-AVAG  PIC S9(04)  COMP.
000530     10 WB-NI-MA-AUTOTELEFON   PIC S9(04)  COMP.
000540     10 WB-NI-MA-TELETEXT      PIC S9(04)  COMP.
000550     10 WB-NI-MA-ZV-NR         PIC S9(04)  COMP.
000560     10 WB-NI-MA-ZV-DAT-BIS-R  PIC S9(04)  COMP.
000570     10 WB-NI-MA-ZV-DAT-BIS    PIC S9(04)  COMP.
000580     10 WB-NI-MA-ZV-DAT-VON    PIC S9(04)  COMP.
000590     10 WB-NI-MA-ZV-KUE-DAT    PIC S9(04)  COMP.
000600     10 WB-NI-MA-ED-ZUSATZ     PIC S9(04)  COMP.
000610     10 WB-NI-MA-ZV-VERL       PIC S9(04)  COMP.
000620     10 WB-NI-MA-FG-DAT        PIC S9(04)  COMP.
000630     10 WB-NI-MA-127-DAT       PIC S9(04)  COMP.
000640     10 WB-NI-MA-VA-DAT        PIC S9(04)  COMP.
000650     10 WB-NI-MA-ANWERBER      PIC S9(04)  COMP.
000660  05  WB-VIVBC004.
000670     10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
000680     10 WB-MA-LAST-UPDATE    PIC X(26)                VALUE SPACE.
000690     10 WB-KTY-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
000700     10 WB-KNR-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
000710     10 WB-MA-BDV-MITGLIED   PIC X(1)                 VALUE SPACE.
000720     10 WB-MA-STRAFE         PIC X(1)                 VALUE SPACE.
000730     10 WB-MA-VERSANDSTOP    PIC X(1)                 VALUE SPACE.
000740     10 WB-MA-ZV-ZVII-ABL    PIC X(1)                 VALUE SPACE.
000750     10 WB-MA-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
000760     10 WB-MA-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
000770     10 WB-MA-FUEH-ZEUG-DAT  PIC X(10)                VALUE SPACE.
000780     10 WB-MA-SCHUFA-DAT     PIC X(10)                VALUE SPACE.
000790     10 WB-MA-34C-DAT        PIC X(10)                VALUE SPACE.
000800     10 WB-MA-AFO            PIC X(1)                 VALUE SPACE.
000810     10 WB-MA-DARLEHEN       PIC X(1)                 VALUE SPACE.
000820     10 WB-MA-AVAG-DVAG      PIC X(10)                VALUE SPACE.
000830     10 WB-MA-KOMPASS-AVAG   PIC X(10)                VALUE SPACE.
000840     10 WB-MA-AUTOTELEFON    PIC X(14)                VALUE SPACE.
000850     10 WB-MA-TELETEXT       PIC X(14)                VALUE SPACE.
000860     10 WB-MA-ZV-NR          PIC X(1)                 VALUE SPACE.
000870     10 WB-MA-ZV-DAT-BIS-R   PIC X(10)                VALUE SPACE.
000880     10 WB-MA-ZV-DAT-BIS     PIC X(10)                VALUE SPACE.
000890     10 WB-MA-ZV-DAT-VON     PIC X(10)                VALUE SPACE.
000900     10 WB-MA-ZV-KUE-DAT     PIC X(10)                VALUE SPACE.
000910     10 WB-MA-ED-ZUSATZ      PIC S9(04)  COMP         VALUE ZERO.
000920     10 WB-MA-ZV-VERL        PIC X(1)                 VALUE SPACE.
000930     10 WB-MA-FG-DAT         PIC X(10)                VALUE SPACE.
000940     10 WB-MA-127-DAT        PIC X(10)                VALUE SPACE.
000950     10 WB-MA-VA-DAT         PIC X(10)                VALUE SPACE.
000960     10 WB-MA-ANWERBER       PIC S9(9)   COMP         VALUE ZERO.
000970******************************************************************
000980* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
000990******************************************************************
