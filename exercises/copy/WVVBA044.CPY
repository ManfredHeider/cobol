000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WVVBA044.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WVVBA044.CPv  $
000140*
000150*   Rev 2.1   Jun 20 2002 14:25:38   t13ae13
000160*AVB00606: Einführung Seminare für DVAG-Wien
000170*
000180*   Rev 2.0   May 22 2002 10:47:22   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220* AENDERUNGEN:                                                  *
000230* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
000240* --------------------------------------------------------------*
000250* AVB00606!24.05.02!2.1 ! NEU                       ! WALB      *
000260*         !        !    !                           !           *
000270*         !        !    !                           !           *
000280*                                                               *
000290* ENDE-LOGBUCH                                                  *
000300*****************************************************************
000310******************************************************************
000320* COBOL DECLARATION FOR TABLE VIVBA044                           *
000330******************************************************************
000340  05  WB-NI-VIVBA044-TAB.
000350     10  WB-NI-VIVBA044-ELE  PIC S9(04)  COMP   OCCURS  6  TIMES.
000360  05  WB-NI-VIVBA044         REDEFINES   WB-NI-VIVBA044-TAB.
000370     10 WB-NI-KTY-SSTS-TYP   PIC S9(04)  COMP.
000380     10 WB-NI-KNR-SSTS-TYP   PIC S9(04)  COMP.
000390     10 WB-NI-MS-NR          PIC S9(04)  COMP.
000400     10 WB-NI-VBP-NR         PIC S9(04)  COMP.
000410     10 WB-NI-KTY-GESELL-TYP PIC S9(04)  COMP.
000420     10 WB-NI-KNR-GESELL-TYP PIC S9(04)  COMP.
000430  05  WB-VIVBA044.
000440     10 WB-KTY-SSTS-TYP      PIC S9(4)  COMP          VALUE ZERO.
000450     10 WB-KNR-SSTS-TYP      PIC S9(4)  COMP          VALUE ZERO.
000460     10 WB-MS-NR             PIC S9(4)  COMP          VALUE ZERO.
000470     10 WB-VBP-NR            PIC S9(4)  COMP          VALUE ZERO.
000480     10 WB-KTY-GESELL-TYP    PIC S9(4)  COMP          VALUE ZERO.
000490     10 WB-KNR-GESELL-TYP    PIC S9(4)  COMP          VALUE ZERO.
000500******************************************************************
000510* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
000520******************************************************************
