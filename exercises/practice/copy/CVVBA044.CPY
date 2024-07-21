000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   CVVBA044.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/CVVBA044.CPv  $
000140*
000150*   Rev 2.1   Jun 20 2002 14:23:36   t13ae13
000160*AVB00606: Einführung Seminare für DVAG-Wien
000170*
000180*   Rev 2.0   May 22 2002 10:47:00   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220******************************************************************
000230* DCLGEN TABLE(VIVBA044)                                         *
000240*        LIBRARY(P130005.BP010000.SRCDCL(CVVBA044))              *
000250*        QUOTE                                                   *
000260* ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
000270******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA044 TABLE
CCC004*    ( KTY_SSTS_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_SSTS_TYP                   SMALLINT NOT NULL,
CCC004*      MS_NR                          SMALLINT NOT NULL,
CCC004*      VBP_NR                         SMALLINT NOT NULL,
CCC004*      KTY_GESELL_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_GESELL_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
000360******************************************************************
000370* COBOL DECLARATION FOR TABLE VIVBA044                           *
000380******************************************************************
000390 01  VIVBA044-GESAMT.
000400  05  NI-VIVBA044-TAB.
000410     10  NI-VIVBA044-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
000420  05  NI-VIVBA044            REDEFINES   NI-VIVBA044-TAB.
000430     10 NI-KTY-SSTS-TYP      PIC S9(04)  COMP.
000440     10 NI-KNR-SSTS-TYP      PIC S9(04)  COMP.
000450     10 NI-MS-NR             PIC S9(04)  COMP.
000460     10 NI-VBP-NR            PIC S9(04)  COMP.
000470     10 NI-KTY-GESELL-TYP    PIC S9(04)  COMP.
000480     10 NI-KNR-GESELL-TYP    PIC S9(04)  COMP.
000490  05  DCL-VIVBA044.
000500     10 KTY-SSTS-TYP         PIC S9(4) USAGE COMP.
000510     10 KNR-SSTS-TYP         PIC S9(4) USAGE COMP.
000520     10 MS-NR                PIC S9(4) USAGE COMP.
000530     10 VBP-NR               PIC S9(4) USAGE COMP.
000540     10 KTY-GESELL-TYP       PIC S9(4) USAGE COMP.
000550     10 KNR-GESELL-TYP       PIC S9(4) USAGE COMP.
000560******************************************************************
000570* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
000580******************************************************************
