      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0053.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0053.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:52:36   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Feb 27 1999 15:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:21:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.07.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01332!08.07.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0053) BVSP                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0053)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0053 TABLE
CCC004*    ( VS_ZEITRAUM                 SMALLINT NOT NULL
CCC004*    , VB_NR                       INTEGER NOT NULL
CCC004*    , VB_BEZUGS_NR                INTEGER NOT NULL
CCC004*    , BETR_DAT_VON                DATE NOT NULL
CCC004*    , BETR_DAT_BIS                DATE
CCC004*    , ERSTELL_DATUM               DATE NOT NULL
CCC004*    , EH_GES_NEU                  DECIMAL(10, 1)
CCC004*    , KTY_EREIGNIS_ANF            SMALLINT
CCC004*    , KNR_EREIGNIS_ANF            SMALLINT
CCC004*    , KTY_EREIGNIS_ENDE           SMALLINT
CCC004*    , KNR_EREIGNIS_ENDE           SMALLINT
CCC004*    , FOLGE_EH_DAT_BIS            DATE
CCC004*    , EH_ZUSCHLAG                 DECIMAL(10, 1)
CCC004*    , ABZPROZ_REGEL_NEU           DECIMAL(05, 2)
CCC004*    , EH_SOLLANTEIL_NEU           DECIMAL(10, 1)
CCC004*    , EH_ABZUG_PROZ_NEU           DECIMAL(10, 1)
CCC004*    , EH_ABZUG_BETR_W             DECIMAL(10, 1)
CCC004*    , PART_EH_DAT_VON             DATE
CCC004*    , PART_EH_DAT_BIS             DATE
CCC004*    , EH_PARTNER                  DECIMAL(10, 1)
CCC004*    , PART_WERTEN                 CHAR(01)
CCC004*    , PART_WERT_AB_RGS            CHAR(01)
CCC004*    , EH_GES_11                   DECIMAL(10, 1)
CCC004*    , ABZPROZ_REGEL_11            DECIMAL(05, 2)
CCC004*    , EH_SOLLANTEIL_11            DECIMAL(10, 1)
CCC004*    , EH_ABZUG_PROZ_11            DECIMAL(10, 1)
CCC004*    , PART_EBENE                  CHAR(01)
CCC004*    , EH_EIG_NEU                  DECIMAL(10, 1)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0053                           *
      *****************************************************************

       01  VIVB0053-GESAMT.
        05 NI-VIVB0053-TAB.
           10 NI-VIVB0053-ELE      PIC S9(04)  BINARY OCCURS  28.
        05 NI-VIVB0053             REDEFINES   NI-VIVB0053-TAB.
           10 NI-VS-ZEITRAUM              PIC S9(04)       BINARY.
           10 NI-VB-NR                    PIC S9(04)       BINARY.
           10 NI-VB-BEZUGS-NR             PIC S9(04)       BINARY.
           10 NI-BETR-DAT-VON             PIC S9(04)       BINARY.
           10 NI-BETR-DAT-BIS             PIC S9(04)       BINARY.
           10 NI-ERSTELL-DATUM            PIC S9(04)       BINARY.
           10 NI-EH-GES-NEU               PIC S9(04)       BINARY.
           10 NI-KTY-EREIGNIS-ANF         PIC S9(04)       BINARY.
           10 NI-KNR-EREIGNIS-ANF         PIC S9(04)       BINARY.
           10 NI-KTY-EREIGNIS-ENDE        PIC S9(04)       BINARY.
           10 NI-KNR-EREIGNIS-ENDE        PIC S9(04)       BINARY.
           10 NI-FOLGE-EH-DAT-BIS         PIC S9(04)       BINARY.
           10 NI-EH-ZUSCHLAG              PIC S9(04)       BINARY.
           10 NI-ABZPROZ-REGEL-NEU        PIC S9(04)       BINARY.
           10 NI-EH-SOLLANTEIL-NEU        PIC S9(04)       BINARY.
           10 NI-EH-ABZUG-PROZ-NEU        PIC S9(04)       BINARY.
           10 NI-EH-ABZUG-BETR-W          PIC S9(04)       BINARY.
           10 NI-PART-EH-DAT-VON          PIC S9(04)       BINARY.
           10 NI-PART-EH-DAT-BIS          PIC S9(04)       BINARY.
           10 NI-EH-PARTNER               PIC S9(04)       BINARY.
           10 NI-PART-WERTEN              PIC S9(04)       BINARY.
           10 NI-PART-WERT-AB-RGS         PIC S9(04)       BINARY.
           10 NI-EH-GES-11                PIC S9(04)       BINARY.
           10 NI-ABZPROZ-REGEL-11         PIC S9(04)       BINARY.
           10 NI-EH-SOLLANTEIL-11         PIC S9(04)       BINARY.
           10 NI-EH-ABZUG-PROZ-11         PIC S9(04)       BINARY.
           10 NI-PART-EBENE               PIC S9(04)       BINARY.
           10 NI-EH-EIG-NEU               PIC S9(04)       BINARY.

        05 DCL-VIVB0053.
           10 VS-ZEITRAUM                 PIC S9(04)       BINARY.
           10 VB-NR                       PIC S9(09)       BINARY.
           10 VB-BEZUGS-NR                PIC S9(09)       BINARY.
           10 BETR-DAT-VON                PIC  X(10).
           10 BETR-DAT-BIS                PIC  X(10).
           10 ERSTELL-DATUM               PIC  X(10).
           10 EH-GES-NEU                  PIC S9(09)V9(01) COMP-3.
           10 KTY-EREIGNIS-ANF            PIC S9(04)       BINARY.
           10 KNR-EREIGNIS-ANF            PIC S9(04)       BINARY.
           10 KTY-EREIGNIS-ENDE           PIC S9(04)       BINARY.
           10 KNR-EREIGNIS-ENDE           PIC S9(04)       BINARY.
           10 FOLGE-EH-DAT-BIS            PIC  X(10).
           10 EH-ZUSCHLAG                 PIC S9(09)V9(01) COMP-3.
           10 ABZPROZ-REGEL-NEU           PIC S9(03)V9(02) COMP-3.
           10 EH-SOLLANTEIL-NEU           PIC S9(09)V9(01) COMP-3.
           10 EH-ABZUG-PROZ-NEU           PIC S9(09)V9(01) COMP-3.
           10 EH-ABZUG-BETR-W             PIC S9(09)V9(01) COMP-3.
           10 PART-EH-DAT-VON             PIC  X(10).
           10 PART-EH-DAT-BIS             PIC  X(10).
           10 EH-PARTNER                  PIC S9(09)V9(01) COMP-3.
           10 PART-WERTEN                 PIC  X(01).
           10 PART-WERT-AB-RGS            PIC  X(01).
           10 EH-GES-11                   PIC S9(09)V9(01) COMP-3.
           10 ABZPROZ-REGEL-11            PIC S9(03)V9(02) COMP-3.
           10 EH-SOLLANTEIL-11            PIC S9(09)V9(01) COMP-3.
           10 EH-ABZUG-PROZ-11            PIC S9(09)V9(01) COMP-3.
           10 PART-EBENE                  PIC  X(01).
           10 EH-EIG-NEU                  PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  28                    *
      *****************************************************************

