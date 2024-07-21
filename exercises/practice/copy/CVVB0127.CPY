      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0127.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0127.CPv  $
      *
      *   Rev 2.1   31 Mar 2020 11:46:26   t13ae56
      *AVB01574: PGM - Nachfolgeregelung Praxis (VB61)
      *
      *   Rev 2.0   Feb 04 2020 14:33:18   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.02.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-670 !04.02.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0127) NRP                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0127)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0127 TABLE
CCC004*    ( NRP_ABGEB_NR                INTEGER NOT NULL
CCC004*    , NRP_AUFNEHM_NR              INTEGER NOT NULL
CCC004*    , KTY_NR_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_NR_TYP                  SMALLINT NOT NULL
CCC004*    , NRP_DAT_VON                 DATE NOT NULL
CCC004*    , NRP_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , NRP_ZAHLUNG1                DATE
CCC004*    , NRP_BETRAG1                 DECIMAL(11, 2)
CCC004*    , NRP_ZAHLUNG2                DATE
CCC004*    , NRP_BETRAG2                 DECIMAL(11, 2)
CCC004*    , NRP_ZAHLUNG3                DATE
CCC004*    , NRP_BETRAG3                 DECIMAL(11, 2)
CCC004*    , NRP_ZAHLUNG4                DATE
CCC004*    , NRP_BETRAG4                 DECIMAL(11, 2)
CCC004*    , NRP_ZAHLUNG5                DATE
CCC004*    , NRP_BETRAG5                 DECIMAL(11, 2)
CCC004*    , NRP_DAUER                   SMALLINT NOT NULL
CCC004*    , NRP_ABSCHL_VON              DATE
CCC004*    , NRP_ABSCHL_BIS              DATE
CCC004*    , NRP_ABSCHL_ZAHLG            DECIMAL(11, 2)
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0127                          *
      *****************************************************************
 
       01  VIVB0127-GESAMT.
        05 NI-VIVB0127-TAB.
           10 NI-VIVB0127-ELE      PIC S9(04)  BINARY OCCURS  20.
        05 NI-VIVB0127             REDEFINES   NI-VIVB0127-TAB.
           10 NI-NRP-ABGEB-NR             PIC S9(04)       BINARY.
           10 NI-NRP-AUFNEHM-NR           PIC S9(04)       BINARY.
           10 NI-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 NI-NRP-DAT-VON              PIC S9(04)       BINARY.
           10 NI-NRP-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-NRP-ZAHLUNG1             PIC S9(04)       BINARY.
           10 NI-NRP-BETRAG1              PIC S9(04)       BINARY.
           10 NI-NRP-ZAHLUNG2             PIC S9(04)       BINARY.
           10 NI-NRP-BETRAG2              PIC S9(04)       BINARY.
           10 NI-NRP-ZAHLUNG3             PIC S9(04)       BINARY.
           10 NI-NRP-BETRAG3              PIC S9(04)       BINARY.
           10 NI-NRP-ZAHLUNG4             PIC S9(04)       BINARY.
           10 NI-NRP-BETRAG4              PIC S9(04)       BINARY.
           10 NI-NRP-ZAHLUNG5             PIC S9(04)       BINARY.
           10 NI-NRP-BETRAG5              PIC S9(04)       BINARY.
           10 NI-NRP-DAUER                PIC S9(04)       BINARY.
           10 NI-NRP-ABSCHL-VON           PIC S9(04)       BINARY.
           10 NI-NRP-ABSCHL-BIS           PIC S9(04)       BINARY.
           10 NI-NRP-ABSCHL-ZAHLG         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0127.
           10 NRP-ABGEB-NR                PIC S9(09)       BINARY.
           10 NRP-AUFNEHM-NR              PIC S9(09)       BINARY.
           10 KTY-NR-TYP                  PIC S9(04)       BINARY.
           10 KNR-NR-TYP                  PIC S9(04)       BINARY.
           10 NRP-DAT-VON                 PIC  X(10).
           10 NRP-LAST-UPDATE             PIC  X(26).
           10 NRP-ZAHLUNG1                PIC  X(10).
           10 NRP-BETRAG1                 PIC S9(09)V9(02) COMP-3.
           10 NRP-ZAHLUNG2                PIC  X(10).
           10 NRP-BETRAG2                 PIC S9(09)V9(02) COMP-3.
           10 NRP-ZAHLUNG3                PIC  X(10).
           10 NRP-BETRAG3                 PIC S9(09)V9(02) COMP-3.
           10 NRP-ZAHLUNG4                PIC  X(10).
           10 NRP-BETRAG4                 PIC S9(09)V9(02) COMP-3.
           10 NRP-ZAHLUNG5                PIC  X(10).
           10 NRP-BETRAG5                 PIC S9(09)V9(02) COMP-3.
           10 NRP-DAUER                   PIC S9(04)       BINARY.
           10 NRP-ABSCHL-VON              PIC  X(10).
           10 NRP-ABSCHL-BIS              PIC  X(10).
           10 NRP-ABSCHL-ZAHLG            PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  20                    *
      *****************************************************************
