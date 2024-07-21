      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.10.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0309.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0309.CPv  $
      *
      *   Rev 2.1   Oct 29 2010 16:07:14   t13ae36
      *AAS00927: Neue 300 Einheiten Regel für alle
      *
      *   Rev 2.0   Oct 13 2010 12:07:54   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    13.10.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00927!13.10.10!2.1 ! NEU                       ! FRANKE    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0309) HNP                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0309)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0309 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , HNP_MONAT                   DATE NOT NULL
CCC004*    , KTY_HSTATUS                 SMALLINT NOT NULL
CCC004*    , KNR_HSTATUS                 SMALLINT NOT NULL
CCC004*    , HNP_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , HNP_HUERDE_BASIS            DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EEG                     DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_HUERDE_ABS              DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_HUERDE_REST             DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_EIGEN-KUM            DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_EIGEN                DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_EIGEN_H              DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_EIGEN_W              DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_GRUPPE               DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_GESAMT               DECIMAL(10, 1) NOT NULL
CCC004*    , HNP_EH_GESAMT_W             DECIMAL(10, 1) NOT NULL
CCC004*    , KTY_SATZ_ART                SMALLINT NOT NULL
CCC004*    , KNR_SATZ_ART                SMALLINT NOT NULL
CCC004*    , HNP_BEMERKUNG               CHAR(20)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0309                           *
      *****************************************************************

       01  VIVB0309-GESAMT.
        05 NI-VIVB0309-TAB.
           10 NI-VIVB0309-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVB0309             REDEFINES   NI-VIVB0309-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-HNP-MONAT                PIC S9(04)       BINARY.
           10 NI-KTY-HSTATUS              PIC S9(04)       BINARY.
           10 NI-KNR-HSTATUS              PIC S9(04)       BINARY.
           10 NI-HNP-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-HNP-HUERDE-BASIS         PIC S9(04)       BINARY.
           10 NI-HNP-EEG                  PIC S9(04)       BINARY.
           10 NI-HNP-HUERDE-ABS           PIC S9(04)       BINARY.
           10 NI-HNP-HUERDE-REST          PIC S9(04)       BINARY.
           10 NI-HNP-EH-EIGEN-KUM         PIC S9(04)       BINARY.
           10 NI-HNP-EH-EIGEN             PIC S9(04)       BINARY.
           10 NI-HNP-EH-EIGEN-H           PIC S9(04)       BINARY.
           10 NI-HNP-EH-EIGEN-W           PIC S9(04)       BINARY.
           10 NI-HNP-EH-GRUPPE            PIC S9(04)       BINARY.
           10 NI-HNP-EH-GESAMT            PIC S9(04)       BINARY.
           10 NI-HNP-EH-GESAMT-W          PIC S9(04)       BINARY.
           10 NI-KTY-SATZ-ART             PIC S9(04)       BINARY.
           10 NI-KNR-SATZ-ART             PIC S9(04)       BINARY.
           10 NI-HNP-BEMERKUNG            PIC S9(04)       BINARY.

        05 DCL-VIVB0309.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 HNP-MONAT                   PIC  X(10).
           10 KTY-HSTATUS                 PIC S9(04)       BINARY.
           10 KNR-HSTATUS                 PIC S9(04)       BINARY.
           10 HNP-LAST-UPDATE             PIC  X(26).
           10 HNP-HUERDE-BASIS            PIC S9(09)V9(01) COMP-3.
           10 HNP-EEG                     PIC S9(09)V9(01) COMP-3.
           10 HNP-HUERDE-ABS              PIC S9(09)V9(01) COMP-3.
           10 HNP-HUERDE-REST             PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-EIGEN-KUM            PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-EIGEN                PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-EIGEN-H              PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-EIGEN-W              PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-GRUPPE               PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-GESAMT               PIC S9(09)V9(01) COMP-3.
           10 HNP-EH-GESAMT-W             PIC S9(09)V9(01) COMP-3.
           10 KTY-SATZ-ART                PIC S9(04)       BINARY.
           10 KNR-SATZ-ART                PIC S9(04)       BINARY.
           10 HNP-BEMERKUNG               PIC  X(20).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

