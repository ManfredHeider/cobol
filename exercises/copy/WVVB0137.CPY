      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VERSIONMANAGER
      *            DATUM      :  31.08.2020
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2020 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   WVVB0137.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:/MF/WVVB0137.CPV  $
      *
      *   REV 2.0   AUG 31 2020 14:15:00   U130213
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0137                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0137 (EHBASISB)                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    31.08.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * -       |31.08.20|2.1 | NEU                       | -         *
      *         |        |    |                           |           *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0137                          *
      *****************************************************************

        05 WB-NI-VIVB0137-TAB.
           10 WB-NI-VIVB0137-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0137          REDEFINES   WB-NI-VIVB0137-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-EHBASISB-MONAT        PIC S9(04)       BINARY.
           10 WB-NI-AVERM                 PIC S9(04)       BINARY.
           10 WB-NI-PROD-ART              PIC S9(04)       BINARY.
           10 WB-NI-EIGEN-KZ              PIC S9(04)       BINARY.
           10 WB-NI-STRUK-BER-KZ          PIC S9(04)       BINARY.
           10 WB-NI-EHBASISB-EH           PIC S9(04)       BINARY.
           10 WB-NI-LAST-UPDATE           PIC S9(04)       BINARY.

        05 WB-VIVB0137.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-EHBASISB-MONAT           PIC  X(10).
           10 WB-AVERM                    PIC S9(09)       BINARY.
           10 WB-PROD-ART                 PIC S9(04)       BINARY.
           10 WB-EIGEN-KZ                 PIC  X(01).
           10 WB-STRUK-BER-KZ             PIC  X(01).
           10 WB-EHBASISB-EH              PIC S9(10)V9(01) COMP-3.
           10 WB-LAST-UPDATE              PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************


