      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.01.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA021.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA021.CPv  $
      *
      *   Rev 2.1   Jan 28 2013 17:51:52   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - BV-Erweite
      *rung
      *
      *   Rev 2.0   Jan 15 2013 11:18:16   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA021                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA021 (BV      )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    15.01.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01244!15.01.13!2.1 ! NEU                       ! PRETSCH   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA021                           *
      *****************************************************************

        05 WB-NI-VIVBA021-TAB.
           10 WB-NI-VIVBA021-ELE   PIC S9(04)  BINARY OCCURS  14.
        05 WB-NI-VIVBA021          REDEFINES   WB-NI-VIVBA021-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-KTY-BV-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-BV-TYP            PIC S9(04)       BINARY.
           10 WB-NI-BV-DAT-VON            PIC S9(04)       BINARY.
           10 WB-NI-BV-LAST-UPDATE        PIC S9(04)       BINARY.
           10 WB-NI-BV-DAT-BIS            PIC S9(04)       BINARY.
           10 WB-NI-KTY-SPERRE            PIC S9(04)       BINARY.
           10 WB-NI-KNR-SPERRE            PIC S9(04)       BINARY.
           10 WB-NI-BV-KTO-NR             PIC S9(04)       BINARY.
           10 WB-NI-BV-BLZ                PIC S9(04)       BINARY.
           10 WB-NI-BV-BANKNAME           PIC S9(04)       BINARY.
           10 WB-NI-BV-KTO-INHABER        PIC S9(04)       BINARY.
           10 WB-NI-BV-IBAN               PIC S9(04)       BINARY.
           10 WB-NI-BV-BIC                PIC S9(04)       BINARY.

        05 WB-VIVBA021.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-KTY-BV-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-BV-TYP               PIC S9(04)       BINARY.
           10 WB-BV-DAT-VON               PIC  X(26).
           10 WB-BV-LAST-UPDATE           PIC  X(26).
           10 WB-BV-DAT-BIS               PIC  X(10).
           10 WB-KTY-SPERRE               PIC S9(04)       BINARY.
           10 WB-KNR-SPERRE               PIC S9(04)       BINARY.
           10 WB-BV-KTO-NR                PIC  X(20).
           10 WB-BV-BLZ                   PIC  X(11).
           10 WB-BV-BANKNAME              PIC  X(40).
           10 WB-BV-KTO-INHABER           PIC  X(40).
           10 WB-BV-IBAN                  PIC  X(34).
           10 WB-BV-BIC                   PIC  X(11).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *
      *****************************************************************
