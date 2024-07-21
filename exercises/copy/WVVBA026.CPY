      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA026.CPv  $
      *
      *   Rev 2.1   Sep 08 2011 15:53:50   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.0   Aug 05 2011 16:04:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA026                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA026 (OLZZ)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00070!08.08.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA026                           *
      *****************************************************************

        05 WB-NI-VIVBA026-TAB.
           10 WB-NI-VIVBA026-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVBA026          REDEFINES   WB-NI-VIVBA026-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-TURNUS           PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-HERKUNFT         PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-VERARB-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-BETRAG           PIC S9(04)       BINARY.

        05 WB-VIVBA026.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-OLZZ-MONAT               PIC  X(10).
           10 WB-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-OLZZ-TURNUS              PIC S9(09)       BINARY.
           10 WB-OLZZ-HERKUNFT            PIC  X(01).
           10 WB-OLZZ-VERARB-TYP          PIC  X(01).
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-OLZZ-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-OLZZ-BETRAG              PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
