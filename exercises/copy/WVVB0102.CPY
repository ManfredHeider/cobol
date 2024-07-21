      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0102.CPv  $
      *
      *   Rev 2.1   08 Nov 2006 16:51:52   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 11:04:54   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: WVVB0102                                     *
      *    --------------------------------------------------------  *
      *    WORKBEREICH FÜR VIVB0102 (SONSTB)                         *
      *    --------------------------------------------------------  *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    26.10.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00060!26.10.06! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0102                          *
      *****************************************************************
        05  WB-NI-VIVB0102-TAB.
           10 WB-NI-VIVB0102-ELE   PIC S9(04)  BINARY OCCURS  18 TIMES.
        05  WB-NI-VIVB0102         REDEFINES   WB-NI-VIVB0102-TAB.
           10 WB-NI-BUCH-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 WB-NI-KTY-BUCH-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-BUCH-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG           PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG           PIC S9(04)       BINARY.
           10 WB-NI-BETRAG                 PIC S9(04)       BINARY.
           10 WB-NI-BUCH-TEXT              PIC S9(04)       BINARY.
           10 WB-NI-KONTO-TYP              PIC S9(04)       BINARY.
           10 WB-NI-BELEGART               PIC S9(04)       BINARY.
           10 WB-NI-BELEG-NR               PIC S9(04)       BINARY.
           10 WB-NI-TURNUS                 PIC S9(04)       BINARY.
           10 WB-NI-BELEG-DAT              PIC S9(04)       BINARY.
           10 WB-NI-BUCH-DAT               PIC S9(04)       BINARY.
           10 WB-NI-KOSTEN-STELLE          PIC S9(04)       BINARY.
           10 WB-NI-GEGEN-KONTO            PIC S9(04)       BINARY.
           10 WB-NI-SHKZ                   PIC S9(04)       BINARY.
           10 WB-NI-BUCH-LAST-UPDATE       PIC S9(04)       BINARY.

        05  WB-VIVB0102.
           10 WB-BUCH-DAT-VON              PIC X(10).
           10 WB-MA-ST-NR                  PIC S9(09)       BINARY.
           10 WB-KTY-BUCH-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-BUCH-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG              PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG              PIC S9(04)       BINARY.
           10 WB-BETRAG                    PIC S9(09)V9(02) COMP-3.
           10 WB-BUCH-TEXT                 PIC X(40).
           10 WB-KONTO-TYP                 PIC X(01).
           10 WB-BELEGART                  PIC X(03).
           10 WB-BELEG-NR                  PIC X(07).
           10 WB-TURNUS                    PIC X(06).
           10 WB-BELEG-DAT                 PIC X(08).
           10 WB-BUCH-DAT                  PIC X(08).
           10 WB-KOSTEN-STELLE             PIC X(08).
           10 WB-GEGEN-KONTO               PIC X(12).
           10 WB-SHKZ                      PIC X(01).
           10 WB-BUCH-LAST-UPDATE          PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************

