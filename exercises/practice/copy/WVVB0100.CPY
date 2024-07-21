      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0100.CPv  $
      *
      *   Rev 2.1   Sep 27 2006 09:31:06   t13ex43
      *AIS00060: PGM - Geldleistungen konfigurierbar machen
      *
      *   Rev 2.0   Aug 04 2006 09:01:28   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0100                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0100 (FIBUZ)                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    08.08.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00060!08.08.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0100                          *
      *****************************************************************
        05  WB-NI-VIVB0100-TAB.
           10 WB-NI-VIVB0100-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0100         REDEFINES   WB-NI-VIVB0100-TAB.
           10 WB-NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-KONTO              PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-DAT-VON            PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-LAST-UPDATE        PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-DAT-BIS            PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-INFO               PIC S9(04)       BINARY.
           10 WB-NI-KTY-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 WB-NI-KNR-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 WB-NI-KTY-DATUM                PIC S9(04)       BINARY.
           10 WB-NI-KNR-DATUM                PIC S9(04)       BINARY.

        05  WB-VIVB0100.
           10 WB-KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 WB-FIBUZ-KONTO                 PIC  X(12).
           10 WB-FIBUZ-DAT-VON               PIC  X(26).
           10 WB-FIBUZ-LAST-UPDATE           PIC  X(26).
           10 WB-FIBUZ-DAT-BIS               PIC  X(10).
           10 WB-FIBUZ-INFO                  PIC  X(30).
           10 WB-KTY-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 WB-KNR-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 WB-KTY-DATUM                   PIC S9(04)       BINARY.
           10 WB-KNR-DATUM                   PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
