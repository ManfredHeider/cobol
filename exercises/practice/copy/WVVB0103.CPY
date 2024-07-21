      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0103.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0103.CPv  $
      *
      *   Rev 2.1   08 Nov 2006 16:51:58   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 12:31:12   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0103                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0103 (BUKA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    26.10.2006                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00060!26.10.06! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0103                           *
      ******************************************************************
        05  WB-NI-VIVB0103-TAB.
           10 WB-NI-VIVB0103-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0103         REDEFINES   WB-NI-VIVB0103-TAB.
           10 WB-NI-GEGEN-KONTO            PIC S9(04)       BINARY.
           10 WB-NI-BELEG-ART              PIC S9(04)       BINARY.
           10 WB-NI-KONTO-TYP              PIC S9(04)       BINARY.
           10 WB-NI-SHKZ                   PIC S9(04)       BINARY.
           10 WB-NI-BUKA-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-BUKA-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KTY-BUCH-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-BUCH-TYP           PIC S9(04)       BINARY.
           10 WB-NI-BUKA-LAST-UPDATE       PIC S9(04)       BINARY.

        05  WB-VIVB0103.
           10 WB-GEGEN-KONTO               PIC X(12).
           10 WB-BELEG-ART                 PIC X(03).
           10 WB-KONTO-TYP                 PIC X(01).
           10 WB-SHKZ                      PIC X(01).
           10 WB-BUKA-DAT-VON              PIC X(10).
           10 WB-BUKA-DAT-BIS              PIC X(10).
           10 WB-KTY-GESELL-TYP            PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP            PIC S9(04)       BINARY.
           10 WB-KTY-BUCH-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-BUCH-TYP              PIC S9(04)       BINARY.
           10 WB-BUKA-LAST-UPDATE          PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

