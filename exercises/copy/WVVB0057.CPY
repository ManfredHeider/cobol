      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0057.CPv  $
      *
      *   Rev 2.1   Feb 15 2006 09:12:38   t13ex43
      *ASL00224: PGM - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   Jan 30 2006 16:39:30   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0057                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0057 (ABBA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    30.01.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00224!30.01.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0057                          *
      *****************************************************************
        05  WB-NI-VIVB0057-TAB.
           10 WB-NI-VIVB0057-ELE   PIC S9(04)  BINARY OCCURS  14 TIMES.
        05  WB-NI-VIVB0057         REDEFINES   WB-NI-VIVB0057-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-ABBA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-ABBA-PART-VBNR        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-ABBA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-ABBA-PART-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-ABBA-PART-EH          PIC S9(04)       BINARY.
           10 WB-NI-ABBA-PART-SEIT        PIC S9(04)       BINARY.
           10 WB-NI-ABBA-BETREUT-SEIT     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0057.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-ABBA-ANSPR-VOM           PIC  X(10).
           10 WB-ABBA-PART-VBNR           PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-ABBA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-ABBA-PART-BETRAG         PIC S9(09)V9(02) COMP-3.
           10 WB-ABBA-PART-EH             PIC S9(09)V9(01) COMP-3.
           10 WB-ABBA-PART-SEIT           PIC  X(10).
           10 WB-ABBA-BETREUT-SEIT        PIC  X(10).
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
