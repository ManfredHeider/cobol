      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0305.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0305.CPv  $
      *
      *   Rev 2.1   May 27 2009 14:16:24   t13ex43
      *AAS00815: PGM - Neukundenbonus
      *
      *   Rev 2.0   May 06 2009 14:17:30   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0305                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0305 (NKWBA   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.05.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00815!27.05.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR VIEW VIVB0305                           *
      *****************************************************************

        05 WB-NI-VIVB0305-TAB.
           10 WB-NI-VIVB0305-ELE   PIC S9(04)  BINARY OCCURS  15.
        05 WB-NI-VIVB0305          REDEFINES   WB-NI-VIVB0305-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-PART-VBNR       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-LAST-UPDAT      PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-ANZ             PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-EH-EING         PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-NKWBA-ANSPR-BETR      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0305.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-NKWBA-ANSPR-VOM          PIC  X(10).
           10 WB-NKWBA-PART-VBNR          PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-NKWBA-LAST-UPDAT         PIC  X(26).
           10 WB-NKWBA-ANZ                PIC S9(04)       BINARY.
           10 WB-NKWBA-EH-EING            PIC S9(09)V9(01) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-NKWBA-ANSPR-BETR         PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  15    *
      *****************************************************************
