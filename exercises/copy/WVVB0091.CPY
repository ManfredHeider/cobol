      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0091.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0091.CPv  $
      *
      *   Rev 2.1   May 11 2004 15:04:14   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.0   May 10 2004 14:06:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0091                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0091 (MAPA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    10.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!10.05.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0091                          *
      *****************************************************************
        05  WB-NI-VIVB0091-TAB.
           10 WB-NI-VIVB0091-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0091         REDEFINES   WB-NI-VIVB0091-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-MAP-ANSPR-VOM         PIC S9(04)       BINARY.
           10 WB-NI-MAP-STUFE             PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-MAP-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-MAP-ANSPR-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0091.
           10 WB-MA-ST-NR                    PIC S9(09)       BINARY.
           10 WB-MAP-ANSPR-VOM               PIC X(10).
           10 WB-MAP-STUFE                   PIC X(10).
           10 WB-KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-MAP-LAST-UPDATE             PIC X(26).
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-MAP-ANSPR-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
