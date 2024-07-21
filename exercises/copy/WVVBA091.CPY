      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA091.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA091.CPv  $
      *
      *   Rev 2.1   Mar 28 2006 17:04:16   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.0   Mar 28 2006 09:51:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA091                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA091 (MAPA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    28.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00226!28.03.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA091                          *
      *****************************************************************
        05  WB-NI-VIVBA091-TAB.
           10 WB-NI-VIVBA091-ELE   PIC S9(04)  BINARY OCCURS  12 TIMES.
        05  WB-NI-VIVBA091         REDEFINES   WB-NI-VIVBA091-TAB.
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
           10 WB-NI-MAPA-UNTST            PIC S9(04)       BINARY.

        05  WB-VIVBA091.
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
           10 WB-MAPA-UNTST                  PIC S9(09)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
