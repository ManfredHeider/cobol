      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0090.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0090.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 12:12:44   t13ex43
      *ASL00169 : Zusatzbonus 2003
      *
      *   Rev 2.0   Aug 19 2003 12:34:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0090                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0090 (ZBOA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    19.08.2003                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00169!19.08.03!2.1 ! NEU (ANALOG WVVB0088)     ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0090                          *
      *****************************************************************
        05  WB-NI-VIVB0090-TAB.
           10 WB-NI-VIVB0090-ELE   PIC S9(04)  BINARY OCCURS  10 TIMES.
        05  WB-NI-VIVB0090         REDEFINES   WB-NI-VIVB0090-TAB.
           10 WB-NI-ZBOA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-ZBOA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-ZBOA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-ZBOA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0090.
           10 WB-ZBOA-ST-NR                  PIC S9(09)       BINARY.
           10 WB-ZBOA-ANSPR-VOM              PIC X(10).
           10 WB-KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-ZBOA-LAST-UPDATE            PIC X(26).
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-ZBOA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
