      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0093.CPv  $
      *
      *   Rev 2.1   Aug 12 2004 11:43:42   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.0   Jun 23 2004 14:51:22   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0093                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0093 (LZUA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    25.06.2004                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00184!25.06.04! 2.1! NEU                       ! T.MUND   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0093                           *
      ******************************************************************
        05  WB-NI-VIVB0093-TAB.
           10 WB-NI-VIVB0093-ELE   PIC S9(04)  BINARY OCCURS  12 TIMES.
        05  WB-NI-VIVB0093         REDEFINES   WB-NI-VIVB0093-TAB.
           10 WB-NI-LZUA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-LZUA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-LZUA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-LZUA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0093.
           10 WB-LZUA-ST-NR                  PIC S9(09)       BINARY.
           10 WB-KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 WB-KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 WB-LZUA-ANSPR-VOM              PIC X(10).
           10 WB-KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-LZUA-LAST-UPDATE            PIC X(26).
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-LZUA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
