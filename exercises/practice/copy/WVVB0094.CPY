      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0094.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0094.CPv  $
      *
      *   Rev 2.1   Aug 12 2004 11:43:34   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.0   Jun 23 2004 14:51:34   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0094                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0094 (LZUB)                            *
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
      * COBOL DECLARATION FOR TABLE VIVB0094                           *
      ******************************************************************
        05  WB-NI-VIVB0094-TAB.
           10 WB-NI-VIVB0094-ELE   PIC S9(04)  BINARY OCCURS  12 TIMES.
        05  WB-NI-VIVB0094         REDEFINES   WB-NI-VIVB0094-TAB.
           10 WB-NI-LZUB-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-LZUB-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-12MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-03MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANZ-MONATE       PIC S9(04)       BINARY.

        05  WB-VIVB0094.
           10 WB-LZUB-ST-NR                  PIC S9(09)       BINARY.
           10 WB-KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 WB-KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 WB-LZUB-STICHTAG               PIC X(10).
           10 WB-LZUB-ANSPR-VOM              PIC X(10).
           10 WB-LZUB-LAST-UPDATE            PIC X(26).
           10 WB-LZUB-MS-NR                  PIC S9(04)       BINARY.
           10 WB-LZUB-VBP-NR                 PIC S9(04)       BINARY.
           10 WB-LZUB-12MON-UMSATZ           PIC S9(09)V9     COMP-3.
           10 WB-LZUB-03MON-UMSATZ           PIC S9(09)V9     COMP-3.
           10 WB-LZUB-FAKTOR                 PIC S9(03)V99    COMP-3.
           10 WB-LZUB-ANZ-MONATE             PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
