      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA015.CPv  $
      *
      *   Rev 2.1   Apr 23 2008 11:27:14   t13ae48
      *AVB00875: Verlängerung der Adressfelder  - MAAN (JMS)
      *
      *   Rev 2.0   Mar 10 2008 13:24:40   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA015                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.MAESS                                          *
      *    DATUM:    29.02.2008                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001
      *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00875!11.03.08!2.1 ! NEU                       ! J.MAESS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA015                           *
      ******************************************************************
        05  WB-NI-VIVBA015-TAB.
           10  WB-NI-VIVBA015-ELE  PIC S9(04)  COMP   OCCURS  24 TIMES.
        05  WB-NI-VIVBA015           REDEFINES   WB-NI-VIVBA015-TAB.
           10 WB-NI-MAAN-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-KTY-MAAN-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-MAAN-TYP     PIC S9(04)  COMP.
           10 WB-NI-MAAN-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-MAAN-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-MAAN-STRASSE     PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ-AKTUELL PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ         PIC S9(04)  COMP.
           10 WB-NI-MAAN-ORT         PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ-PF      PIC S9(04)  COMP.
           10 WB-NI-MAAN-ORT-PF      PIC S9(04)  COMP.
           10 WB-NI-MAAN-POSTFACH    PIC S9(04)  COMP.
           10 WB-NI-MAAN-WOHNT-BEI   PIC S9(04)  COMP.
           10 WB-NI-MAAN-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-MAAN-TELEFON     PIC S9(04)  COMP.
           10 WB-NI-MAAN-TELEFAX     PIC S9(04)  COMP.
           10 WB-NI-KTY-AUSL         PIC S9(04)  COMP.
           10 WB-NI-KNR-AUSL         PIC S9(04)  COMP.
           10 WB-NI-KTY-GRND         PIC S9(04)  COMP.
           10 WB-NI-KNR-GRND         PIC S9(04)  COMP.
           10 WB-NI-KTY-ZUSTELLUNG   PIC S9(04)  COMP.
           10 WB-NI-KNR-ZUSTELLUNG   PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP   PIC S9(04)  COMP.
        05  WB-VIVBA015.
           10 WB-MAAN-BEZUGS-NR    PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KTY-MAAN-TYP      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-MAAN-TYP      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MAAN-DAT-VON      PIC X(26)                VALUE SPACE.
           10 WB-MAAN-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-MAAN-STRASSE      PIC X(40)                VALUE SPACE.
           10 WB-MAAN-PLZ-AKTUELL  PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-PLZ          PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-ORT          PIC X(40)                VALUE SPACE.
           10 WB-MAAN-PLZ-PF       PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-ORT-PF       PIC X(40)                VALUE SPACE.
           10 WB-MAAN-POSTFACH     PIC X(6)                 VALUE SPACE.
           10 WB-MAAN-WOHNT-BEI    PIC X(40)                VALUE SPACE.
           10 WB-MAAN-DAT-BIS      PIC X(10)                VALUE SPACE.
           10 WB-MAAN-TELEFON      PIC X(26)                VALUE SPACE.
           10 WB-MAAN-TELEFAX      PIC X(26)                VALUE SPACE.
           10 WB-KTY-AUSL          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-AUSL          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-GRND          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-GRND          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-ZUSTELLUNG    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-ZUSTELLUNG    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************