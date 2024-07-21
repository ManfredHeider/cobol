      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB015.CPv  $
      *
      *   Rev 2.1   Dec 16 2009 11:12:54   t13ae13
      *AVB01070: Meldeadresse Gewerbeanmeldung
      *
      *   Rev 2.0   Nov 26 2009 10:02:46   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB015                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S.Walb                                           *
      *    DATUM:    26.11.2009                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001
      *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01070!26.11.09!2.1 ! NEU                       ! S.Walb   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB015                           *
      ******************************************************************
        05  WB-NI-VIVBB015-TAB.
           10  WB-NI-VIVBB015-ELE  PIC S9(04)  COMP   OCCURS  26 TIMES.
        05  WB-NI-VIVBB015           REDEFINES   WB-NI-VIVBB015-TAB.
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
           10 WB-NI-KTY-GEWERBE      PIC S9(04)  COMP.
           10 WB-NI-KNR-GEWERBE      PIC S9(04)  COMP.
        05  WB-VIVBB015.
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
           10 WB-KTY-GEWERBE       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-GEWERBE       PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
