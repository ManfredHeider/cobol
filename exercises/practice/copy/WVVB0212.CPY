      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0212.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0212.CPv  $
      *
      *   Rev 2.1   Dec 12 2011 14:26:50   t13ex13
      *ASL00399: MAP für FVD
      *
      *   Rev 2.0   Nov 15 2011 14:52:18   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: WVVB0212                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0212 (MAPFVD  )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    11.11.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00399!11.11.11!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0212                           *
      *****************************************************************

        05 WB-NI-VIVB0212-TAB.
           10 WB-NI-VIVB0212-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 WB-WB-NI-VIVB0212             REDEFINES   WB-NI-VIVB0212-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-REF-VB     PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-TURNUS     PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-LFDNR      PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-REF-KZ     PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG      PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND   PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND   PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-EH         PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS  PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS  PIC S9(04)       BINARY.
           10 WB-NI-MAPFVD-LAST-UPD   PIC S9(04)       BINARY.

        05 WB-VIVB0212.
           10 WB-MA-ST-NR             PIC S9(9) USAGE COMP.
           10 WB-MAPFVD-REF-VB        PIC S9(9) USAGE COMP.
           10 WB-MAPFVD-TURNUS        PIC S9(9) USAGE COMP.
           10 WB-MAPFVD-LFDNR         PIC S9(4) USAGE COMP.
           10 WB-MAPFVD-REF-KZ        PIC X(1).
           10 WB-MAPFVD-BETRAG        PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 WB-KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 WB-KNR-ANSPR-GRUND      PIC S9(4) USAGE COMP.
           10 WB-KTY-ANSPR-GRUND      PIC S9(4) USAGE COMP.
           10 WB-MAPFVD-EH            PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-KTY-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 WB-KNR-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 WB-MAPFVD-LAST-UPD      PIC X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

