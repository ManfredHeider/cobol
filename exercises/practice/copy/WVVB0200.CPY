      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0200.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0200.CPv  $
      *
      *   Rev 2.1   Mar 10 2010 15:03:06   t13ex43
      *ASL00344: PGM - Module für Tabelle CBA
      *
      *   Rev 2.0   Feb 24 2010 13:58:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0200                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0200 (CBA)                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.02.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!24.02.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0200                           *
      *****************************************************************

        05 WB-NI-VIVB0200-TAB.
           10 WB-NI-VIVB0200-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVB0200          REDEFINES   WB-NI-VIVB0200-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-CBA-ANSPR-VOM         PIC S9(04)       BINARY.
           10 WB-NI-CBA-PART-VBNR         PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-CBA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-CBA-PART-BETRAG       PIC S9(04)       BINARY.
           10 WB-NI-CBA-PART-EH           PIC S9(04)       BINARY.
           10 WB-NI-CBA-PART-SEIT         PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0200.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-CBA-ANSPR-VOM            PIC  X(10).
           10 WB-CBA-PART-VBNR            PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-CBA-LAST-UPDATE          PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-CBA-PART-BETRAG          PIC S9(09)V9(02) COMP-3.
           10 WB-CBA-PART-EH              PIC S9(09)V9(01) COMP-3.
           10 WB-CBA-PART-SEIT            PIC  X(10).
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
