      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA206.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA206.CPv  $
      *
      *   Rev 2.1   Jun 01 2017 14:48:58   t13ae30
      *ASL00567: Anpassen QED Ermittlung - TWI
      *
      *   Rev 2.0   Mar 30 2017 14:22:00   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     *****************************************************************
      *    COPY-MEMBER: WVVBA206                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA206 (QEDA    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    06.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!06.04.2017.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA206                           *
      *****************************************************************

        05 WB-NI-VIVBA206-TAB.
           10 WB-NI-VIVBA206-ELE   PIC S9(04)  BINARY OCCURS  12.
        05 WB-NI-VIVBA206          REDEFINES   WB-NI-VIVBA206-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-ED-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-QEDA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-QEDA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-QEDA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-QEDA-ANSPR-BETR-UST   PIC S9(04)       BINARY.

        05 WB-VIVBA206.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-ED-ST-NR                 PIC S9(09)       BINARY.
           10 WB-QEDA-ANSPR-VOM           PIC  X(10).
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-QEDA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-QEDA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-QEDA-ANSPR-BETR-UST      PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

