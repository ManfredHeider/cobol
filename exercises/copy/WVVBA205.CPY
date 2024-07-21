      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA205.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA205.CPv  $
      *
      *   Rev 2.1   Jun 01 2017 14:48:50   t13ae30
      *ASL00567: Anpassen QED Ermittlung - TWI
      *
      *   Rev 2.0   Mar 30 2017 14:21:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA205                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA205 (QEDD    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    06.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!06.04.17!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA205                           *
      *****************************************************************

        05 WB-NI-VIVBA205-TAB.
           10 WB-NI-VIVBA205-ELE   PIC S9(04)  BINARY OCCURS  18.
        05 WB-NI-VIVBA205          REDEFINES   WB-NI-VIVBA205-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-ED-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ED-ZUSATZ        PIC S9(04)       BINARY.
           10 WB-NI-ED-GENERATION         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-DATUM            PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ED-EINH          PIC S9(04)       BINARY.
           10 WB-NI-QEDD-EINH-MAX         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-EINH-GEW         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-DL-NET-GES-EH         PIC S9(04)       BINARY.
           10 WB-NI-DL-EH-MAX-PROZ        PIC S9(04)       BINARY.
           10 WB-NI-ED-EINH-FAKTOR        PIC S9(04)       BINARY.
           10 WB-NI-QUARTAL               PIC S9(04)       BINARY.
           10 WB-NI-QEDD-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-UST-KZ            PIC S9(04)       BINARY.
           10 WB-NI-KNR-UST-KZ            PIC S9(04)       BINARY.

        05 WB-VIVBA205.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-ED-ST-NR                 PIC S9(09)       BINARY.
           10 WB-QEDD-ED-ZUSATZ           PIC S9(04)       BINARY.
           10 WB-ED-GENERATION            PIC S9(04)       BINARY.
           10 WB-QEDD-DATUM               PIC  X(10).
           10 WB-QEDD-ED-EINH             PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-EINH-MAX            PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-EINH-GEW            PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-DL-NET-GES-EH            PIC S9(09)V9(01) COMP-3.
           10 WB-DL-EH-MAX-PROZ           PIC S9(03)V9(03) COMP-3.
           10 WB-ED-EINH-FAKTOR           PIC S9(03)V9(03) COMP-3.
           10 WB-QUARTAL                  PIC  X(17).
           10 WB-QEDD-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-UST-KZ               PIC S9(04)       BINARY.
           10 WB-KNR-UST-KZ               PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  18                    *
      *****************************************************************

