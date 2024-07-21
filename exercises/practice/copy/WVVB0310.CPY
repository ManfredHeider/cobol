      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0310.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0310.CPv  $
      *
      *   Rev 2.1   Jul 22 2011 10:38:38   t13ex13
      *ASL00382: RGT: PGM Ermittlung
      *
      *   Rev 2.0   May 03 2011 16:21:50   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0310                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0310 (FPA     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    03.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00382!03.05.11!2.1 ! NEU                       ! GUTFLEISCH*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0310                           *
      *****************************************************************

        05 WB-NI-VIVB0310-TAB.
           10 WB-NI-VIVB0310-ELE   PIC S9(04)  BINARY OCCURS  10.
        05 WB-NI-VIVB0310          REDEFINES   WB-NI-VIVB0310-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-FPA-ANSPR-VOM         PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-FPA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-FPA-ANSPR-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0310.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-FPA-ANSPR-VOM            PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-FPA-LAST-UPDATE          PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-FPA-ANSPR-BETRAG         PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************
