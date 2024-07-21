      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0214.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WVVB0214.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:43:30   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:51:54   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:38:34   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:29:50   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:15:06   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:05:30   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 13 2011 13:14:48   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: WVVB0214                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0214 (PABOA   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    13.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00420!13.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0214                           *
      *****************************************************************

        05 WB-NI-VIVB0214-TAB.
           10 WB-NI-VIVB0214-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0214          REDEFINES   WB-NI-VIVB0214-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-PABOA-UNTST           PIC S9(04)       BINARY.
           10 WB-NI-PABOA-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-PABA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-PABA-ANSP-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0214.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-PABOA-UNTST              PIC S9(09)       BINARY.
           10 WB-PABOA-ANSPR-VOM          PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-PABA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-PABA-ANSP-BETRAG         PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
