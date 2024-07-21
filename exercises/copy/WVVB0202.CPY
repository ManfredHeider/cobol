      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0202.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:11:00   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 13:43:30   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0202                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0202 (BOZBALT )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    15.07.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00352!15.07.10!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0202                           *
      *****************************************************************

        05 WB-NI-VIVB0202-TAB.
           10 WB-NI-VIVB0202-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVB0202          REDEFINES   WB-NI-VIVB0202-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-STICHTAG              PIC S9(04)       BINARY.
           10 WB-NI-ANSPR-MONAT           PIC S9(04)       BINARY.
           10 WB-NI-ANSPR-VOM             PIC S9(04)       BINARY.
           10 WB-NI-BOZBALT-LAST-UPD      PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-BOZ-12MON-UMS         PIC S9(04)       BINARY.
           10 WB-NI-BOZ-12MON-EQF         PIC S9(04)       BINARY.
           10 WB-NI-BOZ-03MON-UMS         PIC S9(04)       BINARY.
           10 WB-NI-EH-PRO-ABZUG          PIC S9(04)       BINARY.
           10 WB-NI-EH-NACH-PRO-ABZUG     PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ZW-BETRAG         PIC S9(04)       BINARY.
           10 WB-NI-EQF-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-BOZ-BETRAG            PIC S9(04)       BINARY.
           10 WB-NI-BOZ-FAKTOR            PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ANZ-MONATE        PIC S9(04)       BINARY.
           10 WB-NI-UMS-BED-ERF           PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ALT-AUSZAHLUNG    PIC S9(04)       BINARY.
           10 WB-NI-BOZ-NACH-DIR          PIC S9(04)       BINARY.

        05 WB-VIVB0202.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-STICHTAG                 PIC  X(10).
           10 WB-ANSPR-MONAT              PIC  X(10).
           10 WB-ANSPR-VOM                PIC  X(10).
           10 WB-BOZBALT-LAST-UPD         PIC  X(26).
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-BOZ-12MON-UMS            PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-12MON-EQF            PIC S9(03)V9(02) COMP-3.
           10 WB-BOZ-03MON-UMS            PIC S9(09)V9(01) COMP-3.
           10 WB-EH-PRO-ABZUG             PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NACH-PRO-ABZUG        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-ZW-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 WB-EQF-ABZUG                PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-BETRAG               PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-FAKTOR               PIC S9(03)V9(02) COMP-3.
           10 WB-BOZ-ANZ-MONATE           PIC S9(04)       BINARY.
           10 WB-UMS-BED-ERF              PIC  X(01).
           10 WB-BOZ-ALT-AUSZAHLUNG       PIC  X(01).
           10 WB-BOZ-NACH-DIR             PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

