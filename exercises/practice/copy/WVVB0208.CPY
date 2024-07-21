      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0208.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WVVB0208.CPv  $
      *
      *   Rev 2.4   Mar 01 2011 13:52:14   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.3   Feb 24 2011 13:22:06   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.2   Feb 16 2011 13:21:44   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodu
      *l, DCLGEN, Copy-Member
      *
      *   Rev 2.1   Jan 31 2011 11:01:10   t13ae14
      *ASL00366: neuer BOZ-DVAG-Wien incl. 40%-Regel -> Schreibmodule
      *
      *   Rev 2.0   Jan 31 2011 10:57:44   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0208                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0208 (BOZM    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    01.02.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00366!01.02.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0208                           *
      *****************************************************************

        05 WB-NI-VIVB0208-TAB.
           10 WB-NI-VIVB0208-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0208          REDEFINES   WB-NI-VIVB0208-TAB.
           10 WB-NI-BETR-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-PART-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-STICHTAG              PIC S9(04)       BINARY.
           10 WB-NI-KNR-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-KTY-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-BOZM-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-BETR-UMSATZ           PIC S9(04)       BINARY.
           10 WB-NI-BETR-UMSATZ-MAX       PIC S9(04)       BINARY.
           10 WB-NI-PART-UMSATZ           PIC S9(04)       BINARY.
           10 WB-NI-PART-ABZUG            PIC S9(04)       BINARY.
           10 WB-NI-BOZM-LAST-UPDATE      PIC S9(04)       BINARY.

        05 WB-VIVB0208.
           10 WB-BETR-ST-NR               PIC S9(09)       BINARY.
           10 WB-PART-ST-NR               PIC S9(09)       BINARY.
           10 WB-STICHTAG                 PIC  X(10).
           10 WB-KNR-ABZUG                PIC S9(04)       BINARY.
           10 WB-KTY-ABZUG                PIC S9(04)       BINARY.
           10 WB-BOZM-ANSPR-VOM           PIC  X(10).
           10 WB-BETR-UMSATZ              PIC S9(09)V9(01) COMP-3.
           10 WB-BETR-UMSATZ-MAX          PIC S9(09)V9(01) COMP-3.
           10 WB-PART-UMSATZ              PIC S9(09)V9(01) COMP-3.
           10 WB-PART-ABZUG               PIC S9(09)V9(01) COMP-3.
           10 WB-BOZM-LAST-UPDATE         PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************

