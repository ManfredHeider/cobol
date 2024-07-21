      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA089.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA089.CPv  $
      *
      *   Rev 2.1   May 04 2016 14:37:30   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA089
      *
      *   Rev 2.0   May 03 2016 14:29:54   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA089                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA089 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    03.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!03.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA089                           *
      *****************************************************************
 
        05 WB-NI-VIVBA089-TAB.
           10 WB-NI-VIVBA089-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVBA089          REDEFINES   WB-NI-VIVBA089-TAB.
           10 WB-NI-BOZB-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-BOZB-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-BOZB-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-BOZB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-BOZB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-BOZB-12MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-BOZB-12MON-EQF        PIC S9(04)       BINARY.
           10 WB-NI-BOZB-03MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-BOZB-EQF-ABZUG        PIC S9(04)       BINARY.
           10 WB-NI-BOZB-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-BOZB-ANZ-MONATE       PIC S9(04)       BINARY.
           10 WB-NI-BOZB-12M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-BOZB-03M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-BOZB-EQF-ABZ-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-BOZB-EH-ABZ-NEG       PIC S9(04)       BINARY.
           10 WB-NI-BOZB-03M-KORRIG       PIC S9(04)       BINARY.
           10 WB-NI-BOZB-03M-KORR-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-BOZB-EH-ANTEIL        PIC S9(04)       BINARY.
           10 WB-NI-BOZB-EH-ANT-UPFL      PIC S9(04)       BINARY.
 
        05 WB-VIVBA089.
           10 WB-BOZB-ST-NR               PIC S9(09)       BINARY.
           10 WB-BOZB-STICHTAG            PIC  X(10).
           10 WB-BOZB-ANSPR-VOM           PIC  X(10).
           10 WB-BOZB-LAST-UPDATE         PIC  X(26).
           10 WB-BOZB-MS-NR               PIC S9(04)       BINARY.
           10 WB-BOZB-12MON-UMSATZ        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-12MON-EQF           PIC S9(03)V9(02) COMP-3.
           10 WB-BOZB-03MON-UMSATZ        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-EQF-ABZUG           PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-BOZB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 WB-BOZB-12M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-03M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-EQF-ABZ-UPFL        PIC S9(09)V9(02) COMP-3.
           10 WB-BOZB-EH-ABZ-NEG          PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-03M-KORRIG          PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-03M-KORR-UPFL       PIC S9(09)V9(01) COMP-3.
           10 WB-BOZB-EH-ANTEIL           PIC S9(03)V9(02) COMP-3.
           10 WB-BOZB-EH-ANT-UPFL         PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

