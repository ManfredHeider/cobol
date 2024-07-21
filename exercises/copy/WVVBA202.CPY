      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA202.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:37:26   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA088, ZVVBA202
      *
      *   Rev 2.0   Apr 21 2016 13:43:58   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA202                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA202 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    22.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!22.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA202                           *
      *****************************************************************
 
        05 WB-NI-VIVBA202-TAB.
           10 WB-NI-VIVBA202-ELE   PIC S9(04)  BINARY OCCURS  31.
        05 WB-NI-VIVBA202          REDEFINES   WB-NI-VIVBA202-TAB.
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
           10 WB-NI-BOZ-12M-UMS-UPFL      PIC S9(04)       BINARY.
           10 WB-NI-BOZ-03M-UMS-UPFL      PIC S9(04)       BINARY.
           10 WB-NI-EH-PRO-ABZUG-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-EH-N-PRO-ABZ-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-BOZ-ZW-BETRAG-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-EQF-ABZUG-UPFL        PIC S9(04)       BINARY.
           10 WB-NI-BOZ-BETRAG-UPFL       PIC S9(04)       BINARY.
           10 WB-NI-EH-ABZUG-NEGATIV      PIC S9(04)       BINARY.
           10 WB-NI-BOZ-03M-KORRIGIERT    PIC S9(04)       BINARY.
           10 WB-NI-BOZ-03M-KORR-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-EH-ANTEIL             PIC S9(04)       BINARY.
           10 WB-NI-EH-ANTEIL-UPFL        PIC S9(04)       BINARY.
 
        05 WB-VIVBA202.
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
           10 WB-BOZ-12M-UMS-UPFL         PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-03M-UMS-UPFL         PIC S9(09)V9(01) COMP-3.
           10 WB-EH-PRO-ABZUG-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-EH-N-PRO-ABZ-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-ZW-BETRAG-UPFL       PIC S9(09)V9(02) COMP-3.
           10 WB-EQF-ABZUG-UPFL           PIC S9(09)V9(02) COMP-3.
           10 WB-BOZ-BETRAG-UPFL          PIC S9(09)V9(02) COMP-3.
           10 WB-EH-ABZUG-NEGATIV         PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-03M-KORRIGIERT       PIC S9(09)V9(01) COMP-3.
           10 WB-BOZ-03M-KORR-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-EH-ANTEIL                PIC S9(03)V9(02) COMP-3.
           10 WB-EH-ANTEIL-UPFL           PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

