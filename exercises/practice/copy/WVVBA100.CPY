      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA100.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:45:32   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 25 2016 14:24:16   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA100                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA100 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    25.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!25.05.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA100                           *
      *****************************************************************
 
        05 WB-NI-VIVBA100-TAB.
           10 WB-NI-VIVBA100-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVBA100          REDEFINES   WB-NI-VIVBA100-TAB.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-KONTO           PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-DAT-BIS         PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-INFO            PIC S9(04)       BINARY.
           10 WB-NI-KTY-LEISTUNGSTYP      PIC S9(04)       BINARY.
           10 WB-NI-KNR-LEISTUNGSTYP      PIC S9(04)       BINARY.
           10 WB-NI-KTY-DATUM             PIC S9(04)       BINARY.
           10 WB-NI-KNR-DATUM             PIC S9(04)       BINARY.
           10 WB-NI-KTY-LEISTUNGSART      PIC S9(04)       BINARY.
           10 WB-NI-KNR-LEISTUNGSART      PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-STAB-RELEVAN    PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-UST-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-FIBUZ-ZUGEH-KONTO     PIC S9(04)       BINARY.
 
        05 WB-VIVBA100.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-FIBUZ-KONTO              PIC  X(12).
           10 WB-FIBUZ-DAT-VON            PIC  X(26).
           10 WB-FIBUZ-LAST-UPDATE        PIC  X(26).
           10 WB-FIBUZ-DAT-BIS            PIC  X(10).
           10 WB-FIBUZ-INFO               PIC  X(30).
           10 WB-KTY-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 WB-KNR-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 WB-KTY-DATUM                PIC S9(04)       BINARY.
           10 WB-KNR-DATUM                PIC S9(04)       BINARY.
           10 WB-KTY-LEISTUNGSART         PIC S9(04)       BINARY.
           10 WB-KNR-LEISTUNGSART         PIC S9(04)       BINARY.
           10 WB-FIBUZ-STAB-RELEVAN       PIC  X(01).
           10 WB-FIBUZ-UST-STATUS         PIC  X(01).
           10 WB-FIBUZ-ZUGEH-KONTO        PIC  X(12).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

