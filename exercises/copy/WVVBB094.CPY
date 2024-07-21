      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB094.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB094.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:35:44   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA093 u. ZVVBB094
      *
      *   Rev 2.0   Apr 19 2016 09:24:12   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB094.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/WVVBB094.CPv  $
      *
      *   Rev 2.0   Apr 19 2016 09:24:12   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB094                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB094 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    19.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!19.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
240322* VLA-4369!24.03.22!2.2 ! Felddefinitionen COMP-3   ! U. Heim   *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB094                           *
      *****************************************************************
 
        05 WB-NI-VIVBB094-TAB.
           10 WB-NI-VIVBB094-ELE   PIC S9(04)  BINARY OCCURS  25.
        05 WB-NI-VIVBB094          REDEFINES   WB-NI-VIVBB094-TAB.
           10 WB-NI-LZUB-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-LZUB-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-12MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-03MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANZ-MONATE       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-GES       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-ERF       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-SOLL      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-GES        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-ERF        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-SOLL       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-BESHH-STEIG      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-BES-STEIG        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-WERT-GRUPPE      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-QUALI-KZ         PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-ERF-KZ        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-12M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-03M-UMS-UPFL     PIC S9(04)       BINARY.
 
        05 WB-VIVBB094.
           10 WB-LZUB-ST-NR               PIC S9(09)       BINARY.
           10 WB-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-LZUB-STICHTAG            PIC  X(10).
           10 WB-LZUB-ANSPR-VOM           PIC  X(10).
           10 WB-LZUB-LAST-UPDATE         PIC  X(26).
           10 WB-LZUB-MS-NR               PIC S9(04)       BINARY.
           10 WB-LZUB-VBP-NR              PIC S9(04)       BINARY.
240322     10 WB-LZUB-12MON-UMSATZ        PIC S9(10)V9(01) COMP-3.
240322     10 WB-LZUB-03MON-UMSATZ        PIC S9(10)V9(01) COMP-3.
           10 WB-LZUB-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-LZUB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-GES          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-ERF          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-SOLL         PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-GES           PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-ERF           PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-SOLL          PIC S9(04)       BINARY.
           10 WB-LZUB-BESHH-STEIG         PIC S9(09)V9(02) COMP-3.
           10 WB-LZUB-BES-STEIG           PIC S9(09)V9(02) COMP-3.
           10 WB-LZUB-WERT-GRUPPE         PIC S9(04)       BINARY.
           10 WB-LZUB-QUALI-KZ            PIC  X(01).
           10 WB-LZUB-HH-ERF-KZ           PIC  X(01).
240322     10 WB-LZUB-12M-UMS-UPFL        PIC S9(10)V9(01) COMP-3.
240322     10 WB-LZUB-03M-UMS-UPFL        PIC S9(10)V9(01) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************

