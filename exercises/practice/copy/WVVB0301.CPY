      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0301.CPv  $
      *
      *   Rev 2.1   Mar 25 2009 09:32:12   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 24 2009 14:45:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0301                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0301 (DETAILAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    24.03.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!24.03.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0301                          *
      *****************************************************************
        05  WB-NI-VIVB0301-TAB.
           10 WB-NI-VIVB0301-ELE   PIC S9(04)  BINARY OCCURS 13 TIMES.
        05  WB-NI-VIVB0301         REDEFINES   WB-NI-VIVB0301-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-MONAT        PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-UNTST        PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP      PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-LAST-UP      PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-MS-NR        PIC S9(04)       BINARY.
           10 WB-NI-UNTST-MS-NR           PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-EH-IST       PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-EH-SOLL      PIC S9(04)       BINARY.
           10 WB-NI-DETAILAS-ERF          PIC S9(04)       BINARY.

        05  WB-VIVB0301.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-DETAILAS-MONAT           PIC  X(10).
           10 WB-DETAILAS-UNTST           PIC S9(09)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KTY-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 WB-DETAILAS-LAST-UP         PIC  X(26).
           10 WB-DETAILAS-MS-NR           PIC S9(04)       BINARY.
           10 WB-UNTST-MS-NR              PIC S9(04)       BINARY.
           10 WB-DETAILAS-EH-IST          PIC S9(09)V9(01) COMP-3.
           10 WB-DETAILAS-EH-SOLL         PIC S9(09)V9(01) COMP-3.
           10 WB-DETAILAS-ERF             PIC  X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
