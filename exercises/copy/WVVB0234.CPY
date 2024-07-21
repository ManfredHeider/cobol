      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0234.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0234.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 11:59:36   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Jul 29 2019 16:26:52   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0234                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0234 (FAPP)                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.07.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !31.07.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0234                          *
      *****************************************************************
 
        05 WB-NI-VIVB0234-TAB.
           10 WB-NI-VIVB0234-ELE   PIC S9(04)  BINARY OCCURS  12.
        05 WB-NI-VIVB0234          REDEFINES   WB-NI-VIVB0234-TAB.
           10 WB-NI-FAPP-LFDNR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-FAPP-BIS-STAT         PIC S9(04)       BINARY.
           10 WB-NI-FAPP-ART              PIC S9(04)       BINARY.
           10 WB-NI-FAPP-EQF              PIC S9(04)       BINARY.
           10 WB-NI-FAPP-EH-PROZ          PIC S9(04)       BINARY.
           10 WB-NI-FAPP-VB-VERTR         PIC S9(04)       BINARY.
           10 WB-NI-FAPP-BS               PIC S9(04)       BINARY.
           10 WB-NI-FAPP-MIN-PROZ         PIC S9(04)       BINARY.
           10 WB-NI-FAPP-STATUS           PIC S9(04)       BINARY.
           10 WB-NI-FAPP-LAST-UPDATE      PIC S9(04)       BINARY.
 
        05 WB-VIVB0234.
           10 WB-FAPP-LFDNR               PIC S9(04)       BINARY.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-FAPP-BIS-STAT            PIC  X(10).
           10 WB-FAPP-ART                 PIC  X(01).
           10 WB-FAPP-EQF                 PIC S9(03)V9(02) COMP-3.
           10 WB-FAPP-EH-PROZ             PIC S9(03)V9(02) COMP-3.
           10 WB-FAPP-VB-VERTR            PIC S9(04)       BINARY.
           10 WB-FAPP-BS                  PIC  X(01).
           10 WB-FAPP-MIN-PROZ            PIC S9(03)V9(02) COMP-3.
           10 WB-FAPP-STATUS              PIC  X(01).
           10 WB-FAPP-LAST-UPDATE         PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

