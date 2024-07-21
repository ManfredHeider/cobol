      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA115.CPv  $
      *
      *   Rev 2.1   Oct 15 2012 09:43:34   t13ae13
      *ASL00462: Versorgungswerk 2012 - VB32
      *
      *   Rev 2.0   Sep 26 2012 08:47:30   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA115                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA115 (VSWVERS)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Susanne Walb                                     *
      *    DATUM:    26.09.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00462!26.09.12!2.1 ! NEU                       ! Walb      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA115                          *
      *****************************************************************
        05  WB-NI-VIVBA115-TAB.
           10 WB-NI-VIVBA115-ELE   PIC S9(04)  BINARY OCCURS  6 TIMES.
        05  WB-NI-VIVBA115         REDEFINES   WB-NI-VIVBA115-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-VERS-NR               PIC S9(04)       BINARY.
           10 WB-NI-STATUS-KZ             PIC S9(04)       BINARY.
           10 WB-NI-VSWV-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-VERS-KAT          PIC S9(04)       BINARY.
           10 WB-NI-KNR-VERS-KAT          PIC S9(04)       BINARY.

        05  WB-VIVBA115.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-VERS-NR                  PIC S9(15)       COMP-3.
           10 WB-STATUS-KZ                PIC  X(02).
           10 WB-VSWV-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-VERS-KAT             PIC S9(04)       BINARY.
           10 WB-KNR-VERS-KAT             PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
