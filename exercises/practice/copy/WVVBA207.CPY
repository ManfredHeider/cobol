      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA207.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA207.CPv  $
      *
      *   Rev 2.1   Mar 28 2011 10:39:52   t13ae36
      *AAS00952: Allfinanz Wettbewerbe 2011:DHL ; Erf.Eig; Erf.Gr
      *
      *   Rev 2.0   Feb 22 2011 13:17:40   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: WVVBA207                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA207 (LZA     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    16.02.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00952!16.02.11!2.1 ! NEU                       ! FRANKE    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA207                           *
      *****************************************************************

        05 WB-NI-VIVBA207-TAB.
           10 WB-NI-VIVBA207-ELE   PIC S9(04)  BINARY OCCURS  10.
        05 WB-NI-VIVBA207          REDEFINES   WB-NI-VIVBA207-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-LZA-MONAT             PIC S9(04)       BINARY.
           10 WB-NI-LZA-BEITRAG-AKT       PIC S9(04)       BINARY.
           10 WB-NI-LZA-BEITRAG-ALT       PIC S9(04)       BINARY.
           10 WB-NI-LZA-BESTAND-HIN       PIC S9(04)       BINARY.
           10 WB-NI-LZA-BESTAND-WEG       PIC S9(04)       BINARY.
           10 WB-NI-LZA-BESTHIN-1MON      PIC S9(04)       BINARY.
           10 WB-NI-LZA-BESTWEG-1MON      PIC S9(04)       BINARY.
           10 WB-NI-LZA-BEITRGRP-AKT      PIC S9(04)       BINARY.
           10 WB-NI-LZA-BEITRGRP-ALT      PIC S9(04)       BINARY.

        05 WB-VIVBA207.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-LZA-MONAT                PIC  X(10).
           10 WB-LZA-BEITRAG-AKT          PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BEITRAG-ALT          PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BESTAND-HIN          PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BESTAND-WEG          PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BESTHIN-1MON         PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BESTWEG-1MON         PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BEITRGRP-AKT         PIC S9(09)V9(02) COMP-3.
           10 WB-LZA-BEITRGRP-ALT         PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

