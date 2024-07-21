      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.04.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0131.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0131.CPv  $
      *
      *   Rev 2.1   Dec 03 2018 13:57:34   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul für TBVB0131 (GBU)
      *
      *   Rev 2.0   Apr 13 2018 12:07:52   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0131                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0131 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!13.04.18!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0131                           *
      *****************************************************************
 
        05 WB-NI-VIVB0131-TAB.
           10 WB-NI-VIVB0131-ELE   PIC S9(04)  BINARY OCCURS   7.
        05 WB-NI-VIVB0131          REDEFINES   WB-NI-VIVB0131-TAB.
           10 WB-NI-NRDF-ABGEB-NR         PIC S9(04)       BINARY.
           10 WB-NI-NRDF-AUFNEHM-NR       PIC S9(04)       BINARY.
           10 WB-NI-KTY-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-NRDF-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-NRDF-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-NRDF-UEBG-ZTPKT       PIC S9(04)       BINARY.
 
        05 WB-VIVB0131.
           10 WB-NRDF-ABGEB-NR            PIC S9(09)       BINARY.
           10 WB-NRDF-AUFNEHM-NR          PIC S9(09)       BINARY.
           10 WB-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 WB-NRDF-DAT-VON             PIC  X(10).
           10 WB-NRDF-LAST-UPDATE         PIC  X(26).
           10 WB-NRDF-UEBG-ZTPKT          PIC  X(10).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************

