      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0126.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WVVB0126.CPv  $
      *
      *   Rev 2.3   Dec 03 2018 10:57:38   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul Tabellen TBVB0126
      *
      *   Rev 2.2   Aug 16 2018 12:52:56   t13ex18
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.1   02 Aug 2018 16:18:26   t13ae13
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.0   Dec 01 2017 12:59:24   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0126                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0126 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!04.12.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0126                           *
      *****************************************************************
 
        05 WB-NI-VIVB0126-TAB.
           10 WB-NI-VIVB0126-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0126          REDEFINES   WB-NI-VIVB0126-TAB.
           10 WB-NI-NRB-ABGEB-NR          PIC S9(04)       BINARY.
           10 WB-NI-NRB-AUFNEHM-NR        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-NRB-PROJ-NR           PIC S9(04)       BINARY.
           10 WB-NI-NRB-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-NRB-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-NRB-LAST-UPDATE       PIC S9(04)       BINARY.
 
        05 WB-VIVB0126.
           10 WB-NRB-ABGEB-NR             PIC S9(09)       BINARY.
           10 WB-NRB-AUFNEHM-NR           PIC S9(09)       BINARY.
           10 WB-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 WB-NRB-PROJ-NR              PIC  X(10).
           10 WB-NRB-DAT-VON              PIC  X(10).
           10 WB-NRB-DAT-BIS              PIC  X(10).
           10 WB-NRB-LAST-UPDATE          PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
 
 
 

