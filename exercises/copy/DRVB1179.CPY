      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1179.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1179.CPv  $
      *
      *   Rev 2.3   Dec 03 2018 10:58:12   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul Tabellen TBVB0126
      *
      *   Rev 2.2   Aug 16 2018 12:53:38   t13ex18
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.1   02 Aug 2018 16:26:04   t13ae13
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.0   Dec 04 2017 09:54:32   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1179                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0126                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!04.12.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1179 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1179.
          05 PI-DRVB1179-DATEN.
             10 PI-DRVB1179-NRB-ABGEB-NR       PIC S9(09)       BINARY.
             10 PI-DRVB1179-NRB-AUFNEHM-NR     PIC S9(09)       BINARY.
             10 PI-DRVB1179-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB1179-NRB-DAT-VON        PIC  X(10).
 
       01 PO-DRVB1179.
         03 PO-DRVB1179-RC                     PIC  X(03).
             88 PO-DRVB1179-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1179-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1179-DATEN.
          05 PO-DRVB1179-NI-VIVB0126.
             10 NI-DRVB1179-NRB-ABGEB-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1179-NRB-AUFNEHM-NR     PIC S9(04)       BINARY.
             10 NI-DRVB1179-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1179-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1179-NRB-PROJ-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1179-NRB-DAT-VON        PIC S9(04)       BINARY.
             10 NI-DRVB1179-NRB-DAT-BIS        PIC S9(04)       BINARY.
             10 NI-DRVB1179-NRB-LAST-UPDATE    PIC S9(04)       BINARY.
          05 PO-DRVB1179-DCL-VIVB0126.
             10 PO-DRVB1179-NRB-ABGEB-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1179-NRB-AUFNEHM-NR     PIC S9(09)       BINARY.
             10 PO-DRVB1179-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1179-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1179-NRB-PROJ-NR        PIC  X(10).
             10 PO-DRVB1179-NRB-DAT-VON        PIC  X(10).
             10 PO-DRVB1179-NRB-DAT-BIS        PIC  X(10).
             10 PO-DRVB1179-NRB-LAST-UPDATE    PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1179   --------------------------*
      *---------------------------------------------------------------*
 
 
 

