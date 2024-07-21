      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1202.CPv  $
      *
      *   Rev 2.1   Dec 03 2018 10:58:22   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul Tabellen TBVB0126
      *
      *   Rev 2.0   Sep 12 2018 14:43:32   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1202                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0126                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. Bruder                                        *
      *    DATUM:    12.09.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!12.09.18!2.1 ! NEU                       ! G. Bruder *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1202 - ERMITTELN SATZ AUS TABELLE ALIAS       *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1202.
          05 PI-DRVB1202-DATEN.
             10 PI-DRVB1202-NRB-ABGEB-NR       PIC S9(09)       BINARY.
             10 PI-DRVB1202-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB1202-TERMIN             PIC  X(10).
 
       01 PO-DRVB1202.
         03 PO-DRVB1202-RC                     PIC  X(03).
             88 PO-DRVB1202-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1202-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1202-DATEN.
          05 PO-DRVB1202-NI-VIVB0126.
             10 NI-DRVB1202-NRB-ABGEB-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1202-NRB-AUFNEHM-NR     PIC S9(04)       BINARY.
             10 NI-DRVB1202-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1202-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1202-NRB-PROJ-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1202-NRB-DAT-VON        PIC S9(04)       BINARY.
             10 NI-DRVB1202-NRB-DAT-BIS        PIC S9(04)       BINARY.
             10 NI-DRVB1202-NRB-LAST-UPDATE    PIC S9(04)       BINARY.
          05 PO-DRVB1202-DCL-VIVB0126.
             10 PO-DRVB1202-NRB-ABGEB-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1202-NRB-AUFNEHM-NR     PIC S9(09)       BINARY.
             10 PO-DRVB1202-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1202-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1202-NRB-PROJ-NR        PIC  X(10).
             10 PO-DRVB1202-NRB-DAT-VON        PIC  X(10).
             10 PO-DRVB1202-NRB-DAT-BIS        PIC  X(10).
             10 PO-DRVB1202-NRB-LAST-UPDATE    PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1202   --------------------------*
      *---------------------------------------------------------------*
 
 
 

