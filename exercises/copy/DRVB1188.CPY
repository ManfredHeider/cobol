      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.04.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1188.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1188.CPv  $
      *
      *   Rev 2.1   Dec 03 2018 13:58:00   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul für TBVB0131 (GBU)
      *
      *   Rev 2.0   Apr 13 2018 12:08:18   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1188                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0131                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!13.04.18!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1188 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1188.
          05 PI-DRVB1188-DATEN.
             10 PI-DRVB1188-NRDF-ABGEB-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1188-NRDF-AUFNEHM-NR    PIC S9(09)       BINARY.
             10 PI-DRVB1188-NRDF-DAT-VON       PIC  X(10).
 
       01 PO-DRVB1188.
         03 PO-DRVB1188-RC                     PIC  X(03).
             88 PO-DRVB1188-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1188-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1188-DATEN.
          05 PO-DRVB1188-NI-VIVB0131.
             10 NI-DRVB1188-NRDF-ABGEB-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1188-NRDF-AUFNEHM-NR    PIC S9(04)       BINARY.
             10 NI-DRVB1188-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1188-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1188-NRDF-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1188-NRDF-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1188-NRDF-UEBG-ZTPKT    PIC S9(04)       BINARY.
          05 PO-DRVB1188-DCL-VIVB0131.
             10 PO-DRVB1188-NRDF-ABGEB-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1188-NRDF-AUFNEHM-NR    PIC S9(09)       BINARY.
             10 PO-DRVB1188-KTY-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1188-KNR-NR-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1188-NRDF-DAT-VON       PIC  X(10).
             10 PO-DRVB1188-NRDF-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1188-NRDF-UEBG-ZTPKT    PIC  X(10).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1188   --------------------------*
      *---------------------------------------------------------------*

