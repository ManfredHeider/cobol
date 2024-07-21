      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1220.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1220.CPv  $
      *
      *   Rev 2.2   Jan 13 2014 09:47:34   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   Apr 30 1999 15:36:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:06   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1220                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE ÜBER PGEB UND VNR>>         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    22.04.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.04.99!01.01! NEU                      ! STORCH   *
      * APA01526!16.10.13!02.02! ERWEITERUNG PO-BEREICH   ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1220                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1220.
          05 PI-DRPA1220-PROGRAMM            PIC  X(008).
          05 PI-DRPA1220-UFKT                PIC  9(002).
          05 PI-DRPA1220-DATEN.
             10 PI-DRPA1220-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1220-VNR              PIC  X(015).
      *
       01 PO-DRPA1220.
          05 PO-DRPA1220-RC                  PIC  X(003).
             88 PO-DRPA1220-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1220-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1220-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1220-DATEN.
             10 PMPG-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PMPG-VERSION                 PIC S9(004) BINARY.
             10 PMPG-LFD-NR-VNR              PIC S9(009) BINARY.
             10 PDTA-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PMPE-FAELLIG-VON             PIC  X(010).
             10 PMPE-FAELLIG-BIS             PIC  X(010).
             10 KNR-WAEHRUNG                 PIC S9(004) BINARY.
             10 PMPE-VSTAT                   PIC  X(004).
             10 PMPE-KUNDE-NAME              PIC  X(025).
             10 PMPE-PG-ABR-DAT              PIC  X(010).
161013       10 PMPE-UNR                     PIC S9(004) BINARY.
161013       10 PMPE-SCHICHT-NR              PIC S9(004) BINARY.

