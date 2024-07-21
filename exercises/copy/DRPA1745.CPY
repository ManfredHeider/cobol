      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1745.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1745.CPv  $
      *
      *   Rev 2.3   Sep 02 2016 12:38:10   t13ex24
      *APA02151: BasisRente verteilte Abschlussprovisionen
      *
      *   Rev 2.2   Jan 13 2014 10:23:10   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.1   Apr 11 2013 14:04:46   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.0   Apr 08 2013 10:27:28   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1745  FUNKTION                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    09.04.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!09.04.13! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA02151!22.08.16! 2.3! NEUES FELD VERTEILTE PROV.! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1745                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1745.
           05 PI-DRPA1745-BEW-SCHL     PIC  X(006).
           05 PI-DRPA1745-PRODART      PIC  X(001).
           05 PI-DRPA1745-DYNKZ2       PIC  X(001).
220816     05 PI-DRPA1745-AE-KZ        PIC  X(001).
           05 PI-DRPA1745-UNR          PIC S9(004)       BINARY.
           05 PI-DRPA1745-SCHICHT-NR   PIC S9(004)       BINARY.
           05 PI-DRPA1745-VVG-KZ       PIC  X(001).
           05 PI-DRPA1745-NEUST-KZ     PIC  X(001).
           05 PI-DRPA1745-PROV-SCHL    PIC  X(005).
           05 PI-DRPA1745-GES-PROV     PIC S9(009)V9(02) PACKED-DECIMAL.
      *
        01 PO-DRPA1745.
           05 PO-DRPA1745-RC           PIC  X(003).
              88 PO-DRPA1745-RC-OK     VALUE 'OK '.
              88 PO-DRPA1745-RC-NFD    VALUE 'NFD'.
           05 PO-DRPA1745-PGV          PIC S9(004)       BINARY.
 

