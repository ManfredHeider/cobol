      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1747.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1747.CPv  $
      *
      *   Rev 2.3   Sep 02 2016 12:38:16   t13ex24
      *APA02151: BasisRente verteilte Abschlussprovisionen
      *
      *   Rev 2.2   Jan 13 2014 10:23:22   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.1   Apr 11 2013 14:04:56   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.0   Apr 08 2013 10:27:38   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1747  FUNKTION                           *
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
      *    FÜR DIE FUNKTION DRPA1747                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1747.
           05 PI-DRPA1747-BEW-SCHL     PIC  X(006).
           05 PI-DRPA1747-PRODART      PIC  X(001).
           05 PI-DRPA1747-DYNKZ2       PIC  X(001).
220816     05 PI-DRPA1747-AE-KZ        PIC  X(001).
           05 PI-DRPA1747-UNR          PIC  X(002).
           05 PI-DRPA1747-SCHICHT-NR   PIC  X(002).
           05 PI-DRPA1747-VVG-KZ       PIC  X(001).
           05 PI-DRPA1747-NEUST-KZ     PIC  X(001).
           05 PI-DRPA1747-PROV-SCHL    PIC  X(005).
           05 PI-DRPA1747-GESPROV-KZ   PIC  X(001).
      *
        01 PO-DRPA1747.
           05 PO-DRPA1747-RC           PIC  X(003).
              88 PO-DRPA1747-RC-OK     VALUE 'OK '.
              88 PO-DRPA1747-RC-NFD    VALUE 'NFD'.
           05 PO-DRPA1747-PGV          PIC S9(004)       BINARY.
 

