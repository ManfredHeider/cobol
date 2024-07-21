      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1757.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1757.CPv  $
      *
      *   Rev 2.1   Aug 26 2013 08:36:56   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   Aug 20 2013 10:48:08   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1757                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  HAUPTARIF-PRODUKTSCHLÜSSEL ERMITTELN     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!20.08.13!2.0 ! NEU                       !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1757                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1757.
          03 PI-DRPA1757-PROGRAMM                PIC  X(008).
          03 PI-DRPA1757-PGEB-GES                PIC S9(4) USAGE COMP.
          03 PI-DRPA1757-PRINTE                  PIC S9(15)V COMP-3.
          03 PI-DRPA1757-PMBE-UNR                PIC S9(4) USAGE COMP.
          03 PI-DRPA1757-PMBE-SCHICHT-NR         PIC S9(4) USAGE COMP.

      *
       01 PO-DRPA1757.
          03 PO-DRPA1757-RC                      PIC  X(003).
             88 PO-DRPA1757-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1757-RC-NOT-FOUND              VALUE 'NFD'.
          03 PO-DRPA1757-HT-PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
      *
      ******* ENDE COPY-MEMBER DRPA1757 *****************************
