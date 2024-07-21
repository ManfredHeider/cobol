      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1739.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1739.CPv  $
      *
      *   Rev 2.1   Dec 18 2013 14:34:42   t13ae46
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO-Dialoge (PO11-PO13)
      *
      *   Rev 2.0   Mar 13 2013 15:15:42   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1739  FUNKTION                           *
      *                      GET  SAMMELNR UND RABSTUFE  (UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    13.03.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!13.03.13! 01  !NEUERSTELLUNG             ! WANG     *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1739                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1739.
          05 PI-DRPA1739-DATEN.
             10 PI-DRPA1739-PRINTE           PIC S9(015)V USAGE COMP-3.
      *
       01 PO-DRPA1739.
          05 PO-DRPA1739-RC                  PIC  X(003).
             88 PO-DRPA1739-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1739-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1739-MELDUNG.
             10 PO-DRPA1739-MELDUNG-TYP      PIC  X(001).
             10 PO-DRPA1739-MELDUNG-NR       PIC S9(004) BINARY.

          05 PO-DRPA1739-DATEN.
             10 PO-DRPA1739-SAMMEL-NR        PIC S9(009) BINARY.
             10 PO-DRPA1739-KNR-RABSTUFE     PIC S9(004) BINARY.
             10 PO-DRPA1739-RABSTUFE-BK      PIC  X(003).
