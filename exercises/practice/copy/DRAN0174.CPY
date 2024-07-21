      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0174.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0174.CPv  $
      *
      *   Rev 2.1   04 Dec 2006 14:01:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   13 Nov 2006 18:30:16   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0174                                     *
      *    FUNKTION   : PLAUSIBILITÄTEN MASKENFELDER ANTRAG ALLGEMEIN*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!15.11.06!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0174                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0174.
          05 PI-DRAN0174-DATEN.
             10 PI-DRAN0174-KTY-NR         PIC S9(004)        COMP.
             10 PI-DRAN0174-MASKENFELD     PIC  X(030).
             10 PI-DRAN0174-STICHTAG       PIC  X(010).
             10 PI-DRAN0174-PGEB-GES       PIC S9(004)        COMP.
             10 PI-DRAN0174-PROD-PROD-SCHL PIC S9(004)        COMP.
      *
       01 PO-DRAN0174.
          05 PO-DRAN0174-RC                PIC  X(003).
             88 PO-DRAN0174-RC-OK               VALUE 'OK '.
             88 PO-DRAN0174-RC-NOK              VALUE 'NOK'.
          05 PO-DRAN0174-DATEN.
             10 PO-DRAN0174-MASKENFELD     PIC  X(030).
             10 PO-DRAN0174-FELD-NUM       PIC S9(015)V9(003) COMP-3.
             10 PO-DRAN0174-FELD-NUM-2     PIC S9(015)V9(003) COMP-3.
             10 PO-DRAN0174-DATUM          PIC  X(010).
             10 PO-DRAN0174-FELD-ALPHANUM  PIC  X(030).
             10 PO-DRAN0174-KLARTEXT       PIC  X(079).
             10 PO-DRAN0174-HINWEISTEXT    PIC  X(079).
      ******** ENDE COPY-MEMBER **************************************
