      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0311.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0311.CPv  $
      *
      *   Rev 2.1   Oct 13 2016 14:02:24   t13ae34
      *AAN01292: Elektra Rundlauf für Anträge
      *
      *   Rev 2.0   Oct 12 2016 11:50:20   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0311                                     *
      *    FUNKTION   : Ermitteln Antragsnummer für Rundlauf KI-EG-VBI
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    13.10.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01292!13.10.16!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0311                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0311.
          05 PI-DRAN0311-DATEN.
             10 PI-DRAN0311-PGEB-GES             PIC S9(004) COMP.
      *
       01 PO-DRAN0311.
          05 PO-DRAN0311-RC                      PIC  X(003).
             88 PO-DRAN0311-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0311-RC-NFD                    VALUE 'NFD'.
          05 PO-DRAN0311-DATEN.
             10 PO-DRAN0311-ANTA-NR              PIC S9(009) COMP.
      ******** ENDE COPY-MEMBER **************************************

