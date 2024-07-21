      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0223.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0223.CPv  $
      *
      *   Rev 2.2   Sep 28 2009 08:12:12   t13ae34
      *FAN00905: Abbruch DRAN0190 im Dialogkontext
      *
      *   Rev 2.1   Apr 23 2009 10:16:32   t13ae34
      *AAN00882: Antrag
      *
      *   Rev 2.0   Apr 08 2009 11:43:28   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0223                                     *
      *    FUNKTION   : SETZEN ANTRAGSTATUS IM ANTRAGSBESTAND        *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00882!08.07.09!2.1 !NEU                        !HEIDER    *
      * FAN00905!28.09.09!2.2 !ANALOG DRAN0190            !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0223                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0223.
280909    05 FILLER                              PIC 9(01).
          05 PI-DRAN0223-KEY.
             10 PI-DRAN0223-PGEB-GES             PIC S9(004) COMP.
             10 PI-DRAN0223-ANTA-NR              PIC S9(15)V COMP-3.

          05 PI-DRAN0223-DATEN.
             10 PI-DRAN0223-ANTA-ANTR-STATUS-1   PIC S9(004) COMP.
      *
       01 PO-DRAN0223.
          05 PO-DRAN0223-RC                      PIC  X(003).
             88 PO-DRAN0223-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0223-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0223-RC-NO-UPDATE              VALUE 'NUP'.
          05 PO-DRAN0223-DATEN.
             10 FILLER                           PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************
