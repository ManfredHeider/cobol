      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0171.CPv  $
      *
      *   Rev 2.1   10 Nov 2006 13:03:08   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   09 Nov 2006 16:32:34   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0171  FUNKTION DELETE STANDARD-OZP
      *                 FÜR TBAN0011
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!10.11.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0171                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE ANTQ TBAN0011              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0171.
           05 PI-DRAN0171-LAST-UPDATE             PIC  X(026).
           05 ANTQ-KEY.
              10 PI-DRAN0171-PGEB-GES             PIC S9(4) COMP.
              10 PI-DRAN0171-PROD-PROD-SCHL       PIC S9(4) COMP.
              10 PI-DRAN0171-ANTQ-PLAUSI          PIC S9(4) COMP.
              10 PI-DRAN0171-ANTQ-GUELTIG-AB      PIC X(10).
      *
       01  PO-DRAN0171.
           05 PO-DRAN0171-RC                      PIC  X(003).
              88 PO-DRAN0171-RC-OK                     VALUE 'OK '.
              88 PO-DRAN0171-RC-CHANGED                VALUE 'CHA'.
              88 PO-DRAN0171-RC-NOT-FOUND              VALUE 'NFD'.

      ****** ENDE COPY-MEMBER ****************************************
