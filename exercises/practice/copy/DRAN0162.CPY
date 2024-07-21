      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0162.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0162.CPv  $
      *
      *   Rev 2.3   01 Dec 2006 14:56:32   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   16 Nov 2006 10:34:24   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   06 Nov 2006 16:30:48   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   02 Nov 2006 13:20:40   t13ae23
      *Archiverstellung
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0162  FUNKTION LÖSCHEN STANDARD-OZP      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!07.11.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0162                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<                                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0162.
          05 PI-DRAN0162-DATEN.
             10 PI-DRAN0162-PGEB-GES                PIC S9(4) COMP.
             10 PI-DRAN0162-ANTA-NR                 PIC S9(15)V COMP-3.
      *
       01 PO-DRAN0162.
          05 PO-DRAN0162-RC                         PIC  X(003).
             88 PO-DRAN0162-RC-OK                          VALUE 'OK '.
             88 PO-DRAN0162-RC-NOT-FOUND                   VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
      *
