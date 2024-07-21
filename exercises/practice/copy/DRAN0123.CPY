      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0123.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0123.CPv  $
      *
      *   Rev 2.2   Sep 26 2006 10:09:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2006 16:22:16   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:26:02   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0123  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!12.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0123                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE ANTA TBAN0008              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0123.
           05 PI-DRAN0123-LAST-UPDATE         PIC  X(026).
           05 ANTA-KEY.
               10 PGEB-GES                    PIC S9(4)   COMP.
               10 ANTA-NR                     PIC S9(15)V COMP-3.
      *
       01  PO-DRAN0123.
           05 PO-DRAN0123-RC                  PIC  X(003).
             88 PO-DRAN0123-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0123-RC-CHANGED             VALUE 'CHA'.
             88 PO-DRAN0123-RC-NOT-FOUND           VALUE 'NFD'.

