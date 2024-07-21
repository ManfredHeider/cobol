      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0158.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0158.CPv  $
      *
      *   Rev 2.2   01 Dec 2006 14:56:22   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   16 Nov 2006 10:33:36   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Oct 2006 10:22:06   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0158  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!27.10.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0158                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE ANTS TBAN0010              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0158.
          05 PI-DRAN0158-DATEN.
             10 PI-DRAN0158-ANTS-DATEN.
                15 PI-DRAN0158-PGEB-GES             PIC S9(4) COMP.
                15 PI-DRAN0158-ANTS-PIN             PIC S9(9) COMP.
                15 PI-DRAN0158-ANTS-NR              PIC S9(15)V COMP-3.
                15 PI-DRAN0158-ANTS-FREI            PIC X.
                15 PI-DRAN0158-ANTS-AVERM           PIC S9(9) COMP.
      *
       01 PO-DRAN0158.
          05 PO-DRAN0158-RC                         PIC X(3).
             88 PO-DRAN0158-RC-OK                       VALUE 'OK '.
             88 PO-DRAN0158-RC-CHANGED                  VALUE 'CHA'.
             88 PO-DRAN0158-RC-NOT-FOUND                VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

