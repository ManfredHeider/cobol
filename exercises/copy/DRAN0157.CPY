      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0157.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0157.CPv  $
      *
      *   Rev 2.1   16 Nov 2006 10:33:24   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Oct 2006 10:21:54   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0157  FUNKTION INSERT STANDARD-OZP
      *                 FÜR TBAN0010
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!27.10.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0157                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE ANTS TBAN0010              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0157.
          05 PI-DRAN0157-DATEN.
             10 PI-DRAN0157-ANTS-DATEN.
                15 PI-DRAN0157-PGEB-GES             PIC S9(4) COMP.
                15 PI-DRAN0157-ANTS-PIN             PIC S9(9) COMP.
                15 PI-DRAN0157-ANTS-NR              PIC S9(15)V COMP-3.
                15 PI-DRAN0157-ANTS-FREI            PIC X.
                15 PI-DRAN0157-ANTS-AVERM           PIC S9(9) COMP.

       01 PO-DRAN0157.
          05 PO-DRAN0157-RC                         PIC  X(003).
             88 PO-DRAN0157-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0157-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ***************************************
