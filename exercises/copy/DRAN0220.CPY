      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0220.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0220.CPv  $
      *
      *   Rev 2.1   Nov 24 2008 14:02:54   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 19 2008 11:14:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0220  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0220                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0220.
           05 PI-DRAN0220-GENS-KEY.
               10 PI-DRAN0220-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0220-GENS-PIN        PIC S9(9) COMP.
      *
       01  PO-DRAN0220.
           05 PO-DRAN0220-RC                  PIC  X(003).
             88 PO-DRAN0220-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0220-RC-CHANGED             VALUE 'CHA'.
             88 PO-DRAN0220-RC-NOT-FOUND           VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

