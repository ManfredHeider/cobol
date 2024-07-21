      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0197.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0197.CPv  $
      *
      *   Rev 2.2   Nov 24 2008 14:20:06   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:08   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:34:00   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0197  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0197                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE GENL TBAN0012              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0197.
           05 PI-DRAN0197-GENL-KEY.
               10 PI-DRAN0197-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0197-GENL-PIN        PIC S9(9) COMP.
      *
       01  PO-DRAN0197.
           05 PO-DRAN0197-RC                  PIC  X(003).
             88 PO-DRAN0197-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0197-RC-CHANGED             VALUE 'CHA'.
             88 PO-DRAN0197-RC-NOT-FOUND           VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
