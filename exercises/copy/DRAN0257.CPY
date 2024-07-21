      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0257.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0257.CPv  $
      *
      *   Rev 2.1   Jul 05 2013 10:23:36   t13ae23
      *AAN01127: Erfassung Investanträge ohne §34f
      *
      *   Rev 2.0   Jul 03 2013 14:39:42   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0257 FUNKTION SELECT                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01127!05.07.13!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0257                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANWL TBAN0019              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0257.
          05 PI-DRAN0257-ANWL-KEY.
             10 PI-DRAN0257-VB-NUMMER              PIC S9(9)   COMP  .

      *
       01 PO-DRAN0257.
          05 PO-DRAN0257-RC                        PIC   X(003)        .
             88 PO-DRAN0257-RC-OK                  VALUE 'OK '         .
             88 PO-DRAN0257-RC-NOT-FOUND           VALUE 'NFD'         .
             88 PO-DRAN0257-RC-KEY-NOT-UNIQUE      VALUE 'KNU'         .
      *
      ****** ENDE COPY-MEMBER ****************************************

