      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1696.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1696.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:14   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 09 2010 11:07:50   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1696  FUNKTION SELECT PGEB_GES ,VNR      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1696                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PPIA TBPA1696              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1696.
          05 PPIA-KEY.
             10 PI-DRPA1696-PPIA-PGEB-GES       PIC S9(4)   COMP.
             10 PI-DRPA1696-PPIA-VERTRAGSNUM    PIC X(15).
      *
       01 PO-DRPA1696.
          05 PO-DRPA1696-RC                     PIC  X(003).
             88 PO-DRPA1696-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1696-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA1696-RC-KEY-NOT-UNIQUE        VALUE 'KNU'.
      *
          05 PO-DRPA1696-PPIA-DATEN.
             10 PO-DRPA1696-PGEB-GES            PIC S9(004) COMP.
             10 PO-DRPA1696-PPIA-VNR            PIC X(015).
             10 PO-DRPA1696-PPIA-PIN            PIC S9(009) COMP.
             10 PO-DRPA1696-PPIA-DTA-KZ         PIC X(001).
             10 PO-DRPA1696-PPIA-KENNUNG        PIC X(008).
             10 PO-DRPA1696-PPIA-LAST-UPDATE    PIC X(010).
      *
      ****** ENDE COPY-MEMBER ****************************************
