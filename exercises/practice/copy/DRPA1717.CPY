      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1717.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1717.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:54   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   May 20 2010 13:23:28   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1717  FUNKTION SELECT MELDEKENNZ. = N    *
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
      *    FÜR DIE FUNKTION DRPA1717                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MELDEKENNZEICHEN = N <<<                      *
      *         TABELLE PPIA TBPA1696                                *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1717.
          05 PPIA-KEY.
             10 PI-DRPA1717-PPIA-PGEB-GES       PIC S9(4)   COMP.
             10 PI-DRPA1717-PPIA-VERTRAGSNUM    PIC X(15).
      *
       01 PO-DRPA1717.
          05 PO-DRPA1717-RC                     PIC  X(003).
             88 PO-DRPA1717-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1717-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA1717-RC-KEY-NOT-UNIQUE        VALUE 'KNU'.
      *
      ****** ENDE COPY-MEMBER ****************************************

