      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0128.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0128.CPv  $
      *
      *   Rev 2.2   22 Dec 2006 20:22:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 10:30:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:26:52   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0128  FUNKTION UPDATE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0128                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE ANTP TBAN0009              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0128.
          05 PI-DRAN0128-DATEN.
             10 PI-DRAN0128-LAST-UPDATE             PIC  X(026).
             10 PI-DRAN0128-ANTRAG-DATEN.
                15 PI-DRAN0128-ANTRAG-PRODUKT.
                   COPY DRAN0143.
      *
       01 PO-DRAN0128.
          05 PO-DRAN0128-RC                         PIC  X(003).
             88 PO-DRAN0128-RC-OK                          VALUE 'OK '.
             88 PO-DRAN0128-RC-CHANGED                     VALUE 'CHA'.
             88 PO-DRAN0128-RC-NOT-FOUND                   VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
