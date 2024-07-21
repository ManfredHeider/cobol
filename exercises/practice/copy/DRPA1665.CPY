      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1665.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1665.CPv  $
      *
      *   Rev 2.1   Dec 10 2007 13:41:00   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Jul 09 2007 15:25:52   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1665  FUNKTION KL AUS STRUKTUR LÖSCHEN   *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    09.07.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!09.07.07!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1665                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1665.
          05 PI-DRPA1665-PZVP-VERSION        PIC S9(004)  BINARY.
      *
       01 PO-DRPA1665.
          05 PO-DRPA1665-RC                  PIC  X(003).
             88 PO-DRPA1665-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1665-RC-NOT-CHANGED                VALUE 'NCH'.
          05 PO-DRPA1665-DATEN.
             10 PO-DRPA1665-STRK-KZ          PIC  X(001).
      *
      ******** ENDE COPY-MEMBER **************************************

