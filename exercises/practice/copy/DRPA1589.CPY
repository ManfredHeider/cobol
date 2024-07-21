      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1589.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1589.CPv  $
      *
      *   Rev 2.1   Oct 12 2004 08:12:44   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:43:32   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1589  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1589                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMPTH TBPA0317             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1589.
           05 PI-DRPA1589-PMPT-KEY.
              10 PI-DRPA1589-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1589-PMPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1589-PMPT-VERSION
                                       PIC S9(004)       BINARY.
      *
        01 PO-DRPA1589.
           05 PO-DRPA1589-RC           PIC  X(003).
              88 PO-DRPA1589-RC-OK          VALUE 'OK '.
              88 PO-DRPA1589-RC-NOT-FOUND   VALUE 'NFD'.
      *---------------------------------------------------------------*
