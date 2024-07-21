      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1581.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1581.CPv  $
      *
      *   Rev 2.2   May 14 2018 11:17:56   t13ae53
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * TTH
      *
      *   Rev 2.1   Oct 12 2004 08:11:34   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:41:40   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1581  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA1581                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMBEH TBPA0313             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1581.
           05 PI-DRPA1581-PMBE-KEY.
              10 PI-DRPA1581-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1581-PMBE-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1581-PMBE-VERSION
                                       PIC S9(004)       BINARY.
      *
        01 PO-DRPA1581.
           05 PO-DRPA1581-RC           PIC  X(003).
              88 PO-DRPA1581-RC-OK          VALUE 'OK '.
              88 PO-DRPA1581-RC-NOT-FOUND   VALUE 'NFD'.

