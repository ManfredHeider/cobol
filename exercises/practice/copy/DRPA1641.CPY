      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1641.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1641.CPv  $
      *
      *   Rev 2.1   Dec 17 2004 10:02:28   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Nov 30 2004 10:42:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1641  FUNKTION UPDATE STANDARD-OZP       *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1641                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<      TABELLE PASA  TBPA0003              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1641.
           05 PI-DRPA1641-KEY.
              10 PI-DRPA1641-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1641-PASA-ERSTELLT
                                       PIC  X(026).
           05 PI-DRPA1641-PASA-DATEN.
              10 PI-DRPA1641-PASA-VORHALTZEIT
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1641-PRINTE-VON
                                       PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1641-PRINTE-BIS
                                       PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1641-PASA-PRIO PIC S9(004)       BINARY.
              10 PI-DRPA1641-PASA-STATUS
                                       PIC  X(001).
              10 PI-DRPA1641-PASA-START
                                       PIC  X(026).
              10 PI-DRPA1641-PASA-ENDE PIC  X(026).
      *
        01 PO-DRPA1641.
           05 PO-DRPA1641-RC           PIC  X(003).
              88 PO-DRPA1641-RC-OK          VALUE 'OK '.
              88 PO-DRPA1641-RC-NOT-FOUND   VALUE 'NFD'.
