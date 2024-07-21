      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1642.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1642.CPv  $
      *
      *   Rev 2.1   Dec 17 2004 10:02:36   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Nov 30 2004 10:43:02   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1642  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA1642                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL  <<<  TABELLE PASA  TBPA0003              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1642.
           05 PI-DRPA1642-KEY.
              10 PI-DRPA1642-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1642-PASA-ERSTELLT
                                       PIC  X(026).
      *
        01 PO-DRPA1642.
           05 PO-DRPA1642-RC           PIC  X(003).
              88 PO-DRPA1642-RC-OK          VALUE 'OK '.
              88 PO-DRPA1642-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1642-PASA-DATEN.
              10 PO-DRPA1642-PASA-VORHALTZEIT
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1642-PRINTE-VON
                                       PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1642-PRINTE-BIS
                                       PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1642-PASA-PRIO PIC S9(004)       BINARY.
              10 PO-DRPA1642-PASA-STATUS
                                       PIC  X(001).
              10 PO-DRPA1642-PASA-START
                                       PIC  X(026).
              10 PO-DRPA1642-PASA-ENDE PIC  X(026).
              10 PO-DRPA1642-PASA-KENNUNG
                                       PIC  X(008).
