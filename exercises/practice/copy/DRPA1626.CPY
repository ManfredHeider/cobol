      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1626.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1626.CPv  $
      *
      *   Rev 2.1   Oct 15 2004 11:55:48   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 15 2004 09:14:00   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1626  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    12.10.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00295!12.10.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1626                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<      TABELLE PVKB  TBPA0135              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1626.
           05 PI-DRPA1626-UFKT         PIC  9(002).
           05 PI-DRPA1626-PVKB-DATEN.
              10 PI-DRPA1626-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1626-PRINTE    PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1626-PVKB-GUELTIG-AB
                                       PIC  X(026).
              10 PI-DRPA1626-PVKB-KD-BETREUER
                                       PIC S9(009)       BINARY.
              10 PI-DRPA1626-PVKB-F-GUELTIG-AB
                                       PIC  X(010).
              10 PI-DRPA1626-PVKB-DTA-KZ
                                       PIC  X(001).
              10 PI-DRPA1626-PVKB-LAST-UPDATE
                                       PIC  X(026).
              10 PI-DRPA1626-PVKB-KENNUNG
                                       PIC  X(008).
      *
        01 PO-DRPA1626.
           05 PO-DRPA1626-RC           PIC  X(003).
              88 PO-DRPA1626-RC-OK          VALUE 'OK '.
              88 PO-DRPA1626-RC-DUPLICATE-VALUE
                                            VALUE 'DVA'.
