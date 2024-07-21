      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1627.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1627.CPv  $
      *
      *   Rev 2.1   Oct 15 2004 11:55:58   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 15 2004 09:14:18   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1627  FUNKTION DELETE STANDARD-OZP       *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1627                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PVKB  TBPA0135             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1627.
           05 PI-DRPA1627-PVKB-KEY.
              10 PI-DRPA1627-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1627-PRINTE    PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1627-PVKB-GUELTIG-AB
                                       PIC  X(026).
      *
        01 PO-DRPA1627.
           05 PO-DRPA1627-RC           PIC  X(003).
              88 PO-DRPA1627-RC-OK          VALUE 'OK '.
              88 PO-DRPA1627-RC-NOT-FOUND   VALUE 'NFD'.
