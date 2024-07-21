      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1605.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1605.CPv  $
      *
      *   Rev 2.2   Oct 14 2004 14:41:06   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.1   Oct 14 2004 14:04:44   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Jul 16 2004 15:41:58   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1605  FUNKTION SELECT ZU GES. + PRINTE   *
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
      *    FÜR DIE FUNKTION DRPA1605                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL  <<<  TABELLE PVKB  TBPA0135              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1605.
           05 PI-DRPA1605-UFKT-CODE    PIC  9(002).
           05 PI-DRPA1605-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRPA1605-PRINTE       PIC S9(015)       PACKED-DECIMAL.
      *
        01 PO-DRPA1605.
           05 PO-DRPA1605-RC           PIC  X(003).
              88 PO-DRPA1605-RC-OK          VALUE 'OK '.
              88 PO-DRPA1605-RC-EOF         VALUE 'EOF'.
              88 PO-DRPA1605-RC-LEER        VALUE 'LER'.
              88 PO-DRPA1605-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1605-DATEN.
              10 PO-DRPA1605-PGEB-GES  PIC S9(004)       BINARY.
              10 PO-DRPA1605-PRINTE    PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1605-PVKB-GUELTIG-AB
                                       PIC  X(026).
              10 PO-DRPA1605-PVKB-KD-BETREUER
                                       PIC S9(009)       BINARY.
              10 PO-DRPA1605-PVKB-F-GUELTIG-AB
                                       PIC  X(010).
              10 PO-DRPA1605-PVKB-DTA-KZ
                                       PIC  X(001).
              10 PO-DRPA1605-PVKB-LAST-UPDATE
                                       PIC  X(026).
              10 PO-DRPA1605-PVKB-KENNUNG
                                       PIC  X(008).
