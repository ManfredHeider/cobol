      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1634.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1634.CPv  $
      *
      *   Rev 2.2   Nov 03 2004 10:30:42   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.1   Oct 18 2004 13:33:22   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 15 2004 15:12:40   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1634  FUNKTION ERSTELLEN RUECK-DTA       *
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
      *    FÜR DIE FUNKTION DRPA1634                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  ERSTELLEN RÜCK-DTA JE PRODUKTGEBER  <<<              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1634.
           05 PI-DRPA1634-UFKT         PIC  9(002).
           05 PI-DRPA1634-DATEN.
              10 PI-DRPA1634-PGEB-GES  PIC S9(004)       BINARY.
      *
        01 PO-DRPA1634.
           05 PO-DRPA1634-RC           PIC  X(003).
              88 PO-DRPA1634-RC-OK          VALUE 'OK '.
              88 PO-DRPA1634-RC-EOF         VALUE 'EOF'.
              88 PO-DRPA1634-RC-LEER        VALUE 'LER'.
           05 PO-DRPA1634-DATEN.
              10 PO-DRPA1634-PGEB-GES  PIC  9(002).
              10 PO-DRPA1634-VNR       PIC  X(015).
              10 PO-DRPA1634-KD-BETREUER
                                       PIC  9(007).
              10 PO-DRPA1634-PIN       PIC  9(007).
              10 PO-DRPA1634-KUNDE-NAME
                                       PIC  X(025).
              10 PO-DRPA1634-KUNDE-VNAME
                                       PIC  X(020).
