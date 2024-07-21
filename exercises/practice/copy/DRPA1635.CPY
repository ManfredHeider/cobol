      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1635.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1635.CPv  $
      *
      *   Rev 2.1   Oct 21 2004 13:55:36   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 19 2004 09:00:34   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1635  FUNKTION LESEN KUNDENBETREUER      *
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
      *    FÜR DIE FUNKTION DRPA1635                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN KUNDENBETREUER ZU GES./VNR/DATUM  <<<          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1635.
           05 PI-DRPA1635-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRPA1635-VNR          PIC  X(015).
           05 PI-DRPA1635-GUELTIG-AB   PIC  X(010).
      *
        01 PO-DRPA1635.
           05 PO-DRPA1635-RC           PIC  X(003).
              88 PO-DRPA1635-RC-OK          VALUE 'OK '.
              88 PO-DRPA1635-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1635-DATEN.
              10 PO-DRPA1635-KD-BETREUER
                                       PIC  9(007).
