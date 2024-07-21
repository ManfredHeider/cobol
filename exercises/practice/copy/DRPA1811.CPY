      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1811.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1811.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:06:42   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 01 2015 15:43:52   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1811  FUNKTION DELETE STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * APA01932!01.04.15! 2.0! NEUERSTELLUNG             ! STRAUSS
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1811
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<        PRKL TBPA0138
      *
      *****************************************************************
      *
       01 PI-DRPA1811.
          05 PI-DRPA1811-PABR-KEY.
             10 PI-DRPA1811-PGEB-GES           PIC S9(004) USAGE COMP.
             10 PI-DRPA1811-PDPT-ID            PIC S9(015) COMP-3.
             10 PI-DRPA1811-PABR-VBNR          PIC S9(009) BINARY.
      *
       01 PO-DRPA1811.
          05 PO-DRPA1811-RC                    PIC  X(003).
             88 PO-DRPA1811-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1811-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRPA1811-RC-NOT-FOUND            VALUE 'NFD'.
      *
      *****************************************************************

