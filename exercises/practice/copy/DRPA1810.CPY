      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1810.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1810.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:06:34   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 01 2015 15:43:46   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1810  FUNKTION SELECT ALL STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * APA01932!01.04.15! 2.0! NEUERSTELLUNG             ! STRAUSS
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1810
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ALL <<<        PABR TBPA0138
      *
      *****************************************************************
      *
       01 PI-DRPA1810.
          05 PI-DRPA1810-PABR-KEY.
             10 PI-DRPA1810-PGEB-GES         PIC S9(004) USAGE COMP.
             10 PI-DRPA1810-PDPT-ID          PIC S9(015) COMP-3.
             10 PI-DRPA1810-PABR-VBNR        PIC S9(009) BINARY.
      *
          05 P0-DRPA1810-PABR-DATEN.
             10 PO-DRPA1810-PABR-ABRECH-NR   PIC  X(015).
             10 PO-DRPA1810-PDPT-ID-KORR     PIC S9(015) COMP-3.
             10 PO-DRPA1810-PABR-LAST-UPDATE PIC  X(026).
             10 PO-DRPA1810-PABR-KENNUNG     PIC  X(008).
      *
       01 PO-DRPA1810.
          05 PO-DRPA1810-RC                  PIC  X(003).
             88 PO-DRPA1810-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1810-RC-NOT-FOUND          VALUE 'NFD'.
      *
      *****************************************************************

