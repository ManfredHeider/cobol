      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1812.CPY $
      *    $Revision:   2.1  $
      *
      *    $LOG:   L:/MF/DRPA1812.CPV $
      *
      *   Rev 2.0   Oct 04 2016 13:25:08   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1812  FUNKTION SELECT ALL STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * APA02326!04.10.16! 2.0! NEUERSTELLUNG             ! BUCH
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1812
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT DRPA1812 <<< PABR TBPA0138
      *
      *****************************************************************
      *
       01 PI-DRPA1812.
          05 PI-DRPA1812-PABR-KEY.
             10 PI-DRPA1812-PGEB-GES         PIC S9(004) USAGE COMP.
             10 PI-DRPA1812-PDPT-ID-KORR     PIC S9(015) COMP-3.
             10 PI-DRPA1812-PABR-VBNR        PIC S9(009) BINARY.
      *
          05 P0-DRPA1812-PABR-DATEN.
             10 PO-DRPA1812-PABR-ABRECH-NR   PIC  X(015).
             10 PO-DRPA1812-PDPT-ID          PIC S9(015) COMP-3.
             10 PO-DRPA1812-PABR-LAST-UPDATE PIC  X(026).
             10 PO-DRPA1812-PABR-KENNUNG     PIC  X(008).
      *
       01 PO-DRPA1812.
          05 PO-DRPA1812-RC                  PIC  X(003).
             88 PO-DRPA1812-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1812-RC-NOT-FOUND          VALUE 'NFD'.
      *
      *****************************************************************
 

