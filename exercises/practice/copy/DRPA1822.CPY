      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  30.06.2015
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2015 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1822.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:/MF/DRPA1822.CPV  $
      *
      *   REV 2.0   JUN 30 2015 13:16:56   T13AE37
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!30.06.15!2.1 ! NEUERSTELLUNG             ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1822                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1822.
          05 PI-DRPA1822-PROGRAMM            PIC  X(008).
          05 PI-DRPA1822-UFKT                PIC  9(002).
          05 PI-DRPA1822-DATEN.
             10 PI-DRPA1822-PGEB-GES         PIC S9(4) USAGE COMP.
             10 PI-DRPA1822-PDPT-ID          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1822.
          05 PO-DRPA1822-RC                  PIC  X(003).
             88 PO-DRPA1822-RC-OK             VALUE 'OK '.
             88 PO-DRPA1822-RC-LEER           VALUE 'LER'.
             88 PO-DRPA1822-RC-EOF            VALUE 'EOF'.
          05 PO-DRPA1822-DATEN.
             10 PO-DRPA1822-PGEB-GES         PIC S9(4) USAGE COMP.
             10 PO-DRPA1822-PDPT-ID          PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1822-PABR-VBNR        PIC S9(9) USAGE COMP.
             10 PO-DRPA1822-PABR-ABRECH-NR   PIC X(15).
             10 PO-DRPA1822-PDPT-ID-KORR     PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1822-PABR-LAST-UPDATE PIC X(26).
             10 PO-DRPA1822-PABR-KENNUNG     PIC X(8).
      *
      ***** ENDE-COPY-MEMBER *****************************************

