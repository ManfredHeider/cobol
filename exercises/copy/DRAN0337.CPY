      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0337.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0337.CPv  $
      *
      *   Rev 2.2   Oct 24 2018 15:34:04   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.1   Oct 23 2018 11:41:32   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.0   Oct 23 2018 08:27:56   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **********************************************************
      *    COPY-MEMBER: DRAN0337 FUNKTION LIST CURSOR
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01393!23.10.18!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0337                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST    <<<       TABELLE ANVH TBAN0018              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0337.
          05 PI-DRAN0337-PROGRAMM                 PIC  X(008).
          05 PI-DRAN0337-UFKT-CODE                PIC  9(002).
             88  PI-DRAN0337-LIST-FIRST                VALUE 01.
             88  PI-DRAN0337-LIST-NEXT                 VALUE 02.
             88  PI-DRAN0337-LIST-QUIT                 VALUE 03.

      *
       01 PO-DRAN0337.
          05 PO-DRAN0337-RC                       PIC  X(003).
             88 PO-DRAN0337-RC-OK                      VALUE 'OK '.
             88 PO-DRAN0337-RC-EOF                     VALUE 'EOF'.
             88 PO-DRAN0337-RC-LEER                    VALUE 'LER'.
          05 PO-DRAN0337-DATEN.
             10 PO-DRAN0337-PGEB-GES              PIC S9(4) USAGE COMP.
             10 PO-DRAN0337-ANVH-HVANTNUM         PIC X(17).
             10 PO-DRAN0337-ANVH-HINWEIS          PIC X(79).
             10 PO-DRAN0337-ANVH-KENNUNG          PIC X(8).
             10 PO-DRAN0337-ANVH-LAST-UPDATE      PIC X(26).
             10 PO-DRAN0337-ANVH-ANTA-NR       PIC S9(15)V USAGE COMP-3.
             10 PO-DRAN0337-ANVH-EA-DATEINAME     PIC X(50).
             10 PO-DRAN0337-ERSTELLZEITPUNKT      PIC X(26).
             10 PO-DRAN0337-ANVH-GEDRUCKT         PIC X(1).
             10 PO-DRAN0337-ANVH-ANTA-AVERM       PIC S9(9) USAGE COMP.
      *
      ******* ENDE COPY-MEMBER ****************************************

