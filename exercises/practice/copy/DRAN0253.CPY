      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0253.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0253.CPv  $
      *
      *   Rev 2.3   Oct 24 2018 10:06:24   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.2   Oct 22 2018 15:29:50   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.1   Nov 26 2009 14:34:10   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.0   Nov 05 2009 16:25:48   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0253  FUNKTION SELECT ÜBER GES, VORSCHLAG*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00896!06.11.09!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01393!16.10.18!2.2 ! TBAN0018 2.0              ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0253                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANVH TBAN0018              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0253.
          05 PI-DRAN0253-DATEN.
             10 PI-DRAN0253-PGEB-GES          PIC S9(4) COMP.
             10 PI-DRAN0253-ANVH-HVANTNUM     PIC  X(17).
      *
       01 PO-DRAN0253.
          05 PO-DRAN0253-RC                   PIC  X(003).
             88 PO-DRAN0253-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0253-RC-NOT-FOUND           VALUE 'NFD'.
      *
          05 PO-DRAN0253-DATEN.
             10 PO-DRAN0253-PGEB-GES          PIC S9(4) COMP.
             10 PO-DRAN0253-ANVH-HVANTNUM     PIC  X(17).
             10 PO-DRAN0253-ANVH-HINWEIS      PIC  X(79).
             10 PO-DRAN0253-ANVH-KENNUNG      PIC  X(8).
             10 PO-DRAN0253-ANVH-LAST-UPDATE  PIC  X(26).
             10 PO-DRAN0253-ANVH-ANTA-NR      PIC S9(15)V COMP-3.
             10 PO-DRAN0253-ANVH-EA-DATEINAME PIC  X(50).
             10 PO-DRAN0253-ERSTELLZEITPUNKT  PIC  X(26).
             10 PO-DRAN0253-ANVH-GEDRUCKT     PIC  X(1).
                88 PO-DRAN0253-ANVH-GEDRUCKT-JA    VALUE 'J'.
                88 PO-DRAN0253-ANVH-GEDRUCKT-NEIN  VALUE 'N'.
             10 PO-DRAN0253-ANVH-ANTA-AVERM   PIC S9(9) USAGE COMP.
      ****** ENDE COPY-MEMBER ****************************************

