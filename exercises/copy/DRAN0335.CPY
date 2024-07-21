      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0335.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0335.CPv  $
      *
      *   Rev 2.2   Oct 24 2018 10:06:34   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.1   Oct 22 2018 16:00:14   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.0   Oct 22 2018 15:48:14   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0335  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01393!22.10.18!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0335                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE ANVH-GEDRUCKT = J >>> ANVH TBAN0018           *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0335.
          05 PI-DRAN0335-DATEN.
             10 PI-DRAN0335-PGEB-GES             PIC S9(4) COMP.
             10 PI-DRAN0335-ANVH-HVANTNUM        PIC  X(17).
      *
       01 PO-DRAN0335.
          05 PO-DRAN0335-RC                      PIC  X(3).
             88 PO-DRAN0335-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0335-RC-NOT-FOUND              VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************


