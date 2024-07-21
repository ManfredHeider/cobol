      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0252.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0252.CPv  $
      *
      *   Rev 2.3   Oct 24 2018 10:06:14   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.2   Oct 22 2018 15:29:36   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.1   Nov 26 2009 14:34:04   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.0   Nov 05 2009 16:25:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0252  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0252                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE ANVH TBAN0018              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0252.
           05 PI-DRAN0252-DATEN.
             10 PI-DRAN0252-PGEB-GES          PIC S9(4) COMP.
             10 PI-DRAN0252-ANVH-HVANTNUM     PIC  X(17).
      *
       01  PO-DRAN0252.
           05 PO-DRAN0252-RC                  PIC  X(003).
             88 PO-DRAN0252-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0252-RC-NOT-FOUND           VALUE 'NFD'.
      ****** ENDE COPY-MEMBER ****************************************


