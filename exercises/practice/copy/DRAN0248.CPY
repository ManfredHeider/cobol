      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0248.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0248.CPv  $
      *
      *   Rev 2.1   Nov 26 2009 14:33:38   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.0   Oct 01 2009 11:28:58   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0248  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00896!01.10.09!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0123                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE POAA TBAN0003              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0248.
           05 PI-DRAN0248-KEY.
              10 PI-DRAN0248-PGEB-GES         PIC S9(4) COMP.
              10 PI-DRAN0248-POA-HVANTNUM     PIC X(9).
              10 PI-DRAN0248-POA-HVANTNUM-LFD PIC X(1).
      *
       01  PO-DRAN0248.
           05 PO-DRAN0248-RC                  PIC  X(003).
              88 PO-DRAN0248-RC-OK                  VALUE 'OK '.
              88 PO-DRAN0248-RC-CHANGED             VALUE 'CHA'.
              88 PO-DRAN0248-RC-NOT-FOUND           VALUE 'NFD'.

