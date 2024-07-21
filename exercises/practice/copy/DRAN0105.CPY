      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0105.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0105.CPv  $
      *
      *   Rev 2.1   08 Sep 2006 09:40:34   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   Nov 08 2004 15:47:16   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0105  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00685!08.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0105                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PZMA TBAN0007              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0105.
           05 PI-DRAN0105-PZMB-KEY.
               10 PI-DRAN0105-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0105-PZM-HVANTNUM    PIC X(9).
               10 PI-DRAN0105-PZM-VDABVERM    PIC S9(9) COMP.
           05 PI-DRAN0105-PZMB-DATEN.
               10 PI-DRAN0105-PZM-PIN         PIC S9(9) COMP.
               10 PI-DRAN0105-PZM-KENNUNG     PIC X(8).
      *
       01  PO-DRAN0105.
           05 PO-DRAN0105-RC                   PIC  X(003).
             88 PO-DRAN0105-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0105-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0105-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
