      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0115.CPv  $
      *
      *   Rev 2.1   08 Sep 2006 09:41:16   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   19 Jul 2006 11:13:02   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0115  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0115                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PZMA TBAN0115              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0115.
           05 PI-DRAN0115-PZMB-KEY.
               10 PI-DRAN0115-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0115-PZM-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0115.
           05 PO-DRAN0115-RC                  PIC  X(003).
             88 PO-DRAN0115-RC-OK             VALUE 'OK '.
             88 PO-DRAN0115-RC-CHANGED        VALUE 'CHA'.
             88 PO-DRAN0115-RC-NOT-FOUND      VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
