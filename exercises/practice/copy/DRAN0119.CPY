      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0119.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0119.CPv  $
      *
      *   Rev 2.2   27 Dec 2007 11:11:34   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.1   08 Sep 2006 09:41:56   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   19 Jul 2006 11:13:34   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0119  FUNKTION SELECT ÜBER GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00685!08.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!28.12.07!2.2 ! ALLFINANZ                 ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0119                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PZMB TBAN0007              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0119.
           05 PI-DRAN0119-PZMB-KEY.
               10 PI-DRAN0119-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0119-PZM-PIN         PIC S9(9) COMP.
               10 PI-DRAN0119-PZM-VDABVERM    PIC S9(9) COMP.
      *
       01  PO-DRAN0119.
           05 PO-DRAN0119-RC                   PIC  X(003).
             88 PO-DRAN0119-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0119-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0119-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0119-PZMB-DATEN.
               10 PO-DRAN0119-PZM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0119-PZM-KENNUNG     PIC X(8).

      *    PZM-DATEN AUS PC-DATEN
           COPY DRAN0113.

      ****** ENDE COPY-MEMBER ****************************************
