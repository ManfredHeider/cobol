      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0131.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0131.CPv  $
      *
      *   Rev 2.2   22 Dec 2006 20:29:56   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 10:42:22   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:27:24   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0131  FUNKTION SELECT                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!14.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00655!22.12.06!2.2 ! PK ÄNDERUNG               ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0131                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTP TBAN0009              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0131.
          05 ANTP-KEY.
             10 PGEB-GES                         PIC S9(4)   COMP.
             10 ANTP-NR                          PIC S9(15)V COMP-3.
             10 PROD-PROD-SCHL                   PIC S9(4) COMP.
             10 ANTP-LFD-NR                      PIC S9(4) COMP.
      *
       01 PO-DRAN0131.
          05 PO-DRAN0131-RC                      PIC  X(003).
             88 PO-DRAN0131-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0131-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0131-RC-KEY-NOT-UNIQUE         VALUE 'KNU'.
      *
          05 PO-DRAN0131-ANTRAG-DATEN.
             15 ANTP-LAST-UPDATE                 PIC  X(026).
             15 ANTP-KENNUNG                     PIC  X(008).
             15 PO-DRAN0131-ANTRAG-PRODUKT.
                COPY DRAN0143.

      ****** ENDE COPY-MEMBER ****************************************

