      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0519.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0519.CPv  $
      *
      *   Rev 2.1   Aug 09 2013 15:05:30   t13ae34
      *AAT00045: Unterprogramm Zeichenkettenverarbeitung
      *
      *   Rev 2.0   Feb 12 2013 13:23:16   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * COPY-MEMBER: DRAT0519                                         *
      *              FUNKTION UMLAUTE AUFLÖSEN                        *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAT00045!08.08.13!2.1 ! NEU                       ! HEIDER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
          01 PI-DRAT0519.
           05 PI-DRAT0519-DATEN.
            10 PI-DRAT0519-BETRAG         PIC  X(16).
      *        MAX. 2 NACHKOMMASTELLEN
            10 PI-DRAT0519-NK             PIC  9(1).
      *
          01 PO-DRAT0519.
           05 PO-DRAT0519-RC              PIC  X(3).
              88 PO-DRAT0519-RC-OK        VALUE 'OK '.
              88 PO-DRAT0519-RC-NUM       VALUE 'NUM'.
              88 PO-DRAT0519-RC-ZVK       VALUE 'ZVK'.
              88 PO-DRAT0519-RC-NVK       VALUE 'ZNK'.
           05 PO-DRAT0519-DATEN.
            10 PO-DRAT0519-BETRAG-X       PIC  X(16).
            10 PO-DRAT0519-BETRAG-LINKS   PIC  X(16).
            10 PO-DRAT0519-BETRAG-NUM     PIC S9(12)V99 COMP-3.

