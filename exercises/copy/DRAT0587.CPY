      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0587.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0587.CPv  $
      *
      *   Rev 2.2   Aug 09 2013 15:05:46   t13ae34
      *AAT00045: Unterprogramm Zeichenkettenverarbeitung
      *
      *   Rev 2.1   Aug 06 2013 08:40:48   t13ae34
      *AAT00045: Unterprogramm Zeichenkettenverarbeitung
      *
      *   Rev 2.0   Mar 21 2013 12:39:42   i13az40
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * COPY-MEMBER: DRAT0587                                         *
      *              FUNKTION UMLAUTE AUFLÖSEN                        *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAT00045!02.08.13!2.1 ! NEU                       ! HEIDER    *
      * AAT00045!08.08.13!2.2 ! PI-TEXT AUF 500 VERKLEIN. ! HEIDER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
          01 PI-DRAT0587.
           05 PI-DRAT0587-DATEN.
            10 PI-DRAT0587-TEXT           PIC  X(500).
      *
          01 PO-DRAT0587.
           05 PO-DRAT0587-RC              PIC  X(3).
              88 PO-DRAT0587-RC-OK        VALUE 'OK '.
              88 PO-DRAT0587-RC-LER       VALUE 'LER'.
           05 PO-DRAT0587-DATEN.
            10 PO-DRAT0587-TEXT           PIC  X(1000).
            10 PO-DRAT0587-TEXTLAENGE-PI  PIC  9(4)      COMP-3.
            10 PO-DRAT0587-TEXTLAENGE-PO  PIC  9(4)      COMP-3.

