      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0101.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0101.CPv  $
      *
      *   Rev 2.2   20 Nov 2006 16:54:46   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 27 2003 10:24:30   t13ae23
      *FAN00459 - Beheben Problematik gelöschte Datensätze -
      *
      *   Rev 2.0   Oct 22 2003 13:33:38   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN00101 FUNKTIONSAUFRUFE FÜR ZADD0005      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    22.10.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00459!27.10.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZADD0005.
           05  PI-ZADD0005-FUNKTION            PIC  X(008).
               88 PI-ZADD0005-DRAN0101-LESEN        VALUE 'DRAN0101'.
               88 PI-ZBUU0005-DRAN0102-LESEN        VALUE 'DRAN0102'.
               88 PI-ZBUU0005-DRAN0175-LESEN        VALUE 'DRAN0175'.

