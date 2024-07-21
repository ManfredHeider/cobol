      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0147.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0147.CPv  $
      *
      *   Rev 2.2   Jul 30 2008 14:19:52   t13ae23
      *AAN00822: Einbau Elektrl. Datensatz in das Badenia-Sparkonto
      * (VP)
      *
      *   Rev 2.1   24 Oct 2006 13:54:54   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   11 Oct 2006 15:39:14   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0147 FUNKTION                             *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAN0004                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00822!01.08.08 2.1 ! NEU                       ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZXAN0004.
           05  PI-ZXAN0004-FUNKTION            PIC  X(008).
               88 PI-ZXAN0004-DRAN0150              VALUE 'DRAN0150'.
      *
      ******* ENDE COPY-MEMBER **************************************
