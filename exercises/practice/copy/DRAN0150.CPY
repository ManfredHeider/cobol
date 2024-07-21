      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0150.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0150.CPv  $
      *
      *   Rev 2.2   Jul 30 2008 14:19:58   t13ae23
      *AAN00822: Einbau Elektrl. Datensatz in das Badenia-Sparkonto
      * (VP)
      *
      *   Rev 2.1   24 Oct 2006 13:55:40   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   11 Oct 2006 15:40:08   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0150 FUNKTION                             *
      *    FUNKTIONSAUFRUF   LIST TBAN0004                            *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00822!01.08.08!2.1 ! NEU                       ! RAHNER    *
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
       01 PI-DRAN0150.
          05 PI-DRAN0150-PROGRAMM               PIC X(008).
          05 PI-DRAN0150-UFKT                   PIC 9(002).
          05 PI-DRAN0150-PGEB-GES               PIC S9(4)   COMP.
          05 PI-DRAN0150-TIMESTAMP              PIC X(26).
      *
       01 PO-DRAN0150.
          05 PO-DRAN0150-RC                     PIC  X(003).
             88 PO-DRAN0150-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0150-RC-EOF                   VALUE 'EOF'.
             88 PO-DRAN0150-RC-LEER                  VALUE 'LER'.
          05 PO-DRAN0150-DATEN.
             10 PO-DRAN0150-VIA-PIN             PIC S9(9) COMP.
             10 PO-DRAN0150-VIA-PIN2            PIC S9(9) COMP.
             10 PO-DRAN0150-VIA-LAST-UPDATE     PIC X(26).
             10 PO-DRAN0150-VIA-KENNUNG         PIC X(8).
      *     POA-DATEN AUS PC-DATEN
             COPY DRAN0063.
      *
