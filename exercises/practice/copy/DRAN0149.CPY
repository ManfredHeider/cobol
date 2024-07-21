      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0149.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0149.CPv  $
      *
      *   Rev 2.2   04 Dec 2006 13:10:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   24 Oct 2006 13:55:24   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   11 Oct 2006 15:39:50   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0149 FUNKTION                             *
      *                 ÄNDERN DATEN WORKFLOW ANTRAG                  *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00655!16.10.06!2.1 ! NEU                       ! RAHNER    *
      * AAN00655!01.12.06!2.2 ! PO-DRAN0149-RC-CHANGED    !           *
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
       01 PI-DRAN0149.
          05 ANTA-KEY.
             10 PGEB-GES                         PIC S9(4)   COMP.
             10 ANTA-NR                          PIC S9(15)V COMP-3.
      *
       01 PO-DRAN0149.
          05 PO-DRAN0149-RC                      PIC  X(003).
             88 PO-DRAN0149-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0149-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0149-RC-CHANGED                VALUE 'CHA'.
