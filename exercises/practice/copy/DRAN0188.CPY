      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0188.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0188.CPv  $
      *
      *   Rev 2.1   May 15 2007 07:49:48   t13ae34
      *APA00604: MAH Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.0   May 14 2007 17:55:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0188 FUNKTION                             *
      *                 ÄNDERN DATEN WORKFLOW ANTRAG                  *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA00604!14.05.07!2.1 ! NEU                       ! HEIDER    *
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
       01 PI-DRAN0188.
          05 ANTA-KEY.
             10 PGEB-GES                         PIC S9(4)   COMP.
             10 ANTA-NR                          PIC S9(15)V COMP-3.
      *
       01 PO-DRAN0188.
          05 PO-DRAN0188-RC                      PIC  X(003).
             88 PO-DRAN0188-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0188-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0188-RC-CHANGED                VALUE 'CHA'.
