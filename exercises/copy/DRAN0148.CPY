      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0148.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0148.CPv  $
      *
      *   Rev 2.2   04 Dec 2006 13:04:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   24 Oct 2006 13:55:10   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   11 Oct 2006 15:39:34   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0148 FUNKTION                             *
      *                 INSERT WORKFLOW ANTRAG ÜBER VERSNR.           *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00655!13.10.06!2.1 ! NEU                       ! RAHNER    *
      * AAN00655!28.11.06!2.2 ! PI ENTFERNT               ! RAHNER    *
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
       01 PI-DRAN0148.
          05 FILLER                              PIC  X(1).
      *
       01 PO-DRAN0148.
          05 PO-DRAN0148-RC                      PIC  X(003).
             88 PO-DRAN0148-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0148-RC-DUPLICATE-VALUE        VALUE 'DVA'.
