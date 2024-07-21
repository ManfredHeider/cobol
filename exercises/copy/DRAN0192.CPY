      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0192.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0192.CPv  $
      *
      *   Rev 2.2   Nov 21 2007 09:48:56   t13ae34
      *AAN00780: WEAN0009 - Erstellung TDS-Sätze aus Antragsbestand
      * DB2
      *
      *   Rev 2.1   Nov 15 2007 16:56:14   t13ae34
      *AAN00780: WEAN0009 - Erstellung TDS-Sätze aus Antragsbestand
      * DB2
      *
      *   Rev 2.0   Nov 14 2007 15:10:40   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0192 FUNKTION                             *
      *                 INSERT WORKFLOW ANTRAG ÜBER VERSNR.           *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00780!14.11.07!2.1 ! NEU                       ! HEIDER    *
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
       01 PI-DRAN0192.
          05 FILLER                              PIC  X(1).
      *
       01 PO-DRAN0192.
          05 PO-DRAN0192-RC                      PIC  X(003).
             88 PO-DRAN0192-RC-OK                     VALUE 'OK '.
