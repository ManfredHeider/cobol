      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0050.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0050.CPv  $
      *
      *   Rev 2.2   24 Sep 2008 10:05:22   t13ex27
      *AKI00743: DTA-Kontrolle Teil 1
      *
      *   Rev 2.1   24 Jan 2007 13:33:40   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   Oct 26 2006 15:43:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: ZTOK0014                                   *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTOK0014                     *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.11.2006/10:41:25
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00473!14.08.08!2.2 ! DTA-Kontrolle für OKI     !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZTOK0014.
           05  PI-ZTOK0014-FUNKTION            PIC  X(008).


             88   PI-ZTOK0014-DROK0058     VALUE 'DROK0058'.
             88   PI-ZTOK0014-DROK0052     VALUE 'DROK0052'.
             88   PI-ZTOK0014-DROK0051     VALUE 'DROK0051'.
             88   PI-ZTOK0014-DROK0063     VALUE 'DROK0063'.
             88   PI-ZTOK0014-DROK0064     VALUE 'DROK0064'.
140808       88   PI-ZTOK0014-DROK0043     VALUE 'DROK0043'.
140808       88   PI-ZTOK0014-DROK0044     VALUE 'DROK0044'.

      *
      ******* ENDE COPY-MEMBER **************************************
