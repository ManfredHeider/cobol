      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0059.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0059.CPv  $
      *
      *   Rev 2.2   28 Oct 2008 15:49:46   t13ex27
      *AKI00743: DTA-Kontrolle Teil 2
      *
      *   Rev 2.1   24 Jan 2007 13:34:16   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   15 Nov 2006 10:53:58   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: ZWOK0014                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZWOK0014                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
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
       01  PI-ZWOK0014.
           05  PI-ZWOK0014-FUNKTION            PIC  X(008).

             88   PI-ZWOK0014-DROK0060         VALUE 'DROK0060'.
140808       88   PI-ZWOK0014-DROK0045         VALUE 'DROK0045'.
140808       88   PI-ZWOK0014-DROK0046         VALUE 'DROK0046'.
140808       88   PI-ZWOK0014-DROK0047         VALUE 'DROK0047'.
140808       88   PI-ZWOK0014-DROK0048         VALUE 'DROK0048'.

      *
      ******* ENDE COPY-MEMBER **************************************
