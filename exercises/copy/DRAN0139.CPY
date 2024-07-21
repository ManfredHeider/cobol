      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0139.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRAN0139.CPv  $
      *
      *   Rev 2.10   Dec 23 2016 14:08:50   t13ae34
      *AAN01304: Erfassung eAnträge zum Monatsschluss
      *
      *   Rev 2.9   Nov 21 2007 09:48:52   t13ae34
      *AAN00780: WEAN0009 - Erstellung TDS-Sätze aus Antragsbestand
      * DB2
      *
      *   Rev 2.8   Nov 15 2007 16:54:40   t13ae34
      *AAN00780: WEAN0009 - Erstellung TDS-Sätze aus Antragsbestand
      * DB2
      *
      *   Rev 2.7   May 15 2007 09:16:54   t13ae34
      *APA00604: MAH Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.6   May 15 2007 08:22:16   t13ae34
      *APA00604: MAH Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.5   May 14 2007 18:17:42   t13ae34
      *APA00604: MAH Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.4   22 Dec 2006 20:37:48   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   16 Nov 2006 10:32:54   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   10 Oct 2006 15:25:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 06 2006 11:00:08   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 14 2006 16:41:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0139                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UWAN0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!10.10.06!2.1 ! NEU                       ! HEIDER   *
      * AAN00655!16.11.06!2.2 ! WEITERE FUNKTIONEN        ! RAHNER   *
      * AAN00655!21.11.06!2.3 ! DRAN0177                  ! HEIDER   *
      * AAN00655!22.12.06!2.4 ! ENTF. DRAN0150, DRAN0167  ! HEIDER   *
      * APA00604!14.05.07!2.5 ! DRAN0188                  ! HEIDER   *
      * AAN00780!14.11.07!2.8 ! DRAN0192                  ! HEIDER   *
      * AAN01304!23.12.16!2.9 ! DRAN0312                  ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UWAN0001.
           05  PI-UWAN0001-FUNKTION            PIC  X(008).
               88 PI-UWAN0001-DRAN0140              VALUE 'DRAN0140'.
               88 PI-UWAN0001-DRAN0147              VALUE 'DRAN0147'.
               88 PI-UWAN0001-DRAN0148              VALUE 'DRAN0148'.
               88 PI-UWAN0001-DRAN0149              VALUE 'DRAN0149'.
               88 PI-UWAN0001-DRAN0162              VALUE 'DRAN0162'.
               88 PI-UWAN0001-DRAN0188              VALUE 'DRAN0188'.
               88 PI-UWAN0001-DRAN0192              VALUE 'DRAN0192'.
               88 PI-UWAN0001-DRAN0312              VALUE 'DRAN0312'.
      *
      ******* ENDE COPY-MEMBER **************************************

