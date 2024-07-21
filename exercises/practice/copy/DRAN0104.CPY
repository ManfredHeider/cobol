      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0104.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0104.CPv  $
      *
      *   Rev 2.3   Oct 24 2013 11:01:36   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.2   04 Dec 2006 12:44:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Nov 26 2004 15:29:44   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.0   Oct 29 2004 11:41:24   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0104                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO UPAN0013              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: V. RAHNER                                        *
      *    DATUM:    29.10.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00529!26.11.04!2.1 ! NEU                       ! RAHNER   *
      * AAN00655!01.12.64!2.2 ! DRAN0179                  ! HEIDER   *
      *         !        !    !                           !          *
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
       01  PI-UPAN0013.
           05  PI-UPAN0013-FUNKTION            PIC  X(008).
               88 PI-UPAN0013-DRAN0053              VALUE 'DRAN0053'.
               88 PI-UPAN0013-DRAN0054              VALUE 'DRAN0054'.
               88 PI-UPAN0013-DRAN0055              VALUE 'DRAN0055'.
               88 PI-UPAN0013-DRAN0059              VALUE 'DRAN0059'.
               88 PI-UPAN0013-DRAN0179              VALUE 'DRAN0179'.
      *
      ******* ENDE COPY-MEMBER **************************************
