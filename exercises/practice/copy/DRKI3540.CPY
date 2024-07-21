      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3540.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3540.CPv  $
      *
      *   Rev 2.2   Feb 27 2019 09:53:34   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *TABELLEN
      *
      *   Rev 2.1   Jan 30 2019 11:34:58   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.0   Dec 17 2018 15:10:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3540                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0520                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01652!03.12.18!    ! Neuerstellung             ! who      *
      *         !        !    !                           !          *
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
       01  PI-ZXKI0520.
           05  PI-ZXKI0520-FUNKTION             PIC  X(008).
             88   PI-ZXKI0520-DRKI3541          VALUE 'DRKI3541'.
             88   PI-ZXKI0520-DRKI3623          VALUE 'DRKI3623'.
      *



