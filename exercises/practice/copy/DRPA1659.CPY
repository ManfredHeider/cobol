      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1659.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1659.CPv  $
      *
      *   Rev 2.4   Oct 23 2017 09:11:28   t13ae37
      *APA02492: 3. Statistik-Kategorie Folge Neu - ABU Copy
      *
      *   Rev 2.3   Jul 13 2015 08:57:06   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.2   Jun 10 2008 10:03:48   t13ex21
      *APA00712: FVD Stufe 1: Programmänderungen
      *
      *   Rev 2.1   Oct 06 2006 12:14:08   t13ex25
      *APA00495: Generali Versicherung
      *
      *   Rev 2.0   Oct 02 2006 11:43:18   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1659                                      *
      *    --------------------------------------------------------   *
      *    DATEIBESCHREIBUNG FÜR PMPA0045 (DATEN FÜR GESAMTPROTOKOLL  *
      *    KONTROLLSUMMEN JE AP-GESELLSCHAFT)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                   *
      *    DATUM:    06.05.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00712!06.05.08!2.1 ! NEU                       ! STORCH   *
      * APA01932!04.05.15!2.3 ! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02492!23.10.17!2.4 ! EINFÜHRUNG Folge-Neu      ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRPA1659.
           05  DRPA1659-PGEB-GES                 PIC 99.
           05  DRPA1659-KNR-MANDANT              PIC 99.
           05  DRPA1659-PROD-PROD-SCHL           PIC S9(04) BINARY.
           05  DRPA1659-BUCHDATUM.
               10 DRPA1659-BUCHDAT-TT            PIC 99.
               10 DRPA1659-BUCHDAT-MM            PIC 99.
               10 DRPA1659-BUCHDAT-JJ            PIC 99.
231017*    05  DRPA1659-PRODART                  PIC 9.
231017     05  DRPA1659-PRODART                  PIC 99.
           05  DRPA1659-KTOAP1                   PIC 9(7).
           05  DRPA1659-PROVAUSZ                 PIC S9(7)V99 COMP-3.
           05  DRPA1659-PROVRUEC                 PIC S9(7)V99 COMP-3.
040515     05  DRPA1659-PROVUST                  PIC S9(7)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 

