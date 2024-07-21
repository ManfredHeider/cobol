      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0435.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0435.CPv  $
      *
      *   Rev 2.1   Jun 10 2008 10:28:10   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.0   Mar 12 2008 09:06:50   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0435  KONSTANTEN FÜR                     *
      *                           VERTRIEBSGESELLSCHAFT    KTY = 131 *
      *                           (PRODUKT, TARIF)                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    12.03.2008                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00712!12.03.08!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-MANDANT-ALLE           PIC S9(004) BINARY VALUE 1.
           05 K-KNR-MANDANT-NUR-DVAG       PIC S9(004) BINARY VALUE 2.
           05 K-KNR-MANDANT-NUR-ALLF       PIC S9(004) BINARY VALUE 3.
           05 K-KNR-MANDANT-NUR-FVD        PIC S9(004) BINARY VALUE 4.
           05 K-KNR-MANDANT-DVAG-ALLF      PIC S9(004) BINARY VALUE 5.
           05 K-KNR-MANDANT-DVAG-FVD       PIC S9(004) BINARY VALUE 6.
           05 K-KNR-MANDANT-ALLF-FVD       PIC S9(004) BINARY VALUE 7.
      *
