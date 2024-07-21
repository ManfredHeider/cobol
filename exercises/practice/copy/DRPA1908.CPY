      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1908.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1908.CPv  $
      *
      *   Rev 2.1   Sep 20 2018 16:30:50   t13ae37
      *APA02694: Zugriff auf GEL-Kennung für Abrechnung
      *
      *   Rev 2.0   Aug 20 2018 11:11:56   T13AE37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1908                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1908                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02694!20.08.18! 2.1! NEU                       ! BUCH      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *
      *    INDIVIDUELLES DB2-LESE-OZP FUER DIE TABELLE TBPA0113
      *
      *    FUNKTION DRPA1908:
      *    LESEN PMBE-GEL-KENNUNG ANHAND EINER PDPT-ID (FÜR ABRECHNUNG)
      *
      ****************************************************************
      *
        01 PI-DRPA1908.
           05 PI-DRPA1908-DATEN.
              10 PI-DRPA1908-PGEB-GES           PIC S9(004) BINARY.
              10 PI-DRPA1908-PDPT-ID            PIC S9(015) COMP-3.
      *
        01 PO-DRPA1908.
           05 PO-DRPA1908-DATEN.
             10 PO-DRPA1908-PMBE-GEL-KENNUNG    PIC  X(006).
           05 PO-DRPA1908-RC                    PIC  X(003).
              88 PO-DRPA1908-RC-OK                   VALUE 'OK '.
              88 PO-DRPA1908-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************

