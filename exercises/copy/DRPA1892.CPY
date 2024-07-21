      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1892.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1892.CPv  $
      *
      *   Rev 2.1   May 22 2018 13:02:16   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.0   May 17 2018 14:41:14   T13EX44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1892                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1892                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02579!24.04.18! 2.1! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *
      *    INDIVIDUELLES DB2-LESE-OZP FUER DIE TABELLE TBPA0007
      *
      *    FUNKTION DRPA1892:
      *    LESEN HAFTZEITDAUER-PRDUKTGEBER ZU GES+PROD+TERMIN
      *
      ****************************************************************
      *
        01 PI-DRPA1892.
           05 PI-DRPA1892-DATEN.
              10 PI-DRPA1892-PGEB-GES           PIC S9(004) BINARY.
              10 PI-DRPA1892-PROD-PROD-SCHL     PIC S9(004) BINARY.
              10 PI-DRPA1892-TERMIN             PIC  X(010).
      *
        01 PO-DRPA1892.
           05 PO-DRPA1892-DATEN.
             10 PO-DRPA1892-HAFTZEITDAUER-PG    PIC S9(004) BINARY.
           05 PO-DRPA1892-RC                    PIC  X(003).
              88 PO-DRPA1892-RC-OK                   VALUE 'OK '.
              88 PO-DRPA1892-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
 

