      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0321.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0321.CPv  $
      *
      *   Rev 2.2   Jan 22 2018 19:34:14   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.1   Jan 22 2018 19:15:06   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.0   Jan 22 2018 12:03:02   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0321                                     *
      *    FUNKTION   : IST DAS PRODUKT IM EINGEREICHTEN GESCHÄFT?   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    22.01.2018                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01355!22.01.18!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0321                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0321.
          05 PI-DRAN0321-DATEN.
             10 PI-DRAN0321-PGEB-GES       PIC S9(004) COMP.
             10 PI-DRAN0321-PROD-PROD-SCHL PIC S9(004) COMP.
      *
       01 PO-DRAN0321.
          05 PO-DRAN0321-RC              PIC  X(003).
             88 PO-DRAN0321-RC-OK             VALUE 'OK '.
             88 PO-DRAN0321-RC-NOK            VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************


