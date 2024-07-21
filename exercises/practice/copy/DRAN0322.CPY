      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0322.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0322.CPv  $
      *
      *   Rev 2.3   Jan 23 2018 17:20:02   T13AE34
      *AAN01355: Werte Zusatzprodukte AML aufgrund von TBAN0011 
      *
      *   Rev 2.2   Jan 22 2018 19:34:28   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.1   Jan 22 2018 19:15:20   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.0   Jan 22 2018 18:29:38   T13AE34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0322                                     *
      *    FUNKTION   : WIRD DAS PROUKT IN VBI SEPARAT AUSGEWIESEN?  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    22.01.2018                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01355!22.01.18!2.1 !NEU                        !HEIDER    *
      * AAN01355!23.01.18!2.2 !"SEPARAT AUSGEWIESEN.."!!  !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0322                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0322.
          05 PI-DRAN0322-DATEN.
             10 PI-DRAN0322-PGEB-GES       PIC S9(004) COMP.
             10 PI-DRAN0322-PROD-PROD-SCHL PIC S9(004) COMP.
      *
       01 PO-DRAN0322.
          05 PO-DRAN0322-RC              PIC  X(003).
             88 PO-DRAN0322-RC-OK             VALUE 'OK '.
             88 PO-DRAN0322-RC-NOK            VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************



