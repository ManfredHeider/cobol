      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1893.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1893.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:34:06   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Dec 10 2018 13:41:04   t13ae37
      *APA02749: Einführung GC&C - Ersatz für Ventilproduktschlüsse
      *l 0091/0093 - Sourcen ABU
      *
      *   Rev 2.0   Jul 24 2018 13:52:44   T13EX44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1893  PEG-DATENSATZ FUER GES 12 GC&C
      *    --------------------------------------------------------
      ****************************************************************
      *    VERSION 2.1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02749!26.11.16!2.1 ! NEU                       !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      *ENDE-LOGBUCH                                                  *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    GC&C-DTA'S.                                               *
      *    AUFBAU ANALOG AMV,GEV,CKV UND BKK                         *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1893.
          05 PI-DRPA1893-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1893-DATEN.
             10 PI-DRPA1893-PEG-DATEN-GCC.
                COPY DRPA1894.
          05 PI-DRPA1893-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1893.
          05 PO-DRPA1893-RC                         PIC  X(003).
             88 PO-DRPA1893-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1893-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************
 


