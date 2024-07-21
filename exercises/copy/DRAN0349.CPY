      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0349.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0349.CPv  $
      *
      *   Rev 2.3   Dec 18 2018 16:12:24   t13ae23
      *FAN01150: DVAG-Wien EGES: Geburtsdatum für Kunden 2 - 5 mit 
      *inkorrekter Jahreszahl
      *
      *   Rev 2.2   Dec 07 2018 10:15:44   t13ae34
      *AAN01403: Papieranträge Deutsche Bank und Fondsdepotbank: Es
      * sollen zu allen Anträgen Host-Antragsdaten (DFÜ-Daten) gesc
      *rden
      *
      *   Rev 2.1   Dec 07 2018 10:09:04   t13ae34
      *AAN01403: Papieranträge Deutsche Bank und Fondsdepotbank: Es
      * sollen zu allen Anträgen Host-Antragsdaten (DFÜ-Daten) gesc
      *rden
      *
      *   Rev 2.0   Dec 04 2018 15:57:40   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0349                                     *
      *    FUNKTION   : ERM. AGI-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR AGI                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01403!05.12.18!2.1  !NEU                        !HEIDER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0349                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0349.
           05 PI-DRAN0349-AGI-KEY.
               10 PI-DRAN0349-PGEB-GES         PIC X(2).
               10 PI-DRAN0349-AGI-HVANTNUM     PIC X(9).
      *
       01 PO-DRAN0349.
          05 PO-DRAN0349-RC                    PIC   X(003).
             88 PO-DRAN0349-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0349-RC-NOK                   VALUE 'NOK'.
          05 PO-DRAN0349-DATEN.
      *  OUTPUT DATA FOR MAP BAN0034
             10 PO-DRAN0349-VBNRO              PIC X(7).
             10 PO-DRAN0349-PRZVBO             PIC X(1).
             10 PO-DRAN0349-ZNAMEO             PIC X(26).
             10 PO-DRAN0349-VNAMEO             PIC X(26).
             10 PO-DRAN0349-PLZO               PIC X(5).
             10 PO-DRAN0349-ORTO               PIC X(21).
             10 PO-DRAN0349-AUSO               PIC X(3).
             10 PO-DRAN0349-PRODKEY1O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY1O    PIC 9(7).
             10 PO-DRAN0349-ZWP1O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ1O           PIC X(4).
             10 PO-DRAN0349-PRODKEY2O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY2O    PIC 9(7).
             10 PO-DRAN0349-ZWP2O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ2O           PIC X(4).
             10 PO-DRAN0349-PRODKEY3O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY3O    PIC 9(7).
             10 PO-DRAN0349-ZWP3O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ3O           PIC X(4).
             10 PO-DRAN0349-PRODKEY4O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY4O    PIC 9(7).
             10 PO-DRAN0349-ZWP4O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ4O           PIC X(4).
             10 PO-DRAN0349-PRODKEY5O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY5O    PIC 9(7).
             10 PO-DRAN0349-ZWP5O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ5O           PIC X(4).
             10 PO-DRAN0349-PRODKEY6O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY6O    PIC 9(7).
             10 PO-DRAN0349-ZWP6O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ6O           PIC X(4).
             10 PO-DRAN0349-PRODKEY7O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY7O    PIC 9(7).
             10 PO-DRAN0349-ZWP7O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ7O           PIC X(4).
             10 PO-DRAN0349-PRODKEY8O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY8O    PIC 9(7).
             10 PO-DRAN0349-ZWP8O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ8O           PIC X(4).
             10 PO-DRAN0349-PRODKEY9O          PIC 9(4).
             10 PO-DRAN0349-SUMME-PRODKEY9O    PIC 9(7).
             10 PO-DRAN0349-ZWP9O              PIC X(1).
             10 PO-DRAN0349-ELAUFZ9O           PIC X(4).
             10 PO-DRAN0349-VDABLEINO          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0349-HANDO              PIC X(1).
             10 PO-DRAN0349-ANARTO             PIC X(1).
             10 PO-DRAN0349-ANDATO             PIC X(8).
             10 PO-DRAN0349-AFO-34C-VBNR       PIC S9(9) COMP.
             10 PO-DRAN0349-WKZO               PIC X(1).
             10 PO-DRAN0349-WKZTO              PIC X(3).
             10 PO-DRAN0349-SONSTIGE.
                15 PO-DRAN0349-PINO            PIC S9(9) COMP.
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0284.
      *
      ******** ENDE COPY-MEMBER **************************************



