      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1918.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1918.CPv  $
      *
      *   Rev 2.1   Oct 15 2019 07:50:28   t13ex67
      *APA02846: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB
      *
      *   Rev 2.0   Jun 04 2019 09:19:58   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1918  PEG-DATENSATZ VON                   *
      *                           PLANET IMMO (GES. 92)               *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH-SCHÄFER                                   *
      *    DATUM:    04.06.19                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02846!04.06.19!2.1 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    PLANET-IMMO DTA'S.                                        *
      *                                                              *
      ****************************************************************
      *
             30 PLANET-IMMO-DATENSATZ.
                35 PGEB-GES               PIC  9(002).
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 KUNDE-NNAME            PIC  X(035).
                35 KUNDE-VNAME            PIC  X(035).
                35 ABR-DAT-PG             PIC  X(010).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 ZAHLWEISE              PIC  X(002).
                35 PGV-PG                 PIC  X(004).
                35 PG-PROD-BEZ            PIC  X(012).
                35 PPFL-SUMME             PIC S9(007)V99.
                35 PROV-SATZ              PIC  9(003)V999.
                35 PROV-BETRAG            PIC S9(007)V99.
                35 UMSATZ-STEUER-BETRAG   PIC S9(007)V99.
                35 UMSATZ-STEUER-SATZ     PIC  9(003)V999.
                35 GESAMT-BETRAG          PIC S9(007)V99.
???   *         35 FILLER                 PIC  X(036).
      ******* ENDE COPY-MEMBER **************************************
 
