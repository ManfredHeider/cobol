      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1945.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1945.CPv  $
      *
      *   Rev 2.1   Apr 16 2020 15:27:04   t13ae37
      *APA02915: VDL: Neue Produktgebergesellschaft JURADIREKT - AB
      *U - PEG/PVW
      *
      *   Rev 2.0   Feb 26 2020 13:05:52   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1945  PEG-DATENSATZ VON                   *
      *                           JURADIREKT (GES. 86)                *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02915!26.02.20!2.1 ! NEUERSTELLUNG             ! BUCH      *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES     *
      *    JURADIREKT DTAS.                                         *
      *                                                               *
      *****************************************************************
      *
             30 JURADIREKT-DATENSATZ.
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
