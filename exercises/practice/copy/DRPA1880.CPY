      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1880.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1880.CPv  $
      *
      *   Rev 2.1   Nov 16 2017 10:23:08   t13ae46
      *APA02520: Produkteinführung Geiger Goldsparplan - Programme
      *
      *   Rev 2.0   Nov 13 2017 16:38:56   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1880  PEG-DATENSATZ FUER
      *                           GEIGER GOLDSPARPLAN GES 79
      *    --------------------------------------------------------
      *    ERSTELLT: WANG
      *    DATUM:    14.11.17
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02520!14.11.17!2.1 ! NEU                       !WANG      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    GEIGER GOLDSPARPLAN-DTA'S.                                *
      *                                                              *
      ****************************************************************
      *
             30 GEIGER-GOLDSPARPLAN-DATENSATZ.
                35 PGEB-GES               PIC  9(002).
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 KUNDE-NNAME            PIC  X(035).
                35 KUNDE-VNAME            PIC  X(035).
                35 ABR-DAT-PG             PIC  X(010).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 ZAHLWEISE              PIC  9(002).
                35 PGV-PG                 PIC  X(004).
                35 PG-PROD-BEZ            PIC  X(015).
                35 PPFL-SUMME-VORZ        PIC  X(001).
                35 PPFL-SUMME             PIC S9(009)V99.
                35 PROV-SATZ              PIC  9(003)V999.
                35 PROV-BETRAG-VORZ       PIC  X(001).
                35 PROV-BETRAG            PIC S9(009)V99.
                35 SPAR-BETRAG-VORZ       PIC  X(001).
                35 SPAR-BETRAG            PIC S9(009)V99.
                35 FILLER                 PIC  X(09).
      ******* ENDE COPY-MEMBER **************************************


