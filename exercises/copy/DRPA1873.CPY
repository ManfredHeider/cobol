      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1873.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1873.CPv  $
      *
      *   Rev 2.1   Jun 13 2017 12:39:10   t13ex67
      *APA02396: Lohnwerk: PEG-Copy MSS
      *
      *   Rev 2.0   May 18 2017 09:44:24   t13ex24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1873  PEG-DATENSATZ DER LOHNWERK (98)     *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: KÖPRÜCÜ                                          *
      *    DATUM:    18.05.17                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02396!18.05.17!2.1 ! NEUERSTELLUNG             ! KÖPRÜCÜ  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    LOHNWERK-DTA'S.                                           *
      *                                                              *
      ****************************************************************
      *
             30 LOHNWERK-DATENSATZ.
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
                35 FILLER                 PIC  X(107).
      ******* ENDE COPY-MEMBER **************************************




