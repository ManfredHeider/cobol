      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.02.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1867.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1867.CPv  $
      *
      *   Rev 2.1   Mar 27 2017 16:47:36   t13ae46
      *APA02353: Neuer Produktpartner TeleSon - PEG
      *
      *   Rev 2.0   Feb 16 2017 13:01:48   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1867  PEG-DATENSATZ DER TELESON (94)      *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    16.02.17                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02353!16.02.17!2.1 ! NEUERSTELLUNG             ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    TELESON-DTAS.                                             *
      *                                                              *
      ****************************************************************
      *
             30 TELESON-DATENSATZ.
                35 PGEB-GES               PIC  9(002).
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 KUNDE-NNAME            PIC  X(035).
                35 KUNDE-VNAME            PIC  X(035).
                35 ABR-DAT-PG             PIC  X(010).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 ZAHHLWEISE             PIC  X(002).
                35 PGV-PG                 PIC  X(004).
                35 PG-PROD-BEZ            PIC  X(015).
                35 PROD-PARTNER           PIC  X(020).
                35 TARIF-BEZ              PIC  X(050).
                35 LAUFZEIT               PIC  9(002).
                35 VERBRAUCH              PIC  9(010).
                35 ZAEHLER-NR             PIC  X(020).
                35 PPFL-SUMME             PIC S9(007)V99.
                35 PROV-SATZ              PIC  9(003)V999.
                35 PROV-BETRAG            PIC S9(007)V99.
                35 UMSATZ-STEUER-BETRAG   PIC S9(007)V99.
                35 UMSATZ-STEUER-SATZ     PIC  9(003)V999.
                35 GESAMT-BETRAG          PIC S9(007)V99.
                35 STORNO-GRUND           PIC  X(020).
                35 FILLER                 PIC  X(082).
      ******* ENDE COPY-MEMBER **************************************


