      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1887.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1887.CPv  $
      *
      *   Rev 2.1   Apr 04 2018 15:36:38   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.0   Mar 28 2018 10:20:22   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1887  PEG-DATENSATZ FUER GESELLSCHAFT 42
      *    -----------------------------------------------------------
      *    ERSTELLT: WANG
      *    DATUM:    28.03.18
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!28.03.18!2.1 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      *ENDE-LOGBUCH                                                  *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DER    *
      *    GESELLSCHAFT 42.                                          *
      *    DIE SATZLÄNGE BETRÄGT 275 BYTES.                          *
      ****************************************************************
      *
             30 ALAG-INVEST-DATENSATZ.
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 VERTRAG-ABLAUF         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 KUNDE-NNAME            PIC  X(025).
                35 KUNDE-VNAME            PIC  X(020).
                35 ABR-DAT-PG             PIC  X(010).
                35 EINL-DAT-PG            PIC  X(010).
                35 STRUKTURTERMIN         PIC  X(010).
                35 PIN                    PIC  9(009).
                35 ZAHLWEISE              PIC  9(003).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 HAFTZEITBEGINN         PIC  X(010).
                35 HAFTZEITENDE           PIC  X(010).
                35 HAFTZEITDAUER          PIC  9(004).
                35 BELASTUNGSDAUER        PIC  9(004).
                35 PGV-PG                 PIC  9(004).
                35 PG-TARIF               PIC  X(015).
                35 PG-SUMME               PIC S9(011)V9(02).
                35 PPFL-SUMME             PIC S9(011)V9(02).
                35 PROV-SATZ              PIC S9(005)V9(02).
                35 PROV-BETRAG            PIC S9(011)V9(02).
                35 AUSSCHUET-AMBUS        PIC S9(011)V9(02).
                35 FILLER                 PIC  X(024).


