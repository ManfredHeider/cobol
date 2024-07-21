      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1803.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1803.CPv  $
      *
      *   Rev 2.1   Jun 16 2015 08:54:58   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PEG (B
      *atch)
      *
      *
      *
      *   Rev 2.0   Mar 17 2015 09:27:12   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1803  PEG-DATENSATZ DER DVM (90)          *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    17.03.15                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!17.03.15!2.1 ! NEUERSTELLUNG             ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DVM-DTA'S.                                                *
      *                                                              *
      ****************************************************************
      *
             30 DVM-DATENSATZ.
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 KUNDE-NNAME            PIC  X(025).
                35 KUNDE-VNAME            PIC  X(020).
                35 ABR-DAT-PG             PIC  X(010).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 PGV-PG                 PIC  X(004).
                35 PG-PROD-BEZ            PIC  X(012).
                35 PROV-SATZ              PIC  9(003)V999.
                35 PROV-BETRAG            PIC S9(007)V99.
                35 PPFL-SUMME             PIC S9(007)V99.
                35 UMSATZ-STEUER-BETRAG   PIC S9(007)V99.
                35 UMSATZ-STEUER-SATZ     PIC  9(003)V999.
                35 RECHNUNG-NUMMER-DVM    PIC  X(012).
                35 GESAMT-BETRAG          PIC S9(007)V99.
                35 FILLER                 PIC  X(021).
      ******* ENDE COPY-MEMBER **************************************


