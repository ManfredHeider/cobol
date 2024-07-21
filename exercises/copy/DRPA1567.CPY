      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1567.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1567.CPv  $
      *
      *   Rev 2.1   Dec 22 2003 14:18:20   t13ae46
      *APA00262: Neuerstellung
      *
      *   Rev 2.0   Jul 04 2002 15:30:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1567  PEG-DATENSATZ DER DSL -- BANK (67)  *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    04.12.03                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00262!04.12.03!2.1 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DSL-BANK-DTA'S.                                           *
      *    FELDER, DIE MIT "NR-" BEGINNEN, SIND FÜR DAISY NICHT      *
      *    RELEVANT.                                                 *
      ****************************************************************
      *
             30 DSL-BANK-DATENSATZ.
                35 VNR                           PIC  9(015).
                35 VERTRAG-BEGINN                PIC  X(010).
                35 FAELLIG-VON                   PIC  X(010).
                35 AVERM                         PIC  9(007).
                35 PGEB-GES                      PIC  9(002).
                35 KUNDE-NNAME                   PIC  X(025).
                35 KUNDE-VNAME                   PIC  X(020).
                35 ABR-DAT-PG                    PIC  X(010).
                35 VERTRAG-WAEHRUNG              PIC  X(003).
                35 PGV-PG                        PIC  X(004).
                35 PROD-BEZ                      PIC  X(015).
                35 PROV-SATZ                     PIC  9(003)V999.
                35 PROV-BETRAG-VORZ              PIC  X.
                35 PROV-BETRAG                   PIC  9(009)V99.
                35 PPFL-SUMME-VORZ               PIC  X.
                35 PPFL-SUMME                    PIC  9(009)V99.
                35 LAUFZEIT                      PIC  9(004).
      *
      ******** ENDE COPY-MEMBER **************************************
