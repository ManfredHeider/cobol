      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1549.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1549.CPv  $
      *
      *   Rev 2.1   Aug 21 2002 08:57:00   t13ex21
      *APA00206 - Neu: Datensatz DWS Österreich
      *
      *   Rev 2.0   04 Jul 2001 11:11:52   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1549  PEG-DATENSATZ VON DWS ÖSTERREICH 38 *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    03.07.02                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!03.07.02!2.1 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DTA'S FÜR DWS-ÖSTERREICH.                                 *
      ****************************************************************
      *
             30 DWS-A-DATENSATZ.
                35 VNR                    PIC  X(015).
                35 VERTRAG-BEGINN         PIC  9(008).
                35 FAELLIG-VON            PIC  9(008).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 KUNDE-NNAME            PIC  X(025).
                35 KUNDE-VNAME            PIC  X(020).
                35 ABR-DAT-PG             PIC  9(008).
                35 PIN                    PIC  9(007).
                35 ZAHLWEISE              PIC  X(002).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 PGV-PG.
                   40 STORNO-KZ           PIC  X(001).
                   40 UMSATZ-KZ           PIC  X(001).
                   40 EINMAL-KZ           PIC  X(001).
                   40 FILLER              PIC  X(001).
                35 PG-PRODUKT             PIC  X(012).
                35 PROV-SATZ              PIC  9(003)V999.
                35 GSMT-PROV              PIC S9(007)V99.
                35 PPFL-SUMME             PIC S9(007)V99.
                35 UMSATZSCHLUESSEL       PIC  9(002).
                35 FILLER                 PIC  X(102).
      *
      ******** ENDE COPY-MEMBER **************************************
