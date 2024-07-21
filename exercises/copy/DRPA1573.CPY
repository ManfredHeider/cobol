      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1573.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1573.CPv  $
      *
      *   Rev 2.2   Apr 08 2004 11:47:34   t13ex44
      *Einführung neuer öster. Ges. - GENERALI-Bank und -Investment
      *
      *   Rev 2.1   Mar 16 2004 11:52:48   t13ex44
      *APA00270 - PEG für Ges. 39 und 69
      *
      *   Rev 2.0   Jul 04 2002 15:31:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1573  PEG-DATENSATZ GENERALI - BANK (69)  *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    11.03.04                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00270!11.03.04!2.1 ! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    GENERALI-BANK-DTA'S.                                      *
      *    FELDER, DIE MIT "NR-" BEGINNEN, SIND FÜR DAISY NICHT      *
      *    RELEVANT.                                                 *
      ****************************************************************
      *
             30 GENERALI-BANK-DATENSATZ.
                35 VNR                           PIC  9(015).
                35 VERTRAG-BEGINN                PIC  X(010).
                35 FAELLIG-VON                   PIC  X(010).
                35 AVERM                         PIC  9(007).
                35 PGEB-GES                      PIC  9(002).
                35 KUNDE-NNAME                   PIC  X(025).
                35 KUNDE-VNAME                   PIC  X(020).
                35 ABR-DAT-PG                    PIC  X(010).
                35 VERTRAG-WAEHRUNG              PIC  X(003).
                35 PG-PGV                        PIC  X(004).
                35 PG-PROD-BEZ                   PIC  X(012).
                35 PROV-SATZ                     PIC  9(003)V9(03).
                35 PROV-BETRAG-VORZ              PIC  X(001).
                35 PROV-BETRAG                   PIC  9(007)V9(02).
                35 PPFL-SUMME-VORZ               PIC  X(001).
                35 PPFL-SUMME                    PIC  9(007)V9(02).
      *
      ******** ENDE COPY-MEMBER **************************************
