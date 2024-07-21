      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1572.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1572.CPv  $
      *
      *   Rev 2.6   Oct 18 2017 08:31:32   t13ae37
      *APA02498: DVAG-Wien: Einführung neue Gesellschaft Capital Ba
      *nk - PEG
      *
      *   Rev 2.5   Nov 21 2012 10:33:16   t13ex67
      *APA01403: Ges 39- Generali Invest: Anpassung Schnittstelle
      *
      *   Rev 2.4   Aug 14 2006 11:55:02   t13ex44
      *APA00494: GLI-Bank
      *
      *   Rev 2.3   Jul 06 2005 08:42:14   t13ae46
      *APA00363: Generali Bank Invest - Sparpläne
      *
      *   Rev 2.2   Apr 30 2004 08:25:10   t13ex44
      *Einführung neuer öster. Ges. - GENERALI-Bank und -Investment
      *
      *   Rev 2.1   Mar 16 2004 11:52:38   t13ex44
      *APA00270 - PEG für Ges. 39 und 69
      *
      *   Rev 2.0   Jul 04 2002 15:30:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1572  PEG-DATENSATZ GENERALI - INVEST (39)*
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
      * APA00363!24.06.05!2.2 ! TARIF-KZ,SPARPLAN-NR HINZU! WANG     *
      * APA00494!11.08.06!2.2 ! TARIF-KZ 2->3 STELLEN     ! SCHEIDEL *
      * APA01403!24.10.12!2.5 ! SPARPLAN-NR ENTFÄLLT      ! STORCH   *
      * APA02498!22.09.17!2.6 ! GLEICHER SATZ FÜR CAPITAL ! BUCH     *
      *         !        !    ! BANK                      ! BUCH     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    GENERALI-INVEST-DTA'S                                     *
      *    FELDER, DIE MIT "NR-" BEGINNEN, SIND FÜR DAISY NICHT      *
      *    RELEVANT.                                                 *
      ****************************************************************
      *
             30 GENERALI-INVEST-DATENSATZ.
      * WIRD AUCH FÜR CAPITAL BANK VERWENDET
                35 VNR                           PIC  9(015).
                35 VERTRAG-BEGINN                PIC  X(010).
                35 FAELLIG-VON                   PIC  X(010).
                35 AVERM                         PIC  9(007).
                35 PGEB-GES                      PIC  9(002).
                35 PIN                           PIC  9(007).
                35 KUNDE-NNAME                   PIC  X(025).
                35 KUNDE-VNAME                   PIC  X(020).
                35 ABR-DAT-PG                    PIC  X(010).
                35 ZAHLUNGSFREQUENZ              PIC  X(002).
                35 VERTRAG-WAEHRUNG              PIC  X(003).
                35 PGV-PG                        PIC  X(004).
                35 PG-WPKN                       PIC  X(012).
                35 PROV-SATZ                     PIC  9(003)V9(03).
                35 PROV-BETRAG-VORZ              PIC  X(001).
                35 PROV-BETRAG                   PIC S9(007)V9(02).
                35 PPFL-SUMME-VORZ               PIC  X(001).
                35 PPFL-SUMME                    PIC S9(007)V9(02).
                35 SPAR-BETRAG-VORZ              PIC  X(001).
                35 SPAR-BETRAG                   PIC S9(007)V9(02).
                35 TARIF-KZ                      PIC  X(003).
      *         35 SPARPLAN-NR                   PIC  X(003).
                35 FILLER                        PIC  X(003).
      *
      ******** ENDE COPY-MEMBER **************************************

