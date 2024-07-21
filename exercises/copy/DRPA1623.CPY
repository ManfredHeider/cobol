      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1623.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1623.CPv  $
      *
      *   Rev 2.2   Oct 31 2018 14:15:08   t13ae37
      *APA02705: Kundenbestandsprovision zu Champions Select Dynami
      *k  und Champions Select Balance
      *
      *   Rev 2.1   Dec 10 2007 13:36:04   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Aug 03 2004 11:44:28   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1623  PEG-DATENSATZ FUER GESELLSCHAFT 91
      *    --------------------------------------------------------
      *    ERSTELLT: STORCH-SCHÄFER
      *    DATUM:    11.07.07
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!11.07.07!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA02507!07.09.18!2.2 ! 3NACHKOMMASTELLEN PROV-SATZ BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      *ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DER    *
      *    GESELLSCHAFT 91.                                          *
      *    DIE SATZLÄNGE BETRÄGT 275 BYTES.                          *
      ****************************************************************
      *
             30 GES-91-DATENSATZ.
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
                35 PG-BEITRAG             PIC S9(011)V9(02).
                35 PPFL-BETRAG            PIC S9(011)V9(02).
070918*         35 PROV-SATZ              PIC S9(005)V9(02).
070918          35 PROV-SATZ              PIC S9(004)V9(03).
                35 PROV-BETRAG            PIC S9(011)V9(02).
                35 AUSSCHUET-AMBUS        PIC S9(011)V9(02).
                35 FILLER                 PIC  X(024).

