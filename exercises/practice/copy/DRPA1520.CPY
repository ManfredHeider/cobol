      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1520.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRPA1520.CPv  $
      *
      *   Rev 2.3   Apr 26 2002 16:04:18   t13ex21
      *APA00191 - Erweiterung um Laufzeit
      *
      *   Rev 2.2   Feb 21 2002 11:56:32   t13ex21
      *APA00172 - PGV-KZ hinzu
      *
      *   Rev 2.1   Feb 20 2002 12:29:08   t13ex21
      *APA00172 - Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:06   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1520  PEG-DATENSATZ GESELLSCHAFT 60       *
      *                           DEUTSCHE BANK 24                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    08.01.02                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00172!08.01.02!2.1 ! NEU                       ! STORCH   *
      * APA00191!16.04.02!2.2 ! PG-LAUFZEIT HINZU         ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DEUTSCHE-BANK-24 DTAS.                                    *
      *                                                              *
      ****************************************************************
      *
             30 DB24-DATENSATZ.
                35 VNR                      PIC  9(015).
                35 PRODUKT-ID               PIC  X(020).
                35 VERTRAGSBEGINN           PIC  X(010).
                35 FAELLIG-VON              PIC  X(010).
                35 AVERM                    PIC  9(007).
                35 PGEB-GES                 PIC  9(002).
                35 PIN                      PIC  9(007).
                35 PIN-PRUEFZIFFER          PIC  9(002).
                35 KUNDE-NNAME              PIC  X(035).
                35 KUNDE-VNAME              PIC  X(035).
                35 ABR-DAT-PG               PIC  X(010).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 PGV-PG                   PIC  X(004).
                35 PROD-BEZ                 PIC  X(012).
                35 PROV-SATZ                PIC  9(003)V9(03).
                35 PROV-BETRAG              PIC S9(007)V9(02).
                35 PPFL-SUMME               PIC S9(007)V9(02).
                35 PGV-KZ                   PIC  X(001).
160402          35 PG-LAUFZEIT              PIC  9(004).
160402*         35 FILLER                   PIC  X(101).
160402          35 FILLER                   PIC  X(097).
      *
      ******* ENDE COPY-MEMBER **************************************
