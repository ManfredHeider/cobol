      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1554.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1554.CPv  $
      *
      *   Rev 2.4   Nov 22 2006 16:14:08   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Nov 02 2006 10:07:16   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Sep 06 2002 12:37:36   t13ex21
      *APA00206 - Kundenname auf 18 Stellen; Neu: Schicht-KZ
      *
      *   Rev 2.1   Aug 21 2002 09:54:10   t13ex21
      *APA00206 - Neuerstellung
      *
      *   Rev 2.0   Jul 04 2002 15:29:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *COPY-MEMBER: DRPA1554  PEG-DATENSATZ GES. 15 - WIENER ALLIANZ  *
      *                       (SACH- UND LEBENSVERSICHERUNG)          *
      *---------------------------------------------------------------*
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    12.08.02                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!12.08.02!2.1 ! NEU                       ! STORCH   *
      *         !06.09.02!2.2 ! KUNDENNAME AUF 18 STELLEN ! STORCH   *
      *         !        !    ! GEKÜRZT, NEU: SCHICHT-KZ  !          *
      * APA00452!02.11.06!2.3 ! Anpassung an 3-stellige   ! Jäger    *
      *         !        !    ! Produkte                  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    WIENER ALLIANZ DTA'S (LEBEN UND SACH).                    *
      ****************************************************************
      *
             30 WIENER-ALLIANZ-DATENSATZ.
                35 PGEB-GES                 PIC  9(002).
                35 VNR                      PIC  9(015).
                35 FAELLIG-VON              PIC  9(008).
      *         35 PROD-SCHL                PIC  9(002).
                35 PG-TARIF2                PIC  9(002).
                35 PRODART                  PIC  9(001).
                35 PIN                      PIC  9(007).
                35 PIN-PRZ                  PIC  9(002).
                35 ABR-DAT-PG               PIC  9(008).
      *         35 KUNDE-NAME               PIC  X(020).
                35 KUNDE-NAME               PIC  X(018).
                35 SCHICHT-KZ               PIC  X(002).
                35 AVERM                    PIC  9(007).
                35 AVERM-PRZ                PIC  9(001).
                35 BUCHDAT                  PIC  9(008).
                35 PROV-SATZ                PIC  9(002)V9(03).
                35 PPFL-SUMME               PIC  9(007)V9(02).
                35 PROV-BETRAG              PIC  9(007)V9(02).
                35 HAFTZ-BEGINN             PIC  9(006).
                35 HAFTZ-ENDE               PIC  9(006).
                35 RUECKVERRECHNUNG         PIC  9(006).
                35 JN-BEITRAG               PIC  9(007)V9(02).
                35 BESTANDSSCHLUESSEL       PIC  X(002).
                35 PG-TARIF                 PIC  X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
      *                                                              *
