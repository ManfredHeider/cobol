      * T13EX16   Stand: 25/06/96    09:59:05
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0103.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0103.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Jun 25 1996 15:48:38   T13EX16
      *AFVBP226: SUSIS-Tabellen erweitert
      *
      *   Rev 1.1   May 15 1996 14:29:14   T13EX11
      *AFVB0226 Datenverfügbarkeit Pfändungen und
      *         Darlehen ergänzt
      *
      *   Rev 1.0   Mar 27 1996 13:40:16   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0103  STATISTIK-ANFANGS-DATEN:           *
      *                           1. WANN FINGEN DIE LEISTUNGEN AN   *
      *                           2. AB WANN LIEGEN DATEN VOR IN DB2 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                 *
      *    DATUM:    26.02.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0226!26.02.96! 01 ! NEU                       ! SCHOELLH.*
      * AFVB0226!06.05.96! 02 ! GEÄNDERTE ANGABEN FÜR     ! SCHOELLH.*
      *         !        !    ! PFÄNDUNGEN UND NEUE       !          *
      *         !        !    ! FÜR DARLEHENSZAHLUNGEN    !          *
      * AFVB0226!25.06.96! 03 ! GEÄNDERTE ANGABEN FÜR     ! auerbach *
      *         !        !    ! PFÄNDUNGEN                !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      * DATENBASIS: SEIT WANN SIND DIE EINZELNEN WERTE VORHANDEN
      * ES BEDEUTEN:
      *    K-...-LSTG-ANF-ISO  AB WANN GIBT ES DIESE LEISTUNG
      *    K-...-DATEN-ANF-ISO AB WANN LIEGEN DATEN IN DER DB VOR
      *    '1111-11-11' --> BEGINN DER LEISTUNG NICHT BEKANNT
      *                     (AUF JEDEN FALL VOR DEM 1.1.1990)
      *    '9999-12-31' --> DATEN NOCH NICHT VERFÜGBAR
      *    ACHTUNG: SONDERBEHANDLUNG FÜR K-RCKST-AUSZ-M-DATEN-ANF-ISO
      *             (SIEHE KOMMENTAR VOR K-RCKST-AUSZ-M-DATEN-ANF-ISO)
      *
       01  K-DATENBASIS-ANF.

      *    ----- NETTO-PROVISION ---------------------------------------
           05  K-NETTOPROV-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-NETTOPROV-DATEN-ANF-ISO  PIC X(010) VALUE '1989-07-01'.
      *    ----- ERFOLGSPROVISION, AUCH FÜR NEUE AL --------------------
           05  K-ERFOLGSPR-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-ERFOLGSPR-DATEN-ANF-ISO  PIC X(010) VALUE '1989-07-01'.
           05  K-ERFOLGSPR-N-AL-LSTG-ANF-ISO
                                          PIC X(010) VALUE '1995-07-01'.
           05  K-ERFOLGSPR-N-AL-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1995-07-01'.
      *    ----- ERFOLGSBONUS ------------------------------------------
           05  K-ERFOLGSBON-LSTG-ANF-ISO  PIC X(010) VALUE '1111-11-11'.
           05  K-ERFOLGSBON-DATEN-ANF-ISO PIC X(010) VALUE '1989-07-01'.
      *    ----- AUFBAUBONUS -------------------------------------------
           05  K-AUFBAUBONUS-LSTG-ANF-ISO PIC X(010) VALUE '1995-07-01'.
           05  K-AUFBAUBONUS-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1995-07-01'.
      *    ----- ED-PROV (MONATLICH UND VIERTELJÄHRLICH) ---------------
           05  K-EDPROV-MTL-LSTG-ANF-ISO  PIC X(010) VALUE '1111-11-11'.
           05  K-EDPROV-MTL-DATEN-ANF-ISO PIC X(010) VALUE '1989-07-01'.
           05  K-EDPROV-VJ-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-EDPROV-VJ-DATEN-ANF-ISO  PIC X(010) VALUE '1990-01-01'.
      *    ----- OLZ ---------------------------------------------------
           05  K-OLZ-BUERO-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-OLZ-BUERO-DATEN-ANF-ISO  PIC X(010) VALUE '1990-01-01'.
           05  K-OLZ-LEI-LSTG-ANF-ISO     PIC X(010) VALUE '1992-01-01'.
           05  K-OLZ-LEI-DATEN-ANF-ISO    PIC X(010) VALUE '1992-01-01'.
           05  K-OLZ-ORG-LSTG-ANF-ISO     PIC X(010) VALUE '1111-11-11'.
           05  K-OLZ-ORG-DATEN-ANF-ISO    PIC X(010) VALUE '1990-01-01'.
           05  K-OLZ-PARTNER-LSTG-ANF-ISO PIC X(010) VALUE '1995-01-01'.
           05  K-OLZ-PARTNER-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1995-01-01'.
      *    ----- SONSTIGE AUSZAHLUNGEN ---------------------------------
           05  K-BETRBON-LSTG-ANF-ISO     PIC X(010) VALUE '1995-05-01'.
           05  K-BETRBON-DATEN-ANF-ISO    PIC X(010) VALUE '1995-05-01'.
           05  K-FOERDERBON-LSTG-ANF-ISO  PIC X(010) VALUE '1994-10-01'.
           05  K-FOERDERBON-DATEN-ANF-ISO PIC X(010) VALUE '1994-10-01'.
           05  K-AUFBAUBON-BLGR-LSTG-ANF-ISO
                                          PIC X(010) VALUE '1111-11-11'.
           05  K-AUFBAUBON-BLGR-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1990-01-01'.
           05  K-SOPROV-EINM-LSTG-ANF-ISO PIC X(010) VALUE '1981-12-01'.
           05  K-SOPROV-EINM-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1981-12-01'.
           05  K-SOPROV-LFD-LSTG-ANF-ISO  PIC X(010) VALUE '1111-11-11'.
           05  K-SOPROV-LFD-DATEN-ANF-ISO PIC X(010) VALUE '1990-01-01'.
      *    ----- UNTERSTÜTZUNGSKASSE -----------------------------------
           05  K-UNTSTUETZ-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-UNTSTUETZ-DATEN-ANF-ISO  PIC X(010) VALUE '1990-01-01'.
      *    ----- KONTO-STAND DISKONT -----------------------------------
           05  K-KTO-DISK-LSTG-ANF-ISO    PIC X(010) VALUE '1111-11-11'.
           05  K-KTO-DISK-DATEN-ANF-ISO   PIC X(010) VALUE '1994-06-01'.
      *    ----- KONTO-STAND DARLEHEN ----------------------------------
           05  K-KTO-DARL-LSTG-ANF-ISO    PIC X(010) VALUE '1111-11-11'.
           05  K-KTO-DARL-DATEN-ANF-ISO   PIC X(010) VALUE '1994-06-01'.
      *    ----- KONTO-STAND RÜCKSTELLUNGEN ----------------------------
           05  K-KTO-RCKST-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
           05  K-KTO-RCKST-DATEN-ANF-ISO  PIC X(010) VALUE '1989-07-01'.
      *    ----- RÜCKSTELLUNGSZUFLUSS ----------------------------------
           05  K-RCKST-ZUFL-LSTG-ANF-ISO  PIC X(010) VALUE '1111-11-11'.
           05  K-RCKST-ZUFL-DATEN-ANF-ISO PIC X(010) VALUE '1993-08-01'.
      *    ----- RÜCKSTELLUNG-AUSZ WG. ÜBERDECKUNG UND MANUELL ---------
           05  K-RCKST-AUSZ-Q-LSTG-ANF-ISO
                                          PIC X(010) VALUE '1111-11-11'.
           05  K-RCKST-AUSZ-Q-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1990-01-01'.
           05  K-RCKST-AUSZ-M-LSTG-ANF-ISO
                                          PIC X(010) VALUE '1111-11-11'.
      *        ---------------------------------------------------------
      *         SONDERBEHANDLUNG FÜR K-RCKST-AUSZ-M-DATEN-ANF-ISO:
      *            DIE MANUELLEN RCKST-AUSZAHLUNGEN LIEGEN AB DEM
      *            1.7.92 VOR.
      *          DOCH: WENN DIE WERTE FÜR EINEN MONAT (ZUM STICHTAG
      *            15. !!) BENÖTIGT WERDEN, WIRD DER ZEITRAUM VOM 16.
      *            DES VORMANATS BIS ZUM 15. DES AKTUELLEN MONATS
      *            GENOMMEN.
      *          ALSO: FOLGLICH LIEGEN DIE WERTE ZUM STICHTAG 15.7.92
      *            NICHT VOLLSTÄNDIG VOR. DESHALB WIRD DER MONAT 7/92
      *            SO BEHANDELT, ALS LÄGEN HIERFÜR GAR KEINE WERTE VOR.
      *          DESHALB STEHT DER DATUMSWERT AUF 16.7.1992
      *        ---------------------------------------------------------
           05  K-RCKST-AUSZ-M-DATEN-ANF-ISO
                                          PIC X(010) VALUE '1992-07-16'.
060596*    ----- PFÄNDUNGEN --------------------------------------------
060596     05  K-PFAENDUNG-LSTG-ANF-ISO   PIC X(010) VALUE '1111-11-11'.
250696     05  K-PFAENDUNG-DATEN-ANF-ISO  PIC X(010) VALUE '1996-06-01'.
060596*    ----- DARLEHEN/TILGUNGEN ------------------------------------
060596     05  K-DARLEHEN-LSTG-ANF-ISO    PIC X(010) VALUE '1111-11-11'.
060596     05  K-DARLEHEN-DATEN-ANF-ISO   PIC X(010) VALUE '1996-07-01'.
      *
      ******** ENDE COPY-MEMBER DRVB0103 ******************************

