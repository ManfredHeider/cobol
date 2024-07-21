      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0500.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0500.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:33:30   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   23 Jul 1997 12:44:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0500 FUNKTION ERMITTELN KONDITION        *
      *                           FüR EINHEITEN
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!19.09.97! 01 !NEU                        !BOHLINGER *
      * AFPA0020!24.10.97!01.02!RC-KOND-DEF-NOK           !          *
      *         !        !     !ALLE KONDITIONSSäTZE ER-  !          *
      *         !        !     !HALTEN VORZEICHEN         !          *
      *         !        !     !VERW. NESTED-COPY-STRECKE !BOHLINGER *
      *         !        !     !HALTEN VORZEICHEN         !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0500                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0500.
          05 PI-DRPA0500-PZVP-VERSION           PIC S9(004)    BINARY.
          05 PI-DRPA0500-DATEN.
             10 PI-DRPA0500-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0500-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0500-STICHTAG             PIC  X(010).
             10 PI-DRPA0500-PRODART              PIC S9(004)  BINARY.
             10 PI-DRPA0500-LAUFZEIT             PIC S9(004)  BINARY.
      *
       01 PO-DRPA0500.
          05 PO-DRPA0500-RC                      PIC  X(003).
             88 PO-DRPA0500-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0500-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA0500-RC-SEL-KRIT-NOK           VALUE 'SKN'.
             88 PO-DRPA0500-RC-KOND-DEF-NOK           VALUE 'KDN'.
         05 PO-DRPA0500-DATEN.
             COPY DRPA0548.
      *
      ******** Ende Copy-Member **************************************
