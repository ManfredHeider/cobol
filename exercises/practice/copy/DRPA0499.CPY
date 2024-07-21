      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0499.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0499.CPv  $
      *
      *   Rev 2.1   Jan 13 2014 07:22:06   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.0   Feb 27 1999 14:29:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:31:58   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   23 Jul 1997 12:44:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0499 FUNKTION ERMITTELN KONDITION        *
      *                           FüR GESAMTPROVISION                *
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
      * APA01539!01.10.13!02.01!ERWEITERUNG PI-BEREICH    ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0499                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0499.
          05 PI-DRPA0499-PZVP-VERSION            PIC S9(004)  BINARY.
          05 PI-DRPA0499-DATEN.
             10 PI-DRPA0499-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0499-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0499-STICHTAG             PIC  X(010).
             10 PI-DRPA0499-PRODART              PIC S9(004)  BINARY.
             10 PI-DRPA0499-LAUFZEIT             PIC S9(004)  BINARY.
011013       10 PI-DRPA0499-TARIFSCHL-GRP        PIC S9(009)  BINARY.
011013       10 PI-DRPA0499-TARIFSCHL            PIC S9(009)  BINARY.
011013       10 PI-DRPA0499-KNR-TARIF-KZ         PIC S9(004)  BINARY.
      *
       01 PO-DRPA0499.
          05 PO-DRPA0499-RC                      PIC  X(003).
             88 PO-DRPA0499-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0499-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA0499-RC-SEL-KRIT-NOK           VALUE 'SKN'.
             88 PO-DRPA0499-RC-KOND-DEF-NOK           VALUE 'KDN'.
         05 PO-DRPA0499-DATEN.
             COPY DRPA0396.
      *
      ******** Ende Copy-Member **************************************
