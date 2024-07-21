      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0505.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0505.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:36:50   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   12 Sep 1997 16:58:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0505 FUNKTION LESEN KONDITION            *
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
      * AFPA0020!24.10.97!01.02!ALLE KONDITIONSSäTZE ER-  !          *
      *         !        !     !HALTEN VORZEICHEN         !          *
      *         !        !     !VERW. NESTED-COPY-STRECKE !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0505                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0505.
          05 PI-DRPA0505-DATEN.
             10 PI-DRPA0505-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0505-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0505-PRKO-KOND-NR         PIC S9(004)  BINARY.
      *
       01 PO-DRPA0505.
          05 PO-DRPA0505-RC                      PIC  X(003).
             88 PO-DRPA0505-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0505-RC-NOT-FOUND              VALUE 'NFD'.
         05 PO-DRPA0505-DATEN.
             COPY DRPA0396.
      *
      ******** Ende Copy-Member **************************************
