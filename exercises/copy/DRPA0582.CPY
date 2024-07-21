      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0582.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0582.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Aug 1998 18:38:52   T13EX22
      *APAA0032 - PEW Release 2.0
      *
      *   Rev 1.0   22 Oct 1997 13:12:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0582 FUNKTION ERMITTELN WIDERVORLAGE-    *
      *                                   ZEITRAUM                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.10.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!31.10.97! 01  !NEU                       !BOHLINGER *
      *         !02.06.98!01.02!NEU: pvzp-version         !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0582                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0582.
          05 PI-DRPA0582-PZVP-VERSION            PIC S9(004)    BINARY.
          05 PI-DRPA0582-DATEN.
             10 PI-DRPA0582-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0582-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0582-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA0582.
          05 PO-DRPA0582-RC                      PIC  X(003).
             88 PO-DRPA0582-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0582-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRPA0582-DATEN.
             10 PO-DRPA0582-PGRP-WDV-DAUER        PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************
