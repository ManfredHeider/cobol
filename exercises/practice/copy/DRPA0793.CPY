      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0793.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0793.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:47:16   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jul 26 2013 11:42:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Jul 24 2003 16:34:14   t13ex44
      *APA00245: NEU: PVPT-KDBETR-REDUZ
      *
      *   Rev 2.0   May 26 1999 13:15:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:48:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0793                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG PRODUKTTEIL UPDATEN              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    13.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.01.98! 01 ! NEU                       ! STORCH   *
      * APA00245!24.07.03! 2.1! NEU: PVPT-KDBETR-REDUZ    ! SCHEIDEL *
260713* APA01539!26.07.13! 2.2! ERGÄNZUNG FÜR KALYPSO     ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0793                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0793.
          05 PI-DRPA0793-PROGRAMM             PIC  X(008).
          05 PI-DRPA0793-UFKT                 PIC  9(002).
          05 PI-DRPA0793-PVPT-VERSION-ALT     PIC S9(4) USAGE COMP.
          05 PI-DRPA0793-PVPT-DATEN.
             10 PI-DRPA0793-PVPT-ID           PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0793-PVPT-VERSION      PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PVPT-F-GUELTIG-AB  PIC X(10).
             10 PI-DRPA0793-PVPT-F-GUELTIG-BIS PIC X(10).
             10 PI-DRPA0793-KNR-PVPT-ART      PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PRINTE            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0793-PVER-VERSION      PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PGEB-GES          PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PROD-PROD-SCHL    PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PVPT-VT-PRIO      PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PVPT-PPFL-BEGINN  PIC X(10).
             10 PI-DRPA0793-PVPT-PPFL-ENDE    PIC X(10).
             10 PI-DRPA0793-PVPT-BEWERT-BETRAG
                                           PIC S9(9)V9(2) USAGE COMP-3.
             10 PI-DRPA0793-PVPT-PTEIL-BEGINN PIC X(10).
             10 PI-DRPA0793-PVPT-PTEIL-ABLAUF PIC X(10).
             10 PI-DRPA0793-PVPT-LAUFZEIT     PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PRKO-KOND-NR-EE   PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PVPT-EE-ENTSCHEID PIC X(1).
             10 PI-DRPA0793-KNR-EVS-GRUND     PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
             10 PI-DRPA0793-PEWA-ID           PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0793-PVPT-PIN          PIC S9(9)   USAGE COMP.
             10 PI-DRPA0793-PEST-ID           PIC S9(15)V USAGE COMP-3.
240703       10 PI-DRPA0793-PVPT-KDBETR-REDUZ PIC X(1).
             10 PI-DRPA0793-PVPT-LAST-UPDATE  PIC X(26).
260713       10 PI-DRPA0793-PVPT-HAUPTTARIF-KZ PIC X(1).
260713       10 PI-DRPA0793-PVPT-BUZTAR        PIC S9(4)  USAGE COMP.
      *
       01 PO-DRPA0793.
          05 PO-DRPA0793-RC                         PIC  X(003).
             88 PO-DRPA0793-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0793-RC-DUPLICATE-VALUE            VALUE 'DVA'.
             88 PO-DRPA0793-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0793-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
