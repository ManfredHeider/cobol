      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0792.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0792.CPv  $
      *
      *   Rev 2.4   Mar 10 2014 16:47:08   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Jul 26 2013 11:42:04   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Nov 02 2006 14:54:26   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jul 24 2003 16:33:38   t13ex44
      *APA00245: NEU: PVPT-KDBETR-REDUZ
      *
      *   Rev 2.0   May 26 1999 13:15:56   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:48:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0792                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG PRODUKTTEIL EINFüGEN             *
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
260713* APA01539!26.07.13! 2.3! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0792                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0792.
          05 PI-DRPA0792-PROGRAMM              PIC  X(008).
          05 PI-DRPA0792-UFKT                  PIC  9(002).
          05 PI-DRPA0792-PVPT-DATEN.
             10 PI-DRPA0792-PVPT-ID           PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0792-PVPT-VERSION      PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PVPT-F-GUELTIG-AB  PIC X(10).
             10 PI-DRPA0792-PVPT-F-GUELTIG-BIS PIC X(10).
             10 PI-DRPA0792-KNR-PVPT-ART      PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PRINTE            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0792-PVER-VERSION      PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PGEB-GES          PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PROD-PROD-SCHL    PIC S9(4) BINARY.
             10 PI-DRPA0792-PVPT-VT-PRIO      PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PVPT-PPFL-BEGINN  PIC X(10).
             10 PI-DRPA0792-PVPT-PPFL-ENDE    PIC X(10).
             10 PI-DRPA0792-PVPT-BEWERT-BETRAG
                                           PIC S9(9)V9(2) USAGE COMP-3.
             10 PI-DRPA0792-PVPT-PTEIL-BEGINN PIC X(10).
             10 PI-DRPA0792-PVPT-PTEIL-ABLAUF PIC X(10).
             10 PI-DRPA0792-PVPT-LAUFZEIT     PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PRKO-KOND-NR-EE   PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PVPT-EE-ENTSCHEID PIC  X(1).
             10 PI-DRPA0792-KNR-EVS-GRUND     PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
             10 PI-DRPA0792-PEWA-ID           PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0792-PVPT-PIN          PIC S9(9)   USAGE COMP.
             10 PI-DRPA0792-PEST-ID           PIC S9(15)V USAGE COMP-3.
240703       10 PI-DRPA0792-PVPT-KDBETR-REDUZ PIC X(1).
260713       10 PI-DRPA0792-PVPT-HAUPTTARIF-KZ PIC X(1).
260713       10 PI-DRPA0792-PVPT-BUZTAR        PIC S9(4)  USAGE COMP.
      *
       01 PO-DRPA0792.
          05 PO-DRPA0792-RC                         PIC  X(003).
             88 PO-DRPA0792-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0792-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
