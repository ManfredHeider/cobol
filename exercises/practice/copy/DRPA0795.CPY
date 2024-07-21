      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0795.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0795.CPv  $
      *
      *   Rev 2.4   Mar 10 2014 16:47:22   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Jul 26 2013 11:42:16   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Nov 02 2006 14:54:50   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jul 24 2003 16:34:24   t13ex44
      *APA00245: NEU: PVPT-KDBETR-REDUZ
      *
      *   Rev 2.0   May 26 1999 13:16:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:48:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0795                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION SELECT ALL (TBPA0111)       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    13.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.01.98!01.01! NEU                      ! STORCH   *
      * APA00245!24.07.03! 2.1 ! NEU: PVPT-KDBETR-REDUZ   ! SCHEIDEL *
260713* APA01539!26.07.13! 2.3 ! ERGÄNZUNGEN FÜR KALYPSO  ! RAHNER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0795
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0795.
           05 PI-DRPA0795-PROGRAMM             PIC  X(008).
           05 PI-DRPA0795-UFKT                 PIC  9(002).
           05 PI-DRPA0795-DATEN.
              10 PI-DRPA0795-PVPT-ID           PIC S9(15)V USAGE COMP-3.
              10 PI-DRPA0795-PVPT-VERSION      PIC S9(4) USAGE COMP.
      *
       01  PO-DRPA0795.
           05 PO-DRPA0795-RC               PIC  X(003).
              88 PO-DRPA0795-RC-OK              VALUE 'OK '.
              88 PO-DRPA0795-RC-NOT-FOUND       VALUE 'NFD'.
           05 PO-DRPA0795-DATEN.
              10 PO-DRPA0795-LAST-UPDATE       PIC X(26).
              10 PO-DRPA0795-PVPT-ID           PIC S9(15)V USAGE COMP-3.
              10 PO-DRPA0795-PVPT-VERSION      PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PVPT-F-GUELTIG-AB  PIC X(10).
              10 PO-DRPA0795-PVPT-F-GUELTIG-BIS PIC X(10).
              10 PO-DRPA0795-PVPT-V-GUELTIG-AB  PIC X(10).
              10 PO-DRPA0795-PVPT-V-GUELTIG-BIS PIC X(10).
              10 PO-DRPA0795-PVPT-V-KENNUNG    PIC X(8).
              10 PO-DRPA0795-KNR-PVPT-ART      PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PRINTE            PIC S9(15)V USAGE COMP-3.
              10 PO-DRPA0795-PVER-VERSION      PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PGEB-GES          PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PROD-PROD-SCHL    PIC S9(4) BINARY.
              10 PO-DRPA0795-PVPT-VT-PRIO      PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PVPT-PPFL-BEGINN  PIC X(10).
              10 PO-DRPA0795-PVPT-PPFL-ENDE    PIC X(10).
              10 PO-DRPA0795-PVPT-BEWERT-BETRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
              10 PO-DRPA0795-PVPT-PTEIL-BEGINN PIC X(10).
              10 PO-DRPA0795-PVPT-PTEIL-ABLAUF PIC X(10).
              10 PO-DRPA0795-PVPT-LAUFZEIT     PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PRKO-KOND-NR-EE   PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PVPT-EE-ENTSCHEID PIC X(1).
              10 PO-DRPA0795-KNR-EVS-GRUND     PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
              10 PO-DRPA0795-PEWA-ID           PIC S9(15)V USAGE COMP-3.
              10 PO-DRPA0795-PVPT-PIN          PIC S9(9)   USAGE COMP.
              10 PO-DRPA0795-PEST-ID           PIC S9(15)V USAGE COMP-3.
240703        10 PO-DRPA0795-PVPT-KDBETR-REDUZ PIC X(1).
260713        10 PO-DRPA0795-PVPT-HAUPTTARIF-KZ PIC X(1).
260713        10 PO-DRPA0795-PVPT-BUZTAR        PIC S9(4)  USAGE COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
