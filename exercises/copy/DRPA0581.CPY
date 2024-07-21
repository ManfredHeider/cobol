      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0581.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0581.CPv  $
      *
      *   Rev 2.1   Dec 10 2007 09:41:30   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:41:30   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   22 Oct 1997 13:12:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0581 FUNKTION prüfen produkt auf         *
      *                           nicht durchstehende provision      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.10.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!31.10.97! 01 ! NEU                       !BOHLINGER *
      * APA00622!10.07.07!2.01! ERWEITERUNG PO-BEREICH    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0581                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0581.
          05 PI-DRPA0581-PZVP-VERSION            PIC S9(004) BINARY.
          05 PI-DRPA0581-DATEN.
             10 PI-DRPA0581-PGEB-GES             PIC S9(004) BINARY.
             10 PI-DRPA0581-PROD-PROD-SCHL       PIC S9(004) BINARY.
             10 PI-DRPA0581-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA0581.
          05 PO-DRPA0581-RC                      PIC  X(003).
             88 PO-DRPA0581-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0581-RC-NOT-FOUND              VALUE 'NFD'.
100707    05 PO-DRPA0581-DATEN.
100707       10 PO-DRPA0581-KNR-NDPROD           PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
