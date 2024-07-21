      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0747.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0747.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:30:34   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Jan 04 2000 13:09:04   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0747  FUNKTION INSERT PRODUKT            *
      *                                     FÜR UPPA0118             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!18.11.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA00712!12.03.08!2.01! ERWEITERUNG PI-BEREICH    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0747                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT PRODUKT <<<                                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0747.
          05 PI-DRPA0747-PROD-KEY.
             10 PI-DRPA0747-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0747-PROD-PROD-SCHL     PIC S9(004)  BINARY.
             10 PI-DRPA0747-PROD-BEGINN        PIC  X(010).
          05 PI-DRPA0747-PROD-DATEN.
             10 PI-DRPA0747-PROD-ENDE          PIC  X(010).
             10 PI-DRPA0747-PROD-BEZ           PIC  X(040).
             10 PI-DRPA0747-PROD-BEZ-KURZ      PIC  X(010).
120308       10 PI-DRPA0747-KTY-PROD-VGES      PIC S9(004)  BINARY.
120308       10 PI-DRPA0747-KNR-PROD-VGES      PIC S9(004)  BINARY.
      *
       01 PO-DRPA0747.
          05 PO-DRPA0747-RC                    PIC  X(003).
             88 PO-DRPA0747-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0747-RC-DUPLICATE-VALUE      VALUE 'DVA'.
             88 PO-DRPA0747-RC-UNZULAESSIG          VALUE 'UNZ'.
      *
          05 PO-DRPA0747-MELDUNG.
             10 PO-DRPA0747-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0747-MELDUNG-NR         PIC S9(004) BINARY.

      ****** Ende Copy-Member ****************************************
