      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0738.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0738.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:58:52   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:08:58   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0738  FUNKTION INSERT PRODUKT-TARIF-     *
      *                           ZUORDNUNG FÜR UPPA0031             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!08.11.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0738                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT PRODUKT-TARIFZUORDNUNG <<<<                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0738.
          05 PI-DRPA0738-PGTA-KEY.
             10 PI-DRPA0738-PGEB-GES        PIC S9(004)  BINARY.
210303*      10 PI-DRPA0738-PGTA-TARIF      PIC  X(012).
210303       10 PI-DRPA0738-PGTA-TARIF      PIC  X(015).
             10 PI-DRPA0738-PGTA-GUELT-AB   PIC  X(010).
          05 PI-DRPA0738-PGTA-DATEN.
             10 PI-DRPA0738-PGTA-GUELT-BIS  PIC  X(010).
             10 PI-DRPA0738-PROD-PROD-SCHL  PIC S9(004)  BINARY.
      *
       01 PO-DRPA0738.
          05 PO-DRPA0738-RC                 PIC  X(003).
             88 PO-DRPA0738-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0738-RC-DUPLICATE-VALUE           VALUE 'DVA'.
             88 PO-DRPA0738-RC-UNZULAESSIG               VALUE 'UNZ'.
      *
          05 PO-DRPA0738-MELDUNG.
             10 PO-DRPA0738-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0738-MELDUNG-NR         PIC S9(004) BINARY.

      *
      ****** Ende Copy-Member ****************************************
