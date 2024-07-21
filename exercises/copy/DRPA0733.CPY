      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0733.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0733.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:58:50   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:08:52   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      ****************************************************************
      *    COPY-MEMBER: DRPA0733 FUNKTION AUFBEREITEN DES            *
      *                                   PRODUKTGERTARIFS           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    21.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA000xx!21.10.99! 01 !NEU                        !BOHLINGER *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0733                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0733.
          05 PI-DRPA0733-DATEN.
               10 PI-DRPA0733-PGEB-GES             PIC 9(002).
210303*        10 PI-DRPA0733-PGTA-TARIF           PIC X(012).
210303         10 PI-DRPA0733-PGTA-TARIF           PIC X(015).
               10 PI-DRPA0733-STICHTAG             PIC X(010).
               10 PI-DRPA0733-S-STICHTAG           PIC X(001).
      *
      *
       01 PO-DRPA0733.
          05  PO-DRPA0733-RC                     PIC  X(003).
              88 PO-DRPA0733-RC-OK                        VALUE 'OK '.
              88 PO-DRPA0733-RC-OK-TARIF-GEN              VALUE 'GEN'.
              88 PO-DRPA0733-RC-UNZULAESSIG               VALUE 'UNZ'.

          05 PO-DRPA0733-DATEN.
210303*        10 PO-DRPA0733-PGTA-TARIF           PIC X(012).
210303         10 PO-DRPA0733-PGTA-TARIF           PIC X(015).
               10 PO-DRPA0733-S-TARIF-VORH         PIC X(001).
               10 PO-DRPA0733-PROD-PROD-SCHL       PIC S9(004) binary.

      *
      ******** Ende Copy-Member **************************************
