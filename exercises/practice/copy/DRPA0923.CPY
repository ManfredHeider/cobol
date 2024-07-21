      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0923.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0923.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:59:44   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:10:34   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:14:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0923  FUNKTION BEENDEN TARIF             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!18.11.99!2.01! NEUERSTELLUNG             ! BOHLINGER*
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0923                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  BEENDEN TARIF <<< TABELLE PGTA TBPA0009              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0923.
          05 PI-DRPA0923-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0923-PGTA-KEY.
             10 PI-DRPA0923-PGEB-GES        PIC S9(004)  BINARY.
210303*      10 PI-DRPA0923-PGTA-TARIF      PIC  X(012).
210303       10 PI-DRPA0923-PGTA-TARIF      PIC  X(015).
             10 PI-DRPA0923-PGTA-GUELT-AB   PIC  X(010).
          05 PI-DRPA0923-PGTA-DATEN.
             10 PI-DRPA0923-PGTA-GUELT-BIS  PIC  X(010).
      *
       01 PO-DRPA0923.
          05 PO-DRPA0923-RC                 PIC  X(003).
             88 PO-DRPA0923-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0923-RC-CHANGED                   VALUE 'CHG'.
             88 PO-DRPA0923-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
