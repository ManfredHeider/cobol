      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0816.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0816.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:59:26   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:10:28   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jan 1998 12:44:04   P13SP99
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0816  FUNKTION BEENDEN PRODUKT-TARIF-    *
      *                            ZUORDNUNG  FÜR UPPA0031           *
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
      *    FÜR DIE FUNKTION DRPA0816                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> BEENDEN PRODUKT-TARIF-ZUORDNUNG <<<                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0816.
          05 PI-DRPA0816-PGTA-DATEN.
             10 PI-DRPA0816-PGEB-GES         PIC S9(004)  BINARY.
210303*      10 PI-DRPA0816-PGTA-TARIF       PIC  X(012).
210303       10 PI-DRPA0816-PGTA-TARIF       PIC  X(015).
             10 PI-DRPA0816-PGTA-GUELT-AB    PIC  X(010).
             10 PI-DRPA0816-PGTA-LAST-UPDATE PIC  X(026).

          05 PI-DRPA0816-PGTA-DATEN.
             10 PI-DRPA0816-PGTA-GUELT-BIS   PIC  X(010).
      *
      *
       01 PO-DRPA0816.
          05 PO-DRPA0816-RC                  PIC  X(003).
             88 PO-DRPA0816-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0816-RC-NOT-FOUND                 VALUE 'NFD'.
