      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0740.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0740.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:58:54   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:09:02   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0740  FUNKTION DELETE PRODUKT-TARIF-     *
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
      *    FÜR DIE FUNKTION DRPA0740                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<                                           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0740.
          05 PI-DRPA0740-PGTA-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA0740-PGTA-KEY.
             10 PI-DRPA0740-PGEB-GES        PIC S9(004)  BINARY.
210303*      10 PI-DRPA0740-PGTA-TARIF      PIC  X(012).
210303       10 PI-DRPA0740-PGTA-TARIF      PIC  X(015).
             10 PI-DRPA0740-PGTA-GUELT-AB   PIC  X(010).
      *
       01 PO-DRPA0740.
          05 PO-DRPA0740-RC                 PIC  X(003).
             88 PO-DRPA0740-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0740-RC-NOT-FOUND                 VALUE 'NFD'.
      *
