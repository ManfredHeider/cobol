      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0739.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0739.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 10:58:54   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:09:00   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0739  FUNKTION UPDATE PRODUKT-TARIF-     *
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
      *    FÜR DIE FUNKTION DRPA0739                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE PRODUKT-TARIF-ZUORDNUNG <<<                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0739.
          05 PI-DRPA0739-PGTA-DATEN-ALT.
             10 PGEB-GES                   PIC S9(004)  BINARY.
210303*      10 PGTA-TARIF                 PIC  X(012).
210303       10 PGTA-TARIF                 PIC  X(015).
             10 PGTA-GUELT-AB              PIC  X(010).
             10 PGTA-LAST-UPDATE           PIC  X(026).

          05 PI-DRPA0739-PGTA-DATEN-NEU.
210303*      10 PGTA-TARIF                  PIC  X(012).
210303       10 PGTA-TARIF                  PIC  X(015).
             10 PGTA-GUELT-AB               PIC  X(010).
             10 PGTA-GUELT-BIS              PIC  X(010).
             10 PROD-PROD-SCHL              PIC S9(004)  BINARY.
      *
      *
       01 PO-DRPA0739.
          05 PO-DRPA0739-RC                 PIC  X(003).
             88 PO-DRPA0739-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0739-RC-UNZULAESSIG               VALUE 'UNZ'.
             88 PO-DRPA0739-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
          05 PO-DRPA0738-MELDUNG.
             10 PO-DRPA0739-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0739-MELDUNG-NR         PIC S9(004) BINARY.
