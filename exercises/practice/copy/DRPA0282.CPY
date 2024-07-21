      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0282.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0282.CPv  $
      *
      *   Rev 2.3   Apr 05 2018 23:31:14   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.2   Jun 10 2008 10:30:22   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Mar 27 2003 10:58:30   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.0   Feb 27 1999 14:28:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:43:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0282                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GÜLTIGER PROD-SCHL  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      * APA00712!07.04.08!2.02! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA02579!16.02.18!2.3 ! MANDANT ENTFERNT          ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0282                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0282.
          05 PI-DRPA0282-DATEN.
             10 PI-DRPA0282-PGEB-GES         PIC S9(004) BINARY.
210303*      10 PI-DRPA0282-PGTA-TARIF       PIC  X(012).
210303       10 PI-DRPA0282-PGTA-TARIF       PIC  X(015).
             10 PI-DRPA0282-STICHTAG         PIC  X(010).
      *
       01 PO-DRPA0282.
          05 PO-DRPA0282-RC                  PIC  X(003).
             88 PO-DRPA0282-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0282-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0282-DATEN.
             10 PO-DRPA0282-PROD-PROD-SCHL   PIC S9(004) BINARY.
160218*      10 PO-DRPA0282-KNR-MANDANT      PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

