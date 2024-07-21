      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1266.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1266.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 11:00:18   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jul 26 1999 10:38:14   T13AE34
      *APA00046-Daisy-Batch
      *
      *   Rev 2.0   Mar 30 1999 08:44:18   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1266                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PG-TARIFE ERMITTELN ZU DVAG-PRODSCHL AMV>*
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.06.99!2.0 ! NEU                       ! HEIDER   *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1266                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1266.
          05 PI-DRPA1266-DATEN.
             10 PI-DRPA1266-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA1266-PROD-PROD-SCHL   PIC S9(004)  BINARY.
             10 PI-DRPA1266-STICHTAG         PIC  X(010).
      *
       01 PO-DRPA1266.
          05 PO-DRPA1266-RC                  PIC  X(003).
             88 PO-DRPA1266-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1266-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1266-DATEN.
210303*   10 PO-DRPA1266-TARIF      PIC  X(012).
210303       10 PO-DRPA1266-TARIF      PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
