      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1267.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1267.CPv  $
      *
      *   Rev 2.2   Mar 27 2003 11:00:16   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jul 26 1999 10:38:16   T13AE34
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
      *    COPY-MEMBER: DRPA1267                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PGTA-TARIF PG-TARIFE ZU DVAG-PROD*
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97!    ! NEU                       ! WOLFF    *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1267                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1267.
          05 PI-DRPA1267-PROGRAMM            PIC  X(008).
          05 PI-DRPA1267-UFKT                PIC  9(002).
          05 PI-DRPA1267-DATEN.
             10 PI-DRPA1267-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1267-PROD-PROD-SCHL   PIC S9(004)  BINARY.
      *
       01 PO-DRPA1267.
          05 PO-DRPA1267-RC                  PIC  X(003).
             88 PO-DRPA1267-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1267-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1267-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1267-DATEN.
210303*      10 PO-DRPA1267-PGTA-TARIF       PIC  X(012).
210303       10 PO-DRPA1267-PGTA-TARIF       PIC  X(015).
             10 PO-DRPA1267-PGTA-GUELT-AB    PIC  X(010).
             10 PO-DRPA1267-PGTA-GUELT-BIS   PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
