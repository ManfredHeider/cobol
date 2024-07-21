      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0065.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0065.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:29:22   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Mar 27 2003 10:58:10   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.0   Feb 27 1999 14:27:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:54:18   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0065  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      * APA00712!11.03.08!2.02! ERWEITERUNG UM MANDANT    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0065                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGTA TBPA0009                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0065.
          05 PI-DRPA0065-PGTA-KEY.
             10 PI-DRPA0065-PGEB-GES        PIC S9(004)  BINARY.
210303*      10 PI-DRPA0065-PGTA-TARIF      PIC  X(012).
210303       10 PI-DRPA0065-PGTA-TARIF      PIC  X(015).
             10 PI-DRPA0065-PGTA-GUELT-AB   PIC  X(010).
      *
       01 PO-DRPA0065.
          05 PO-DRPA0065-RC                 PIC  X(003).
             88 PO-DRPA0065-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0065-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0065-PGTA-DATEN.
             10 PO-DRPA0065-PGTA-GUELT-BIS  PIC  X(010).
             10 PO-DRPA0065-PROD-PROD-SCHL  PIC S9(004)  BINARY.
          05 PO-DRPA0065-PGTA-LAST-UPDATE   PIC  X(026).
          05 PO-DRPA0065-PGTA-KENNUNG       PIC  X(008).
110308    05 PO-DRPA0065-KNR-MANDANT        PIC S9(004)  BINARY.
      *
      ****** Ende Copy-Member ****************************************
