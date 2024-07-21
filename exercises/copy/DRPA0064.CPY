      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0064.CPv  $
      *
      *   Rev 2.1   Mar 27 2003 10:58:08   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.0   Feb 27 1999 14:27:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:54:14   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0064  FUNKTION DELETE STANDARD-OZP       *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0064                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PGTA TBPA0009                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0064.
          05 PI-DRPA0064-LAST-UPDATE        PIC  X(026).
          05 PI-DRPA0064-PGTA-KEY.
             10 PI-DRPA0064-PGEB-GES        PIC S9(004)  BINARY.
210303*      10 PI-DRPA0064-PGTA-TARIF      PIC  X(012).
210303       10 PI-DRPA0064-PGTA-TARIF      PIC  X(015).
             10 PI-DRPA0064-PGTA-GUELT-AB   PIC  X(010).
      *
       01 PO-DRPA0064.
          05 PO-DRPA0064-RC                 PIC  X(003).
             88 PO-DRPA0064-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0064-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0064-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
