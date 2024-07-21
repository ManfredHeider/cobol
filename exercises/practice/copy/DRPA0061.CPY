      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0061.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:13:42   T13EX21
      *APA00012 - Anpassung an Tabellener-
      *weiterung
      *
      *   Rev 2.0   Feb 27 1999 14:27:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:53:58   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0061  FUNKTION SELECT STANDARD-OZP       *
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
      * AFPA0014!11.04.97! 02 ! ANPASSUNG AN TABELLEN-    !BOHLINGER/*
      *         !        !    !              ERWEITERUNG  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0061                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGEB TBPA0008                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0061.
          05 PI-DRPA0061-PGEB-KEY.
             10 PI-DRPA0061-PGEB-GES            PIC S9(004)  BINARY.
      *
       01 PO-DRPA0061.
          05 PO-DRPA0061-RC                     PIC  X(003).
             88 PO-DRPA0061-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0061-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0061-PGEB-DATEN.
             10 PO-DRPA0061-KTY-GES             PIC S9(004)  BINARY.
             10 PO-DRPA0061-KNR-WAEHRUNG        PIC S9(004)  BINARY.
             10 PO-DRPA0061-KTY-WAEHRUNG        PIC S9(004)  BINARY.
             10 PO-DRPA0061-KNR-EXPORT-ART      PIC S9(004)  BINARY.
             10 PO-DRPA0061-KTY-EXPORT-ART      PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KNR-VERTRIEB-GES    PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KTY-VERTRIEB-GES    PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KNR-AUSL            PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KTY-AUSL            PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KNR-BEWERT-ART      PIC S9(004)  BINARY.
110497       10 PO-DRPA0061-KTY-BEWERT-ART      PIC S9(004)  BINARY.
          05 PO-DRPA0061-PGEB-LAST-UPDATE       PIC  X(026).
          05 PO-DRPA0061-PGEB-KENNUNG           PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************

