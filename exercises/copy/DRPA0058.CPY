      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0058.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0058.CPv  $
      *
      *   Rev 2.0   May 26 1999 09:42:30   T13EX21
      *APA00012 - Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0058  FUNKTION INSERT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0058                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PGEB TBPA0008                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0058.
          05 PI-DRPA0058-PGEB-KEY.
             10 PI-DRPA0058-PGEB-GES           PIC S9(004)  BINARY.
          05 PI-DRPA0058-PGEB-DATEN.
             10 PI-DRPA0058-KTY-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0058-KNR-WAEHRUNG       PIC S9(004)  BINARY.
110497*      10 PI-DRPA0058-KTY-WAEHRUNG       PIC S9(004)  BINARY.
             10 PI-DRPA0058-KNR-EXPORT-ART     PIC S9(004)  BINARY.
110497*      10 PI-DRPA0058-KTY-EXPORT-ART     PIC S9(004)  BINARY.
110497       10 PI-DRPA0058-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
110497       10 PI-DRPA0058-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
110497       10 PI-DRPA0058-KNR-AUSL           PIC S9(004)  BINARY.
110497*      10 PI-DRPA0058-KTY-AUSL           PIC S9(004)  BINARY.
110497       10 PI-DRPA0058-KNR-BEWERT-ART     PIC S9(004)  BINARY.
110497*      10 PI-DRPA0058-KTY-BEWERT-ART     PIC S9(004)  BINARY.
      *
       01 PO-DRPA0058.
          05 PO-DRPA0058-RC                  PIC  X(003).
             88 PO-DRPA0058-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0058-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
