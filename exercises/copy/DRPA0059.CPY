      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  november 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 Atlas Dienstleistungs Gmbh
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0059.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0059.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:35:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   feb 19 1997 10:06:38   P13PVCS
      *Initial Revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0059  FUNKTION UPDATE STANDARD-OZP       *
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
      * AFPA0014!11.04.97! 02 ! ERWEITERUNG DER TABELLE   !BOHLINGER/*
      *         !        !    !                 TBPA0008  ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0059                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        PGEB TBPA0008                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0059.
          05 PI-DRPA0059-LAST-UPDATE           PIC  X(026).
          05 PI-DRPA0059-PGEB-KEY.
             10 PI-DRPA0059-PGEB-GES           PIC S9(004)  BINARY.
110497*      10 PI-DRPA0059-PGEB-GES-GUELT-AB  PIC X(010).
110497       10 PI-DRPA0059-PGEB-GES-GUELT-AB  PIC X(026).
          05 PI-DRPA0059-PGEB-DATEN.
110497       10 PI-DRPA0059-PGEB-GES-GUELT-BIS PIC X(010).
             10 PI-DRPA0059-KTY-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0059-KNR-WAEHRUNG       PIC S9(004)  BINARY.
110497*      10 PI-DRPA0059-KTY-WAEHRUNG       PIC S9(004)  BINARY.
             10 PI-DRPA0059-KNR-EXPORT-ART     PIC S9(004)  BINARY.
110497*      10 PI-DRPA0059-KTY-EXPORT-ART     PIC S9(004)  BINARY.
110497       10 PI-DRPA0059-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
110497       10 PI-DRPA0059-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
110497       10 PI-DRPA0059-KNR-AUSL           PIC S9(004)  BINARY.
110497*      10 PI-DRPA0059-KTY-AUSL           PIC S9(004)  BINARY.
110497       10 PI-DRPA0059-KNR-BEWERT-ART     PIC S9(004)  BINARY.
110497*      10 PI-DRPA0059-KTY-BEWERT-ART     PIC S9(004)  BINARY.
      *
       01 PO-DRPA0059.
          05 PO-DRPA0059-RC                  PIC  X(003).
             88 PO-DRPA0059-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0059-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0059-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
