      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  november 1995
      *     Kopf aus:  n:profilekopfcbl.dat
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 Atlas Dienstleistungs Gmbh
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0060.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0060.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:46:48   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   may 26 1999 09:42:30   T13EX21
      *APA00012 - NEUERSTELLUNG
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0060  FUNKTION DELETE STANDARD-OZP       *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0060                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PGEB TBPA0008                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0060.
          05 PI-DRPA0060-LAST-UPDATE           PIC  X(026).
          05 PI-DRPA0060-PGEB-KEY.
             10 PI-DRPA0060-PGEB-GES           PIC S9(004)  BINARY.
110497*      10 PI-DRPA0060-PGEB-GES-GUELT-AB  PIC  X(010).
      *
       01 PO-DRPA0060.
          05 PO-DRPA0060-RC                 PIC  X(003).
             88 PO-DRPA0060-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0060-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0060-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
