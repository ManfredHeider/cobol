      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1120.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1120.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:52:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:58:08   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1120  FUNKTION JOIN-OZP FÜR TABELLE      *
      *                                    PPGR UND PGRP             *
      *                           FüR ALLE GESELLSCHAFTEN            *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    05.11.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!05.11.96!01.01!NEUERSTELLUNG             !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1120                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1120.
          05 PI-DRPA1120-PROGRAMM            PIC  X(008).
          05 PI-DRPA1120-UFKT                PIC  9(002).
           05  PI-DRPA1120-AUSWAHL-KRITERIEN.
               10 PI-DRPA1120-KTY-PGRP-ART       PIC  S9(04) COMP.
      *
       01 PO-DRPA1120.
          05 PO-DRPA1120-RC                  PIC  X(003).
             88 PO-DRPA1120-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1120-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1120-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1120-DATEN.
             10 PO-DRPA1120-PGEB-GES           PIC S9(004)     COMP.
             10 PO-DRPA1120-PROD-PROD-SCHL     PIC S9(004)     COMP.
             10 PO-DRPA1120-KNR-PGRP-ART-AUSPR PIC S9(004)     COMP.
             10 PO-DRPA1120-KTY-PGRP-ART-AUSPR PIC S9(004)     COMP.
             10 PO-DRPA1120-PGRP-NUMMER        PIC S9(009)     COMP.
             10 PO-DRPA1120-PPGR-GUELT-AB      PIC  X(010).
             10 PO-DRPA1120-PPGR-GUELT-BIS     PIC  X(010).

      ***** Ende-Copy-Member *****************************************
