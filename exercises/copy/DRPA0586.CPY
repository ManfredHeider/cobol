      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0586.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0586.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:42:00   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   22 Oct 1997 13:12:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0586  FUNKTION JOIN-OZP FÜR TABELLE      *
      *                                    PPGR UND PGRP             *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!31.10.97!01.01!NEUERSTELLUNG             !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0586                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0586.
          05 PI-DRPA0586-PROGRAMM            PIC  X(008).
          05 PI-DRPA0586-UFKT                PIC  9(002).
           05  PI-DRPA0586-AUSWAHL-KRITERIEN.
               10 PI-DRPA0586-PGEB-GES           PIC  S9(04) COMP.
               10 PI-DRPA0586-KTY-PGRP-ART       PIC  S9(04) COMP.
      *
       01 PO-DRPA0586.
          05 PO-DRPA0586-RC                  PIC  X(003).
             88 PO-DRPA0586-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0586-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0586-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0586-DATEN.
             10 PO-DRPA0586-PROD-PROD-SCHL     PIC S9(004)     COMP.
             10 PO-DRPA0586-KNR-PGRP-ART-AUSPR PIC S9(004)     COMP.
             10 PO-DRPA0586-KTY-PGRP-ART-AUSPR PIC S9(004)     COMP.
             10 PO-DRPA0586-PGRP-NUMMER        PIC S9(009)     COMP.
             10 PO-DRPA0586-PPGR-GUELT-AB      PIC  X(010).
             10 PO-DRPA0586-PPGR-GUELT-BIS     PIC  X(010).

      ***** Ende-Copy-Member *****************************************
