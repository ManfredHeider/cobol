      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0962.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0962.CPv  $
      *
      *   Rev 2.1   Nov 24 1999 09:46:06   T13AE34
      *AAN00234-Bündelung Sach DRPA0962
      *
      *   Rev 2.0   Jun 23 1999 15:38:56   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:18:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0962                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0962                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                         *
      *    DATUM:    23.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00234!23.11.99!2.1 ! DRPA0962                  ! HEIDER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
      ****************************************************************
      *
       01 PI-DRPA0962.
          05 PI-DRPA0962-DATEN.
             10 PI-DRPA0962-PGEB-GES          PIC S9(004)  BINARY.
             10 PI-DRPA0962-PROD-PROD-SCHL    PIC S9(004)  BINARY.
             10 PI-DRPA0962-STICHTAG          PIC  X(010).
             10 PI-DRPA0962-KNR-PGRP-ART      PIC S9(004)  BINARY.
             10 PI-DRPA0962-KTY-PGRP-ART      PIC S9(004)  BINARY.
      *
       01 PO-DRPA0962.
          05 PO-DRPA0962-RC                   PIC  X(003).
             88 PO-DRPA0962-RC-OK                  VALUE 'OK '.
             88 PO-DRPA0962-RC-LEER                VALUE 'LER'.
          05 PO-DRPA0962-DATEN.
             10 PO-DRPA0962-ANZAHL            PIC S9(004)  BINARY.
             10 PO-DRPA0962-TABELLE OCCURS 10.
                15 PO-DRPA0962-PGRP-ID            PIC S9(004)  BINARY.
                15 PO-DRPA0962-PGRP-ALPHA         PIC  X(010).
                15 PO-DRPA0962-PGRP-NUMMER        PIC S9(009)  BINARY.
220997          15 PO-DRPA0962-KNR-PGRP-ART-AUSPR PIC S9(004)  BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
