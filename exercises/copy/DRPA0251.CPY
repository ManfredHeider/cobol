      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0251.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0251.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   07 Aug 1998 11:30:54   T13AE34
      *AAN00167 - AAT00001
      *
      *   Rev 1.1   04 Aug 1997 10:10:30   T13EX22
      *AFPA0010 - Neuerstellung Funkt. Ermitteln Kond. zu Ges
      *
      *   Rev 1.0   01 Aug 1997 11:30:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0251  FUNKTION KONDITION ERMITTELN       *
      *                           WIEDERVORLAGEDATUM                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!30.06.97! 01 !NEU                        !BOHLINGER *
      * AFPA0023!22.09.97!    !ANPASSEN WG. äNDERUNG TAB. !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0251                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0251.
          05 PI-DRPA0251-DATEN.
             10 PI-DRPA0251-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0251-PROD-PROD-SCHL     PIC S9(004)  BINARY.
             10 PI-DRPA0251-STICHTAG           PIC  X(010).
             10 PI-DRPA0251-KNR-PGRP-ART       PIC S9(004)  BINARY.
             10 PI-DRPA0251-KTY-PGRP-ART       PIC S9(004)  BINARY.
      *
       01 PO-DRPA0251.
          05 PO-DRPA0251-RC                    PIC  X(003).
             88 PO-DRPA0251-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0251-RC-NOT-FOUND            VALUE 'NFD'.
          05 PO-DRPA0251-DATEN.
             10 PO-DRPA0251-PGRP-ID            PIC S9(004)  BINARY.
             10 PO-DRPA0251-PGRP-ALPHA         PIC  X(010).
             10 PO-DRPA0251-PGRP-NUMMER        PIC S9(009)  BINARY.
220997       10 PO-DRPA0251-KNR-PGRP-ART-AUSPR PIC S9(004)  BINARY.
