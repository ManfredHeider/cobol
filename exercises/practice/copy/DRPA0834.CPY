      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0834.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0834.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:33:36   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:45:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0834  FUNKTION                           *
      *                           ERMITTELN RATENZAHLUNGSZUSCHLAG    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    06.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00020!06.02.98! 01  !NEU                       !BOHLINGER *
      */AAN00167!        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0834                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0834.
          05 PI-DRPA0834-DATEN.
             10 PI-DRPA0834-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA0834-STICHTAG            PIC  X(010).
             10 PI-DRPA0834-KNR-ZAHLWEISE       PIC S9(004) BINARY.
             10 PI-DRPA0834-S-ZAHLWEISE         PIC  X(001).
      *
       01 PO-DRPA0834.
          05 PO-DRPA0834-RC                     PIC  X(003).
             88 PO-DRPA0834-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0834-RC-NOT-FOUND             VALUE 'NFD'.
          05 PO-DRPA0834-DATEN.
             10 PO-DRPA0834-RZAHLUNG-ZUSCHLAG   PIC S9(003)V999.
      ******** Ende Copy-Member **************************************
