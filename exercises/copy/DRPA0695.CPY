      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0695.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0695.CPv  $
      *
      *   Rev 2.1   Jul 22 2013 08:14:08   t13ae39
      *FPA01304: Fehler beim DTA trennen
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:21:04   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 17:00:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0695  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    10.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.10.97! 01 ! NEUERSTELLUNG             ! D.WILHELM*
      * FPA01304!10.07.13!2.1 ! PO-BEREICH ERWEITERT      ! A.STRAUSS*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0695                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  MASSEN-UPDATE <<< TABELLE      TBPA0102 PMPE         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0695.
          05 PI-DRPA0695-VSTAT-KLASSE         PIC  X.
          05 PI-DRPA0695-PMPG-KEY.
             10 PI-DRPA0695-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0695-PMPG-VERSION      PIC S9(004) COMP.
      *
          05 PI-DRPA0695-PDTA-ID              PIC S9(015) COMP-3.
          05 PI-DRPA0695-PDTA-ID-NEU          PIC S9(015) COMP-3.
          05 PI-DRPA0695-PGEB-GES             PIC S9(004) COMP.
      *
       01 PO-DRPA0695.
          05 PO-DRPA0695-RC                   PIC  X(003).
             88 PO-DRPA0695-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0695-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0695-RC-NOT-FOUND                  VALUE 'NFD'.
100713    05 PO-DRPA0695-PMPG-ANZ-UPD         PIC S9(004) COMP.
      *
      ****** Ende Copy-Member ****************************************
