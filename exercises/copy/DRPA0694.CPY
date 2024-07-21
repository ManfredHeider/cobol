      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0694.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0694.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:20:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 17:00:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0694  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.Wilhelm                                       *
      *    DATUM:    10.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.12.97! 01 ! NEUERSTELLUNG             ! D.Wilhelm*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0694                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  MASSEN-UPDATE <<  TABELLE PMPG TBPA0100              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0694.
          05 PI-DRPA0694-VSTAT-KLASSE         PIC  X.
          05 PI-DRPA0694-PMPG-KEY.
             10 PI-DRPA0694-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0694-PMPG-VERSION      PIC S9(004) COMP.
      *
          05 PI-DRPA0694-PDTA-ID              PIC S9(015) COMP-3.
          05 PI-DRPA0694-PDTA-ID-NEU          PIC S9(015) COMP-3.
      *
       01 PO-DRPA0694.
          05 PO-DRPA0694-RC                   PIC  X(003).
             88 PO-DRPA0694-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0694-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0694-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0694-PMPG-ANZ-UPD         PIC S9(004) COMP.
      *
      ****** Ende Copy-Member ****************************************
