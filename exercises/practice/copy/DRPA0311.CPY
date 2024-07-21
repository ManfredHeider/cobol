      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0311.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0311.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:43:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:34:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0311  Funktion DELETE Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Opritz                                          *
      *    DATUM:    05.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01 ! NEUERSTELLUNG             ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drpa0311                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  delete  <<<       TABELLE PMPG TBPA0100              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0311.
          05 PI-DRPA0311-PMPG-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0311-PMPG-KEY.
             10 PI-DRPA0311-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0311-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA0311.
          05 PO-DRPA0311-RC                   PIC  X(003).
             88 PO-DRPA0311-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0311-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0311-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
