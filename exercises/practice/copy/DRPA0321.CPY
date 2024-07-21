      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0321.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0321.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:49:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:35:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0321  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0321                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0102 PMPE         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0321.
          05 PI-DRPA0321-UFKT                 PIC  X(002).
          05 PI-DRPA0321-PMPE-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0321-PMPG-KEY.
             10 PI-DRPA0321-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0321-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA0321.
          05 PO-DRPA0321-RC                   PIC  X(003).
             88 PO-DRPA0321-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0321-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0321-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
