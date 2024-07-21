      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0331.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0331.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:55:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:38:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0331  Funktion DELETE Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Opritz                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0328                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0104 PDIF         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0331.
          05 PI-DRPA0331-PDIF-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0331-PMPG-KEY.
             10 PI-DRPA0331-PDTA-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0331-PDIF-PGV          PIC S9(004) COMP.
             10 PI-DRPA0331-PDIF-VSTAT        PIC  X(004).
      *
       01 PO-DRPA0331.
          05 PO-DRPA0331-RC                   PIC  X(003).
             88 PO-DRPA0331-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0331-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0331-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
