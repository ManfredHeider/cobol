      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0315.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0315.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:46:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:35:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0315  Funktion Update Standard-OZP       *
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
      *         !18.08.97!    ! pmgh-last -> pmpgh-last   ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPa0315                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PMGH TBPA0101              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0315.
180897    05 PI-DRPA0315-PMPGH-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0315-PMPG-KEY.
             10 PI-DRPA0315-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0315-PMPG-VERSION      PIC S9(004) COMP.
      *
          05 PI-DRPA0315-PDTA-ID              PIC S9(015) COMP-3.
          05 PI-DRPA0315-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PI-DRPA0315-PMPG-VSTAT           PIC  X(004).
          05 PI-DRPA0315-PMPG-INHALT.
             10 PI-DRPA0315-PMPG-INHALT-LEN   PIC S9(004) COMP.
             10 PI-DRPA0315-PMPG-INHALT-TEXT  PIC X(3900).
      *
       01 PO-DRPA0315.
          05 PO-DRPA0315-RC                   PIC  X(003).
             88 PO-DRPA0315-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0315-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0315-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
