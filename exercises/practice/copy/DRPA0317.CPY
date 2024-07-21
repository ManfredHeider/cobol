      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0317.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0317.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:47:10   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:35:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0317  Funktion SELECT Standard-OZP       *
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
      *         !18.08.97!    ! pmgh-last -> pmpgh-last   !          *
      *         !        !    ! pmgh-kennung -> pmpgh-ke..!  Opritz  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0317                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT ALL<<      TABELLE PMGH TBPA0101              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0317.
          05 PI-DRPA0317-PMPG-KEY.
             10 PI-DRPA0317-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0317-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA0317.
          05 PO-DRPA0317-RC                   PIC  X(003).
             88 PO-DRPA0317-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0317-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA0317-PDTA-ID              PIC S9(015) COMP-3.
          05 PO-DRPA0317-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PO-DRPA0317-PMPG-VSTAT           PIC  X(004).
180897    05 PO-DRPA0317-PMPGH-LAST-UPDATE    PIC  X(026).
180897    05 PO-DRPA0317-PMPGH-KENNUNG        PIC  X(008).
          05 PO-DRPA0317-PMPG-INHALT.
             10 PO-DRPA0317-PMPG-INHALT-LEN   PIC S9(004) COMP.
             10 PO-DRPA0317-PMPG-INHALT-TEXT  PIC X(3900).
      *
      ****** Ende Copy-Member ****************************************
