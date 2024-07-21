      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0314.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0314.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:45:28   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:34:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0314  Funktion Insert Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0314                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMGH TBPA0101              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0314.
          05 PI-DRPA0314-UFKT                 PIC  X(002).
          05 PI-DRPA0314-PMPG-KEY.
             10 PI-DRPA0314-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0314-PMPG-VERSION      PIC S9(004) COMP.
      *
          05 PI-DRPA0314-PDTA-ID              PIC S9(015) COMP-3.
          05 PI-DRPA0314-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PI-DRPA0314-PMPG-VSTAT           PIC  X(004).
          05 PI-DRPA0314-PMPG-INHALT.
             10 PI-DRPA0314-PMPG-INHALT-LEN   PIC S9(004) COMP.
             10 PI-DRPA0314-PMPG-INHALT-TEXT  PIC X(3900).
      *
       01 PO-DRPA0314.
          05 PO-DRPA0314-RC                   PIC  X(003).
             88 PO-DRPA0314-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0314-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
