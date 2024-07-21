      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0309.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0309.cpv  $
      *
      *   Rev 2.1   Aug 23 1999 11:35:56   T13EX21
      *APA00049 - Gruppenfeld definiert
      *
      *   Rev 2.0   Feb 27 1999 14:28:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:42:16   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0046  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    05.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !        !    !                           !          *
      * APA00012!08.07.99!    ! PI-DRPA0309-PMPG-DATEN    ! STORCH   *
      *         !        !    ! DEFINIERT.                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0308                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMPG TBPA0100              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0309.
          05 PI-DRPA0309-UFKT                 PIC  X(002).
080799    05 PI-DRPA0309-PMPG-DATEN.
            10 PI-DRPA0309-PMPG-KEY.
               15 PI-DRPA0309-PMPG-ID           PIC S9(015) COMP-3.
               15 PI-DRPA0309-PMPG-VERSION      PIC S9(004) COMP.
      *
            10 PI-DRPA0309-PDTA-ID              PIC S9(015) COMP-3.
            10 PI-DRPA0309-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
            10 PI-DRPA0309-PMPG-VSTAT           PIC  X(004).
            10 PI-DRPA0309-PMPG-INHALT.
               15 PI-DRPA0309-PMPG-INHALT-LEN   PIC S9(004) COMP.
               15 PI-DRPA0309-PMPG-INHALT-TEXT  PIC X(3900).
      *
       01 PO-DRPA0309.
          05 PO-DRPA0309-RC                   PIC  X(003).
             88 PO-DRPA0309-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0309-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
