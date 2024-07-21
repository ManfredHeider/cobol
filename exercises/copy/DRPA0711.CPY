      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0711.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0711.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0711  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0711                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PDPE TBPA0121              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0711.
          05 PI-DRPA0711-PROGRAMM              PIC  X(008).
          05 PI-DRPA0711-UFKT                  PIC  9(002).
          05 PI-DRPA0711-PDPE-DATEN.
             10 PI-DRPA0711-PDPT-ID             PIC S9(15)V COMP-3.
             10 PI-DRPA0711-PDPE-POSITIONSNR    PIC S9(4) COMP.
             10 PI-DRPA0711-PDPE-LAST-UPDATE    PIC X(26).
             10 PI-DRPA0711-PDPE-NT-PBTRG-BRWG  PIC S9(9)V9(2) COMP-3.
             10 PI-DRPA0711-PDPE-PROV-RCK-BRWG  PIC S9(9)V9(2) COMP-3.
             10 PI-DRPA0711-PDPE-EINH           PIC S9(6)V9(2) COMP-3.
             10 PI-DRPA0711-PDPE-NT-PBTRG-AZWG  PIC S9(9)V9(2) COMP-3.
             10 PI-DRPA0711-PDPE-PROV-RCK-AZWG  PIC S9(9)V9(2) COMP-3.
             10 PI-DRPA0711-KNR-AUSZ-WAEHRUNG   PIC S9(4) COMP.
             10 PI-DRPA0711-PDPE-BUCH-RELEVANT  PIC X(1).
      *
       01 PO-DRPA0711.
          05 PO-DRPA0711-RC                   PIC  X(003).
             88 PO-DRPA0711-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0711-RC-DUPLICATE-VALUE            VALUE 'DVA'.

