      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0714.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0714.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:28   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0714  Funktion Select Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0714                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PDPE TBPA0121              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0714.
          05 PI-DRPA0714-PDPE-KEY.
             10 PI-DRPA0714-PDPT-ID             PIC S9(15)V COMP-3.
             10 PI-DRPA0714-PDPE-POSITIONSNR    PIC S9(4) COMP.
      *
       01 PO-DRPA0714.
          05 PO-DRPA0714-RC                   PIC  X(003).
             88 PO-DRPA0714-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0714-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0714-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0714-PDPE-DATEN.
             10 PO-DRPA0714-PDPE-LAST-UPDATE    PIC X(26).
             10 PO-DRPA0714-PDPE-NT-PBTRG-BRWG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0714-PDPE-PROV-RCK-BRWG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0714-PDPE-EINH           PIC S9(6)V9(2) COMP-3.
             10 PO-DRPA0714-PDPE-NT-PBTRG-AZWG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0714-PDPE-PROV-RCK-AZWG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0714-KNR-AUSZ-WAEHRUNG   PIC S9(4) COMP.
             10 PO-DRPA0714-PDPE-BUCH-RELEVANT  PIC X(1).
             10 PO-DRPA0714-PDPE-KENNUNG        PIC X(08).

