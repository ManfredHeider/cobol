      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0303.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0303.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   16 Nov 1998 10:53:26   T01JM2S
      *PEG-Produktionsuebergabe
      *
      *   Rev 1.0   May 26 1997 15:21:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0303  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Storch                                          *
      *    DATUM:    28.05.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0010!28.05.97! 01 ! Neuerstellung             ! Storch   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0303                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGEB TBPA0008                      *
      *         mit PGEB_GES und PGEB_GES_GUELT_BIS=K-MAX-DAT-EURO   *
      *                                                              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0303.
          05 PI-DRPA0303-PGEB-KEY.
             10 PI-DRPA0303-PGEB-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0303-PGEB-GES-GUELT-BIS  PIC  X(010).
      *
       01 PO-DRPA0303.
          05 PO-DRPA0303-RC                     PIC  X(003).
             88 PO-DRPA0303-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0303-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0303-PGEB-DATEN.
             10 PO-DRPA0303-PGEB-GES-GUELT-AB   PIC  X(010).
             10 PO-DRPA0303-KTY-GES             PIC S9(004)  BINARY.
             10 PO-DRPA0303-KNR-WAEHRUNG        PIC S9(004)  BINARY.
             10 PO-DRPA0303-KTY-WAEHRUNG        PIC S9(004)  BINARY.
             10 PO-DRPA0303-KNR-EXPORT-ART      PIC S9(004)  BINARY.
             10 PO-DRPA0303-KTY-EXPORT-ART      PIC S9(004)  BINARY.
             10 PO-DRPA0303-KNR-VERTRIEB-GES    PIC S9(004)  BINARY.
             10 PO-DRPA0303-KTY-VERTRIEB-GES    PIC S9(004)  BINARY.
             10 PO-DRPA0303-KNR-AUSL            PIC S9(004)  BINARY.
             10 PO-DRPA0303-KTY-AUSL            PIC S9(004)  BINARY.
             10 PO-DRPA0303-KNR-BEWERT-ART      PIC S9(004)  BINARY.
             10 PO-DRPA0303-KTY-BEWERT-ART      PIC S9(004)  BINARY.
          05 PO-DRPA0303-PGEB-LAST-UPDATE       PIC  X(026).
          05 PO-DRPA0303-PGEB-KENNUNG           PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
