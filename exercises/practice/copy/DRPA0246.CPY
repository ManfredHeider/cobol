      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0246.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0246.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:00:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0246  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.03.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.03.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0246                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGVG TBPA0020                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0246.
          05 PI-DRPA0246-PGVG-KEY.
             10 PI-DRPA0246-PGEB-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0246-PGVG-GUELT-AB       PIC  X(010).
             10 PI-DRPA0246-KNR-VERTRIEB-GES    PIC S9(004)  BINARY.
      *
       01 PO-DRPA0246.
          05 PO-DRPA0246-RC                     PIC  X(003).
             88 PO-DRPA0246-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0246-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0246-PGVG-DATEN.
             10 PO-DRPA0246-PGVG-GUELT-BIS      PIC  X(010).
             10 PO-DRPA0246-KTY-GES             PIC S9(004)  BINARY.
             10 PO-DRPA0246-KTY-VERTRIEB-GES    PIC S9(004)  BINARY.
             10 PO-DRPA0246-PGVG-KONTO        PIC  X(010).
             10 PO-DRPA0246-PGVG-EDV-GKTO-EIN PIC S9(003)V USAGE COMP-3.
             10 PO-DRPA0246-PGVG-EDV-GKTO-AUS PIC S9(003)V USAGE COMP-3.
          05 PO-DRPA0246-PGVG-LAST-UPDATE       PIC  X(026).
          05 PO-DRPA0246-PGVG-KENNUNG           PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
