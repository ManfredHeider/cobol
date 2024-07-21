      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0247.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0247.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:00:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0247  FUNKTION SELECT AKTUELLER SATZ     *
      *                                           STANDARD-OZP       *
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
      *         !25.08.97!    ! Änderungen aufgrund von   ! Storch   *
      *         !        !    ! Tabellenänderungen        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0247                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-AKT <<<    PGVG TBPA0020                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0247.
          05 PI-DRPA0247-PGVG-KEY.
             10 PI-DRPA0247-PGEB-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0247-KNR-VERTRIEB-GES    PIC S9(004)  BINARY.
      *
       01 PO-DRPA0247.
          05 PO-DRPA0247-RC                     PIC  X(003).
             88 PO-DRPA0247-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0247-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0247-PGVG-DATEN.
250897*      10 PO-DRPA0247-PGVG-GUELT-AB       PIC  X(010).
250897       10 PO-DRPA0247-PGVG-GUELT-AB       PIC  X(026).
             10 PO-DRPA0247-PGVG-GUELT-BIS      PIC  X(010).
             10 PO-DRPA0247-KTY-GES             PIC S9(004)  BINARY.
             10 PO-DRPA0247-KTY-VERTRIEB-GES    PIC S9(004)  BINARY.
             10 PO-DRPA0247-PGVG-KONTO        PIC  X(010).
             10 PO-DRPA0247-PGVG-EDV-GKTO-EIN PIC S9(003)V USAGE COMP-3.
             10 PO-DRPA0247-PGVG-EDV-GKTO-AUS PIC S9(003)V USAGE COMP-3.
          05 PO-DRPA0247-PGVG-LAST-UPDATE       PIC  X(026).
          05 PO-DRPA0247-PGVG-KENNUNG           PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
