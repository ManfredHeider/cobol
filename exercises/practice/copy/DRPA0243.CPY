      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0243.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0243.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:59:56   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0243  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.03.97! 01 ! NEUERSTELLUNG             !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0243                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PGVG TBPA0020                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0243.
          05 PI-DRPA0243-PGVG-KEY.
             10 PI-DRPA0243-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0243-PGVG-GUELT-AB      PIC  X(010).
             10 PI-DRPA0243-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
          05 PI-DRPA0243-PGVG-DATEN.
             10 PI-DRPA0243-PGVG-GUELT-BIS     PIC  X(010).
             10 PI-DRPA0243-KTY-GES            PIC S9(004)  BINARY.
             10 PI-DRPA0243-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
             10 PI-DRPA0243-PGVG-KONTO        PIC  X(010).
             10 PI-DRPA0243-PGVG-EDV-GKTO-EIN PIC S9(003)V USAGE COMP-3.
             10 PI-DRPA0243-PGVG-EDV-GKTO-AUS PIC S9(003)V USAGE COMP-3.
      *
       01 PO-DRPA0243.
          05 PO-DRPA0243-RC                  PIC  X(003).
             88 PO-DRPA0243-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0243-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
