      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0391.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0391.CPv  $
      *
      *   Rev 2.0   May 26 1999 14:58:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:30:54   P13SP99
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0391  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    24.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!24.07.97! 01 ! NEUERSTELLUNG             !STORCH    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0391                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  insert <<<        VGES TBPA0021                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0391.
          05 PI-DRPA0391-VGES-KEY.
             10 PI-DRPA0391-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
          05 PI-DRPA0391-VGES-DATEN.
             10 PI-DRPA0391-VGES-GUELTIG-BIS   PIC  X(010).
             10 PI-DRPA0391-VGES-VWF           PIC S9(004)  BINARY.
             10 PI-DRPA0391-VGES-VTF           PIC S9(004)  BINARY.
             10 PI-DRPA0391-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
             10 PI-DRPA0391-KNR-WAEHRUNG       PIC S9(004)  BINARY.
             10 PI-DRPA0391-KNR-AUSL           PIC S9(004)  BINARY.
      *
       01 PO-DRPA0391.
          05 PO-DRPA0391-RC                  PIC  X(003).
             88 PO-DRPA0391-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0391-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
