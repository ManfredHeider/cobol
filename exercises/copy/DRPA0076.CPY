      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0076.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0076.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:55:18   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0076  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0076                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PZVP TBPA0012                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0076.
          05 PI-DRPA0076-LAST-UPDATE        PIC  X(026).
          05 PI-DRPA0076-PZVP-KEY.
             10 PI-DRPA0076-PZVP-VERFAHREN  PIC  X(020).
             10 PI-DRPA0076-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA0076-PROD-PROD-SCHL  PIC S9(004) BINARY.
             10 PI-DRPA0076-PZVP-GUELT-AB   PIC  X(010).
      *
       01 PO-DRPA0076.
          05 PO-DRPA0076-RC                 PIC  X(003).
             88 PO-DRPA0076-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0076-RC-CHANGED                  VALUE 'CHA'.
             88 PO-DRPA0076-RC-NOT-FOUND                VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
