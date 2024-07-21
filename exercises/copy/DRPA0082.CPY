      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0082.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0082.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:56:04   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0082  Funktion Insert Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0082                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PEWS TBPA0014                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0082.
          05 PI-DRPA0082-PEWS-KEY.
             10 PI-DRPA0082-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0082-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA0082-PEWS-PROGN-DAT   PIC  X(010).
             10 PI-DRPA0082-PVSG-VSTAT-GRP   PIC S9(004) BINARY.
          05 PI-DRPA0082-PEWS-DATEN.
             10 PI-DRPA0082-PEWS-ANZ-ANTRAG  PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-ANZ-VERTR-6 PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-ANZ-VERTR-4 PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-ANZ-VERTR-2 PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-ANZ-VERTR-1 PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-BETR-ANTR   PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-BETR-VERTR  PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-EINH-ANTR   PIC S9(009) BINARY.
             10 PI-DRPA0082-PEWS-EINH-VERTR  PIC S9(009) BINARY.
      *
       01 PO-DRPA0082.
          05 PO-DRPA0082-RC                  PIC  X(003).
             88 PO-DRPA0082-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0082-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
