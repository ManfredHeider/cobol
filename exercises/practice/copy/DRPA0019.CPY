      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:58   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0019  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    13.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !13.08.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0019                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PDSP TBPA0001              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0019.
          05 PI-DRPA0019-PDSP-KEY.
             10 PI-DRPA0019-PGEB-GES        PIC S9(004)  BINARY.
             10 PI-DRPA0019-PDSP-DTA-ART    PIC S9(004)  BINARY.
          05 PI-DRPA0019-PDSP-DATEN.
             10 PI-DRPA0019-KTY-DTA-ART     PIC S9(004)  BINARY.
             10 PI-DRPA0019-PDSP-FREIGABE-JN PIC X(001).
             10 PI-DRPA0019-PDSP-ANZ-FEHL   PIC S9(009)  BINARY.
             10 PI-DRPA0019-PDSP-PSATZ-FEHL PIC S9(003)V99
                                                         PACKED-DECIMAL.
      *
       01 PO-DRPA0019.
          05 PO-DRPA0019-RC                 PIC  X(003).
             88 PO-DRPA0019-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0019-RC-DUPLICATE-KEY             VALUE 'DVA'.
      *
      ***** Ende-Copy-Member *****************************************
