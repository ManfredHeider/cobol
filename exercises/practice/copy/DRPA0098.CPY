      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0098.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0098.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:57:46   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0098  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    19.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !19.08.96! 01 ! Neuerstellung             ! Prömpeler*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0098                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PROZ TBPA0018              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0098.
          05 PI-DRPA0098-PROZ-KEY.
             10 PI-DRPA0098-PROT-ID           PIC S9(015)
                                              PACKED-DECIMAL.
             10 PI-DRPA0098-PROZ-ZEILE        PIC S9(004) BINARY.
          05 PI-DRPA0098-PROZ-DATEN.
             10 PI-DRPA0098-PROZ-TEXT         PIC  X(080).
             10 PI-DRPA0098-PROZ-BETRAG-JN    PIC  X(001).
             10 PI-DRPA0098-PROZ-BETRAG       PIC S9(009)V99
                                              PACKED-DECIMAL.
      *
       01 PO-DRPA0098.
          05 PO-DRPA0098-RC                   PIC  X(003).
             88 PO-DRPA0098-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0098-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ***** Ende-Copy-Member *****************************************
