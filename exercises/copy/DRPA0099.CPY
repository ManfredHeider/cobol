      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0099.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0099.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:57:50   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0099  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    19.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
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
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0099                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PROZ TBPA0018                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0099.
          05 PI-DRPA0099-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0099-PROZ-KEY.
             10 PI-DRPA0099-PROT-ID           PIC S9(015)
                                              PACKED-DECIMAL.
             10 PI-DRPA0099-PROZ-ZEILE        PIC S9(004) BINARY.
          05 PI-DRPA0099-PROZ-DATEN.
             10 PI-DRPA0099-PROZ-TEXT         PIC  X(080).
             10 PI-DRPA0099-PROZ-BETRAG-JN    PIC  X(001).
             10 PI-DRPA0099-PROZ-BETRAG       PIC S9(009)V99
                                              PACKED-DECIMAL.
      *
       01 PO-DRPA0099.
          05 PO-DRPA0099-RC                   PIC  X(003).
             88 PO-DRPA0099-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0099-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0099-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
