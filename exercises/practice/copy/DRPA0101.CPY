      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0101.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0101.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:58:04   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0101  Funktion Select Standard-OZP       *
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
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0101                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PROZ TBPA0018                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0101.
          05 PI-DRPA0101-PROZ-KEY.
             10 PI-DRPA0101-PROT-ID           PIC S9(015)
                                              PACKED-DECIMAL.
             10 PI-DRPA0101-PROZ-ZEILE        PIC S9(004) BINARY.
      *
       01 PO-DRPA0101.
          05 PO-DRPA0101-RC                   PIC  X(003).
             88 PO-DRPA0101-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0101-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0101-PROZ-DATEN.
             10 PO-DRPA0101-PROZ-TEXT         PIC  X(080).
             10 PO-DRPA0101-PROZ-BETRAG-JN    PIC  X(001).
             10 PO-DRPA0101-PROZ-BETRAG       PIC S9(009)V99
                                              PACKED-DECIMAL.
          05 PO-DRPA0101-PROZ-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0101-PROZ-KENNUNG         PIC  X(008).
      *
      ****** Ende Copy-Member *****************************************
