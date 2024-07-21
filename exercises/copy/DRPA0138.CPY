      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0138.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0138.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:48   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0138  FUNKTION Kondition ermitteln Liste *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    10.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0138                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0138.
          05 PI-DRPA0138-DATEN.
             10 PI-DRPA0138-PGRP-ID           PIC S9(004)  BINARY.
             10 PI-DRPA0138-STICHTAG          PIC  X(010).
             10 PI-DRPA0138-SEL-1             PIC S9(004)  BINARY.
             10 PI-DRPA0138-SEL-2             PIC S9(004)  BINARY.
             10 PI-DRPA0138-SEL-3             PIC S9(004)  BINARY.
      *
       01 PO-DRPA0138.
          05 PO-DRPA0138-RC                   PIC  X(003).
             88 PO-DRPA0138-RC-OK                          VALUE 'OK '.
             88 PO-DRPA0138-RC-LEER                        VALUE 'LER'.
          05 PO-DRPA0138-DATEN.
             10 PO-DRPA0138-ANZAHL            PIC S9(004)  BINARY.
             10 PO-DRPA0138-TABELLE OCCURS 10.
                15 PO-DRPA0138-PKON-KOND-NR   PIC S9(004)   BINARY.
                15 PO-DRPA0138-PKON-KOND-ART  PIC S9(004)   BINARY.
                15 PO-DRPA0138-KNR-REGEL      PIC S9(004)   BINARY.
                15 PO-DRPA0138-PKON-SATZ-1    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-SATZ-2    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-SATZ-3    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-SATZ-4    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-SATZ-5    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-SATZ-6    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PO-DRPA0138-PKON-WERT-1    PIC S9(009)   BINARY.
                15 PO-DRPA0138-PKON-WERT-2    PIC S9(009)   BINARY.
                15 PO-DRPA0138-PKON-WERT-3    PIC S9(009)   BINARY.
                15 PO-DRPA0138-PKON-WERT-4    PIC S9(009)   BINARY.
                15 PO-DRPA0138-PKON-WERT-5    PIC S9(009)   BINARY.
                15 PO-DRPA0138-PKON-WERT-6    PIC S9(009)   BINARY.
      *
      ******** Ende Copy-Member **************************************
