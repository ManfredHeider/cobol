      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0137.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0137.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:42   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0137  FUNKTION Kondition ermitteln       *
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
      *    FÜR DIE FUNKTION DRPA0137                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0137.
          05 PI-DRPA0137-DATEN.
             10 PI-DRPA0137-PGRP-ID       PIC S9(004)    BINARY.
             10 PI-DRPA0137-STICHTAG      PIC  X(010).
             10 PI-DRPA0137-SEL-1         PIC S9(004)    BINARY.
             10 PI-DRPA0137-SEL-2         PIC S9(004)    BINARY.
             10 PI-DRPA0137-SEL-3         PIC S9(004)    BINARY.
             10 PI-DRPA0137-PKON-KOND-ART PIC S9(004)    BINARY.
      *
       01 PO-DRPA0137.
          05 PO-DRPA0137-RC               PIC  X(003).
             88 PO-DRPA0137-RC-OK              VALUE 'OK '.
             88 PO-DRPA0137-RC-NOT-FOUND       VALUE 'NFD'.
          05 PO-DRPA0137-DATEN.
             10 PO-DRPA0137-PKON-KOND-NR  PIC S9(004)    BINARY.
             10 PO-DRPA0137-PKON-KOND-ART PIC S9(004)    BINARY.
             10 PO-DRPA0137-KNR-REGEL     PIC S9(004)    BINARY.
             10 PO-DRPA0137-PKON-SATZ-1   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-SATZ-2   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-SATZ-3   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-SATZ-4   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-SATZ-5   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-SATZ-6   PIC S9(003)V99 PACKED-DECIMAL.
             10 PO-DRPA0137-PKON-WERT-1   PIC S9(009)    BINARY.
             10 PO-DRPA0137-PKON-WERT-2   PIC S9(009)    BINARY.
             10 PO-DRPA0137-PKON-WERT-3   PIC S9(009)    BINARY.
             10 PO-DRPA0137-PKON-WERT-4   PIC S9(009)    BINARY.
             10 PO-DRPA0137-PKON-WERT-5   PIC S9(009)    BINARY.
             10 PO-DRPA0137-PKON-WERT-6   PIC S9(009)    BINARY.
      *
      ******** Ende Copy-Member **************************************
