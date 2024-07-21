      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0126.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0126.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:34   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0126  FUNKTION GRUNDPROVISION BERECHNEN  *
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
      *    FÜR DIE FUNKTION DRPA0126                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0126.
          05 PI-DRPA0126-PZVP-VERSION     PIC S9(004)    BINARY.
          05 PI-DRPA0126-DATEN.
             10 PI-DRPA0126-BEITRAG       PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0126-SUMME         PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0126-KONDITION.
                15 PI-DRPA0126-PKON-KOND-NR   PIC S9(004)   BINARY.
                15 PI-DRPA0126-PKON-KOND-ART  PIC S9(004)   BINARY.
                15 PI-DRPA0126-KNR-REGEL      PIC S9(004)   BINARY.
                15 PI-DRPA0126-PKON-SATZ-1    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-SATZ-2    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-SATZ-3    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-SATZ-4    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-SATZ-5    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-SATZ-6    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0126-PKON-WERT-1    PIC S9(009)   BINARY.
                15 PI-DRPA0126-PKON-WERT-2    PIC S9(009)   BINARY.
                15 PI-DRPA0126-PKON-WERT-3    PIC S9(009)   BINARY.
                15 PI-DRPA0126-PKON-WERT-4    PIC S9(009)   BINARY.
                15 PI-DRPA0126-PKON-WERT-5    PIC S9(009)   BINARY.
                15 PI-DRPA0126-PKON-WERT-6    PIC S9(009)   BINARY.
      *
       01 PO-DRPA0126.
          05 PO-DRPA0126-RC               PIC  X(003).
             88 PO-DRPA0126-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0126-RC-KEINE-PROV                VALUE 'KPR'.
             88 PO-DRPA0126-RC-REGEL-FALSCH              VALUE 'REF'.
             88 PO-DRPA0126-RC-K-WERT-FEHLT              VALUE 'KWF'.
          05 PO-DRPA0126-DATEN.
             10 PO-DRPA0126-GRUND-PROV    PIC S9(009)V99 PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
