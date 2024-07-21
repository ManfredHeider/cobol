      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0125.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0125.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:26   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0125  FUNKTION GESAMTPROVISION BERECHNEN *
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
      *    FÜR DIE FUNKTION DRPA0125                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0125.
          05 PI-DRPA0125-PZVP-VERSION     PIC S9(004)    BINARY.
          05 PI-DRPA0125-DATEN.
             10 PI-DRPA0125-BEITRAG       PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0125-SUMME         PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0125-KONDITION.
                15 PI-DRPA0125-PKON-KOND-NR   PIC S9(004)   BINARY.
                15 PI-DRPA0125-PKON-KOND-ART  PIC S9(004)   BINARY.
                15 PI-DRPA0125-KNR-REGEL      PIC S9(004)   BINARY.
                15 PI-DRPA0125-PKON-SATZ-1    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-SATZ-2    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-SATZ-3    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-SATZ-4    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-SATZ-5    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-SATZ-6    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0125-PKON-WERT-1    PIC S9(009)   BINARY.
                15 PI-DRPA0125-PKON-WERT-2    PIC S9(009)   BINARY.
                15 PI-DRPA0125-PKON-WERT-3    PIC S9(009)   BINARY.
                15 PI-DRPA0125-PKON-WERT-4    PIC S9(009)   BINARY.
                15 PI-DRPA0125-PKON-WERT-5    PIC S9(009)   BINARY.
                15 PI-DRPA0125-PKON-WERT-6    PIC S9(009)   BINARY.
      *
       01 PO-DRPA0125.
          05 PO-DRPA0125-RC               PIC  X(003).
             88 PO-DRPA0125-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0125-RC-KEINE-PROV                VALUE 'KPR'.
             88 PO-DRPA0125-RC-REGEL-FALSCH              VALUE 'REF'.
             88 PO-DRPA0125-RC-K-WERT-FEHLT              VALUE 'KWF'.
             88 PO-DRPA0125-RC-PROV-ZU-GROSS             VALUE 'PZG'.
          05 PO-DRPA0125-DATEN.
             10 PO-DRPA0125-GESAMT-PROV   PIC S9(009)V99 PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
