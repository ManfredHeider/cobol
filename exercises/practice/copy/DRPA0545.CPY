      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0545.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0545.CPv  $
      *
      *   Rev 2.2   Dec 19 2013 11:12:44   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Kollektivverträge
      *
      *   Rev 2.1   Mar 23 2000 11:35:12   T13EX25
      *APA00089, Summe hinzugefügt
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 10:31:56   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   01 Oct 1997 11:28:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0545                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GRUND-PROV BERECH. 2 >>                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    13.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.10.97! 01 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *         !23.03.00! 02 ! Summe hinzugefügt         ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0545                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0545.
          05 PI-DRPA0545-PZVP-VERSION     PIC S9(004) BINARY.
          05 PI-DRPA0545-DATEN.
             10 PI-DRPA0545-BEITRAG       PIC S9(009)V99 PACKED-DECIMAL.
230300       10 PI-DRPA0545-SUMME         PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0545-KOND-GRP.
                COPY DRPA0547.
      *
       01 PO-DRPA0545.
          05 PO-DRPA0545-RC                         PIC  X(003).
             88 PO-DRPA0545-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0545-RC-KEINE-PROV                VALUE 'KPR'.
             88 PO-DRPA0545-RC-REGEL-FALSCH              VALUE 'REF'.
             88 PO-DRPA0545-RC-K-WERT-FEHLT              VALUE 'KWF'.
          05 PO-DRPA0545-DATEN.
             10 PO-DRPA0545-GRUND-PROV    PIC S9(009)V99 PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
