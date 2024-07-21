      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0539.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0539.CPv  $
      *
      *   Rev 2.2   Jun 05 2002 14:22:42   t13ex21
      *APA00191 - PI-Bereich um Laufzeit erweitert
      *
      *   Rev 2.1   Mar 23 2000 16:20:46   T13EX22
      *APA00067: PEG Anpassungen für Badenia
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 10:49:04   T01JM2S
      *APPA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   12 Sep 1997 17:00:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0539                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GSMT-PROV BERECH. 2 >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    01.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!01.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00067!15.03.00!2.01! PPFL-SUMME IN PI-DRPA0539 ! MUND     *
      *         !        !    ! ERGÄNZT                   !          *
      * APA00191!17.05.02!2.02! PI-LAUFZEIT ERGÄNZT       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0539                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0539.
          05 PI-DRPA0539-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0539-DATEN.
             10 PI-DRPA0539-BEITRAG                 PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0539-KOND-GSP.
                COPY DRPA0396.
150300       10 PI-DRPA0539-SUMME                   PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
170502       10 PI-DRPA0539-LAUFZEIT                PIC S9(004) BINARY.
      *
       01 PO-DRPA0539.
          05 PO-DRPA0539-RC                         PIC  X(003).
             88 PO-DRPA0539-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0539-RC-VSTAT-MIT-FEHL             VALUE 'VMF'.
150300*      88 PO-DRPA0539-RC-VSTAT-OHNE-FEHL            VALUE 'VOF'.
          05 PO-DRPA0539-DATEN.
             10 PO-DRPA0539-GSMT-PROV               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
