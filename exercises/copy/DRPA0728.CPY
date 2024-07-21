      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0728.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA0728.CPv  $
      *
      *   Rev 2.9   Mar 24 2020 06:52:16   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.8   May 14 2018 11:19:34   t13ae54
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * GHO
      *
      *   Rev 2.7   Apr 11 2018 13:54:06   t13ae37
      *FPA01636: Anpassung Satzlänge nach Verlängerung Rech-Nr.
      *
      *   Rev 2.6   Oct 02 2015 09:14:48   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.5   May 04 2015 11:08:12   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.4   Mar 10 2014 16:46:24   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Jul 26 2013 11:41:32   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Dec 10 2007 10:38:20   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Feb 17 2000 17:41:36   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.0   May 26 1999 15:15:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0728  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.OPRITZ                                        *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!22.12.97! 01 ! NEUERSTELLUNG             ! J.OPRITZ *
      * APA00081!01.02.00!2.03! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      * APA00622!04.06.07!2.04! ERWEITERUNG UM 2 STELLEN  ! STORCH   *
      *         !        !    ! WEGEN KNR-VGES            !          *
      * APA01539!26.07.13!2.3 ! NEUE LÄNGE 557 F. KALYPSO ! RAHNER   *
      * APA01932!31.03.15!2.4 ! NEUE LÄNGE 570 F. LUCY    ! KÖPRÜCÜ  *
      *         !02.10.15!2.5 ! NEUE LÄNGE 572 (CKV)      ! KÖPRÜCÜ  *
      * FPA01636!11.04.18!2.6 ! NEUE LÄNGE 590 (RECH-NR.) ! BUCH     *
      * APA02579!26.04.18!2.7 ! FrWI NEUE DB2-ATTRIBUTE   ! HOFMANN  *
      *         !        !    ! SATZLÄNGE ERHÖHT          !          *
      * APA02884!19.11.19!2.8 ! SATZLÄNGE ERHÖHT          ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0728                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0728.
          05 PI-DRPA0728-UFKT               PIC  9(002).
110418*   05 PI-DRPA0728-DATEN              PIC  X(590).
260418*   05 PI-DRPA0728-DATEN              PIC  X(596).
191119    05 PI-DRPA0728-DATEN              PIC  X(598).
      *
       01 PO-DRPA0728.
          05 PO-DRPA0728-RC                 PIC  X(003).
             88 PO-DRPA0728-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************

