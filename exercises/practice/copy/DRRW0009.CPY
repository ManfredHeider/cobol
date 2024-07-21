      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0009.CPY  $
      *    $Revision:   2.18  $
      *
      *    $Log:   L:/mf/CPY/DRRW0009.CPv  $
      *
      *   Rev 2.18   Jan 29 2013 10:49:06   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.17   Jan 28 2013 17:46:42   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.16   Nov 19 2012 16:50:48   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.15   Apr 26 2010 11:02:16   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.14   Apr 26 2010 09:48:52   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.13   Apr 20 2010 15:00:42   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.12   Apr 16 2010 10:56:04   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.11   Apr 16 2010 10:51:40   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.10   Apr 16 2010 10:42:44   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.9   Apr 15 2010 16:57:52   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.8   Apr 15 2010 15:45:38   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.7   Apr 15 2010 14:20:42   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.6   Apr 15 2010 13:58:16   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.5   Apr 15 2010 11:04:54   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.4   Apr 15 2010 10:50:20   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.3   Apr 15 2010 10:37:56   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.2   Apr 14 2010 15:40:52   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.1   Apr 14 2010 10:58:06   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   10 Mar 1998 17:19:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0009                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRRW0009                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    01.04.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00071!01.04.10! 01 ! NEU                       !OBER      *
      * AVB01244!15.11.12! 02 ! SEPA-Anpassung (jetzt ist !B.PRETSCH *
      *         !        !    ! DRRW0009 -> DRRW0010)     !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRRW0009: I B A N - S P L I T T U N G
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRRW0009.
           05  PI-DRRW0009-DATEN.
               10 PI-DRRW0009-IBAN             PIC  X(034).
      *
       01  PO-DRRW0009.
           05  PO-DRRW0009-RC                  PIC  X(003).
               88 PO-DRRW0009-RC-OK                        VALUE 'OK '.
      *--- IBAN-Berechnung nicht O.K.
               88 PO-DRRW0009-RC-BER-NOK                   VALUE 'BNK'.
      *--- IBAN-Validierung nicht O.K. (IBAN-Neu ungleich IBAN-Alt)
               88 PO-DRRW0009-RC-VAL-NOK                   VALUE 'NOK'.
      *--- Land nicht DE oder AT
               88 PO-DRRW0009-RC-LZ-NO-DE-AT               VALUE 'NDA'.
      *--- IBAN-Land nicht in KTY/KNR (BEZ10) vorhanden
               88 PO-DRRW0009-RC-LAND-NFD                  VALUE 'LNF'.
      *--- IBAN-Länge nicht wie in KTY/KNR (BEZ10) definiert
               88 PO-DRRW0009-RC-LAENGE-NOK                VALUE 'LNK'.
      *--- BLZ in BLZ-Tabelle nicht gefunden
               88 PO-DRRW0009-RC-BLZ-NFD                   VALUE 'NFD'.
           05  PO-DRRW0009-DATEN.
               10 PO-DRRW0009-LZ               PIC  X(002).
               10 PO-DRRW0009-KTO              PIC  X(020).
               10 PO-DRRW0009-BLZ              PIC  X(011).
               10 PO-DRRW0009-BIC              PIC  X(011).
      *
      ******************** ENDE COPY-MEMBER ***************************

