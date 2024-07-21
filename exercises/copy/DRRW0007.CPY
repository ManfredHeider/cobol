      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0007.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/DRRW0007.CPv  $
      *
      *   Rev 2.19   Jun 19 2013 11:39:54   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.18   Jan 29 2013 10:48:56   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.17   Jan 28 2013 17:45:28   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.16   Nov 19 2012 16:50:36   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.15   Apr 26 2010 11:02:28   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.14   Apr 26 2010 09:49:32   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.13   Apr 20 2010 15:00:58   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.12   Apr 16 2010 10:56:16   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.11   Apr 16 2010 10:51:52   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.10   Apr 16 2010 10:42:54   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.9   Apr 15 2010 16:58:04   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.8   Apr 15 2010 15:45:52   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.7   Apr 15 2010 14:20:54   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.6   Apr 15 2010 13:58:28   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.5   Apr 15 2010 11:05:08   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.4   Apr 15 2010 10:50:30   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.3   Apr 15 2010 10:38:08   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.2   Apr 14 2010 15:41:08   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.1   Apr 14 2010 10:58:18   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   10 Mar 1998 17:19:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0007                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRRW0007                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    01.04.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00071!01.04.10! 01 ! NEU                       !K.OBER    *
      * AVB01244!15.11.12! 02 ! SEPA-Anpassung            !B.PRETSCH *
      * AVB01244!05.06.13! 03 ! Nur IBAN-Eingabe          !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRRW0007: I B A N - V A L I D I E R U N G
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRRW0007.
           05  PI-DRRW0007-DATEN.
060613*        10 PI-DRRW0007-LZ               PIC  X(002).
060613*        10 PI-DRRW0007-BLZ              PIC  X(011).
060613*        10 PI-DRRW0007-KTO              PIC  X(020).
               10 PI-DRRW0007-IBAN             PIC  X(034).
      *
       01  PO-DRRW0007.
           05  PO-DRRW0007-RC                  PIC  X(003).
060613*--- IBAN-Validierung nicht O.K.
060613*        88 PO-DRRW0007-RC-VAL-NOK                   VALUE 'VNK'.
      *--- IBAN-Validierung O.K.
               88 PO-DRRW0007-RC-OK                        VALUE 'OK '.
060613*--- IBAN-Land nicht in KTY/KNR (BEZ10) vorhanden
060613         88 PO-DRRW0007-RC-LAND-NFD                  VALUE 'LNF'.
060613*--- IBAN-Länge nicht wie in KTY/KNR (BEZ10) definiert
060613         88 PO-DRRW0007-RC-LAENGE-NOK                VALUE 'LNK'.
      *--- Land nicht DE oder AT
               88 PO-DRRW0007-RC-LZ-NO-DE-AT               VALUE 'NDA'.
      *--- IBAN-Berechnung nicht O.K.
               88 PO-DRRW0007-RC-BER-NOK                   VALUE 'BNK'.
060613*--- IBAN-Prüfziffer stimmt nicht
060613         88 PO-DRRW0007-RC-PZ-NOK                    VALUE 'PNK'.
      *
           05  PO-DRRW0007-DATEN.
060613*        10 PO-DRRW0007-BLZ-KORR         PIC  9(011).
060613*        10 PO-DRRW0007-KTO-KORR         PIC  9(012).
060613         10 PO-DRRW0007-LZ               PIC  X(002).
060613         10 PO-DRRW0007-PZ               PIC  9(002).
060613         10 PO-DRRW0007-BLZ              PIC  X(011).
060613         10 PO-DRRW0007-KTO              PIC  X(012).
               10 PO-DRRW0007-IBAN             PIC  X(034).
      *
      ******************** ENDE COPY-MEMBER ***************************

