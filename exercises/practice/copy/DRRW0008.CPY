      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0008.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/DRRW0008.CPv  $
      *
      *   Rev 2.19   Jun 19 2013 11:39:32   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.18   Jan 29 2013 10:49:02   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.17   Jan 28 2013 17:46:30   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.16   Nov 19 2012 16:50:42   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.15   Apr 26 2010 11:02:34   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.14   Apr 26 2010 09:49:40   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.13   Apr 20 2010 15:01:04   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.12   Apr 16 2010 10:56:20   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.11   Apr 16 2010 10:51:58   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.10   Apr 16 2010 10:43:00   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.9   Apr 15 2010 16:58:10   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.8   Apr 15 2010 15:45:58   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.7   Apr 15 2010 14:20:58   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.6   Apr 15 2010 13:58:32   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.5   Apr 15 2010 11:05:12   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.4   Apr 15 2010 10:50:36   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.3   Apr 15 2010 10:38:14   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.2   Apr 14 2010 15:41:14   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.1   Apr 14 2010 10:58:24   i13az23
      *ARW00071: Überprüfung IBAN
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   10 Mar 1998 17:19:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0008                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRRW0008                    *
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
      * AVB01244!21.04.13! 03 ! IBAN-Eingabe erforderlich !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRRW0008: I B A N - B E R E C H N U N G
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRRW0008.
           05  PI-DRRW0008-DATEN.
               10 PI-DRRW0008-LZ               PIC  X(002).
               10 PI-DRRW0008-BLZ              PIC  X(011).
               10 PI-DRRW0008-KTO              PIC  X(020).
      *
       01  PO-DRRW0008.
           05  PO-DRRW0008-RC                  PIC  X(003).
               88 PO-DRRW0008-RC-OK                        VALUE 'OK '.
      *--- IBAN-Berechnung nicht O.K.
               88 PO-DRRW0008-RC-BER-NOK                   VALUE 'BNK'.
      *--- Land nicht DE oder AT
               88 PO-DRRW0008-RC-LZ-NO-DE-AT               VALUE 'NDA'.
      *--- BLZ in BLZ-Tabelle nicht gefunden
               88 PO-DRRW0008-RC-BLZ-NFD                   VALUE 'NFD'.
      *--- IBAN-Eingabe erforderlich
               88 PO-DRRW0008-RC-ENTER-IBAN                VALUE 'EIB'.
      *--- IBAN-Berechnung nicht möglich/erlaubt
               88 PO-DRRW0008-RC-NO-IBAN-ERL               VALUE 'NIB'.
      *
           05  PO-DRRW0008-DATEN.
               10 PO-DRRW0008-BLZ-KORR         PIC  9(011).
               10 PO-DRRW0008-KTO-KORR         PIC  9(012).
               10 PO-DRRW0008-IBAN             PIC  X(034).
               10 PO-DRRW0008-BIC              PIC  X(011).
      *
      ******************** ENDE COPY-MEMBER ***************************

