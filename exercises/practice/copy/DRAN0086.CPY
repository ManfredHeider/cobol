      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0086.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRAN0086.CPv  $
      *
      *   Rev 2.13   Dec 18 2015 08:58:26   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.12   Oct 21 2015 14:40:28   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.11   Oct 07 2013 09:42:50   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *Advocard (Transfer DVAG -> Prod.Gebr)
      *
      *   Rev 2.10   02 Jan 2008 10:40:08   t13ae23
      *AAN00730: Allfinanz Stufe 1 im eingereichten Geschäft
      *
      *   Rev 2.9   28 Dec 2007 08:36:32   t13ae23
      *FAN00796: Kein Mandatenkennzeichen
      *
      *   Rev 2.8   27 Dec 2007 17:06:44   t13ae23
      *FAN00796: Kein Mandatenkennzeichen
      *
      *   Rev 2.7   21 Dec 2007 10:35:14   t13ae23
      *AAN00730: VRA - Allfinanz Stufe 1 im eingereichten Geschäft
      *- tägl FTP Antragsdaten  POA Advocard
      *
      *   Rev 2.6   Feb 01 2005 15:29:22   t13ae34
      *AAN00544: DB2+Batch: PoA Optimierung Advocard
      *
      *   Rev 2.5   Dec 30 2003 08:56:30   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.4   Dec 12 2003 11:47:00   t13ae34
      *AAN00471 - DRAN0086 aus AAN00422
      *
      *   Rev 2.3   Oct 31 2003 10:53:12   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.2   Oct 07 2003 09:43:38   t13ae34
      *AAN00422 - Feld 'Direktion' in Schnittstelle DVAG an Advocard
      *
      *   Rev 2.1   Aug 04 2003 08:06:42   t13ae23
      *AAN00422 - Elektron. Antrag Advocard -
      *
      *   Rev 2.0   Mar 18 2002 09:16:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  POA-ADVOCARD: FILETRANSFERDATEN VON DVAG-HOST ZUR ADVOCARD
      ****************************************************************
      *    COPY-MEMBER: DRAN0086                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!01.08.03!2.1 !NEU                        !RAHNER    *
      * AAN00422!31.10.03!2.2 !HINZUFÜGEN DIREKTION VB    !RAHNER    *
      *         !        !    !VERGRÖßERN BLACKBOX        !          *
      * AAN00422!30.12.03!2.3 !AKTIVIEREN ÄNDERUNGEN      !          *
      *         !        !    !31.10.30                   !RAHNER    *
      * AAN00544!20.01.05!2.6 !ADVOCARD OPTIMIERUNG QUOTE !HEIDER    *
      * AAN00730!21.12.07!2.7 !ALLFINANZ + VPLUS RAUS     !RAHNER    *
      * FAN00796!27.12.07!2.8 !ÜBERGABE AAN00730 RÜCKGÄN. !RAHNER    *
      * AAN00730!02.01.08!2.9 !ALLFINANZ                  !RAHNER    *
      * AAN01073!07.10.13!2.10!SEPA                       !RAHNER    *
      * AAN01223!08.12.15!2.13!BEGINN/ABLAUF ÄNDERBAR     !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 FT-DVAG-ADVOCARD.

      **** VORSCHLAGSNUMMER
          05 FT-DVAG-ADVC-ANTRAGSNUMMER            PIC X(9).
      *
      **** ZUGEORDNETE PIN + PRÜFZIFFER
          05 FT-DVAG-ADVC-PIN                      PIC 9(07).
      *
      *** DATEN ZUM ABSCHLUßVERMITTLER
          05 FT-DVAG-ADVC-VB-DATEN.
             10 FT-DVAG-ADVC-VBNR-ABVERM           PIC 9(07).
             10 FT-DVAG-ADVC-VBNR-ABVERM-PZ        PIC X(01).
             10 FT-DVAG-ADVC-VB-ZUNAME             PIC X(35).
             10 FT-DVAG-ADVC-VB-VORNAME            PIC X(35).
             10 FT-DVAG-ADVC-VB-ZUSATZ1            PIC X(40).
             10 FT-DVAG-ADVC-VB-ZUSATZ2            PIC X(40).
             10 FT-DVAG-ADVC-VB-STRASSE            PIC X(46).
             10 FT-DVAG-ADVC-VB-LAND               PIC X(03).
             10 FT-DVAG-ADVC-VB-PLZ                PIC X(08).
             10 FT-DVAG-ADVC-VB-ORT                PIC X(40).
             10 FT-DVAG-ADVC-VB-TELEFON            PIC X(17).
             10 FT-DVAG-ADVC-VB-FAX                PIC X(17).
             10 FT-DVAG-ADVC-VB-MAIL               PIC X(50).
             10 FT-DVAG-ADVC-VM-NUMMER             PIC 9(07).
             10 FT-DVAG-ADVC-VM-NUMMER-PZ          PIC X(01).
             10 FT-DVAG-ADVC-VM-ZUNAME             PIC X(26).
             10 FT-DVAG-ADVC-VM-VORNAME            PIC X(26).

      *** KENNZEICHEN HANDSCHRIFTLICHE ÄNDERUNGEN J/N
          05 FT-DVAG-ADVC-MANUELLE-AEND            PIC X(01).
      *
      *** DATUM DER ERFASSUNG DES ANTRAGS BEI DVAG
          05 FT-DVAG-ADVC-ANTR-ERFT-DAT            PIC 9(08).
      *
      *** DIREKTION DES ABSCHLUßVERMITTLERS
          05 FT-DVAG-ADVC-VB-DIR                   PIC X(03).

      *>>>>>
          05 FT-DVAG-ADVC-KONTONR                  PIC 9(12).
          05 FT-DVAG-ADVC-BLZ                      PIC 9(8).
          05 FT-DVAG-ADVC-IBAN                     PIC X(34).
          05 FT-DVAG-ADVC-BIC                      PIC X(11).
          05 FT-DVAG-ADVC-AMTLKZ                   PIC X(12).
      *   1=JÄHRLICH, 2=HALBJÄHRLICH, 4=VIERTELJÄHRLICH, 8=MONATLICH
          05 FT-DVAG-ADVC-ZAHLW                    PIC 9(1).
      *   'N'=OHNE V-PLUS-ANTRAG, WENN V-PLUS ERWARTET
      *   05 FT-DVAG-ADVC-V-PLUS                   PIC X.
      *   KZ FÜR DVAG (13) OD. ALLFINANF(66)
          05 FT-DVAG-ADVC-VBGES                    PIC 9(2).
      *
          05 FT-DVAG-ADVC-BEGINN                   PIC 9(08).
          05 FT-DVAG-ADVC-ABLAUF                   PIC 9(08).
      *<<<<<
      *   05 FILLER                                PIC X(112).
          05 FILLER                                PIC X(096).
      *** XGDV-DATEN BEGINN UND ENDE KENNZEICHEN $
      *   05 FT-DVAG-ADVC-BLACKBOX                 PIC X(9624).
      *   05 FT-DVAG-ADVC-BLACKBOX                 PIC X(32460).
          05 FT-DVAG-ADVC-BLACKBOX                 PIC X(20000).


