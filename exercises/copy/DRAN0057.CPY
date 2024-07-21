      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0057.CPY  $
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/DRAN0057.CPv  $
      *
      *   Rev 2.17   Oct 07 2013 09:42:46   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *Advocard (Transfer DVAG -> Prod.Gebr)
      *
      *   Rev 2.16   Jun 19 2009 09:16:20   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.15   Mar 24 2009 10:56:46   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.14   Mar 11 2009 16:12:36   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.13   May 28 2008 08:50:16   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.12   02 Jan 2008 14:49:26   t13ae23
      *AAN00730: Allfinanz Stufe 1 im eingereichten Geschäft-
      *POA-
      *AM-VERS Mandantenkennzeichen
      *
      *   Rev 2.11   28 Dec 2007 08:36:00   t13ae23
      *FAN00796: Kein Mandatenkennzeichen
      *
      *   Rev 2.10   27 Dec 2007 17:06:14   t13ae23
      *FAN00796: Kein Mandatenkennzeichen
      *
      *   Rev 2.9   21 Dec 2007 15:07:12   t13ae23
      *AAN00730: Allfinanz Stufe 1 im eingereichten Geschäft -
      *Täg
      *liche FTP Antragsdaten POA AM-Vers
      *
      *   Rev 2.8   Jul 19 2007 12:12:46   t13ae34
      *AAN00753: Funktionen und Dialoge: Erweiterung der PIN auf 9 
      *Stellen
      *
      *   Rev 2.7   Jan 11 2005 08:05:38   t13ae34
      *AAN00529: POA AMVers. Optimierung Quote - Batchteil
      *
      *   Rev 2.6   Nov 26 2004 15:41:20   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote - Batchteil
      *
      *   Rev 2.5   Jul 16 2003 16:44:04   t13ae34
      *AAN00447-POA Versicherungsbeginn überschreibbar 
      *
      *   Rev 2.4   Jun 05 2002 13:25:32   t13ae34
      *AAN00357 - POA; Endekennzeichen '$'
      *
      *   Rev 2.3   Jun 05 2002 13:20:54   t13ae34
      *AAN00357 - POA; Satzverschiebung DRAN0057
      *
      *   Rev 2.2   May 03 2002 12:37:22   t13ae34
      *AAN00357 - POA-AMV; hier: mit Prüfziffer PIN
      *
      *   Rev 2.1   Apr 23 2002 17:00:06   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    POA-AMV: FILETRANSFERDATEN VON DVAG-HOST ZUR AMV
      ****************************************************************
      *    COPY-MEMBER: DRAN0057                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!10.04.02!2.1 !NEU                        !HEIDER    *
      * AAN00357!05.06.02!2.3 !ATTRIBUTE VERSCHOBEN       !HEIDER    *
      * AAN00357!05.06.02!2.4 !ENDEKENNZEICHEN '$'        !HEIDER    *
      * AAN00447!08.07.03!2.5 ! POA-AMV: VERS.BEGINN ÜBER. HEIDER    *
      * AAN00529!26.11.04!2.6 !OPTIMIERUNG POA-QUOTE      !RAHNER    *
      * AAN00529!01.12.04!2.7 !ZAHLWEISE 2 STELLIG        !HEIDER    *
      * AAN00753!21.06.07!2.8 !PIN-ERWEIERUNG AUF 9-ST.   !HEIDER    *
      * AAN00730!21.12.07!2.9 !ALLFINANZ                  !RAHNER    *
      * FAN00796!27.12.07!2.10!ÜBERGABE AAN00730 RÜCKGÄN. !RAHNER    *
      * AAN00730!02.01.08!2.9 !ALLFINANZ                  !RAHNER    *
      * AAN00801!11.03.08!2.13!KENNZ. WIDERSPRUCH AMBGSM  !HEIDER    *
      * AAN00843!11.03.09!2.14!VERGRÖßERN XGDV-FELD       !RAHNER    *
      * AAN00884!17.06.09!2.16!- KZ FÜR TARIFÄNDERUNG     !          *
      *         !        !    !- FELDER FÜR DIREKT POA    !RAHNER    *
      * AAN01073!07.10.13!2.17!SEPA                       !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  POA-FT-DVAG-AMV.

      *       VERSIONSNUMMER
           05 POA-FT-DVAG-AMV-VERSION              PIC 9(4).
      *       DIE 10-STELLLIGE VORGANGSNUMMER BESTEHT AUS EINER
      *       9-ST. VORSCHLAGSNUMMER UND EINER 1-ST. LAUFENDEN NUMMER
           05 POA-FT-DVAG-AMV-VORGANGSNR.
                10 POA-FT-DVAG-AMV-HVANTNUM        PIC X(9).
                10 POA-FT-DVAG-AMV-HVANTNUM-LFD    PIC X(1).
      *
      *       KENNZEICHEN VERTRIEBS-GESELLSCHAFT (13=DVAG 66=ALLFINANZ)
           05 POA-FT-DVAG-VB-GES                   PIC X(2).
      *
      *       PIF/PIN
           05 POA-FT-DVAG-AMV-PIF                  PIC X(9).
           05 POA-FT-DVAG-AMV-PIF-9 REDEFINES
              POA-FT-DVAG-AMV-PIF                  PIC 9(9).
           05 POA-FT-DVAG-AMV-PIFPZ                PIC X(2).
      *
      *       PIF NUMMER IN BARCODE VERSCHLÜSSELT
           05 POA-FT-DVAG-AMV-BARCODE              PIC X(10).
      *
      *       KENNZEICHEN FÜR SCHNELLANTRAG
           05 POA-FT-DVAG-AMV-SCHNELLANTRAG        PIC X(1).
      *
      *       KENNZEICHEN HANDSCHRIFTLICHE ÄNDERUNG J/N
           05 POA-FT-DVAG-AMV-HAND                 PIC X(1).

      *       ABSCHLUSSVERMITTLER
           05 POA-FT-DVAG-AMV-VDABVERM             PIC X(7).
           05 POA-FT-DVAG-AMV-VDABVERM-9 REDEFINES
              POA-FT-DVAG-AMV-VDABVERM             PIC 9(7).

      *       PRÜFZIFFER ABSCHLUSSVERMITTLER
           05 POA-FT-DVAG-AMV-VDABVERMPZ           PIC X(1).

      *       DIREKTIONSNUMMER BEI DVAG
           05 POA-FT-DVAG-AMV-VDDIREKT             PIC X(3).
           05 POA-FT-DVAG-AMV-VDDIREKT-9 REDEFINES
              POA-FT-DVAG-AMV-VDDIREKT             PIC 9(3).

      *       PRODUKTE 1-5
           05 POA-FT-DVAG-AMV-PROD-TAB.
      *       PRODUKTE 1-5
              10 POA-FT-DVAG-AMV-PROD-TABO OCCURS 5.
      *       DIREKTIONSNUMMER BEI DVAG
                 15 POA-FT-DVAG-AMV-VDPRODKEY      PIC X(4).
                 15 POA-FT-DVAG-AMV-VDPRODKEY-9 REDEFINES
                    POA-FT-DVAG-AMV-VDPRODKEY      PIC 9(4).
      *       VERICHERUNGSBEGINN FORMAT TT.MM.JHJJ
           05 POA-FT-DVAG-AMV-VERSBEGINN           PIC X(10).
      *       VERICHERUNGSABLAUF FORMAT TT.MM.JHJJ
           05 POA-FT-DVAG-AMV-VERSABLAUF           PIC X(10).
      *       ZAHLWEISE
      *    05 POA-FT-DVAG-AMV-ZAHLWEISE            PIC X(1).
           05 POA-FT-DVAG-AMV-ZAHLWEISE            PIC X(2).
      *       BANKLEITZAHL
           05 POA-FT-DVAG-AMV-BANKLEITZAHL         PIC 9(8).
      *       KONTONUMMER
           05 POA-FT-DVAG-AMV-KONTONR              PIC 9(10).
      *       IBAN
           05 POA-FT-DVAG-AMV-IBAN                 PIC X(34).
      *       BIC
           05 POA-FT-DVAG-AMV-BIC                  PIC X(11).
      *       KM-STAND-BEGINN
           05 POA-FT-DVAG-AMV-KM-STAND-BEGIN       PIC 9(6).
      *       FAHRGESTELLNUMMER
           05 POA-FT-DVAG-AMV-FAHRGESTELLNR        PIC X(17).
      *       AMTLICHES-KENNZEICHEN
           05 POA-FT-DVAG-AMV-AMTL-KZ              PIC X(11).
      *       JÄHRLICHE FAHRLEISTUNG
           05 POA-FT-DVAG-AMV-JAHR-FAHRL           PIC 9(6).
      *
      *       KENNZ. WIDERSPRUCH SCHADENBEARBEITUNG DURCH AMGSM
      *       'J' (=WIDERSPRUCH) ODER 'N' (=KEIN WIDERSPRUCH)
           05 POA-FT-DVAG-AMV-WIDERSPRUCH          PIC X(1).

      *       XGDV-DATEN; ENDEKENNZEICHEN = '$'
      *    05 POA-FT-DVAG-AMV-XGDV                 PIC X(9999).
           05 POA-FT-DVAG-AMV-XGDV                 PIC X(25000).

