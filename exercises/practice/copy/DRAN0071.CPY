      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0071.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0071.CPv  $
      *
      *   Rev 2.4   Nov 19 2015 08:04:40   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.3   Nov 04 2015 13:32:02   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.2   02 Jan 2008 13:00:04   t13ae23
      *AAN00730: Allfinanz Stufe 1 im eingereichten Geschäft -
      *Täg
      *liche FTP Antragsdaten BADENIA POA + Autom. Erf.
      *
      *   Rev 2.1   May 14 2003 13:42:44   t13ae23
      *AAN00409 - VIA BADENIA -
      *
      *   Rev 2.0   Mar 18 2002 09:15:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    VIA-BADENIA: FILETRANSFERDATEN VON DVAG-HOST ZUR BADENIA
      ****************************************************************
      *    COPY-MEMBER: DRAN0071                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!14.05.03!2.1 !NEU                        !RAHNER    *
      * AAN00730!02.01.08!2.2 !ALLFINANZ                  !RAHNER    *
      * AAN01211!21.10.15!2.3 !BLACKBOX AUF 30000         !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 VIA-FT-DVAG-BADENIA.
 
      **** VORSCHLAGSNUMMER
          05 VIA-FT-DVAG-BAD-ANTRAGSNUMMER         PIC 9(17).
      *
      **** BEI PRODUKT IMMOFIT WERDEN 2 SÄTZE ÜBERTRAGEN D.H.
          05 VIA-FT-DVAG-BAD-PIN-1                 PIC 9(07).
          05 VIA-FT-DVAG-BAD-PIN-2                 PIC 9(07).
      *
      *** KENNZEICHEN HANDSCHRIFTLICHE ÄNDERUNGEN J/N
          05 VIA-FT-DVAG-BAD-MANUELLE-AEND.
             10 VIA-FT-DVAG-BAD-KZ-AEND-BSV        PIC X(01).
             10 VIA-FT-DVAG-BAD-KZ-AEND-TARIF      PIC X(01).
             10 VIA-FT-DVAG-BAD-KZ-AEND-FRST       PIC X(01).
             10 VIA-FT-DVAG-BAD-KZ-AEND-LEG        PIC X(01).
             10 VIA-FT-DVAG-BAD-KZ-AEND-VL1        PIC X(01).
             10 VIA-FT-DVAG-BAD-KZ-AEND-VL2        PIC X(01).
      *
      *** USER-ID DVAG DER ANTRAG ERFAßT HAT
          05 VIA-FT-DVAG-BAD-USER-ID               PIC X(07).
      *
      *** DATUM DER ERFASSUNG DES ANTRAGS BEI DVAG
          05 VIA-FT-DVAG-BAD-ANTR-ERFT-DAT         PIC 9(08).
      *
      *** VERSIONSNUMMER PC-DATEN
          05 VIA-FT-DVAG-BAD-VERSIONSNR            PIC 9(04).
      *
      *** DATEN ZUM ABSCHLUßVERMITTLER
          05 VIA-FT-DVAG-BAD-VB-DATEN.
             10 VIA-FT-DVAG-BAD-VBNR-ABVERM        PIC 9(07).
             10 VIA-FT-DVAG-BAD-VB-ANREDE          PIC 9(01).
             10 VIA-FT-DVAG-BAD-VB-ZUNAME          PIC X(35).
             10 VIA-FT-DVAG-BAD-VB-VORNAME         PIC X(35).
             10 VIA-FT-DVAG-BAD-VB-ZUSATZ1         PIC X(40).
             10 VIA-FT-DVAG-BAD-VB-ZUSATZ2         PIC X(40).
             10 VIA-FT-DVAG-BAD-VB-STRASSE         PIC X(46).
             10 VIA-FT-DVAG-BAD-VB-LAND            PIC X(03).
             10 VIA-FT-DVAG-BAD-VB-PLZ             PIC X(08).
             10 VIA-FT-DVAG-BAD-VB-ORT             PIC X(40).
             10 VIA-FT-DVAG-BAD-VB-TELEFON         PIC X(17).
             10 VIA-FT-DVAG-BAD-VB-MAIL            PIC X(50).
             10 VIA-FT-DVAG-BAD-VERMGES            PIC 9(02).
      *
      *** XGDV-DATEN BEGINN UND ENDE KENNZEICHEN $
          05 VIA-FT-DVAG-BAD-BLACKBOX              PIC X(30000).
      *
      *   ENDE COPYMEMBER *
 

