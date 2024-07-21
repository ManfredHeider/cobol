      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1462.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1462.CPv  $
      *
      *   Rev 2.2   Apr 04 2003 14:17:38   t13ex21
      *APA00232 - Satzaufbau geändert
      *
      *   Rev 2.1   05 Feb 2001 10:20:18   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47
      *         (BfG Immoinvest)
      *
      *   Rev 2.0   09 Jan 2001 10:12:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1462 FUNKTION ORIGINAL-DATENSATZ         *
      *                                   SEB INVEST/IMMOINVEST      *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    28.03.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00232!28.03.03!2.1 ! KOPF NEU ANGELEGT         ! STORCH   *
      *         !        !    ! SATZÄNDERUNG              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1462                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************

       01  PI-DRPA1462.
           05  PI-DRPA1462-SATZ.
             10   PI-DRPA1462-SATZ-ART      PIC X(001).
             10   PI-DRPA1462-SPESENART     PIC X(003).
280303*      10   PI-DRPA1462-VERMITTLER    PIC 9(010).
280303       10   PI-DRPA1462-VERMITTLER    PIC 9(018).
             10   PI-DRPA1462-MITARBEITER.
                 15 PI-DRPA1462-MITARB-VBNR       PIC 9(010).
                 15 PI-DRPA1462-MITARB-REST       PIC X(020).
             10   PI-DRPA1462-VERSICHERUNG.
                 15  PI-DRPA1462-VERS-INTVSNR     PIC 9(007).
                 15  PI-DRPA1462-VERS-INTVSNR-PRZ PIC 9(002).
                 15  PI-DRPA1462-VERS-REST        PIC X(006).
             10   PI-DRPA1462-UNTERDEPOT    PIC 9(010).
             10   PI-DRPA1462-VERMOEGENSGEG PIC 9(009).
             10   PI-DRPA1462-VERTRAGSART   PIC 9(002).
             10   PI-DRPA1462-VERTRAGSUART  PIC 9(002).
             10   PI-DRPA1462-NAME-NACHN    PIC X(040).
             10   PI-DRPA1462-NAME-VORN     PIC X(040).
             10   PI-DRPA1462-STRASSE       PIC X(042).
             10   PI-DRPA1462-PLZ           PIC X(008).
             10   PI-DRPA1462-ORT           PIC X(040).
             10   PI-DRPA1462-EROEFFN-DAT   PIC 9(008).
             10   PI-DRPA1462-ANLAGEBETRAG  PIC 9(012)V99.
             10   PI-DRPA1462-PROVPFL-BETR  PIC 9(012)V99.
             10   PI-DRPA1462-PROVISION-VW  PIC 9(012)V99.
             10   PI-DRPA1462-PROVISION-ZW  PIC 9(012)V99.
             10   PI-DRPA1462-AB-ZU-KZ      PIC X(001).
             10   PI-DRPA1462-UMSATZ-KZ     PIC X(001).
             10   PI-DRPA1462-PROV-SATZ     PIC 9(003)V999.
             10   PI-DRPA1462-BUCH-DAT9     PIC 9(008).
             10   PI-DRPA1462-NEUANLAGE-KZ  PIC X(001).
             10   PI-DRPA1462-STORNO-KZ     PIC X(001).
             10   PI-DRPA1462-ZAHLUNGSVORF  PIC X(003).
             10   PI-DRPA1462-ABSCHL-ART    PIC 9(003).
             10   PI-DRPA1462-PRODUKT       PIC 9(010).
             10   PI-DRPA1462-AP-KZ         PIC X(001).
             10   PI-DRPA1462-EN-KZ         PIC X(001).
             10   PI-DRPA1462-RED-AUFSCHLAG-KZ   PIC 9(002).
             10   PI-DRPA1462-WHR-VERMG     PIC X(003).
             10   PI-DRPA1462-WHR-ZAHLUNG   PIC X(003).
             10   PI-DRPA1462-FIRMA         PIC 9(001).
             10   PI-DRPA1462-PROV-SCHLUESSEL PIC X(010).
             10   PI-DRPA1462-PLAN-ZIEL       PIC 9(007)V99.
             10   PI-DRPA1462-ZAHLUNGSMOD-VL  PIC X(002).
             10   PI-DRPA1462-ZAHLUNGSMOD-AP  PIC X(002).
             10   PI-DRPA1462-ZAHLUNGSMOD-EN  PIC X(002).
             10   PI-DRPA1462-ANTEILWERT      PIC 9(012)V99.
280303*      10   PI-DRPA1462-VERMITTLER-DU   PIC 9(010).
280303       10   PI-DRPA1462-VERMITTLER-DU   PIC 9(018).
             10   PI-DRPA1462-SATZENDE        PIC X(001).
280303       10   FILLER                      PIC X(140).
                                                
           05  PI-DRPA1462-A-SATZ REDEFINES PI-DRPA1462-SATZ.
             10    PI-DRPA1462-A-SATZART      PIC X(001).
             10    PI-DRPA1462-A-FIRMA        PIC X(040).
             10    PI-DRPA1462-A-ERSTELL-DAT  PIC 9(008).
             10    PI-DRPA1462-A-VON-DAT      PIC 9(008).
             10    PI-DRPA1462-A-BIS-DAT      PIC 9(008).
             10    PI-DRPA1462-A-EMPFAENGER   PIC X(040).
             10    PI-DRPA1462-A-TEXT         PIC X(040).
             10    PI-DRPA1462-A-WHR-ZAHLUNG  PIC X(003).
             10    PI-DRPA1462-A-SATZENDE     PIC X(001).
280303*      10    FILLER                     PIC X(262).
280303       10    FILLER                     PIC X(418).
                                             
           05  PI-DRPA1462-E-SATZ REDEFINES PI-DRPA1462-SATZ.
             10    PI-DRPA1462-E-SATZART      PIC X(001).
             10    PI-DRPA1462-E-ANZAHL       PIC 9(007).
             10    PI-DRPA1462-E-SUMME-PROV   PIC 9(16)V99.
             10    PI-DRPA1462-E-SUMME-HABEN  PIC 9(16)V99.
             10    PI-DRPA1462-E-SUMME-SOLL   PIC 9(16)V99.
             10    PI-DRPA1462-E-SATZENDE     PIC X(001).
280303*      10    FILLER                     PIC X(348).
280303       10    FILLER                     PIC X(504).
