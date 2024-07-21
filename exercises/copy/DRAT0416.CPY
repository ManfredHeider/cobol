      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0416.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRAT0416.CPv  $
      *
      *   Rev 2.15   Nov 23 2017 14:56:28   t13ae23
      *AAN01346: Header-Datensätze: Neue Antragsarten
      *
      *   Rev 2.14   Nov 23 2017 13:37:30   t13ae23
      *FAN01115: Fehler bei Auflösung Umlaute E-SIGN Anträge GENERALI
      *
      *   Rev 2.13   Jun 20 2017 12:24:44   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.12   Jun 14 2016 10:33:20   t13ae23
      *AAN01271: Neue Feldbelegung im AN 01
      *
      *   Rev 2.11   Jun 02 2016 13:41:24   t13ae34
      *AAN01270: VSP-Vertragsumstellung
      *
      *   Rev 2.10   Jun 15 2015 08:53:08   t13ae34
      *AAN01221: Bestandsübertragung DWS Anträge
      *
      *   Rev 2.9   May 28 2015 09:54:42   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.8   Apr 23 2015 13:25:02   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.6   Jul 05 2013 10:23:46   t13ae23
      *AAN01127: Erfassung Investanträge ohne §34f
      *
      *   Rev 2.5   Apr 24 2012 15:11:40   t13ae23
      *AAT00006: Änderung Konstantencopy
      *
      *   Rev 2.4   Nov 26 2009 14:36:38   t13ae34
      *AAN00896: Dialoge
      *
      *   Rev 2.3   Jul 11 2007 08:44:52   t13ae34
      *AAN00761: Kürzel für das Feld Antragsart
      *
      *   Rev 2.2   09 Nov 2006 14:52:30   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.1   Sep 26 2006 11:22:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 20 2006 11:33:08   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    KONSTANTEN FÜR     KTY = 217                              *
      *    ANTRAG ART                                                *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!20.09.06! 2.1 ! NEUERSTELLUNG            ! HEIDER   *
      * AAN00761!09.07.07! 2.2 ! NEUE WERTEBEREICHE       ! HEIDER   *
      * AAN00896!22.10.09! 2.4 ! OPTIMA KENNZ. SCHNELLANT.! HEIDER   *
      *         !24.04.12!     ! NACHTRAG KENZ. REP-ANTR  ! RAHNER   *
      * AAN01051!24.04.12! 2.5 ! NEU RISK-CARE,SICHERH-PK ! RAHNER   *
      * AAN01127!05.07.13! 2.6 ! NEU K BEI §34F SONDERWEG ! RAHNER   *
      * AAN01211!13.03.15! 2.7 ! ESIGN                    ! HEIDER   *
      * AAN01221!12.06.15! 2.10! BESTANDÜBERTRAGUNG       ! HEIDER   *
      * AAN01270!30.05.16! 2.11! VSP-VERTRAGSUMSTELLUNG   ! HEIDER   *
      * AAN01271!10.06.16! 2.12! VB-DEPOT                 ! RAHNER   *
      * AAN01291!20.06.17! 2.13! E-SIGN ÖSTERREICH        ! RAHNER   *
      * AAN01346!23.11.17! 2.14! NEUE ANTRAGSARTEN ÖSTERR.! RAHNER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ANTRAG-ART-NEU        PIC S9(004) BINARY VALUE 1.
           05 K-KNR-ANTRAG-ART-ERHOEHUNGB PIC S9(004) BINARY VALUE 2.
090707*    05 K-KNR-ANTRAG-ART-NACHERF    PIC S9(004) BINARY VALUE 3.
090707     05 K-KNR-ANTRAG-ART-MEMO       PIC S9(004) BINARY VALUE 3.
           05 K-KNR-ANTRAG-ART-FAX-AD     PIC S9(004) BINARY VALUE 4.
           05 K-KNR-ANTRAG-ART-ERHOEHUNG  PIC S9(004) BINARY VALUE 5.
           05 K-KNR-ANTRAG-ART-FAX-PG     PIC S9(004) BINARY VALUE 6.
           05 K-KNR-ANTRAG-ART-SCHNELL    PIC S9(004) BINARY VALUE 7.
           05 K-KNR-ANTRAG-ART-TELEFON    PIC S9(004) BINARY VALUE 8.
221009     05 K-KNR-ANTRAG-ART-OPTIMA     PIC S9(004) BINARY VALUE 9.
240412     05 K-KNR-ANTRAG-ART-REP-ANTR   PIC S9(004) BINARY VALUE 10.
240412     05 K-KNR-ANTRAG-ART-RISK-CARE  PIC S9(004) BINARY VALUE 11.
240412     05 K-KNR-ANTRAG-ART-SICHERH-PK PIC S9(004) BINARY VALUE 12.
050713     05 K-KNR-ANTRAG-ART-34F-SONDER PIC S9(004) BINARY VALUE 13.
130315     05 K-KNR-ANTRAG-ART-ESIGN      PIC S9(004) BINARY VALUE 14.
120615     05 K-KNR-ANTRAG-ART-BESTANDSUEB
120615                                    PIC S9(004) BINARY VALUE 15.
300516     05 K-KNR-ANTRAG-ART-VSP-UMSTEL PIC S9(004) BINARY VALUE 16.
100616     05 K-KNR-ANTRAG-ART-VB-DEPOT   PIC S9(004) BINARY VALUE 17.
200617     05 K-KNR-ANTRAG-ART-ESIGN-OE-H PIC S9(004) BINARY VALUE 18.
231117     05 K-KNR-ANTRAG-ART-KONV-OE-E  PIC S9(004) BINARY VALUE 19.
231117     05 K-KNR-ANTRAG-ART-KONV-OE-P  PIC S9(004) BINARY VALUE 20.
      ****************************************************************
      * ENDE COPY-MEMBER                                             *
      ****************************************************************






