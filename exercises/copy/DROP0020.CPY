      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROP0020.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DROP0020.CPv  $
      *
      *   Rev 2.5   May 15 2009 12:20:38   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.4   May 15 2009 10:39:32   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.3   Apr 30 2009 09:39:38   t13ex44
      *APA00852: Ges. 92 - Vertragsnr.
      *
      *   Rev 2.2   Nov 13 2006 09:54:14   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jun 01 2006 12:32:00   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.0   Feb 27 1999 14:14:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:00:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROP0020  SPNP-ARBEITSSATZ OPAL               *
      *    --------------------------------------------------------   *
      *    VERSION   2    27.02.95 10:29:22
      *    --------------------------------------------------------   *
      *    ERSTELLT: WIECHERS                                         *
      *    DATUM:    19.01.1995                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFOP0055!19.01.95! 01 ! NEU                       !WIECHERS  *
      * AFOP0055!24.02.95! 02 ! AENDERUNG: DIE FELDER     !WIECHERS  *
      *         !        !    ! PGM-NAME UND FREIKZ       !          *
      *         !        !    ! ZUSAETZLICH               !          *
      * FPA00846!16.05.06!2.1 ! PROVPFLSU 10 VK-STELLEN   ! SCHEIDEL *
      *         !        !    ! PROVEINH   9 VK-STELLEN   !          *
      * APA00452!01.11.06!2.2 ! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA00852!26.02.09!2.3 ! ALPHANUMMERISCHE VSNR     ! SCHEIDEL *
      * APA00896!12.05.09!2.4 ! MIK-DATEN ERGAENZT        ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  EF1-SATZ.
           05  EF1-GES          PIC XX.
260209*    05  EF1-VSNR         PIC 9(15).
260209     05  EF1-VSNR         PIC X(15).
           05  EF1-FILLER-1     PIC X.
           05  EF1-INTVSNR      PIC 9(15).
011106*    05  EF1-PRODSCHL     PIC XX.
011106     05  EF1-PROD-PROD-SCHL PIC S9(04) BINARY.
           05  EF1-EDATUM       PIC 9(7).
           05  EF1-FILLER-2     PIC X.
           05  EF1-FAELLIG      PIC 9(7).
           05  EF1-FILLER-3     PIC X.
           05  EF1-PRODART      PIC 9.
160506*    05  EF1-PROVPFLSU    PIC S9(7)V99.
160506     05  EF1-PROVPFLSU    PIC S9(10)V99.
           05  EF1-PROVPFLSU-V  PIC X.
160506*    05  EF1-PROVEINH     PIC S9(6)V9.
160506     05  EF1-PROVEINH     PIC S9(9)V9.
           05  EF1-PROVEINH-V   PIC X.
           05  EF1-PGMNAME      PIC X(8).
           05  EF1-FREIKZ       PIC X.
120509     05  EF1-MA-ABR-DAT   PIC X(010).
120509     05  EF1-STRUKTUR     PIC X(720).
120509     05  FILLER           PIC X(185).
