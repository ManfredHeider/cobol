      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0138.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAT0138.CPv  $
      *
      *   Rev 2.4   Jan 29 2013 10:37:36   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.3   10 May 2001 13:10:40   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.2   07 Feb 2001 13:10:58   T13EX21
      *APA00118 - Neue Funktion DRAT0344
      *
      *   Rev 2.1   Aug 02 1999 10:43:52   T13AE34
      *AAT00010-Neukonzeption KS10
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   27 Aug 1997 17:41:00   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.2   10 Jul 1997 12:35:20   T13AE34
      *AFPA0021 - s. letzter put
      *
      *   Rev 1.1   08 Jul 1997 08:53:20   T13AE34
      *AFPA0021 - CICS-Name / Verzweigung Vorschalt-PG
      *
      *   Rev 1.0   Feb 19 1997 12:21:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0138                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO ZWAT0008              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                        *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0013!30.05.97!    ! NEU FUNKTION DRAT0139     ! BOHLINGER*
      *         !        !    ! DB2-LESE-OZP AUF TBAT0008 !          *
      *         !        !    ! IN: KTY-NR, KNR-BEZ-KURZ  !          *
      *         !        !    ! OUT: KNR-NR, KNR-BEZ10    !          *
      * AFPA0021!06.07.97!    ! FUNKTION 0143             ! HEIDER   *
      *         !        !    ! IN: KTY-NR, KNR-BEZ10     !          *
      *         !        !    ! OUT: KNR-NR, KNR-BEZ-KURZ !          *
      *         !        !    !      KNR-BEZ              !          *
      * APPA0022!22.08.97!    ! DRAT0151 NEU EINGEFUEGT   ! OPRITZ   *
      * AAT00010!16.07.99!2.1 ! DRAT0297 NEU EINGEFUEGT   ! HEIDER   *
      * APA00118!07.01.01!2.2 ! NEUE FUNKTION DRAT0344    ! STORCH   *
      * ACV00015!08.03.01!2.3 ! NEUE FUNKTION DRAT0353    ! HEIDER   *
      * AVB01244!12.12.12! 03 ! NEUE FUNKTION DRAT0498    !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWAT0008.
           05  PI-ZWAT0008-FUNKTION            PIC  X(008).
               88 PI-ZWAT0008-DRAT0139              VALUE 'DRAT0139'.
060797         88 PI-ZWAT0008-DRAT0143              VALUE 'DRAT0143'.
220897         88 PI-ZWAT0008-DRAT0151              VALUE 'DRAT0151'.
160799         88 PI-ZWAT0008-DRAT0297              VALUE 'DRAT0297'.
070101         88 PI-ZWAT0008-DRAT0344              VALUE 'DRAT0344'.
080301         88 PI-ZWAT0008-DRAT0353              VALUE 'DRAT0353'.
121212         88 PI-ZWAT0008-DRAT0498              VALUE 'DRAT0498'.
      *
      ******* ENDE COPY-MEMBER **************************************
