      *---------------------------------------------------------------*
      *    Standard fÜr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0549.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0549.CPv  $
      *
      *   Rev 2.4   Jul 19 2019 14:34:52   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.3   May 09 2019 13:00:24   t13ex67
      *APA02573: DSGVO - Dialoge MSS
      *
      *   Rev 2.2   Jul 13 1999 17:11:14   T13AE34
      *AAN00190-Mappen DRKD0039 au KI
      *
      *   Rev 2.1   May 27 1999 11:21:52   T13EX26
      *MFE-Umstellung 2
      *
      *   Rev 2.0   May 26 1999 16:57:42   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Oct 1997 11:29:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0549                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0069 KI-DATEN ERMITTELN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.06.97! 01 ! NEU                       ! PARENT   *
      * AAN00190!30.06.99!2.2 ! FKT. DRPA1275             ! HEIDER   *
      * APA01912!25.10.18!2.3 ! NEUE FUNKTION DRPA1912    ! STORCH   *
      * APA02814!12.03.19!2.4 ! NEUE FUNKTION DRPA1914    ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
      ****************************************************************
       01  PI-UPPA0069.
           05 PI-UPPA0069-FUNKTION            PIC X(008).
              88 PI-UPPA0069-DRPA0550             VALUE 'DRPA0550'.
              88 PI-UPPA0069-DRPA0551             VALUE 'DRPA0551'.
              88 PI-UPPA0069-DRPA1275             VALUE 'DRPA1275'.
              88 PI-UPPA0069-DRPA1912             VALUE 'DRPA1912'.
              88 PI-UPPA0069-DRPA1914             VALUE 'DRPA1914'.


