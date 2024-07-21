      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROP0022.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DROP0022.CPv  $
      *
      *   Rev 2.7   May 18 2009 15:53:36   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.6   May 15 2009 14:47:04   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.5   May 15 2009 12:21:40   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.4   May 15 2009 10:40:32   t13ex44
      *APA00896: Anpassung MIK-Dateien / Lieferung Daisy-Daten
      *
      *   Rev 2.3   Apr 30 2009 09:41:02   t13ex44
      *APA00852: Ges. 92 - Vertragsnr.
      *
      *   Rev 2.2   Nov 13 2006 09:54:40   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 18 1999 15:24:36   T13EX15
      *AOP00189: Auswertung SPNP nach Direktion
      *                   rür AVBG
      *
      *   Rev 2.0   Feb 27 1999 14:14:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:01:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DROP0022  SPNP-STATISTIKSATZ NORMIERT/DROM2415* 
      *    --------------------------------------------------------   * 
      *    VERSION   1    19.01.95 16:02:37
      *    --------------------------------------------------------   * 
      *    ERSTELLT: PROEMPELER                                       * 
      *    DATUM:    19.01.1995                                       * 
      *    STAND:    19.01.1995                                       * 
      *****************************************************************
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFOP0055!19.01.95! 01 ! NEU                       !WIECHERS  *  
      *         !        !    !                           !          *  
      * AOP00189!18.10.99! 02 ! NEUE DATENFELDER          !WIECHERS  *  
      *         !        !    ! AVERM UND DIREKTION       !          *  
      * APA00852!26.02.09!2.3 ! ALPHANUMMERISCHE VSNR     ! SCHEIDEL *
      * APA00896!12.05.09!2.4 ! MIK-DATEN ERGAENZT        ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
000010 01  DROM2415.
000020     03 SATZ-KZ                   PICTURE X.
260209*    03 VERTRAGSNR-EXTERN         PICTURE 9(15).
260209     03 VERTRAGSNR-EXTERN         PICTURE X(15).
000040     03 GESKZ                     PICTURE 99.
011106*    03 PRODUKTSCHL               PICTURE 99.
011106     03 PROD-PROD-SCHL            PICTURE S9(04) BINARY.
000060     03 GEVO-TYP                  PICTURE X(4).
000070     03 PRODART                   PICTURE 9.
000080     03 BEWERTUNGS-BETR           PICTURE S9(11)V99
000090                                                 COMPUTATIONAL-3.
000100     03 PROVEINH                  PICTURE S9(9)V9
000110                                                 COMPUTATIONAL-3.
000120     03 AE-EING-GESCH             PICTURE S9(9)V9
000130                                                 COMPUTATIONAL-3.
000140     03 BEWERTUNGS-BETR-DYN       PICTURE S9(11)V99
000150                                                 COMPUTATIONAL-3.
991018     03 AVERM                     PICTURE 9(07).
991018     03 FILLER                    PICTURE X(03).
991018     03 DIR                       PICTURE X(03).
120509     03 MIK-ABR-DAT               PICTURE X(010).
120509     03 MIK-STRUKTUR.
120509        05 MIK-STRUKTUR-ZEILE     OCCURS 20
120509                                  INDEXED BY IX-MIK-STRUK
120509                                  PICTURE X(036).
120509     03 FILLER                    PICTURE X(206).
