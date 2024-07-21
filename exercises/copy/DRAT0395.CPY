      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0395.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0395.CPv  $
      *
      *   Rev 2.5   Feb 21 2013 15:27:10   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.4   Feb 12 2013 18:54:36   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.3   Nov 19 2012 16:43:26   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.2   Apr 13 2010 09:45:42   t13ex57
      *AVB01053: Kto.Nr.-Prüfung in OSTA 
      *
      *   Rev 2.1   Sep 05 2003 13:53:48   t13ae30
      *h:\mfe\cpy\DRAT0395.CPY -> L:\mf\CPY\DRAT0395.CPv
      *AVB00536 - PLAUSIBILISIERUNG DER BLZ
      *
      *   Rev 2.0   26 Jun 2001 12:55:18   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0395                                      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *---------------------------------------------------------------*
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    04.09.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00536!04.09.03! 01 ! NEUERSTELLUNG             ! WILLIG   *
290709* AVB01053!29.07.09! 02 ! Ermittlung des Länderkz   ! U.BOLL   *
      *         !        !    ! für Kto prüfung           !          *
151112* AVB01244!15.11.12! 03 ! SEPA-Anp. BIC holen       ! B.PRETSCH*
110213* AVB01244!11.02.13! 04 ! SEPA-Anp. BIC prüfen      ! B.PRETSCH*
190213* AVB01244!19.02.13! 05 ! SEPA-Anp. BLZ,Bankname hol! B.PRETSCH*
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
       01  PI-ZWAT0035.
           05  PI-ZWAT0035-FUNKTION            PIC  X(008).
               88 PI-ZWAT0035-DRAT0396              VALUE 'DRAT0396'.
290709         88 PI-ZWAT0035-DRAT0463              VALUE 'DRAT0463'.
151112         88 PI-ZWAT0035-DRAT0496              VALUE 'DRAT0496'.
110213         88 PI-ZWAT0035-DRAT0499              VALUE 'DRAT0499'.
190213         88 PI-ZWAT0035-DRAT0575              VALUE 'DRAT0575'.
      *
