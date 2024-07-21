      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0451.CPY  $

      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRVB0451.CPv  $
      *
      *   Rev 2.11   May 13 2019 18:09:58   T13AE30
      *AAS01285: Siegeradler: ED-Hinzurechnung bei VB 8047670, Will
      *i Sebold
      *
      *   Rev 2.10   Apr 07 2009 10:47:38   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.9   Nov 27 2003 13:39:34   t13ex57
      *AVB00711 - Neue Funktion DRVB0689
      *
      *   Rev 2.8   Jun 10 2003 14:19:54   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.7   Apr 30 2003 09:11:50   t13ae27
      *LP2003
      *
      *   Rev 2.6   05 Mar 2002 11:17:20   t13ae13
      *ASL00135: Dir. herausg. D werden zeitlich unbegrenzt gewertet
      *
      *   Rev 2.5   29 Nov 2001 09:57:18   t13ae13
      *ASL00103: EQF-Prüfung entfällt 12 Quartale für D mit ND's
      *
      *   Rev 2.4   16 Oct 2001 11:52:38   t13ae30
      *FAS00072 - NEUE FUNKTION DRVB0558
      *
      *   Rev 2.3   20 Apr 2001 14:20:22   t13ae30
      *FAS00063 - Fehler beim Ermitteln Leistungsplan / Wettbewerb
      *
      *   Rev 2.2   29 Sep 2000 12:14:34   T13AE35
      *ASL00070 - neue Fkt. DRVB0502
      *
      *   Rev 2.1   Feb 23 2000 12:17:48   T13AE28
      *AAS00201: Jahreswettbewerb 2000
      *
      *   Rev 2.0   Feb 07 2000 17:17:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0451                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                        *
      *    DATUM:    22.02.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00201!22.02.00! 01 ! NEU                       ! D.Ehrmann*
      * ASL00070!20.09.00! 02 ! Neue Funktion DRVB0502    ! S.Walb   *
      * FAS00063!06.04.01! 03 ! NEUE FUNKTION DRVB0530    ! TH.WILLIG*
      * FAS00072!16.10.01! 04 ! NEUE FUNKTION DRVB0558    ! TH.WILLIG*
      * ASL00103!23.11.01! 05 ! NEUE FUNKTION DRVB0036    ! S. Walb  *
      * ASL00135!27.02.02! 06 ! NEUE FUNKTION DRVB0575    ! S. Walb  *
      * AAS00400!02.04.03! 07 ! NEUE FUNKTION DRVB0661    ! P. DUERR *
      * AVB00666!03.06.03! 08 ! NEUE FUNKTION DRVB0666    ! MUND     *
      * AVB00711!06.11.03! 09 ! NEUE FUNKTION DRVB0689    ! U. Boll  *
250209* AAS00812!25.02.09!2.10! NEUE FUNKTION DRVB0929    ! KLEEMEIER*
      * AAS01285!13.05.19!2.11! NEUE FUNKTION DRVB1212    ! TH.WILLIG*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    ==== FUNKTIONENCOPY ===
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO (HIER ZJVB0015) ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZJVB0015.
           05 PI-ZJVB0015-FUNKTION            PIC X(008).
              88 PI-ZJVB0015-DRVB0452             VALUE 'DRVB0452'.
              88 PI-ZJVB0015-DRVB0453             VALUE 'DRVB0453'.
200900        88 PI-ZJVB0015-DRVB0502             VALUE 'DRVB0502'.
060401        88 PI-ZJVB0015-DRVB0530             VALUE 'DRVB0530'.
161001        88 PI-ZJVB0015-DRVB0558             VALUE 'DRVB0558'.
231101        88 PI-ZJVB0015-DRVB0036             VALUE 'DRVB0036'.
270202        88 PI-ZJVB0015-DRVB0575             VALUE 'DRVB0575'.
020403        88 PI-ZJVB0015-DRVB0649             VALUE 'DRVB0649'.
030603        88 PI-ZJVB0015-DRVB0661             VALUE 'DRVB0661'.
061103        88 PI-ZJVB0015-DRVB0689             VALUE 'DRVB0689'.
250209        88 PI-ZJVB0015-DRVB0929             VALUE 'DRVB0929'.
130519        88 PI-ZJVB0015-DRVB1212             VALUE 'DRVB1212'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *

