      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0134.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0134.CPv  $
      *
      *   Rev 2.5   Apr 05 2011 09:14:58   t13ex43
      *AVB01132: VB58 - Schliessen einer Direktionsleitung
      *
      *   Rev 2.4   Jul 25 2006 13:11:32   t13ex43
      *AVB00850: PGM4 - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.3   Feb 27 2006 09:50:00   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.2   Oct 11 2004 12:04:56   t13ae27
      *ASL00183: MAP - Manageraufbauplan Ermittlung Ansprüche für D
      *-Leiter (Fiktiv)
      *
      *   Rev 2.1   May 17 1999 16:00:08   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 09 1999 16:38:46   T13AE35
      *AAS00095 neue Fkt
      *
      *   Rev 1.3   15 Dec 1998 11:03:10   T13AE35
      *AAS00079 - Aenderung, neue Funktion
      *
      *   Rev 1.2   26 Aug 1998 12:13:50   T13AE35
      *AAS00049 - Aenderung, neue Fkt DRVB0294
      *
      *   Rev 1.1   Mar 06 1997 15:14:40   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0134                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    25.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.01.97! 01 ! NEU                       ! SCHÄFER  *
      * AAS00049!07.08.98! 02 ! NEUE FUNKTION DRVB0294    ! WALB     *
      * AAS00079!21.10.98! 03 ! NEUE FUNKTION DRVB0325    ! WALB     *
      * AAS00095!05.01.99! 04 ! NEUE FUNKTION DRVB0347    ! WALB     *
      * AAS00096!26.01.99! 05 ! NEUE FUNKTION DRVB0351    ! WALB     *
      * AAS00117!22.02.99! 05 ! NEUE FUNKTION DRVB0363    ! SCHMIDT,B*
      * ASL00183!26.08.04! 06 ! NEUE FUNKTION DRVB0732    ! P.DUERR  *
      * ASL00595!23.01.06! 07 ! NEUE FUNKTION DRVB0817    ! P.DUERR  *
200706* AVB00850!20.07.06!2.4 ! NEUE FUNKTION DRVB0828    ! KLEEMEIER*
100311* AVB01132!10.03.11!2.5 ! NEUE FUNKTION DRVB1011    ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0011.
           05  PI-ZWVB0011-FUNKTION            PIC  X(008).
               88 PI-ZWVB0011-DRVB0135              VALUE 'DRVB0135'.
070898         88 PI-ZWVB0011-DRVB0294              VALUE 'DRVB0294'.
211098         88 PI-ZWVB0011-DRVB0325              VALUE 'DRVB0325'.
050199         88 PI-ZWVB0011-DRVB0347              VALUE 'DRVB0347'.
260199         88 PI-ZWVB0011-DRVB0351              VALUE 'DRVB0351'.
220299         88 PI-ZWVB0011-DRVB0363              VALUE 'DRVB0363'.
260804         88 PI-ZWVB0011-DRVB0732              VALUE 'DRVB0732'.
230106         88 PI-ZWVB0011-DRVB0817              VALUE 'DRVB0817'.
200706         88 PI-ZWVB0011-DRVB0828              VALUE 'DRVB0828'.
100311         88 PI-ZWVB0011-DRVB1011              VALUE 'DRVB1011'.
      *
      ******* ENDE COPY-MEMBER DRVB0134 *****************************
      *
