      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0111.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0111.CPv  $
      *
      *   Rev 2.5   Jan 08 2019 16:15:14   t13ae56
      *ACV00185: PGM - Vertragsrückläufer
      *
      *   Rev 2.4   Nov 18 2008 14:41:00   t13ae36
      *AVB01012: Dublettenmeldung
      *
      *   Rev 2.3   Apr 15 2002 17:13:50   t13ae30
      *AAS00344 - REGIONENWBW 2002 DER SW1
      *
      *   Rev 2.2   Aug 04 1999 15:38:08   T13AE30
      *FAS34 - Fehler Schnittstelle F&A
      *
      *   Rev 2.1   Apr 21 1999 10:04:16   T13AE30
      *AAS00051 - Neuerstellung PMVB0212
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   13 Aug 1998 16:44:46   T13AE35
      *AAS00049 - Aenderung 3 neue Funktionen
      *
      *   Rev 1.2   17 Oct 1997 11:54:10   T13AE35
      *AFVB0364 - Aenderung, DRVB0111 SST PMVB0107-ZWVB0014, Fkt
      *
      *   Rev 1.1   Apr 09 1997 16:56:22   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0111                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0014  MAN                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Giese                                            *
      *    DATUM:    12.09.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.09.96! 01 ! NEU                       ! Giese    *
      * AFVB0364!23.07.97! 02 ! NEUE FUNKTION             ! J.HERING *
      * AAS00049!20.06.98!    ! NEUE FUNKTION DRVB0281    ! WALB     *
      * AAS00049!13.07.98! 03 ! NEUE FUNKTION DRVB0299    ! WALB     *
      * AKI00001!15.07.98! 04 ! NEUE FUNKTION DRVB0255    !Zgur-Vujic*
      * AAS00051!20.04.99! 05 ! NEUE FUNKTION DRVB0382    ! WILLIG   *
      * FAS34   !04.08.99! 06 ! NEUE FUNKTION DRVB0433    ! WILLIG   *
      * AAS00344!20.03.02! 07 ! NEUE FUNKTION DRVB0584    ! WILLIG   *
      * AVB01012!06.11.08! 08 ! NEUE FUNKTION DRVB0923    ! Franke   *
080119* ACV00185!08.01.19! 2.5! NEUE FUNKTION DRVB1210    ! KLEEMEIER*
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
       01  PI-ZWVB0014.
           05  PI-ZWVB0014-FUNKTION            PIC  X(008).
               88 PI-ZWVB0014-DRVB0112              VALUE 'DRVB0112'.
230797         88 PI-ZWVB0014-DRVB0139              VALUE 'DRVB0139'.
200698         88 PI-ZWVB0014-DRVB0281              VALUE 'DRVB0281'.
130798         88 PI-ZWVB0014-DRVB0299              VALUE 'DRVB0299'.
150798         88 PI-ZWVB0014-DRVB0255              VALUE 'DRVB0255'.
200499         88 PI-ZWVB0014-DRVB0382              VALUE 'DRVB0382'.
040899         88 PI-ZWVB0014-DRVB0433              VALUE 'DRVB0433'.
200302         88 PI-ZWVB0014-DRVB0584              VALUE 'DRVB0584'.
061108         88 PI-ZWVB0014-DRVB0923              VALUE 'DRVB0923'.
080119         88 PI-ZWVB0014-DRVB1210              VALUE 'DRVB1210'.
      *
      ******* ENDE COPY-MEMBER **************************************

