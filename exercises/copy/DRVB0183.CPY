      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0183.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRVB0183.CPv  $
      *
      *   Rev 2.10   25 Jun 2018 15:28:50   t13ae50
      *AVB01479: Einführung Allfinanz AG im VB-System - Statistikpa
      *ket - Programmen(sla)
      *
      *   Rev 2.9   Jan 07 2015 11:23:52   t13ae13
      *AVB01321: Integration FVD - Rest (SWL)
      *
      *   Rev 2.8   Oct 23 2008 10:17:52   t13ae36
      *AAS00788: Anpassung Postverteilerliste Allfinanz
      *
      *   Rev 2.7   Apr 22 2008 13:56:42   t13ex43
      *AVB00961: PGM - Statistiken FVD (bkl)
      *
      *   Rev 2.6   13 Nov 2007 10:53:36   t13ex43
      *AVB00905: PGM - Batchanpassungen für Allfinanz (bkl)
      *
      *   Rev 2.5   19 Dec 2006 12:32:32   t13ae13
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.4   Nov 09 2006 09:11:44   t13ex43
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.3   31 May 2001 10:37:08   t13ae27
      *neue Funktion
      *
      *   Rev 2.2   Jan 11 2000 10:49:04   T13AE34
      *AAN00237 - MIS Antragsbereich
      *
      *   Rev 2.1   Dec 15 1999 16:12:50   T13AE42
      *AAS00177 - neue Funktion eingebaut
      *
      *   Rev 2.0   Mar 09 1999 14:28:40   T13AE35
      *AAS00096 - neue Funktion
      *
      *   Rev 1.3   27 Jul 1998 12:45:16   T13AE35
      *AAS00049 - Neue Funktion DRVB0291
      *
      *   Rev 1.2   16 Dec 1997 09:42:20   T13AE35
      *AVB00400 - Aenderung, neue Funktionen
      *
      *   Rev 1.1   05 Dec 1997 11:26:52   T13AE35
      *AVB00393 - Neuerstellung, Fkt fuer ZWVB0006
      *
      *   Rev 1.0   23 Jul 1997 12:12:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0183                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0006  DIR                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HERING                                           *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00393!12.11.97! 01 ! NEU                       ! J.HERING *
      * AVB00400!08.12.97! 02 ! NEUE FUNKTIONEN           ! J.HERING *
      * AAS00049!01.07.98! 03 ! NEUE FUNKTION DRVB0291    ! S.WALB   *
      * AAS00096!16.02.99! 04 ! NEUE FUNKTION DRVB0355    ! S.WALB   *
      * AAS00177!15.12.99! 05 ! NEUE FUNKTION DRVB0262    ! B.SCHMIDT*
      * AAS00274!14.05.01! 06 ! NEUE FUNKTION DRVB0542    ! P.DUERR  *
      */AAN00237!        !    !                           !          *
060906* AVB00868!06.09.06!2.4 ! NEUE FUNKTION DRVB0834    ! KLEEMEIER*
270906* AVB00868!27.09.06! "  ! NEUE FUNKTION DRVB0836    ! KLEEMEIER*
010108* AVB00905!01.01.08!2.6 ! NEUE FUNKTION DRVB0871    ! KLEEMEIER*
010708* AVB00961!01.07.08!2.7 ! NEUE FUNKTION DRVB0897    ! KLEEMEIER*
231008* AAS00788!23.10.08!2.8 ! NEUE FUNKTION DRVB0921    ! R.Franke *
      * AVB01321!11.11.14!2.9 ! FUNKTION DRVB0897 entfällt! S. Walb  *
220618* AVB01479!22.06.18!2.10! NEUE FUNKTION DRVB1191    ! S.LAS    *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPIES PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0006.
           05  PI-ZWVB0006-FUNKTION            PIC  X(008).
               88 PI-ZWVB0006-DRVB0184              VALUE 'DRVB0184'.
               88 PI-ZWVB0006-DRVB0185              VALUE 'DRVB0185'.
               88 PI-ZWVB0006-DRVB0186              VALUE 'DRVB0186'.
               88 PI-ZWVB0006-DRVB0197              VALUE 'DRVB0197'.
               88 PI-ZWVB0006-DRVB0198              VALUE 'DRVB0198'.
               88 PI-ZWVB0006-DRVB0199              VALUE 'DRVB0199'.
               88 PI-ZWVB0006-DRVB0200              VALUE 'DRVB0200'.
               88 PI-ZWVB0006-DRVB0201              VALUE 'DRVB0201'.
               88 PI-ZWVB0006-DRVB0202              VALUE 'DRVB0202'.
               88 PI-ZWVB0006-DRVB0291              VALUE 'DRVB0291'.
160299         88 PI-ZWVB0006-DRVB0355              VALUE 'DRVB0355'.
151299         88 PI-ZWVB0006-DRVB0262              VALUE 'DRVB0262'.
140501         88 PI-ZWVB0006-DRVB0542              VALUE 'DRVB0542'.
060906         88 PI-ZWVB0006-DRVB0834              VALUE 'DRVB0834'.
270906         88 PI-ZWVB0006-DRVB0836              VALUE 'DRVB0836'.
010108         88 PI-ZWVB0006-DRVB0871              VALUE 'DRVB0871'.
111114*010708  88 PI-ZWVB0006-DRVB0897              VALUE 'DRVB0897'.
231008         88 PI-ZWVB0006-DRVB0921              VALUE 'DRVB0921'.
220618         88 PI-ZWVB0006-DRVB1191              VALUE 'DRVB1191'.
      *
      ******* ENDE COPY-MEMBER **************************************
 

