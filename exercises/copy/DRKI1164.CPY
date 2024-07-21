      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1164.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1164.CPv  $
      *
      *   Rev 2.7   Jul 20 2012 10:05:54   t13ex27
      *AKI01135: Kundendaten physisch löschen-UP für Vertragsdaten
      *
      *   Rev 2.6   Jan 27 2010 09:47:20   t13ex27
      *AKI00888: Reob-Tabellen aus Abgleich entfernen
      *
      *   Rev 2.5   Mar 28 2008 16:21:04   t13ex51
      *AKI00705: Anpassung Kundenaktualisierung am Host
      *
      *   Rev 2.4   03 Dec 2007 11:14:38   t13ex27
      *AKL00664: Allfinanz: Status VNR erledigt
      *
      *   Rev 2.3   Dec 20 2006 13:55:08   t13ex43
      *FKI00979: Fremdverträge mit mehreren VN
      *
      *   Rev 2.2   Jan 15 2003 15:07:44   t13ex39
      *AKI00303 KoKü
      *
      *   Rev 2.1   31 Jan 2001 14:25:54   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.1   08 Feb 1999 18:19:30   T13EX47
      *Funktionsauswahl fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1164                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:    8.01.2001                                        *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068! 8.01.01! 01 ! NEUES SELECT FÜR DRKI1155 ! WHO      *
      * AKI00303!27.09.02!2.2 ! Funktion DRKI1449 eingef. !I.Frenzel *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00303!15.10.02! 04 ! Select mit VRBKOMMBETRKZ  ! HORNUNG  *
      *         !        !    ! Fkt. 1480                 !          *
      * -------------------------------------------------------------*
      * AKI00556!29.08.06!2.3 ! Funktion DRKI1941 und     !I. Frenzel*
      *         !        !    ! DRKI1449 nach DRKI2001    !          *
181206* FKI00979!18.12.06!2.3 ! DRKI1978 HINZU            !KLEEMEIER *
      * -------------------------------------------------------------*
      * AKL00664!13.09.07!2.4 ! Neue Funktion DRKI2096    !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00705!19.03.08!2.4 ! Neue Funktion DRKI2236    !HRO       *
      * -------------------------------------------------------------*
      * AKI00888!08.10.09!2.6 ! Neue Funktion DRKI2364    !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI01135!09.09.12!2.7 ! Neue Funktion DRKI2610    !I. Frenzel*
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
       01  PI-ZWKI0359.
           05  PI-ZWKI0359-FUNKTION            PIC  X(008).
               88 PI-ZWKI0359-DRKI1155              VALUE 'DRKI1155'.
               88 PI-ZWKI0359-DRKI1166              VALUE 'DRKI1166'.
290806*        88 PI-ZWKI0359-DRKI1449              VALUE 'DRKI1449'.
290806         88 PI-ZWKI0359-DRKI2001              VALUE 'DRKI2001'.
               88 PI-ZWKI0359-DRKI1480              VALUE 'DRKI1480'.
290806         88 PI-ZWKI0359-DRKI1941              VALUE 'DRKI1941'.
181206         88 PI-ZWKI0359-DRKI1978              VALUE 'DRKI1978'.
130907         88 PI-ZWKI0359-DRKI2096              VALUE 'DRKI2096'.
130907         88 PI-ZWKI0359-DRKI2236              VALUE 'DRKI2236'.
081009         88 PI-ZWKI0359-DRKI2364              VALUE 'DRKI2364'.
090512         88 PI-ZWKI0359-DRKI2610              VALUE 'DRKI2610'.
      *
      ******* ENDE COPY-MEMBER DRKI0706 *****************************
