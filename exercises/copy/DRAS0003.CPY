      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0003.CPv  $
      *
      *   Rev 2.2   Apr 08 2004 12:02:32   t13ae27
      *Siegeradler-Ranglisten / Trennung DVAG Wien
      *
      *   Rev 2.1   Dec 21 1999 09:13:38   T13AE27
      *Neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:55:34   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0003                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Service-Modul UPAS0001               *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    20.07.19969                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !28.07.99! 01 ! NEU                       ! Dürr     *
      *         !02.03.04! 02 ! neue Funktion DRVB0014    ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-UPAS0001.
           05  PI-UPAS0001-FUNKTION            PIC  X(008).
               88 PI-UPAS0001-DRAS0004              VALUE 'DRAS0004'.
020304         88 PI-UPAS0001-DRAS0014              VALUE 'DRAS0014'.
      *
      ******* ENDE COPY-MEMBER **************************************

