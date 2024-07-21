      * T13EX47   Stand: 17/11/98    16:23:16
      * I13PC18   Stand: 31/08/98    08:53:42
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0482.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0482.CPv  $
      *
      *   Rev 2.5   Mar 21 2016 16:18:18   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)
      *
      *   Rev 2.4   Apr 02 2004 08:44:30   t13ex49
      *h:\mfe\cpy\DRKI0482.CPY -> L:\mf\CPY\DRKI0482.CPv
      *Enter description of changes
      *Auswertung Kundenabgänge
      *
      *   Rev 2.3   Apr 02 2004 07:47:30   t13ex49
      *Übergabe Auswertung Kundenabgänge
      *
      *   Rev 2.2   Jan 15 2003 13:40:18   t13ex39
      *AKI00303 KoKü
      *
      *   Rev 2.1   Jan 06 2000 07:47:40   T13AE21
      *FKI00255
      *
      *   Rev 2.0   Feb 27 1999 12:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:50:52   T13EX39
      *FUNKTIONEN FUER ZTKI0359
      *
      *   Rev 1.0   28 Jul 1998 09:57:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
151098*    COPY-MEMBER: DRKI0482                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  31.08.98/08.47.23
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !15.10.98!    !Funktion eingefuegt        !  gbe     *
      * FKI00255!15.12.99!    !Funktion eingefuegt        !PHILIPP   *
      * -------------------------------------------------------------*
      * AKI00303!14.10.02!2.4 ! Neue Fkt. für KoKü        !WHO       *
      * -------------------------------------------------------------*
      * AKI00409!29.03.04!    ! Select VRBKOMMBETRKZ      ! P. Pohl  *
      * AKI01400!24.11.15!2.4 ! Neuer Insert              ! IFR      *
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
       01  PI-ZTKI0359.
           05  PI-ZTKI0359-FUNKTION            PIC  X(008).


             88   PI-ZTKI0359-DRKI0484     VALUE 'DRKI0484'.
             88   PI-ZTKI0359-DRKI0486     VALUE 'DRKI0486'.
             88   PI-ZTKI0359-DRKI0485     VALUE 'DRKI0485'.
             88   PI-ZTKI0359-DRKI0483     VALUE 'DRKI0483'.
151098       88   PI-ZTKI0359-DRKI0729     VALUE 'DRKI0729'.
151299       88   PI-ZTKI0359-DRKI0766     VALUE 'DRKI0766'.
             88   PI-ZTKI0359-DRKI1470     VALUE 'DRKI1470'.
290304       88   PI-ZTKI0359-DRKI1667     VALUE 'DRKI1667'.
241115       88   PI-ZTKI0359-DRKI2989     VALUE 'DRKI2989'.

      *
      ******* ENDE COPY-MEMBER **************************************

