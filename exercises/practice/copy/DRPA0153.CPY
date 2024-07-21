      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0153.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0153.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:48:40   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 10:20:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0153                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    25.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.10.96! 1  ! NEU                       ! AUERBACH *
      * AFPA....!24.03.97! 2  ! Fkt. DRPA0237             ! A.CECCONI*
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
       01  PI-ZWPA0011.
           05  PI-ZWPA0011-FUNKTION            PIC  X(008).
               88 PI-ZWPA0011-DRPA0156              VALUE 'DRPA0156'.
               88 PI-ZWPA0011-DRPA0157              VALUE 'DRPA0157'.
               88 PI-ZWPA0011-DRPA0237              VALUE 'DRPA0237'.
      *
      ******* ENDE COPY-MEMBER **************************************
